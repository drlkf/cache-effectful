{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
--   Module      : Effectful.Cache
--   Copyright   : © drlkf, 2026
--   License     : MIT
--   Maintainer  : hecate@glitchbra.in
--   Stability   : stable
--
--   An effect wrapper around 'Data.Cache' for the Effectful ecosystem
module Effectful.Cache.Memcached
  ( runCacheMemcache
  , insert'
  , lookup'
  ) where

import Control.Exception (bracket)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.Functor (($>))
import Data.Hashable (Hashable)
import Data.Kind (Type)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Serialize (Serialize, decode, encode)
import qualified Database.Memcache.Client as M
import Database.Memcache.Cluster (Cluster)
import qualified Database.Memcache.Socket as MS (recv, send)
import Database.Memcache.Types (Expiration, Flags, Request)
import Effectful (Eff, Effect, IOE, liftIO, (:>))
import Effectful.Cache (Cache (..))
import Effectful.Dispatch.Dynamic (interpret, send)
import Network.Socket
  ( HostName
  , ServiceName
  , Socket
  , SocketType (Stream)
  , addrAddress
  , addrSocketType
  , close
  , connect
  , defaultHints
  , getAddrInfo
  , openSocket
  )

lruMetadumpAll :: Request
lruMetadumpAll = Request (ReqRaw)
  where
    req = "lru_crawler metadump all"

data MemcachedMetadumpError
  = MetadumpBusy
  | MetadumpError
  | MetadumpClientError
  | MetadumpBadClass

metadumpErrorHeader :: String
metadumpErrorHeader = "memcached: lru_crawler metadump "

instance Show MemcachedMetadumpError where
  show MetadumpBusy = metadumpErrorHeader <> "busy"
  show MetadumpError = metadumpErrorHeader <> "error"
  show MetadumpClientError = metadumpErrorHeader <> "client error"
  show MetadumpBadClass = metadumpErrorHeader <> "bad class"

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

trd3 :: (a, b, c) -> c
trd3 (_, _, c) = c

-- | Integration handler for 'memcache' in conjunction with 'cereal' for
-- in-memory key representation.
--
-- Flags and expiration are provided in the interpreter and may be overridden
-- mid-action with 'reinterpret' and the likes. By default, expiration is set to
-- 0, meaning never expire.
--
-- Helper functions are provided to compose operations on 'Serialize'-able data
-- types.
--
-- @since UNRELEASED
runCacheMemcache
  :: forall (k :: Type) (es :: [Effect]) (a :: Type)
   . IOE :> es
  => Serialize k
  => Cluster
  -> [M.ServerSpec]
  -> Maybe Flags
  -> Maybe Expiration
  -> Eff (Cache k ByteString : es) a
  -> Eff es a
runCacheMemcache cluster specs flags expire = interpret $ \_ -> \case
  Insert k v ->
    liftIO (M.set cluster (encode k) v (fromMaybe 0 flags) (fromMaybe 0 expire))
      $> ()
  Lookup k -> fmap fst3 <$> liftIO (M.get cluster (encode k))
  Keys -> liftIO $ do
    raw <- concat <$> mapM (\spec -> metadumpKeys (M.ssHost spec) (M.ssPort spec)) specs
    pure [k | Right k <- map (decode @k) raw]
  -- unfortunate double-query but memcached delete semantics require key version
  Delete k -> liftIO $ do
    res <- M.get cluster (encode k)
    maybe (pure False) (M.delete cluster (encode k) . trd3) res $> ()
  FilterWithKey _ -> liftIO $ undefined -- TODO: get all values, flush no-satisfy ones

-- | Insert into 'memcache' using data 'Serialize' instance.
--
-- @since UNRELEASED
insert'
  :: forall (k :: Type) (v :: Type) (es :: [Effect])
   . Hashable k
  => Serialize v
  => Cache k ByteString :> es
  => k
  -> v
  -> Eff es ()
insert' k v = send (Insert k (encode v))

-- | Lookup into 'memcache' using data 'Serialize' instance.
--
-- @since UNRELEASED
lookup'
  :: forall (k :: Type) (v :: Type) (es :: [Effect])
   . Hashable k
  => Serialize v
  => Cache k ByteString :> es
  => k
  -> Eff es (Maybe v)
lookup' k = maybe Nothing (either (const Nothing) Just . decode) <$> send (Lookup k)

-- | Fetch all keys from a server via the @lru_crawler metadump all@
-- command. Keys are returned URI-decoded but still cereal-encoded.
metadumpKeys
  :: HostName
  -> ServiceName
  -> IO (t ByteString)
metadumpKeys host port = do
  let hints = defaultHints {addrSocketType = Stream}
  addrs <- getAddrInfo (Just hints) (Just host) (Just port)
  case addrs of
    [] -> ioError (userError $ "memcached: no address for " ++ host ++ ":" ++ port)
    addr : _ -> bracket (openSocket addr) close $ \s -> do
      connect s (addrAddress addr)
      MS.send s "lru_crawler metadump all\r\n"
      resp <- MS.recv s
      checkMetadumpError resp
      pure (fmap uriDecode (mapMaybe parseMetadumpKey (B8.lines resp)))

-- | Read until the @END@ line, the @OK@ line, or the connection closes.
recvUntilEnd
  :: Socket
  -> IO ByteString
recvUntilEnd s = go BS.empty
  where
    go acc = do
      chunk <- recv s 4096
      let acc' = acc <> chunk
      if BS.null chunk || BS.isSuffixOf "END\r\n" acc' || acc' == "OK\r\n"
        then pure acc'
        else go acc'

-- | Throw on metadump error responses instead of silently returning no keys.
checkMetadumpError
  :: ByteString
  -> IO ()
checkMetadumpError resp
  | "BUSY" `B8.isPrefixOf` resp =
      ioError (userError "memcached: lru_crawler metadump busy")
  | "ERROR" `B8.isPrefixOf` resp =
      ioError (userError "memcached: lru_crawler metadump error")
  | "CLIENT_ERROR" `B8.isPrefixOf` resp =
      ioError (userError "memcached: lru_crawler metadump client error")
  | "BADCLASS" `B8.isPrefixOf` resp =
      ioError (userError "memcached: lru_crawler metadump bad class")
  | otherwise = pure ()

-- | Extract the URI-encoded @key=@ field from a metadump line.
parseMetadumpKey :: ByteString -> Maybe ByteString
parseMetadumpKey line = do
  rest <- B8.stripPrefix "key=" line
  pure (B8.takeWhile (/= ' ') rest)

-- | Minimal percent-encoding and @+@-as-space decoder.
uriDecode :: ByteString -> ByteString
uriDecode = B8.pack . go . B8.unpack
  where
    go [] = []
    go ('%' : a : b : rest) = hex a b : go rest
    go ('+' : rest) = ' ' : go rest
    go (c : rest) = c : go rest
    hex a b = toEnum (16 * hexVal a + hexVal b)
    hexVal c
      | '0' <= c && c <= '9' = fromEnum c - fromEnum '0'
      | 'a' <= c && c <= 'f' = fromEnum c - fromEnum 'a' + 10
      | 'A' <= c && c <= 'F' = fromEnum c - fromEnum 'A' + 10
      | otherwise = 0
