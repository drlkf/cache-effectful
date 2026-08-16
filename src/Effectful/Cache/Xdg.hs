{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeFamilies #-}

-- |
--   Module      : Effectful.Cache.Xdg
--   Copyright   : © drlkf, 2026
--   License     : MIT
--   Maintainer  : hecate@glitchbra.in
--   Stability   : stable
--
--   A file-backed 'Cache' interpreter storing values under the XDG cache
--   directory.
module Effectful.Cache.Xdg
  ( -- * Types
    XdgCacheError (..)

    -- * Handlers
  , runCacheXdg

    -- * Cache operations
  , szInsert
  , dzLookup

    -- * Helpers
  , validateKey
  ) where

import Control.Monad (forM_, unless)
import Control.Monad.Extra (ifM, whenM)
import Data.ByteString (ByteString)
import Data.Foldable (foldlM)
import Data.Functor ((<&>))
import Data.List (isPrefixOf)
import Data.Serialize (Serialize, decode, encode)
import Effectful (Eff, Effect, (:>))
import Effectful.Cache (Cache (..), insert, lookup)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Dynamic (Error, throwError)
import Effectful.FileSystem (FileSystem, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getXdgDirectory, listDirectory, removeFile)
import Effectful.FileSystem.IO.ByteString (readFile)
import Effectful.FileSystem.IO.File (writeBinaryFileDurableAtomic)
import System.Directory (XdgDirectory (..))
import System.FilePath (makeRelative, pathSeparator, splitDirectories, takeDirectory, (</>))
import Prelude hiding (lookup, readFile, writeFile)

-- | XDG cache-specific errors.
data XdgCacheError
  = -- | An invalid key was provided, most likely containing a @.@ or @..@ which
    -- could cause cache directory escape.
    InvalidKey FilePath String
  | -- | An error occurred while deserializing a value.
    XdgDecodeError String
  deriving (Eq)

instance Show XdgCacheError where
  show (InvalidKey k reason) = mconcat ["invalid key '", k, "': ", reason]
  show (XdgDecodeError reason) = "xdg cache deserialize error: " <> reason

-- | Run a 'Cache' effect backed by files under @$XDG_CACHE_HOME/<namespace>@.
--
-- Each key is stored as a file at @$XDG_CACHE_HOME/<namespace>/<key>@, with
-- sub-directories created on write. Keys that would escape the namespace
-- directory (e.g. @..@ or absolute paths) are rejected.
--
-- Your stack must handle an 'Error XdgCacheError' effect on top of this
-- interpreter.
--
-- @since UNRELEASED
runCacheXdg
  :: forall (es :: [Effect]) a
   . FileSystem :> es
  => Error XdgCacheError :> es
  => FilePath
  -> Eff (Cache FilePath ByteString : es) a
  -> Eff es a
runCacheXdg namespace = interpret $ \_ -> \case
  Insert key value -> do
    path <- resolveKey namespace key
    createDirectoryIfMissing True (takeDirectory path)
    writeBinaryFileDurableAtomic path value
  Lookup key -> do
    path <- resolveKey namespace key

    ifM
      (doesFileExist path)
      (Just <$> readFile path)
      (pure Nothing)
  Keys -> do
    root <- getXdgDirectory XdgCache namespace

    ifM
      (doesDirectoryExist root)
      (fmap (makeRelative root) <$> listFilesRecursive root)
      (pure mempty)
  Delete key -> do
    path <- resolveKey namespace key
    whenM (doesFileExist path) $
      removeFile path
  FilterWithKey fun -> do
    root <- getXdgDirectory XdgCache namespace

    whenM (doesDirectoryExist root) $ do
      files <- listFilesRecursive root
      forM_ (files :: [FilePath]) $ \file -> do
        let key = makeRelative root file
        value <- readFile file
        unless (fun key value) $ removeFile file

-- | Insert a serialized value under the given key.
--
-- @since UNRELEASED
szInsert
  :: forall (es :: [Effect]) v
   . Cache FilePath ByteString :> es
  => Serialize v
  => FilePath
  -> v
  -> Eff es ()
szInsert key value = insert @FilePath @ByteString key (encode value)

-- | Look up a serialized value under the given key.
--
-- @since UNRELEASED
dzLookup
  :: forall (es :: [Effect]) v
   . Cache FilePath ByteString :> es
  => Error XdgCacheError :> es
  => Serialize v
  => FilePath
  -> Eff es (Maybe v)
dzLookup key = do
  mval <- lookup @FilePath @ByteString key
  case mval of
    Nothing -> pure Nothing
    Just v -> either (throwError . XdgDecodeError) (pure . Just) (decode v)

resolveKey
  :: forall (es :: [Effect])
   . FileSystem :> es
  => Error XdgCacheError :> es
  => FilePath
  -> FilePath
  -> Eff es FilePath
resolveKey namespace key =
  maybe
    ((getXdgDirectory XdgCache namespace) <&> (</> key))
    throwError
    (validateKey key)

validateKey
  :: FilePath
  -> Maybe XdgCacheError
validateKey key
  | [pathSeparator] `isPrefixOf` key =
      Just (InvalidKey key "key be an absolute path")
  | any ("." `isPrefixOf`) (splitDirectories key) =
      Just (InvalidKey key "key cannot contain paths starting with '.'")
  | otherwise = Nothing

listFilesRecursive
  :: forall (es :: [Effect]) t
   . FileSystem :> es
  => Foldable t
  => Applicative t
  => Monoid (t FilePath)
  => FilePath
  -> Eff es (t FilePath)
listFilesRecursive dir = do
  entries <- listDirectory dir
  foldlM (\t e -> (t <>) <$> (getEntry e)) mempty entries
  where
    getEntry entry =
      let path = dir </> entry
      in ifM
           (doesDirectoryExist path)
           (listFilesRecursive path)
           (pure (pure path))
