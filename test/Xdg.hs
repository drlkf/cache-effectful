{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}

module Xdg (tests) where

import Control.Exception (finally)
import Data.ByteString (ByteString)
import Data.List (sort)
import Data.Maybe (isJust)
import Effectful (Eff, Effect, IOE, runEff, (:>))
import Effectful.Cache (Cache, delete, filterWithKey, insert, keys, lookup)
import Effectful.Cache.Xdg (XdgCacheError)
import qualified Effectful.Cache.Xdg as X
import Effectful.Error.Dynamic (Error, runErrorNoCallStack)
import Effectful.FileSystem (FileSystem, runFileSystem)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import Test.Tasty (DependencyType (..), TestTree, dependentTestGroup, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)
import qualified Utils as U
import Prelude hiding (lookup)

tests :: TestTree
tests =
  testGroup
    "Xdg"
    -- sequential to avoid env collisions
    [ dependentTestGroup
        "files"
        AllSucceed
        [ testCase "Insert & Lookup" testInsertAndLookup
        , testCase "Listing keys" testListKeys
        , testCase "Deleting keys" testDeleteKeys
        , testCase "Filter with key" testFilterWithKey
        , testCase "szInsert/dzLookup roundtrip" testSerializedRoundtrip
        ]
    , testGroup
        "validation"
        [ testCase "Path with dot rejected" testPathEscapeDot
        , testCase "Absolute path rejected" testPathEscapeAbs
        ]
    ]

runXdgWithError
  :: forall (es :: [Effect]) a
   . IOE :> es
  => Eff (Cache FilePath ByteString : FileSystem : Error XdgCacheError : es) a
  -> Eff es (Either XdgCacheError a)
runXdgWithError =
  runErrorNoCallStack
    . runFileSystem
    . X.runCacheXdg "test-cache"

withXdgCache :: (FilePath -> IO a) -> IO a
withXdgCache action = do
  -- leave the directory present after tests for inspection in case of failure
  dir <-
    flip createTempDirectory "cache-effectful-test"
      =<< getCanonicalTemporaryDirectory

  old <- lookupEnv "XDG_CACHE_HOME"
  setEnv "XDG_CACHE_HOME" dir
  action dir
    `finally` maybe (unsetEnv "XDG_CACHE_HOME") (setEnv "XDG_CACHE_HOME") old

testInsertAndLookup :: Assertion
testInsertAndLookup = withXdgCache $ \_ -> runEff $ do
  result <- runXdgWithError insertAndLookup
  U.assertEqual
    "Looking up a nested key yields the value"
    (Right (Just v))
    result
  where
    k :: FilePath
    k = "full" </> "path" </> "to" </> "key"
    v :: ByteString
    v = "hello"
    insertAndLookup
      :: Cache FilePath ByteString :> es
      => Eff es (Maybe ByteString)
    insertAndLookup = do
      insert k v
      lookup k

testListKeys :: Assertion
testListKeys = withXdgCache $ \_ -> runEff $ do
  result <- runXdgWithError listKeys
  U.assertEqual
    "Keys are relative paths under the namespace"
    ( Right
        [ "a" </> "b" </> "c"
        , "a" </> "d"
        , "top"
        ]
    )
    (fmap sort result)
  where
    listKeys = do
      mapM_
        (flip (insert @FilePath @ByteString) "v")
        [ "a" </> "b" </> "c"
        , "a" </> "d"
        , "top"
        ]
      keys @FilePath @ByteString

testDeleteKeys :: Assertion
testDeleteKeys = withXdgCache $ \_ -> runEff $ do
  result <- runXdgWithError deleteKeys
  U.assertEqual
    "Deleted key is gone, others remain"
    (Right ["a" </> "b" </> "c", "top"])
    (fmap sort result)
  where
    deleteKeys
      :: Cache FilePath ByteString :> es
      => Eff es [FilePath]
    deleteKeys = do
      mapM_
        (flip (insert @FilePath @ByteString) "v")
        [ "a" </> "b" </> "c"
        , "a" </> "d"
        , "top"
        ]
      delete @FilePath @ByteString ("a" </> "d")
      keys @FilePath @ByteString

testFilterWithKey :: Assertion
testFilterWithKey = withXdgCache $ \_ -> runEff $ do
  result <- runXdgWithError filterKeys
  U.assertEqual
    "Keys are properly filtered"
    ( Right
        [ "a" </> "b" </> "c"
        , "top"
        ]
    )
    (fmap sort result)
  where
    filterKeys
      :: Cache FilePath ByteString :> es
      => Eff es [FilePath]
    filterKeys = do
      mapM_
        (flip (insert @FilePath @ByteString) "v")
        [ "a" </> "b" </> "c"
        , "a" </> "d"
        , "top"
        ]
      filterWithKey @FilePath @ByteString (\key _ -> key /= "a" </> "d")
      keys @FilePath @ByteString

testPathEscapeDot :: Assertion
testPathEscapeDot =
  assertBool
    "Relative escape is rejected"
    (isJust @XdgCacheError (X.validateKey (".." </> "outside")))

testPathEscapeAbs :: Assertion
testPathEscapeAbs = do
  dir <- getCanonicalTemporaryDirectory

  assertBool
    "Absolute key is rejected"
    (isJust @XdgCacheError (X.validateKey (dir </> "abs")))

testSerializedRoundtrip :: Assertion
testSerializedRoundtrip = withXdgCache $ \_ -> runEff $ do
  result <- runXdgWithError roundtrip
  U.assertEqual
    "Serialized value roundtrips"
    (Right (Just (42 :: Int)))
    result
  where
    roundtrip
      :: Cache FilePath ByteString :> es
      => Error XdgCacheError :> es
      => Eff es (Maybe Int)
    roundtrip = do
      X.szInsert "answer" (42 :: Int)
      X.dzLookup "answer"
