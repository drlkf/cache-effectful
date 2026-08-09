{-# LANGUAGE NoOverloadedStrings #-}

module Main where

import Control.Exception (SomeException, try)
import qualified Database.Memcache.Client as M
import Database.Memcache.Cluster (newCluster)
import Data.ByteString.Char8 (ByteString, pack)
import Data.List (sort)
import Effectful (liftIO, runEff)
import Effectful.Cache (delete, insert, keys, lookup)
import Effectful.Cache.Memcached (runCacheMemcache)
import Test.Tasty (TestTree, defaultMain, testCase, testGroup)
import Test.Tasty.HUnit (assertEqual)
import Prelude hiding (lookup)

spec :: M.ServerSpec
spec = M.ServerSpec "127.0.0.1" "11211" M.NoAuth

main :: IO ()
main = do
  cluster <- newCluster [spec] M.def
  up <- probe cluster
  if not up
    then putStrLn "memcached not reachable at 127.0.0.1:11211, skipping live tests"
    else defaultMain $ testGroup "cache-effectful-memcached" (tests cluster)

tests :: M.Cluster -> [TestTree]
tests cluster =
  [ testCase "Insert & Lookup" $ testInsertAndLookup cluster
  , testCase "Listing keys" $ testListKeys cluster
  , testCase "Deleting keys" $ testDeleteKeys cluster
  ]

probe :: M.Cluster -> IO Bool
probe cluster = do
  r <- try @SomeException (M.version cluster)
  pure (either (const False) (const True) r)

testInsertAndLookup :: M.Cluster -> Assertion
testInsertAndLookup cluster = runEff $ do
  result <- runCacheMemcache @String cluster [spec] Nothing Nothing $ do
    insert @String @ByteString "hello" (pack "world")
    lookup @String "hello"
  assertEqual "Looking up key yields the value" (Just (pack "world")) result

testListKeys :: M.Cluster -> Assertion
testListKeys cluster = runEff $ do
  result <- runCacheMemcache @String cluster [spec] Nothing Nothing $ do
    liftIO (M.flush cluster Nothing)
    insert @String @ByteString "a" (pack "1")
    insert @String @ByteString "b" (pack "2")
    sort <$> keys @String @ByteString
  assertEqual "Correct list of keys" ["a", "b"] result

testDeleteKeys :: M.Cluster -> Assertion
testDeleteKeys cluster = runEff $ do
  result <- runCacheMemcache @String cluster [spec] Nothing Nothing $ do
    liftIO (M.flush cluster Nothing)
    insert @String @ByteString "a" (pack "1")
    insert @String @ByteString "b" (pack "2")
    delete @String @ByteString "a"
    sort <$> keys @String @ByteString
  assertEqual "Keys are deleted" ["b"] result
