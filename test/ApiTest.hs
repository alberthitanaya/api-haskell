{-# LANGUAGE OverloadedStrings #-}

module Apitest where

import Control.Concurrent (killThread)
import Control.Monad (forM, void)
import Data.Either (isLeft, isRight)
import Data.Int (Int64)
import Data.Maybe (isJust)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Database.Persist.Postgresql (fromSqlKey, Key, toSqlKey, entityVal, Entity(..))
import Servant.Client (runClientM, ClientEnv)
import Test.Hspec

import Api (fetchUserClient, createUserClient)
import Database (PGInfo, RedisInfo, fetchUserPG, deleteUserPG, fetchUserRedis, deleteUserCache,
                 createUserPG)
import Schema (User(..))
import TestUtils (setupTests)

main :: IO (PGInfo, RedisInfo, ClientEnv, ThreadID)
main = do
  pgInfo <- fetchPostgresConnection
  redisInfo <- fetchRedisConnection
  mgr <- newManager tlsManagerSettings
  baseUrl <- parseBaseUrl "http://127.0.0.1:8000"
  let clientEnv = ClientEnv mgr baseUrl
  runStdoutLoggingT $ withPostgresqlConn pgInfo $ \dbConn ->
    runReaderT (runMigrationSilent migrateAll) dbConn
  threadId <- forkIO runServer
  threadDelay 1000000
  return (pgInfo, redisInfo, clientEnv, serverThreadId)

-- Test #1 - requesting user by ID on an empty throws an error

beforeHook1 :: ClientEnv -> PGInfo -> RedisInfo -> IO (Bool, Bool, Bool)
beforeHook1 clientEnv pgInfo redisInfo = do
  callResult <- runClientM (fetchUserClient 1) clientEnv
  let throwsError = isLeft (callResult)
  inPG <- isJust <$> fetchUserPG pgInfo 1
  inRedis <- isJust <$> fetchUserRedis redisInfo 1
  return (throwsError, inPG, inRedis)

spec1 :: SpecWith (Bool, Bool, Bool)
spec1 = describe "After fetching on an empty database" $ do
  it "The fetch call should throw an error" $ \(throwsError, _, _) ->
    throwsError `shouldBe` True
  it "There should be no user in Postgres" $ \(_, inPG, _) ->
    inPG `shouldBe` False
  it "There should be no user in Redis" $ \(_, _, inRedis) ->
    inRedis `shouldBe` False

-- Test 2 - creating a user leads to a DB user but no cache user

beforeHook2 :: ClientEnv -> PGInfo -> RedisInfo -> IO (Bool, Bool, Bool)
beforeHook2 clientEnv pgInfo redisInfo = do
  userKeyEither <- runClientM (createUserClient testUser) clientEnv
  case userKeyEither of
    Left _ -> error "DB call failed on spec 2!"
    Right userKey -> do
      inPG <- isJust <$> fetchUserPG pgInfo userKey
      inRedis <- isJust <$> fetchUserRedis redisInfo userKey
      return (inPG, inRedis, userKey)

spec2 :: SpecWith (Bool, Bool, Int64)
spec2 = describe "After creating the user but not fetching" $ do
  it "There should be a user in Postgres" $ \(inPg, _, _) -> inPg `shouldBe` True
  it "There should be no user in redis"   $ \(_, inRedis, _) -> inRedis `shouldBe` False

afterHook :: PGInfo -> RedisInfo -> (Bool, Bool, Int64) -> IO ()
afterHook pgInfo redisInfo (_,_,key) = do
  deleteUserCache redisInfo key
  deleteUserPG pgInfo key

-- Test 3: Creating then requesting a user should result in a DB user and a cache user

beforeHook3 :: ClientEnv -> PGInfo -> RedisInfo -> IO (Bool, Bool, Int64)
beforeHook3 c p = do
  userKeyEither <- runClientM (createUserClient testUser) clientEnv
  case userKeyEither of
    Left _ -> error "DB Call failed on spec 3!"
    Right userKey -> do
      _ <- runClientM (fetchUserClient userKey) clientEnv
      inPG <- isJust <$> fetchUserPG pgInfo userKey
      inRedis <- isJust <$> fetchUserRedis redisInfo userKey
      return (inPG, inRedis, userKey)

spec3 :: SpecWith (Bool, Bool, Int64)
spec3 = describe "After creating the user and fetching" $ do
  it "There should be a user in postgres" $ \(inPG, _, _) -> inPG `shouldBe` True
  it "There should be a a user in redis" $ \(_, inRedis, _) -> inRedis `shouldBe` True

-- Main func

main :: IO ()
main = do
  (pgInfo, redisInfo, clientEnv, tid) <- setupTests
  hspec $ before (beforeHook1 clientEnv pgInfo redisInfo) spec1
  hspec $ before (beforeHook1 clientEnv pgInfo redisInfo) $ after (afterHook pgInfo redisInfo) $ spec2
  hspec $ before (beforeHook3 clientEnv pgInfo redisInfo) $ after (afterHook pgInfo redisInfo) $ spec3
  killThread tid
  return ()
