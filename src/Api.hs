{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Api where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except (throwE)
import           Data.Int (Int64)
import           Data.Proxy (Proxy(..))
import           Database.Persist (Key, Entity)
import           Database.Persist.Postgresql (ConnectionString)
import           Network.Wai.Handler.Warp (run)
import           Servant.API
import           Servant.Client
import           Servant.Server

import          Database (cacheUser, createUserPG, fetchUserPG, fetchUserRedis, fetchPostgresConnection, fetchRedisConnection, RedisInfo, PGInfo)
import          Schema

type UsersAPI =
       -- The 'Capture' component allows for URL param extraction
       "users" :> Capture "userid" Int64 :> Get '[JSON] User
  :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] Int64

-- Handlers

fetchUserHandler :: PGInfo -> RedisInfo -> Int64 -> Handler User
fetchUserHandler pgInfo redisInfo uid = do
  maybeCachedUser <- liftIO $ fetchUserRedis redisInfo uid
  case maybeCachedUser of
    Just user -> return user
    Nothing -> do
      maybeUser <- liftIO $ fetchUserPG pgInfo uid
      case maybeUser of
        Just user -> liftIO (cacheUser redisInfo uid user) >> return user
        Nothing -> Handler $ (throwE $ err401 { errBody = "Could not find user with that ID" })

createUserHandler :: ConnectionString -> User -> Handler Int64
createUserHandler connString user = liftIO $ createUserPG connString user

-- server
-- TODO: These must line up with UsersAPI subtypes
usersServer :: PGInfo -> RedisInfo -> Server UsersAPI
usersServer pgInfo redisInfo =
  (fetchUserHandler pgInfo redisInfo) :<|>
  (createUserHandler pgInfo)

usersApi :: Proxy UsersAPI
usersApi = Proxy :: Proxy UsersAPI

runServer :: IO ()
runServer = do
  pgInfo <- fetchPostgresConnection
  redisInfo <- fetchRedisConnection
  run 8000 (serve usersApi (usersServer pgInfo redisInfo))

-- servant-client funcs

fetchUserClient :: Int64 -> ClientM User
createUserClient :: User -> ClientM Int64

(fetchUserClient
  :<|> createUserClient) = client (Proxy :: Proxy UsersAPI)
