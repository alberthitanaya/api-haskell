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

import          Database (createUserPG, fetchUserPG, fetchPostgresConnection)
import          Schema

type UsersAPI =
       -- The 'Capture' component allows for URL param extraction
       "users" :> Capture "userid" Int64 :> Get '[JSON] User
  :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] Int64

-- Handlers

fetchUserHandler :: ConnectionString -> Int64 -> Handler User
fetchUserHandler connString uid = do
  maybeUser <- liftIO $ fetchUserPG connString uid
  case maybeUser of
    Just user -> return user
    Nothing -> Handler $ (throwE $ err401 { errBody = "Could not find user with that ID" })

createUserHandler :: ConnectionString -> User -> Handler Int64
createUserHandler connString user = liftIO $ createUserPG connString user

-- server
-- TODO: These must line up with UsersAPI subtypes
usersServer :: ConnectionString -> Server UsersAPI
usersServer pgInfo =
  (fetchUserHandler pgInfo) :<|>
  (createUserHandler pgInfo)

usersApi :: Proxy UsersAPI
usersApi = Proxy :: Proxy UsersAPI

runServer :: IO ()
runServer = do
  connString <- fetchPostgresConnection
  run 8000 (serve usersApi (usersServer connString))
