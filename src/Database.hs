{-# LANGUAGE OverloadedStrings #-}

module Database where

import           Control.Monad.Logger (runStdoutLoggingT, MonadLogger, LoggingT)
import           Control.Monad.Reader (runReaderT)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Int (Int64)
import           Database.Persist (Entity(..), selectList, (==.), (<.), SelectOpt(..), get, insert, delete)
import           Database.Persist.Postgresql (ConnectionString, withPostgresqlConn, runMigration, SqlPersistT, fromSqlKey, toSqlKey)

import           Schema

type PGInfo = ConnectionString

-- Function for Main.hs
runDB :: IO ()
runDB = migrateDB localConnString

localConnString :: ConnectionString
localConnString = "host=127.0.0.1 port=5432 user=postgres dbname=postgres"

runAction :: ConnectionString -> SqlPersistT (LoggingT IO) a -> IO a
runAction connectionString action =
  runStdoutLoggingT $ withPostgresqlConn connectionString $ \backend ->
    runReaderT action backend

migrateDB :: ConnectionString -> IO ()
migrateDB connString = runAction connString (runMigration migrateAll)

fetchPostgresConnection :: IO ConnectionString
fetchPostgresConnection = return localConnString

-- CRUD

-- Create

createUserPG :: PGInfo -> User -> IO Int64
createUserPG connString user = fromSqlKey <$> runAction connString (insert user)

-- Read

fetchUserPG :: ConnectionString -> Int64 -> IO (Maybe User)
fetchUserPG connString uid = runAction connString (get (toSqlKey uid))

selectUserPG :: PGInfo -> Int64 -> IO (Maybe User)
selectUserPG connString uid = runAction connString (get (toSqlKey uid))

-- Delete
deleteUserPG :: ConnectionString -> Int64 -> IO ()
deleteUserPG connString uid = runAction connString (delete userKey)
  where
    userKey :: Key User
    userKey = toSqlKey uid

-- Dummy data
sampleUser :: Entity User
sampleUser = Entity (toSqlKey 1) $ User
  { userName = "test name"
  , userEmail = "jake@finder.com"
  , userAge = 25
  , userOccupation = "dev"
  }
