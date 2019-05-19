module Main where

import Api
import Database
-- Start server from App.hs
main :: IO ()
main = runServer

-- Migrate DB from Database.hs
startDB :: IO ()
startDB = runDB
