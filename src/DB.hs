{-# LANGUAGE OverloadedStrings #-}

module DB where

import           Control.Exception
import           Control.Monad
import           Data.Text                        (Text, unpack)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Time  (UTCTimestamp)
import           Database.PostgreSQL.Simple.Types
import           Settings

type TableName = Text

type Code = Text

getConnection :: IO Connection
getConnection = do
  sts <- settings
  connect $
    ConnectInfo
      { connectHost = (dbHost sts),
        connectPort = (dbPort sts),
        connectUser = (dbUser sts),
        connectDatabase = (dbName sts),
        connectPassword = (dbPswd sts)
      }

doesTableExists :: Connection -> TableName -> IO Bool
doesTableExists conn tn = do
  tables <- fetchTables conn
  case lookup tn tables of
    Just _ -> pure True
    _      -> pure False

createTable :: Connection -> TableName -> IO ()
createTable conn tn = do
  doesTableExists conn tn >>= \case
    False -> do
      _ <- execute conn "insert into table_index (table_name, table_time) values (?, default)" (Only tn)
      _ <- execute conn "create table ? (code varchar(40) primary key);" (Only . Identifier $ tn)
      putStrLn $ "LOG: Table '" <> unpack tn <> "' created"
    True -> putStrLn $ "LOG : Table '" <> unpack tn <> "' already exists"

fetchTables :: Connection -> IO [(TableName, UTCTimestamp)]
fetchTables conn = query_ conn "select table_name, table_time from table_index"

addRecord :: Connection -> TableName -> Code -> IO ()
addRecord conn tn code = do
  _ <- execute conn "insert into ? (code) values (?)" (Identifier tn, code)
  pure ()

getRecords :: Connection -> TableName -> IO [String]
getRecords conn tn = do
  records :: [Only Text] <- query conn "select * from ?" (Only . Identifier $ tn)
  pure $ map (\(Only record) -> unpack record) records
