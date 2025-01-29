{-# LANGUAGE OverloadedStrings #-}

module Router where

import           Data.List                       (intercalate)
import qualified Data.Text                       as T
import qualified Data.Text.Lazy                  as LT
import           Data.Time
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Time (UTCTimestamp)
import           DB
import           Web.Scotty

router :: Connection -> IO ()
router conn = do
  putStrLn "Running server on port: 3000"
  scotty 3000 $ do
    get "/" $ do
      html "<h1>Main router page</h1>"
    get "/tables" $ do
      tables <- liftIO $ fetchTables conn
      html $ LT.pack $ renderTables tables
    get "/create/:tablename" $ do
      tn <- param "tablename"
      liftIO $ createTable conn tn
      html $ "Table " <> LT.fromStrict tn <> " has been created"
    get "/table/:tablename" $ do
      tn <- param "tablename"
      records <- liftIO $ getRecords conn tn
      html $ "<h2> Table " <> LT.fromStrict tn <> "</h2>" <> if (null records) then "<p>The table is empty</p>" else renderRecords records
    get "/add/:id/:label/" $ do
      q <- getQuery ["name", "desc"]
      (p :: [Param]) <- pathParams
      liftIO $ mapM_ (\(k, v) -> putStrLn $ T.unpack k <> " : " <> T.unpack v) p
      html $ "done"

getQuery :: [LT.Text] -> ActionM [(LT.Text, LT.Text)]
getQuery [] = pure []
getQuery (k : keys) =
  queryParamMaybe k >>= \case
    Nothing  -> getQuery keys
    (Just v) -> ((k, v) :) <$> getQuery keys

renderTables :: [(T.Text, UTCTimestamp)] -> String
renderTables tables = "<h2>List of available talbes</h2>\n" <> intercalate "<br>" (renderTable <$> tables)

renderTable :: (T.Text, UTCTimestamp) -> String
renderTable (tn, time) = "<a href=\"/table/" <> T.unpack tn <> "\">" <> T.unpack tn <> " : " <> show time <> "</a>"

renderRecords :: [String] -> LT.Text
renderRecords records = LT.pack $ intercalate "\n" (map (\record -> "<p>" <> record <> "</p>") records)
