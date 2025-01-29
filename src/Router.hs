{-# LANGUAGE OverloadedStrings #-}

module Router where

import           Data.List                       (intercalate)
import qualified Data.Text                       as T
import qualified Data.Text.Lazy                  as LT
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Time (UTCTimestamp)
import           DB
import           Web.Scotty

router :: Int -> Connection -> IO ()
router port conn = do
  putStrLn $ "Running server on port: " <> show port
  scotty port $ do
    get "/" $ do
      html $ mainPage <> toTables
    get "/tables" $ do
      tables <- liftIO $ fetchTables conn
      html $ LT.pack $ renderTables tables
    get "/create/:tablename" $ do
      tn <- pathParam "tablename"
      liftIO $ createTable conn tn
      html $ "Table " <> LT.fromStrict tn <> " has been created" <> toTables
    get "/table/:tablename" $ do
      tn <- pathParam "tablename"
      records <- liftIO $ getRecords conn tn
      html $ "<h2> Table " <> LT.fromStrict tn <> "</h2>" <> (if (null records) then "<p>The table is empty</p>" else renderRecords records) <> toTables
    get "/add/:table/:code" $ do
      tn <- pathParam "table"
      c <- pathParam "code"
      liftIO $ addRecord conn tn c
      html $ "<p>Record added</p>" <> toTables

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

toTables :: LT.Text
toTables = "\n<a href=\"/tables\"> Back to the list of tables</a>"

mainPage :: LT.Text
mainPage =
  "<h2>Конфигурация в файле ~/.dbcrazy</h2>\
  \<p>формат ПАРАМЕТР ПРОБЕЛ ЗНАЧЕНИЕ (в скобках - дефолтные параметры)<p>\
  \<p>port число (80) - порт сервера<p>\
  \<p>dbport число (5432) - сокет постргреса<p>\
  \<p>dbname строка - имя базы данных (crazydb) <p>\
  \<p>dbuser строка - пользователь БД (postgres)<p>\
  \<p>dbpassword строка - пароль БД (пустая строка)<p>\
  \<p>dbhost строка - хост БД (localhost)<p>\
  \<h5>значения строк без кавычек - dbhost localhost, парсит до пробела или конца строки<h5>\
  \<br><br><h2>Маршруты</h2>\
  \<p>/tables - список таблиц, со ссылками на список записей сразу<p>\
  \<p>/create/tablename - создать таблицy tablename</p>\
  \<p>/table/tablename - вывести записи в таблице tablename</p>\
  \<p>/add/tablename/code = записать поле code в таблицу tablename (в примере - длина поля code до 40 символов)</p>\
  \<br><br><h2>Список таблиц хранится в отдельной таблице table_index, надо создать самому</h2>\
  \<p>я в примере создавал в постгресе компандой</p>\
  \<p>create table table_index (table_name varchar(100) primary key, table_time timestamptz default now())</p>"
