module Settings where

import           Control.Applicative           (some)
import           Data.Char                     (isAlphaNum)
import           Data.Maybe                    (catMaybes)
import           Data.Word
import           System.Directory
import           System.FilePath
import           Text.Regex.Applicative
import           Text.Regex.Applicative.Common

data Settings = Settings { port    :: Int
                         , dbPort  :: Word16
                         , dbHost  :: String
                         , dbName  :: String
                         , dbTable :: String
                         , dbUser  :: String
                         , dbPswd  :: String
                         }
  deriving (Show)

defaultSettings :: Settings
defaultSettings = Settings 80 5432 "localhost" "crazydb" "work" "postgres" ""

data Preference = Port Int
                | DBPort Word16
                | DBHost String
                | DBName String
                | DBTable String
                | DBUser String
                | DBPswd String
  deriving (Show)

updateSettings :: Settings -> Preference -> Settings
updateSettings sts = \case
  Port p     -> sts {port = p}
  DBPort p   -> sts {dbPort = p}
  DBName db  -> sts {dbName = db}
  DBTable tb -> sts {dbTable = tb}
  DBUser tb  -> sts {dbUser = tb}
  DBPswd tb  -> sts {dbPswd = tb}
  DBHost tb  -> sts {dbHost = tb}

settings :: IO Settings
settings = do
  configFile <- (flip (</>) ".dbcrazy") <$> getHomeDirectory
  hasSettings <- doesFileExist configFile
  case hasSettings of
    False -> pure defaultSettings
    True -> do
      config <- readFile configFile
      let params = catMaybes . map (match (portP <|> dbPortP <|> dbNameP <|> dbTableP <|> dbHostP <|> dbUserP <|> dbPswdP)) . lines $ config
      pure $ foldl updateSettings defaultSettings params

type Parser = RE Char

ws :: Parser String
ws = some (psym (\c -> elem c " \t"))

portP, dbPortP, dbNameP, dbTableP, dbHostP, dbUserP, dbPswdP :: Parser Preference
portP = Port <$> (string "port" *> ws *> decimal)
dbPortP = (DBPort . fromIntegral) <$> (string "dbport" *> ws *> decimal)
dbNameP = DBName <$> (string "dbname" *> ws *> some (psym isAlphaNum))
dbTableP = DBTable <$> (string "dbtable" *> ws *> some (psym isAlphaNum))
dbUserP = DBUser <$> (string "dbuser" *> ws *> some (psym isAlphaNum))
dbPswdP = DBTable <$> (string "dbpassword" *> ws *> some (psym isAlphaNum))
dbHostP = DBTable <$> (string "dbhost" *> ws *> some (psym isAlphaNum))
