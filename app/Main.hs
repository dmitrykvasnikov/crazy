{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Data.Text
import           DB
import           Router        (router)
import           Settings

main :: IO ()
main = do
  conn <- getConnection
  sts <- settings
  router (port sts) conn
