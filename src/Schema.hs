{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Schema where

import           Data.Int
import           Data.Text
import           Database.Beam
import           Database.Beam.Postgres

data ShopT f = Shop { _id          :: Columnar f Int32
                    , _name        :: Columnar f Text
                    , _description :: Columnar f Text
                    , _price       :: Columnar f Int32
                    }
  deriving (Beamable, Generic)

instance Table ShopT where
  data PrimaryKey ShopT f = ShopID (Columnar f Int32) deriving (Generic, Beamable)
  primaryKey = ShopID <$> _id

type Shop = ShopT Identity

deriving instance Show Shop

deriving instance Eq Shop

type ShopID = PrimaryKey ShopT Identity

deriving instance Show ShopID

deriving instance Eq ShopID
