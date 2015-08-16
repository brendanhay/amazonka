{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-- |
-- Module      : Network.AWS.Data.Numeric
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Data.Numeric where

import           Control.Lens
import           Control.Monad
import           Data.Aeson.Types
import           Data.Data                   (Data, Typeable)
import           Data.Monoid
import           Data.Scientific
import           GHC.Generics                (Generic)
import           Network.AWS.Data.ByteString
import           Network.AWS.Data.Query
import           Network.AWS.Data.Text
import           Network.AWS.Data.XML
import           Numeric.Natural

import           Prelude

newtype Nat = Nat { unNat :: Natural }
    deriving
        ( Eq
        , Ord
        , Read
        , Show
        , Enum
        , Num
        , Real
        , Integral
        , Data
        , Typeable
        , Generic
        , ToByteString
        , FromText
        , ToText
        , FromXML
        , ToXML
        , ToQuery
        )

_Nat :: Iso' Nat Natural
_Nat = iso unNat Nat

instance FromJSON Nat where
    parseJSON = parseJSON >=> go
      where
        go n = case floatingOrInteger n of
            Left  (_ :: Double) -> fail (floatErr n)
            Right i
                | n < 0         -> fail (negateErr n)
                | otherwise     -> return . Nat $ fromInteger i

        floatErr  = mappend "Cannot convert float to Natural: " . show
        negateErr = mappend "Cannot convert negative number to Natural: " . show

instance ToJSON Nat where
    toJSON = Number . flip scientific 0 . toInteger . unNat
