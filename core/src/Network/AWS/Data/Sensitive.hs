{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Network.AWS.Data.Sensitive
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Data.Sensitive where

import           Control.Lens
import           Data.Data                   (Data, Typeable)
import           Data.Monoid                 (Monoid)
import           Data.String
import           GHC.Generics                (Generic)
import           Network.AWS.Data.ByteString
import           Network.AWS.Data.JSON
import           Network.AWS.Data.Query
import           Network.AWS.Data.Text
import           Network.AWS.Data.XML

-- | /Note/: read . show /= isomorphic
newtype Sensitive a = Sensitive { desensitise :: a }
    deriving
        ( Eq
        , Ord
        , Read
        , IsString
        , Monoid
        , Data
        , Typeable
        , Generic
        , ToByteString
        , FromText
        , ToText
        , FromXML
        , ToXML
        , ToQuery
        , ToJSON
        , FromJSON
        )

instance Show (Sensitive a) where
    show = const "******"

_Sensitive :: Iso' (Sensitive a) a
_Sensitive = iso desensitise Sensitive
