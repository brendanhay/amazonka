{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

-- |
-- Module      : Network.AWS.Data.Sensitive
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Data.Sensitive where

import           Control.DeepSeq
import           Data.Data                   (Data, Typeable)
import           Data.Hashable
import           Data.Monoid                 (Monoid)
import           Data.Semigroup              (Semigroup)
import           Data.String

import           GHC.Generics                (Generic)

import           Network.AWS.Data.Headers
import           Network.AWS.Data.ByteString
import           Network.AWS.Data.JSON
import           Network.AWS.Data.Log        (ToLog (..))
import           Network.AWS.Data.Query
import           Network.AWS.Data.Text
import           Network.AWS.Data.XML
import           Network.AWS.Lens            (Iso', iso)

-- | /Note/: read . show /= isomorphic
newtype Sensitive a = Sensitive { desensitise :: a }
    deriving
        ( Eq
        , Ord
        , IsString
        , Semigroup
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
        , ToHeader
        )

instance Show  (Sensitive a) where show  = const "******"
instance ToLog (Sensitive a) where build = const "******"

instance Hashable a => Hashable (Sensitive a)
instance NFData   a => NFData   (Sensitive a)

_Sensitive :: Iso' (Sensitive a) a
_Sensitive = iso desensitise Sensitive
