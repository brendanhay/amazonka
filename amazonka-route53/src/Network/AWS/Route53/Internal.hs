{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Network.AWS.Route53.Internal
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53.Internal
    ( Region     (..)
    , ResourceId (..)
    ) where

import Data.String

import Network.AWS.Data.Log
import Network.AWS.Data.XML
import Network.AWS.Prelude
import Network.AWS.Types    (Region (..))

import qualified Data.Text as Text

-- | A Route53 identifier for resources such as hosted zones and delegation sets.
--
-- Since Route53 outputs prefixed resource identifiers such as
-- @/hostedzone/ABC123@, but expects unprefixed identifiers as inputs, such as
-- @ABC123@, the @FromXML@ instance will strip this prefix take care to ensure
-- the correct input format is observed and @decodeXML . encodeXML == id@
-- holds.
newtype ResourceId = ResourceId { fromResourceId :: Text }
    deriving
        ( Eq
        , Ord
        , Read
        , Show
        , Data
        , Typeable
        , Generic
        , IsString
        , FromText
        , ToText
        , ToByteString
        , ToXML
        , ToQuery
        , ToLog
        )

instance Hashable ResourceId
instance NFData   ResourceId

-- | Handles prefixed Route53 resource identifiers.
instance FromXML ResourceId where
    parseXML = fmap (ResourceId . Text.takeWhileEnd (/= '/')) . parseXML
