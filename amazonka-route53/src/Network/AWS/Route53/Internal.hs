{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

{-# OPTIONS_GHC -Wall -Werror #-}

-- |
-- Module      : Network.AWS.Route53.Internal
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53.Internal
    ( Region     (..)
    , ResourceId (..)

      -- * Website Endpoints
    , getHostedZoneId
    ) where

import Data.String (IsString)

import Network.AWS.Data.Log (ToLog)
import Network.AWS.Prelude

import qualified Data.Text as Text

-- | A Route53 identifier for resources such as hosted zones and delegation sets.
--
-- Since Route53 outputs prefixed resource identifiers such as
-- @/hostedzone/ABC123@, but expects unprefixed identifiers as inputs, such as
-- @ABC123@, the 'FromXML' instance will strip this prefix take care to ensure
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

-- | Get the hosted zone identifier for an S3 website endpoint.
--
-- When you configure your bucket as a website, the website is available using
-- a region-specific website endpoint. This hosted zone identifier is used
-- adding an alias record to the website to your hosted zone.
--
-- /See:/ <http://docs.aws.amazon.com/general/latest/gr/rande.html#s3_website_region_endpoints Amazon Simple Storage Service Website Endpoints>.
getHostedZoneId :: Region -> Maybe ResourceId
getHostedZoneId = \case
    NorthVirginia   -> Just "Z3AQBSTGFYJSTF"
    Ohio            -> Just "Z2O1EMRO9K5GLX"
    NorthCalifornia -> Just "Z2F56UZL2M1ACD"
    Oregon          -> Just "Z3BJ6K6RIION7M"
    Montreal        -> Just "Z1QDHH18159H29"
    Tokyo           -> Just "Z2M4EHUR26P7ZW"
    Seoul           -> Just "Z3W03O7B5YMIYP"
    Mumbai          -> Just "Z11RGJOFQNVJUP"
    Singapore       -> Just "Z3O0J2DXBE1FTB"
    Sydney          -> Just "Z1WCIGYICN2BYD"
    SaoPaulo        -> Just "Z7KQH4QJS55SO"
    Ireland         -> Just "Z1BKCTXD74EZPE"
    London          -> Just "Z3GKZC51ZF0DB4"
    Frankfurt       -> Just "Z21DNDUVLTQW6Q"
    GovCloud        -> Just "Z31GFT0UA1I2HV"
    GovCloudFIPS    -> Just "Z31GFT0UA1I2HV"
    Beijing         -> Nothing
