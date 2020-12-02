{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.HealthCheckRegion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.HealthCheckRegion where

import Network.AWS.Prelude
import Network.AWS.Route53.Internal

data HealthCheckRegion
  = HCRApNortheast1
  | HCRApSoutheast1
  | HCRApSoutheast2
  | HCREuWest1
  | HCRSaEast1
  | HCRUsEast1
  | HCRUsWest1
  | HCRUsWest2
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText HealthCheckRegion where
  parser =
    takeLowerText >>= \case
      "ap-northeast-1" -> pure HCRApNortheast1
      "ap-southeast-1" -> pure HCRApSoutheast1
      "ap-southeast-2" -> pure HCRApSoutheast2
      "eu-west-1" -> pure HCREuWest1
      "sa-east-1" -> pure HCRSaEast1
      "us-east-1" -> pure HCRUsEast1
      "us-west-1" -> pure HCRUsWest1
      "us-west-2" -> pure HCRUsWest2
      e ->
        fromTextError $
          "Failure parsing HealthCheckRegion from value: '" <> e
            <> "'. Accepted values: ap-northeast-1, ap-southeast-1, ap-southeast-2, eu-west-1, sa-east-1, us-east-1, us-west-1, us-west-2"

instance ToText HealthCheckRegion where
  toText = \case
    HCRApNortheast1 -> "ap-northeast-1"
    HCRApSoutheast1 -> "ap-southeast-1"
    HCRApSoutheast2 -> "ap-southeast-2"
    HCREuWest1 -> "eu-west-1"
    HCRSaEast1 -> "sa-east-1"
    HCRUsEast1 -> "us-east-1"
    HCRUsWest1 -> "us-west-1"
    HCRUsWest2 -> "us-west-2"

instance Hashable HealthCheckRegion

instance NFData HealthCheckRegion

instance ToByteString HealthCheckRegion

instance ToQuery HealthCheckRegion

instance ToHeader HealthCheckRegion

instance FromXML HealthCheckRegion where
  parseXML = parseXMLText "HealthCheckRegion"

instance ToXML HealthCheckRegion where
  toXML = toXMLText
