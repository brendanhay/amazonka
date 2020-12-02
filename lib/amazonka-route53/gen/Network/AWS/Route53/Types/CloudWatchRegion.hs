{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.CloudWatchRegion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.CloudWatchRegion where

import Network.AWS.Prelude
import Network.AWS.Route53.Internal

data CloudWatchRegion
  = CWRAfSouth1
  | CWRApEast1
  | CWRApNortheast1
  | CWRApNortheast2
  | CWRApNortheast3
  | CWRApSouth1
  | CWRApSoutheast1
  | CWRApSoutheast2
  | CWRCaCentral1
  | CWRCnNorth1
  | CWRCnNorthwest1
  | CWREuCentral1
  | CWREuNorth1
  | CWREuSouth1
  | CWREuWest1
  | CWREuWest2
  | CWREuWest3
  | CWRMeSouth1
  | CWRSaEast1
  | CWRUsEast1
  | CWRUsEast2
  | CWRUsGovEast1
  | CWRUsGovWest1
  | CWRUsIsoEast1
  | CWRUsIsobEast1
  | CWRUsWest1
  | CWRUsWest2
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

instance FromText CloudWatchRegion where
  parser =
    takeLowerText >>= \case
      "af-south-1" -> pure CWRAfSouth1
      "ap-east-1" -> pure CWRApEast1
      "ap-northeast-1" -> pure CWRApNortheast1
      "ap-northeast-2" -> pure CWRApNortheast2
      "ap-northeast-3" -> pure CWRApNortheast3
      "ap-south-1" -> pure CWRApSouth1
      "ap-southeast-1" -> pure CWRApSoutheast1
      "ap-southeast-2" -> pure CWRApSoutheast2
      "ca-central-1" -> pure CWRCaCentral1
      "cn-north-1" -> pure CWRCnNorth1
      "cn-northwest-1" -> pure CWRCnNorthwest1
      "eu-central-1" -> pure CWREuCentral1
      "eu-north-1" -> pure CWREuNorth1
      "eu-south-1" -> pure CWREuSouth1
      "eu-west-1" -> pure CWREuWest1
      "eu-west-2" -> pure CWREuWest2
      "eu-west-3" -> pure CWREuWest3
      "me-south-1" -> pure CWRMeSouth1
      "sa-east-1" -> pure CWRSaEast1
      "us-east-1" -> pure CWRUsEast1
      "us-east-2" -> pure CWRUsEast2
      "us-gov-east-1" -> pure CWRUsGovEast1
      "us-gov-west-1" -> pure CWRUsGovWest1
      "us-iso-east-1" -> pure CWRUsIsoEast1
      "us-isob-east-1" -> pure CWRUsIsobEast1
      "us-west-1" -> pure CWRUsWest1
      "us-west-2" -> pure CWRUsWest2
      e ->
        fromTextError $
          "Failure parsing CloudWatchRegion from value: '" <> e
            <> "'. Accepted values: af-south-1, ap-east-1, ap-northeast-1, ap-northeast-2, ap-northeast-3, ap-south-1, ap-southeast-1, ap-southeast-2, ca-central-1, cn-north-1, cn-northwest-1, eu-central-1, eu-north-1, eu-south-1, eu-west-1, eu-west-2, eu-west-3, me-south-1, sa-east-1, us-east-1, us-east-2, us-gov-east-1, us-gov-west-1, us-iso-east-1, us-isob-east-1, us-west-1, us-west-2"

instance ToText CloudWatchRegion where
  toText = \case
    CWRAfSouth1 -> "af-south-1"
    CWRApEast1 -> "ap-east-1"
    CWRApNortheast1 -> "ap-northeast-1"
    CWRApNortheast2 -> "ap-northeast-2"
    CWRApNortheast3 -> "ap-northeast-3"
    CWRApSouth1 -> "ap-south-1"
    CWRApSoutheast1 -> "ap-southeast-1"
    CWRApSoutheast2 -> "ap-southeast-2"
    CWRCaCentral1 -> "ca-central-1"
    CWRCnNorth1 -> "cn-north-1"
    CWRCnNorthwest1 -> "cn-northwest-1"
    CWREuCentral1 -> "eu-central-1"
    CWREuNorth1 -> "eu-north-1"
    CWREuSouth1 -> "eu-south-1"
    CWREuWest1 -> "eu-west-1"
    CWREuWest2 -> "eu-west-2"
    CWREuWest3 -> "eu-west-3"
    CWRMeSouth1 -> "me-south-1"
    CWRSaEast1 -> "sa-east-1"
    CWRUsEast1 -> "us-east-1"
    CWRUsEast2 -> "us-east-2"
    CWRUsGovEast1 -> "us-gov-east-1"
    CWRUsGovWest1 -> "us-gov-west-1"
    CWRUsIsoEast1 -> "us-iso-east-1"
    CWRUsIsobEast1 -> "us-isob-east-1"
    CWRUsWest1 -> "us-west-1"
    CWRUsWest2 -> "us-west-2"

instance Hashable CloudWatchRegion

instance NFData CloudWatchRegion

instance ToByteString CloudWatchRegion

instance ToQuery CloudWatchRegion

instance ToHeader CloudWatchRegion

instance FromXML CloudWatchRegion where
  parseXML = parseXMLText "CloudWatchRegion"

instance ToXML CloudWatchRegion where
  toXML = toXMLText
