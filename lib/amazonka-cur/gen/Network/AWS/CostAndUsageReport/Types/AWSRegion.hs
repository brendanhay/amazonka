{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostAndUsageReport.Types.AWSRegion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostAndUsageReport.Types.AWSRegion where

import Network.AWS.Prelude

-- | The region of the S3 bucket that AWS delivers the report into.
data AWSRegion
  = AfSouth1
  | ApEast1
  | ApNortheast1
  | ApNortheast2
  | ApNortheast3
  | ApSouth1
  | ApSoutheast1
  | ApSoutheast2
  | CaCentral1
  | CnNorth1
  | CnNorthwest1
  | EuCentral1
  | EuNorth1
  | EuSouth1
  | EuWest1
  | EuWest2
  | EuWest3
  | MeSouth1
  | SaEast1
  | UsEast1
  | UsEast2
  | UsWest1
  | UsWest2
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

instance FromText AWSRegion where
  parser =
    takeLowerText >>= \case
      "af-south-1" -> pure AfSouth1
      "ap-east-1" -> pure ApEast1
      "ap-northeast-1" -> pure ApNortheast1
      "ap-northeast-2" -> pure ApNortheast2
      "ap-northeast-3" -> pure ApNortheast3
      "ap-south-1" -> pure ApSouth1
      "ap-southeast-1" -> pure ApSoutheast1
      "ap-southeast-2" -> pure ApSoutheast2
      "ca-central-1" -> pure CaCentral1
      "cn-north-1" -> pure CnNorth1
      "cn-northwest-1" -> pure CnNorthwest1
      "eu-central-1" -> pure EuCentral1
      "eu-north-1" -> pure EuNorth1
      "eu-south-1" -> pure EuSouth1
      "eu-west-1" -> pure EuWest1
      "eu-west-2" -> pure EuWest2
      "eu-west-3" -> pure EuWest3
      "me-south-1" -> pure MeSouth1
      "sa-east-1" -> pure SaEast1
      "us-east-1" -> pure UsEast1
      "us-east-2" -> pure UsEast2
      "us-west-1" -> pure UsWest1
      "us-west-2" -> pure UsWest2
      e ->
        fromTextError $
          "Failure parsing AWSRegion from value: '" <> e
            <> "'. Accepted values: af-south-1, ap-east-1, ap-northeast-1, ap-northeast-2, ap-northeast-3, ap-south-1, ap-southeast-1, ap-southeast-2, ca-central-1, cn-north-1, cn-northwest-1, eu-central-1, eu-north-1, eu-south-1, eu-west-1, eu-west-2, eu-west-3, me-south-1, sa-east-1, us-east-1, us-east-2, us-west-1, us-west-2"

instance ToText AWSRegion where
  toText = \case
    AfSouth1 -> "af-south-1"
    ApEast1 -> "ap-east-1"
    ApNortheast1 -> "ap-northeast-1"
    ApNortheast2 -> "ap-northeast-2"
    ApNortheast3 -> "ap-northeast-3"
    ApSouth1 -> "ap-south-1"
    ApSoutheast1 -> "ap-southeast-1"
    ApSoutheast2 -> "ap-southeast-2"
    CaCentral1 -> "ca-central-1"
    CnNorth1 -> "cn-north-1"
    CnNorthwest1 -> "cn-northwest-1"
    EuCentral1 -> "eu-central-1"
    EuNorth1 -> "eu-north-1"
    EuSouth1 -> "eu-south-1"
    EuWest1 -> "eu-west-1"
    EuWest2 -> "eu-west-2"
    EuWest3 -> "eu-west-3"
    MeSouth1 -> "me-south-1"
    SaEast1 -> "sa-east-1"
    UsEast1 -> "us-east-1"
    UsEast2 -> "us-east-2"
    UsWest1 -> "us-west-1"
    UsWest2 -> "us-west-2"

instance Hashable AWSRegion

instance NFData AWSRegion

instance ToByteString AWSRegion

instance ToQuery AWSRegion

instance ToHeader AWSRegion

instance ToJSON AWSRegion where
  toJSON = toJSONText

instance FromJSON AWSRegion where
  parseJSON = parseJSONText "AWSRegion"
