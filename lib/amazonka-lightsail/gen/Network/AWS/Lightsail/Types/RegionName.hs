{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.RegionName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.RegionName where

import Network.AWS.Prelude

data RegionName
  = ApNortheast1
  | ApNortheast2
  | ApSouth1
  | ApSoutheast1
  | ApSoutheast2
  | CaCentral1
  | EuCentral1
  | EuWest1
  | EuWest2
  | EuWest3
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

instance FromText RegionName where
  parser =
    takeLowerText >>= \case
      "ap-northeast-1" -> pure ApNortheast1
      "ap-northeast-2" -> pure ApNortheast2
      "ap-south-1" -> pure ApSouth1
      "ap-southeast-1" -> pure ApSoutheast1
      "ap-southeast-2" -> pure ApSoutheast2
      "ca-central-1" -> pure CaCentral1
      "eu-central-1" -> pure EuCentral1
      "eu-west-1" -> pure EuWest1
      "eu-west-2" -> pure EuWest2
      "eu-west-3" -> pure EuWest3
      "us-east-1" -> pure UsEast1
      "us-east-2" -> pure UsEast2
      "us-west-1" -> pure UsWest1
      "us-west-2" -> pure UsWest2
      e ->
        fromTextError $
          "Failure parsing RegionName from value: '" <> e
            <> "'. Accepted values: ap-northeast-1, ap-northeast-2, ap-south-1, ap-southeast-1, ap-southeast-2, ca-central-1, eu-central-1, eu-west-1, eu-west-2, eu-west-3, us-east-1, us-east-2, us-west-1, us-west-2"

instance ToText RegionName where
  toText = \case
    ApNortheast1 -> "ap-northeast-1"
    ApNortheast2 -> "ap-northeast-2"
    ApSouth1 -> "ap-south-1"
    ApSoutheast1 -> "ap-southeast-1"
    ApSoutheast2 -> "ap-southeast-2"
    CaCentral1 -> "ca-central-1"
    EuCentral1 -> "eu-central-1"
    EuWest1 -> "eu-west-1"
    EuWest2 -> "eu-west-2"
    EuWest3 -> "eu-west-3"
    UsEast1 -> "us-east-1"
    UsEast2 -> "us-east-2"
    UsWest1 -> "us-west-1"
    UsWest2 -> "us-west-2"

instance Hashable RegionName

instance NFData RegionName

instance ToByteString RegionName

instance ToQuery RegionName

instance ToHeader RegionName

instance ToJSON RegionName where
  toJSON = toJSONText

instance FromJSON RegionName where
  parseJSON = parseJSONText "RegionName"
