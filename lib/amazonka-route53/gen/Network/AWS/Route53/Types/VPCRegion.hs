{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.VPCRegion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.VPCRegion where

import Network.AWS.Prelude
import Network.AWS.Route53.Internal

data VPCRegion
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
  | UsGovEast1
  | UsGovWest1
  | UsIsoEast1
  | UsIsobEast1
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

instance FromText VPCRegion where
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
      "us-gov-east-1" -> pure UsGovEast1
      "us-gov-west-1" -> pure UsGovWest1
      "us-iso-east-1" -> pure UsIsoEast1
      "us-isob-east-1" -> pure UsIsobEast1
      "us-west-1" -> pure UsWest1
      "us-west-2" -> pure UsWest2
      e ->
        fromTextError $
          "Failure parsing VPCRegion from value: '" <> e
            <> "'. Accepted values: af-south-1, ap-east-1, ap-northeast-1, ap-northeast-2, ap-northeast-3, ap-south-1, ap-southeast-1, ap-southeast-2, ca-central-1, cn-north-1, eu-central-1, eu-north-1, eu-south-1, eu-west-1, eu-west-2, eu-west-3, me-south-1, sa-east-1, us-east-1, us-east-2, us-gov-east-1, us-gov-west-1, us-iso-east-1, us-isob-east-1, us-west-1, us-west-2"

instance ToText VPCRegion where
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
    UsGovEast1 -> "us-gov-east-1"
    UsGovWest1 -> "us-gov-west-1"
    UsIsoEast1 -> "us-iso-east-1"
    UsIsobEast1 -> "us-isob-east-1"
    UsWest1 -> "us-west-1"
    UsWest2 -> "us-west-2"

instance Hashable VPCRegion

instance NFData VPCRegion

instance ToByteString VPCRegion

instance ToQuery VPCRegion

instance ToHeader VPCRegion

instance FromXML VPCRegion where
  parseXML = parseXMLText "VPCRegion"

instance ToXML VPCRegion where
  toXML = toXMLText
