{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.HostedZoneLimitType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.HostedZoneLimitType where

import Network.AWS.Prelude
import Network.AWS.Route53.Internal

data HostedZoneLimitType
  = MaxRrsetsByZone
  | MaxVPCsAssociatedByZone
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

instance FromText HostedZoneLimitType where
  parser =
    takeLowerText >>= \case
      "max_rrsets_by_zone" -> pure MaxRrsetsByZone
      "max_vpcs_associated_by_zone" -> pure MaxVPCsAssociatedByZone
      e ->
        fromTextError $
          "Failure parsing HostedZoneLimitType from value: '" <> e
            <> "'. Accepted values: max_rrsets_by_zone, max_vpcs_associated_by_zone"

instance ToText HostedZoneLimitType where
  toText = \case
    MaxRrsetsByZone -> "MAX_RRSETS_BY_ZONE"
    MaxVPCsAssociatedByZone -> "MAX_VPCS_ASSOCIATED_BY_ZONE"

instance Hashable HostedZoneLimitType

instance NFData HostedZoneLimitType

instance ToByteString HostedZoneLimitType

instance ToQuery HostedZoneLimitType

instance ToHeader HostedZoneLimitType

instance FromXML HostedZoneLimitType where
  parseXML = parseXMLText "HostedZoneLimitType"

instance ToXML HostedZoneLimitType where
  toXML = toXMLText
