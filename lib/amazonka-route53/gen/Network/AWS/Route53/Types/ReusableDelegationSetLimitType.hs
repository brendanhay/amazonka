{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.ReusableDelegationSetLimitType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.ReusableDelegationSetLimitType where

import Network.AWS.Prelude
import Network.AWS.Route53.Internal

data ReusableDelegationSetLimitType = MaxZonesByReusableDelegationSet
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

instance FromText ReusableDelegationSetLimitType where
  parser =
    takeLowerText >>= \case
      "max_zones_by_reusable_delegation_set" -> pure MaxZonesByReusableDelegationSet
      e ->
        fromTextError $
          "Failure parsing ReusableDelegationSetLimitType from value: '" <> e
            <> "'. Accepted values: max_zones_by_reusable_delegation_set"

instance ToText ReusableDelegationSetLimitType where
  toText = \case
    MaxZonesByReusableDelegationSet -> "MAX_ZONES_BY_REUSABLE_DELEGATION_SET"

instance Hashable ReusableDelegationSetLimitType

instance NFData ReusableDelegationSetLimitType

instance ToByteString ReusableDelegationSetLimitType

instance ToQuery ReusableDelegationSetLimitType

instance ToHeader ReusableDelegationSetLimitType

instance FromXML ReusableDelegationSetLimitType where
  parseXML = parseXMLText "ReusableDelegationSetLimitType"

instance ToXML ReusableDelegationSetLimitType where
  toXML = toXMLText
