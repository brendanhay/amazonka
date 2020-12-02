{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.AccountLimitType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.AccountLimitType where

import Network.AWS.Prelude
import Network.AWS.Route53.Internal

data AccountLimitType
  = MaxHealthChecksByOwner
  | MaxHostedZonesByOwner
  | MaxReusableDelegationSetsByOwner
  | MaxTrafficPoliciesByOwner
  | MaxTrafficPolicyInstancesByOwner
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

instance FromText AccountLimitType where
  parser =
    takeLowerText >>= \case
      "max_health_checks_by_owner" -> pure MaxHealthChecksByOwner
      "max_hosted_zones_by_owner" -> pure MaxHostedZonesByOwner
      "max_reusable_delegation_sets_by_owner" -> pure MaxReusableDelegationSetsByOwner
      "max_traffic_policies_by_owner" -> pure MaxTrafficPoliciesByOwner
      "max_traffic_policy_instances_by_owner" -> pure MaxTrafficPolicyInstancesByOwner
      e ->
        fromTextError $
          "Failure parsing AccountLimitType from value: '" <> e
            <> "'. Accepted values: max_health_checks_by_owner, max_hosted_zones_by_owner, max_reusable_delegation_sets_by_owner, max_traffic_policies_by_owner, max_traffic_policy_instances_by_owner"

instance ToText AccountLimitType where
  toText = \case
    MaxHealthChecksByOwner -> "MAX_HEALTH_CHECKS_BY_OWNER"
    MaxHostedZonesByOwner -> "MAX_HOSTED_ZONES_BY_OWNER"
    MaxReusableDelegationSetsByOwner -> "MAX_REUSABLE_DELEGATION_SETS_BY_OWNER"
    MaxTrafficPoliciesByOwner -> "MAX_TRAFFIC_POLICIES_BY_OWNER"
    MaxTrafficPolicyInstancesByOwner -> "MAX_TRAFFIC_POLICY_INSTANCES_BY_OWNER"

instance Hashable AccountLimitType

instance NFData AccountLimitType

instance ToByteString AccountLimitType

instance ToQuery AccountLimitType

instance ToHeader AccountLimitType

instance FromXML AccountLimitType where
  parseXML = parseXMLText "AccountLimitType"

instance ToXML AccountLimitType where
  toXML = toXMLText
