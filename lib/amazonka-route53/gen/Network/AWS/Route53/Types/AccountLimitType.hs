-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.AccountLimitType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.AccountLimitType
  ( AccountLimitType
      ( AccountLimitType',
        MaxHealthChecksByOwner,
        MaxHostedZonesByOwner,
        MaxReusableDelegationSetsByOwner,
        MaxTrafficPoliciesByOwner,
        MaxTrafficPolicyInstancesByOwner
      ),
  )
where

import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53.Internal

newtype AccountLimitType = AccountLimitType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern MaxHealthChecksByOwner :: AccountLimitType
pattern MaxHealthChecksByOwner = AccountLimitType' "MAX_HEALTH_CHECKS_BY_OWNER"

pattern MaxHostedZonesByOwner :: AccountLimitType
pattern MaxHostedZonesByOwner = AccountLimitType' "MAX_HOSTED_ZONES_BY_OWNER"

pattern MaxReusableDelegationSetsByOwner :: AccountLimitType
pattern MaxReusableDelegationSetsByOwner = AccountLimitType' "MAX_REUSABLE_DELEGATION_SETS_BY_OWNER"

pattern MaxTrafficPoliciesByOwner :: AccountLimitType
pattern MaxTrafficPoliciesByOwner = AccountLimitType' "MAX_TRAFFIC_POLICIES_BY_OWNER"

pattern MaxTrafficPolicyInstancesByOwner :: AccountLimitType
pattern MaxTrafficPolicyInstancesByOwner = AccountLimitType' "MAX_TRAFFIC_POLICY_INSTANCES_BY_OWNER"

{-# COMPLETE
  MaxHealthChecksByOwner,
  MaxHostedZonesByOwner,
  MaxReusableDelegationSetsByOwner,
  MaxTrafficPoliciesByOwner,
  MaxTrafficPolicyInstancesByOwner,
  AccountLimitType'
  #-}
