{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.AccountLimitType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53.Types.AccountLimitType
  ( AccountLimitType
    ( AccountLimitType'
    , AccountLimitTypeMaxHealthChecksByOwner
    , AccountLimitTypeMaxHostedZonesByOwner
    , AccountLimitTypeMaxTrafficPolicyInstancesByOwner
    , AccountLimitTypeMaxReusableDelegationSetsByOwner
    , AccountLimitTypeMaxTrafficPoliciesByOwner
    , fromAccountLimitType
    )
  ) where

import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53.Internal as Types

newtype AccountLimitType = AccountLimitType'{fromAccountLimitType
                                             :: Core.Text}
                             deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                             Core.Generic)
                             deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                               Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                               Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                               Core.FromText, Core.ToByteString, Core.ToQuery,
                                               Core.ToHeader)

pattern AccountLimitTypeMaxHealthChecksByOwner :: AccountLimitType
pattern AccountLimitTypeMaxHealthChecksByOwner = AccountLimitType' "MAX_HEALTH_CHECKS_BY_OWNER"

pattern AccountLimitTypeMaxHostedZonesByOwner :: AccountLimitType
pattern AccountLimitTypeMaxHostedZonesByOwner = AccountLimitType' "MAX_HOSTED_ZONES_BY_OWNER"

pattern AccountLimitTypeMaxTrafficPolicyInstancesByOwner :: AccountLimitType
pattern AccountLimitTypeMaxTrafficPolicyInstancesByOwner = AccountLimitType' "MAX_TRAFFIC_POLICY_INSTANCES_BY_OWNER"

pattern AccountLimitTypeMaxReusableDelegationSetsByOwner :: AccountLimitType
pattern AccountLimitTypeMaxReusableDelegationSetsByOwner = AccountLimitType' "MAX_REUSABLE_DELEGATION_SETS_BY_OWNER"

pattern AccountLimitTypeMaxTrafficPoliciesByOwner :: AccountLimitType
pattern AccountLimitTypeMaxTrafficPoliciesByOwner = AccountLimitType' "MAX_TRAFFIC_POLICIES_BY_OWNER"

{-# COMPLETE 
  AccountLimitTypeMaxHealthChecksByOwner,

  AccountLimitTypeMaxHostedZonesByOwner,

  AccountLimitTypeMaxTrafficPolicyInstancesByOwner,

  AccountLimitTypeMaxReusableDelegationSetsByOwner,

  AccountLimitTypeMaxTrafficPoliciesByOwner,
  AccountLimitType'
  #-}
