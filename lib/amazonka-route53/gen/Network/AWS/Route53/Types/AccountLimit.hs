{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.AccountLimit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53.Types.AccountLimit
  ( AccountLimit (..)
  -- * Smart constructor
  , mkAccountLimit
  -- * Lenses
  , alType
  , alValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53.Internal as Types
import qualified Network.AWS.Route53.Types.AccountLimitType as Types

-- | A complex type that contains the type of limit that you specified in the request and the current value for that limit.
--
-- /See:/ 'mkAccountLimit' smart constructor.
data AccountLimit = AccountLimit'
  { type' :: Types.AccountLimitType
    -- ^ The limit that you requested. Valid values include the following:
--
--
--     * __MAX_HEALTH_CHECKS_BY_OWNER__ : The maximum number of health checks that you can create using the current account.
--
--
--     * __MAX_HOSTED_ZONES_BY_OWNER__ : The maximum number of hosted zones that you can create using the current account.
--
--
--     * __MAX_REUSABLE_DELEGATION_SETS_BY_OWNER__ : The maximum number of reusable delegation sets that you can create using the current account.
--
--
--     * __MAX_TRAFFIC_POLICIES_BY_OWNER__ : The maximum number of traffic policies that you can create using the current account.
--
--
--     * __MAX_TRAFFIC_POLICY_INSTANCES_BY_OWNER__ : The maximum number of traffic policy instances that you can create using the current account. (Traffic policy instances are referred to as traffic flow policy records in the Amazon Route 53 console.)
--
--
  , value :: Core.Natural
    -- ^ The current value for the limit that is specified by <https://docs.aws.amazon.com/Route53/latest/APIReference/API_AccountLimit.html#Route53-Type-AccountLimit-Type Type> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AccountLimit' value with any optional fields omitted.
mkAccountLimit
    :: Types.AccountLimitType -- ^ 'type\''
    -> Core.Natural -- ^ 'value'
    -> AccountLimit
mkAccountLimit type' value = AccountLimit'{type', value}

-- | The limit that you requested. Valid values include the following:
--
--
--     * __MAX_HEALTH_CHECKS_BY_OWNER__ : The maximum number of health checks that you can create using the current account.
--
--
--     * __MAX_HOSTED_ZONES_BY_OWNER__ : The maximum number of hosted zones that you can create using the current account.
--
--
--     * __MAX_REUSABLE_DELEGATION_SETS_BY_OWNER__ : The maximum number of reusable delegation sets that you can create using the current account.
--
--
--     * __MAX_TRAFFIC_POLICIES_BY_OWNER__ : The maximum number of traffic policies that you can create using the current account.
--
--
--     * __MAX_TRAFFIC_POLICY_INSTANCES_BY_OWNER__ : The maximum number of traffic policy instances that you can create using the current account. (Traffic policy instances are referred to as traffic flow policy records in the Amazon Route 53 console.)
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alType :: Lens.Lens' AccountLimit Types.AccountLimitType
alType = Lens.field @"type'"
{-# INLINEABLE alType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The current value for the limit that is specified by <https://docs.aws.amazon.com/Route53/latest/APIReference/API_AccountLimit.html#Route53-Type-AccountLimit-Type Type> .
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alValue :: Lens.Lens' AccountLimit Core.Natural
alValue = Lens.field @"value"
{-# INLINEABLE alValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromXML AccountLimit where
        parseXML x
          = AccountLimit' Core.<$>
              (x Core..@ "Type") Core.<*> x Core..@ "Value"
