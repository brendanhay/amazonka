{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.AccountLimit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.AccountLimit
  ( AccountLimit (..),

    -- * Smart constructor
    mkAccountLimit,

    -- * Lenses
    alValue,
    alType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53.Internal
import Network.AWS.Route53.Types.AccountLimitType

-- | A complex type that contains the type of limit that you specified in the request and the current value for that limit.
--
-- /See:/ 'mkAccountLimit' smart constructor.
data AccountLimit = AccountLimit'
  { -- | The current value for the limit that is specified by <https://docs.aws.amazon.com/Route53/latest/APIReference/API_AccountLimit.html#Route53-Type-AccountLimit-Type Type> .
    value :: Lude.Natural,
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
    type' :: AccountLimitType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AccountLimit' with the minimum fields required to make a request.
--
-- * 'value' - The current value for the limit that is specified by <https://docs.aws.amazon.com/Route53/latest/APIReference/API_AccountLimit.html#Route53-Type-AccountLimit-Type Type> .
-- * 'type'' - The limit that you requested. Valid values include the following:
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
mkAccountLimit ::
  -- | 'value'
  Lude.Natural ->
  -- | 'type''
  AccountLimitType ->
  AccountLimit
mkAccountLimit pValue_ pType_ =
  AccountLimit' {value = pValue_, type' = pType_}

-- | The current value for the limit that is specified by <https://docs.aws.amazon.com/Route53/latest/APIReference/API_AccountLimit.html#Route53-Type-AccountLimit-Type Type> .
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alValue :: Lens.Lens' AccountLimit Lude.Natural
alValue = Lens.lens (value :: AccountLimit -> Lude.Natural) (\s a -> s {value = a} :: AccountLimit)
{-# DEPRECATED alValue "Use generic-lens or generic-optics with 'value' instead." #-}

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
alType :: Lens.Lens' AccountLimit AccountLimitType
alType = Lens.lens (type' :: AccountLimit -> AccountLimitType) (\s a -> s {type' = a} :: AccountLimit)
{-# DEPRECATED alType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromXML AccountLimit where
  parseXML x =
    AccountLimit'
      Lude.<$> (x Lude..@ "Value") Lude.<*> (x Lude..@ "Type")
