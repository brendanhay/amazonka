{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.LBCookieStickinessPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.LBCookieStickinessPolicy
  ( LBCookieStickinessPolicy (..),

    -- * Smart constructor
    mkLBCookieStickinessPolicy,

    -- * Lenses
    lbcspCookieExpirationPeriod,
    lbcspPolicyName,
  )
where

import qualified Network.AWS.ELB.Internal as Types
import qualified Network.AWS.ELB.Types.PolicyName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a policy for duration-based session stickiness.
--
-- /See:/ 'mkLBCookieStickinessPolicy' smart constructor.
data LBCookieStickinessPolicy = LBCookieStickinessPolicy'
  { -- | The time period, in seconds, after which the cookie should be considered stale. If this parameter is not specified, the stickiness session lasts for the duration of the browser session.
    cookieExpirationPeriod :: Core.Maybe Core.Integer,
    -- | The name of the policy. This name must be unique within the set of policies for this load balancer.
    policyName :: Core.Maybe Types.PolicyName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LBCookieStickinessPolicy' value with any optional fields omitted.
mkLBCookieStickinessPolicy ::
  LBCookieStickinessPolicy
mkLBCookieStickinessPolicy =
  LBCookieStickinessPolicy'
    { cookieExpirationPeriod = Core.Nothing,
      policyName = Core.Nothing
    }

-- | The time period, in seconds, after which the cookie should be considered stale. If this parameter is not specified, the stickiness session lasts for the duration of the browser session.
--
-- /Note:/ Consider using 'cookieExpirationPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbcspCookieExpirationPeriod :: Lens.Lens' LBCookieStickinessPolicy (Core.Maybe Core.Integer)
lbcspCookieExpirationPeriod = Lens.field @"cookieExpirationPeriod"
{-# DEPRECATED lbcspCookieExpirationPeriod "Use generic-lens or generic-optics with 'cookieExpirationPeriod' instead." #-}

-- | The name of the policy. This name must be unique within the set of policies for this load balancer.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbcspPolicyName :: Lens.Lens' LBCookieStickinessPolicy (Core.Maybe Types.PolicyName)
lbcspPolicyName = Lens.field @"policyName"
{-# DEPRECATED lbcspPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

instance Core.FromXML LBCookieStickinessPolicy where
  parseXML x =
    LBCookieStickinessPolicy'
      Core.<$> (x Core..@? "CookieExpirationPeriod")
      Core.<*> (x Core..@? "PolicyName")
