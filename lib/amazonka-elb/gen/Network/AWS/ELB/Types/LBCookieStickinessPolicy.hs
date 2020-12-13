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
    lbcspPolicyName,
    lbcspCookieExpirationPeriod,
  )
where

import Network.AWS.ELB.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a policy for duration-based session stickiness.
--
-- /See:/ 'mkLBCookieStickinessPolicy' smart constructor.
data LBCookieStickinessPolicy = LBCookieStickinessPolicy'
  { -- | The name of the policy. This name must be unique within the set of policies for this load balancer.
    policyName :: Lude.Maybe Lude.Text,
    -- | The time period, in seconds, after which the cookie should be considered stale. If this parameter is not specified, the stickiness session lasts for the duration of the browser session.
    cookieExpirationPeriod :: Lude.Maybe Lude.Integer
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LBCookieStickinessPolicy' with the minimum fields required to make a request.
--
-- * 'policyName' - The name of the policy. This name must be unique within the set of policies for this load balancer.
-- * 'cookieExpirationPeriod' - The time period, in seconds, after which the cookie should be considered stale. If this parameter is not specified, the stickiness session lasts for the duration of the browser session.
mkLBCookieStickinessPolicy ::
  LBCookieStickinessPolicy
mkLBCookieStickinessPolicy =
  LBCookieStickinessPolicy'
    { policyName = Lude.Nothing,
      cookieExpirationPeriod = Lude.Nothing
    }

-- | The name of the policy. This name must be unique within the set of policies for this load balancer.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbcspPolicyName :: Lens.Lens' LBCookieStickinessPolicy (Lude.Maybe Lude.Text)
lbcspPolicyName = Lens.lens (policyName :: LBCookieStickinessPolicy -> Lude.Maybe Lude.Text) (\s a -> s {policyName = a} :: LBCookieStickinessPolicy)
{-# DEPRECATED lbcspPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The time period, in seconds, after which the cookie should be considered stale. If this parameter is not specified, the stickiness session lasts for the duration of the browser session.
--
-- /Note:/ Consider using 'cookieExpirationPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbcspCookieExpirationPeriod :: Lens.Lens' LBCookieStickinessPolicy (Lude.Maybe Lude.Integer)
lbcspCookieExpirationPeriod = Lens.lens (cookieExpirationPeriod :: LBCookieStickinessPolicy -> Lude.Maybe Lude.Integer) (\s a -> s {cookieExpirationPeriod = a} :: LBCookieStickinessPolicy)
{-# DEPRECATED lbcspCookieExpirationPeriod "Use generic-lens or generic-optics with 'cookieExpirationPeriod' instead." #-}

instance Lude.FromXML LBCookieStickinessPolicy where
  parseXML x =
    LBCookieStickinessPolicy'
      Lude.<$> (x Lude..@? "PolicyName")
      Lude.<*> (x Lude..@? "CookieExpirationPeriod")
