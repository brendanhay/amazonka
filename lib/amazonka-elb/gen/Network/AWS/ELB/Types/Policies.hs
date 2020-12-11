-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.Policies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.Policies
  ( Policies (..),

    -- * Smart constructor
    mkPolicies,

    -- * Lenses
    pOtherPolicies,
    pLBCookieStickinessPolicies,
    pAppCookieStickinessPolicies,
  )
where

import Network.AWS.ELB.Internal
import Network.AWS.ELB.Types.AppCookieStickinessPolicy
import Network.AWS.ELB.Types.LBCookieStickinessPolicy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The policies for a load balancer.
--
-- /See:/ 'mkPolicies' smart constructor.
data Policies = Policies'
  { otherPolicies :: Lude.Maybe [Lude.Text],
    lBCookieStickinessPolicies ::
      Lude.Maybe [LBCookieStickinessPolicy],
    appCookieStickinessPolicies ::
      Lude.Maybe [AppCookieStickinessPolicy]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Policies' with the minimum fields required to make a request.
--
-- * 'appCookieStickinessPolicies' - The stickiness policies created using 'CreateAppCookieStickinessPolicy' .
-- * 'lBCookieStickinessPolicies' - The stickiness policies created using 'CreateLBCookieStickinessPolicy' .
-- * 'otherPolicies' - The policies other than the stickiness policies.
mkPolicies ::
  Policies
mkPolicies =
  Policies'
    { otherPolicies = Lude.Nothing,
      lBCookieStickinessPolicies = Lude.Nothing,
      appCookieStickinessPolicies = Lude.Nothing
    }

-- | The policies other than the stickiness policies.
--
-- /Note:/ Consider using 'otherPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pOtherPolicies :: Lens.Lens' Policies (Lude.Maybe [Lude.Text])
pOtherPolicies = Lens.lens (otherPolicies :: Policies -> Lude.Maybe [Lude.Text]) (\s a -> s {otherPolicies = a} :: Policies)
{-# DEPRECATED pOtherPolicies "Use generic-lens or generic-optics with 'otherPolicies' instead." #-}

-- | The stickiness policies created using 'CreateLBCookieStickinessPolicy' .
--
-- /Note:/ Consider using 'lBCookieStickinessPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pLBCookieStickinessPolicies :: Lens.Lens' Policies (Lude.Maybe [LBCookieStickinessPolicy])
pLBCookieStickinessPolicies = Lens.lens (lBCookieStickinessPolicies :: Policies -> Lude.Maybe [LBCookieStickinessPolicy]) (\s a -> s {lBCookieStickinessPolicies = a} :: Policies)
{-# DEPRECATED pLBCookieStickinessPolicies "Use generic-lens or generic-optics with 'lBCookieStickinessPolicies' instead." #-}

-- | The stickiness policies created using 'CreateAppCookieStickinessPolicy' .
--
-- /Note:/ Consider using 'appCookieStickinessPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pAppCookieStickinessPolicies :: Lens.Lens' Policies (Lude.Maybe [AppCookieStickinessPolicy])
pAppCookieStickinessPolicies = Lens.lens (appCookieStickinessPolicies :: Policies -> Lude.Maybe [AppCookieStickinessPolicy]) (\s a -> s {appCookieStickinessPolicies = a} :: Policies)
{-# DEPRECATED pAppCookieStickinessPolicies "Use generic-lens or generic-optics with 'appCookieStickinessPolicies' instead." #-}

instance Lude.FromXML Policies where
  parseXML x =
    Policies'
      Lude.<$> ( x Lude..@? "OtherPolicies" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> ( x Lude..@? "LBCookieStickinessPolicies" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> ( x Lude..@? "AppCookieStickinessPolicies" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
