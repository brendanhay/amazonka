{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.AppCookieStickinessPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.AppCookieStickinessPolicy
  ( AppCookieStickinessPolicy (..),

    -- * Smart constructor
    mkAppCookieStickinessPolicy,

    -- * Lenses
    acspPolicyName,
    acspCookieName,
  )
where

import Network.AWS.ELB.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a policy for application-controlled session stickiness.
--
-- /See:/ 'mkAppCookieStickinessPolicy' smart constructor.
data AppCookieStickinessPolicy = AppCookieStickinessPolicy'
  { policyName ::
      Lude.Maybe Lude.Text,
    cookieName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AppCookieStickinessPolicy' with the minimum fields required to make a request.
--
-- * 'cookieName' - The name of the application cookie used for stickiness.
-- * 'policyName' - The mnemonic name for the policy being created. The name must be unique within a set of policies for this load balancer.
mkAppCookieStickinessPolicy ::
  AppCookieStickinessPolicy
mkAppCookieStickinessPolicy =
  AppCookieStickinessPolicy'
    { policyName = Lude.Nothing,
      cookieName = Lude.Nothing
    }

-- | The mnemonic name for the policy being created. The name must be unique within a set of policies for this load balancer.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acspPolicyName :: Lens.Lens' AppCookieStickinessPolicy (Lude.Maybe Lude.Text)
acspPolicyName = Lens.lens (policyName :: AppCookieStickinessPolicy -> Lude.Maybe Lude.Text) (\s a -> s {policyName = a} :: AppCookieStickinessPolicy)
{-# DEPRECATED acspPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The name of the application cookie used for stickiness.
--
-- /Note:/ Consider using 'cookieName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acspCookieName :: Lens.Lens' AppCookieStickinessPolicy (Lude.Maybe Lude.Text)
acspCookieName = Lens.lens (cookieName :: AppCookieStickinessPolicy -> Lude.Maybe Lude.Text) (\s a -> s {cookieName = a} :: AppCookieStickinessPolicy)
{-# DEPRECATED acspCookieName "Use generic-lens or generic-optics with 'cookieName' instead." #-}

instance Lude.FromXML AppCookieStickinessPolicy where
  parseXML x =
    AppCookieStickinessPolicy'
      Lude.<$> (x Lude..@? "PolicyName") Lude.<*> (x Lude..@? "CookieName")
