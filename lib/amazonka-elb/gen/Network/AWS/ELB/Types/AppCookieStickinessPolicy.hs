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
    acspCookieName,
    acspPolicyName,
  )
where

import qualified Network.AWS.ELB.Internal as Types
import qualified Network.AWS.ELB.Types.CookieName as Types
import qualified Network.AWS.ELB.Types.PolicyName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a policy for application-controlled session stickiness.
--
-- /See:/ 'mkAppCookieStickinessPolicy' smart constructor.
data AppCookieStickinessPolicy = AppCookieStickinessPolicy'
  { -- | The name of the application cookie used for stickiness.
    cookieName :: Core.Maybe Types.CookieName,
    -- | The mnemonic name for the policy being created. The name must be unique within a set of policies for this load balancer.
    policyName :: Core.Maybe Types.PolicyName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AppCookieStickinessPolicy' value with any optional fields omitted.
mkAppCookieStickinessPolicy ::
  AppCookieStickinessPolicy
mkAppCookieStickinessPolicy =
  AppCookieStickinessPolicy'
    { cookieName = Core.Nothing,
      policyName = Core.Nothing
    }

-- | The name of the application cookie used for stickiness.
--
-- /Note:/ Consider using 'cookieName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acspCookieName :: Lens.Lens' AppCookieStickinessPolicy (Core.Maybe Types.CookieName)
acspCookieName = Lens.field @"cookieName"
{-# DEPRECATED acspCookieName "Use generic-lens or generic-optics with 'cookieName' instead." #-}

-- | The mnemonic name for the policy being created. The name must be unique within a set of policies for this load balancer.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acspPolicyName :: Lens.Lens' AppCookieStickinessPolicy (Core.Maybe Types.PolicyName)
acspPolicyName = Lens.field @"policyName"
{-# DEPRECATED acspPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

instance Core.FromXML AppCookieStickinessPolicy where
  parseXML x =
    AppCookieStickinessPolicy'
      Core.<$> (x Core..@? "CookieName") Core.<*> (x Core..@? "PolicyName")
