{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.Policies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELB.Types.Policies
  ( Policies (..)
  -- * Smart constructor
  , mkPolicies
  -- * Lenses
  , pAppCookieStickinessPolicies
  , pLBCookieStickinessPolicies
  , pOtherPolicies
  ) where

import qualified Network.AWS.ELB.Internal as Types
import qualified Network.AWS.ELB.Types.AppCookieStickinessPolicy as Types
import qualified Network.AWS.ELB.Types.LBCookieStickinessPolicy as Types
import qualified Network.AWS.ELB.Types.PolicyName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The policies for a load balancer.
--
-- /See:/ 'mkPolicies' smart constructor.
data Policies = Policies'
  { appCookieStickinessPolicies :: Core.Maybe [Types.AppCookieStickinessPolicy]
    -- ^ The stickiness policies created using 'CreateAppCookieStickinessPolicy' .
  , lBCookieStickinessPolicies :: Core.Maybe [Types.LBCookieStickinessPolicy]
    -- ^ The stickiness policies created using 'CreateLBCookieStickinessPolicy' .
  , otherPolicies :: Core.Maybe [Types.PolicyName]
    -- ^ The policies other than the stickiness policies.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Policies' value with any optional fields omitted.
mkPolicies
    :: Policies
mkPolicies
  = Policies'{appCookieStickinessPolicies = Core.Nothing,
              lBCookieStickinessPolicies = Core.Nothing,
              otherPolicies = Core.Nothing}

-- | The stickiness policies created using 'CreateAppCookieStickinessPolicy' .
--
-- /Note:/ Consider using 'appCookieStickinessPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pAppCookieStickinessPolicies :: Lens.Lens' Policies (Core.Maybe [Types.AppCookieStickinessPolicy])
pAppCookieStickinessPolicies = Lens.field @"appCookieStickinessPolicies"
{-# INLINEABLE pAppCookieStickinessPolicies #-}
{-# DEPRECATED appCookieStickinessPolicies "Use generic-lens or generic-optics with 'appCookieStickinessPolicies' instead"  #-}

-- | The stickiness policies created using 'CreateLBCookieStickinessPolicy' .
--
-- /Note:/ Consider using 'lBCookieStickinessPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pLBCookieStickinessPolicies :: Lens.Lens' Policies (Core.Maybe [Types.LBCookieStickinessPolicy])
pLBCookieStickinessPolicies = Lens.field @"lBCookieStickinessPolicies"
{-# INLINEABLE pLBCookieStickinessPolicies #-}
{-# DEPRECATED lBCookieStickinessPolicies "Use generic-lens or generic-optics with 'lBCookieStickinessPolicies' instead"  #-}

-- | The policies other than the stickiness policies.
--
-- /Note:/ Consider using 'otherPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pOtherPolicies :: Lens.Lens' Policies (Core.Maybe [Types.PolicyName])
pOtherPolicies = Lens.field @"otherPolicies"
{-# INLINEABLE pOtherPolicies #-}
{-# DEPRECATED otherPolicies "Use generic-lens or generic-optics with 'otherPolicies' instead"  #-}

instance Core.FromXML Policies where
        parseXML x
          = Policies' Core.<$>
              (x Core..@? "AppCookieStickinessPolicies" Core..<@>
                 Core.parseXMLList "member")
                Core.<*>
                x Core..@? "LBCookieStickinessPolicies" Core..<@>
                  Core.parseXMLList "member"
                Core.<*>
                x Core..@? "OtherPolicies" Core..<@> Core.parseXMLList "member"
