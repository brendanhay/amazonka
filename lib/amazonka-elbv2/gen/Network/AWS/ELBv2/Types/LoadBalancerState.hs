{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.LoadBalancerState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.LoadBalancerState
  ( LoadBalancerState (..),

    -- * Smart constructor
    mkLoadBalancerState,

    -- * Lenses
    lbsCode,
    lbsReason,
  )
where

import qualified Network.AWS.ELBv2.Types.LoadBalancerStateEnum as Types
import qualified Network.AWS.ELBv2.Types.Reason as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the state of the load balancer.
--
-- /See:/ 'mkLoadBalancerState' smart constructor.
data LoadBalancerState = LoadBalancerState'
  { -- | The state code. The initial state of the load balancer is @provisioning@ . After the load balancer is fully set up and ready to route traffic, its state is @active@ . If the load balancer could not be set up, its state is @failed@ .
    code :: Core.Maybe Types.LoadBalancerStateEnum,
    -- | A description of the state.
    reason :: Core.Maybe Types.Reason
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LoadBalancerState' value with any optional fields omitted.
mkLoadBalancerState ::
  LoadBalancerState
mkLoadBalancerState =
  LoadBalancerState' {code = Core.Nothing, reason = Core.Nothing}

-- | The state code. The initial state of the load balancer is @provisioning@ . After the load balancer is fully set up and ready to route traffic, its state is @active@ . If the load balancer could not be set up, its state is @failed@ .
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbsCode :: Lens.Lens' LoadBalancerState (Core.Maybe Types.LoadBalancerStateEnum)
lbsCode = Lens.field @"code"
{-# DEPRECATED lbsCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | A description of the state.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbsReason :: Lens.Lens' LoadBalancerState (Core.Maybe Types.Reason)
lbsReason = Lens.field @"reason"
{-# DEPRECATED lbsReason "Use generic-lens or generic-optics with 'reason' instead." #-}

instance Core.FromXML LoadBalancerState where
  parseXML x =
    LoadBalancerState'
      Core.<$> (x Core..@? "Code") Core.<*> (x Core..@? "Reason")
