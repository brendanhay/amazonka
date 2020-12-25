{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.LoadBalancerState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.LoadBalancerState
  ( LoadBalancerState (..),

    -- * Smart constructor
    mkLoadBalancerState,

    -- * Lenses
    lbsLoadBalancerName,
    lbsState,
  )
where

import qualified Network.AWS.AutoScaling.Types.XmlStringMaxLen255 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the state of a Classic Load Balancer.
--
-- If you specify a load balancer when creating the Auto Scaling group, the state of the load balancer is @InService@ .
-- If you attach a load balancer to an existing Auto Scaling group, the initial state is @Adding@ . The state transitions to @Added@ after all instances in the group are registered with the load balancer. If Elastic Load Balancing health checks are enabled for the load balancer, the state transitions to @InService@ after at least one instance in the group passes the health check. If EC2 health checks are enabled instead, the load balancer remains in the @Added@ state.
--
-- /See:/ 'mkLoadBalancerState' smart constructor.
data LoadBalancerState = LoadBalancerState'
  { -- | The name of the load balancer.
    loadBalancerName :: Core.Maybe Types.XmlStringMaxLen255,
    -- | One of the following load balancer states:
    --
    --
    --     * @Adding@ - The instances in the group are being registered with the load balancer.
    --
    --
    --     * @Added@ - All instances in the group are registered with the load balancer.
    --
    --
    --     * @InService@ - At least one instance in the group passed an ELB health check.
    --
    --
    --     * @Removing@ - The instances in the group are being deregistered from the load balancer. If connection draining is enabled, Elastic Load Balancing waits for in-flight requests to complete before deregistering the instances.
    --
    --
    --     * @Removed@ - All instances in the group are deregistered from the load balancer.
    state :: Core.Maybe Types.XmlStringMaxLen255
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LoadBalancerState' value with any optional fields omitted.
mkLoadBalancerState ::
  LoadBalancerState
mkLoadBalancerState =
  LoadBalancerState'
    { loadBalancerName = Core.Nothing,
      state = Core.Nothing
    }

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbsLoadBalancerName :: Lens.Lens' LoadBalancerState (Core.Maybe Types.XmlStringMaxLen255)
lbsLoadBalancerName = Lens.field @"loadBalancerName"
{-# DEPRECATED lbsLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

-- | One of the following load balancer states:
--
--
--     * @Adding@ - The instances in the group are being registered with the load balancer.
--
--
--     * @Added@ - All instances in the group are registered with the load balancer.
--
--
--     * @InService@ - At least one instance in the group passed an ELB health check.
--
--
--     * @Removing@ - The instances in the group are being deregistered from the load balancer. If connection draining is enabled, Elastic Load Balancing waits for in-flight requests to complete before deregistering the instances.
--
--
--     * @Removed@ - All instances in the group are deregistered from the load balancer.
--
--
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbsState :: Lens.Lens' LoadBalancerState (Core.Maybe Types.XmlStringMaxLen255)
lbsState = Lens.field @"state"
{-# DEPRECATED lbsState "Use generic-lens or generic-optics with 'state' instead." #-}

instance Core.FromXML LoadBalancerState where
  parseXML x =
    LoadBalancerState'
      Core.<$> (x Core..@? "LoadBalancerName") Core.<*> (x Core..@? "State")
