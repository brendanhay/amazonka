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
    lbsState,
    lbsLoadBalancerName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the state of a Classic Load Balancer.
--
-- If you specify a load balancer when creating the Auto Scaling group, the state of the load balancer is @InService@ .
-- If you attach a load balancer to an existing Auto Scaling group, the initial state is @Adding@ . The state transitions to @Added@ after all instances in the group are registered with the load balancer. If Elastic Load Balancing health checks are enabled for the load balancer, the state transitions to @InService@ after at least one instance in the group passes the health check. If EC2 health checks are enabled instead, the load balancer remains in the @Added@ state.
--
-- /See:/ 'mkLoadBalancerState' smart constructor.
data LoadBalancerState = LoadBalancerState'
  { state ::
      Lude.Maybe Lude.Text,
    loadBalancerName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LoadBalancerState' with the minimum fields required to make a request.
--
-- * 'loadBalancerName' - The name of the load balancer.
-- * 'state' - One of the following load balancer states:
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
mkLoadBalancerState ::
  LoadBalancerState
mkLoadBalancerState =
  LoadBalancerState'
    { state = Lude.Nothing,
      loadBalancerName = Lude.Nothing
    }

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
lbsState :: Lens.Lens' LoadBalancerState (Lude.Maybe Lude.Text)
lbsState = Lens.lens (state :: LoadBalancerState -> Lude.Maybe Lude.Text) (\s a -> s {state = a} :: LoadBalancerState)
{-# DEPRECATED lbsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbsLoadBalancerName :: Lens.Lens' LoadBalancerState (Lude.Maybe Lude.Text)
lbsLoadBalancerName = Lens.lens (loadBalancerName :: LoadBalancerState -> Lude.Maybe Lude.Text) (\s a -> s {loadBalancerName = a} :: LoadBalancerState)
{-# DEPRECATED lbsLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

instance Lude.FromXML LoadBalancerState where
  parseXML x =
    LoadBalancerState'
      Lude.<$> (x Lude..@? "State") Lude.<*> (x Lude..@? "LoadBalancerName")
