{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.LoadBalancerTargetGroupState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.LoadBalancerTargetGroupState
  ( LoadBalancerTargetGroupState (..),

    -- * Smart constructor
    mkLoadBalancerTargetGroupState,

    -- * Lenses
    lbtgsState,
    lbtgsLoadBalancerTargetGroupARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the state of a target group.
--
-- If you attach a target group to an existing Auto Scaling group, the initial state is @Adding@ . The state transitions to @Added@ after all Auto Scaling instances are registered with the target group. If Elastic Load Balancing health checks are enabled, the state transitions to @InService@ after at least one Auto Scaling instance passes the health check. If EC2 health checks are enabled instead, the target group remains in the @Added@ state.
--
-- /See:/ 'mkLoadBalancerTargetGroupState' smart constructor.
data LoadBalancerTargetGroupState = LoadBalancerTargetGroupState'
  { -- | The state of the target group.
    --
    --
    --     * @Adding@ - The Auto Scaling instances are being registered with the target group.
    --
    --
    --     * @Added@ - All Auto Scaling instances are registered with the target group.
    --
    --
    --     * @InService@ - At least one Auto Scaling instance passed an ELB health check.
    --
    --
    --     * @Removing@ - The Auto Scaling instances are being deregistered from the target group. If connection draining is enabled, Elastic Load Balancing waits for in-flight requests to complete before deregistering the instances.
    --
    --
    --     * @Removed@ - All Auto Scaling instances are deregistered from the target group.
    state :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the target group.
    loadBalancerTargetGroupARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LoadBalancerTargetGroupState' with the minimum fields required to make a request.
--
-- * 'state' - The state of the target group.
--
--
--     * @Adding@ - The Auto Scaling instances are being registered with the target group.
--
--
--     * @Added@ - All Auto Scaling instances are registered with the target group.
--
--
--     * @InService@ - At least one Auto Scaling instance passed an ELB health check.
--
--
--     * @Removing@ - The Auto Scaling instances are being deregistered from the target group. If connection draining is enabled, Elastic Load Balancing waits for in-flight requests to complete before deregistering the instances.
--
--
--     * @Removed@ - All Auto Scaling instances are deregistered from the target group.
--
--
-- * 'loadBalancerTargetGroupARN' - The Amazon Resource Name (ARN) of the target group.
mkLoadBalancerTargetGroupState ::
  LoadBalancerTargetGroupState
mkLoadBalancerTargetGroupState =
  LoadBalancerTargetGroupState'
    { state = Lude.Nothing,
      loadBalancerTargetGroupARN = Lude.Nothing
    }

-- | The state of the target group.
--
--
--     * @Adding@ - The Auto Scaling instances are being registered with the target group.
--
--
--     * @Added@ - All Auto Scaling instances are registered with the target group.
--
--
--     * @InService@ - At least one Auto Scaling instance passed an ELB health check.
--
--
--     * @Removing@ - The Auto Scaling instances are being deregistered from the target group. If connection draining is enabled, Elastic Load Balancing waits for in-flight requests to complete before deregistering the instances.
--
--
--     * @Removed@ - All Auto Scaling instances are deregistered from the target group.
--
--
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtgsState :: Lens.Lens' LoadBalancerTargetGroupState (Lude.Maybe Lude.Text)
lbtgsState = Lens.lens (state :: LoadBalancerTargetGroupState -> Lude.Maybe Lude.Text) (\s a -> s {state = a} :: LoadBalancerTargetGroupState)
{-# DEPRECATED lbtgsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The Amazon Resource Name (ARN) of the target group.
--
-- /Note:/ Consider using 'loadBalancerTargetGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtgsLoadBalancerTargetGroupARN :: Lens.Lens' LoadBalancerTargetGroupState (Lude.Maybe Lude.Text)
lbtgsLoadBalancerTargetGroupARN = Lens.lens (loadBalancerTargetGroupARN :: LoadBalancerTargetGroupState -> Lude.Maybe Lude.Text) (\s a -> s {loadBalancerTargetGroupARN = a} :: LoadBalancerTargetGroupState)
{-# DEPRECATED lbtgsLoadBalancerTargetGroupARN "Use generic-lens or generic-optics with 'loadBalancerTargetGroupARN' instead." #-}

instance Lude.FromXML LoadBalancerTargetGroupState where
  parseXML x =
    LoadBalancerTargetGroupState'
      Lude.<$> (x Lude..@? "State")
      Lude.<*> (x Lude..@? "LoadBalancerTargetGroupARN")
