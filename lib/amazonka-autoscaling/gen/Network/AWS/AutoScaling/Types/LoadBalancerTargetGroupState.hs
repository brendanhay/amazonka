{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.LoadBalancerTargetGroupState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScaling.Types.LoadBalancerTargetGroupState
  ( LoadBalancerTargetGroupState (..)
  -- * Smart constructor
  , mkLoadBalancerTargetGroupState
  -- * Lenses
  , lbtgsLoadBalancerTargetGroupARN
  , lbtgsState
  ) where

import qualified Network.AWS.AutoScaling.Types.XmlStringMaxLen255 as Types
import qualified Network.AWS.AutoScaling.Types.XmlStringMaxLen511 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the state of a target group.
--
-- If you attach a target group to an existing Auto Scaling group, the initial state is @Adding@ . The state transitions to @Added@ after all Auto Scaling instances are registered with the target group. If Elastic Load Balancing health checks are enabled, the state transitions to @InService@ after at least one Auto Scaling instance passes the health check. If EC2 health checks are enabled instead, the target group remains in the @Added@ state.
--
-- /See:/ 'mkLoadBalancerTargetGroupState' smart constructor.
data LoadBalancerTargetGroupState = LoadBalancerTargetGroupState'
  { loadBalancerTargetGroupARN :: Core.Maybe Types.XmlStringMaxLen511
    -- ^ The Amazon Resource Name (ARN) of the target group.
  , state :: Core.Maybe Types.XmlStringMaxLen255
    -- ^ The state of the target group.
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LoadBalancerTargetGroupState' value with any optional fields omitted.
mkLoadBalancerTargetGroupState
    :: LoadBalancerTargetGroupState
mkLoadBalancerTargetGroupState
  = LoadBalancerTargetGroupState'{loadBalancerTargetGroupARN =
                                    Core.Nothing,
                                  state = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the target group.
--
-- /Note:/ Consider using 'loadBalancerTargetGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtgsLoadBalancerTargetGroupARN :: Lens.Lens' LoadBalancerTargetGroupState (Core.Maybe Types.XmlStringMaxLen511)
lbtgsLoadBalancerTargetGroupARN = Lens.field @"loadBalancerTargetGroupARN"
{-# INLINEABLE lbtgsLoadBalancerTargetGroupARN #-}
{-# DEPRECATED loadBalancerTargetGroupARN "Use generic-lens or generic-optics with 'loadBalancerTargetGroupARN' instead"  #-}

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
lbtgsState :: Lens.Lens' LoadBalancerTargetGroupState (Core.Maybe Types.XmlStringMaxLen255)
lbtgsState = Lens.field @"state"
{-# INLINEABLE lbtgsState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

instance Core.FromXML LoadBalancerTargetGroupState where
        parseXML x
          = LoadBalancerTargetGroupState' Core.<$>
              (x Core..@? "LoadBalancerTargetGroupARN") Core.<*>
                x Core..@? "State"
