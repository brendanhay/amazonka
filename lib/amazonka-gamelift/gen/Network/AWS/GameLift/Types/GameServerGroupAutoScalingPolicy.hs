{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameServerGroupAutoScalingPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.GameServerGroupAutoScalingPolicy
  ( GameServerGroupAutoScalingPolicy (..)
  -- * Smart constructor
  , mkGameServerGroupAutoScalingPolicy
  -- * Lenses
  , gsgaspTargetTrackingConfiguration
  , gsgaspEstimatedInstanceWarmup
  ) where

import qualified Network.AWS.GameLift.Types.TargetTrackingConfiguration as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | __This data type is used with the Amazon GameLift FleetIQ and game server groups.__ 
--
-- Configuration settings for intelligent automatic scaling that uses target tracking. These settings are used to add an Auto Scaling policy when creating the corresponding Auto Scaling group with 'CreateGameServerGroup' . After the Auto Scaling group is created, all updates to Auto Scaling policies, including changing this policy and adding or removing other policies, is done directly on the Auto Scaling group. 
--
-- /See:/ 'mkGameServerGroupAutoScalingPolicy' smart constructor.
data GameServerGroupAutoScalingPolicy = GameServerGroupAutoScalingPolicy'
  { targetTrackingConfiguration :: Types.TargetTrackingConfiguration
    -- ^ Settings for a target-based scaling policy applied to Auto Scaling group. These settings are used to create a target-based policy that tracks the GameLift FleetIQ metric @"PercentUtilizedGameServers"@ and specifies a target value for the metric. As player usage changes, the policy triggers to adjust the game server group capacity so that the metric returns to the target value. 
  , estimatedInstanceWarmup :: Core.Maybe Core.Natural
    -- ^ Length of time, in seconds, it takes for a new instance to start new game server processes and register with GameLift FleetIQ. Specifying a warm-up time can be useful, particularly with game servers that take a long time to start up, because it avoids prematurely starting new instances. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GameServerGroupAutoScalingPolicy' value with any optional fields omitted.
mkGameServerGroupAutoScalingPolicy
    :: Types.TargetTrackingConfiguration -- ^ 'targetTrackingConfiguration'
    -> GameServerGroupAutoScalingPolicy
mkGameServerGroupAutoScalingPolicy targetTrackingConfiguration
  = GameServerGroupAutoScalingPolicy'{targetTrackingConfiguration,
                                      estimatedInstanceWarmup = Core.Nothing}

-- | Settings for a target-based scaling policy applied to Auto Scaling group. These settings are used to create a target-based policy that tracks the GameLift FleetIQ metric @"PercentUtilizedGameServers"@ and specifies a target value for the metric. As player usage changes, the policy triggers to adjust the game server group capacity so that the metric returns to the target value. 
--
-- /Note:/ Consider using 'targetTrackingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgaspTargetTrackingConfiguration :: Lens.Lens' GameServerGroupAutoScalingPolicy Types.TargetTrackingConfiguration
gsgaspTargetTrackingConfiguration = Lens.field @"targetTrackingConfiguration"
{-# INLINEABLE gsgaspTargetTrackingConfiguration #-}
{-# DEPRECATED targetTrackingConfiguration "Use generic-lens or generic-optics with 'targetTrackingConfiguration' instead"  #-}

-- | Length of time, in seconds, it takes for a new instance to start new game server processes and register with GameLift FleetIQ. Specifying a warm-up time can be useful, particularly with game servers that take a long time to start up, because it avoids prematurely starting new instances. 
--
-- /Note:/ Consider using 'estimatedInstanceWarmup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgaspEstimatedInstanceWarmup :: Lens.Lens' GameServerGroupAutoScalingPolicy (Core.Maybe Core.Natural)
gsgaspEstimatedInstanceWarmup = Lens.field @"estimatedInstanceWarmup"
{-# INLINEABLE gsgaspEstimatedInstanceWarmup #-}
{-# DEPRECATED estimatedInstanceWarmup "Use generic-lens or generic-optics with 'estimatedInstanceWarmup' instead"  #-}

instance Core.FromJSON GameServerGroupAutoScalingPolicy where
        toJSON GameServerGroupAutoScalingPolicy{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("TargetTrackingConfiguration" Core..=
                       targetTrackingConfiguration),
                  ("EstimatedInstanceWarmup" Core..=) Core.<$>
                    estimatedInstanceWarmup])
