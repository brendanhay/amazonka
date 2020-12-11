-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameServerGroupAutoScalingPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameServerGroupAutoScalingPolicy
  ( GameServerGroupAutoScalingPolicy (..),

    -- * Smart constructor
    mkGameServerGroupAutoScalingPolicy,

    -- * Lenses
    gsgaspEstimatedInstanceWarmup,
    gsgaspTargetTrackingConfiguration,
  )
where

import Network.AWS.GameLift.Types.TargetTrackingConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | __This data type is used with the Amazon GameLift FleetIQ and game server groups.__
--
-- Configuration settings for intelligent automatic scaling that uses target tracking. These settings are used to add an Auto Scaling policy when creating the corresponding Auto Scaling group with 'CreateGameServerGroup' . After the Auto Scaling group is created, all updates to Auto Scaling policies, including changing this policy and adding or removing other policies, is done directly on the Auto Scaling group.
--
-- /See:/ 'mkGameServerGroupAutoScalingPolicy' smart constructor.
data GameServerGroupAutoScalingPolicy = GameServerGroupAutoScalingPolicy'
  { estimatedInstanceWarmup ::
      Lude.Maybe Lude.Natural,
    targetTrackingConfiguration ::
      TargetTrackingConfiguration
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GameServerGroupAutoScalingPolicy' with the minimum fields required to make a request.
--
-- * 'estimatedInstanceWarmup' - Length of time, in seconds, it takes for a new instance to start new game server processes and register with GameLift FleetIQ. Specifying a warm-up time can be useful, particularly with game servers that take a long time to start up, because it avoids prematurely starting new instances.
-- * 'targetTrackingConfiguration' - Settings for a target-based scaling policy applied to Auto Scaling group. These settings are used to create a target-based policy that tracks the GameLift FleetIQ metric @"PercentUtilizedGameServers"@ and specifies a target value for the metric. As player usage changes, the policy triggers to adjust the game server group capacity so that the metric returns to the target value.
mkGameServerGroupAutoScalingPolicy ::
  -- | 'targetTrackingConfiguration'
  TargetTrackingConfiguration ->
  GameServerGroupAutoScalingPolicy
mkGameServerGroupAutoScalingPolicy pTargetTrackingConfiguration_ =
  GameServerGroupAutoScalingPolicy'
    { estimatedInstanceWarmup =
        Lude.Nothing,
      targetTrackingConfiguration = pTargetTrackingConfiguration_
    }

-- | Length of time, in seconds, it takes for a new instance to start new game server processes and register with GameLift FleetIQ. Specifying a warm-up time can be useful, particularly with game servers that take a long time to start up, because it avoids prematurely starting new instances.
--
-- /Note:/ Consider using 'estimatedInstanceWarmup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgaspEstimatedInstanceWarmup :: Lens.Lens' GameServerGroupAutoScalingPolicy (Lude.Maybe Lude.Natural)
gsgaspEstimatedInstanceWarmup = Lens.lens (estimatedInstanceWarmup :: GameServerGroupAutoScalingPolicy -> Lude.Maybe Lude.Natural) (\s a -> s {estimatedInstanceWarmup = a} :: GameServerGroupAutoScalingPolicy)
{-# DEPRECATED gsgaspEstimatedInstanceWarmup "Use generic-lens or generic-optics with 'estimatedInstanceWarmup' instead." #-}

-- | Settings for a target-based scaling policy applied to Auto Scaling group. These settings are used to create a target-based policy that tracks the GameLift FleetIQ metric @"PercentUtilizedGameServers"@ and specifies a target value for the metric. As player usage changes, the policy triggers to adjust the game server group capacity so that the metric returns to the target value.
--
-- /Note:/ Consider using 'targetTrackingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgaspTargetTrackingConfiguration :: Lens.Lens' GameServerGroupAutoScalingPolicy TargetTrackingConfiguration
gsgaspTargetTrackingConfiguration = Lens.lens (targetTrackingConfiguration :: GameServerGroupAutoScalingPolicy -> TargetTrackingConfiguration) (\s a -> s {targetTrackingConfiguration = a} :: GameServerGroupAutoScalingPolicy)
{-# DEPRECATED gsgaspTargetTrackingConfiguration "Use generic-lens or generic-optics with 'targetTrackingConfiguration' instead." #-}

instance Lude.ToJSON GameServerGroupAutoScalingPolicy where
  toJSON GameServerGroupAutoScalingPolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("EstimatedInstanceWarmup" Lude..=)
              Lude.<$> estimatedInstanceWarmup,
            Lude.Just
              ( "TargetTrackingConfiguration"
                  Lude..= targetTrackingConfiguration
              )
          ]
      )
