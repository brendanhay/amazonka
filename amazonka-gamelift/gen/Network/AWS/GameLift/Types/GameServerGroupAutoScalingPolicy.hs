{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameServerGroupAutoScalingPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameServerGroupAutoScalingPolicy where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types.TargetTrackingConfiguration
import qualified Network.AWS.Lens as Lens

-- | __This data type is used with the Amazon GameLift FleetIQ and game
-- server groups.__
--
-- Configuration settings for intelligent automatic scaling that uses
-- target tracking. These settings are used to add an Auto Scaling policy
-- when creating the corresponding Auto Scaling group with
-- CreateGameServerGroup. After the Auto Scaling group is created, all
-- updates to Auto Scaling policies, including changing this policy and
-- adding or removing other policies, is done directly on the Auto Scaling
-- group.
--
-- /See:/ 'newGameServerGroupAutoScalingPolicy' smart constructor.
data GameServerGroupAutoScalingPolicy = GameServerGroupAutoScalingPolicy'
  { -- | Length of time, in seconds, it takes for a new instance to start new
    -- game server processes and register with GameLift FleetIQ. Specifying a
    -- warm-up time can be useful, particularly with game servers that take a
    -- long time to start up, because it avoids prematurely starting new
    -- instances.
    estimatedInstanceWarmup :: Core.Maybe Core.Natural,
    -- | Settings for a target-based scaling policy applied to Auto Scaling
    -- group. These settings are used to create a target-based policy that
    -- tracks the GameLift FleetIQ metric @\"PercentUtilizedGameServers\"@ and
    -- specifies a target value for the metric. As player usage changes, the
    -- policy triggers to adjust the game server group capacity so that the
    -- metric returns to the target value.
    targetTrackingConfiguration :: TargetTrackingConfiguration
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GameServerGroupAutoScalingPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'estimatedInstanceWarmup', 'gameServerGroupAutoScalingPolicy_estimatedInstanceWarmup' - Length of time, in seconds, it takes for a new instance to start new
-- game server processes and register with GameLift FleetIQ. Specifying a
-- warm-up time can be useful, particularly with game servers that take a
-- long time to start up, because it avoids prematurely starting new
-- instances.
--
-- 'targetTrackingConfiguration', 'gameServerGroupAutoScalingPolicy_targetTrackingConfiguration' - Settings for a target-based scaling policy applied to Auto Scaling
-- group. These settings are used to create a target-based policy that
-- tracks the GameLift FleetIQ metric @\"PercentUtilizedGameServers\"@ and
-- specifies a target value for the metric. As player usage changes, the
-- policy triggers to adjust the game server group capacity so that the
-- metric returns to the target value.
newGameServerGroupAutoScalingPolicy ::
  -- | 'targetTrackingConfiguration'
  TargetTrackingConfiguration ->
  GameServerGroupAutoScalingPolicy
newGameServerGroupAutoScalingPolicy
  pTargetTrackingConfiguration_ =
    GameServerGroupAutoScalingPolicy'
      { estimatedInstanceWarmup =
          Core.Nothing,
        targetTrackingConfiguration =
          pTargetTrackingConfiguration_
      }

-- | Length of time, in seconds, it takes for a new instance to start new
-- game server processes and register with GameLift FleetIQ. Specifying a
-- warm-up time can be useful, particularly with game servers that take a
-- long time to start up, because it avoids prematurely starting new
-- instances.
gameServerGroupAutoScalingPolicy_estimatedInstanceWarmup :: Lens.Lens' GameServerGroupAutoScalingPolicy (Core.Maybe Core.Natural)
gameServerGroupAutoScalingPolicy_estimatedInstanceWarmup = Lens.lens (\GameServerGroupAutoScalingPolicy' {estimatedInstanceWarmup} -> estimatedInstanceWarmup) (\s@GameServerGroupAutoScalingPolicy' {} a -> s {estimatedInstanceWarmup = a} :: GameServerGroupAutoScalingPolicy)

-- | Settings for a target-based scaling policy applied to Auto Scaling
-- group. These settings are used to create a target-based policy that
-- tracks the GameLift FleetIQ metric @\"PercentUtilizedGameServers\"@ and
-- specifies a target value for the metric. As player usage changes, the
-- policy triggers to adjust the game server group capacity so that the
-- metric returns to the target value.
gameServerGroupAutoScalingPolicy_targetTrackingConfiguration :: Lens.Lens' GameServerGroupAutoScalingPolicy TargetTrackingConfiguration
gameServerGroupAutoScalingPolicy_targetTrackingConfiguration = Lens.lens (\GameServerGroupAutoScalingPolicy' {targetTrackingConfiguration} -> targetTrackingConfiguration) (\s@GameServerGroupAutoScalingPolicy' {} a -> s {targetTrackingConfiguration = a} :: GameServerGroupAutoScalingPolicy)

instance
  Core.Hashable
    GameServerGroupAutoScalingPolicy

instance Core.NFData GameServerGroupAutoScalingPolicy

instance Core.ToJSON GameServerGroupAutoScalingPolicy where
  toJSON GameServerGroupAutoScalingPolicy' {..} =
    Core.object
      ( Core.catMaybes
          [ ("EstimatedInstanceWarmup" Core..=)
              Core.<$> estimatedInstanceWarmup,
            Core.Just
              ( "TargetTrackingConfiguration"
                  Core..= targetTrackingConfiguration
              )
          ]
      )
