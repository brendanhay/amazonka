{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameServerGroupAutoScalingPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameServerGroupAutoScalingPolicy where

import Network.AWS.GameLift.Types.TargetTrackingConfiguration
import Network.AWS.Lens
import Network.AWS.Prelude

-- | __This data type is used with the Amazon GameLift FleetIQ and game server groups.__
--
--
-- Configuration settings for intelligent automatic scaling that uses target tracking. These settings are used to add an Auto Scaling policy when creating the corresponding Auto Scaling group with 'CreateGameServerGroup' . After the Auto Scaling group is created, all updates to Auto Scaling policies, including changing this policy and adding or removing other policies, is done directly on the Auto Scaling group.
--
--
-- /See:/ 'gameServerGroupAutoScalingPolicy' smart constructor.
data GameServerGroupAutoScalingPolicy = GameServerGroupAutoScalingPolicy'
  { _gsgaspEstimatedInstanceWarmup ::
      !(Maybe Nat),
    _gsgaspTargetTrackingConfiguration ::
      !TargetTrackingConfiguration
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GameServerGroupAutoScalingPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsgaspEstimatedInstanceWarmup' - Length of time, in seconds, it takes for a new instance to start new game server processes and register with GameLift FleetIQ. Specifying a warm-up time can be useful, particularly with game servers that take a long time to start up, because it avoids prematurely starting new instances.
--
-- * 'gsgaspTargetTrackingConfiguration' - Settings for a target-based scaling policy applied to Auto Scaling group. These settings are used to create a target-based policy that tracks the GameLift FleetIQ metric @"PercentUtilizedGameServers"@ and specifies a target value for the metric. As player usage changes, the policy triggers to adjust the game server group capacity so that the metric returns to the target value.
gameServerGroupAutoScalingPolicy ::
  -- | 'gsgaspTargetTrackingConfiguration'
  TargetTrackingConfiguration ->
  GameServerGroupAutoScalingPolicy
gameServerGroupAutoScalingPolicy pTargetTrackingConfiguration_ =
  GameServerGroupAutoScalingPolicy'
    { _gsgaspEstimatedInstanceWarmup =
        Nothing,
      _gsgaspTargetTrackingConfiguration =
        pTargetTrackingConfiguration_
    }

-- | Length of time, in seconds, it takes for a new instance to start new game server processes and register with GameLift FleetIQ. Specifying a warm-up time can be useful, particularly with game servers that take a long time to start up, because it avoids prematurely starting new instances.
gsgaspEstimatedInstanceWarmup :: Lens' GameServerGroupAutoScalingPolicy (Maybe Natural)
gsgaspEstimatedInstanceWarmup = lens _gsgaspEstimatedInstanceWarmup (\s a -> s {_gsgaspEstimatedInstanceWarmup = a}) . mapping _Nat

-- | Settings for a target-based scaling policy applied to Auto Scaling group. These settings are used to create a target-based policy that tracks the GameLift FleetIQ metric @"PercentUtilizedGameServers"@ and specifies a target value for the metric. As player usage changes, the policy triggers to adjust the game server group capacity so that the metric returns to the target value.
gsgaspTargetTrackingConfiguration :: Lens' GameServerGroupAutoScalingPolicy TargetTrackingConfiguration
gsgaspTargetTrackingConfiguration = lens _gsgaspTargetTrackingConfiguration (\s a -> s {_gsgaspTargetTrackingConfiguration = a})

instance Hashable GameServerGroupAutoScalingPolicy

instance NFData GameServerGroupAutoScalingPolicy

instance ToJSON GameServerGroupAutoScalingPolicy where
  toJSON GameServerGroupAutoScalingPolicy' {..} =
    object
      ( catMaybes
          [ ("EstimatedInstanceWarmup" .=) <$> _gsgaspEstimatedInstanceWarmup,
            Just
              ( "TargetTrackingConfiguration"
                  .= _gsgaspTargetTrackingConfiguration
              )
          ]
      )
