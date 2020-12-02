{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.TargetTrackingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.TargetTrackingConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | __This data type is used with the Amazon GameLift FleetIQ and game server groups.__
--
--
-- Settings for a target-based scaling policy as part of a 'GameServerGroupAutoScalingPolicy' . These settings are used to create a target-based policy that tracks the GameLift FleetIQ metric @"PercentUtilizedGameServers"@ and specifies a target value for the metric. As player usage changes, the policy triggers to adjust the game server group capacity so that the metric returns to the target value.
--
--
-- /See:/ 'targetTrackingConfiguration' smart constructor.
newtype TargetTrackingConfiguration = TargetTrackingConfiguration'
  { _ttcTargetValue ::
      Double
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TargetTrackingConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ttcTargetValue' - Desired value to use with a game server group target-based scaling policy.
targetTrackingConfiguration ::
  -- | 'ttcTargetValue'
  Double ->
  TargetTrackingConfiguration
targetTrackingConfiguration pTargetValue_ =
  TargetTrackingConfiguration' {_ttcTargetValue = pTargetValue_}

-- | Desired value to use with a game server group target-based scaling policy.
ttcTargetValue :: Lens' TargetTrackingConfiguration Double
ttcTargetValue = lens _ttcTargetValue (\s a -> s {_ttcTargetValue = a})

instance Hashable TargetTrackingConfiguration

instance NFData TargetTrackingConfiguration

instance ToJSON TargetTrackingConfiguration where
  toJSON TargetTrackingConfiguration' {..} =
    object (catMaybes [Just ("TargetValue" .= _ttcTargetValue)])
