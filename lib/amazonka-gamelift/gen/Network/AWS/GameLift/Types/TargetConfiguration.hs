{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.TargetConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.TargetConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Settings for a target-based scaling policy (see 'ScalingPolicy' . A target-based policy tracks a particular fleet metric specifies a target value for the metric. As player usage changes, the policy triggers Amazon GameLift to adjust capacity so that the metric returns to the target value. The target configuration specifies settings as needed for the target based policy, including the target value.
--
--
--     * 'DescribeFleetCapacity'
--
--     * 'UpdateFleetCapacity'
--
--     * 'DescribeEC2InstanceLimits'
--
--     * Manage scaling policies:
--
--     * 'PutScalingPolicy' (auto-scaling)
--
--     * 'DescribeScalingPolicies' (auto-scaling)
--
--     * 'DeleteScalingPolicy' (auto-scaling)
--
--
--
--     * Manage fleet actions:
--
--     * 'StartFleetActions'
--
--     * 'StopFleetActions'
--
--
--
--
--
--
-- /See:/ 'targetConfiguration' smart constructor.
newtype TargetConfiguration = TargetConfiguration'
  { _tcTargetValue ::
      Double
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TargetConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tcTargetValue' - Desired value to use with a target-based scaling policy. The value must be relevant for whatever metric the scaling policy is using. For example, in a policy using the metric PercentAvailableGameSessions, the target value should be the preferred size of the fleet's buffer (the percent of capacity that should be idle and ready for new game sessions).
targetConfiguration ::
  -- | 'tcTargetValue'
  Double ->
  TargetConfiguration
targetConfiguration pTargetValue_ =
  TargetConfiguration' {_tcTargetValue = pTargetValue_}

-- | Desired value to use with a target-based scaling policy. The value must be relevant for whatever metric the scaling policy is using. For example, in a policy using the metric PercentAvailableGameSessions, the target value should be the preferred size of the fleet's buffer (the percent of capacity that should be idle and ready for new game sessions).
tcTargetValue :: Lens' TargetConfiguration Double
tcTargetValue = lens _tcTargetValue (\s a -> s {_tcTargetValue = a})

instance FromJSON TargetConfiguration where
  parseJSON =
    withObject
      "TargetConfiguration"
      (\x -> TargetConfiguration' <$> (x .: "TargetValue"))

instance Hashable TargetConfiguration

instance NFData TargetConfiguration

instance ToJSON TargetConfiguration where
  toJSON TargetConfiguration' {..} =
    object (catMaybes [Just ("TargetValue" .= _tcTargetValue)])
