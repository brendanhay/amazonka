{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.BehaviorCriteria
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.BehaviorCriteria where

import Network.AWS.IoT.Types.ComparisonOperator
import Network.AWS.IoT.Types.MetricValue
import Network.AWS.IoT.Types.StatisticalThreshold
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The criteria by which the behavior is determined to be normal.
--
--
--
-- /See:/ 'behaviorCriteria' smart constructor.
data BehaviorCriteria = BehaviorCriteria'
  { _bcValue ::
      !(Maybe MetricValue),
    _bcConsecutiveDatapointsToAlarm :: !(Maybe Nat),
    _bcComparisonOperator :: !(Maybe ComparisonOperator),
    _bcStatisticalThreshold :: !(Maybe StatisticalThreshold),
    _bcDurationSeconds :: !(Maybe Int),
    _bcConsecutiveDatapointsToClear :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BehaviorCriteria' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bcValue' - The value to be compared with the @metric@ .
--
-- * 'bcConsecutiveDatapointsToAlarm' - If a device is in violation of the behavior for the specified number of consecutive datapoints, an alarm occurs. If not specified, the default is 1.
--
-- * 'bcComparisonOperator' - The operator that relates the thing measured (@metric@ ) to the criteria (containing a @value@ or @statisticalThreshold@ ).
--
-- * 'bcStatisticalThreshold' - A statistical ranking (percentile) which indicates a threshold value by which a behavior is determined to be in compliance or in violation of the behavior.
--
-- * 'bcDurationSeconds' - Use this to specify the time duration over which the behavior is evaluated, for those criteria which have a time dimension (for example, @NUM_MESSAGES_SENT@ ). For a @statisticalThreshhold@ metric comparison, measurements from all devices are accumulated over this time duration before being used to calculate percentiles, and later, measurements from an individual device are also accumulated over this time duration before being given a percentile rank.
--
-- * 'bcConsecutiveDatapointsToClear' - If an alarm has occurred and the offending device is no longer in violation of the behavior for the specified number of consecutive datapoints, the alarm is cleared. If not specified, the default is 1.
behaviorCriteria ::
  BehaviorCriteria
behaviorCriteria =
  BehaviorCriteria'
    { _bcValue = Nothing,
      _bcConsecutiveDatapointsToAlarm = Nothing,
      _bcComparisonOperator = Nothing,
      _bcStatisticalThreshold = Nothing,
      _bcDurationSeconds = Nothing,
      _bcConsecutiveDatapointsToClear = Nothing
    }

-- | The value to be compared with the @metric@ .
bcValue :: Lens' BehaviorCriteria (Maybe MetricValue)
bcValue = lens _bcValue (\s a -> s {_bcValue = a})

-- | If a device is in violation of the behavior for the specified number of consecutive datapoints, an alarm occurs. If not specified, the default is 1.
bcConsecutiveDatapointsToAlarm :: Lens' BehaviorCriteria (Maybe Natural)
bcConsecutiveDatapointsToAlarm = lens _bcConsecutiveDatapointsToAlarm (\s a -> s {_bcConsecutiveDatapointsToAlarm = a}) . mapping _Nat

-- | The operator that relates the thing measured (@metric@ ) to the criteria (containing a @value@ or @statisticalThreshold@ ).
bcComparisonOperator :: Lens' BehaviorCriteria (Maybe ComparisonOperator)
bcComparisonOperator = lens _bcComparisonOperator (\s a -> s {_bcComparisonOperator = a})

-- | A statistical ranking (percentile) which indicates a threshold value by which a behavior is determined to be in compliance or in violation of the behavior.
bcStatisticalThreshold :: Lens' BehaviorCriteria (Maybe StatisticalThreshold)
bcStatisticalThreshold = lens _bcStatisticalThreshold (\s a -> s {_bcStatisticalThreshold = a})

-- | Use this to specify the time duration over which the behavior is evaluated, for those criteria which have a time dimension (for example, @NUM_MESSAGES_SENT@ ). For a @statisticalThreshhold@ metric comparison, measurements from all devices are accumulated over this time duration before being used to calculate percentiles, and later, measurements from an individual device are also accumulated over this time duration before being given a percentile rank.
bcDurationSeconds :: Lens' BehaviorCriteria (Maybe Int)
bcDurationSeconds = lens _bcDurationSeconds (\s a -> s {_bcDurationSeconds = a})

-- | If an alarm has occurred and the offending device is no longer in violation of the behavior for the specified number of consecutive datapoints, the alarm is cleared. If not specified, the default is 1.
bcConsecutiveDatapointsToClear :: Lens' BehaviorCriteria (Maybe Natural)
bcConsecutiveDatapointsToClear = lens _bcConsecutiveDatapointsToClear (\s a -> s {_bcConsecutiveDatapointsToClear = a}) . mapping _Nat

instance FromJSON BehaviorCriteria where
  parseJSON =
    withObject
      "BehaviorCriteria"
      ( \x ->
          BehaviorCriteria'
            <$> (x .:? "value")
            <*> (x .:? "consecutiveDatapointsToAlarm")
            <*> (x .:? "comparisonOperator")
            <*> (x .:? "statisticalThreshold")
            <*> (x .:? "durationSeconds")
            <*> (x .:? "consecutiveDatapointsToClear")
      )

instance Hashable BehaviorCriteria

instance NFData BehaviorCriteria

instance ToJSON BehaviorCriteria where
  toJSON BehaviorCriteria' {..} =
    object
      ( catMaybes
          [ ("value" .=) <$> _bcValue,
            ("consecutiveDatapointsToAlarm" .=)
              <$> _bcConsecutiveDatapointsToAlarm,
            ("comparisonOperator" .=) <$> _bcComparisonOperator,
            ("statisticalThreshold" .=) <$> _bcStatisticalThreshold,
            ("durationSeconds" .=) <$> _bcDurationSeconds,
            ("consecutiveDatapointsToClear" .=)
              <$> _bcConsecutiveDatapointsToClear
          ]
      )
