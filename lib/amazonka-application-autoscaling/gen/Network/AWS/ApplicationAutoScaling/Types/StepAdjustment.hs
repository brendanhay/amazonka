{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types.StepAdjustment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ApplicationAutoScaling.Types.StepAdjustment where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a step adjustment for a <https://docs.aws.amazon.com/autoscaling/application/APIReference/API_StepScalingPolicyConfiguration.html StepScalingPolicyConfiguration> . Describes an adjustment based on the difference between the value of the aggregated CloudWatch metric and the breach threshold that you've defined for the alarm.
--
--
-- For the following examples, suppose that you have an alarm with a breach threshold of 50:
--
--     * To trigger the adjustment when the metric is greater than or equal to 50 and less than 60, specify a lower bound of 0 and an upper bound of 10.
--
--     * To trigger the adjustment when the metric is greater than 40 and less than or equal to 50, specify a lower bound of -10 and an upper bound of 0.
--
--
--
-- There are a few rules for the step adjustments for your step policy:
--
--     * The ranges of your step adjustments can't overlap or have a gap.
--
--     * At most one step adjustment can have a null lower bound. If one step adjustment has a negative lower bound, then there must be a step adjustment with a null lower bound.
--
--     * At most one step adjustment can have a null upper bound. If one step adjustment has a positive upper bound, then there must be a step adjustment with a null upper bound.
--
--     * The upper and lower bound can't be null in the same step adjustment.
--
--
--
--
-- /See:/ 'stepAdjustment' smart constructor.
data StepAdjustment = StepAdjustment'
  { _saMetricIntervalLowerBound ::
      !(Maybe Double),
    _saMetricIntervalUpperBound :: !(Maybe Double),
    _saScalingAdjustment :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StepAdjustment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'saMetricIntervalLowerBound' - The lower bound for the difference between the alarm threshold and the CloudWatch metric. If the metric value is above the breach threshold, the lower bound is inclusive (the metric must be greater than or equal to the threshold plus the lower bound). Otherwise, it is exclusive (the metric must be greater than the threshold plus the lower bound). A null value indicates negative infinity.
--
-- * 'saMetricIntervalUpperBound' - The upper bound for the difference between the alarm threshold and the CloudWatch metric. If the metric value is above the breach threshold, the upper bound is exclusive (the metric must be less than the threshold plus the upper bound). Otherwise, it is inclusive (the metric must be less than or equal to the threshold plus the upper bound). A null value indicates positive infinity. The upper bound must be greater than the lower bound.
--
-- * 'saScalingAdjustment' - The amount by which to scale, based on the specified adjustment type. A positive value adds to the current capacity while a negative number removes from the current capacity. For exact capacity, you must specify a positive value.
stepAdjustment ::
  -- | 'saScalingAdjustment'
  Int ->
  StepAdjustment
stepAdjustment pScalingAdjustment_ =
  StepAdjustment'
    { _saMetricIntervalLowerBound = Nothing,
      _saMetricIntervalUpperBound = Nothing,
      _saScalingAdjustment = pScalingAdjustment_
    }

-- | The lower bound for the difference between the alarm threshold and the CloudWatch metric. If the metric value is above the breach threshold, the lower bound is inclusive (the metric must be greater than or equal to the threshold plus the lower bound). Otherwise, it is exclusive (the metric must be greater than the threshold plus the lower bound). A null value indicates negative infinity.
saMetricIntervalLowerBound :: Lens' StepAdjustment (Maybe Double)
saMetricIntervalLowerBound = lens _saMetricIntervalLowerBound (\s a -> s {_saMetricIntervalLowerBound = a})

-- | The upper bound for the difference between the alarm threshold and the CloudWatch metric. If the metric value is above the breach threshold, the upper bound is exclusive (the metric must be less than the threshold plus the upper bound). Otherwise, it is inclusive (the metric must be less than or equal to the threshold plus the upper bound). A null value indicates positive infinity. The upper bound must be greater than the lower bound.
saMetricIntervalUpperBound :: Lens' StepAdjustment (Maybe Double)
saMetricIntervalUpperBound = lens _saMetricIntervalUpperBound (\s a -> s {_saMetricIntervalUpperBound = a})

-- | The amount by which to scale, based on the specified adjustment type. A positive value adds to the current capacity while a negative number removes from the current capacity. For exact capacity, you must specify a positive value.
saScalingAdjustment :: Lens' StepAdjustment Int
saScalingAdjustment = lens _saScalingAdjustment (\s a -> s {_saScalingAdjustment = a})

instance FromJSON StepAdjustment where
  parseJSON =
    withObject
      "StepAdjustment"
      ( \x ->
          StepAdjustment'
            <$> (x .:? "MetricIntervalLowerBound")
            <*> (x .:? "MetricIntervalUpperBound")
            <*> (x .: "ScalingAdjustment")
      )

instance Hashable StepAdjustment

instance NFData StepAdjustment

instance ToJSON StepAdjustment where
  toJSON StepAdjustment' {..} =
    object
      ( catMaybes
          [ ("MetricIntervalLowerBound" .=) <$> _saMetricIntervalLowerBound,
            ("MetricIntervalUpperBound" .=) <$> _saMetricIntervalUpperBound,
            Just ("ScalingAdjustment" .= _saScalingAdjustment)
          ]
      )
