{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.TargetTrackingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.TargetTrackingConfiguration where

import Network.AWS.AutoScalingPlans.Types.CustomizedScalingMetricSpecification
import Network.AWS.AutoScalingPlans.Types.PredefinedScalingMetricSpecification
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a target tracking configuration to use with AWS Auto Scaling. Used with 'ScalingInstruction' and 'ScalingPolicy' .
--
--
--
-- /See:/ 'targetTrackingConfiguration' smart constructor.
data TargetTrackingConfiguration = TargetTrackingConfiguration'
  { _ttcEstimatedInstanceWarmup ::
      !(Maybe Int),
    _ttcPredefinedScalingMetricSpecification ::
      !( Maybe
           PredefinedScalingMetricSpecification
       ),
    _ttcScaleInCooldown :: !(Maybe Int),
    _ttcDisableScaleIn :: !(Maybe Bool),
    _ttcCustomizedScalingMetricSpecification ::
      !( Maybe
           CustomizedScalingMetricSpecification
       ),
    _ttcScaleOutCooldown ::
      !(Maybe Int),
    _ttcTargetValue :: !Double
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TargetTrackingConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ttcEstimatedInstanceWarmup' - The estimated time, in seconds, until a newly launched instance can contribute to the CloudWatch metrics. This value is used only if the resource is an Auto Scaling group.
--
-- * 'ttcPredefinedScalingMetricSpecification' - A predefined metric. You can specify either a predefined metric or a customized metric.
--
-- * 'ttcScaleInCooldown' - The amount of time, in seconds, after a scale in activity completes before another scale in activity can start. This value is not used if the scalable resource is an Auto Scaling group. The cooldown period is used to block subsequent scale in requests until it has expired. The intention is to scale in conservatively to protect your application's availability. However, if another alarm triggers a scale-out policy during the cooldown period after a scale-in, AWS Auto Scaling scales out your scalable target immediately.
--
-- * 'ttcDisableScaleIn' - Indicates whether scale in by the target tracking scaling policy is disabled. If the value is @true@ , scale in is disabled and the target tracking scaling policy doesn't remove capacity from the scalable resource. Otherwise, scale in is enabled and the target tracking scaling policy can remove capacity from the scalable resource.  The default value is @false@ .
--
-- * 'ttcCustomizedScalingMetricSpecification' - A customized metric. You can specify either a predefined metric or a customized metric.
--
-- * 'ttcScaleOutCooldown' - The amount of time, in seconds, after a scale-out activity completes before another scale-out activity can start. This value is not used if the scalable resource is an Auto Scaling group. While the cooldown period is in effect, the capacity that has been added by the previous scale-out event that initiated the cooldown is calculated as part of the desired capacity for the next scale out. The intention is to continuously (but not excessively) scale out.
--
-- * 'ttcTargetValue' - The target value for the metric. The range is 8.515920e-109 to 1.174271e+108 (Base 10) or 2e-360 to 2e360 (Base 2).
targetTrackingConfiguration ::
  -- | 'ttcTargetValue'
  Double ->
  TargetTrackingConfiguration
targetTrackingConfiguration pTargetValue_ =
  TargetTrackingConfiguration'
    { _ttcEstimatedInstanceWarmup =
        Nothing,
      _ttcPredefinedScalingMetricSpecification = Nothing,
      _ttcScaleInCooldown = Nothing,
      _ttcDisableScaleIn = Nothing,
      _ttcCustomizedScalingMetricSpecification = Nothing,
      _ttcScaleOutCooldown = Nothing,
      _ttcTargetValue = pTargetValue_
    }

-- | The estimated time, in seconds, until a newly launched instance can contribute to the CloudWatch metrics. This value is used only if the resource is an Auto Scaling group.
ttcEstimatedInstanceWarmup :: Lens' TargetTrackingConfiguration (Maybe Int)
ttcEstimatedInstanceWarmup = lens _ttcEstimatedInstanceWarmup (\s a -> s {_ttcEstimatedInstanceWarmup = a})

-- | A predefined metric. You can specify either a predefined metric or a customized metric.
ttcPredefinedScalingMetricSpecification :: Lens' TargetTrackingConfiguration (Maybe PredefinedScalingMetricSpecification)
ttcPredefinedScalingMetricSpecification = lens _ttcPredefinedScalingMetricSpecification (\s a -> s {_ttcPredefinedScalingMetricSpecification = a})

-- | The amount of time, in seconds, after a scale in activity completes before another scale in activity can start. This value is not used if the scalable resource is an Auto Scaling group. The cooldown period is used to block subsequent scale in requests until it has expired. The intention is to scale in conservatively to protect your application's availability. However, if another alarm triggers a scale-out policy during the cooldown period after a scale-in, AWS Auto Scaling scales out your scalable target immediately.
ttcScaleInCooldown :: Lens' TargetTrackingConfiguration (Maybe Int)
ttcScaleInCooldown = lens _ttcScaleInCooldown (\s a -> s {_ttcScaleInCooldown = a})

-- | Indicates whether scale in by the target tracking scaling policy is disabled. If the value is @true@ , scale in is disabled and the target tracking scaling policy doesn't remove capacity from the scalable resource. Otherwise, scale in is enabled and the target tracking scaling policy can remove capacity from the scalable resource.  The default value is @false@ .
ttcDisableScaleIn :: Lens' TargetTrackingConfiguration (Maybe Bool)
ttcDisableScaleIn = lens _ttcDisableScaleIn (\s a -> s {_ttcDisableScaleIn = a})

-- | A customized metric. You can specify either a predefined metric or a customized metric.
ttcCustomizedScalingMetricSpecification :: Lens' TargetTrackingConfiguration (Maybe CustomizedScalingMetricSpecification)
ttcCustomizedScalingMetricSpecification = lens _ttcCustomizedScalingMetricSpecification (\s a -> s {_ttcCustomizedScalingMetricSpecification = a})

-- | The amount of time, in seconds, after a scale-out activity completes before another scale-out activity can start. This value is not used if the scalable resource is an Auto Scaling group. While the cooldown period is in effect, the capacity that has been added by the previous scale-out event that initiated the cooldown is calculated as part of the desired capacity for the next scale out. The intention is to continuously (but not excessively) scale out.
ttcScaleOutCooldown :: Lens' TargetTrackingConfiguration (Maybe Int)
ttcScaleOutCooldown = lens _ttcScaleOutCooldown (\s a -> s {_ttcScaleOutCooldown = a})

-- | The target value for the metric. The range is 8.515920e-109 to 1.174271e+108 (Base 10) or 2e-360 to 2e360 (Base 2).
ttcTargetValue :: Lens' TargetTrackingConfiguration Double
ttcTargetValue = lens _ttcTargetValue (\s a -> s {_ttcTargetValue = a})

instance FromJSON TargetTrackingConfiguration where
  parseJSON =
    withObject
      "TargetTrackingConfiguration"
      ( \x ->
          TargetTrackingConfiguration'
            <$> (x .:? "EstimatedInstanceWarmup")
            <*> (x .:? "PredefinedScalingMetricSpecification")
            <*> (x .:? "ScaleInCooldown")
            <*> (x .:? "DisableScaleIn")
            <*> (x .:? "CustomizedScalingMetricSpecification")
            <*> (x .:? "ScaleOutCooldown")
            <*> (x .: "TargetValue")
      )

instance Hashable TargetTrackingConfiguration

instance NFData TargetTrackingConfiguration

instance ToJSON TargetTrackingConfiguration where
  toJSON TargetTrackingConfiguration' {..} =
    object
      ( catMaybes
          [ ("EstimatedInstanceWarmup" .=) <$> _ttcEstimatedInstanceWarmup,
            ("PredefinedScalingMetricSpecification" .=)
              <$> _ttcPredefinedScalingMetricSpecification,
            ("ScaleInCooldown" .=) <$> _ttcScaleInCooldown,
            ("DisableScaleIn" .=) <$> _ttcDisableScaleIn,
            ("CustomizedScalingMetricSpecification" .=)
              <$> _ttcCustomizedScalingMetricSpecification,
            ("ScaleOutCooldown" .=) <$> _ttcScaleOutCooldown,
            Just ("TargetValue" .= _ttcTargetValue)
          ]
      )
