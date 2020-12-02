{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types.StepScalingPolicyConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ApplicationAutoScaling.Types.StepScalingPolicyConfiguration where

import Network.AWS.ApplicationAutoScaling.Types.AdjustmentType
import Network.AWS.ApplicationAutoScaling.Types.MetricAggregationType
import Network.AWS.ApplicationAutoScaling.Types.StepAdjustment
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a step scaling policy configuration to use with Application Auto Scaling.
--
--
--
-- /See:/ 'stepScalingPolicyConfiguration' smart constructor.
data StepScalingPolicyConfiguration = StepScalingPolicyConfiguration'
  { _sspcStepAdjustments ::
      !(Maybe [StepAdjustment]),
    _sspcAdjustmentType ::
      !(Maybe AdjustmentType),
    _sspcCooldown :: !(Maybe Int),
    _sspcMetricAggregationType ::
      !( Maybe
           MetricAggregationType
       ),
    _sspcMinAdjustmentMagnitude ::
      !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StepScalingPolicyConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sspcStepAdjustments' - A set of adjustments that enable you to scale based on the size of the alarm breach. At least one step adjustment is required if you are adding a new step scaling policy configuration.
--
-- * 'sspcAdjustmentType' - Specifies how the @ScalingAdjustment@ value in a <https://docs.aws.amazon.com/autoscaling/application/APIReference/API_StepAdjustment.html StepAdjustment> is interpreted (for example, an absolute number or a percentage). The valid values are @ChangeInCapacity@ , @ExactCapacity@ , and @PercentChangeInCapacity@ .  @AdjustmentType@ is required if you are adding a new step scaling policy configuration.
--
-- * 'sspcCooldown' - The amount of time, in seconds, to wait for a previous scaling activity to take effect.  With scale-out policies, the intention is to continuously (but not excessively) scale out. After Application Auto Scaling successfully scales out using a step scaling policy, it starts to calculate the cooldown time. The scaling policy won't increase the desired capacity again unless either a larger scale out is triggered or the cooldown period ends. While the cooldown period is in effect, capacity added by the initiating scale-out activity is calculated as part of the desired capacity for the next scale-out activity. For example, when an alarm triggers a step scaling policy to increase the capacity by 2, the scaling activity completes successfully, and a cooldown period starts. If the alarm triggers again during the cooldown period but at a more aggressive step adjustment of 3, the previous increase of 2 is considered part of the current capacity. Therefore, only 1 is added to the capacity. With scale-in policies, the intention is to scale in conservatively to protect your application’s availability, so scale-in activities are blocked until the cooldown period has expired. However, if another alarm triggers a scale-out activity during the cooldown period after a scale-in activity, Application Auto Scaling scales out the target immediately. In this case, the cooldown period for the scale-in activity stops and doesn't complete. Application Auto Scaling provides a default value of 300 for the following scalable targets:     * ECS services     * Spot Fleet requests     * EMR clusters     * AppStream 2.0 fleets     * Aurora DB clusters     * Amazon SageMaker endpoint variants     * Custom resources For all other scalable targets, the default value is 0:     * DynamoDB tables     * DynamoDB global secondary indexes     * Amazon Comprehend document classification and entity recognizer endpoints     * Lambda provisioned concurrency     * Amazon Keyspaces tables     * Amazon MSK cluster storage
--
-- * 'sspcMetricAggregationType' - The aggregation type for the CloudWatch metrics. Valid values are @Minimum@ , @Maximum@ , and @Average@ . If the aggregation type is null, the value is treated as @Average@ .
--
-- * 'sspcMinAdjustmentMagnitude' - The minimum value to scale by when the adjustment type is @PercentChangeInCapacity@ . For example, suppose that you create a step scaling policy to scale out an Amazon ECS service by 25 percent and you specify a @MinAdjustmentMagnitude@ of 2. If the service has 4 tasks and the scaling policy is performed, 25 percent of 4 is 1. However, because you specified a @MinAdjustmentMagnitude@ of 2, Application Auto Scaling scales out the service by 2 tasks.
stepScalingPolicyConfiguration ::
  StepScalingPolicyConfiguration
stepScalingPolicyConfiguration =
  StepScalingPolicyConfiguration'
    { _sspcStepAdjustments = Nothing,
      _sspcAdjustmentType = Nothing,
      _sspcCooldown = Nothing,
      _sspcMetricAggregationType = Nothing,
      _sspcMinAdjustmentMagnitude = Nothing
    }

-- | A set of adjustments that enable you to scale based on the size of the alarm breach. At least one step adjustment is required if you are adding a new step scaling policy configuration.
sspcStepAdjustments :: Lens' StepScalingPolicyConfiguration [StepAdjustment]
sspcStepAdjustments = lens _sspcStepAdjustments (\s a -> s {_sspcStepAdjustments = a}) . _Default . _Coerce

-- | Specifies how the @ScalingAdjustment@ value in a <https://docs.aws.amazon.com/autoscaling/application/APIReference/API_StepAdjustment.html StepAdjustment> is interpreted (for example, an absolute number or a percentage). The valid values are @ChangeInCapacity@ , @ExactCapacity@ , and @PercentChangeInCapacity@ .  @AdjustmentType@ is required if you are adding a new step scaling policy configuration.
sspcAdjustmentType :: Lens' StepScalingPolicyConfiguration (Maybe AdjustmentType)
sspcAdjustmentType = lens _sspcAdjustmentType (\s a -> s {_sspcAdjustmentType = a})

-- | The amount of time, in seconds, to wait for a previous scaling activity to take effect.  With scale-out policies, the intention is to continuously (but not excessively) scale out. After Application Auto Scaling successfully scales out using a step scaling policy, it starts to calculate the cooldown time. The scaling policy won't increase the desired capacity again unless either a larger scale out is triggered or the cooldown period ends. While the cooldown period is in effect, capacity added by the initiating scale-out activity is calculated as part of the desired capacity for the next scale-out activity. For example, when an alarm triggers a step scaling policy to increase the capacity by 2, the scaling activity completes successfully, and a cooldown period starts. If the alarm triggers again during the cooldown period but at a more aggressive step adjustment of 3, the previous increase of 2 is considered part of the current capacity. Therefore, only 1 is added to the capacity. With scale-in policies, the intention is to scale in conservatively to protect your application’s availability, so scale-in activities are blocked until the cooldown period has expired. However, if another alarm triggers a scale-out activity during the cooldown period after a scale-in activity, Application Auto Scaling scales out the target immediately. In this case, the cooldown period for the scale-in activity stops and doesn't complete. Application Auto Scaling provides a default value of 300 for the following scalable targets:     * ECS services     * Spot Fleet requests     * EMR clusters     * AppStream 2.0 fleets     * Aurora DB clusters     * Amazon SageMaker endpoint variants     * Custom resources For all other scalable targets, the default value is 0:     * DynamoDB tables     * DynamoDB global secondary indexes     * Amazon Comprehend document classification and entity recognizer endpoints     * Lambda provisioned concurrency     * Amazon Keyspaces tables     * Amazon MSK cluster storage
sspcCooldown :: Lens' StepScalingPolicyConfiguration (Maybe Int)
sspcCooldown = lens _sspcCooldown (\s a -> s {_sspcCooldown = a})

-- | The aggregation type for the CloudWatch metrics. Valid values are @Minimum@ , @Maximum@ , and @Average@ . If the aggregation type is null, the value is treated as @Average@ .
sspcMetricAggregationType :: Lens' StepScalingPolicyConfiguration (Maybe MetricAggregationType)
sspcMetricAggregationType = lens _sspcMetricAggregationType (\s a -> s {_sspcMetricAggregationType = a})

-- | The minimum value to scale by when the adjustment type is @PercentChangeInCapacity@ . For example, suppose that you create a step scaling policy to scale out an Amazon ECS service by 25 percent and you specify a @MinAdjustmentMagnitude@ of 2. If the service has 4 tasks and the scaling policy is performed, 25 percent of 4 is 1. However, because you specified a @MinAdjustmentMagnitude@ of 2, Application Auto Scaling scales out the service by 2 tasks.
sspcMinAdjustmentMagnitude :: Lens' StepScalingPolicyConfiguration (Maybe Int)
sspcMinAdjustmentMagnitude = lens _sspcMinAdjustmentMagnitude (\s a -> s {_sspcMinAdjustmentMagnitude = a})

instance FromJSON StepScalingPolicyConfiguration where
  parseJSON =
    withObject
      "StepScalingPolicyConfiguration"
      ( \x ->
          StepScalingPolicyConfiguration'
            <$> (x .:? "StepAdjustments" .!= mempty)
            <*> (x .:? "AdjustmentType")
            <*> (x .:? "Cooldown")
            <*> (x .:? "MetricAggregationType")
            <*> (x .:? "MinAdjustmentMagnitude")
      )

instance Hashable StepScalingPolicyConfiguration

instance NFData StepScalingPolicyConfiguration

instance ToJSON StepScalingPolicyConfiguration where
  toJSON StepScalingPolicyConfiguration' {..} =
    object
      ( catMaybes
          [ ("StepAdjustments" .=) <$> _sspcStepAdjustments,
            ("AdjustmentType" .=) <$> _sspcAdjustmentType,
            ("Cooldown" .=) <$> _sspcCooldown,
            ("MetricAggregationType" .=) <$> _sspcMetricAggregationType,
            ("MinAdjustmentMagnitude" .=) <$> _sspcMinAdjustmentMagnitude
          ]
      )
