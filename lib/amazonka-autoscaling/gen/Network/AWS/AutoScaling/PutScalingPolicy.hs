{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.PutScalingPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a scaling policy for an Auto Scaling group.
--
--
-- For more information about using scaling policies to scale your Auto Scaling group, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-scaling-target-tracking.html Target tracking scaling policies> and <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-scaling-simple-step.html Step and simple scaling policies> in the /Amazon EC2 Auto Scaling User Guide/ .
module Network.AWS.AutoScaling.PutScalingPolicy
  ( -- * Creating a Request
    putScalingPolicy,
    PutScalingPolicy,

    -- * Request Lenses
    pspMinAdjustmentStep,
    pspEstimatedInstanceWarmup,
    pspEnabled,
    pspPolicyType,
    pspStepAdjustments,
    pspTargetTrackingConfiguration,
    pspAdjustmentType,
    pspScalingAdjustment,
    pspCooldown,
    pspMetricAggregationType,
    pspMinAdjustmentMagnitude,
    pspAutoScalingGroupName,
    pspPolicyName,

    -- * Destructuring the Response
    putScalingPolicyResponse,
    PutScalingPolicyResponse,

    -- * Response Lenses
    psprsPolicyARN,
    psprsAlarms,
    psprsResponseStatus,
  )
where

import Network.AWS.AutoScaling.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putScalingPolicy' smart constructor.
data PutScalingPolicy = PutScalingPolicy'
  { _pspMinAdjustmentStep ::
      !(Maybe Int),
    _pspEstimatedInstanceWarmup :: !(Maybe Int),
    _pspEnabled :: !(Maybe Bool),
    _pspPolicyType :: !(Maybe Text),
    _pspStepAdjustments :: !(Maybe [StepAdjustment]),
    _pspTargetTrackingConfiguration ::
      !(Maybe TargetTrackingConfiguration),
    _pspAdjustmentType :: !(Maybe Text),
    _pspScalingAdjustment :: !(Maybe Int),
    _pspCooldown :: !(Maybe Int),
    _pspMetricAggregationType :: !(Maybe Text),
    _pspMinAdjustmentMagnitude :: !(Maybe Int),
    _pspAutoScalingGroupName :: !Text,
    _pspPolicyName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutScalingPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pspMinAdjustmentStep' - Available for backward compatibility. Use @MinAdjustmentMagnitude@ instead.
--
-- * 'pspEstimatedInstanceWarmup' - The estimated time, in seconds, until a newly launched instance can contribute to the CloudWatch metrics. If not provided, the default is to use the value from the default cooldown period for the Auto Scaling group. Valid only if the policy type is @TargetTrackingScaling@ or @StepScaling@ .
--
-- * 'pspEnabled' - Indicates whether the scaling policy is enabled or disabled. The default is enabled. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-enable-disable-scaling-policy.html Disabling a scaling policy for an Auto Scaling group> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- * 'pspPolicyType' - One of the following policy types:      * @TargetTrackingScaling@      * @StepScaling@      * @SimpleScaling@ (default)
--
-- * 'pspStepAdjustments' - A set of adjustments that enable you to scale based on the size of the alarm breach. Required if the policy type is @StepScaling@ . (Not used with any other policy type.)
--
-- * 'pspTargetTrackingConfiguration' - A target tracking scaling policy. Includes support for predefined or customized metrics. The following predefined metrics are available:     * @ASGAverageCPUUtilization@      * @ASGAverageNetworkIn@      * @ASGAverageNetworkOut@      * @ALBRequestCountPerTarget@  If you specify @ALBRequestCountPerTarget@ for the metric, you must specify the @ResourceLabel@ parameter with the @PredefinedMetricSpecification@ . For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/APIReference/API_TargetTrackingConfiguration.html TargetTrackingConfiguration> in the /Amazon EC2 Auto Scaling API Reference/ . Required if the policy type is @TargetTrackingScaling@ .
--
-- * 'pspAdjustmentType' - Specifies how the scaling adjustment is interpreted (for example, an absolute number or a percentage). The valid values are @ChangeInCapacity@ , @ExactCapacity@ , and @PercentChangeInCapacity@ . Required if the policy type is @StepScaling@ or @SimpleScaling@ . For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-scaling-simple-step.html#as-scaling-adjustment Scaling adjustment types> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- * 'pspScalingAdjustment' - The amount by which to scale, based on the specified adjustment type. A positive value adds to the current capacity while a negative number removes from the current capacity. For exact capacity, you must specify a positive value. Required if the policy type is @SimpleScaling@ . (Not used with any other policy type.)
--
-- * 'pspCooldown' - The duration of the policy's cooldown period, in seconds. When a cooldown period is specified here, it overrides the default cooldown period defined for the Auto Scaling group. Valid only if the policy type is @SimpleScaling@ . For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/Cooldown.html Scaling cooldowns for Amazon EC2 Auto Scaling> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- * 'pspMetricAggregationType' - The aggregation type for the CloudWatch metrics. The valid values are @Minimum@ , @Maximum@ , and @Average@ . If the aggregation type is null, the value is treated as @Average@ . Valid only if the policy type is @StepScaling@ .
--
-- * 'pspMinAdjustmentMagnitude' - The minimum value to scale by when the adjustment type is @PercentChangeInCapacity@ . For example, suppose that you create a step scaling policy to scale out an Auto Scaling group by 25 percent and you specify a @MinAdjustmentMagnitude@ of 2. If the group has 4 instances and the scaling policy is performed, 25 percent of 4 is 1. However, because you specified a @MinAdjustmentMagnitude@ of 2, Amazon EC2 Auto Scaling scales out the group by 2 instances. Valid only if the policy type is @StepScaling@ or @SimpleScaling@ . For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-scaling-simple-step.html#as-scaling-adjustment Scaling adjustment types> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- * 'pspAutoScalingGroupName' - The name of the Auto Scaling group.
--
-- * 'pspPolicyName' - The name of the policy.
putScalingPolicy ::
  -- | 'pspAutoScalingGroupName'
  Text ->
  -- | 'pspPolicyName'
  Text ->
  PutScalingPolicy
putScalingPolicy pAutoScalingGroupName_ pPolicyName_ =
  PutScalingPolicy'
    { _pspMinAdjustmentStep = Nothing,
      _pspEstimatedInstanceWarmup = Nothing,
      _pspEnabled = Nothing,
      _pspPolicyType = Nothing,
      _pspStepAdjustments = Nothing,
      _pspTargetTrackingConfiguration = Nothing,
      _pspAdjustmentType = Nothing,
      _pspScalingAdjustment = Nothing,
      _pspCooldown = Nothing,
      _pspMetricAggregationType = Nothing,
      _pspMinAdjustmentMagnitude = Nothing,
      _pspAutoScalingGroupName = pAutoScalingGroupName_,
      _pspPolicyName = pPolicyName_
    }

-- | Available for backward compatibility. Use @MinAdjustmentMagnitude@ instead.
pspMinAdjustmentStep :: Lens' PutScalingPolicy (Maybe Int)
pspMinAdjustmentStep = lens _pspMinAdjustmentStep (\s a -> s {_pspMinAdjustmentStep = a})

-- | The estimated time, in seconds, until a newly launched instance can contribute to the CloudWatch metrics. If not provided, the default is to use the value from the default cooldown period for the Auto Scaling group. Valid only if the policy type is @TargetTrackingScaling@ or @StepScaling@ .
pspEstimatedInstanceWarmup :: Lens' PutScalingPolicy (Maybe Int)
pspEstimatedInstanceWarmup = lens _pspEstimatedInstanceWarmup (\s a -> s {_pspEstimatedInstanceWarmup = a})

-- | Indicates whether the scaling policy is enabled or disabled. The default is enabled. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-enable-disable-scaling-policy.html Disabling a scaling policy for an Auto Scaling group> in the /Amazon EC2 Auto Scaling User Guide/ .
pspEnabled :: Lens' PutScalingPolicy (Maybe Bool)
pspEnabled = lens _pspEnabled (\s a -> s {_pspEnabled = a})

-- | One of the following policy types:      * @TargetTrackingScaling@      * @StepScaling@      * @SimpleScaling@ (default)
pspPolicyType :: Lens' PutScalingPolicy (Maybe Text)
pspPolicyType = lens _pspPolicyType (\s a -> s {_pspPolicyType = a})

-- | A set of adjustments that enable you to scale based on the size of the alarm breach. Required if the policy type is @StepScaling@ . (Not used with any other policy type.)
pspStepAdjustments :: Lens' PutScalingPolicy [StepAdjustment]
pspStepAdjustments = lens _pspStepAdjustments (\s a -> s {_pspStepAdjustments = a}) . _Default . _Coerce

-- | A target tracking scaling policy. Includes support for predefined or customized metrics. The following predefined metrics are available:     * @ASGAverageCPUUtilization@      * @ASGAverageNetworkIn@      * @ASGAverageNetworkOut@      * @ALBRequestCountPerTarget@  If you specify @ALBRequestCountPerTarget@ for the metric, you must specify the @ResourceLabel@ parameter with the @PredefinedMetricSpecification@ . For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/APIReference/API_TargetTrackingConfiguration.html TargetTrackingConfiguration> in the /Amazon EC2 Auto Scaling API Reference/ . Required if the policy type is @TargetTrackingScaling@ .
pspTargetTrackingConfiguration :: Lens' PutScalingPolicy (Maybe TargetTrackingConfiguration)
pspTargetTrackingConfiguration = lens _pspTargetTrackingConfiguration (\s a -> s {_pspTargetTrackingConfiguration = a})

-- | Specifies how the scaling adjustment is interpreted (for example, an absolute number or a percentage). The valid values are @ChangeInCapacity@ , @ExactCapacity@ , and @PercentChangeInCapacity@ . Required if the policy type is @StepScaling@ or @SimpleScaling@ . For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-scaling-simple-step.html#as-scaling-adjustment Scaling adjustment types> in the /Amazon EC2 Auto Scaling User Guide/ .
pspAdjustmentType :: Lens' PutScalingPolicy (Maybe Text)
pspAdjustmentType = lens _pspAdjustmentType (\s a -> s {_pspAdjustmentType = a})

-- | The amount by which to scale, based on the specified adjustment type. A positive value adds to the current capacity while a negative number removes from the current capacity. For exact capacity, you must specify a positive value. Required if the policy type is @SimpleScaling@ . (Not used with any other policy type.)
pspScalingAdjustment :: Lens' PutScalingPolicy (Maybe Int)
pspScalingAdjustment = lens _pspScalingAdjustment (\s a -> s {_pspScalingAdjustment = a})

-- | The duration of the policy's cooldown period, in seconds. When a cooldown period is specified here, it overrides the default cooldown period defined for the Auto Scaling group. Valid only if the policy type is @SimpleScaling@ . For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/Cooldown.html Scaling cooldowns for Amazon EC2 Auto Scaling> in the /Amazon EC2 Auto Scaling User Guide/ .
pspCooldown :: Lens' PutScalingPolicy (Maybe Int)
pspCooldown = lens _pspCooldown (\s a -> s {_pspCooldown = a})

-- | The aggregation type for the CloudWatch metrics. The valid values are @Minimum@ , @Maximum@ , and @Average@ . If the aggregation type is null, the value is treated as @Average@ . Valid only if the policy type is @StepScaling@ .
pspMetricAggregationType :: Lens' PutScalingPolicy (Maybe Text)
pspMetricAggregationType = lens _pspMetricAggregationType (\s a -> s {_pspMetricAggregationType = a})

-- | The minimum value to scale by when the adjustment type is @PercentChangeInCapacity@ . For example, suppose that you create a step scaling policy to scale out an Auto Scaling group by 25 percent and you specify a @MinAdjustmentMagnitude@ of 2. If the group has 4 instances and the scaling policy is performed, 25 percent of 4 is 1. However, because you specified a @MinAdjustmentMagnitude@ of 2, Amazon EC2 Auto Scaling scales out the group by 2 instances. Valid only if the policy type is @StepScaling@ or @SimpleScaling@ . For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-scaling-simple-step.html#as-scaling-adjustment Scaling adjustment types> in the /Amazon EC2 Auto Scaling User Guide/ .
pspMinAdjustmentMagnitude :: Lens' PutScalingPolicy (Maybe Int)
pspMinAdjustmentMagnitude = lens _pspMinAdjustmentMagnitude (\s a -> s {_pspMinAdjustmentMagnitude = a})

-- | The name of the Auto Scaling group.
pspAutoScalingGroupName :: Lens' PutScalingPolicy Text
pspAutoScalingGroupName = lens _pspAutoScalingGroupName (\s a -> s {_pspAutoScalingGroupName = a})

-- | The name of the policy.
pspPolicyName :: Lens' PutScalingPolicy Text
pspPolicyName = lens _pspPolicyName (\s a -> s {_pspPolicyName = a})

instance AWSRequest PutScalingPolicy where
  type Rs PutScalingPolicy = PutScalingPolicyResponse
  request = postQuery autoScaling
  response =
    receiveXMLWrapper
      "PutScalingPolicyResult"
      ( \s h x ->
          PutScalingPolicyResponse'
            <$> (x .@? "PolicyARN")
            <*> (x .@? "Alarms" .!@ mempty >>= may (parseXMLList "member"))
            <*> (pure (fromEnum s))
      )

instance Hashable PutScalingPolicy

instance NFData PutScalingPolicy

instance ToHeaders PutScalingPolicy where
  toHeaders = const mempty

instance ToPath PutScalingPolicy where
  toPath = const "/"

instance ToQuery PutScalingPolicy where
  toQuery PutScalingPolicy' {..} =
    mconcat
      [ "Action" =: ("PutScalingPolicy" :: ByteString),
        "Version" =: ("2011-01-01" :: ByteString),
        "MinAdjustmentStep" =: _pspMinAdjustmentStep,
        "EstimatedInstanceWarmup" =: _pspEstimatedInstanceWarmup,
        "Enabled" =: _pspEnabled,
        "PolicyType" =: _pspPolicyType,
        "StepAdjustments"
          =: toQuery (toQueryList "member" <$> _pspStepAdjustments),
        "TargetTrackingConfiguration" =: _pspTargetTrackingConfiguration,
        "AdjustmentType" =: _pspAdjustmentType,
        "ScalingAdjustment" =: _pspScalingAdjustment,
        "Cooldown" =: _pspCooldown,
        "MetricAggregationType" =: _pspMetricAggregationType,
        "MinAdjustmentMagnitude" =: _pspMinAdjustmentMagnitude,
        "AutoScalingGroupName" =: _pspAutoScalingGroupName,
        "PolicyName" =: _pspPolicyName
      ]

-- | Contains the output of PutScalingPolicy.
--
--
--
-- /See:/ 'putScalingPolicyResponse' smart constructor.
data PutScalingPolicyResponse = PutScalingPolicyResponse'
  { _psprsPolicyARN ::
      !(Maybe Text),
    _psprsAlarms :: !(Maybe [Alarm]),
    _psprsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutScalingPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psprsPolicyARN' - The Amazon Resource Name (ARN) of the policy.
--
-- * 'psprsAlarms' - The CloudWatch alarms created for the target tracking scaling policy.
--
-- * 'psprsResponseStatus' - -- | The response status code.
putScalingPolicyResponse ::
  -- | 'psprsResponseStatus'
  Int ->
  PutScalingPolicyResponse
putScalingPolicyResponse pResponseStatus_ =
  PutScalingPolicyResponse'
    { _psprsPolicyARN = Nothing,
      _psprsAlarms = Nothing,
      _psprsResponseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the policy.
psprsPolicyARN :: Lens' PutScalingPolicyResponse (Maybe Text)
psprsPolicyARN = lens _psprsPolicyARN (\s a -> s {_psprsPolicyARN = a})

-- | The CloudWatch alarms created for the target tracking scaling policy.
psprsAlarms :: Lens' PutScalingPolicyResponse [Alarm]
psprsAlarms = lens _psprsAlarms (\s a -> s {_psprsAlarms = a}) . _Default . _Coerce

-- | -- | The response status code.
psprsResponseStatus :: Lens' PutScalingPolicyResponse Int
psprsResponseStatus = lens _psprsResponseStatus (\s a -> s {_psprsResponseStatus = a})

instance NFData PutScalingPolicyResponse
