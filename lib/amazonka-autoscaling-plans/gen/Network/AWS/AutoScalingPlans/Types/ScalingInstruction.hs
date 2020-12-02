{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.ScalingInstruction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.ScalingInstruction where

import Network.AWS.AutoScalingPlans.Types.CustomizedLoadMetricSpecification
import Network.AWS.AutoScalingPlans.Types.PredefinedLoadMetricSpecification
import Network.AWS.AutoScalingPlans.Types.PredictiveScalingMaxCapacityBehavior
import Network.AWS.AutoScalingPlans.Types.PredictiveScalingMode
import Network.AWS.AutoScalingPlans.Types.ScalableDimension
import Network.AWS.AutoScalingPlans.Types.ScalingPolicyUpdateBehavior
import Network.AWS.AutoScalingPlans.Types.ServiceNamespace
import Network.AWS.AutoScalingPlans.Types.TargetTrackingConfiguration
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a scaling instruction for a scalable resource.
--
--
-- The scaling instruction is used in combination with a scaling plan, which is a set of instructions for configuring dynamic scaling and predictive scaling for the scalable resources in your application. Each scaling instruction applies to one resource.
--
-- AWS Auto Scaling creates target tracking scaling policies based on the scaling instructions. Target tracking scaling policies adjust the capacity of your scalable resource as required to maintain resource utilization at the target value that you specified.
--
-- AWS Auto Scaling also configures predictive scaling for your Amazon EC2 Auto Scaling groups using a subset of parameters, including the load metric, the scaling metric, the target value for the scaling metric, the predictive scaling mode (forecast and scale or forecast only), and the desired behavior when the forecast capacity exceeds the maximum capacity of the resource. With predictive scaling, AWS Auto Scaling generates forecasts with traffic predictions for the two days ahead and schedules scaling actions that proactively add and remove resource capacity to match the forecast.
--
-- We recommend waiting a minimum of 24 hours after creating an Auto Scaling group to configure predictive scaling. At minimum, there must be 24 hours of historical data to generate a forecast.
--
-- For more information, see <https://docs.aws.amazon.com/autoscaling/plans/userguide/auto-scaling-getting-started.html Getting Started with AWS Auto Scaling> .
--
--
-- /See:/ 'scalingInstruction' smart constructor.
data ScalingInstruction = ScalingInstruction'
  { _siScheduledActionBufferTime ::
      !(Maybe Nat),
    _siPredictiveScalingMaxCapacityBuffer :: !(Maybe Int),
    _siScalingPolicyUpdateBehavior ::
      !(Maybe ScalingPolicyUpdateBehavior),
    _siCustomizedLoadMetricSpecification ::
      !(Maybe CustomizedLoadMetricSpecification),
    _siPredictiveScalingMode ::
      !(Maybe PredictiveScalingMode),
    _siDisableDynamicScaling :: !(Maybe Bool),
    _siPredictiveScalingMaxCapacityBehavior ::
      !(Maybe PredictiveScalingMaxCapacityBehavior),
    _siPredefinedLoadMetricSpecification ::
      !(Maybe PredefinedLoadMetricSpecification),
    _siServiceNamespace :: !ServiceNamespace,
    _siResourceId :: !Text,
    _siScalableDimension :: !ScalableDimension,
    _siMinCapacity :: !Int,
    _siMaxCapacity :: !Int,
    _siTargetTrackingConfigurations ::
      ![TargetTrackingConfiguration]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ScalingInstruction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'siScheduledActionBufferTime' - The amount of time, in seconds, to buffer the run time of scheduled scaling actions when scaling out. For example, if the forecast says to add capacity at 10:00 AM, and the buffer time is 5 minutes, then the run time of the corresponding scheduled scaling action will be 9:55 AM. The intention is to give resources time to be provisioned. For example, it can take a few minutes to launch an EC2 instance. The actual amount of time required depends on several factors, such as the size of the instance and whether there are startup scripts to complete.  The value must be less than the forecast interval duration of 3600 seconds (60 minutes). The default is 300 seconds.  Only valid when configuring predictive scaling.
--
-- * 'siPredictiveScalingMaxCapacityBuffer' - The size of the capacity buffer to use when the forecast capacity is close to or exceeds the maximum capacity. The value is specified as a percentage relative to the forecast capacity. For example, if the buffer is 10, this means a 10 percent buffer, such that if the forecast capacity is 50, and the maximum capacity is 40, then the effective maximum capacity is 55. Only valid when configuring predictive scaling. Required if the __PredictiveScalingMaxCapacityBehavior__ is set to @SetMaxCapacityAboveForecastCapacity@ , and cannot be used otherwise. The range is 1-100.
--
-- * 'siScalingPolicyUpdateBehavior' - Controls whether a resource's externally created scaling policies are kept or replaced.  The default value is @KeepExternalPolicies@ . If the parameter is set to @ReplaceExternalPolicies@ , any scaling policies that are external to AWS Auto Scaling are deleted and new target tracking scaling policies created.  Only valid when configuring dynamic scaling.  Condition: The number of existing policies to be replaced must be less than or equal to 50. If there are more than 50 policies to be replaced, AWS Auto Scaling keeps all existing policies and does not create new ones.
--
-- * 'siCustomizedLoadMetricSpecification' - The customized load metric to use for predictive scaling. This parameter or a __PredefinedLoadMetricSpecification__ is required when configuring predictive scaling, and cannot be used otherwise.
--
-- * 'siPredictiveScalingMode' - The predictive scaling mode. The default value is @ForecastAndScale@ . Otherwise, AWS Auto Scaling forecasts capacity but does not create any scheduled scaling actions based on the capacity forecast.
--
-- * 'siDisableDynamicScaling' - Controls whether dynamic scaling by AWS Auto Scaling is disabled. When dynamic scaling is enabled, AWS Auto Scaling creates target tracking scaling policies based on the specified target tracking configurations.  The default is enabled (@false@ ).
--
-- * 'siPredictiveScalingMaxCapacityBehavior' - Defines the behavior that should be applied if the forecast capacity approaches or exceeds the maximum capacity specified for the resource. The default value is @SetForecastCapacityToMaxCapacity@ . The following are possible values:     * @SetForecastCapacityToMaxCapacity@ - AWS Auto Scaling cannot scale resource capacity higher than the maximum capacity. The maximum capacity is enforced as a hard limit.      * @SetMaxCapacityToForecastCapacity@ - AWS Auto Scaling may scale resource capacity higher than the maximum capacity to equal but not exceed forecast capacity.     * @SetMaxCapacityAboveForecastCapacity@ - AWS Auto Scaling may scale resource capacity higher than the maximum capacity by a specified buffer value. The intention is to give the target tracking scaling policy extra capacity if unexpected traffic occurs.  Only valid when configuring predictive scaling.
--
-- * 'siPredefinedLoadMetricSpecification' - The predefined load metric to use for predictive scaling. This parameter or a __CustomizedLoadMetricSpecification__ is required when configuring predictive scaling, and cannot be used otherwise.
--
-- * 'siServiceNamespace' - The namespace of the AWS service.
--
-- * 'siResourceId' - The ID of the resource. This string consists of the resource type and unique identifier.     * Auto Scaling group - The resource type is @autoScalingGroup@ and the unique identifier is the name of the Auto Scaling group. Example: @autoScalingGroup/my-asg@ .     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot Fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot Fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * DynamoDB table - The resource type is @table@ and the unique identifier is the resource ID. Example: @table/my-table@ .     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the resource ID. Example: @table/my-table/index/my-table-index@ .     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .
--
-- * 'siScalableDimension' - The scalable dimension associated with the resource.     * @autoscaling:autoScalingGroup:DesiredCapacity@ - The desired capacity of an Auto Scaling group.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot Fleet request.     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition and Aurora PostgreSQL-compatible edition.
--
-- * 'siMinCapacity' - The minimum capacity of the resource.
--
-- * 'siMaxCapacity' - The maximum capacity of the resource. The exception to this upper limit is if you specify a non-default setting for __PredictiveScalingMaxCapacityBehavior__ .
--
-- * 'siTargetTrackingConfigurations' - The structure that defines new target tracking configurations (up to 10). Each of these structures includes a specific scaling metric and a target value for the metric, along with various parameters to use with dynamic scaling.  With predictive scaling and dynamic scaling, the resource scales based on the target tracking configuration that provides the largest capacity for both scale in and scale out.  Condition: The scaling metric must be unique across target tracking configurations.
scalingInstruction ::
  -- | 'siServiceNamespace'
  ServiceNamespace ->
  -- | 'siResourceId'
  Text ->
  -- | 'siScalableDimension'
  ScalableDimension ->
  -- | 'siMinCapacity'
  Int ->
  -- | 'siMaxCapacity'
  Int ->
  ScalingInstruction
scalingInstruction
  pServiceNamespace_
  pResourceId_
  pScalableDimension_
  pMinCapacity_
  pMaxCapacity_ =
    ScalingInstruction'
      { _siScheduledActionBufferTime = Nothing,
        _siPredictiveScalingMaxCapacityBuffer = Nothing,
        _siScalingPolicyUpdateBehavior = Nothing,
        _siCustomizedLoadMetricSpecification = Nothing,
        _siPredictiveScalingMode = Nothing,
        _siDisableDynamicScaling = Nothing,
        _siPredictiveScalingMaxCapacityBehavior = Nothing,
        _siPredefinedLoadMetricSpecification = Nothing,
        _siServiceNamespace = pServiceNamespace_,
        _siResourceId = pResourceId_,
        _siScalableDimension = pScalableDimension_,
        _siMinCapacity = pMinCapacity_,
        _siMaxCapacity = pMaxCapacity_,
        _siTargetTrackingConfigurations = mempty
      }

-- | The amount of time, in seconds, to buffer the run time of scheduled scaling actions when scaling out. For example, if the forecast says to add capacity at 10:00 AM, and the buffer time is 5 minutes, then the run time of the corresponding scheduled scaling action will be 9:55 AM. The intention is to give resources time to be provisioned. For example, it can take a few minutes to launch an EC2 instance. The actual amount of time required depends on several factors, such as the size of the instance and whether there are startup scripts to complete.  The value must be less than the forecast interval duration of 3600 seconds (60 minutes). The default is 300 seconds.  Only valid when configuring predictive scaling.
siScheduledActionBufferTime :: Lens' ScalingInstruction (Maybe Natural)
siScheduledActionBufferTime = lens _siScheduledActionBufferTime (\s a -> s {_siScheduledActionBufferTime = a}) . mapping _Nat

-- | The size of the capacity buffer to use when the forecast capacity is close to or exceeds the maximum capacity. The value is specified as a percentage relative to the forecast capacity. For example, if the buffer is 10, this means a 10 percent buffer, such that if the forecast capacity is 50, and the maximum capacity is 40, then the effective maximum capacity is 55. Only valid when configuring predictive scaling. Required if the __PredictiveScalingMaxCapacityBehavior__ is set to @SetMaxCapacityAboveForecastCapacity@ , and cannot be used otherwise. The range is 1-100.
siPredictiveScalingMaxCapacityBuffer :: Lens' ScalingInstruction (Maybe Int)
siPredictiveScalingMaxCapacityBuffer = lens _siPredictiveScalingMaxCapacityBuffer (\s a -> s {_siPredictiveScalingMaxCapacityBuffer = a})

-- | Controls whether a resource's externally created scaling policies are kept or replaced.  The default value is @KeepExternalPolicies@ . If the parameter is set to @ReplaceExternalPolicies@ , any scaling policies that are external to AWS Auto Scaling are deleted and new target tracking scaling policies created.  Only valid when configuring dynamic scaling.  Condition: The number of existing policies to be replaced must be less than or equal to 50. If there are more than 50 policies to be replaced, AWS Auto Scaling keeps all existing policies and does not create new ones.
siScalingPolicyUpdateBehavior :: Lens' ScalingInstruction (Maybe ScalingPolicyUpdateBehavior)
siScalingPolicyUpdateBehavior = lens _siScalingPolicyUpdateBehavior (\s a -> s {_siScalingPolicyUpdateBehavior = a})

-- | The customized load metric to use for predictive scaling. This parameter or a __PredefinedLoadMetricSpecification__ is required when configuring predictive scaling, and cannot be used otherwise.
siCustomizedLoadMetricSpecification :: Lens' ScalingInstruction (Maybe CustomizedLoadMetricSpecification)
siCustomizedLoadMetricSpecification = lens _siCustomizedLoadMetricSpecification (\s a -> s {_siCustomizedLoadMetricSpecification = a})

-- | The predictive scaling mode. The default value is @ForecastAndScale@ . Otherwise, AWS Auto Scaling forecasts capacity but does not create any scheduled scaling actions based on the capacity forecast.
siPredictiveScalingMode :: Lens' ScalingInstruction (Maybe PredictiveScalingMode)
siPredictiveScalingMode = lens _siPredictiveScalingMode (\s a -> s {_siPredictiveScalingMode = a})

-- | Controls whether dynamic scaling by AWS Auto Scaling is disabled. When dynamic scaling is enabled, AWS Auto Scaling creates target tracking scaling policies based on the specified target tracking configurations.  The default is enabled (@false@ ).
siDisableDynamicScaling :: Lens' ScalingInstruction (Maybe Bool)
siDisableDynamicScaling = lens _siDisableDynamicScaling (\s a -> s {_siDisableDynamicScaling = a})

-- | Defines the behavior that should be applied if the forecast capacity approaches or exceeds the maximum capacity specified for the resource. The default value is @SetForecastCapacityToMaxCapacity@ . The following are possible values:     * @SetForecastCapacityToMaxCapacity@ - AWS Auto Scaling cannot scale resource capacity higher than the maximum capacity. The maximum capacity is enforced as a hard limit.      * @SetMaxCapacityToForecastCapacity@ - AWS Auto Scaling may scale resource capacity higher than the maximum capacity to equal but not exceed forecast capacity.     * @SetMaxCapacityAboveForecastCapacity@ - AWS Auto Scaling may scale resource capacity higher than the maximum capacity by a specified buffer value. The intention is to give the target tracking scaling policy extra capacity if unexpected traffic occurs.  Only valid when configuring predictive scaling.
siPredictiveScalingMaxCapacityBehavior :: Lens' ScalingInstruction (Maybe PredictiveScalingMaxCapacityBehavior)
siPredictiveScalingMaxCapacityBehavior = lens _siPredictiveScalingMaxCapacityBehavior (\s a -> s {_siPredictiveScalingMaxCapacityBehavior = a})

-- | The predefined load metric to use for predictive scaling. This parameter or a __CustomizedLoadMetricSpecification__ is required when configuring predictive scaling, and cannot be used otherwise.
siPredefinedLoadMetricSpecification :: Lens' ScalingInstruction (Maybe PredefinedLoadMetricSpecification)
siPredefinedLoadMetricSpecification = lens _siPredefinedLoadMetricSpecification (\s a -> s {_siPredefinedLoadMetricSpecification = a})

-- | The namespace of the AWS service.
siServiceNamespace :: Lens' ScalingInstruction ServiceNamespace
siServiceNamespace = lens _siServiceNamespace (\s a -> s {_siServiceNamespace = a})

-- | The ID of the resource. This string consists of the resource type and unique identifier.     * Auto Scaling group - The resource type is @autoScalingGroup@ and the unique identifier is the name of the Auto Scaling group. Example: @autoScalingGroup/my-asg@ .     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot Fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot Fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * DynamoDB table - The resource type is @table@ and the unique identifier is the resource ID. Example: @table/my-table@ .     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the resource ID. Example: @table/my-table/index/my-table-index@ .     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .
siResourceId :: Lens' ScalingInstruction Text
siResourceId = lens _siResourceId (\s a -> s {_siResourceId = a})

-- | The scalable dimension associated with the resource.     * @autoscaling:autoScalingGroup:DesiredCapacity@ - The desired capacity of an Auto Scaling group.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot Fleet request.     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition and Aurora PostgreSQL-compatible edition.
siScalableDimension :: Lens' ScalingInstruction ScalableDimension
siScalableDimension = lens _siScalableDimension (\s a -> s {_siScalableDimension = a})

-- | The minimum capacity of the resource.
siMinCapacity :: Lens' ScalingInstruction Int
siMinCapacity = lens _siMinCapacity (\s a -> s {_siMinCapacity = a})

-- | The maximum capacity of the resource. The exception to this upper limit is if you specify a non-default setting for __PredictiveScalingMaxCapacityBehavior__ .
siMaxCapacity :: Lens' ScalingInstruction Int
siMaxCapacity = lens _siMaxCapacity (\s a -> s {_siMaxCapacity = a})

-- | The structure that defines new target tracking configurations (up to 10). Each of these structures includes a specific scaling metric and a target value for the metric, along with various parameters to use with dynamic scaling.  With predictive scaling and dynamic scaling, the resource scales based on the target tracking configuration that provides the largest capacity for both scale in and scale out.  Condition: The scaling metric must be unique across target tracking configurations.
siTargetTrackingConfigurations :: Lens' ScalingInstruction [TargetTrackingConfiguration]
siTargetTrackingConfigurations = lens _siTargetTrackingConfigurations (\s a -> s {_siTargetTrackingConfigurations = a}) . _Coerce

instance FromJSON ScalingInstruction where
  parseJSON =
    withObject
      "ScalingInstruction"
      ( \x ->
          ScalingInstruction'
            <$> (x .:? "ScheduledActionBufferTime")
            <*> (x .:? "PredictiveScalingMaxCapacityBuffer")
            <*> (x .:? "ScalingPolicyUpdateBehavior")
            <*> (x .:? "CustomizedLoadMetricSpecification")
            <*> (x .:? "PredictiveScalingMode")
            <*> (x .:? "DisableDynamicScaling")
            <*> (x .:? "PredictiveScalingMaxCapacityBehavior")
            <*> (x .:? "PredefinedLoadMetricSpecification")
            <*> (x .: "ServiceNamespace")
            <*> (x .: "ResourceId")
            <*> (x .: "ScalableDimension")
            <*> (x .: "MinCapacity")
            <*> (x .: "MaxCapacity")
            <*> (x .:? "TargetTrackingConfigurations" .!= mempty)
      )

instance Hashable ScalingInstruction

instance NFData ScalingInstruction

instance ToJSON ScalingInstruction where
  toJSON ScalingInstruction' {..} =
    object
      ( catMaybes
          [ ("ScheduledActionBufferTime" .=) <$> _siScheduledActionBufferTime,
            ("PredictiveScalingMaxCapacityBuffer" .=)
              <$> _siPredictiveScalingMaxCapacityBuffer,
            ("ScalingPolicyUpdateBehavior" .=)
              <$> _siScalingPolicyUpdateBehavior,
            ("CustomizedLoadMetricSpecification" .=)
              <$> _siCustomizedLoadMetricSpecification,
            ("PredictiveScalingMode" .=) <$> _siPredictiveScalingMode,
            ("DisableDynamicScaling" .=) <$> _siDisableDynamicScaling,
            ("PredictiveScalingMaxCapacityBehavior" .=)
              <$> _siPredictiveScalingMaxCapacityBehavior,
            ("PredefinedLoadMetricSpecification" .=)
              <$> _siPredefinedLoadMetricSpecification,
            Just ("ServiceNamespace" .= _siServiceNamespace),
            Just ("ResourceId" .= _siResourceId),
            Just ("ScalableDimension" .= _siScalableDimension),
            Just ("MinCapacity" .= _siMinCapacity),
            Just ("MaxCapacity" .= _siMaxCapacity),
            Just
              ( "TargetTrackingConfigurations"
                  .= _siTargetTrackingConfigurations
              )
          ]
      )
