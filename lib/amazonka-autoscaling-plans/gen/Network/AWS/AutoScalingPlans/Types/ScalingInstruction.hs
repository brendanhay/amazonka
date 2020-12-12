{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.ScalingInstruction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.ScalingInstruction
  ( ScalingInstruction (..),

    -- * Smart constructor
    mkScalingInstruction,

    -- * Lenses
    siScheduledActionBufferTime,
    siPredictiveScalingMaxCapacityBuffer,
    siScalingPolicyUpdateBehavior,
    siCustomizedLoadMetricSpecification,
    siPredictiveScalingMode,
    siDisableDynamicScaling,
    siPredictiveScalingMaxCapacityBehavior,
    siPredefinedLoadMetricSpecification,
    siServiceNamespace,
    siResourceId,
    siScalableDimension,
    siMinCapacity,
    siMaxCapacity,
    siTargetTrackingConfigurations,
  )
where

import Network.AWS.AutoScalingPlans.Types.CustomizedLoadMetricSpecification
import Network.AWS.AutoScalingPlans.Types.PredefinedLoadMetricSpecification
import Network.AWS.AutoScalingPlans.Types.PredictiveScalingMaxCapacityBehavior
import Network.AWS.AutoScalingPlans.Types.PredictiveScalingMode
import Network.AWS.AutoScalingPlans.Types.ScalableDimension
import Network.AWS.AutoScalingPlans.Types.ScalingPolicyUpdateBehavior
import Network.AWS.AutoScalingPlans.Types.ServiceNamespace
import Network.AWS.AutoScalingPlans.Types.TargetTrackingConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a scaling instruction for a scalable resource.
--
-- The scaling instruction is used in combination with a scaling plan, which is a set of instructions for configuring dynamic scaling and predictive scaling for the scalable resources in your application. Each scaling instruction applies to one resource.
-- AWS Auto Scaling creates target tracking scaling policies based on the scaling instructions. Target tracking scaling policies adjust the capacity of your scalable resource as required to maintain resource utilization at the target value that you specified.
-- AWS Auto Scaling also configures predictive scaling for your Amazon EC2 Auto Scaling groups using a subset of parameters, including the load metric, the scaling metric, the target value for the scaling metric, the predictive scaling mode (forecast and scale or forecast only), and the desired behavior when the forecast capacity exceeds the maximum capacity of the resource. With predictive scaling, AWS Auto Scaling generates forecasts with traffic predictions for the two days ahead and schedules scaling actions that proactively add and remove resource capacity to match the forecast.
-- We recommend waiting a minimum of 24 hours after creating an Auto Scaling group to configure predictive scaling. At minimum, there must be 24 hours of historical data to generate a forecast.
-- For more information, see <https://docs.aws.amazon.com/autoscaling/plans/userguide/auto-scaling-getting-started.html Getting Started with AWS Auto Scaling> .
--
-- /See:/ 'mkScalingInstruction' smart constructor.
data ScalingInstruction = ScalingInstruction'
  { scheduledActionBufferTime ::
      Lude.Maybe Lude.Natural,
    predictiveScalingMaxCapacityBuffer ::
      Lude.Maybe Lude.Int,
    scalingPolicyUpdateBehavior ::
      Lude.Maybe ScalingPolicyUpdateBehavior,
    customizedLoadMetricSpecification ::
      Lude.Maybe CustomizedLoadMetricSpecification,
    predictiveScalingMode ::
      Lude.Maybe PredictiveScalingMode,
    disableDynamicScaling :: Lude.Maybe Lude.Bool,
    predictiveScalingMaxCapacityBehavior ::
      Lude.Maybe PredictiveScalingMaxCapacityBehavior,
    predefinedLoadMetricSpecification ::
      Lude.Maybe PredefinedLoadMetricSpecification,
    serviceNamespace :: ServiceNamespace,
    resourceId :: Lude.Text,
    scalableDimension :: ScalableDimension,
    minCapacity :: Lude.Int,
    maxCapacity :: Lude.Int,
    targetTrackingConfigurations ::
      [TargetTrackingConfiguration]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ScalingInstruction' with the minimum fields required to make a request.
--
-- * 'customizedLoadMetricSpecification' - The customized load metric to use for predictive scaling. This parameter or a __PredefinedLoadMetricSpecification__ is required when configuring predictive scaling, and cannot be used otherwise.
-- * 'disableDynamicScaling' - Controls whether dynamic scaling by AWS Auto Scaling is disabled. When dynamic scaling is enabled, AWS Auto Scaling creates target tracking scaling policies based on the specified target tracking configurations.
--
-- The default is enabled (@false@ ).
-- * 'maxCapacity' - The maximum capacity of the resource. The exception to this upper limit is if you specify a non-default setting for __PredictiveScalingMaxCapacityBehavior__ .
-- * 'minCapacity' - The minimum capacity of the resource.
-- * 'predefinedLoadMetricSpecification' - The predefined load metric to use for predictive scaling. This parameter or a __CustomizedLoadMetricSpecification__ is required when configuring predictive scaling, and cannot be used otherwise.
-- * 'predictiveScalingMaxCapacityBehavior' - Defines the behavior that should be applied if the forecast capacity approaches or exceeds the maximum capacity specified for the resource. The default value is @SetForecastCapacityToMaxCapacity@ .
--
-- The following are possible values:
--
--     * @SetForecastCapacityToMaxCapacity@ - AWS Auto Scaling cannot scale resource capacity higher than the maximum capacity. The maximum capacity is enforced as a hard limit.
--
--
--     * @SetMaxCapacityToForecastCapacity@ - AWS Auto Scaling may scale resource capacity higher than the maximum capacity to equal but not exceed forecast capacity.
--
--
--     * @SetMaxCapacityAboveForecastCapacity@ - AWS Auto Scaling may scale resource capacity higher than the maximum capacity by a specified buffer value. The intention is to give the target tracking scaling policy extra capacity if unexpected traffic occurs.
--
--
-- Only valid when configuring predictive scaling.
-- * 'predictiveScalingMaxCapacityBuffer' - The size of the capacity buffer to use when the forecast capacity is close to or exceeds the maximum capacity. The value is specified as a percentage relative to the forecast capacity. For example, if the buffer is 10, this means a 10 percent buffer, such that if the forecast capacity is 50, and the maximum capacity is 40, then the effective maximum capacity is 55.
--
-- Only valid when configuring predictive scaling. Required if the __PredictiveScalingMaxCapacityBehavior__ is set to @SetMaxCapacityAboveForecastCapacity@ , and cannot be used otherwise.
-- The range is 1-100.
-- * 'predictiveScalingMode' - The predictive scaling mode. The default value is @ForecastAndScale@ . Otherwise, AWS Auto Scaling forecasts capacity but does not create any scheduled scaling actions based on the capacity forecast.
-- * 'resourceId' - The ID of the resource. This string consists of the resource type and unique identifier.
--
--
--     * Auto Scaling group - The resource type is @autoScalingGroup@ and the unique identifier is the name of the Auto Scaling group. Example: @autoScalingGroup/my-asg@ .
--
--
--     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .
--
--
--     * Spot Fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot Fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .
--
--
--     * DynamoDB table - The resource type is @table@ and the unique identifier is the resource ID. Example: @table/my-table@ .
--
--
--     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the resource ID. Example: @table/my-table/index/my-table-index@ .
--
--
--     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .
--
--
-- * 'scalableDimension' - The scalable dimension associated with the resource.
--
--
--     * @autoscaling:autoScalingGroup:DesiredCapacity@ - The desired capacity of an Auto Scaling group.
--
--
--     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.
--
--
--     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot Fleet request.
--
--
--     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.
--
--
--     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.
--
--
--     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.
--
--
--     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.
--
--
--     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition and Aurora PostgreSQL-compatible edition.
--
--
-- * 'scalingPolicyUpdateBehavior' - Controls whether a resource's externally created scaling policies are kept or replaced.
--
-- The default value is @KeepExternalPolicies@ . If the parameter is set to @ReplaceExternalPolicies@ , any scaling policies that are external to AWS Auto Scaling are deleted and new target tracking scaling policies created.
-- Only valid when configuring dynamic scaling.
-- Condition: The number of existing policies to be replaced must be less than or equal to 50. If there are more than 50 policies to be replaced, AWS Auto Scaling keeps all existing policies and does not create new ones.
-- * 'scheduledActionBufferTime' - The amount of time, in seconds, to buffer the run time of scheduled scaling actions when scaling out. For example, if the forecast says to add capacity at 10:00 AM, and the buffer time is 5 minutes, then the run time of the corresponding scheduled scaling action will be 9:55 AM. The intention is to give resources time to be provisioned. For example, it can take a few minutes to launch an EC2 instance. The actual amount of time required depends on several factors, such as the size of the instance and whether there are startup scripts to complete.
--
-- The value must be less than the forecast interval duration of 3600 seconds (60 minutes). The default is 300 seconds.
-- Only valid when configuring predictive scaling.
-- * 'serviceNamespace' - The namespace of the AWS service.
-- * 'targetTrackingConfigurations' - The structure that defines new target tracking configurations (up to 10). Each of these structures includes a specific scaling metric and a target value for the metric, along with various parameters to use with dynamic scaling.
--
-- With predictive scaling and dynamic scaling, the resource scales based on the target tracking configuration that provides the largest capacity for both scale in and scale out.
-- Condition: The scaling metric must be unique across target tracking configurations.
mkScalingInstruction ::
  -- | 'serviceNamespace'
  ServiceNamespace ->
  -- | 'resourceId'
  Lude.Text ->
  -- | 'scalableDimension'
  ScalableDimension ->
  -- | 'minCapacity'
  Lude.Int ->
  -- | 'maxCapacity'
  Lude.Int ->
  ScalingInstruction
mkScalingInstruction
  pServiceNamespace_
  pResourceId_
  pScalableDimension_
  pMinCapacity_
  pMaxCapacity_ =
    ScalingInstruction'
      { scheduledActionBufferTime = Lude.Nothing,
        predictiveScalingMaxCapacityBuffer = Lude.Nothing,
        scalingPolicyUpdateBehavior = Lude.Nothing,
        customizedLoadMetricSpecification = Lude.Nothing,
        predictiveScalingMode = Lude.Nothing,
        disableDynamicScaling = Lude.Nothing,
        predictiveScalingMaxCapacityBehavior = Lude.Nothing,
        predefinedLoadMetricSpecification = Lude.Nothing,
        serviceNamespace = pServiceNamespace_,
        resourceId = pResourceId_,
        scalableDimension = pScalableDimension_,
        minCapacity = pMinCapacity_,
        maxCapacity = pMaxCapacity_,
        targetTrackingConfigurations = Lude.mempty
      }

-- | The amount of time, in seconds, to buffer the run time of scheduled scaling actions when scaling out. For example, if the forecast says to add capacity at 10:00 AM, and the buffer time is 5 minutes, then the run time of the corresponding scheduled scaling action will be 9:55 AM. The intention is to give resources time to be provisioned. For example, it can take a few minutes to launch an EC2 instance. The actual amount of time required depends on several factors, such as the size of the instance and whether there are startup scripts to complete.
--
-- The value must be less than the forecast interval duration of 3600 seconds (60 minutes). The default is 300 seconds.
-- Only valid when configuring predictive scaling.
--
-- /Note:/ Consider using 'scheduledActionBufferTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siScheduledActionBufferTime :: Lens.Lens' ScalingInstruction (Lude.Maybe Lude.Natural)
siScheduledActionBufferTime = Lens.lens (scheduledActionBufferTime :: ScalingInstruction -> Lude.Maybe Lude.Natural) (\s a -> s {scheduledActionBufferTime = a} :: ScalingInstruction)
{-# DEPRECATED siScheduledActionBufferTime "Use generic-lens or generic-optics with 'scheduledActionBufferTime' instead." #-}

-- | The size of the capacity buffer to use when the forecast capacity is close to or exceeds the maximum capacity. The value is specified as a percentage relative to the forecast capacity. For example, if the buffer is 10, this means a 10 percent buffer, such that if the forecast capacity is 50, and the maximum capacity is 40, then the effective maximum capacity is 55.
--
-- Only valid when configuring predictive scaling. Required if the __PredictiveScalingMaxCapacityBehavior__ is set to @SetMaxCapacityAboveForecastCapacity@ , and cannot be used otherwise.
-- The range is 1-100.
--
-- /Note:/ Consider using 'predictiveScalingMaxCapacityBuffer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siPredictiveScalingMaxCapacityBuffer :: Lens.Lens' ScalingInstruction (Lude.Maybe Lude.Int)
siPredictiveScalingMaxCapacityBuffer = Lens.lens (predictiveScalingMaxCapacityBuffer :: ScalingInstruction -> Lude.Maybe Lude.Int) (\s a -> s {predictiveScalingMaxCapacityBuffer = a} :: ScalingInstruction)
{-# DEPRECATED siPredictiveScalingMaxCapacityBuffer "Use generic-lens or generic-optics with 'predictiveScalingMaxCapacityBuffer' instead." #-}

-- | Controls whether a resource's externally created scaling policies are kept or replaced.
--
-- The default value is @KeepExternalPolicies@ . If the parameter is set to @ReplaceExternalPolicies@ , any scaling policies that are external to AWS Auto Scaling are deleted and new target tracking scaling policies created.
-- Only valid when configuring dynamic scaling.
-- Condition: The number of existing policies to be replaced must be less than or equal to 50. If there are more than 50 policies to be replaced, AWS Auto Scaling keeps all existing policies and does not create new ones.
--
-- /Note:/ Consider using 'scalingPolicyUpdateBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siScalingPolicyUpdateBehavior :: Lens.Lens' ScalingInstruction (Lude.Maybe ScalingPolicyUpdateBehavior)
siScalingPolicyUpdateBehavior = Lens.lens (scalingPolicyUpdateBehavior :: ScalingInstruction -> Lude.Maybe ScalingPolicyUpdateBehavior) (\s a -> s {scalingPolicyUpdateBehavior = a} :: ScalingInstruction)
{-# DEPRECATED siScalingPolicyUpdateBehavior "Use generic-lens or generic-optics with 'scalingPolicyUpdateBehavior' instead." #-}

-- | The customized load metric to use for predictive scaling. This parameter or a __PredefinedLoadMetricSpecification__ is required when configuring predictive scaling, and cannot be used otherwise.
--
-- /Note:/ Consider using 'customizedLoadMetricSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siCustomizedLoadMetricSpecification :: Lens.Lens' ScalingInstruction (Lude.Maybe CustomizedLoadMetricSpecification)
siCustomizedLoadMetricSpecification = Lens.lens (customizedLoadMetricSpecification :: ScalingInstruction -> Lude.Maybe CustomizedLoadMetricSpecification) (\s a -> s {customizedLoadMetricSpecification = a} :: ScalingInstruction)
{-# DEPRECATED siCustomizedLoadMetricSpecification "Use generic-lens or generic-optics with 'customizedLoadMetricSpecification' instead." #-}

-- | The predictive scaling mode. The default value is @ForecastAndScale@ . Otherwise, AWS Auto Scaling forecasts capacity but does not create any scheduled scaling actions based on the capacity forecast.
--
-- /Note:/ Consider using 'predictiveScalingMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siPredictiveScalingMode :: Lens.Lens' ScalingInstruction (Lude.Maybe PredictiveScalingMode)
siPredictiveScalingMode = Lens.lens (predictiveScalingMode :: ScalingInstruction -> Lude.Maybe PredictiveScalingMode) (\s a -> s {predictiveScalingMode = a} :: ScalingInstruction)
{-# DEPRECATED siPredictiveScalingMode "Use generic-lens or generic-optics with 'predictiveScalingMode' instead." #-}

-- | Controls whether dynamic scaling by AWS Auto Scaling is disabled. When dynamic scaling is enabled, AWS Auto Scaling creates target tracking scaling policies based on the specified target tracking configurations.
--
-- The default is enabled (@false@ ).
--
-- /Note:/ Consider using 'disableDynamicScaling' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siDisableDynamicScaling :: Lens.Lens' ScalingInstruction (Lude.Maybe Lude.Bool)
siDisableDynamicScaling = Lens.lens (disableDynamicScaling :: ScalingInstruction -> Lude.Maybe Lude.Bool) (\s a -> s {disableDynamicScaling = a} :: ScalingInstruction)
{-# DEPRECATED siDisableDynamicScaling "Use generic-lens or generic-optics with 'disableDynamicScaling' instead." #-}

-- | Defines the behavior that should be applied if the forecast capacity approaches or exceeds the maximum capacity specified for the resource. The default value is @SetForecastCapacityToMaxCapacity@ .
--
-- The following are possible values:
--
--     * @SetForecastCapacityToMaxCapacity@ - AWS Auto Scaling cannot scale resource capacity higher than the maximum capacity. The maximum capacity is enforced as a hard limit.
--
--
--     * @SetMaxCapacityToForecastCapacity@ - AWS Auto Scaling may scale resource capacity higher than the maximum capacity to equal but not exceed forecast capacity.
--
--
--     * @SetMaxCapacityAboveForecastCapacity@ - AWS Auto Scaling may scale resource capacity higher than the maximum capacity by a specified buffer value. The intention is to give the target tracking scaling policy extra capacity if unexpected traffic occurs.
--
--
-- Only valid when configuring predictive scaling.
--
-- /Note:/ Consider using 'predictiveScalingMaxCapacityBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siPredictiveScalingMaxCapacityBehavior :: Lens.Lens' ScalingInstruction (Lude.Maybe PredictiveScalingMaxCapacityBehavior)
siPredictiveScalingMaxCapacityBehavior = Lens.lens (predictiveScalingMaxCapacityBehavior :: ScalingInstruction -> Lude.Maybe PredictiveScalingMaxCapacityBehavior) (\s a -> s {predictiveScalingMaxCapacityBehavior = a} :: ScalingInstruction)
{-# DEPRECATED siPredictiveScalingMaxCapacityBehavior "Use generic-lens or generic-optics with 'predictiveScalingMaxCapacityBehavior' instead." #-}

-- | The predefined load metric to use for predictive scaling. This parameter or a __CustomizedLoadMetricSpecification__ is required when configuring predictive scaling, and cannot be used otherwise.
--
-- /Note:/ Consider using 'predefinedLoadMetricSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siPredefinedLoadMetricSpecification :: Lens.Lens' ScalingInstruction (Lude.Maybe PredefinedLoadMetricSpecification)
siPredefinedLoadMetricSpecification = Lens.lens (predefinedLoadMetricSpecification :: ScalingInstruction -> Lude.Maybe PredefinedLoadMetricSpecification) (\s a -> s {predefinedLoadMetricSpecification = a} :: ScalingInstruction)
{-# DEPRECATED siPredefinedLoadMetricSpecification "Use generic-lens or generic-optics with 'predefinedLoadMetricSpecification' instead." #-}

-- | The namespace of the AWS service.
--
-- /Note:/ Consider using 'serviceNamespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siServiceNamespace :: Lens.Lens' ScalingInstruction ServiceNamespace
siServiceNamespace = Lens.lens (serviceNamespace :: ScalingInstruction -> ServiceNamespace) (\s a -> s {serviceNamespace = a} :: ScalingInstruction)
{-# DEPRECATED siServiceNamespace "Use generic-lens or generic-optics with 'serviceNamespace' instead." #-}

-- | The ID of the resource. This string consists of the resource type and unique identifier.
--
--
--     * Auto Scaling group - The resource type is @autoScalingGroup@ and the unique identifier is the name of the Auto Scaling group. Example: @autoScalingGroup/my-asg@ .
--
--
--     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .
--
--
--     * Spot Fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot Fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .
--
--
--     * DynamoDB table - The resource type is @table@ and the unique identifier is the resource ID. Example: @table/my-table@ .
--
--
--     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the resource ID. Example: @table/my-table/index/my-table-index@ .
--
--
--     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .
--
--
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siResourceId :: Lens.Lens' ScalingInstruction Lude.Text
siResourceId = Lens.lens (resourceId :: ScalingInstruction -> Lude.Text) (\s a -> s {resourceId = a} :: ScalingInstruction)
{-# DEPRECATED siResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The scalable dimension associated with the resource.
--
--
--     * @autoscaling:autoScalingGroup:DesiredCapacity@ - The desired capacity of an Auto Scaling group.
--
--
--     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.
--
--
--     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot Fleet request.
--
--
--     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.
--
--
--     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.
--
--
--     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.
--
--
--     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.
--
--
--     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition and Aurora PostgreSQL-compatible edition.
--
--
--
-- /Note:/ Consider using 'scalableDimension' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siScalableDimension :: Lens.Lens' ScalingInstruction ScalableDimension
siScalableDimension = Lens.lens (scalableDimension :: ScalingInstruction -> ScalableDimension) (\s a -> s {scalableDimension = a} :: ScalingInstruction)
{-# DEPRECATED siScalableDimension "Use generic-lens or generic-optics with 'scalableDimension' instead." #-}

-- | The minimum capacity of the resource.
--
-- /Note:/ Consider using 'minCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siMinCapacity :: Lens.Lens' ScalingInstruction Lude.Int
siMinCapacity = Lens.lens (minCapacity :: ScalingInstruction -> Lude.Int) (\s a -> s {minCapacity = a} :: ScalingInstruction)
{-# DEPRECATED siMinCapacity "Use generic-lens or generic-optics with 'minCapacity' instead." #-}

-- | The maximum capacity of the resource. The exception to this upper limit is if you specify a non-default setting for __PredictiveScalingMaxCapacityBehavior__ .
--
-- /Note:/ Consider using 'maxCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siMaxCapacity :: Lens.Lens' ScalingInstruction Lude.Int
siMaxCapacity = Lens.lens (maxCapacity :: ScalingInstruction -> Lude.Int) (\s a -> s {maxCapacity = a} :: ScalingInstruction)
{-# DEPRECATED siMaxCapacity "Use generic-lens or generic-optics with 'maxCapacity' instead." #-}

-- | The structure that defines new target tracking configurations (up to 10). Each of these structures includes a specific scaling metric and a target value for the metric, along with various parameters to use with dynamic scaling.
--
-- With predictive scaling and dynamic scaling, the resource scales based on the target tracking configuration that provides the largest capacity for both scale in and scale out.
-- Condition: The scaling metric must be unique across target tracking configurations.
--
-- /Note:/ Consider using 'targetTrackingConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siTargetTrackingConfigurations :: Lens.Lens' ScalingInstruction [TargetTrackingConfiguration]
siTargetTrackingConfigurations = Lens.lens (targetTrackingConfigurations :: ScalingInstruction -> [TargetTrackingConfiguration]) (\s a -> s {targetTrackingConfigurations = a} :: ScalingInstruction)
{-# DEPRECATED siTargetTrackingConfigurations "Use generic-lens or generic-optics with 'targetTrackingConfigurations' instead." #-}

instance Lude.FromJSON ScalingInstruction where
  parseJSON =
    Lude.withObject
      "ScalingInstruction"
      ( \x ->
          ScalingInstruction'
            Lude.<$> (x Lude..:? "ScheduledActionBufferTime")
            Lude.<*> (x Lude..:? "PredictiveScalingMaxCapacityBuffer")
            Lude.<*> (x Lude..:? "ScalingPolicyUpdateBehavior")
            Lude.<*> (x Lude..:? "CustomizedLoadMetricSpecification")
            Lude.<*> (x Lude..:? "PredictiveScalingMode")
            Lude.<*> (x Lude..:? "DisableDynamicScaling")
            Lude.<*> (x Lude..:? "PredictiveScalingMaxCapacityBehavior")
            Lude.<*> (x Lude..:? "PredefinedLoadMetricSpecification")
            Lude.<*> (x Lude..: "ServiceNamespace")
            Lude.<*> (x Lude..: "ResourceId")
            Lude.<*> (x Lude..: "ScalableDimension")
            Lude.<*> (x Lude..: "MinCapacity")
            Lude.<*> (x Lude..: "MaxCapacity")
            Lude.<*> (x Lude..:? "TargetTrackingConfigurations" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON ScalingInstruction where
  toJSON ScalingInstruction' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ScheduledActionBufferTime" Lude..=)
              Lude.<$> scheduledActionBufferTime,
            ("PredictiveScalingMaxCapacityBuffer" Lude..=)
              Lude.<$> predictiveScalingMaxCapacityBuffer,
            ("ScalingPolicyUpdateBehavior" Lude..=)
              Lude.<$> scalingPolicyUpdateBehavior,
            ("CustomizedLoadMetricSpecification" Lude..=)
              Lude.<$> customizedLoadMetricSpecification,
            ("PredictiveScalingMode" Lude..=) Lude.<$> predictiveScalingMode,
            ("DisableDynamicScaling" Lude..=) Lude.<$> disableDynamicScaling,
            ("PredictiveScalingMaxCapacityBehavior" Lude..=)
              Lude.<$> predictiveScalingMaxCapacityBehavior,
            ("PredefinedLoadMetricSpecification" Lude..=)
              Lude.<$> predefinedLoadMetricSpecification,
            Lude.Just ("ServiceNamespace" Lude..= serviceNamespace),
            Lude.Just ("ResourceId" Lude..= resourceId),
            Lude.Just ("ScalableDimension" Lude..= scalableDimension),
            Lude.Just ("MinCapacity" Lude..= minCapacity),
            Lude.Just ("MaxCapacity" Lude..= maxCapacity),
            Lude.Just
              ( "TargetTrackingConfigurations"
                  Lude..= targetTrackingConfigurations
              )
          ]
      )
