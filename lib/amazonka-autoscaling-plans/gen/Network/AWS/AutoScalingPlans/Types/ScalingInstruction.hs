{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.ScalingInstruction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScalingPlans.Types.ScalingInstruction
  ( ScalingInstruction (..)
  -- * Smart constructor
  , mkScalingInstruction
  -- * Lenses
  , siServiceNamespace
  , siResourceId
  , siScalableDimension
  , siMinCapacity
  , siMaxCapacity
  , siTargetTrackingConfigurations
  , siCustomizedLoadMetricSpecification
  , siDisableDynamicScaling
  , siPredefinedLoadMetricSpecification
  , siPredictiveScalingMaxCapacityBehavior
  , siPredictiveScalingMaxCapacityBuffer
  , siPredictiveScalingMode
  , siScalingPolicyUpdateBehavior
  , siScheduledActionBufferTime
  ) where

import qualified Network.AWS.AutoScalingPlans.Types.CustomizedLoadMetricSpecification as Types
import qualified Network.AWS.AutoScalingPlans.Types.PredefinedLoadMetricSpecification as Types
import qualified Network.AWS.AutoScalingPlans.Types.PredictiveScalingMaxCapacityBehavior as Types
import qualified Network.AWS.AutoScalingPlans.Types.PredictiveScalingMode as Types
import qualified Network.AWS.AutoScalingPlans.Types.ResourceIdMaxLen1600 as Types
import qualified Network.AWS.AutoScalingPlans.Types.ScalableDimension as Types
import qualified Network.AWS.AutoScalingPlans.Types.ScalingPolicyUpdateBehavior as Types
import qualified Network.AWS.AutoScalingPlans.Types.ServiceNamespace as Types
import qualified Network.AWS.AutoScalingPlans.Types.TargetTrackingConfiguration as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

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
  { serviceNamespace :: Types.ServiceNamespace
    -- ^ The namespace of the AWS service.
  , resourceId :: Types.ResourceIdMaxLen1600
    -- ^ The ID of the resource. This string consists of the resource type and unique identifier.
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
  , scalableDimension :: Types.ScalableDimension
    -- ^ The scalable dimension associated with the resource.
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
  , minCapacity :: Core.Int
    -- ^ The minimum capacity of the resource. 
  , maxCapacity :: Core.Int
    -- ^ The maximum capacity of the resource. The exception to this upper limit is if you specify a non-default setting for __PredictiveScalingMaxCapacityBehavior__ . 
  , targetTrackingConfigurations :: [Types.TargetTrackingConfiguration]
    -- ^ The structure that defines new target tracking configurations (up to 10). Each of these structures includes a specific scaling metric and a target value for the metric, along with various parameters to use with dynamic scaling. 
--
-- With predictive scaling and dynamic scaling, the resource scales based on the target tracking configuration that provides the largest capacity for both scale in and scale out. 
-- Condition: The scaling metric must be unique across target tracking configurations.
  , customizedLoadMetricSpecification :: Core.Maybe Types.CustomizedLoadMetricSpecification
    -- ^ The customized load metric to use for predictive scaling. This parameter or a __PredefinedLoadMetricSpecification__ is required when configuring predictive scaling, and cannot be used otherwise. 
  , disableDynamicScaling :: Core.Maybe Core.Bool
    -- ^ Controls whether dynamic scaling by AWS Auto Scaling is disabled. When dynamic scaling is enabled, AWS Auto Scaling creates target tracking scaling policies based on the specified target tracking configurations. 
--
-- The default is enabled (@false@ ). 
  , predefinedLoadMetricSpecification :: Core.Maybe Types.PredefinedLoadMetricSpecification
    -- ^ The predefined load metric to use for predictive scaling. This parameter or a __CustomizedLoadMetricSpecification__ is required when configuring predictive scaling, and cannot be used otherwise. 
  , predictiveScalingMaxCapacityBehavior :: Core.Maybe Types.PredictiveScalingMaxCapacityBehavior
    -- ^ Defines the behavior that should be applied if the forecast capacity approaches or exceeds the maximum capacity specified for the resource. The default value is @SetForecastCapacityToMaxCapacity@ .
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
  , predictiveScalingMaxCapacityBuffer :: Core.Maybe Core.Int
    -- ^ The size of the capacity buffer to use when the forecast capacity is close to or exceeds the maximum capacity. The value is specified as a percentage relative to the forecast capacity. For example, if the buffer is 10, this means a 10 percent buffer, such that if the forecast capacity is 50, and the maximum capacity is 40, then the effective maximum capacity is 55.
--
-- Only valid when configuring predictive scaling. Required if the __PredictiveScalingMaxCapacityBehavior__ is set to @SetMaxCapacityAboveForecastCapacity@ , and cannot be used otherwise.
-- The range is 1-100.
  , predictiveScalingMode :: Core.Maybe Types.PredictiveScalingMode
    -- ^ The predictive scaling mode. The default value is @ForecastAndScale@ . Otherwise, AWS Auto Scaling forecasts capacity but does not create any scheduled scaling actions based on the capacity forecast. 
  , scalingPolicyUpdateBehavior :: Core.Maybe Types.ScalingPolicyUpdateBehavior
    -- ^ Controls whether a resource's externally created scaling policies are kept or replaced. 
--
-- The default value is @KeepExternalPolicies@ . If the parameter is set to @ReplaceExternalPolicies@ , any scaling policies that are external to AWS Auto Scaling are deleted and new target tracking scaling policies created. 
-- Only valid when configuring dynamic scaling. 
-- Condition: The number of existing policies to be replaced must be less than or equal to 50. If there are more than 50 policies to be replaced, AWS Auto Scaling keeps all existing policies and does not create new ones.
  , scheduledActionBufferTime :: Core.Maybe Core.Natural
    -- ^ The amount of time, in seconds, to buffer the run time of scheduled scaling actions when scaling out. For example, if the forecast says to add capacity at 10:00 AM, and the buffer time is 5 minutes, then the run time of the corresponding scheduled scaling action will be 9:55 AM. The intention is to give resources time to be provisioned. For example, it can take a few minutes to launch an EC2 instance. The actual amount of time required depends on several factors, such as the size of the instance and whether there are startup scripts to complete. 
--
-- The value must be less than the forecast interval duration of 3600 seconds (60 minutes). The default is 300 seconds. 
-- Only valid when configuring predictive scaling. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ScalingInstruction' value with any optional fields omitted.
mkScalingInstruction
    :: Types.ServiceNamespace -- ^ 'serviceNamespace'
    -> Types.ResourceIdMaxLen1600 -- ^ 'resourceId'
    -> Types.ScalableDimension -- ^ 'scalableDimension'
    -> Core.Int -- ^ 'minCapacity'
    -> Core.Int -- ^ 'maxCapacity'
    -> ScalingInstruction
mkScalingInstruction serviceNamespace resourceId scalableDimension
  minCapacity maxCapacity
  = ScalingInstruction'{serviceNamespace, resourceId,
                        scalableDimension, minCapacity, maxCapacity,
                        targetTrackingConfigurations = Core.mempty,
                        customizedLoadMetricSpecification = Core.Nothing,
                        disableDynamicScaling = Core.Nothing,
                        predefinedLoadMetricSpecification = Core.Nothing,
                        predictiveScalingMaxCapacityBehavior = Core.Nothing,
                        predictiveScalingMaxCapacityBuffer = Core.Nothing,
                        predictiveScalingMode = Core.Nothing,
                        scalingPolicyUpdateBehavior = Core.Nothing,
                        scheduledActionBufferTime = Core.Nothing}

-- | The namespace of the AWS service.
--
-- /Note:/ Consider using 'serviceNamespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siServiceNamespace :: Lens.Lens' ScalingInstruction Types.ServiceNamespace
siServiceNamespace = Lens.field @"serviceNamespace"
{-# INLINEABLE siServiceNamespace #-}
{-# DEPRECATED serviceNamespace "Use generic-lens or generic-optics with 'serviceNamespace' instead"  #-}

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
siResourceId :: Lens.Lens' ScalingInstruction Types.ResourceIdMaxLen1600
siResourceId = Lens.field @"resourceId"
{-# INLINEABLE siResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

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
siScalableDimension :: Lens.Lens' ScalingInstruction Types.ScalableDimension
siScalableDimension = Lens.field @"scalableDimension"
{-# INLINEABLE siScalableDimension #-}
{-# DEPRECATED scalableDimension "Use generic-lens or generic-optics with 'scalableDimension' instead"  #-}

-- | The minimum capacity of the resource. 
--
-- /Note:/ Consider using 'minCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siMinCapacity :: Lens.Lens' ScalingInstruction Core.Int
siMinCapacity = Lens.field @"minCapacity"
{-# INLINEABLE siMinCapacity #-}
{-# DEPRECATED minCapacity "Use generic-lens or generic-optics with 'minCapacity' instead"  #-}

-- | The maximum capacity of the resource. The exception to this upper limit is if you specify a non-default setting for __PredictiveScalingMaxCapacityBehavior__ . 
--
-- /Note:/ Consider using 'maxCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siMaxCapacity :: Lens.Lens' ScalingInstruction Core.Int
siMaxCapacity = Lens.field @"maxCapacity"
{-# INLINEABLE siMaxCapacity #-}
{-# DEPRECATED maxCapacity "Use generic-lens or generic-optics with 'maxCapacity' instead"  #-}

-- | The structure that defines new target tracking configurations (up to 10). Each of these structures includes a specific scaling metric and a target value for the metric, along with various parameters to use with dynamic scaling. 
--
-- With predictive scaling and dynamic scaling, the resource scales based on the target tracking configuration that provides the largest capacity for both scale in and scale out. 
-- Condition: The scaling metric must be unique across target tracking configurations.
--
-- /Note:/ Consider using 'targetTrackingConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siTargetTrackingConfigurations :: Lens.Lens' ScalingInstruction [Types.TargetTrackingConfiguration]
siTargetTrackingConfigurations = Lens.field @"targetTrackingConfigurations"
{-# INLINEABLE siTargetTrackingConfigurations #-}
{-# DEPRECATED targetTrackingConfigurations "Use generic-lens or generic-optics with 'targetTrackingConfigurations' instead"  #-}

-- | The customized load metric to use for predictive scaling. This parameter or a __PredefinedLoadMetricSpecification__ is required when configuring predictive scaling, and cannot be used otherwise. 
--
-- /Note:/ Consider using 'customizedLoadMetricSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siCustomizedLoadMetricSpecification :: Lens.Lens' ScalingInstruction (Core.Maybe Types.CustomizedLoadMetricSpecification)
siCustomizedLoadMetricSpecification = Lens.field @"customizedLoadMetricSpecification"
{-# INLINEABLE siCustomizedLoadMetricSpecification #-}
{-# DEPRECATED customizedLoadMetricSpecification "Use generic-lens or generic-optics with 'customizedLoadMetricSpecification' instead"  #-}

-- | Controls whether dynamic scaling by AWS Auto Scaling is disabled. When dynamic scaling is enabled, AWS Auto Scaling creates target tracking scaling policies based on the specified target tracking configurations. 
--
-- The default is enabled (@false@ ). 
--
-- /Note:/ Consider using 'disableDynamicScaling' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siDisableDynamicScaling :: Lens.Lens' ScalingInstruction (Core.Maybe Core.Bool)
siDisableDynamicScaling = Lens.field @"disableDynamicScaling"
{-# INLINEABLE siDisableDynamicScaling #-}
{-# DEPRECATED disableDynamicScaling "Use generic-lens or generic-optics with 'disableDynamicScaling' instead"  #-}

-- | The predefined load metric to use for predictive scaling. This parameter or a __CustomizedLoadMetricSpecification__ is required when configuring predictive scaling, and cannot be used otherwise. 
--
-- /Note:/ Consider using 'predefinedLoadMetricSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siPredefinedLoadMetricSpecification :: Lens.Lens' ScalingInstruction (Core.Maybe Types.PredefinedLoadMetricSpecification)
siPredefinedLoadMetricSpecification = Lens.field @"predefinedLoadMetricSpecification"
{-# INLINEABLE siPredefinedLoadMetricSpecification #-}
{-# DEPRECATED predefinedLoadMetricSpecification "Use generic-lens or generic-optics with 'predefinedLoadMetricSpecification' instead"  #-}

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
siPredictiveScalingMaxCapacityBehavior :: Lens.Lens' ScalingInstruction (Core.Maybe Types.PredictiveScalingMaxCapacityBehavior)
siPredictiveScalingMaxCapacityBehavior = Lens.field @"predictiveScalingMaxCapacityBehavior"
{-# INLINEABLE siPredictiveScalingMaxCapacityBehavior #-}
{-# DEPRECATED predictiveScalingMaxCapacityBehavior "Use generic-lens or generic-optics with 'predictiveScalingMaxCapacityBehavior' instead"  #-}

-- | The size of the capacity buffer to use when the forecast capacity is close to or exceeds the maximum capacity. The value is specified as a percentage relative to the forecast capacity. For example, if the buffer is 10, this means a 10 percent buffer, such that if the forecast capacity is 50, and the maximum capacity is 40, then the effective maximum capacity is 55.
--
-- Only valid when configuring predictive scaling. Required if the __PredictiveScalingMaxCapacityBehavior__ is set to @SetMaxCapacityAboveForecastCapacity@ , and cannot be used otherwise.
-- The range is 1-100.
--
-- /Note:/ Consider using 'predictiveScalingMaxCapacityBuffer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siPredictiveScalingMaxCapacityBuffer :: Lens.Lens' ScalingInstruction (Core.Maybe Core.Int)
siPredictiveScalingMaxCapacityBuffer = Lens.field @"predictiveScalingMaxCapacityBuffer"
{-# INLINEABLE siPredictiveScalingMaxCapacityBuffer #-}
{-# DEPRECATED predictiveScalingMaxCapacityBuffer "Use generic-lens or generic-optics with 'predictiveScalingMaxCapacityBuffer' instead"  #-}

-- | The predictive scaling mode. The default value is @ForecastAndScale@ . Otherwise, AWS Auto Scaling forecasts capacity but does not create any scheduled scaling actions based on the capacity forecast. 
--
-- /Note:/ Consider using 'predictiveScalingMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siPredictiveScalingMode :: Lens.Lens' ScalingInstruction (Core.Maybe Types.PredictiveScalingMode)
siPredictiveScalingMode = Lens.field @"predictiveScalingMode"
{-# INLINEABLE siPredictiveScalingMode #-}
{-# DEPRECATED predictiveScalingMode "Use generic-lens or generic-optics with 'predictiveScalingMode' instead"  #-}

-- | Controls whether a resource's externally created scaling policies are kept or replaced. 
--
-- The default value is @KeepExternalPolicies@ . If the parameter is set to @ReplaceExternalPolicies@ , any scaling policies that are external to AWS Auto Scaling are deleted and new target tracking scaling policies created. 
-- Only valid when configuring dynamic scaling. 
-- Condition: The number of existing policies to be replaced must be less than or equal to 50. If there are more than 50 policies to be replaced, AWS Auto Scaling keeps all existing policies and does not create new ones.
--
-- /Note:/ Consider using 'scalingPolicyUpdateBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siScalingPolicyUpdateBehavior :: Lens.Lens' ScalingInstruction (Core.Maybe Types.ScalingPolicyUpdateBehavior)
siScalingPolicyUpdateBehavior = Lens.field @"scalingPolicyUpdateBehavior"
{-# INLINEABLE siScalingPolicyUpdateBehavior #-}
{-# DEPRECATED scalingPolicyUpdateBehavior "Use generic-lens or generic-optics with 'scalingPolicyUpdateBehavior' instead"  #-}

-- | The amount of time, in seconds, to buffer the run time of scheduled scaling actions when scaling out. For example, if the forecast says to add capacity at 10:00 AM, and the buffer time is 5 minutes, then the run time of the corresponding scheduled scaling action will be 9:55 AM. The intention is to give resources time to be provisioned. For example, it can take a few minutes to launch an EC2 instance. The actual amount of time required depends on several factors, such as the size of the instance and whether there are startup scripts to complete. 
--
-- The value must be less than the forecast interval duration of 3600 seconds (60 minutes). The default is 300 seconds. 
-- Only valid when configuring predictive scaling. 
--
-- /Note:/ Consider using 'scheduledActionBufferTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siScheduledActionBufferTime :: Lens.Lens' ScalingInstruction (Core.Maybe Core.Natural)
siScheduledActionBufferTime = Lens.field @"scheduledActionBufferTime"
{-# INLINEABLE siScheduledActionBufferTime #-}
{-# DEPRECATED scheduledActionBufferTime "Use generic-lens or generic-optics with 'scheduledActionBufferTime' instead"  #-}

instance Core.FromJSON ScalingInstruction where
        toJSON ScalingInstruction{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ServiceNamespace" Core..= serviceNamespace),
                  Core.Just ("ResourceId" Core..= resourceId),
                  Core.Just ("ScalableDimension" Core..= scalableDimension),
                  Core.Just ("MinCapacity" Core..= minCapacity),
                  Core.Just ("MaxCapacity" Core..= maxCapacity),
                  Core.Just
                    ("TargetTrackingConfigurations" Core..=
                       targetTrackingConfigurations),
                  ("CustomizedLoadMetricSpecification" Core..=) Core.<$>
                    customizedLoadMetricSpecification,
                  ("DisableDynamicScaling" Core..=) Core.<$> disableDynamicScaling,
                  ("PredefinedLoadMetricSpecification" Core..=) Core.<$>
                    predefinedLoadMetricSpecification,
                  ("PredictiveScalingMaxCapacityBehavior" Core..=) Core.<$>
                    predictiveScalingMaxCapacityBehavior,
                  ("PredictiveScalingMaxCapacityBuffer" Core..=) Core.<$>
                    predictiveScalingMaxCapacityBuffer,
                  ("PredictiveScalingMode" Core..=) Core.<$> predictiveScalingMode,
                  ("ScalingPolicyUpdateBehavior" Core..=) Core.<$>
                    scalingPolicyUpdateBehavior,
                  ("ScheduledActionBufferTime" Core..=) Core.<$>
                    scheduledActionBufferTime])

instance Core.FromJSON ScalingInstruction where
        parseJSON
          = Core.withObject "ScalingInstruction" Core.$
              \ x ->
                ScalingInstruction' Core.<$>
                  (x Core..: "ServiceNamespace") Core.<*> x Core..: "ResourceId"
                    Core.<*> x Core..: "ScalableDimension"
                    Core.<*> x Core..: "MinCapacity"
                    Core.<*> x Core..: "MaxCapacity"
                    Core.<*>
                    x Core..:? "TargetTrackingConfigurations" Core..!= Core.mempty
                    Core.<*> x Core..:? "CustomizedLoadMetricSpecification"
                    Core.<*> x Core..:? "DisableDynamicScaling"
                    Core.<*> x Core..:? "PredefinedLoadMetricSpecification"
                    Core.<*> x Core..:? "PredictiveScalingMaxCapacityBehavior"
                    Core.<*> x Core..:? "PredictiveScalingMaxCapacityBuffer"
                    Core.<*> x Core..:? "PredictiveScalingMode"
                    Core.<*> x Core..:? "ScalingPolicyUpdateBehavior"
                    Core.<*> x Core..:? "ScheduledActionBufferTime"
