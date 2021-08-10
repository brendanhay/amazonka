{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.ScalingInstruction
-- Copyright   : (c) 2013-2021 Brendan Hay
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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a scaling instruction for a scalable resource in a scaling
-- plan. Each scaling instruction applies to one resource.
--
-- AWS Auto Scaling creates target tracking scaling policies based on the
-- scaling instructions. Target tracking scaling policies adjust the
-- capacity of your scalable resource as required to maintain resource
-- utilization at the target value that you specified.
--
-- AWS Auto Scaling also configures predictive scaling for your Amazon EC2
-- Auto Scaling groups using a subset of parameters, including the load
-- metric, the scaling metric, the target value for the scaling metric, the
-- predictive scaling mode (forecast and scale or forecast only), and the
-- desired behavior when the forecast capacity exceeds the maximum capacity
-- of the resource. With predictive scaling, AWS Auto Scaling generates
-- forecasts with traffic predictions for the two days ahead and schedules
-- scaling actions that proactively add and remove resource capacity to
-- match the forecast.
--
-- We recommend waiting a minimum of 24 hours after creating an Auto
-- Scaling group to configure predictive scaling. At minimum, there must be
-- 24 hours of historical data to generate a forecast. For more
-- information, see
-- <https://docs.aws.amazon.com/autoscaling/plans/userguide/gs-best-practices.html Best Practices for AWS Auto Scaling>
-- in the /AWS Auto Scaling User Guide/.
--
-- /See:/ 'newScalingInstruction' smart constructor.
data ScalingInstruction = ScalingInstruction'
  { -- | Controls whether dynamic scaling by AWS Auto Scaling is disabled. When
    -- dynamic scaling is enabled, AWS Auto Scaling creates target tracking
    -- scaling policies based on the specified target tracking configurations.
    --
    -- The default is enabled (@false@).
    disableDynamicScaling :: Prelude.Maybe Prelude.Bool,
    -- | The predefined load metric to use for predictive scaling. This parameter
    -- or a __CustomizedLoadMetricSpecification__ is required when configuring
    -- predictive scaling, and cannot be used otherwise.
    predefinedLoadMetricSpecification :: Prelude.Maybe PredefinedLoadMetricSpecification,
    -- | The customized load metric to use for predictive scaling. This parameter
    -- or a __PredefinedLoadMetricSpecification__ is required when configuring
    -- predictive scaling, and cannot be used otherwise.
    customizedLoadMetricSpecification :: Prelude.Maybe CustomizedLoadMetricSpecification,
    -- | Defines the behavior that should be applied if the forecast capacity
    -- approaches or exceeds the maximum capacity specified for the resource.
    -- The default value is @SetForecastCapacityToMaxCapacity@.
    --
    -- The following are possible values:
    --
    -- -   @SetForecastCapacityToMaxCapacity@ - AWS Auto Scaling cannot scale
    --     resource capacity higher than the maximum capacity. The maximum
    --     capacity is enforced as a hard limit.
    --
    -- -   @SetMaxCapacityToForecastCapacity@ - AWS Auto Scaling may scale
    --     resource capacity higher than the maximum capacity to equal but not
    --     exceed forecast capacity.
    --
    -- -   @SetMaxCapacityAboveForecastCapacity@ - AWS Auto Scaling may scale
    --     resource capacity higher than the maximum capacity by a specified
    --     buffer value. The intention is to give the target tracking scaling
    --     policy extra capacity if unexpected traffic occurs.
    --
    -- Only valid when configuring predictive scaling.
    predictiveScalingMaxCapacityBehavior :: Prelude.Maybe PredictiveScalingMaxCapacityBehavior,
    -- | The size of the capacity buffer to use when the forecast capacity is
    -- close to or exceeds the maximum capacity. The value is specified as a
    -- percentage relative to the forecast capacity. For example, if the buffer
    -- is 10, this means a 10 percent buffer, such that if the forecast
    -- capacity is 50, and the maximum capacity is 40, then the effective
    -- maximum capacity is 55.
    --
    -- Only valid when configuring predictive scaling. Required if the
    -- __PredictiveScalingMaxCapacityBehavior__ is set to
    -- @SetMaxCapacityAboveForecastCapacity@, and cannot be used otherwise.
    --
    -- The range is 1-100.
    predictiveScalingMaxCapacityBuffer :: Prelude.Maybe Prelude.Int,
    -- | The predictive scaling mode. The default value is @ForecastAndScale@.
    -- Otherwise, AWS Auto Scaling forecasts capacity but does not create any
    -- scheduled scaling actions based on the capacity forecast.
    predictiveScalingMode :: Prelude.Maybe PredictiveScalingMode,
    -- | Controls whether a resource\'s externally created scaling policies are
    -- kept or replaced.
    --
    -- The default value is @KeepExternalPolicies@. If the parameter is set to
    -- @ReplaceExternalPolicies@, any scaling policies that are external to AWS
    -- Auto Scaling are deleted and new target tracking scaling policies
    -- created.
    --
    -- Only valid when configuring dynamic scaling.
    --
    -- Condition: The number of existing policies to be replaced must be less
    -- than or equal to 50. If there are more than 50 policies to be replaced,
    -- AWS Auto Scaling keeps all existing policies and does not create new
    -- ones.
    scalingPolicyUpdateBehavior :: Prelude.Maybe ScalingPolicyUpdateBehavior,
    -- | The amount of time, in seconds, to buffer the run time of scheduled
    -- scaling actions when scaling out. For example, if the forecast says to
    -- add capacity at 10:00 AM, and the buffer time is 5 minutes, then the run
    -- time of the corresponding scheduled scaling action will be 9:55 AM. The
    -- intention is to give resources time to be provisioned. For example, it
    -- can take a few minutes to launch an EC2 instance. The actual amount of
    -- time required depends on several factors, such as the size of the
    -- instance and whether there are startup scripts to complete.
    --
    -- The value must be less than the forecast interval duration of 3600
    -- seconds (60 minutes). The default is 300 seconds.
    --
    -- Only valid when configuring predictive scaling.
    scheduledActionBufferTime :: Prelude.Maybe Prelude.Natural,
    -- | The namespace of the AWS service.
    serviceNamespace :: ServiceNamespace,
    -- | The ID of the resource. This string consists of the resource type and
    -- unique identifier.
    --
    -- -   Auto Scaling group - The resource type is @autoScalingGroup@ and the
    --     unique identifier is the name of the Auto Scaling group. Example:
    --     @autoScalingGroup\/my-asg@.
    --
    -- -   ECS service - The resource type is @service@ and the unique
    --     identifier is the cluster name and service name. Example:
    --     @service\/default\/sample-webapp@.
    --
    -- -   Spot Fleet request - The resource type is @spot-fleet-request@ and
    --     the unique identifier is the Spot Fleet request ID. Example:
    --     @spot-fleet-request\/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@.
    --
    -- -   DynamoDB table - The resource type is @table@ and the unique
    --     identifier is the resource ID. Example: @table\/my-table@.
    --
    -- -   DynamoDB global secondary index - The resource type is @index@ and
    --     the unique identifier is the resource ID. Example:
    --     @table\/my-table\/index\/my-table-index@.
    --
    -- -   Aurora DB cluster - The resource type is @cluster@ and the unique
    --     identifier is the cluster name. Example: @cluster:my-db-cluster@.
    resourceId :: Prelude.Text,
    -- | The scalable dimension associated with the resource.
    --
    -- -   @autoscaling:autoScalingGroup:DesiredCapacity@ - The desired
    --     capacity of an Auto Scaling group.
    --
    -- -   @ecs:service:DesiredCount@ - The desired task count of an ECS
    --     service.
    --
    -- -   @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a
    --     Spot Fleet request.
    --
    -- -   @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity
    --     for a DynamoDB table.
    --
    -- -   @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity
    --     for a DynamoDB table.
    --
    -- -   @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity
    --     for a DynamoDB global secondary index.
    --
    -- -   @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity
    --     for a DynamoDB global secondary index.
    --
    -- -   @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an
    --     Aurora DB cluster. Available for Aurora MySQL-compatible edition and
    --     Aurora PostgreSQL-compatible edition.
    scalableDimension :: ScalableDimension,
    -- | The minimum capacity of the resource.
    minCapacity :: Prelude.Int,
    -- | The maximum capacity of the resource. The exception to this upper limit
    -- is if you specify a non-default setting for
    -- __PredictiveScalingMaxCapacityBehavior__.
    maxCapacity :: Prelude.Int,
    -- | The target tracking configurations (up to 10). Each of these structures
    -- must specify a unique scaling metric and a target value for the metric.
    targetTrackingConfigurations :: [TargetTrackingConfiguration]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScalingInstruction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'disableDynamicScaling', 'scalingInstruction_disableDynamicScaling' - Controls whether dynamic scaling by AWS Auto Scaling is disabled. When
-- dynamic scaling is enabled, AWS Auto Scaling creates target tracking
-- scaling policies based on the specified target tracking configurations.
--
-- The default is enabled (@false@).
--
-- 'predefinedLoadMetricSpecification', 'scalingInstruction_predefinedLoadMetricSpecification' - The predefined load metric to use for predictive scaling. This parameter
-- or a __CustomizedLoadMetricSpecification__ is required when configuring
-- predictive scaling, and cannot be used otherwise.
--
-- 'customizedLoadMetricSpecification', 'scalingInstruction_customizedLoadMetricSpecification' - The customized load metric to use for predictive scaling. This parameter
-- or a __PredefinedLoadMetricSpecification__ is required when configuring
-- predictive scaling, and cannot be used otherwise.
--
-- 'predictiveScalingMaxCapacityBehavior', 'scalingInstruction_predictiveScalingMaxCapacityBehavior' - Defines the behavior that should be applied if the forecast capacity
-- approaches or exceeds the maximum capacity specified for the resource.
-- The default value is @SetForecastCapacityToMaxCapacity@.
--
-- The following are possible values:
--
-- -   @SetForecastCapacityToMaxCapacity@ - AWS Auto Scaling cannot scale
--     resource capacity higher than the maximum capacity. The maximum
--     capacity is enforced as a hard limit.
--
-- -   @SetMaxCapacityToForecastCapacity@ - AWS Auto Scaling may scale
--     resource capacity higher than the maximum capacity to equal but not
--     exceed forecast capacity.
--
-- -   @SetMaxCapacityAboveForecastCapacity@ - AWS Auto Scaling may scale
--     resource capacity higher than the maximum capacity by a specified
--     buffer value. The intention is to give the target tracking scaling
--     policy extra capacity if unexpected traffic occurs.
--
-- Only valid when configuring predictive scaling.
--
-- 'predictiveScalingMaxCapacityBuffer', 'scalingInstruction_predictiveScalingMaxCapacityBuffer' - The size of the capacity buffer to use when the forecast capacity is
-- close to or exceeds the maximum capacity. The value is specified as a
-- percentage relative to the forecast capacity. For example, if the buffer
-- is 10, this means a 10 percent buffer, such that if the forecast
-- capacity is 50, and the maximum capacity is 40, then the effective
-- maximum capacity is 55.
--
-- Only valid when configuring predictive scaling. Required if the
-- __PredictiveScalingMaxCapacityBehavior__ is set to
-- @SetMaxCapacityAboveForecastCapacity@, and cannot be used otherwise.
--
-- The range is 1-100.
--
-- 'predictiveScalingMode', 'scalingInstruction_predictiveScalingMode' - The predictive scaling mode. The default value is @ForecastAndScale@.
-- Otherwise, AWS Auto Scaling forecasts capacity but does not create any
-- scheduled scaling actions based on the capacity forecast.
--
-- 'scalingPolicyUpdateBehavior', 'scalingInstruction_scalingPolicyUpdateBehavior' - Controls whether a resource\'s externally created scaling policies are
-- kept or replaced.
--
-- The default value is @KeepExternalPolicies@. If the parameter is set to
-- @ReplaceExternalPolicies@, any scaling policies that are external to AWS
-- Auto Scaling are deleted and new target tracking scaling policies
-- created.
--
-- Only valid when configuring dynamic scaling.
--
-- Condition: The number of existing policies to be replaced must be less
-- than or equal to 50. If there are more than 50 policies to be replaced,
-- AWS Auto Scaling keeps all existing policies and does not create new
-- ones.
--
-- 'scheduledActionBufferTime', 'scalingInstruction_scheduledActionBufferTime' - The amount of time, in seconds, to buffer the run time of scheduled
-- scaling actions when scaling out. For example, if the forecast says to
-- add capacity at 10:00 AM, and the buffer time is 5 minutes, then the run
-- time of the corresponding scheduled scaling action will be 9:55 AM. The
-- intention is to give resources time to be provisioned. For example, it
-- can take a few minutes to launch an EC2 instance. The actual amount of
-- time required depends on several factors, such as the size of the
-- instance and whether there are startup scripts to complete.
--
-- The value must be less than the forecast interval duration of 3600
-- seconds (60 minutes). The default is 300 seconds.
--
-- Only valid when configuring predictive scaling.
--
-- 'serviceNamespace', 'scalingInstruction_serviceNamespace' - The namespace of the AWS service.
--
-- 'resourceId', 'scalingInstruction_resourceId' - The ID of the resource. This string consists of the resource type and
-- unique identifier.
--
-- -   Auto Scaling group - The resource type is @autoScalingGroup@ and the
--     unique identifier is the name of the Auto Scaling group. Example:
--     @autoScalingGroup\/my-asg@.
--
-- -   ECS service - The resource type is @service@ and the unique
--     identifier is the cluster name and service name. Example:
--     @service\/default\/sample-webapp@.
--
-- -   Spot Fleet request - The resource type is @spot-fleet-request@ and
--     the unique identifier is the Spot Fleet request ID. Example:
--     @spot-fleet-request\/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@.
--
-- -   DynamoDB table - The resource type is @table@ and the unique
--     identifier is the resource ID. Example: @table\/my-table@.
--
-- -   DynamoDB global secondary index - The resource type is @index@ and
--     the unique identifier is the resource ID. Example:
--     @table\/my-table\/index\/my-table-index@.
--
-- -   Aurora DB cluster - The resource type is @cluster@ and the unique
--     identifier is the cluster name. Example: @cluster:my-db-cluster@.
--
-- 'scalableDimension', 'scalingInstruction_scalableDimension' - The scalable dimension associated with the resource.
--
-- -   @autoscaling:autoScalingGroup:DesiredCapacity@ - The desired
--     capacity of an Auto Scaling group.
--
-- -   @ecs:service:DesiredCount@ - The desired task count of an ECS
--     service.
--
-- -   @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a
--     Spot Fleet request.
--
-- -   @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity
--     for a DynamoDB table.
--
-- -   @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity
--     for a DynamoDB table.
--
-- -   @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity
--     for a DynamoDB global secondary index.
--
-- -   @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity
--     for a DynamoDB global secondary index.
--
-- -   @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an
--     Aurora DB cluster. Available for Aurora MySQL-compatible edition and
--     Aurora PostgreSQL-compatible edition.
--
-- 'minCapacity', 'scalingInstruction_minCapacity' - The minimum capacity of the resource.
--
-- 'maxCapacity', 'scalingInstruction_maxCapacity' - The maximum capacity of the resource. The exception to this upper limit
-- is if you specify a non-default setting for
-- __PredictiveScalingMaxCapacityBehavior__.
--
-- 'targetTrackingConfigurations', 'scalingInstruction_targetTrackingConfigurations' - The target tracking configurations (up to 10). Each of these structures
-- must specify a unique scaling metric and a target value for the metric.
newScalingInstruction ::
  -- | 'serviceNamespace'
  ServiceNamespace ->
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'scalableDimension'
  ScalableDimension ->
  -- | 'minCapacity'
  Prelude.Int ->
  -- | 'maxCapacity'
  Prelude.Int ->
  ScalingInstruction
newScalingInstruction
  pServiceNamespace_
  pResourceId_
  pScalableDimension_
  pMinCapacity_
  pMaxCapacity_ =
    ScalingInstruction'
      { disableDynamicScaling =
          Prelude.Nothing,
        predefinedLoadMetricSpecification = Prelude.Nothing,
        customizedLoadMetricSpecification = Prelude.Nothing,
        predictiveScalingMaxCapacityBehavior =
          Prelude.Nothing,
        predictiveScalingMaxCapacityBuffer = Prelude.Nothing,
        predictiveScalingMode = Prelude.Nothing,
        scalingPolicyUpdateBehavior = Prelude.Nothing,
        scheduledActionBufferTime = Prelude.Nothing,
        serviceNamespace = pServiceNamespace_,
        resourceId = pResourceId_,
        scalableDimension = pScalableDimension_,
        minCapacity = pMinCapacity_,
        maxCapacity = pMaxCapacity_,
        targetTrackingConfigurations = Prelude.mempty
      }

-- | Controls whether dynamic scaling by AWS Auto Scaling is disabled. When
-- dynamic scaling is enabled, AWS Auto Scaling creates target tracking
-- scaling policies based on the specified target tracking configurations.
--
-- The default is enabled (@false@).
scalingInstruction_disableDynamicScaling :: Lens.Lens' ScalingInstruction (Prelude.Maybe Prelude.Bool)
scalingInstruction_disableDynamicScaling = Lens.lens (\ScalingInstruction' {disableDynamicScaling} -> disableDynamicScaling) (\s@ScalingInstruction' {} a -> s {disableDynamicScaling = a} :: ScalingInstruction)

-- | The predefined load metric to use for predictive scaling. This parameter
-- or a __CustomizedLoadMetricSpecification__ is required when configuring
-- predictive scaling, and cannot be used otherwise.
scalingInstruction_predefinedLoadMetricSpecification :: Lens.Lens' ScalingInstruction (Prelude.Maybe PredefinedLoadMetricSpecification)
scalingInstruction_predefinedLoadMetricSpecification = Lens.lens (\ScalingInstruction' {predefinedLoadMetricSpecification} -> predefinedLoadMetricSpecification) (\s@ScalingInstruction' {} a -> s {predefinedLoadMetricSpecification = a} :: ScalingInstruction)

-- | The customized load metric to use for predictive scaling. This parameter
-- or a __PredefinedLoadMetricSpecification__ is required when configuring
-- predictive scaling, and cannot be used otherwise.
scalingInstruction_customizedLoadMetricSpecification :: Lens.Lens' ScalingInstruction (Prelude.Maybe CustomizedLoadMetricSpecification)
scalingInstruction_customizedLoadMetricSpecification = Lens.lens (\ScalingInstruction' {customizedLoadMetricSpecification} -> customizedLoadMetricSpecification) (\s@ScalingInstruction' {} a -> s {customizedLoadMetricSpecification = a} :: ScalingInstruction)

-- | Defines the behavior that should be applied if the forecast capacity
-- approaches or exceeds the maximum capacity specified for the resource.
-- The default value is @SetForecastCapacityToMaxCapacity@.
--
-- The following are possible values:
--
-- -   @SetForecastCapacityToMaxCapacity@ - AWS Auto Scaling cannot scale
--     resource capacity higher than the maximum capacity. The maximum
--     capacity is enforced as a hard limit.
--
-- -   @SetMaxCapacityToForecastCapacity@ - AWS Auto Scaling may scale
--     resource capacity higher than the maximum capacity to equal but not
--     exceed forecast capacity.
--
-- -   @SetMaxCapacityAboveForecastCapacity@ - AWS Auto Scaling may scale
--     resource capacity higher than the maximum capacity by a specified
--     buffer value. The intention is to give the target tracking scaling
--     policy extra capacity if unexpected traffic occurs.
--
-- Only valid when configuring predictive scaling.
scalingInstruction_predictiveScalingMaxCapacityBehavior :: Lens.Lens' ScalingInstruction (Prelude.Maybe PredictiveScalingMaxCapacityBehavior)
scalingInstruction_predictiveScalingMaxCapacityBehavior = Lens.lens (\ScalingInstruction' {predictiveScalingMaxCapacityBehavior} -> predictiveScalingMaxCapacityBehavior) (\s@ScalingInstruction' {} a -> s {predictiveScalingMaxCapacityBehavior = a} :: ScalingInstruction)

-- | The size of the capacity buffer to use when the forecast capacity is
-- close to or exceeds the maximum capacity. The value is specified as a
-- percentage relative to the forecast capacity. For example, if the buffer
-- is 10, this means a 10 percent buffer, such that if the forecast
-- capacity is 50, and the maximum capacity is 40, then the effective
-- maximum capacity is 55.
--
-- Only valid when configuring predictive scaling. Required if the
-- __PredictiveScalingMaxCapacityBehavior__ is set to
-- @SetMaxCapacityAboveForecastCapacity@, and cannot be used otherwise.
--
-- The range is 1-100.
scalingInstruction_predictiveScalingMaxCapacityBuffer :: Lens.Lens' ScalingInstruction (Prelude.Maybe Prelude.Int)
scalingInstruction_predictiveScalingMaxCapacityBuffer = Lens.lens (\ScalingInstruction' {predictiveScalingMaxCapacityBuffer} -> predictiveScalingMaxCapacityBuffer) (\s@ScalingInstruction' {} a -> s {predictiveScalingMaxCapacityBuffer = a} :: ScalingInstruction)

-- | The predictive scaling mode. The default value is @ForecastAndScale@.
-- Otherwise, AWS Auto Scaling forecasts capacity but does not create any
-- scheduled scaling actions based on the capacity forecast.
scalingInstruction_predictiveScalingMode :: Lens.Lens' ScalingInstruction (Prelude.Maybe PredictiveScalingMode)
scalingInstruction_predictiveScalingMode = Lens.lens (\ScalingInstruction' {predictiveScalingMode} -> predictiveScalingMode) (\s@ScalingInstruction' {} a -> s {predictiveScalingMode = a} :: ScalingInstruction)

-- | Controls whether a resource\'s externally created scaling policies are
-- kept or replaced.
--
-- The default value is @KeepExternalPolicies@. If the parameter is set to
-- @ReplaceExternalPolicies@, any scaling policies that are external to AWS
-- Auto Scaling are deleted and new target tracking scaling policies
-- created.
--
-- Only valid when configuring dynamic scaling.
--
-- Condition: The number of existing policies to be replaced must be less
-- than or equal to 50. If there are more than 50 policies to be replaced,
-- AWS Auto Scaling keeps all existing policies and does not create new
-- ones.
scalingInstruction_scalingPolicyUpdateBehavior :: Lens.Lens' ScalingInstruction (Prelude.Maybe ScalingPolicyUpdateBehavior)
scalingInstruction_scalingPolicyUpdateBehavior = Lens.lens (\ScalingInstruction' {scalingPolicyUpdateBehavior} -> scalingPolicyUpdateBehavior) (\s@ScalingInstruction' {} a -> s {scalingPolicyUpdateBehavior = a} :: ScalingInstruction)

-- | The amount of time, in seconds, to buffer the run time of scheduled
-- scaling actions when scaling out. For example, if the forecast says to
-- add capacity at 10:00 AM, and the buffer time is 5 minutes, then the run
-- time of the corresponding scheduled scaling action will be 9:55 AM. The
-- intention is to give resources time to be provisioned. For example, it
-- can take a few minutes to launch an EC2 instance. The actual amount of
-- time required depends on several factors, such as the size of the
-- instance and whether there are startup scripts to complete.
--
-- The value must be less than the forecast interval duration of 3600
-- seconds (60 minutes). The default is 300 seconds.
--
-- Only valid when configuring predictive scaling.
scalingInstruction_scheduledActionBufferTime :: Lens.Lens' ScalingInstruction (Prelude.Maybe Prelude.Natural)
scalingInstruction_scheduledActionBufferTime = Lens.lens (\ScalingInstruction' {scheduledActionBufferTime} -> scheduledActionBufferTime) (\s@ScalingInstruction' {} a -> s {scheduledActionBufferTime = a} :: ScalingInstruction)

-- | The namespace of the AWS service.
scalingInstruction_serviceNamespace :: Lens.Lens' ScalingInstruction ServiceNamespace
scalingInstruction_serviceNamespace = Lens.lens (\ScalingInstruction' {serviceNamespace} -> serviceNamespace) (\s@ScalingInstruction' {} a -> s {serviceNamespace = a} :: ScalingInstruction)

-- | The ID of the resource. This string consists of the resource type and
-- unique identifier.
--
-- -   Auto Scaling group - The resource type is @autoScalingGroup@ and the
--     unique identifier is the name of the Auto Scaling group. Example:
--     @autoScalingGroup\/my-asg@.
--
-- -   ECS service - The resource type is @service@ and the unique
--     identifier is the cluster name and service name. Example:
--     @service\/default\/sample-webapp@.
--
-- -   Spot Fleet request - The resource type is @spot-fleet-request@ and
--     the unique identifier is the Spot Fleet request ID. Example:
--     @spot-fleet-request\/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@.
--
-- -   DynamoDB table - The resource type is @table@ and the unique
--     identifier is the resource ID. Example: @table\/my-table@.
--
-- -   DynamoDB global secondary index - The resource type is @index@ and
--     the unique identifier is the resource ID. Example:
--     @table\/my-table\/index\/my-table-index@.
--
-- -   Aurora DB cluster - The resource type is @cluster@ and the unique
--     identifier is the cluster name. Example: @cluster:my-db-cluster@.
scalingInstruction_resourceId :: Lens.Lens' ScalingInstruction Prelude.Text
scalingInstruction_resourceId = Lens.lens (\ScalingInstruction' {resourceId} -> resourceId) (\s@ScalingInstruction' {} a -> s {resourceId = a} :: ScalingInstruction)

-- | The scalable dimension associated with the resource.
--
-- -   @autoscaling:autoScalingGroup:DesiredCapacity@ - The desired
--     capacity of an Auto Scaling group.
--
-- -   @ecs:service:DesiredCount@ - The desired task count of an ECS
--     service.
--
-- -   @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a
--     Spot Fleet request.
--
-- -   @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity
--     for a DynamoDB table.
--
-- -   @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity
--     for a DynamoDB table.
--
-- -   @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity
--     for a DynamoDB global secondary index.
--
-- -   @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity
--     for a DynamoDB global secondary index.
--
-- -   @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an
--     Aurora DB cluster. Available for Aurora MySQL-compatible edition and
--     Aurora PostgreSQL-compatible edition.
scalingInstruction_scalableDimension :: Lens.Lens' ScalingInstruction ScalableDimension
scalingInstruction_scalableDimension = Lens.lens (\ScalingInstruction' {scalableDimension} -> scalableDimension) (\s@ScalingInstruction' {} a -> s {scalableDimension = a} :: ScalingInstruction)

-- | The minimum capacity of the resource.
scalingInstruction_minCapacity :: Lens.Lens' ScalingInstruction Prelude.Int
scalingInstruction_minCapacity = Lens.lens (\ScalingInstruction' {minCapacity} -> minCapacity) (\s@ScalingInstruction' {} a -> s {minCapacity = a} :: ScalingInstruction)

-- | The maximum capacity of the resource. The exception to this upper limit
-- is if you specify a non-default setting for
-- __PredictiveScalingMaxCapacityBehavior__.
scalingInstruction_maxCapacity :: Lens.Lens' ScalingInstruction Prelude.Int
scalingInstruction_maxCapacity = Lens.lens (\ScalingInstruction' {maxCapacity} -> maxCapacity) (\s@ScalingInstruction' {} a -> s {maxCapacity = a} :: ScalingInstruction)

-- | The target tracking configurations (up to 10). Each of these structures
-- must specify a unique scaling metric and a target value for the metric.
scalingInstruction_targetTrackingConfigurations :: Lens.Lens' ScalingInstruction [TargetTrackingConfiguration]
scalingInstruction_targetTrackingConfigurations = Lens.lens (\ScalingInstruction' {targetTrackingConfigurations} -> targetTrackingConfigurations) (\s@ScalingInstruction' {} a -> s {targetTrackingConfigurations = a} :: ScalingInstruction) Prelude.. Lens._Coerce

instance Core.FromJSON ScalingInstruction where
  parseJSON =
    Core.withObject
      "ScalingInstruction"
      ( \x ->
          ScalingInstruction'
            Prelude.<$> (x Core..:? "DisableDynamicScaling")
            Prelude.<*> (x Core..:? "PredefinedLoadMetricSpecification")
            Prelude.<*> (x Core..:? "CustomizedLoadMetricSpecification")
            Prelude.<*> (x Core..:? "PredictiveScalingMaxCapacityBehavior")
            Prelude.<*> (x Core..:? "PredictiveScalingMaxCapacityBuffer")
            Prelude.<*> (x Core..:? "PredictiveScalingMode")
            Prelude.<*> (x Core..:? "ScalingPolicyUpdateBehavior")
            Prelude.<*> (x Core..:? "ScheduledActionBufferTime")
            Prelude.<*> (x Core..: "ServiceNamespace")
            Prelude.<*> (x Core..: "ResourceId")
            Prelude.<*> (x Core..: "ScalableDimension")
            Prelude.<*> (x Core..: "MinCapacity")
            Prelude.<*> (x Core..: "MaxCapacity")
            Prelude.<*> ( x Core..:? "TargetTrackingConfigurations"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ScalingInstruction

instance Prelude.NFData ScalingInstruction

instance Core.ToJSON ScalingInstruction where
  toJSON ScalingInstruction' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DisableDynamicScaling" Core..=)
              Prelude.<$> disableDynamicScaling,
            ("PredefinedLoadMetricSpecification" Core..=)
              Prelude.<$> predefinedLoadMetricSpecification,
            ("CustomizedLoadMetricSpecification" Core..=)
              Prelude.<$> customizedLoadMetricSpecification,
            ("PredictiveScalingMaxCapacityBehavior" Core..=)
              Prelude.<$> predictiveScalingMaxCapacityBehavior,
            ("PredictiveScalingMaxCapacityBuffer" Core..=)
              Prelude.<$> predictiveScalingMaxCapacityBuffer,
            ("PredictiveScalingMode" Core..=)
              Prelude.<$> predictiveScalingMode,
            ("ScalingPolicyUpdateBehavior" Core..=)
              Prelude.<$> scalingPolicyUpdateBehavior,
            ("ScheduledActionBufferTime" Core..=)
              Prelude.<$> scheduledActionBufferTime,
            Prelude.Just
              ("ServiceNamespace" Core..= serviceNamespace),
            Prelude.Just ("ResourceId" Core..= resourceId),
            Prelude.Just
              ("ScalableDimension" Core..= scalableDimension),
            Prelude.Just ("MinCapacity" Core..= minCapacity),
            Prelude.Just ("MaxCapacity" Core..= maxCapacity),
            Prelude.Just
              ( "TargetTrackingConfigurations"
                  Core..= targetTrackingConfigurations
              )
          ]
      )
