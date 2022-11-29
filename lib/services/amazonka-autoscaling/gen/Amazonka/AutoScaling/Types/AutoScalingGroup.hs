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
-- Module      : Amazonka.AutoScaling.Types.AutoScalingGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.AutoScalingGroup where

import Amazonka.AutoScaling.Types.EnabledMetric
import Amazonka.AutoScaling.Types.Instance
import Amazonka.AutoScaling.Types.LaunchTemplateSpecification
import Amazonka.AutoScaling.Types.MixedInstancesPolicy
import Amazonka.AutoScaling.Types.SuspendedProcess
import Amazonka.AutoScaling.Types.TagDescription
import Amazonka.AutoScaling.Types.WarmPoolConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes an Auto Scaling group.
--
-- /See:/ 'newAutoScalingGroup' smart constructor.
data AutoScalingGroup = AutoScalingGroup'
  { -- | The tags for the group.
    tags :: Prelude.Maybe [TagDescription],
    -- | The EC2 instances associated with the group.
    instances :: Prelude.Maybe [Instance],
    -- | The warm pool for the group.
    warmPoolConfiguration :: Prelude.Maybe WarmPoolConfiguration,
    -- | The Amazon Resource Name (ARN) of the Auto Scaling group.
    autoScalingGroupARN :: Prelude.Maybe Prelude.Text,
    -- | One or more load balancers associated with the group.
    loadBalancerNames :: Prelude.Maybe [Prelude.Text],
    -- | The predicted capacity of the group when it has a predictive scaling
    -- policy.
    predictedCapacity :: Prelude.Maybe Prelude.Int,
    -- | The duration of the health check grace period, in seconds.
    healthCheckGracePeriod :: Prelude.Maybe Prelude.Int,
    -- | The launch template for the group.
    launchTemplate :: Prelude.Maybe LaunchTemplateSpecification,
    -- | The current size of the warm pool.
    warmPoolSize :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the service-linked role that the Auto
    -- Scaling group uses to call other Amazon Web Services on your behalf.
    serviceLinkedRoleARN :: Prelude.Maybe Prelude.Text,
    -- | Reserved.
    context :: Prelude.Maybe Prelude.Text,
    -- | The current state of the group when the DeleteAutoScalingGroup operation
    -- is in progress.
    status :: Prelude.Maybe Prelude.Text,
    -- | The maximum amount of time, in seconds, that an instance can be in
    -- service.
    --
    -- Valid Range: Minimum value of 0.
    maxInstanceLifetime :: Prelude.Maybe Prelude.Int,
    -- | One or more subnet IDs, if applicable, separated by commas.
    vPCZoneIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The name of the associated launch configuration.
    launchConfigurationName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Names (ARN) of the target groups for your load
    -- balancer.
    targetGroupARNs :: Prelude.Maybe [Prelude.Text],
    -- | The duration of the default instance warmup, in seconds.
    defaultInstanceWarmup :: Prelude.Maybe Prelude.Int,
    -- | The mixed instances policy for the group.
    mixedInstancesPolicy :: Prelude.Maybe MixedInstancesPolicy,
    -- | The name of the placement group into which to launch your instances, if
    -- any.
    placementGroup :: Prelude.Maybe Prelude.Text,
    -- | The unit of measurement for the value specified for desired capacity.
    -- Amazon EC2 Auto Scaling supports @DesiredCapacityType@ for
    -- attribute-based instance type selection only.
    desiredCapacityType :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether newly launched instances are protected from
    -- termination by Amazon EC2 Auto Scaling when scaling in.
    newInstancesProtectedFromScaleIn' :: Prelude.Maybe Prelude.Bool,
    -- | The termination policies for the group.
    terminationPolicies :: Prelude.Maybe [Prelude.Text],
    -- | The suspended processes associated with the group.
    suspendedProcesses :: Prelude.Maybe [SuspendedProcess],
    -- | The metrics enabled for the group.
    enabledMetrics :: Prelude.Maybe [EnabledMetric],
    -- | Indicates whether Capacity Rebalancing is enabled.
    capacityRebalance :: Prelude.Maybe Prelude.Bool,
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Text,
    -- | The minimum size of the group.
    minSize :: Prelude.Int,
    -- | The maximum size of the group.
    maxSize :: Prelude.Int,
    -- | The desired size of the group.
    desiredCapacity :: Prelude.Int,
    -- | The duration of the default cooldown period, in seconds.
    defaultCooldown :: Prelude.Int,
    -- | One or more Availability Zones for the group.
    availabilityZones :: [Prelude.Text],
    -- | The service to use for the health checks. The valid values are @EC2@ and
    -- @ELB@. If you configure an Auto Scaling group to use @ELB@ health
    -- checks, it considers the instance unhealthy if it fails either the EC2
    -- status checks or the load balancer health checks.
    healthCheckType :: Prelude.Text,
    -- | The date and time the group was created.
    createdTime :: Core.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoScalingGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'autoScalingGroup_tags' - The tags for the group.
--
-- 'instances', 'autoScalingGroup_instances' - The EC2 instances associated with the group.
--
-- 'warmPoolConfiguration', 'autoScalingGroup_warmPoolConfiguration' - The warm pool for the group.
--
-- 'autoScalingGroupARN', 'autoScalingGroup_autoScalingGroupARN' - The Amazon Resource Name (ARN) of the Auto Scaling group.
--
-- 'loadBalancerNames', 'autoScalingGroup_loadBalancerNames' - One or more load balancers associated with the group.
--
-- 'predictedCapacity', 'autoScalingGroup_predictedCapacity' - The predicted capacity of the group when it has a predictive scaling
-- policy.
--
-- 'healthCheckGracePeriod', 'autoScalingGroup_healthCheckGracePeriod' - The duration of the health check grace period, in seconds.
--
-- 'launchTemplate', 'autoScalingGroup_launchTemplate' - The launch template for the group.
--
-- 'warmPoolSize', 'autoScalingGroup_warmPoolSize' - The current size of the warm pool.
--
-- 'serviceLinkedRoleARN', 'autoScalingGroup_serviceLinkedRoleARN' - The Amazon Resource Name (ARN) of the service-linked role that the Auto
-- Scaling group uses to call other Amazon Web Services on your behalf.
--
-- 'context', 'autoScalingGroup_context' - Reserved.
--
-- 'status', 'autoScalingGroup_status' - The current state of the group when the DeleteAutoScalingGroup operation
-- is in progress.
--
-- 'maxInstanceLifetime', 'autoScalingGroup_maxInstanceLifetime' - The maximum amount of time, in seconds, that an instance can be in
-- service.
--
-- Valid Range: Minimum value of 0.
--
-- 'vPCZoneIdentifier', 'autoScalingGroup_vPCZoneIdentifier' - One or more subnet IDs, if applicable, separated by commas.
--
-- 'launchConfigurationName', 'autoScalingGroup_launchConfigurationName' - The name of the associated launch configuration.
--
-- 'targetGroupARNs', 'autoScalingGroup_targetGroupARNs' - The Amazon Resource Names (ARN) of the target groups for your load
-- balancer.
--
-- 'defaultInstanceWarmup', 'autoScalingGroup_defaultInstanceWarmup' - The duration of the default instance warmup, in seconds.
--
-- 'mixedInstancesPolicy', 'autoScalingGroup_mixedInstancesPolicy' - The mixed instances policy for the group.
--
-- 'placementGroup', 'autoScalingGroup_placementGroup' - The name of the placement group into which to launch your instances, if
-- any.
--
-- 'desiredCapacityType', 'autoScalingGroup_desiredCapacityType' - The unit of measurement for the value specified for desired capacity.
-- Amazon EC2 Auto Scaling supports @DesiredCapacityType@ for
-- attribute-based instance type selection only.
--
-- 'newInstancesProtectedFromScaleIn'', 'autoScalingGroup_newInstancesProtectedFromScaleIn' - Indicates whether newly launched instances are protected from
-- termination by Amazon EC2 Auto Scaling when scaling in.
--
-- 'terminationPolicies', 'autoScalingGroup_terminationPolicies' - The termination policies for the group.
--
-- 'suspendedProcesses', 'autoScalingGroup_suspendedProcesses' - The suspended processes associated with the group.
--
-- 'enabledMetrics', 'autoScalingGroup_enabledMetrics' - The metrics enabled for the group.
--
-- 'capacityRebalance', 'autoScalingGroup_capacityRebalance' - Indicates whether Capacity Rebalancing is enabled.
--
-- 'autoScalingGroupName', 'autoScalingGroup_autoScalingGroupName' - The name of the Auto Scaling group.
--
-- 'minSize', 'autoScalingGroup_minSize' - The minimum size of the group.
--
-- 'maxSize', 'autoScalingGroup_maxSize' - The maximum size of the group.
--
-- 'desiredCapacity', 'autoScalingGroup_desiredCapacity' - The desired size of the group.
--
-- 'defaultCooldown', 'autoScalingGroup_defaultCooldown' - The duration of the default cooldown period, in seconds.
--
-- 'availabilityZones', 'autoScalingGroup_availabilityZones' - One or more Availability Zones for the group.
--
-- 'healthCheckType', 'autoScalingGroup_healthCheckType' - The service to use for the health checks. The valid values are @EC2@ and
-- @ELB@. If you configure an Auto Scaling group to use @ELB@ health
-- checks, it considers the instance unhealthy if it fails either the EC2
-- status checks or the load balancer health checks.
--
-- 'createdTime', 'autoScalingGroup_createdTime' - The date and time the group was created.
newAutoScalingGroup ::
  -- | 'autoScalingGroupName'
  Prelude.Text ->
  -- | 'minSize'
  Prelude.Int ->
  -- | 'maxSize'
  Prelude.Int ->
  -- | 'desiredCapacity'
  Prelude.Int ->
  -- | 'defaultCooldown'
  Prelude.Int ->
  -- | 'healthCheckType'
  Prelude.Text ->
  -- | 'createdTime'
  Prelude.UTCTime ->
  AutoScalingGroup
newAutoScalingGroup
  pAutoScalingGroupName_
  pMinSize_
  pMaxSize_
  pDesiredCapacity_
  pDefaultCooldown_
  pHealthCheckType_
  pCreatedTime_ =
    AutoScalingGroup'
      { tags = Prelude.Nothing,
        instances = Prelude.Nothing,
        warmPoolConfiguration = Prelude.Nothing,
        autoScalingGroupARN = Prelude.Nothing,
        loadBalancerNames = Prelude.Nothing,
        predictedCapacity = Prelude.Nothing,
        healthCheckGracePeriod = Prelude.Nothing,
        launchTemplate = Prelude.Nothing,
        warmPoolSize = Prelude.Nothing,
        serviceLinkedRoleARN = Prelude.Nothing,
        context = Prelude.Nothing,
        status = Prelude.Nothing,
        maxInstanceLifetime = Prelude.Nothing,
        vPCZoneIdentifier = Prelude.Nothing,
        launchConfigurationName = Prelude.Nothing,
        targetGroupARNs = Prelude.Nothing,
        defaultInstanceWarmup = Prelude.Nothing,
        mixedInstancesPolicy = Prelude.Nothing,
        placementGroup = Prelude.Nothing,
        desiredCapacityType = Prelude.Nothing,
        newInstancesProtectedFromScaleIn' = Prelude.Nothing,
        terminationPolicies = Prelude.Nothing,
        suspendedProcesses = Prelude.Nothing,
        enabledMetrics = Prelude.Nothing,
        capacityRebalance = Prelude.Nothing,
        autoScalingGroupName = pAutoScalingGroupName_,
        minSize = pMinSize_,
        maxSize = pMaxSize_,
        desiredCapacity = pDesiredCapacity_,
        defaultCooldown = pDefaultCooldown_,
        availabilityZones = Prelude.mempty,
        healthCheckType = pHealthCheckType_,
        createdTime = Core._Time Lens.# pCreatedTime_
      }

-- | The tags for the group.
autoScalingGroup_tags :: Lens.Lens' AutoScalingGroup (Prelude.Maybe [TagDescription])
autoScalingGroup_tags = Lens.lens (\AutoScalingGroup' {tags} -> tags) (\s@AutoScalingGroup' {} a -> s {tags = a} :: AutoScalingGroup) Prelude.. Lens.mapping Lens.coerced

-- | The EC2 instances associated with the group.
autoScalingGroup_instances :: Lens.Lens' AutoScalingGroup (Prelude.Maybe [Instance])
autoScalingGroup_instances = Lens.lens (\AutoScalingGroup' {instances} -> instances) (\s@AutoScalingGroup' {} a -> s {instances = a} :: AutoScalingGroup) Prelude.. Lens.mapping Lens.coerced

-- | The warm pool for the group.
autoScalingGroup_warmPoolConfiguration :: Lens.Lens' AutoScalingGroup (Prelude.Maybe WarmPoolConfiguration)
autoScalingGroup_warmPoolConfiguration = Lens.lens (\AutoScalingGroup' {warmPoolConfiguration} -> warmPoolConfiguration) (\s@AutoScalingGroup' {} a -> s {warmPoolConfiguration = a} :: AutoScalingGroup)

-- | The Amazon Resource Name (ARN) of the Auto Scaling group.
autoScalingGroup_autoScalingGroupARN :: Lens.Lens' AutoScalingGroup (Prelude.Maybe Prelude.Text)
autoScalingGroup_autoScalingGroupARN = Lens.lens (\AutoScalingGroup' {autoScalingGroupARN} -> autoScalingGroupARN) (\s@AutoScalingGroup' {} a -> s {autoScalingGroupARN = a} :: AutoScalingGroup)

-- | One or more load balancers associated with the group.
autoScalingGroup_loadBalancerNames :: Lens.Lens' AutoScalingGroup (Prelude.Maybe [Prelude.Text])
autoScalingGroup_loadBalancerNames = Lens.lens (\AutoScalingGroup' {loadBalancerNames} -> loadBalancerNames) (\s@AutoScalingGroup' {} a -> s {loadBalancerNames = a} :: AutoScalingGroup) Prelude.. Lens.mapping Lens.coerced

-- | The predicted capacity of the group when it has a predictive scaling
-- policy.
autoScalingGroup_predictedCapacity :: Lens.Lens' AutoScalingGroup (Prelude.Maybe Prelude.Int)
autoScalingGroup_predictedCapacity = Lens.lens (\AutoScalingGroup' {predictedCapacity} -> predictedCapacity) (\s@AutoScalingGroup' {} a -> s {predictedCapacity = a} :: AutoScalingGroup)

-- | The duration of the health check grace period, in seconds.
autoScalingGroup_healthCheckGracePeriod :: Lens.Lens' AutoScalingGroup (Prelude.Maybe Prelude.Int)
autoScalingGroup_healthCheckGracePeriod = Lens.lens (\AutoScalingGroup' {healthCheckGracePeriod} -> healthCheckGracePeriod) (\s@AutoScalingGroup' {} a -> s {healthCheckGracePeriod = a} :: AutoScalingGroup)

-- | The launch template for the group.
autoScalingGroup_launchTemplate :: Lens.Lens' AutoScalingGroup (Prelude.Maybe LaunchTemplateSpecification)
autoScalingGroup_launchTemplate = Lens.lens (\AutoScalingGroup' {launchTemplate} -> launchTemplate) (\s@AutoScalingGroup' {} a -> s {launchTemplate = a} :: AutoScalingGroup)

-- | The current size of the warm pool.
autoScalingGroup_warmPoolSize :: Lens.Lens' AutoScalingGroup (Prelude.Maybe Prelude.Int)
autoScalingGroup_warmPoolSize = Lens.lens (\AutoScalingGroup' {warmPoolSize} -> warmPoolSize) (\s@AutoScalingGroup' {} a -> s {warmPoolSize = a} :: AutoScalingGroup)

-- | The Amazon Resource Name (ARN) of the service-linked role that the Auto
-- Scaling group uses to call other Amazon Web Services on your behalf.
autoScalingGroup_serviceLinkedRoleARN :: Lens.Lens' AutoScalingGroup (Prelude.Maybe Prelude.Text)
autoScalingGroup_serviceLinkedRoleARN = Lens.lens (\AutoScalingGroup' {serviceLinkedRoleARN} -> serviceLinkedRoleARN) (\s@AutoScalingGroup' {} a -> s {serviceLinkedRoleARN = a} :: AutoScalingGroup)

-- | Reserved.
autoScalingGroup_context :: Lens.Lens' AutoScalingGroup (Prelude.Maybe Prelude.Text)
autoScalingGroup_context = Lens.lens (\AutoScalingGroup' {context} -> context) (\s@AutoScalingGroup' {} a -> s {context = a} :: AutoScalingGroup)

-- | The current state of the group when the DeleteAutoScalingGroup operation
-- is in progress.
autoScalingGroup_status :: Lens.Lens' AutoScalingGroup (Prelude.Maybe Prelude.Text)
autoScalingGroup_status = Lens.lens (\AutoScalingGroup' {status} -> status) (\s@AutoScalingGroup' {} a -> s {status = a} :: AutoScalingGroup)

-- | The maximum amount of time, in seconds, that an instance can be in
-- service.
--
-- Valid Range: Minimum value of 0.
autoScalingGroup_maxInstanceLifetime :: Lens.Lens' AutoScalingGroup (Prelude.Maybe Prelude.Int)
autoScalingGroup_maxInstanceLifetime = Lens.lens (\AutoScalingGroup' {maxInstanceLifetime} -> maxInstanceLifetime) (\s@AutoScalingGroup' {} a -> s {maxInstanceLifetime = a} :: AutoScalingGroup)

-- | One or more subnet IDs, if applicable, separated by commas.
autoScalingGroup_vPCZoneIdentifier :: Lens.Lens' AutoScalingGroup (Prelude.Maybe Prelude.Text)
autoScalingGroup_vPCZoneIdentifier = Lens.lens (\AutoScalingGroup' {vPCZoneIdentifier} -> vPCZoneIdentifier) (\s@AutoScalingGroup' {} a -> s {vPCZoneIdentifier = a} :: AutoScalingGroup)

-- | The name of the associated launch configuration.
autoScalingGroup_launchConfigurationName :: Lens.Lens' AutoScalingGroup (Prelude.Maybe Prelude.Text)
autoScalingGroup_launchConfigurationName = Lens.lens (\AutoScalingGroup' {launchConfigurationName} -> launchConfigurationName) (\s@AutoScalingGroup' {} a -> s {launchConfigurationName = a} :: AutoScalingGroup)

-- | The Amazon Resource Names (ARN) of the target groups for your load
-- balancer.
autoScalingGroup_targetGroupARNs :: Lens.Lens' AutoScalingGroup (Prelude.Maybe [Prelude.Text])
autoScalingGroup_targetGroupARNs = Lens.lens (\AutoScalingGroup' {targetGroupARNs} -> targetGroupARNs) (\s@AutoScalingGroup' {} a -> s {targetGroupARNs = a} :: AutoScalingGroup) Prelude.. Lens.mapping Lens.coerced

-- | The duration of the default instance warmup, in seconds.
autoScalingGroup_defaultInstanceWarmup :: Lens.Lens' AutoScalingGroup (Prelude.Maybe Prelude.Int)
autoScalingGroup_defaultInstanceWarmup = Lens.lens (\AutoScalingGroup' {defaultInstanceWarmup} -> defaultInstanceWarmup) (\s@AutoScalingGroup' {} a -> s {defaultInstanceWarmup = a} :: AutoScalingGroup)

-- | The mixed instances policy for the group.
autoScalingGroup_mixedInstancesPolicy :: Lens.Lens' AutoScalingGroup (Prelude.Maybe MixedInstancesPolicy)
autoScalingGroup_mixedInstancesPolicy = Lens.lens (\AutoScalingGroup' {mixedInstancesPolicy} -> mixedInstancesPolicy) (\s@AutoScalingGroup' {} a -> s {mixedInstancesPolicy = a} :: AutoScalingGroup)

-- | The name of the placement group into which to launch your instances, if
-- any.
autoScalingGroup_placementGroup :: Lens.Lens' AutoScalingGroup (Prelude.Maybe Prelude.Text)
autoScalingGroup_placementGroup = Lens.lens (\AutoScalingGroup' {placementGroup} -> placementGroup) (\s@AutoScalingGroup' {} a -> s {placementGroup = a} :: AutoScalingGroup)

-- | The unit of measurement for the value specified for desired capacity.
-- Amazon EC2 Auto Scaling supports @DesiredCapacityType@ for
-- attribute-based instance type selection only.
autoScalingGroup_desiredCapacityType :: Lens.Lens' AutoScalingGroup (Prelude.Maybe Prelude.Text)
autoScalingGroup_desiredCapacityType = Lens.lens (\AutoScalingGroup' {desiredCapacityType} -> desiredCapacityType) (\s@AutoScalingGroup' {} a -> s {desiredCapacityType = a} :: AutoScalingGroup)

-- | Indicates whether newly launched instances are protected from
-- termination by Amazon EC2 Auto Scaling when scaling in.
autoScalingGroup_newInstancesProtectedFromScaleIn :: Lens.Lens' AutoScalingGroup (Prelude.Maybe Prelude.Bool)
autoScalingGroup_newInstancesProtectedFromScaleIn = Lens.lens (\AutoScalingGroup' {newInstancesProtectedFromScaleIn'} -> newInstancesProtectedFromScaleIn') (\s@AutoScalingGroup' {} a -> s {newInstancesProtectedFromScaleIn' = a} :: AutoScalingGroup)

-- | The termination policies for the group.
autoScalingGroup_terminationPolicies :: Lens.Lens' AutoScalingGroup (Prelude.Maybe [Prelude.Text])
autoScalingGroup_terminationPolicies = Lens.lens (\AutoScalingGroup' {terminationPolicies} -> terminationPolicies) (\s@AutoScalingGroup' {} a -> s {terminationPolicies = a} :: AutoScalingGroup) Prelude.. Lens.mapping Lens.coerced

-- | The suspended processes associated with the group.
autoScalingGroup_suspendedProcesses :: Lens.Lens' AutoScalingGroup (Prelude.Maybe [SuspendedProcess])
autoScalingGroup_suspendedProcesses = Lens.lens (\AutoScalingGroup' {suspendedProcesses} -> suspendedProcesses) (\s@AutoScalingGroup' {} a -> s {suspendedProcesses = a} :: AutoScalingGroup) Prelude.. Lens.mapping Lens.coerced

-- | The metrics enabled for the group.
autoScalingGroup_enabledMetrics :: Lens.Lens' AutoScalingGroup (Prelude.Maybe [EnabledMetric])
autoScalingGroup_enabledMetrics = Lens.lens (\AutoScalingGroup' {enabledMetrics} -> enabledMetrics) (\s@AutoScalingGroup' {} a -> s {enabledMetrics = a} :: AutoScalingGroup) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether Capacity Rebalancing is enabled.
autoScalingGroup_capacityRebalance :: Lens.Lens' AutoScalingGroup (Prelude.Maybe Prelude.Bool)
autoScalingGroup_capacityRebalance = Lens.lens (\AutoScalingGroup' {capacityRebalance} -> capacityRebalance) (\s@AutoScalingGroup' {} a -> s {capacityRebalance = a} :: AutoScalingGroup)

-- | The name of the Auto Scaling group.
autoScalingGroup_autoScalingGroupName :: Lens.Lens' AutoScalingGroup Prelude.Text
autoScalingGroup_autoScalingGroupName = Lens.lens (\AutoScalingGroup' {autoScalingGroupName} -> autoScalingGroupName) (\s@AutoScalingGroup' {} a -> s {autoScalingGroupName = a} :: AutoScalingGroup)

-- | The minimum size of the group.
autoScalingGroup_minSize :: Lens.Lens' AutoScalingGroup Prelude.Int
autoScalingGroup_minSize = Lens.lens (\AutoScalingGroup' {minSize} -> minSize) (\s@AutoScalingGroup' {} a -> s {minSize = a} :: AutoScalingGroup)

-- | The maximum size of the group.
autoScalingGroup_maxSize :: Lens.Lens' AutoScalingGroup Prelude.Int
autoScalingGroup_maxSize = Lens.lens (\AutoScalingGroup' {maxSize} -> maxSize) (\s@AutoScalingGroup' {} a -> s {maxSize = a} :: AutoScalingGroup)

-- | The desired size of the group.
autoScalingGroup_desiredCapacity :: Lens.Lens' AutoScalingGroup Prelude.Int
autoScalingGroup_desiredCapacity = Lens.lens (\AutoScalingGroup' {desiredCapacity} -> desiredCapacity) (\s@AutoScalingGroup' {} a -> s {desiredCapacity = a} :: AutoScalingGroup)

-- | The duration of the default cooldown period, in seconds.
autoScalingGroup_defaultCooldown :: Lens.Lens' AutoScalingGroup Prelude.Int
autoScalingGroup_defaultCooldown = Lens.lens (\AutoScalingGroup' {defaultCooldown} -> defaultCooldown) (\s@AutoScalingGroup' {} a -> s {defaultCooldown = a} :: AutoScalingGroup)

-- | One or more Availability Zones for the group.
autoScalingGroup_availabilityZones :: Lens.Lens' AutoScalingGroup [Prelude.Text]
autoScalingGroup_availabilityZones = Lens.lens (\AutoScalingGroup' {availabilityZones} -> availabilityZones) (\s@AutoScalingGroup' {} a -> s {availabilityZones = a} :: AutoScalingGroup) Prelude.. Lens.coerced

-- | The service to use for the health checks. The valid values are @EC2@ and
-- @ELB@. If you configure an Auto Scaling group to use @ELB@ health
-- checks, it considers the instance unhealthy if it fails either the EC2
-- status checks or the load balancer health checks.
autoScalingGroup_healthCheckType :: Lens.Lens' AutoScalingGroup Prelude.Text
autoScalingGroup_healthCheckType = Lens.lens (\AutoScalingGroup' {healthCheckType} -> healthCheckType) (\s@AutoScalingGroup' {} a -> s {healthCheckType = a} :: AutoScalingGroup)

-- | The date and time the group was created.
autoScalingGroup_createdTime :: Lens.Lens' AutoScalingGroup Prelude.UTCTime
autoScalingGroup_createdTime = Lens.lens (\AutoScalingGroup' {createdTime} -> createdTime) (\s@AutoScalingGroup' {} a -> s {createdTime = a} :: AutoScalingGroup) Prelude.. Core._Time

instance Core.FromXML AutoScalingGroup where
  parseXML x =
    AutoScalingGroup'
      Prelude.<$> ( x Core..@? "Tags" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> ( x Core..@? "Instances" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "WarmPoolConfiguration")
      Prelude.<*> (x Core..@? "AutoScalingGroupARN")
      Prelude.<*> ( x Core..@? "LoadBalancerNames"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "PredictedCapacity")
      Prelude.<*> (x Core..@? "HealthCheckGracePeriod")
      Prelude.<*> (x Core..@? "LaunchTemplate")
      Prelude.<*> (x Core..@? "WarmPoolSize")
      Prelude.<*> (x Core..@? "ServiceLinkedRoleARN")
      Prelude.<*> (x Core..@? "Context")
      Prelude.<*> (x Core..@? "Status")
      Prelude.<*> (x Core..@? "MaxInstanceLifetime")
      Prelude.<*> (x Core..@? "VPCZoneIdentifier")
      Prelude.<*> (x Core..@? "LaunchConfigurationName")
      Prelude.<*> ( x Core..@? "TargetGroupARNs" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "DefaultInstanceWarmup")
      Prelude.<*> (x Core..@? "MixedInstancesPolicy")
      Prelude.<*> (x Core..@? "PlacementGroup")
      Prelude.<*> (x Core..@? "DesiredCapacityType")
      Prelude.<*> (x Core..@? "NewInstancesProtectedFromScaleIn")
      Prelude.<*> ( x Core..@? "TerminationPolicies"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> ( x Core..@? "SuspendedProcesses"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> ( x Core..@? "EnabledMetrics" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "CapacityRebalance")
      Prelude.<*> (x Core..@ "AutoScalingGroupName")
      Prelude.<*> (x Core..@ "MinSize")
      Prelude.<*> (x Core..@ "MaxSize")
      Prelude.<*> (x Core..@ "DesiredCapacity")
      Prelude.<*> (x Core..@ "DefaultCooldown")
      Prelude.<*> ( x Core..@? "AvailabilityZones"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.parseXMLList "member"
                  )
      Prelude.<*> (x Core..@ "HealthCheckType")
      Prelude.<*> (x Core..@ "CreatedTime")

instance Prelude.Hashable AutoScalingGroup where
  hashWithSalt _salt AutoScalingGroup' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` instances
      `Prelude.hashWithSalt` warmPoolConfiguration
      `Prelude.hashWithSalt` autoScalingGroupARN
      `Prelude.hashWithSalt` loadBalancerNames
      `Prelude.hashWithSalt` predictedCapacity
      `Prelude.hashWithSalt` healthCheckGracePeriod
      `Prelude.hashWithSalt` launchTemplate
      `Prelude.hashWithSalt` warmPoolSize
      `Prelude.hashWithSalt` serviceLinkedRoleARN
      `Prelude.hashWithSalt` context
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` maxInstanceLifetime
      `Prelude.hashWithSalt` vPCZoneIdentifier
      `Prelude.hashWithSalt` launchConfigurationName
      `Prelude.hashWithSalt` targetGroupARNs
      `Prelude.hashWithSalt` defaultInstanceWarmup
      `Prelude.hashWithSalt` mixedInstancesPolicy
      `Prelude.hashWithSalt` placementGroup
      `Prelude.hashWithSalt` desiredCapacityType
      `Prelude.hashWithSalt` newInstancesProtectedFromScaleIn'
      `Prelude.hashWithSalt` terminationPolicies
      `Prelude.hashWithSalt` suspendedProcesses
      `Prelude.hashWithSalt` enabledMetrics
      `Prelude.hashWithSalt` capacityRebalance
      `Prelude.hashWithSalt` autoScalingGroupName
      `Prelude.hashWithSalt` minSize
      `Prelude.hashWithSalt` maxSize
      `Prelude.hashWithSalt` desiredCapacity
      `Prelude.hashWithSalt` defaultCooldown
      `Prelude.hashWithSalt` availabilityZones
      `Prelude.hashWithSalt` healthCheckType
      `Prelude.hashWithSalt` createdTime

instance Prelude.NFData AutoScalingGroup where
  rnf AutoScalingGroup' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf instances
      `Prelude.seq` Prelude.rnf warmPoolConfiguration
      `Prelude.seq` Prelude.rnf autoScalingGroupARN
      `Prelude.seq` Prelude.rnf loadBalancerNames
      `Prelude.seq` Prelude.rnf predictedCapacity
      `Prelude.seq` Prelude.rnf healthCheckGracePeriod
      `Prelude.seq` Prelude.rnf launchTemplate
      `Prelude.seq` Prelude.rnf warmPoolSize
      `Prelude.seq` Prelude.rnf serviceLinkedRoleARN
      `Prelude.seq` Prelude.rnf context
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf maxInstanceLifetime
      `Prelude.seq` Prelude.rnf vPCZoneIdentifier
      `Prelude.seq` Prelude.rnf launchConfigurationName
      `Prelude.seq` Prelude.rnf targetGroupARNs
      `Prelude.seq` Prelude.rnf defaultInstanceWarmup
      `Prelude.seq` Prelude.rnf mixedInstancesPolicy
      `Prelude.seq` Prelude.rnf placementGroup
      `Prelude.seq` Prelude.rnf
        desiredCapacityType
      `Prelude.seq` Prelude.rnf
        newInstancesProtectedFromScaleIn'
      `Prelude.seq` Prelude.rnf
        terminationPolicies
      `Prelude.seq` Prelude.rnf
        suspendedProcesses
      `Prelude.seq` Prelude.rnf
        enabledMetrics
      `Prelude.seq` Prelude.rnf
        capacityRebalance
      `Prelude.seq` Prelude.rnf
        autoScalingGroupName
      `Prelude.seq` Prelude.rnf
        minSize
      `Prelude.seq` Prelude.rnf
        maxSize
      `Prelude.seq` Prelude.rnf
        desiredCapacity
      `Prelude.seq` Prelude.rnf
        defaultCooldown
      `Prelude.seq` Prelude.rnf
        availabilityZones
      `Prelude.seq` Prelude.rnf
        healthCheckType
      `Prelude.seq` Prelude.rnf
        createdTime
