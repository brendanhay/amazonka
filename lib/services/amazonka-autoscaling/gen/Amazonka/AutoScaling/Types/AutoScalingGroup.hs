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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
import Amazonka.AutoScaling.Types.TrafficSourceIdentifier
import Amazonka.AutoScaling.Types.WarmPoolConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an Auto Scaling group.
--
-- /See:/ 'newAutoScalingGroup' smart constructor.
data AutoScalingGroup = AutoScalingGroup'
  { -- | The Amazon Resource Name (ARN) of the Auto Scaling group.
    autoScalingGroupARN :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether Capacity Rebalancing is enabled.
    capacityRebalance :: Prelude.Maybe Prelude.Bool,
    -- | Reserved.
    context :: Prelude.Maybe Prelude.Text,
    -- | The duration of the default instance warmup, in seconds.
    defaultInstanceWarmup :: Prelude.Maybe Prelude.Int,
    -- | The unit of measurement for the value specified for desired capacity.
    -- Amazon EC2 Auto Scaling supports @DesiredCapacityType@ for
    -- attribute-based instance type selection only.
    desiredCapacityType :: Prelude.Maybe Prelude.Text,
    -- | The metrics enabled for the group.
    enabledMetrics :: Prelude.Maybe [EnabledMetric],
    -- | The duration of the health check grace period, in seconds.
    healthCheckGracePeriod :: Prelude.Maybe Prelude.Int,
    -- | The EC2 instances associated with the group.
    instances :: Prelude.Maybe [Instance],
    -- | The name of the associated launch configuration.
    launchConfigurationName :: Prelude.Maybe Prelude.Text,
    -- | The launch template for the group.
    launchTemplate :: Prelude.Maybe LaunchTemplateSpecification,
    -- | One or more load balancers associated with the group.
    loadBalancerNames :: Prelude.Maybe [Prelude.Text],
    -- | The maximum amount of time, in seconds, that an instance can be in
    -- service.
    --
    -- Valid Range: Minimum value of 0.
    maxInstanceLifetime :: Prelude.Maybe Prelude.Int,
    -- | The mixed instances policy for the group.
    mixedInstancesPolicy :: Prelude.Maybe MixedInstancesPolicy,
    -- | Indicates whether newly launched instances are protected from
    -- termination by Amazon EC2 Auto Scaling when scaling in.
    newInstancesProtectedFromScaleIn' :: Prelude.Maybe Prelude.Bool,
    -- | The name of the placement group into which to launch your instances, if
    -- any.
    placementGroup :: Prelude.Maybe Prelude.Text,
    -- | The predicted capacity of the group when it has a predictive scaling
    -- policy.
    predictedCapacity :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the service-linked role that the Auto
    -- Scaling group uses to call other Amazon Web Services on your behalf.
    serviceLinkedRoleARN :: Prelude.Maybe Prelude.Text,
    -- | The current state of the group when the DeleteAutoScalingGroup operation
    -- is in progress.
    status :: Prelude.Maybe Prelude.Text,
    -- | The suspended processes associated with the group.
    suspendedProcesses :: Prelude.Maybe [SuspendedProcess],
    -- | The tags for the group.
    tags :: Prelude.Maybe [TagDescription],
    -- | The Amazon Resource Names (ARN) of the target groups for your load
    -- balancer.
    targetGroupARNs :: Prelude.Maybe [Prelude.Text],
    -- | The termination policies for the group.
    terminationPolicies :: Prelude.Maybe [Prelude.Text],
    -- | The traffic sources associated with this Auto Scaling group.
    trafficSources :: Prelude.Maybe [TrafficSourceIdentifier],
    -- | One or more subnet IDs, if applicable, separated by commas.
    vPCZoneIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The warm pool for the group.
    warmPoolConfiguration :: Prelude.Maybe WarmPoolConfiguration,
    -- | The current size of the warm pool.
    warmPoolSize :: Prelude.Maybe Prelude.Int,
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
    -- | A comma-separated value string of one or more health check types.
    healthCheckType :: Prelude.Text,
    -- | The date and time the group was created.
    createdTime :: Data.ISO8601
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
-- 'autoScalingGroupARN', 'autoScalingGroup_autoScalingGroupARN' - The Amazon Resource Name (ARN) of the Auto Scaling group.
--
-- 'capacityRebalance', 'autoScalingGroup_capacityRebalance' - Indicates whether Capacity Rebalancing is enabled.
--
-- 'context', 'autoScalingGroup_context' - Reserved.
--
-- 'defaultInstanceWarmup', 'autoScalingGroup_defaultInstanceWarmup' - The duration of the default instance warmup, in seconds.
--
-- 'desiredCapacityType', 'autoScalingGroup_desiredCapacityType' - The unit of measurement for the value specified for desired capacity.
-- Amazon EC2 Auto Scaling supports @DesiredCapacityType@ for
-- attribute-based instance type selection only.
--
-- 'enabledMetrics', 'autoScalingGroup_enabledMetrics' - The metrics enabled for the group.
--
-- 'healthCheckGracePeriod', 'autoScalingGroup_healthCheckGracePeriod' - The duration of the health check grace period, in seconds.
--
-- 'instances', 'autoScalingGroup_instances' - The EC2 instances associated with the group.
--
-- 'launchConfigurationName', 'autoScalingGroup_launchConfigurationName' - The name of the associated launch configuration.
--
-- 'launchTemplate', 'autoScalingGroup_launchTemplate' - The launch template for the group.
--
-- 'loadBalancerNames', 'autoScalingGroup_loadBalancerNames' - One or more load balancers associated with the group.
--
-- 'maxInstanceLifetime', 'autoScalingGroup_maxInstanceLifetime' - The maximum amount of time, in seconds, that an instance can be in
-- service.
--
-- Valid Range: Minimum value of 0.
--
-- 'mixedInstancesPolicy', 'autoScalingGroup_mixedInstancesPolicy' - The mixed instances policy for the group.
--
-- 'newInstancesProtectedFromScaleIn'', 'autoScalingGroup_newInstancesProtectedFromScaleIn' - Indicates whether newly launched instances are protected from
-- termination by Amazon EC2 Auto Scaling when scaling in.
--
-- 'placementGroup', 'autoScalingGroup_placementGroup' - The name of the placement group into which to launch your instances, if
-- any.
--
-- 'predictedCapacity', 'autoScalingGroup_predictedCapacity' - The predicted capacity of the group when it has a predictive scaling
-- policy.
--
-- 'serviceLinkedRoleARN', 'autoScalingGroup_serviceLinkedRoleARN' - The Amazon Resource Name (ARN) of the service-linked role that the Auto
-- Scaling group uses to call other Amazon Web Services on your behalf.
--
-- 'status', 'autoScalingGroup_status' - The current state of the group when the DeleteAutoScalingGroup operation
-- is in progress.
--
-- 'suspendedProcesses', 'autoScalingGroup_suspendedProcesses' - The suspended processes associated with the group.
--
-- 'tags', 'autoScalingGroup_tags' - The tags for the group.
--
-- 'targetGroupARNs', 'autoScalingGroup_targetGroupARNs' - The Amazon Resource Names (ARN) of the target groups for your load
-- balancer.
--
-- 'terminationPolicies', 'autoScalingGroup_terminationPolicies' - The termination policies for the group.
--
-- 'trafficSources', 'autoScalingGroup_trafficSources' - The traffic sources associated with this Auto Scaling group.
--
-- 'vPCZoneIdentifier', 'autoScalingGroup_vPCZoneIdentifier' - One or more subnet IDs, if applicable, separated by commas.
--
-- 'warmPoolConfiguration', 'autoScalingGroup_warmPoolConfiguration' - The warm pool for the group.
--
-- 'warmPoolSize', 'autoScalingGroup_warmPoolSize' - The current size of the warm pool.
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
-- 'healthCheckType', 'autoScalingGroup_healthCheckType' - A comma-separated value string of one or more health check types.
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
      { autoScalingGroupARN =
          Prelude.Nothing,
        capacityRebalance = Prelude.Nothing,
        context = Prelude.Nothing,
        defaultInstanceWarmup = Prelude.Nothing,
        desiredCapacityType = Prelude.Nothing,
        enabledMetrics = Prelude.Nothing,
        healthCheckGracePeriod = Prelude.Nothing,
        instances = Prelude.Nothing,
        launchConfigurationName = Prelude.Nothing,
        launchTemplate = Prelude.Nothing,
        loadBalancerNames = Prelude.Nothing,
        maxInstanceLifetime = Prelude.Nothing,
        mixedInstancesPolicy = Prelude.Nothing,
        newInstancesProtectedFromScaleIn' = Prelude.Nothing,
        placementGroup = Prelude.Nothing,
        predictedCapacity = Prelude.Nothing,
        serviceLinkedRoleARN = Prelude.Nothing,
        status = Prelude.Nothing,
        suspendedProcesses = Prelude.Nothing,
        tags = Prelude.Nothing,
        targetGroupARNs = Prelude.Nothing,
        terminationPolicies = Prelude.Nothing,
        trafficSources = Prelude.Nothing,
        vPCZoneIdentifier = Prelude.Nothing,
        warmPoolConfiguration = Prelude.Nothing,
        warmPoolSize = Prelude.Nothing,
        autoScalingGroupName = pAutoScalingGroupName_,
        minSize = pMinSize_,
        maxSize = pMaxSize_,
        desiredCapacity = pDesiredCapacity_,
        defaultCooldown = pDefaultCooldown_,
        availabilityZones = Prelude.mempty,
        healthCheckType = pHealthCheckType_,
        createdTime = Data._Time Lens.# pCreatedTime_
      }

-- | The Amazon Resource Name (ARN) of the Auto Scaling group.
autoScalingGroup_autoScalingGroupARN :: Lens.Lens' AutoScalingGroup (Prelude.Maybe Prelude.Text)
autoScalingGroup_autoScalingGroupARN = Lens.lens (\AutoScalingGroup' {autoScalingGroupARN} -> autoScalingGroupARN) (\s@AutoScalingGroup' {} a -> s {autoScalingGroupARN = a} :: AutoScalingGroup)

-- | Indicates whether Capacity Rebalancing is enabled.
autoScalingGroup_capacityRebalance :: Lens.Lens' AutoScalingGroup (Prelude.Maybe Prelude.Bool)
autoScalingGroup_capacityRebalance = Lens.lens (\AutoScalingGroup' {capacityRebalance} -> capacityRebalance) (\s@AutoScalingGroup' {} a -> s {capacityRebalance = a} :: AutoScalingGroup)

-- | Reserved.
autoScalingGroup_context :: Lens.Lens' AutoScalingGroup (Prelude.Maybe Prelude.Text)
autoScalingGroup_context = Lens.lens (\AutoScalingGroup' {context} -> context) (\s@AutoScalingGroup' {} a -> s {context = a} :: AutoScalingGroup)

-- | The duration of the default instance warmup, in seconds.
autoScalingGroup_defaultInstanceWarmup :: Lens.Lens' AutoScalingGroup (Prelude.Maybe Prelude.Int)
autoScalingGroup_defaultInstanceWarmup = Lens.lens (\AutoScalingGroup' {defaultInstanceWarmup} -> defaultInstanceWarmup) (\s@AutoScalingGroup' {} a -> s {defaultInstanceWarmup = a} :: AutoScalingGroup)

-- | The unit of measurement for the value specified for desired capacity.
-- Amazon EC2 Auto Scaling supports @DesiredCapacityType@ for
-- attribute-based instance type selection only.
autoScalingGroup_desiredCapacityType :: Lens.Lens' AutoScalingGroup (Prelude.Maybe Prelude.Text)
autoScalingGroup_desiredCapacityType = Lens.lens (\AutoScalingGroup' {desiredCapacityType} -> desiredCapacityType) (\s@AutoScalingGroup' {} a -> s {desiredCapacityType = a} :: AutoScalingGroup)

-- | The metrics enabled for the group.
autoScalingGroup_enabledMetrics :: Lens.Lens' AutoScalingGroup (Prelude.Maybe [EnabledMetric])
autoScalingGroup_enabledMetrics = Lens.lens (\AutoScalingGroup' {enabledMetrics} -> enabledMetrics) (\s@AutoScalingGroup' {} a -> s {enabledMetrics = a} :: AutoScalingGroup) Prelude.. Lens.mapping Lens.coerced

-- | The duration of the health check grace period, in seconds.
autoScalingGroup_healthCheckGracePeriod :: Lens.Lens' AutoScalingGroup (Prelude.Maybe Prelude.Int)
autoScalingGroup_healthCheckGracePeriod = Lens.lens (\AutoScalingGroup' {healthCheckGracePeriod} -> healthCheckGracePeriod) (\s@AutoScalingGroup' {} a -> s {healthCheckGracePeriod = a} :: AutoScalingGroup)

-- | The EC2 instances associated with the group.
autoScalingGroup_instances :: Lens.Lens' AutoScalingGroup (Prelude.Maybe [Instance])
autoScalingGroup_instances = Lens.lens (\AutoScalingGroup' {instances} -> instances) (\s@AutoScalingGroup' {} a -> s {instances = a} :: AutoScalingGroup) Prelude.. Lens.mapping Lens.coerced

-- | The name of the associated launch configuration.
autoScalingGroup_launchConfigurationName :: Lens.Lens' AutoScalingGroup (Prelude.Maybe Prelude.Text)
autoScalingGroup_launchConfigurationName = Lens.lens (\AutoScalingGroup' {launchConfigurationName} -> launchConfigurationName) (\s@AutoScalingGroup' {} a -> s {launchConfigurationName = a} :: AutoScalingGroup)

-- | The launch template for the group.
autoScalingGroup_launchTemplate :: Lens.Lens' AutoScalingGroup (Prelude.Maybe LaunchTemplateSpecification)
autoScalingGroup_launchTemplate = Lens.lens (\AutoScalingGroup' {launchTemplate} -> launchTemplate) (\s@AutoScalingGroup' {} a -> s {launchTemplate = a} :: AutoScalingGroup)

-- | One or more load balancers associated with the group.
autoScalingGroup_loadBalancerNames :: Lens.Lens' AutoScalingGroup (Prelude.Maybe [Prelude.Text])
autoScalingGroup_loadBalancerNames = Lens.lens (\AutoScalingGroup' {loadBalancerNames} -> loadBalancerNames) (\s@AutoScalingGroup' {} a -> s {loadBalancerNames = a} :: AutoScalingGroup) Prelude.. Lens.mapping Lens.coerced

-- | The maximum amount of time, in seconds, that an instance can be in
-- service.
--
-- Valid Range: Minimum value of 0.
autoScalingGroup_maxInstanceLifetime :: Lens.Lens' AutoScalingGroup (Prelude.Maybe Prelude.Int)
autoScalingGroup_maxInstanceLifetime = Lens.lens (\AutoScalingGroup' {maxInstanceLifetime} -> maxInstanceLifetime) (\s@AutoScalingGroup' {} a -> s {maxInstanceLifetime = a} :: AutoScalingGroup)

-- | The mixed instances policy for the group.
autoScalingGroup_mixedInstancesPolicy :: Lens.Lens' AutoScalingGroup (Prelude.Maybe MixedInstancesPolicy)
autoScalingGroup_mixedInstancesPolicy = Lens.lens (\AutoScalingGroup' {mixedInstancesPolicy} -> mixedInstancesPolicy) (\s@AutoScalingGroup' {} a -> s {mixedInstancesPolicy = a} :: AutoScalingGroup)

-- | Indicates whether newly launched instances are protected from
-- termination by Amazon EC2 Auto Scaling when scaling in.
autoScalingGroup_newInstancesProtectedFromScaleIn :: Lens.Lens' AutoScalingGroup (Prelude.Maybe Prelude.Bool)
autoScalingGroup_newInstancesProtectedFromScaleIn = Lens.lens (\AutoScalingGroup' {newInstancesProtectedFromScaleIn'} -> newInstancesProtectedFromScaleIn') (\s@AutoScalingGroup' {} a -> s {newInstancesProtectedFromScaleIn' = a} :: AutoScalingGroup)

-- | The name of the placement group into which to launch your instances, if
-- any.
autoScalingGroup_placementGroup :: Lens.Lens' AutoScalingGroup (Prelude.Maybe Prelude.Text)
autoScalingGroup_placementGroup = Lens.lens (\AutoScalingGroup' {placementGroup} -> placementGroup) (\s@AutoScalingGroup' {} a -> s {placementGroup = a} :: AutoScalingGroup)

-- | The predicted capacity of the group when it has a predictive scaling
-- policy.
autoScalingGroup_predictedCapacity :: Lens.Lens' AutoScalingGroup (Prelude.Maybe Prelude.Int)
autoScalingGroup_predictedCapacity = Lens.lens (\AutoScalingGroup' {predictedCapacity} -> predictedCapacity) (\s@AutoScalingGroup' {} a -> s {predictedCapacity = a} :: AutoScalingGroup)

-- | The Amazon Resource Name (ARN) of the service-linked role that the Auto
-- Scaling group uses to call other Amazon Web Services on your behalf.
autoScalingGroup_serviceLinkedRoleARN :: Lens.Lens' AutoScalingGroup (Prelude.Maybe Prelude.Text)
autoScalingGroup_serviceLinkedRoleARN = Lens.lens (\AutoScalingGroup' {serviceLinkedRoleARN} -> serviceLinkedRoleARN) (\s@AutoScalingGroup' {} a -> s {serviceLinkedRoleARN = a} :: AutoScalingGroup)

-- | The current state of the group when the DeleteAutoScalingGroup operation
-- is in progress.
autoScalingGroup_status :: Lens.Lens' AutoScalingGroup (Prelude.Maybe Prelude.Text)
autoScalingGroup_status = Lens.lens (\AutoScalingGroup' {status} -> status) (\s@AutoScalingGroup' {} a -> s {status = a} :: AutoScalingGroup)

-- | The suspended processes associated with the group.
autoScalingGroup_suspendedProcesses :: Lens.Lens' AutoScalingGroup (Prelude.Maybe [SuspendedProcess])
autoScalingGroup_suspendedProcesses = Lens.lens (\AutoScalingGroup' {suspendedProcesses} -> suspendedProcesses) (\s@AutoScalingGroup' {} a -> s {suspendedProcesses = a} :: AutoScalingGroup) Prelude.. Lens.mapping Lens.coerced

-- | The tags for the group.
autoScalingGroup_tags :: Lens.Lens' AutoScalingGroup (Prelude.Maybe [TagDescription])
autoScalingGroup_tags = Lens.lens (\AutoScalingGroup' {tags} -> tags) (\s@AutoScalingGroup' {} a -> s {tags = a} :: AutoScalingGroup) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Names (ARN) of the target groups for your load
-- balancer.
autoScalingGroup_targetGroupARNs :: Lens.Lens' AutoScalingGroup (Prelude.Maybe [Prelude.Text])
autoScalingGroup_targetGroupARNs = Lens.lens (\AutoScalingGroup' {targetGroupARNs} -> targetGroupARNs) (\s@AutoScalingGroup' {} a -> s {targetGroupARNs = a} :: AutoScalingGroup) Prelude.. Lens.mapping Lens.coerced

-- | The termination policies for the group.
autoScalingGroup_terminationPolicies :: Lens.Lens' AutoScalingGroup (Prelude.Maybe [Prelude.Text])
autoScalingGroup_terminationPolicies = Lens.lens (\AutoScalingGroup' {terminationPolicies} -> terminationPolicies) (\s@AutoScalingGroup' {} a -> s {terminationPolicies = a} :: AutoScalingGroup) Prelude.. Lens.mapping Lens.coerced

-- | The traffic sources associated with this Auto Scaling group.
autoScalingGroup_trafficSources :: Lens.Lens' AutoScalingGroup (Prelude.Maybe [TrafficSourceIdentifier])
autoScalingGroup_trafficSources = Lens.lens (\AutoScalingGroup' {trafficSources} -> trafficSources) (\s@AutoScalingGroup' {} a -> s {trafficSources = a} :: AutoScalingGroup) Prelude.. Lens.mapping Lens.coerced

-- | One or more subnet IDs, if applicable, separated by commas.
autoScalingGroup_vPCZoneIdentifier :: Lens.Lens' AutoScalingGroup (Prelude.Maybe Prelude.Text)
autoScalingGroup_vPCZoneIdentifier = Lens.lens (\AutoScalingGroup' {vPCZoneIdentifier} -> vPCZoneIdentifier) (\s@AutoScalingGroup' {} a -> s {vPCZoneIdentifier = a} :: AutoScalingGroup)

-- | The warm pool for the group.
autoScalingGroup_warmPoolConfiguration :: Lens.Lens' AutoScalingGroup (Prelude.Maybe WarmPoolConfiguration)
autoScalingGroup_warmPoolConfiguration = Lens.lens (\AutoScalingGroup' {warmPoolConfiguration} -> warmPoolConfiguration) (\s@AutoScalingGroup' {} a -> s {warmPoolConfiguration = a} :: AutoScalingGroup)

-- | The current size of the warm pool.
autoScalingGroup_warmPoolSize :: Lens.Lens' AutoScalingGroup (Prelude.Maybe Prelude.Int)
autoScalingGroup_warmPoolSize = Lens.lens (\AutoScalingGroup' {warmPoolSize} -> warmPoolSize) (\s@AutoScalingGroup' {} a -> s {warmPoolSize = a} :: AutoScalingGroup)

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

-- | A comma-separated value string of one or more health check types.
autoScalingGroup_healthCheckType :: Lens.Lens' AutoScalingGroup Prelude.Text
autoScalingGroup_healthCheckType = Lens.lens (\AutoScalingGroup' {healthCheckType} -> healthCheckType) (\s@AutoScalingGroup' {} a -> s {healthCheckType = a} :: AutoScalingGroup)

-- | The date and time the group was created.
autoScalingGroup_createdTime :: Lens.Lens' AutoScalingGroup Prelude.UTCTime
autoScalingGroup_createdTime = Lens.lens (\AutoScalingGroup' {createdTime} -> createdTime) (\s@AutoScalingGroup' {} a -> s {createdTime = a} :: AutoScalingGroup) Prelude.. Data._Time

instance Data.FromXML AutoScalingGroup where
  parseXML x =
    AutoScalingGroup'
      Prelude.<$> (x Data..@? "AutoScalingGroupARN")
      Prelude.<*> (x Data..@? "CapacityRebalance")
      Prelude.<*> (x Data..@? "Context")
      Prelude.<*> (x Data..@? "DefaultInstanceWarmup")
      Prelude.<*> (x Data..@? "DesiredCapacityType")
      Prelude.<*> ( x
                      Data..@? "EnabledMetrics"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "HealthCheckGracePeriod")
      Prelude.<*> ( x
                      Data..@? "Instances"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "LaunchConfigurationName")
      Prelude.<*> (x Data..@? "LaunchTemplate")
      Prelude.<*> ( x
                      Data..@? "LoadBalancerNames"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "MaxInstanceLifetime")
      Prelude.<*> (x Data..@? "MixedInstancesPolicy")
      Prelude.<*> (x Data..@? "NewInstancesProtectedFromScaleIn")
      Prelude.<*> (x Data..@? "PlacementGroup")
      Prelude.<*> (x Data..@? "PredictedCapacity")
      Prelude.<*> (x Data..@? "ServiceLinkedRoleARN")
      Prelude.<*> (x Data..@? "Status")
      Prelude.<*> ( x
                      Data..@? "SuspendedProcesses"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> ( x
                      Data..@? "Tags"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> ( x
                      Data..@? "TargetGroupARNs"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> ( x
                      Data..@? "TerminationPolicies"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> ( x
                      Data..@? "TrafficSources"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "VPCZoneIdentifier")
      Prelude.<*> (x Data..@? "WarmPoolConfiguration")
      Prelude.<*> (x Data..@? "WarmPoolSize")
      Prelude.<*> (x Data..@ "AutoScalingGroupName")
      Prelude.<*> (x Data..@ "MinSize")
      Prelude.<*> (x Data..@ "MaxSize")
      Prelude.<*> (x Data..@ "DesiredCapacity")
      Prelude.<*> (x Data..@ "DefaultCooldown")
      Prelude.<*> ( x
                      Data..@? "AvailabilityZones"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Data.parseXMLList "member"
                  )
      Prelude.<*> (x Data..@ "HealthCheckType")
      Prelude.<*> (x Data..@ "CreatedTime")

instance Prelude.Hashable AutoScalingGroup where
  hashWithSalt _salt AutoScalingGroup' {..} =
    _salt
      `Prelude.hashWithSalt` autoScalingGroupARN
      `Prelude.hashWithSalt` capacityRebalance
      `Prelude.hashWithSalt` context
      `Prelude.hashWithSalt` defaultInstanceWarmup
      `Prelude.hashWithSalt` desiredCapacityType
      `Prelude.hashWithSalt` enabledMetrics
      `Prelude.hashWithSalt` healthCheckGracePeriod
      `Prelude.hashWithSalt` instances
      `Prelude.hashWithSalt` launchConfigurationName
      `Prelude.hashWithSalt` launchTemplate
      `Prelude.hashWithSalt` loadBalancerNames
      `Prelude.hashWithSalt` maxInstanceLifetime
      `Prelude.hashWithSalt` mixedInstancesPolicy
      `Prelude.hashWithSalt` newInstancesProtectedFromScaleIn'
      `Prelude.hashWithSalt` placementGroup
      `Prelude.hashWithSalt` predictedCapacity
      `Prelude.hashWithSalt` serviceLinkedRoleARN
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` suspendedProcesses
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` targetGroupARNs
      `Prelude.hashWithSalt` terminationPolicies
      `Prelude.hashWithSalt` trafficSources
      `Prelude.hashWithSalt` vPCZoneIdentifier
      `Prelude.hashWithSalt` warmPoolConfiguration
      `Prelude.hashWithSalt` warmPoolSize
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
    Prelude.rnf autoScalingGroupARN
      `Prelude.seq` Prelude.rnf capacityRebalance
      `Prelude.seq` Prelude.rnf context
      `Prelude.seq` Prelude.rnf defaultInstanceWarmup
      `Prelude.seq` Prelude.rnf desiredCapacityType
      `Prelude.seq` Prelude.rnf enabledMetrics
      `Prelude.seq` Prelude.rnf healthCheckGracePeriod
      `Prelude.seq` Prelude.rnf instances
      `Prelude.seq` Prelude.rnf launchConfigurationName
      `Prelude.seq` Prelude.rnf launchTemplate
      `Prelude.seq` Prelude.rnf loadBalancerNames
      `Prelude.seq` Prelude.rnf maxInstanceLifetime
      `Prelude.seq` Prelude.rnf mixedInstancesPolicy
      `Prelude.seq` Prelude.rnf
        newInstancesProtectedFromScaleIn'
      `Prelude.seq` Prelude.rnf placementGroup
      `Prelude.seq` Prelude.rnf predictedCapacity
      `Prelude.seq` Prelude.rnf serviceLinkedRoleARN
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf suspendedProcesses
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf targetGroupARNs
      `Prelude.seq` Prelude.rnf
        terminationPolicies
      `Prelude.seq` Prelude.rnf
        trafficSources
      `Prelude.seq` Prelude.rnf
        vPCZoneIdentifier
      `Prelude.seq` Prelude.rnf
        warmPoolConfiguration
      `Prelude.seq` Prelude.rnf
        warmPoolSize
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
