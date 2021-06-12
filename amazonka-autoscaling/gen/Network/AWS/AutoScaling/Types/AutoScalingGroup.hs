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
-- Module      : Network.AWS.AutoScaling.Types.AutoScalingGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.AutoScalingGroup where

import Network.AWS.AutoScaling.Types.EnabledMetric
import Network.AWS.AutoScaling.Types.Instance
import Network.AWS.AutoScaling.Types.LaunchTemplateSpecification
import Network.AWS.AutoScaling.Types.MixedInstancesPolicy
import Network.AWS.AutoScaling.Types.SuspendedProcess
import Network.AWS.AutoScaling.Types.TagDescription
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes an Auto Scaling group.
--
-- /See:/ 'newAutoScalingGroup' smart constructor.
data AutoScalingGroup = AutoScalingGroup'
  { -- | The current state of the group when the DeleteAutoScalingGroup operation
    -- is in progress.
    status :: Core.Maybe Core.Text,
    -- | The name of the placement group into which to launch your instances, if
    -- any.
    placementGroup :: Core.Maybe Core.Text,
    -- | The suspended processes associated with the group.
    suspendedProcesses :: Core.Maybe [SuspendedProcess],
    -- | The maximum amount of time, in seconds, that an instance can be in
    -- service.
    --
    -- Valid Range: Minimum value of 0.
    maxInstanceLifetime :: Core.Maybe Core.Int,
    -- | The Amazon Resource Name (ARN) of the Auto Scaling group.
    autoScalingGroupARN :: Core.Maybe Core.Text,
    -- | The launch template for the group.
    launchTemplate :: Core.Maybe LaunchTemplateSpecification,
    -- | The EC2 instances associated with the group.
    instances :: Core.Maybe [Instance],
    -- | The name of the associated launch configuration.
    launchConfigurationName :: Core.Maybe Core.Text,
    -- | The mixed instances policy for the group.
    mixedInstancesPolicy :: Core.Maybe MixedInstancesPolicy,
    -- | The tags for the group.
    tags :: Core.Maybe [TagDescription],
    -- | One or more load balancers associated with the group.
    loadBalancerNames :: Core.Maybe [Core.Text],
    -- | One or more subnet IDs, if applicable, separated by commas.
    vPCZoneIdentifier :: Core.Maybe Core.Text,
    -- | The Amazon Resource Names (ARN) of the target groups for your load
    -- balancer.
    targetGroupARNs :: Core.Maybe [Core.Text],
    -- | Indicates whether Capacity Rebalancing is enabled.
    capacityRebalance :: Core.Maybe Core.Bool,
    -- | Indicates whether newly launched instances are protected from
    -- termination by Amazon EC2 Auto Scaling when scaling in.
    newInstancesProtectedFromScaleIn' :: Core.Maybe Core.Bool,
    -- | The Amazon Resource Name (ARN) of the service-linked role that the Auto
    -- Scaling group uses to call other AWS services on your behalf.
    serviceLinkedRoleARN :: Core.Maybe Core.Text,
    -- | The amount of time, in seconds, that Amazon EC2 Auto Scaling waits
    -- before checking the health status of an EC2 instance that has come into
    -- service.
    healthCheckGracePeriod :: Core.Maybe Core.Int,
    -- | The metrics enabled for the group.
    enabledMetrics :: Core.Maybe [EnabledMetric],
    -- | The termination policies for the group.
    terminationPolicies :: Core.Maybe [Core.Text],
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Core.Text,
    -- | The minimum size of the group.
    minSize :: Core.Int,
    -- | The maximum size of the group.
    maxSize :: Core.Int,
    -- | The desired size of the group.
    desiredCapacity :: Core.Int,
    -- | The duration of the default cooldown period, in seconds.
    defaultCooldown :: Core.Int,
    -- | One or more Availability Zones for the group.
    availabilityZones :: [Core.Text],
    -- | The service to use for the health checks. The valid values are @EC2@ and
    -- @ELB@. If you configure an Auto Scaling group to use ELB health checks,
    -- it considers the instance unhealthy if it fails either the EC2 status
    -- checks or the load balancer health checks.
    healthCheckType :: Core.Text,
    -- | The date and time the group was created.
    createdTime :: Core.ISO8601
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AutoScalingGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'autoScalingGroup_status' - The current state of the group when the DeleteAutoScalingGroup operation
-- is in progress.
--
-- 'placementGroup', 'autoScalingGroup_placementGroup' - The name of the placement group into which to launch your instances, if
-- any.
--
-- 'suspendedProcesses', 'autoScalingGroup_suspendedProcesses' - The suspended processes associated with the group.
--
-- 'maxInstanceLifetime', 'autoScalingGroup_maxInstanceLifetime' - The maximum amount of time, in seconds, that an instance can be in
-- service.
--
-- Valid Range: Minimum value of 0.
--
-- 'autoScalingGroupARN', 'autoScalingGroup_autoScalingGroupARN' - The Amazon Resource Name (ARN) of the Auto Scaling group.
--
-- 'launchTemplate', 'autoScalingGroup_launchTemplate' - The launch template for the group.
--
-- 'instances', 'autoScalingGroup_instances' - The EC2 instances associated with the group.
--
-- 'launchConfigurationName', 'autoScalingGroup_launchConfigurationName' - The name of the associated launch configuration.
--
-- 'mixedInstancesPolicy', 'autoScalingGroup_mixedInstancesPolicy' - The mixed instances policy for the group.
--
-- 'tags', 'autoScalingGroup_tags' - The tags for the group.
--
-- 'loadBalancerNames', 'autoScalingGroup_loadBalancerNames' - One or more load balancers associated with the group.
--
-- 'vPCZoneIdentifier', 'autoScalingGroup_vPCZoneIdentifier' - One or more subnet IDs, if applicable, separated by commas.
--
-- 'targetGroupARNs', 'autoScalingGroup_targetGroupARNs' - The Amazon Resource Names (ARN) of the target groups for your load
-- balancer.
--
-- 'capacityRebalance', 'autoScalingGroup_capacityRebalance' - Indicates whether Capacity Rebalancing is enabled.
--
-- 'newInstancesProtectedFromScaleIn'', 'autoScalingGroup_newInstancesProtectedFromScaleIn' - Indicates whether newly launched instances are protected from
-- termination by Amazon EC2 Auto Scaling when scaling in.
--
-- 'serviceLinkedRoleARN', 'autoScalingGroup_serviceLinkedRoleARN' - The Amazon Resource Name (ARN) of the service-linked role that the Auto
-- Scaling group uses to call other AWS services on your behalf.
--
-- 'healthCheckGracePeriod', 'autoScalingGroup_healthCheckGracePeriod' - The amount of time, in seconds, that Amazon EC2 Auto Scaling waits
-- before checking the health status of an EC2 instance that has come into
-- service.
--
-- 'enabledMetrics', 'autoScalingGroup_enabledMetrics' - The metrics enabled for the group.
--
-- 'terminationPolicies', 'autoScalingGroup_terminationPolicies' - The termination policies for the group.
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
-- @ELB@. If you configure an Auto Scaling group to use ELB health checks,
-- it considers the instance unhealthy if it fails either the EC2 status
-- checks or the load balancer health checks.
--
-- 'createdTime', 'autoScalingGroup_createdTime' - The date and time the group was created.
newAutoScalingGroup ::
  -- | 'autoScalingGroupName'
  Core.Text ->
  -- | 'minSize'
  Core.Int ->
  -- | 'maxSize'
  Core.Int ->
  -- | 'desiredCapacity'
  Core.Int ->
  -- | 'defaultCooldown'
  Core.Int ->
  -- | 'healthCheckType'
  Core.Text ->
  -- | 'createdTime'
  Core.UTCTime ->
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
      { status = Core.Nothing,
        placementGroup = Core.Nothing,
        suspendedProcesses = Core.Nothing,
        maxInstanceLifetime = Core.Nothing,
        autoScalingGroupARN = Core.Nothing,
        launchTemplate = Core.Nothing,
        instances = Core.Nothing,
        launchConfigurationName = Core.Nothing,
        mixedInstancesPolicy = Core.Nothing,
        tags = Core.Nothing,
        loadBalancerNames = Core.Nothing,
        vPCZoneIdentifier = Core.Nothing,
        targetGroupARNs = Core.Nothing,
        capacityRebalance = Core.Nothing,
        newInstancesProtectedFromScaleIn' = Core.Nothing,
        serviceLinkedRoleARN = Core.Nothing,
        healthCheckGracePeriod = Core.Nothing,
        enabledMetrics = Core.Nothing,
        terminationPolicies = Core.Nothing,
        autoScalingGroupName = pAutoScalingGroupName_,
        minSize = pMinSize_,
        maxSize = pMaxSize_,
        desiredCapacity = pDesiredCapacity_,
        defaultCooldown = pDefaultCooldown_,
        availabilityZones = Core.mempty,
        healthCheckType = pHealthCheckType_,
        createdTime = Core._Time Lens.# pCreatedTime_
      }

-- | The current state of the group when the DeleteAutoScalingGroup operation
-- is in progress.
autoScalingGroup_status :: Lens.Lens' AutoScalingGroup (Core.Maybe Core.Text)
autoScalingGroup_status = Lens.lens (\AutoScalingGroup' {status} -> status) (\s@AutoScalingGroup' {} a -> s {status = a} :: AutoScalingGroup)

-- | The name of the placement group into which to launch your instances, if
-- any.
autoScalingGroup_placementGroup :: Lens.Lens' AutoScalingGroup (Core.Maybe Core.Text)
autoScalingGroup_placementGroup = Lens.lens (\AutoScalingGroup' {placementGroup} -> placementGroup) (\s@AutoScalingGroup' {} a -> s {placementGroup = a} :: AutoScalingGroup)

-- | The suspended processes associated with the group.
autoScalingGroup_suspendedProcesses :: Lens.Lens' AutoScalingGroup (Core.Maybe [SuspendedProcess])
autoScalingGroup_suspendedProcesses = Lens.lens (\AutoScalingGroup' {suspendedProcesses} -> suspendedProcesses) (\s@AutoScalingGroup' {} a -> s {suspendedProcesses = a} :: AutoScalingGroup) Core.. Lens.mapping Lens._Coerce

-- | The maximum amount of time, in seconds, that an instance can be in
-- service.
--
-- Valid Range: Minimum value of 0.
autoScalingGroup_maxInstanceLifetime :: Lens.Lens' AutoScalingGroup (Core.Maybe Core.Int)
autoScalingGroup_maxInstanceLifetime = Lens.lens (\AutoScalingGroup' {maxInstanceLifetime} -> maxInstanceLifetime) (\s@AutoScalingGroup' {} a -> s {maxInstanceLifetime = a} :: AutoScalingGroup)

-- | The Amazon Resource Name (ARN) of the Auto Scaling group.
autoScalingGroup_autoScalingGroupARN :: Lens.Lens' AutoScalingGroup (Core.Maybe Core.Text)
autoScalingGroup_autoScalingGroupARN = Lens.lens (\AutoScalingGroup' {autoScalingGroupARN} -> autoScalingGroupARN) (\s@AutoScalingGroup' {} a -> s {autoScalingGroupARN = a} :: AutoScalingGroup)

-- | The launch template for the group.
autoScalingGroup_launchTemplate :: Lens.Lens' AutoScalingGroup (Core.Maybe LaunchTemplateSpecification)
autoScalingGroup_launchTemplate = Lens.lens (\AutoScalingGroup' {launchTemplate} -> launchTemplate) (\s@AutoScalingGroup' {} a -> s {launchTemplate = a} :: AutoScalingGroup)

-- | The EC2 instances associated with the group.
autoScalingGroup_instances :: Lens.Lens' AutoScalingGroup (Core.Maybe [Instance])
autoScalingGroup_instances = Lens.lens (\AutoScalingGroup' {instances} -> instances) (\s@AutoScalingGroup' {} a -> s {instances = a} :: AutoScalingGroup) Core.. Lens.mapping Lens._Coerce

-- | The name of the associated launch configuration.
autoScalingGroup_launchConfigurationName :: Lens.Lens' AutoScalingGroup (Core.Maybe Core.Text)
autoScalingGroup_launchConfigurationName = Lens.lens (\AutoScalingGroup' {launchConfigurationName} -> launchConfigurationName) (\s@AutoScalingGroup' {} a -> s {launchConfigurationName = a} :: AutoScalingGroup)

-- | The mixed instances policy for the group.
autoScalingGroup_mixedInstancesPolicy :: Lens.Lens' AutoScalingGroup (Core.Maybe MixedInstancesPolicy)
autoScalingGroup_mixedInstancesPolicy = Lens.lens (\AutoScalingGroup' {mixedInstancesPolicy} -> mixedInstancesPolicy) (\s@AutoScalingGroup' {} a -> s {mixedInstancesPolicy = a} :: AutoScalingGroup)

-- | The tags for the group.
autoScalingGroup_tags :: Lens.Lens' AutoScalingGroup (Core.Maybe [TagDescription])
autoScalingGroup_tags = Lens.lens (\AutoScalingGroup' {tags} -> tags) (\s@AutoScalingGroup' {} a -> s {tags = a} :: AutoScalingGroup) Core.. Lens.mapping Lens._Coerce

-- | One or more load balancers associated with the group.
autoScalingGroup_loadBalancerNames :: Lens.Lens' AutoScalingGroup (Core.Maybe [Core.Text])
autoScalingGroup_loadBalancerNames = Lens.lens (\AutoScalingGroup' {loadBalancerNames} -> loadBalancerNames) (\s@AutoScalingGroup' {} a -> s {loadBalancerNames = a} :: AutoScalingGroup) Core.. Lens.mapping Lens._Coerce

-- | One or more subnet IDs, if applicable, separated by commas.
autoScalingGroup_vPCZoneIdentifier :: Lens.Lens' AutoScalingGroup (Core.Maybe Core.Text)
autoScalingGroup_vPCZoneIdentifier = Lens.lens (\AutoScalingGroup' {vPCZoneIdentifier} -> vPCZoneIdentifier) (\s@AutoScalingGroup' {} a -> s {vPCZoneIdentifier = a} :: AutoScalingGroup)

-- | The Amazon Resource Names (ARN) of the target groups for your load
-- balancer.
autoScalingGroup_targetGroupARNs :: Lens.Lens' AutoScalingGroup (Core.Maybe [Core.Text])
autoScalingGroup_targetGroupARNs = Lens.lens (\AutoScalingGroup' {targetGroupARNs} -> targetGroupARNs) (\s@AutoScalingGroup' {} a -> s {targetGroupARNs = a} :: AutoScalingGroup) Core.. Lens.mapping Lens._Coerce

-- | Indicates whether Capacity Rebalancing is enabled.
autoScalingGroup_capacityRebalance :: Lens.Lens' AutoScalingGroup (Core.Maybe Core.Bool)
autoScalingGroup_capacityRebalance = Lens.lens (\AutoScalingGroup' {capacityRebalance} -> capacityRebalance) (\s@AutoScalingGroup' {} a -> s {capacityRebalance = a} :: AutoScalingGroup)

-- | Indicates whether newly launched instances are protected from
-- termination by Amazon EC2 Auto Scaling when scaling in.
autoScalingGroup_newInstancesProtectedFromScaleIn :: Lens.Lens' AutoScalingGroup (Core.Maybe Core.Bool)
autoScalingGroup_newInstancesProtectedFromScaleIn = Lens.lens (\AutoScalingGroup' {newInstancesProtectedFromScaleIn'} -> newInstancesProtectedFromScaleIn') (\s@AutoScalingGroup' {} a -> s {newInstancesProtectedFromScaleIn' = a} :: AutoScalingGroup)

-- | The Amazon Resource Name (ARN) of the service-linked role that the Auto
-- Scaling group uses to call other AWS services on your behalf.
autoScalingGroup_serviceLinkedRoleARN :: Lens.Lens' AutoScalingGroup (Core.Maybe Core.Text)
autoScalingGroup_serviceLinkedRoleARN = Lens.lens (\AutoScalingGroup' {serviceLinkedRoleARN} -> serviceLinkedRoleARN) (\s@AutoScalingGroup' {} a -> s {serviceLinkedRoleARN = a} :: AutoScalingGroup)

-- | The amount of time, in seconds, that Amazon EC2 Auto Scaling waits
-- before checking the health status of an EC2 instance that has come into
-- service.
autoScalingGroup_healthCheckGracePeriod :: Lens.Lens' AutoScalingGroup (Core.Maybe Core.Int)
autoScalingGroup_healthCheckGracePeriod = Lens.lens (\AutoScalingGroup' {healthCheckGracePeriod} -> healthCheckGracePeriod) (\s@AutoScalingGroup' {} a -> s {healthCheckGracePeriod = a} :: AutoScalingGroup)

-- | The metrics enabled for the group.
autoScalingGroup_enabledMetrics :: Lens.Lens' AutoScalingGroup (Core.Maybe [EnabledMetric])
autoScalingGroup_enabledMetrics = Lens.lens (\AutoScalingGroup' {enabledMetrics} -> enabledMetrics) (\s@AutoScalingGroup' {} a -> s {enabledMetrics = a} :: AutoScalingGroup) Core.. Lens.mapping Lens._Coerce

-- | The termination policies for the group.
autoScalingGroup_terminationPolicies :: Lens.Lens' AutoScalingGroup (Core.Maybe [Core.Text])
autoScalingGroup_terminationPolicies = Lens.lens (\AutoScalingGroup' {terminationPolicies} -> terminationPolicies) (\s@AutoScalingGroup' {} a -> s {terminationPolicies = a} :: AutoScalingGroup) Core.. Lens.mapping Lens._Coerce

-- | The name of the Auto Scaling group.
autoScalingGroup_autoScalingGroupName :: Lens.Lens' AutoScalingGroup Core.Text
autoScalingGroup_autoScalingGroupName = Lens.lens (\AutoScalingGroup' {autoScalingGroupName} -> autoScalingGroupName) (\s@AutoScalingGroup' {} a -> s {autoScalingGroupName = a} :: AutoScalingGroup)

-- | The minimum size of the group.
autoScalingGroup_minSize :: Lens.Lens' AutoScalingGroup Core.Int
autoScalingGroup_minSize = Lens.lens (\AutoScalingGroup' {minSize} -> minSize) (\s@AutoScalingGroup' {} a -> s {minSize = a} :: AutoScalingGroup)

-- | The maximum size of the group.
autoScalingGroup_maxSize :: Lens.Lens' AutoScalingGroup Core.Int
autoScalingGroup_maxSize = Lens.lens (\AutoScalingGroup' {maxSize} -> maxSize) (\s@AutoScalingGroup' {} a -> s {maxSize = a} :: AutoScalingGroup)

-- | The desired size of the group.
autoScalingGroup_desiredCapacity :: Lens.Lens' AutoScalingGroup Core.Int
autoScalingGroup_desiredCapacity = Lens.lens (\AutoScalingGroup' {desiredCapacity} -> desiredCapacity) (\s@AutoScalingGroup' {} a -> s {desiredCapacity = a} :: AutoScalingGroup)

-- | The duration of the default cooldown period, in seconds.
autoScalingGroup_defaultCooldown :: Lens.Lens' AutoScalingGroup Core.Int
autoScalingGroup_defaultCooldown = Lens.lens (\AutoScalingGroup' {defaultCooldown} -> defaultCooldown) (\s@AutoScalingGroup' {} a -> s {defaultCooldown = a} :: AutoScalingGroup)

-- | One or more Availability Zones for the group.
autoScalingGroup_availabilityZones :: Lens.Lens' AutoScalingGroup [Core.Text]
autoScalingGroup_availabilityZones = Lens.lens (\AutoScalingGroup' {availabilityZones} -> availabilityZones) (\s@AutoScalingGroup' {} a -> s {availabilityZones = a} :: AutoScalingGroup) Core.. Lens._Coerce

-- | The service to use for the health checks. The valid values are @EC2@ and
-- @ELB@. If you configure an Auto Scaling group to use ELB health checks,
-- it considers the instance unhealthy if it fails either the EC2 status
-- checks or the load balancer health checks.
autoScalingGroup_healthCheckType :: Lens.Lens' AutoScalingGroup Core.Text
autoScalingGroup_healthCheckType = Lens.lens (\AutoScalingGroup' {healthCheckType} -> healthCheckType) (\s@AutoScalingGroup' {} a -> s {healthCheckType = a} :: AutoScalingGroup)

-- | The date and time the group was created.
autoScalingGroup_createdTime :: Lens.Lens' AutoScalingGroup Core.UTCTime
autoScalingGroup_createdTime = Lens.lens (\AutoScalingGroup' {createdTime} -> createdTime) (\s@AutoScalingGroup' {} a -> s {createdTime = a} :: AutoScalingGroup) Core.. Core._Time

instance Core.FromXML AutoScalingGroup where
  parseXML x =
    AutoScalingGroup'
      Core.<$> (x Core..@? "Status")
      Core.<*> (x Core..@? "PlacementGroup")
      Core.<*> ( x Core..@? "SuspendedProcesses" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "MaxInstanceLifetime")
      Core.<*> (x Core..@? "AutoScalingGroupARN")
      Core.<*> (x Core..@? "LaunchTemplate")
      Core.<*> ( x Core..@? "Instances" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "LaunchConfigurationName")
      Core.<*> (x Core..@? "MixedInstancesPolicy")
      Core.<*> ( x Core..@? "Tags" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> ( x Core..@? "LoadBalancerNames" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "VPCZoneIdentifier")
      Core.<*> ( x Core..@? "TargetGroupARNs" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "CapacityRebalance")
      Core.<*> (x Core..@? "NewInstancesProtectedFromScaleIn")
      Core.<*> (x Core..@? "ServiceLinkedRoleARN")
      Core.<*> (x Core..@? "HealthCheckGracePeriod")
      Core.<*> ( x Core..@? "EnabledMetrics" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> ( x Core..@? "TerminationPolicies"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@ "AutoScalingGroupName")
      Core.<*> (x Core..@ "MinSize")
      Core.<*> (x Core..@ "MaxSize")
      Core.<*> (x Core..@ "DesiredCapacity")
      Core.<*> (x Core..@ "DefaultCooldown")
      Core.<*> ( x Core..@? "AvailabilityZones" Core..!@ Core.mempty
                   Core.>>= Core.parseXMLList "member"
               )
      Core.<*> (x Core..@ "HealthCheckType")
      Core.<*> (x Core..@ "CreatedTime")

instance Core.Hashable AutoScalingGroup

instance Core.NFData AutoScalingGroup
