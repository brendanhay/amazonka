{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an Auto Scaling group.
--
-- /See:/ 'newAutoScalingGroup' smart constructor.
data AutoScalingGroup = AutoScalingGroup'
  { -- | The current state of the group when the DeleteAutoScalingGroup operation
    -- is in progress.
    status :: Prelude.Maybe Prelude.Text,
    -- | The name of the placement group into which to launch your instances, if
    -- any.
    placementGroup :: Prelude.Maybe Prelude.Text,
    -- | The suspended processes associated with the group.
    suspendedProcesses :: Prelude.Maybe [SuspendedProcess],
    -- | The maximum amount of time, in seconds, that an instance can be in
    -- service.
    --
    -- Valid Range: Minimum value of 0.
    maxInstanceLifetime :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the Auto Scaling group.
    autoScalingGroupARN :: Prelude.Maybe Prelude.Text,
    -- | The launch template for the group.
    launchTemplate :: Prelude.Maybe LaunchTemplateSpecification,
    -- | The EC2 instances associated with the group.
    instances :: Prelude.Maybe [Instance],
    -- | The name of the associated launch configuration.
    launchConfigurationName :: Prelude.Maybe Prelude.Text,
    -- | The mixed instances policy for the group.
    mixedInstancesPolicy :: Prelude.Maybe MixedInstancesPolicy,
    -- | The tags for the group.
    tags :: Prelude.Maybe [TagDescription],
    -- | One or more load balancers associated with the group.
    loadBalancerNames :: Prelude.Maybe [Prelude.Text],
    -- | One or more subnet IDs, if applicable, separated by commas.
    vPCZoneIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Names (ARN) of the target groups for your load
    -- balancer.
    targetGroupARNs :: Prelude.Maybe [Prelude.Text],
    -- | Indicates whether Capacity Rebalancing is enabled.
    capacityRebalance :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether newly launched instances are protected from
    -- termination by Amazon EC2 Auto Scaling when scaling in.
    newInstancesProtectedFromScaleIn' :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the service-linked role that the Auto
    -- Scaling group uses to call other AWS services on your behalf.
    serviceLinkedRoleARN :: Prelude.Maybe Prelude.Text,
    -- | The amount of time, in seconds, that Amazon EC2 Auto Scaling waits
    -- before checking the health status of an EC2 instance that has come into
    -- service.
    healthCheckGracePeriod :: Prelude.Maybe Prelude.Int,
    -- | The metrics enabled for the group.
    enabledMetrics :: Prelude.Maybe [EnabledMetric],
    -- | The termination policies for the group.
    terminationPolicies :: Prelude.Maybe [Prelude.Text],
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
    -- @ELB@. If you configure an Auto Scaling group to use ELB health checks,
    -- it considers the instance unhealthy if it fails either the EC2 status
    -- checks or the load balancer health checks.
    healthCheckType :: Prelude.Text,
    -- | The date and time the group was created.
    createdTime :: Prelude.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
      { status = Prelude.Nothing,
        placementGroup = Prelude.Nothing,
        suspendedProcesses = Prelude.Nothing,
        maxInstanceLifetime = Prelude.Nothing,
        autoScalingGroupARN = Prelude.Nothing,
        launchTemplate = Prelude.Nothing,
        instances = Prelude.Nothing,
        launchConfigurationName = Prelude.Nothing,
        mixedInstancesPolicy = Prelude.Nothing,
        tags = Prelude.Nothing,
        loadBalancerNames = Prelude.Nothing,
        vPCZoneIdentifier = Prelude.Nothing,
        targetGroupARNs = Prelude.Nothing,
        capacityRebalance = Prelude.Nothing,
        newInstancesProtectedFromScaleIn' = Prelude.Nothing,
        serviceLinkedRoleARN = Prelude.Nothing,
        healthCheckGracePeriod = Prelude.Nothing,
        enabledMetrics = Prelude.Nothing,
        terminationPolicies = Prelude.Nothing,
        autoScalingGroupName = pAutoScalingGroupName_,
        minSize = pMinSize_,
        maxSize = pMaxSize_,
        desiredCapacity = pDesiredCapacity_,
        defaultCooldown = pDefaultCooldown_,
        availabilityZones = Prelude.mempty,
        healthCheckType = pHealthCheckType_,
        createdTime = Prelude._Time Lens.# pCreatedTime_
      }

-- | The current state of the group when the DeleteAutoScalingGroup operation
-- is in progress.
autoScalingGroup_status :: Lens.Lens' AutoScalingGroup (Prelude.Maybe Prelude.Text)
autoScalingGroup_status = Lens.lens (\AutoScalingGroup' {status} -> status) (\s@AutoScalingGroup' {} a -> s {status = a} :: AutoScalingGroup)

-- | The name of the placement group into which to launch your instances, if
-- any.
autoScalingGroup_placementGroup :: Lens.Lens' AutoScalingGroup (Prelude.Maybe Prelude.Text)
autoScalingGroup_placementGroup = Lens.lens (\AutoScalingGroup' {placementGroup} -> placementGroup) (\s@AutoScalingGroup' {} a -> s {placementGroup = a} :: AutoScalingGroup)

-- | The suspended processes associated with the group.
autoScalingGroup_suspendedProcesses :: Lens.Lens' AutoScalingGroup (Prelude.Maybe [SuspendedProcess])
autoScalingGroup_suspendedProcesses = Lens.lens (\AutoScalingGroup' {suspendedProcesses} -> suspendedProcesses) (\s@AutoScalingGroup' {} a -> s {suspendedProcesses = a} :: AutoScalingGroup) Prelude.. Lens.mapping Prelude._Coerce

-- | The maximum amount of time, in seconds, that an instance can be in
-- service.
--
-- Valid Range: Minimum value of 0.
autoScalingGroup_maxInstanceLifetime :: Lens.Lens' AutoScalingGroup (Prelude.Maybe Prelude.Int)
autoScalingGroup_maxInstanceLifetime = Lens.lens (\AutoScalingGroup' {maxInstanceLifetime} -> maxInstanceLifetime) (\s@AutoScalingGroup' {} a -> s {maxInstanceLifetime = a} :: AutoScalingGroup)

-- | The Amazon Resource Name (ARN) of the Auto Scaling group.
autoScalingGroup_autoScalingGroupARN :: Lens.Lens' AutoScalingGroup (Prelude.Maybe Prelude.Text)
autoScalingGroup_autoScalingGroupARN = Lens.lens (\AutoScalingGroup' {autoScalingGroupARN} -> autoScalingGroupARN) (\s@AutoScalingGroup' {} a -> s {autoScalingGroupARN = a} :: AutoScalingGroup)

-- | The launch template for the group.
autoScalingGroup_launchTemplate :: Lens.Lens' AutoScalingGroup (Prelude.Maybe LaunchTemplateSpecification)
autoScalingGroup_launchTemplate = Lens.lens (\AutoScalingGroup' {launchTemplate} -> launchTemplate) (\s@AutoScalingGroup' {} a -> s {launchTemplate = a} :: AutoScalingGroup)

-- | The EC2 instances associated with the group.
autoScalingGroup_instances :: Lens.Lens' AutoScalingGroup (Prelude.Maybe [Instance])
autoScalingGroup_instances = Lens.lens (\AutoScalingGroup' {instances} -> instances) (\s@AutoScalingGroup' {} a -> s {instances = a} :: AutoScalingGroup) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the associated launch configuration.
autoScalingGroup_launchConfigurationName :: Lens.Lens' AutoScalingGroup (Prelude.Maybe Prelude.Text)
autoScalingGroup_launchConfigurationName = Lens.lens (\AutoScalingGroup' {launchConfigurationName} -> launchConfigurationName) (\s@AutoScalingGroup' {} a -> s {launchConfigurationName = a} :: AutoScalingGroup)

-- | The mixed instances policy for the group.
autoScalingGroup_mixedInstancesPolicy :: Lens.Lens' AutoScalingGroup (Prelude.Maybe MixedInstancesPolicy)
autoScalingGroup_mixedInstancesPolicy = Lens.lens (\AutoScalingGroup' {mixedInstancesPolicy} -> mixedInstancesPolicy) (\s@AutoScalingGroup' {} a -> s {mixedInstancesPolicy = a} :: AutoScalingGroup)

-- | The tags for the group.
autoScalingGroup_tags :: Lens.Lens' AutoScalingGroup (Prelude.Maybe [TagDescription])
autoScalingGroup_tags = Lens.lens (\AutoScalingGroup' {tags} -> tags) (\s@AutoScalingGroup' {} a -> s {tags = a} :: AutoScalingGroup) Prelude.. Lens.mapping Prelude._Coerce

-- | One or more load balancers associated with the group.
autoScalingGroup_loadBalancerNames :: Lens.Lens' AutoScalingGroup (Prelude.Maybe [Prelude.Text])
autoScalingGroup_loadBalancerNames = Lens.lens (\AutoScalingGroup' {loadBalancerNames} -> loadBalancerNames) (\s@AutoScalingGroup' {} a -> s {loadBalancerNames = a} :: AutoScalingGroup) Prelude.. Lens.mapping Prelude._Coerce

-- | One or more subnet IDs, if applicable, separated by commas.
autoScalingGroup_vPCZoneIdentifier :: Lens.Lens' AutoScalingGroup (Prelude.Maybe Prelude.Text)
autoScalingGroup_vPCZoneIdentifier = Lens.lens (\AutoScalingGroup' {vPCZoneIdentifier} -> vPCZoneIdentifier) (\s@AutoScalingGroup' {} a -> s {vPCZoneIdentifier = a} :: AutoScalingGroup)

-- | The Amazon Resource Names (ARN) of the target groups for your load
-- balancer.
autoScalingGroup_targetGroupARNs :: Lens.Lens' AutoScalingGroup (Prelude.Maybe [Prelude.Text])
autoScalingGroup_targetGroupARNs = Lens.lens (\AutoScalingGroup' {targetGroupARNs} -> targetGroupARNs) (\s@AutoScalingGroup' {} a -> s {targetGroupARNs = a} :: AutoScalingGroup) Prelude.. Lens.mapping Prelude._Coerce

-- | Indicates whether Capacity Rebalancing is enabled.
autoScalingGroup_capacityRebalance :: Lens.Lens' AutoScalingGroup (Prelude.Maybe Prelude.Bool)
autoScalingGroup_capacityRebalance = Lens.lens (\AutoScalingGroup' {capacityRebalance} -> capacityRebalance) (\s@AutoScalingGroup' {} a -> s {capacityRebalance = a} :: AutoScalingGroup)

-- | Indicates whether newly launched instances are protected from
-- termination by Amazon EC2 Auto Scaling when scaling in.
autoScalingGroup_newInstancesProtectedFromScaleIn :: Lens.Lens' AutoScalingGroup (Prelude.Maybe Prelude.Bool)
autoScalingGroup_newInstancesProtectedFromScaleIn = Lens.lens (\AutoScalingGroup' {newInstancesProtectedFromScaleIn'} -> newInstancesProtectedFromScaleIn') (\s@AutoScalingGroup' {} a -> s {newInstancesProtectedFromScaleIn' = a} :: AutoScalingGroup)

-- | The Amazon Resource Name (ARN) of the service-linked role that the Auto
-- Scaling group uses to call other AWS services on your behalf.
autoScalingGroup_serviceLinkedRoleARN :: Lens.Lens' AutoScalingGroup (Prelude.Maybe Prelude.Text)
autoScalingGroup_serviceLinkedRoleARN = Lens.lens (\AutoScalingGroup' {serviceLinkedRoleARN} -> serviceLinkedRoleARN) (\s@AutoScalingGroup' {} a -> s {serviceLinkedRoleARN = a} :: AutoScalingGroup)

-- | The amount of time, in seconds, that Amazon EC2 Auto Scaling waits
-- before checking the health status of an EC2 instance that has come into
-- service.
autoScalingGroup_healthCheckGracePeriod :: Lens.Lens' AutoScalingGroup (Prelude.Maybe Prelude.Int)
autoScalingGroup_healthCheckGracePeriod = Lens.lens (\AutoScalingGroup' {healthCheckGracePeriod} -> healthCheckGracePeriod) (\s@AutoScalingGroup' {} a -> s {healthCheckGracePeriod = a} :: AutoScalingGroup)

-- | The metrics enabled for the group.
autoScalingGroup_enabledMetrics :: Lens.Lens' AutoScalingGroup (Prelude.Maybe [EnabledMetric])
autoScalingGroup_enabledMetrics = Lens.lens (\AutoScalingGroup' {enabledMetrics} -> enabledMetrics) (\s@AutoScalingGroup' {} a -> s {enabledMetrics = a} :: AutoScalingGroup) Prelude.. Lens.mapping Prelude._Coerce

-- | The termination policies for the group.
autoScalingGroup_terminationPolicies :: Lens.Lens' AutoScalingGroup (Prelude.Maybe [Prelude.Text])
autoScalingGroup_terminationPolicies = Lens.lens (\AutoScalingGroup' {terminationPolicies} -> terminationPolicies) (\s@AutoScalingGroup' {} a -> s {terminationPolicies = a} :: AutoScalingGroup) Prelude.. Lens.mapping Prelude._Coerce

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
autoScalingGroup_availabilityZones = Lens.lens (\AutoScalingGroup' {availabilityZones} -> availabilityZones) (\s@AutoScalingGroup' {} a -> s {availabilityZones = a} :: AutoScalingGroup) Prelude.. Prelude._Coerce

-- | The service to use for the health checks. The valid values are @EC2@ and
-- @ELB@. If you configure an Auto Scaling group to use ELB health checks,
-- it considers the instance unhealthy if it fails either the EC2 status
-- checks or the load balancer health checks.
autoScalingGroup_healthCheckType :: Lens.Lens' AutoScalingGroup Prelude.Text
autoScalingGroup_healthCheckType = Lens.lens (\AutoScalingGroup' {healthCheckType} -> healthCheckType) (\s@AutoScalingGroup' {} a -> s {healthCheckType = a} :: AutoScalingGroup)

-- | The date and time the group was created.
autoScalingGroup_createdTime :: Lens.Lens' AutoScalingGroup Prelude.UTCTime
autoScalingGroup_createdTime = Lens.lens (\AutoScalingGroup' {createdTime} -> createdTime) (\s@AutoScalingGroup' {} a -> s {createdTime = a} :: AutoScalingGroup) Prelude.. Prelude._Time

instance Prelude.FromXML AutoScalingGroup where
  parseXML x =
    AutoScalingGroup'
      Prelude.<$> (x Prelude..@? "Status")
      Prelude.<*> (x Prelude..@? "PlacementGroup")
      Prelude.<*> ( x Prelude..@? "SuspendedProcesses"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )
      Prelude.<*> (x Prelude..@? "MaxInstanceLifetime")
      Prelude.<*> (x Prelude..@? "AutoScalingGroupARN")
      Prelude.<*> (x Prelude..@? "LaunchTemplate")
      Prelude.<*> ( x Prelude..@? "Instances" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )
      Prelude.<*> (x Prelude..@? "LaunchConfigurationName")
      Prelude.<*> (x Prelude..@? "MixedInstancesPolicy")
      Prelude.<*> ( x Prelude..@? "Tags" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )
      Prelude.<*> ( x Prelude..@? "LoadBalancerNames"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )
      Prelude.<*> (x Prelude..@? "VPCZoneIdentifier")
      Prelude.<*> ( x Prelude..@? "TargetGroupARNs"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )
      Prelude.<*> (x Prelude..@? "CapacityRebalance")
      Prelude.<*> (x Prelude..@? "NewInstancesProtectedFromScaleIn")
      Prelude.<*> (x Prelude..@? "ServiceLinkedRoleARN")
      Prelude.<*> (x Prelude..@? "HealthCheckGracePeriod")
      Prelude.<*> ( x Prelude..@? "EnabledMetrics"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )
      Prelude.<*> ( x Prelude..@? "TerminationPolicies"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )
      Prelude.<*> (x Prelude..@ "AutoScalingGroupName")
      Prelude.<*> (x Prelude..@ "MinSize")
      Prelude.<*> (x Prelude..@ "MaxSize")
      Prelude.<*> (x Prelude..@ "DesiredCapacity")
      Prelude.<*> (x Prelude..@ "DefaultCooldown")
      Prelude.<*> ( x Prelude..@? "AvailabilityZones"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.parseXMLList "member"
                  )
      Prelude.<*> (x Prelude..@ "HealthCheckType")
      Prelude.<*> (x Prelude..@ "CreatedTime")

instance Prelude.Hashable AutoScalingGroup

instance Prelude.NFData AutoScalingGroup
