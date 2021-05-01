{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.UpdateAutoScalingGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __We strongly recommend that all Auto Scaling groups use launch
-- templates to ensure full functionality for Amazon EC2 Auto Scaling and
-- Amazon EC2.__
--
-- Updates the configuration for the specified Auto Scaling group.
--
-- To update an Auto Scaling group, specify the name of the group and the
-- parameter that you want to change. Any parameters that you don\'t
-- specify are not changed by this update request. The new settings take
-- effect on any scaling activities after this call returns.
--
-- If you associate a new launch configuration or template with an Auto
-- Scaling group, all new instances will get the updated configuration.
-- Existing instances continue to run with the configuration that they were
-- originally launched with. When you update a group to specify a mixed
-- instances policy instead of a launch configuration or template, existing
-- instances may be replaced to match the new purchasing options that you
-- specified in the policy. For example, if the group currently has 100%
-- On-Demand capacity and the policy specifies 50% Spot capacity, this
-- means that half of your instances will be gradually terminated and
-- relaunched as Spot Instances. When replacing instances, Amazon EC2 Auto
-- Scaling launches new instances before terminating the old ones, so that
-- updating your group does not compromise the performance or availability
-- of your application.
--
-- Note the following about changing @DesiredCapacity@, @MaxSize@, or
-- @MinSize@:
--
-- -   If a scale-in activity occurs as a result of a new @DesiredCapacity@
--     value that is lower than the current size of the group, the Auto
--     Scaling group uses its termination policy to determine which
--     instances to terminate.
--
-- -   If you specify a new value for @MinSize@ without specifying a value
--     for @DesiredCapacity@, and the new @MinSize@ is larger than the
--     current size of the group, this sets the group\'s @DesiredCapacity@
--     to the new @MinSize@ value.
--
-- -   If you specify a new value for @MaxSize@ without specifying a value
--     for @DesiredCapacity@, and the new @MaxSize@ is smaller than the
--     current size of the group, this sets the group\'s @DesiredCapacity@
--     to the new @MaxSize@ value.
--
-- To see which parameters have been set, call the
-- DescribeAutoScalingGroups API. To view the scaling policies for an Auto
-- Scaling group, call the DescribePolicies API. If the group has scaling
-- policies, you can update them by calling the PutScalingPolicy API.
module Network.AWS.AutoScaling.UpdateAutoScalingGroup
  ( -- * Creating a Request
    UpdateAutoScalingGroup (..),
    newUpdateAutoScalingGroup,

    -- * Request Lenses
    updateAutoScalingGroup_minSize,
    updateAutoScalingGroup_desiredCapacity,
    updateAutoScalingGroup_availabilityZones,
    updateAutoScalingGroup_placementGroup,
    updateAutoScalingGroup_defaultCooldown,
    updateAutoScalingGroup_maxInstanceLifetime,
    updateAutoScalingGroup_launchTemplate,
    updateAutoScalingGroup_launchConfigurationName,
    updateAutoScalingGroup_healthCheckType,
    updateAutoScalingGroup_mixedInstancesPolicy,
    updateAutoScalingGroup_maxSize,
    updateAutoScalingGroup_vPCZoneIdentifier,
    updateAutoScalingGroup_capacityRebalance,
    updateAutoScalingGroup_newInstancesProtectedFromScaleIn,
    updateAutoScalingGroup_serviceLinkedRoleARN,
    updateAutoScalingGroup_healthCheckGracePeriod,
    updateAutoScalingGroup_terminationPolicies,
    updateAutoScalingGroup_autoScalingGroupName,

    -- * Destructuring the Response
    UpdateAutoScalingGroupResponse (..),
    newUpdateAutoScalingGroupResponse,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateAutoScalingGroup' smart constructor.
data UpdateAutoScalingGroup = UpdateAutoScalingGroup'
  { -- | The minimum size of the Auto Scaling group.
    minSize :: Prelude.Maybe Prelude.Int,
    -- | The desired capacity is the initial capacity of the Auto Scaling group
    -- after this operation completes and the capacity it attempts to maintain.
    -- This number must be greater than or equal to the minimum size of the
    -- group and less than or equal to the maximum size of the group.
    desiredCapacity :: Prelude.Maybe Prelude.Int,
    -- | One or more Availability Zones for the group.
    availabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | The name of an existing placement group into which to launch your
    -- instances, if any. A placement group is a logical grouping of instances
    -- within a single Availability Zone. You cannot specify multiple
    -- Availability Zones and a placement group. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement Groups>
    -- in the /Amazon EC2 User Guide for Linux Instances/.
    placementGroup :: Prelude.Maybe Prelude.Text,
    -- | The amount of time, in seconds, after a scaling activity completes
    -- before another scaling activity can start. The default value is @300@.
    -- This setting applies when using simple scaling policies, but not when
    -- using other scaling policies or scheduled scaling. For more information,
    -- see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/Cooldown.html Scaling cooldowns for Amazon EC2 Auto Scaling>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    defaultCooldown :: Prelude.Maybe Prelude.Int,
    -- | The maximum amount of time, in seconds, that an instance can be in
    -- service. The default is null. If specified, the value must be either 0
    -- or a number equal to or greater than 86,400 seconds (1 day). To clear a
    -- previously set value, specify a new value of 0. For more information,
    -- see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-max-instance-lifetime.html Replacing Auto Scaling instances based on maximum instance lifetime>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    maxInstanceLifetime :: Prelude.Maybe Prelude.Int,
    -- | The launch template and version to use to specify the updates. If you
    -- specify @LaunchTemplate@ in your update request, you can\'t specify
    -- @LaunchConfigurationName@ or @MixedInstancesPolicy@.
    launchTemplate :: Prelude.Maybe LaunchTemplateSpecification,
    -- | The name of the launch configuration. If you specify
    -- @LaunchConfigurationName@ in your update request, you can\'t specify
    -- @LaunchTemplate@ or @MixedInstancesPolicy@.
    launchConfigurationName :: Prelude.Maybe Prelude.Text,
    -- | The service to use for the health checks. The valid values are @EC2@ and
    -- @ELB@. If you configure an Auto Scaling group to use ELB health checks,
    -- it considers the instance unhealthy if it fails either the EC2 status
    -- checks or the load balancer health checks.
    healthCheckType :: Prelude.Maybe Prelude.Text,
    -- | An embedded object that specifies a mixed instances policy. When you
    -- make changes to an existing policy, all optional parameters are left
    -- unchanged if not specified. For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-purchase-options.html Auto Scaling groups with multiple instance types and purchase options>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    mixedInstancesPolicy :: Prelude.Maybe MixedInstancesPolicy,
    -- | The maximum size of the Auto Scaling group.
    --
    -- With a mixed instances policy that uses instance weighting, Amazon EC2
    -- Auto Scaling may need to go above @MaxSize@ to meet your capacity
    -- requirements. In this event, Amazon EC2 Auto Scaling will never go above
    -- @MaxSize@ by more than your largest instance weight (weights that define
    -- how many units each instance contributes to the desired capacity of the
    -- group).
    maxSize :: Prelude.Maybe Prelude.Int,
    -- | A comma-separated list of subnet IDs for a virtual private cloud (VPC).
    -- If you specify @VPCZoneIdentifier@ with @AvailabilityZones@, the subnets
    -- that you specify for this parameter must reside in those Availability
    -- Zones.
    vPCZoneIdentifier :: Prelude.Maybe Prelude.Text,
    -- | Enables or disables Capacity Rebalancing. For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/capacity-rebalance.html Amazon EC2 Auto Scaling Capacity Rebalancing>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    capacityRebalance :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether newly launched instances are protected from
    -- termination by Amazon EC2 Auto Scaling when scaling in. For more
    -- information about preventing instances from terminating on scale in, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html#instance-protection Instance scale-in protection>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    newInstancesProtectedFromScaleIn' :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the service-linked role that the Auto
    -- Scaling group uses to call other AWS services on your behalf. For more
    -- information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-service-linked-role.html Service-linked roles>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    serviceLinkedRoleARN :: Prelude.Maybe Prelude.Text,
    -- | The amount of time, in seconds, that Amazon EC2 Auto Scaling waits
    -- before checking the health status of an EC2 instance that has come into
    -- service. The default value is @0@. For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/healthcheck.html#health-check-grace-period Health check grace period>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    --
    -- Conditional: Required if you are adding an @ELB@ health check.
    healthCheckGracePeriod :: Prelude.Maybe Prelude.Int,
    -- | A policy or a list of policies that are used to select the instances to
    -- terminate. The policies are executed in the order that you list them.
    -- For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html Controlling which Auto Scaling instances terminate during scale in>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    terminationPolicies :: Prelude.Maybe [Prelude.Text],
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateAutoScalingGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'minSize', 'updateAutoScalingGroup_minSize' - The minimum size of the Auto Scaling group.
--
-- 'desiredCapacity', 'updateAutoScalingGroup_desiredCapacity' - The desired capacity is the initial capacity of the Auto Scaling group
-- after this operation completes and the capacity it attempts to maintain.
-- This number must be greater than or equal to the minimum size of the
-- group and less than or equal to the maximum size of the group.
--
-- 'availabilityZones', 'updateAutoScalingGroup_availabilityZones' - One or more Availability Zones for the group.
--
-- 'placementGroup', 'updateAutoScalingGroup_placementGroup' - The name of an existing placement group into which to launch your
-- instances, if any. A placement group is a logical grouping of instances
-- within a single Availability Zone. You cannot specify multiple
-- Availability Zones and a placement group. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement Groups>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- 'defaultCooldown', 'updateAutoScalingGroup_defaultCooldown' - The amount of time, in seconds, after a scaling activity completes
-- before another scaling activity can start. The default value is @300@.
-- This setting applies when using simple scaling policies, but not when
-- using other scaling policies or scheduled scaling. For more information,
-- see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/Cooldown.html Scaling cooldowns for Amazon EC2 Auto Scaling>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- 'maxInstanceLifetime', 'updateAutoScalingGroup_maxInstanceLifetime' - The maximum amount of time, in seconds, that an instance can be in
-- service. The default is null. If specified, the value must be either 0
-- or a number equal to or greater than 86,400 seconds (1 day). To clear a
-- previously set value, specify a new value of 0. For more information,
-- see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-max-instance-lifetime.html Replacing Auto Scaling instances based on maximum instance lifetime>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- 'launchTemplate', 'updateAutoScalingGroup_launchTemplate' - The launch template and version to use to specify the updates. If you
-- specify @LaunchTemplate@ in your update request, you can\'t specify
-- @LaunchConfigurationName@ or @MixedInstancesPolicy@.
--
-- 'launchConfigurationName', 'updateAutoScalingGroup_launchConfigurationName' - The name of the launch configuration. If you specify
-- @LaunchConfigurationName@ in your update request, you can\'t specify
-- @LaunchTemplate@ or @MixedInstancesPolicy@.
--
-- 'healthCheckType', 'updateAutoScalingGroup_healthCheckType' - The service to use for the health checks. The valid values are @EC2@ and
-- @ELB@. If you configure an Auto Scaling group to use ELB health checks,
-- it considers the instance unhealthy if it fails either the EC2 status
-- checks or the load balancer health checks.
--
-- 'mixedInstancesPolicy', 'updateAutoScalingGroup_mixedInstancesPolicy' - An embedded object that specifies a mixed instances policy. When you
-- make changes to an existing policy, all optional parameters are left
-- unchanged if not specified. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-purchase-options.html Auto Scaling groups with multiple instance types and purchase options>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- 'maxSize', 'updateAutoScalingGroup_maxSize' - The maximum size of the Auto Scaling group.
--
-- With a mixed instances policy that uses instance weighting, Amazon EC2
-- Auto Scaling may need to go above @MaxSize@ to meet your capacity
-- requirements. In this event, Amazon EC2 Auto Scaling will never go above
-- @MaxSize@ by more than your largest instance weight (weights that define
-- how many units each instance contributes to the desired capacity of the
-- group).
--
-- 'vPCZoneIdentifier', 'updateAutoScalingGroup_vPCZoneIdentifier' - A comma-separated list of subnet IDs for a virtual private cloud (VPC).
-- If you specify @VPCZoneIdentifier@ with @AvailabilityZones@, the subnets
-- that you specify for this parameter must reside in those Availability
-- Zones.
--
-- 'capacityRebalance', 'updateAutoScalingGroup_capacityRebalance' - Enables or disables Capacity Rebalancing. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/capacity-rebalance.html Amazon EC2 Auto Scaling Capacity Rebalancing>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- 'newInstancesProtectedFromScaleIn'', 'updateAutoScalingGroup_newInstancesProtectedFromScaleIn' - Indicates whether newly launched instances are protected from
-- termination by Amazon EC2 Auto Scaling when scaling in. For more
-- information about preventing instances from terminating on scale in, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html#instance-protection Instance scale-in protection>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- 'serviceLinkedRoleARN', 'updateAutoScalingGroup_serviceLinkedRoleARN' - The Amazon Resource Name (ARN) of the service-linked role that the Auto
-- Scaling group uses to call other AWS services on your behalf. For more
-- information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-service-linked-role.html Service-linked roles>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- 'healthCheckGracePeriod', 'updateAutoScalingGroup_healthCheckGracePeriod' - The amount of time, in seconds, that Amazon EC2 Auto Scaling waits
-- before checking the health status of an EC2 instance that has come into
-- service. The default value is @0@. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/healthcheck.html#health-check-grace-period Health check grace period>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- Conditional: Required if you are adding an @ELB@ health check.
--
-- 'terminationPolicies', 'updateAutoScalingGroup_terminationPolicies' - A policy or a list of policies that are used to select the instances to
-- terminate. The policies are executed in the order that you list them.
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html Controlling which Auto Scaling instances terminate during scale in>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- 'autoScalingGroupName', 'updateAutoScalingGroup_autoScalingGroupName' - The name of the Auto Scaling group.
newUpdateAutoScalingGroup ::
  -- | 'autoScalingGroupName'
  Prelude.Text ->
  UpdateAutoScalingGroup
newUpdateAutoScalingGroup pAutoScalingGroupName_ =
  UpdateAutoScalingGroup'
    { minSize = Prelude.Nothing,
      desiredCapacity = Prelude.Nothing,
      availabilityZones = Prelude.Nothing,
      placementGroup = Prelude.Nothing,
      defaultCooldown = Prelude.Nothing,
      maxInstanceLifetime = Prelude.Nothing,
      launchTemplate = Prelude.Nothing,
      launchConfigurationName = Prelude.Nothing,
      healthCheckType = Prelude.Nothing,
      mixedInstancesPolicy = Prelude.Nothing,
      maxSize = Prelude.Nothing,
      vPCZoneIdentifier = Prelude.Nothing,
      capacityRebalance = Prelude.Nothing,
      newInstancesProtectedFromScaleIn' = Prelude.Nothing,
      serviceLinkedRoleARN = Prelude.Nothing,
      healthCheckGracePeriod = Prelude.Nothing,
      terminationPolicies = Prelude.Nothing,
      autoScalingGroupName = pAutoScalingGroupName_
    }

-- | The minimum size of the Auto Scaling group.
updateAutoScalingGroup_minSize :: Lens.Lens' UpdateAutoScalingGroup (Prelude.Maybe Prelude.Int)
updateAutoScalingGroup_minSize = Lens.lens (\UpdateAutoScalingGroup' {minSize} -> minSize) (\s@UpdateAutoScalingGroup' {} a -> s {minSize = a} :: UpdateAutoScalingGroup)

-- | The desired capacity is the initial capacity of the Auto Scaling group
-- after this operation completes and the capacity it attempts to maintain.
-- This number must be greater than or equal to the minimum size of the
-- group and less than or equal to the maximum size of the group.
updateAutoScalingGroup_desiredCapacity :: Lens.Lens' UpdateAutoScalingGroup (Prelude.Maybe Prelude.Int)
updateAutoScalingGroup_desiredCapacity = Lens.lens (\UpdateAutoScalingGroup' {desiredCapacity} -> desiredCapacity) (\s@UpdateAutoScalingGroup' {} a -> s {desiredCapacity = a} :: UpdateAutoScalingGroup)

-- | One or more Availability Zones for the group.
updateAutoScalingGroup_availabilityZones :: Lens.Lens' UpdateAutoScalingGroup (Prelude.Maybe [Prelude.Text])
updateAutoScalingGroup_availabilityZones = Lens.lens (\UpdateAutoScalingGroup' {availabilityZones} -> availabilityZones) (\s@UpdateAutoScalingGroup' {} a -> s {availabilityZones = a} :: UpdateAutoScalingGroup) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of an existing placement group into which to launch your
-- instances, if any. A placement group is a logical grouping of instances
-- within a single Availability Zone. You cannot specify multiple
-- Availability Zones and a placement group. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement Groups>
-- in the /Amazon EC2 User Guide for Linux Instances/.
updateAutoScalingGroup_placementGroup :: Lens.Lens' UpdateAutoScalingGroup (Prelude.Maybe Prelude.Text)
updateAutoScalingGroup_placementGroup = Lens.lens (\UpdateAutoScalingGroup' {placementGroup} -> placementGroup) (\s@UpdateAutoScalingGroup' {} a -> s {placementGroup = a} :: UpdateAutoScalingGroup)

-- | The amount of time, in seconds, after a scaling activity completes
-- before another scaling activity can start. The default value is @300@.
-- This setting applies when using simple scaling policies, but not when
-- using other scaling policies or scheduled scaling. For more information,
-- see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/Cooldown.html Scaling cooldowns for Amazon EC2 Auto Scaling>
-- in the /Amazon EC2 Auto Scaling User Guide/.
updateAutoScalingGroup_defaultCooldown :: Lens.Lens' UpdateAutoScalingGroup (Prelude.Maybe Prelude.Int)
updateAutoScalingGroup_defaultCooldown = Lens.lens (\UpdateAutoScalingGroup' {defaultCooldown} -> defaultCooldown) (\s@UpdateAutoScalingGroup' {} a -> s {defaultCooldown = a} :: UpdateAutoScalingGroup)

-- | The maximum amount of time, in seconds, that an instance can be in
-- service. The default is null. If specified, the value must be either 0
-- or a number equal to or greater than 86,400 seconds (1 day). To clear a
-- previously set value, specify a new value of 0. For more information,
-- see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-max-instance-lifetime.html Replacing Auto Scaling instances based on maximum instance lifetime>
-- in the /Amazon EC2 Auto Scaling User Guide/.
updateAutoScalingGroup_maxInstanceLifetime :: Lens.Lens' UpdateAutoScalingGroup (Prelude.Maybe Prelude.Int)
updateAutoScalingGroup_maxInstanceLifetime = Lens.lens (\UpdateAutoScalingGroup' {maxInstanceLifetime} -> maxInstanceLifetime) (\s@UpdateAutoScalingGroup' {} a -> s {maxInstanceLifetime = a} :: UpdateAutoScalingGroup)

-- | The launch template and version to use to specify the updates. If you
-- specify @LaunchTemplate@ in your update request, you can\'t specify
-- @LaunchConfigurationName@ or @MixedInstancesPolicy@.
updateAutoScalingGroup_launchTemplate :: Lens.Lens' UpdateAutoScalingGroup (Prelude.Maybe LaunchTemplateSpecification)
updateAutoScalingGroup_launchTemplate = Lens.lens (\UpdateAutoScalingGroup' {launchTemplate} -> launchTemplate) (\s@UpdateAutoScalingGroup' {} a -> s {launchTemplate = a} :: UpdateAutoScalingGroup)

-- | The name of the launch configuration. If you specify
-- @LaunchConfigurationName@ in your update request, you can\'t specify
-- @LaunchTemplate@ or @MixedInstancesPolicy@.
updateAutoScalingGroup_launchConfigurationName :: Lens.Lens' UpdateAutoScalingGroup (Prelude.Maybe Prelude.Text)
updateAutoScalingGroup_launchConfigurationName = Lens.lens (\UpdateAutoScalingGroup' {launchConfigurationName} -> launchConfigurationName) (\s@UpdateAutoScalingGroup' {} a -> s {launchConfigurationName = a} :: UpdateAutoScalingGroup)

-- | The service to use for the health checks. The valid values are @EC2@ and
-- @ELB@. If you configure an Auto Scaling group to use ELB health checks,
-- it considers the instance unhealthy if it fails either the EC2 status
-- checks or the load balancer health checks.
updateAutoScalingGroup_healthCheckType :: Lens.Lens' UpdateAutoScalingGroup (Prelude.Maybe Prelude.Text)
updateAutoScalingGroup_healthCheckType = Lens.lens (\UpdateAutoScalingGroup' {healthCheckType} -> healthCheckType) (\s@UpdateAutoScalingGroup' {} a -> s {healthCheckType = a} :: UpdateAutoScalingGroup)

-- | An embedded object that specifies a mixed instances policy. When you
-- make changes to an existing policy, all optional parameters are left
-- unchanged if not specified. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-purchase-options.html Auto Scaling groups with multiple instance types and purchase options>
-- in the /Amazon EC2 Auto Scaling User Guide/.
updateAutoScalingGroup_mixedInstancesPolicy :: Lens.Lens' UpdateAutoScalingGroup (Prelude.Maybe MixedInstancesPolicy)
updateAutoScalingGroup_mixedInstancesPolicy = Lens.lens (\UpdateAutoScalingGroup' {mixedInstancesPolicy} -> mixedInstancesPolicy) (\s@UpdateAutoScalingGroup' {} a -> s {mixedInstancesPolicy = a} :: UpdateAutoScalingGroup)

-- | The maximum size of the Auto Scaling group.
--
-- With a mixed instances policy that uses instance weighting, Amazon EC2
-- Auto Scaling may need to go above @MaxSize@ to meet your capacity
-- requirements. In this event, Amazon EC2 Auto Scaling will never go above
-- @MaxSize@ by more than your largest instance weight (weights that define
-- how many units each instance contributes to the desired capacity of the
-- group).
updateAutoScalingGroup_maxSize :: Lens.Lens' UpdateAutoScalingGroup (Prelude.Maybe Prelude.Int)
updateAutoScalingGroup_maxSize = Lens.lens (\UpdateAutoScalingGroup' {maxSize} -> maxSize) (\s@UpdateAutoScalingGroup' {} a -> s {maxSize = a} :: UpdateAutoScalingGroup)

-- | A comma-separated list of subnet IDs for a virtual private cloud (VPC).
-- If you specify @VPCZoneIdentifier@ with @AvailabilityZones@, the subnets
-- that you specify for this parameter must reside in those Availability
-- Zones.
updateAutoScalingGroup_vPCZoneIdentifier :: Lens.Lens' UpdateAutoScalingGroup (Prelude.Maybe Prelude.Text)
updateAutoScalingGroup_vPCZoneIdentifier = Lens.lens (\UpdateAutoScalingGroup' {vPCZoneIdentifier} -> vPCZoneIdentifier) (\s@UpdateAutoScalingGroup' {} a -> s {vPCZoneIdentifier = a} :: UpdateAutoScalingGroup)

-- | Enables or disables Capacity Rebalancing. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/capacity-rebalance.html Amazon EC2 Auto Scaling Capacity Rebalancing>
-- in the /Amazon EC2 Auto Scaling User Guide/.
updateAutoScalingGroup_capacityRebalance :: Lens.Lens' UpdateAutoScalingGroup (Prelude.Maybe Prelude.Bool)
updateAutoScalingGroup_capacityRebalance = Lens.lens (\UpdateAutoScalingGroup' {capacityRebalance} -> capacityRebalance) (\s@UpdateAutoScalingGroup' {} a -> s {capacityRebalance = a} :: UpdateAutoScalingGroup)

-- | Indicates whether newly launched instances are protected from
-- termination by Amazon EC2 Auto Scaling when scaling in. For more
-- information about preventing instances from terminating on scale in, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html#instance-protection Instance scale-in protection>
-- in the /Amazon EC2 Auto Scaling User Guide/.
updateAutoScalingGroup_newInstancesProtectedFromScaleIn :: Lens.Lens' UpdateAutoScalingGroup (Prelude.Maybe Prelude.Bool)
updateAutoScalingGroup_newInstancesProtectedFromScaleIn = Lens.lens (\UpdateAutoScalingGroup' {newInstancesProtectedFromScaleIn'} -> newInstancesProtectedFromScaleIn') (\s@UpdateAutoScalingGroup' {} a -> s {newInstancesProtectedFromScaleIn' = a} :: UpdateAutoScalingGroup)

-- | The Amazon Resource Name (ARN) of the service-linked role that the Auto
-- Scaling group uses to call other AWS services on your behalf. For more
-- information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-service-linked-role.html Service-linked roles>
-- in the /Amazon EC2 Auto Scaling User Guide/.
updateAutoScalingGroup_serviceLinkedRoleARN :: Lens.Lens' UpdateAutoScalingGroup (Prelude.Maybe Prelude.Text)
updateAutoScalingGroup_serviceLinkedRoleARN = Lens.lens (\UpdateAutoScalingGroup' {serviceLinkedRoleARN} -> serviceLinkedRoleARN) (\s@UpdateAutoScalingGroup' {} a -> s {serviceLinkedRoleARN = a} :: UpdateAutoScalingGroup)

-- | The amount of time, in seconds, that Amazon EC2 Auto Scaling waits
-- before checking the health status of an EC2 instance that has come into
-- service. The default value is @0@. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/healthcheck.html#health-check-grace-period Health check grace period>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- Conditional: Required if you are adding an @ELB@ health check.
updateAutoScalingGroup_healthCheckGracePeriod :: Lens.Lens' UpdateAutoScalingGroup (Prelude.Maybe Prelude.Int)
updateAutoScalingGroup_healthCheckGracePeriod = Lens.lens (\UpdateAutoScalingGroup' {healthCheckGracePeriod} -> healthCheckGracePeriod) (\s@UpdateAutoScalingGroup' {} a -> s {healthCheckGracePeriod = a} :: UpdateAutoScalingGroup)

-- | A policy or a list of policies that are used to select the instances to
-- terminate. The policies are executed in the order that you list them.
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html Controlling which Auto Scaling instances terminate during scale in>
-- in the /Amazon EC2 Auto Scaling User Guide/.
updateAutoScalingGroup_terminationPolicies :: Lens.Lens' UpdateAutoScalingGroup (Prelude.Maybe [Prelude.Text])
updateAutoScalingGroup_terminationPolicies = Lens.lens (\UpdateAutoScalingGroup' {terminationPolicies} -> terminationPolicies) (\s@UpdateAutoScalingGroup' {} a -> s {terminationPolicies = a} :: UpdateAutoScalingGroup) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the Auto Scaling group.
updateAutoScalingGroup_autoScalingGroupName :: Lens.Lens' UpdateAutoScalingGroup Prelude.Text
updateAutoScalingGroup_autoScalingGroupName = Lens.lens (\UpdateAutoScalingGroup' {autoScalingGroupName} -> autoScalingGroupName) (\s@UpdateAutoScalingGroup' {} a -> s {autoScalingGroupName = a} :: UpdateAutoScalingGroup)

instance Prelude.AWSRequest UpdateAutoScalingGroup where
  type
    Rs UpdateAutoScalingGroup =
      UpdateAutoScalingGroupResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      UpdateAutoScalingGroupResponse'

instance Prelude.Hashable UpdateAutoScalingGroup

instance Prelude.NFData UpdateAutoScalingGroup

instance Prelude.ToHeaders UpdateAutoScalingGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath UpdateAutoScalingGroup where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateAutoScalingGroup where
  toQuery UpdateAutoScalingGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("UpdateAutoScalingGroup" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2011-01-01" :: Prelude.ByteString),
        "MinSize" Prelude.=: minSize,
        "DesiredCapacity" Prelude.=: desiredCapacity,
        "AvailabilityZones"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "member"
                Prelude.<$> availabilityZones
            ),
        "PlacementGroup" Prelude.=: placementGroup,
        "DefaultCooldown" Prelude.=: defaultCooldown,
        "MaxInstanceLifetime" Prelude.=: maxInstanceLifetime,
        "LaunchTemplate" Prelude.=: launchTemplate,
        "LaunchConfigurationName"
          Prelude.=: launchConfigurationName,
        "HealthCheckType" Prelude.=: healthCheckType,
        "MixedInstancesPolicy"
          Prelude.=: mixedInstancesPolicy,
        "MaxSize" Prelude.=: maxSize,
        "VPCZoneIdentifier" Prelude.=: vPCZoneIdentifier,
        "CapacityRebalance" Prelude.=: capacityRebalance,
        "NewInstancesProtectedFromScaleIn"
          Prelude.=: newInstancesProtectedFromScaleIn',
        "ServiceLinkedRoleARN"
          Prelude.=: serviceLinkedRoleARN,
        "HealthCheckGracePeriod"
          Prelude.=: healthCheckGracePeriod,
        "TerminationPolicies"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "member"
                Prelude.<$> terminationPolicies
            ),
        "AutoScalingGroupName"
          Prelude.=: autoScalingGroupName
      ]

-- | /See:/ 'newUpdateAutoScalingGroupResponse' smart constructor.
data UpdateAutoScalingGroupResponse = UpdateAutoScalingGroupResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateAutoScalingGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateAutoScalingGroupResponse ::
  UpdateAutoScalingGroupResponse
newUpdateAutoScalingGroupResponse =
  UpdateAutoScalingGroupResponse'

instance
  Prelude.NFData
    UpdateAutoScalingGroupResponse
