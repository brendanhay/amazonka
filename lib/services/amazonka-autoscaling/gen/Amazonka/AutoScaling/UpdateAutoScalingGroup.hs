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
-- Module      : Amazonka.AutoScaling.UpdateAutoScalingGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
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
-- property that you want to change. Any properties that you don\'t specify
-- are not changed by this update request. The new settings take effect on
-- any scaling activities after this call returns.
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
-- To see which properties have been set, call the
-- DescribeAutoScalingGroups API. To view the scaling policies for an Auto
-- Scaling group, call the DescribePolicies API. If the group has scaling
-- policies, you can update them by calling the PutScalingPolicy API.
module Amazonka.AutoScaling.UpdateAutoScalingGroup
  ( -- * Creating a Request
    UpdateAutoScalingGroup (..),
    newUpdateAutoScalingGroup,

    -- * Request Lenses
    updateAutoScalingGroup_availabilityZones,
    updateAutoScalingGroup_capacityRebalance,
    updateAutoScalingGroup_context,
    updateAutoScalingGroup_defaultCooldown,
    updateAutoScalingGroup_defaultInstanceWarmup,
    updateAutoScalingGroup_desiredCapacity,
    updateAutoScalingGroup_desiredCapacityType,
    updateAutoScalingGroup_healthCheckGracePeriod,
    updateAutoScalingGroup_healthCheckType,
    updateAutoScalingGroup_launchConfigurationName,
    updateAutoScalingGroup_launchTemplate,
    updateAutoScalingGroup_maxInstanceLifetime,
    updateAutoScalingGroup_maxSize,
    updateAutoScalingGroup_minSize,
    updateAutoScalingGroup_mixedInstancesPolicy,
    updateAutoScalingGroup_newInstancesProtectedFromScaleIn,
    updateAutoScalingGroup_placementGroup,
    updateAutoScalingGroup_serviceLinkedRoleARN,
    updateAutoScalingGroup_terminationPolicies,
    updateAutoScalingGroup_vPCZoneIdentifier,
    updateAutoScalingGroup_autoScalingGroupName,

    -- * Destructuring the Response
    UpdateAutoScalingGroupResponse (..),
    newUpdateAutoScalingGroupResponse,
  )
where

import Amazonka.AutoScaling.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateAutoScalingGroup' smart constructor.
data UpdateAutoScalingGroup = UpdateAutoScalingGroup'
  { -- | One or more Availability Zones for the group.
    availabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | Enables or disables Capacity Rebalancing. For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-capacity-rebalancing.html Use Capacity Rebalancing to handle Amazon EC2 Spot Interruptions>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    capacityRebalance :: Prelude.Maybe Prelude.Bool,
    -- | Reserved.
    context :: Prelude.Maybe Prelude.Text,
    -- | /Only needed if you use simple scaling policies./
    --
    -- The amount of time, in seconds, between one scaling activity ending and
    -- another one starting due to simple scaling policies. For more
    -- information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/Cooldown.html Scaling cooldowns for Amazon EC2 Auto Scaling>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    defaultCooldown :: Prelude.Maybe Prelude.Int,
    -- | The amount of time, in seconds, until a new instance is considered to
    -- have finished initializing and resource consumption to become stable
    -- after it enters the @InService@ state.
    --
    -- During an instance refresh, Amazon EC2 Auto Scaling waits for the
    -- warm-up period after it replaces an instance before it moves on to
    -- replacing the next instance. Amazon EC2 Auto Scaling also waits for the
    -- warm-up period before aggregating the metrics for new instances with
    -- existing instances in the Amazon CloudWatch metrics that are used for
    -- scaling, resulting in more reliable usage data. For more information,
    -- see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-default-instance-warmup.html Set the default instance warmup for an Auto Scaling group>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    --
    -- To manage various warm-up settings at the group level, we recommend that
    -- you set the default instance warmup, /even if it is set to 0 seconds/.
    -- To remove a value that you previously set, include the property but
    -- specify @-1@ for the value. However, we strongly recommend keeping the
    -- default instance warmup enabled by specifying a value of @0@ or other
    -- nominal value.
    defaultInstanceWarmup :: Prelude.Maybe Prelude.Int,
    -- | The desired capacity is the initial capacity of the Auto Scaling group
    -- after this operation completes and the capacity it attempts to maintain.
    -- This number must be greater than or equal to the minimum size of the
    -- group and less than or equal to the maximum size of the group.
    desiredCapacity :: Prelude.Maybe Prelude.Int,
    -- | The unit of measurement for the value specified for desired capacity.
    -- Amazon EC2 Auto Scaling supports @DesiredCapacityType@ for
    -- attribute-based instance type selection only. For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-asg-instance-type-requirements.html Creating an Auto Scaling group using attribute-based instance type selection>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    --
    -- By default, Amazon EC2 Auto Scaling specifies @units@, which translates
    -- into number of instances.
    --
    -- Valid values: @units@ | @vcpu@ | @memory-mib@
    desiredCapacityType :: Prelude.Maybe Prelude.Text,
    -- | The amount of time, in seconds, that Amazon EC2 Auto Scaling waits
    -- before checking the health status of an EC2 instance that has come into
    -- service and marking it unhealthy due to a failed health check. This is
    -- useful if your instances do not immediately pass their health checks
    -- after they enter the @InService@ state. For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/health-check-grace-period.html Set the health check grace period for an Auto Scaling group>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    healthCheckGracePeriod :: Prelude.Maybe Prelude.Int,
    -- | A comma-separated value string of one or more health check types.
    --
    -- The valid values are @EC2@, @ELB@, and @VPC_LATTICE@. @EC2@ is the
    -- default health check and cannot be disabled. For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/healthcheck.html Health checks for Auto Scaling instances>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    --
    -- Only specify @EC2@ if you must clear a value that was previously set.
    healthCheckType :: Prelude.Maybe Prelude.Text,
    -- | The name of the launch configuration. If you specify
    -- @LaunchConfigurationName@ in your update request, you can\'t specify
    -- @LaunchTemplate@ or @MixedInstancesPolicy@.
    launchConfigurationName :: Prelude.Maybe Prelude.Text,
    -- | The launch template and version to use to specify the updates. If you
    -- specify @LaunchTemplate@ in your update request, you can\'t specify
    -- @LaunchConfigurationName@ or @MixedInstancesPolicy@.
    launchTemplate :: Prelude.Maybe LaunchTemplateSpecification,
    -- | The maximum amount of time, in seconds, that an instance can be in
    -- service. The default is null. If specified, the value must be either 0
    -- or a number equal to or greater than 86,400 seconds (1 day). To clear a
    -- previously set value, specify a new value of 0. For more information,
    -- see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-max-instance-lifetime.html Replacing Auto Scaling instances based on maximum instance lifetime>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    maxInstanceLifetime :: Prelude.Maybe Prelude.Int,
    -- | The maximum size of the Auto Scaling group.
    --
    -- With a mixed instances policy that uses instance weighting, Amazon EC2
    -- Auto Scaling may need to go above @MaxSize@ to meet your capacity
    -- requirements. In this event, Amazon EC2 Auto Scaling will never go above
    -- @MaxSize@ by more than your largest instance weight (weights that define
    -- how many units each instance contributes to the desired capacity of the
    -- group).
    maxSize :: Prelude.Maybe Prelude.Int,
    -- | The minimum size of the Auto Scaling group.
    minSize :: Prelude.Maybe Prelude.Int,
    -- | The mixed instances policy. For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-mixed-instances-groups.html Auto Scaling groups with multiple instance types and purchase options>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    mixedInstancesPolicy :: Prelude.Maybe MixedInstancesPolicy,
    -- | Indicates whether newly launched instances are protected from
    -- termination by Amazon EC2 Auto Scaling when scaling in. For more
    -- information about preventing instances from terminating on scale in, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-instance-protection.html Using instance scale-in protection>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    newInstancesProtectedFromScaleIn' :: Prelude.Maybe Prelude.Bool,
    -- | The name of an existing placement group into which to launch your
    -- instances. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement groups>
    -- in the /Amazon EC2 User Guide for Linux Instances/.
    --
    -- A /cluster/ placement group is a logical grouping of instances within a
    -- single Availability Zone. You cannot specify multiple Availability Zones
    -- and a cluster placement group.
    placementGroup :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the service-linked role that the Auto
    -- Scaling group uses to call other Amazon Web Services on your behalf. For
    -- more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-service-linked-role.html Service-linked roles>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    serviceLinkedRoleARN :: Prelude.Maybe Prelude.Text,
    -- | A policy or a list of policies that are used to select the instances to
    -- terminate. The policies are executed in the order that you list them.
    -- For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-termination-policies.html Work with Amazon EC2 Auto Scaling termination policies>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    --
    -- Valid values: @Default@ | @AllocationStrategy@ |
    -- @ClosestToNextInstanceHour@ | @NewestInstance@ | @OldestInstance@ |
    -- @OldestLaunchConfiguration@ | @OldestLaunchTemplate@ |
    -- @arn:aws:lambda:region:account-id:function:my-function:my-alias@
    terminationPolicies :: Prelude.Maybe [Prelude.Text],
    -- | A comma-separated list of subnet IDs for a virtual private cloud (VPC).
    -- If you specify @VPCZoneIdentifier@ with @AvailabilityZones@, the subnets
    -- that you specify must reside in those Availability Zones.
    vPCZoneIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAutoScalingGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZones', 'updateAutoScalingGroup_availabilityZones' - One or more Availability Zones for the group.
--
-- 'capacityRebalance', 'updateAutoScalingGroup_capacityRebalance' - Enables or disables Capacity Rebalancing. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-capacity-rebalancing.html Use Capacity Rebalancing to handle Amazon EC2 Spot Interruptions>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- 'context', 'updateAutoScalingGroup_context' - Reserved.
--
-- 'defaultCooldown', 'updateAutoScalingGroup_defaultCooldown' - /Only needed if you use simple scaling policies./
--
-- The amount of time, in seconds, between one scaling activity ending and
-- another one starting due to simple scaling policies. For more
-- information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/Cooldown.html Scaling cooldowns for Amazon EC2 Auto Scaling>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- 'defaultInstanceWarmup', 'updateAutoScalingGroup_defaultInstanceWarmup' - The amount of time, in seconds, until a new instance is considered to
-- have finished initializing and resource consumption to become stable
-- after it enters the @InService@ state.
--
-- During an instance refresh, Amazon EC2 Auto Scaling waits for the
-- warm-up period after it replaces an instance before it moves on to
-- replacing the next instance. Amazon EC2 Auto Scaling also waits for the
-- warm-up period before aggregating the metrics for new instances with
-- existing instances in the Amazon CloudWatch metrics that are used for
-- scaling, resulting in more reliable usage data. For more information,
-- see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-default-instance-warmup.html Set the default instance warmup for an Auto Scaling group>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- To manage various warm-up settings at the group level, we recommend that
-- you set the default instance warmup, /even if it is set to 0 seconds/.
-- To remove a value that you previously set, include the property but
-- specify @-1@ for the value. However, we strongly recommend keeping the
-- default instance warmup enabled by specifying a value of @0@ or other
-- nominal value.
--
-- 'desiredCapacity', 'updateAutoScalingGroup_desiredCapacity' - The desired capacity is the initial capacity of the Auto Scaling group
-- after this operation completes and the capacity it attempts to maintain.
-- This number must be greater than or equal to the minimum size of the
-- group and less than or equal to the maximum size of the group.
--
-- 'desiredCapacityType', 'updateAutoScalingGroup_desiredCapacityType' - The unit of measurement for the value specified for desired capacity.
-- Amazon EC2 Auto Scaling supports @DesiredCapacityType@ for
-- attribute-based instance type selection only. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-asg-instance-type-requirements.html Creating an Auto Scaling group using attribute-based instance type selection>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- By default, Amazon EC2 Auto Scaling specifies @units@, which translates
-- into number of instances.
--
-- Valid values: @units@ | @vcpu@ | @memory-mib@
--
-- 'healthCheckGracePeriod', 'updateAutoScalingGroup_healthCheckGracePeriod' - The amount of time, in seconds, that Amazon EC2 Auto Scaling waits
-- before checking the health status of an EC2 instance that has come into
-- service and marking it unhealthy due to a failed health check. This is
-- useful if your instances do not immediately pass their health checks
-- after they enter the @InService@ state. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/health-check-grace-period.html Set the health check grace period for an Auto Scaling group>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- 'healthCheckType', 'updateAutoScalingGroup_healthCheckType' - A comma-separated value string of one or more health check types.
--
-- The valid values are @EC2@, @ELB@, and @VPC_LATTICE@. @EC2@ is the
-- default health check and cannot be disabled. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/healthcheck.html Health checks for Auto Scaling instances>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- Only specify @EC2@ if you must clear a value that was previously set.
--
-- 'launchConfigurationName', 'updateAutoScalingGroup_launchConfigurationName' - The name of the launch configuration. If you specify
-- @LaunchConfigurationName@ in your update request, you can\'t specify
-- @LaunchTemplate@ or @MixedInstancesPolicy@.
--
-- 'launchTemplate', 'updateAutoScalingGroup_launchTemplate' - The launch template and version to use to specify the updates. If you
-- specify @LaunchTemplate@ in your update request, you can\'t specify
-- @LaunchConfigurationName@ or @MixedInstancesPolicy@.
--
-- 'maxInstanceLifetime', 'updateAutoScalingGroup_maxInstanceLifetime' - The maximum amount of time, in seconds, that an instance can be in
-- service. The default is null. If specified, the value must be either 0
-- or a number equal to or greater than 86,400 seconds (1 day). To clear a
-- previously set value, specify a new value of 0. For more information,
-- see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-max-instance-lifetime.html Replacing Auto Scaling instances based on maximum instance lifetime>
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
-- 'minSize', 'updateAutoScalingGroup_minSize' - The minimum size of the Auto Scaling group.
--
-- 'mixedInstancesPolicy', 'updateAutoScalingGroup_mixedInstancesPolicy' - The mixed instances policy. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-mixed-instances-groups.html Auto Scaling groups with multiple instance types and purchase options>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- 'newInstancesProtectedFromScaleIn'', 'updateAutoScalingGroup_newInstancesProtectedFromScaleIn' - Indicates whether newly launched instances are protected from
-- termination by Amazon EC2 Auto Scaling when scaling in. For more
-- information about preventing instances from terminating on scale in, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-instance-protection.html Using instance scale-in protection>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- 'placementGroup', 'updateAutoScalingGroup_placementGroup' - The name of an existing placement group into which to launch your
-- instances. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement groups>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- A /cluster/ placement group is a logical grouping of instances within a
-- single Availability Zone. You cannot specify multiple Availability Zones
-- and a cluster placement group.
--
-- 'serviceLinkedRoleARN', 'updateAutoScalingGroup_serviceLinkedRoleARN' - The Amazon Resource Name (ARN) of the service-linked role that the Auto
-- Scaling group uses to call other Amazon Web Services on your behalf. For
-- more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-service-linked-role.html Service-linked roles>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- 'terminationPolicies', 'updateAutoScalingGroup_terminationPolicies' - A policy or a list of policies that are used to select the instances to
-- terminate. The policies are executed in the order that you list them.
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-termination-policies.html Work with Amazon EC2 Auto Scaling termination policies>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- Valid values: @Default@ | @AllocationStrategy@ |
-- @ClosestToNextInstanceHour@ | @NewestInstance@ | @OldestInstance@ |
-- @OldestLaunchConfiguration@ | @OldestLaunchTemplate@ |
-- @arn:aws:lambda:region:account-id:function:my-function:my-alias@
--
-- 'vPCZoneIdentifier', 'updateAutoScalingGroup_vPCZoneIdentifier' - A comma-separated list of subnet IDs for a virtual private cloud (VPC).
-- If you specify @VPCZoneIdentifier@ with @AvailabilityZones@, the subnets
-- that you specify must reside in those Availability Zones.
--
-- 'autoScalingGroupName', 'updateAutoScalingGroup_autoScalingGroupName' - The name of the Auto Scaling group.
newUpdateAutoScalingGroup ::
  -- | 'autoScalingGroupName'
  Prelude.Text ->
  UpdateAutoScalingGroup
newUpdateAutoScalingGroup pAutoScalingGroupName_ =
  UpdateAutoScalingGroup'
    { availabilityZones =
        Prelude.Nothing,
      capacityRebalance = Prelude.Nothing,
      context = Prelude.Nothing,
      defaultCooldown = Prelude.Nothing,
      defaultInstanceWarmup = Prelude.Nothing,
      desiredCapacity = Prelude.Nothing,
      desiredCapacityType = Prelude.Nothing,
      healthCheckGracePeriod = Prelude.Nothing,
      healthCheckType = Prelude.Nothing,
      launchConfigurationName = Prelude.Nothing,
      launchTemplate = Prelude.Nothing,
      maxInstanceLifetime = Prelude.Nothing,
      maxSize = Prelude.Nothing,
      minSize = Prelude.Nothing,
      mixedInstancesPolicy = Prelude.Nothing,
      newInstancesProtectedFromScaleIn' = Prelude.Nothing,
      placementGroup = Prelude.Nothing,
      serviceLinkedRoleARN = Prelude.Nothing,
      terminationPolicies = Prelude.Nothing,
      vPCZoneIdentifier = Prelude.Nothing,
      autoScalingGroupName = pAutoScalingGroupName_
    }

-- | One or more Availability Zones for the group.
updateAutoScalingGroup_availabilityZones :: Lens.Lens' UpdateAutoScalingGroup (Prelude.Maybe [Prelude.Text])
updateAutoScalingGroup_availabilityZones = Lens.lens (\UpdateAutoScalingGroup' {availabilityZones} -> availabilityZones) (\s@UpdateAutoScalingGroup' {} a -> s {availabilityZones = a} :: UpdateAutoScalingGroup) Prelude.. Lens.mapping Lens.coerced

-- | Enables or disables Capacity Rebalancing. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-capacity-rebalancing.html Use Capacity Rebalancing to handle Amazon EC2 Spot Interruptions>
-- in the /Amazon EC2 Auto Scaling User Guide/.
updateAutoScalingGroup_capacityRebalance :: Lens.Lens' UpdateAutoScalingGroup (Prelude.Maybe Prelude.Bool)
updateAutoScalingGroup_capacityRebalance = Lens.lens (\UpdateAutoScalingGroup' {capacityRebalance} -> capacityRebalance) (\s@UpdateAutoScalingGroup' {} a -> s {capacityRebalance = a} :: UpdateAutoScalingGroup)

-- | Reserved.
updateAutoScalingGroup_context :: Lens.Lens' UpdateAutoScalingGroup (Prelude.Maybe Prelude.Text)
updateAutoScalingGroup_context = Lens.lens (\UpdateAutoScalingGroup' {context} -> context) (\s@UpdateAutoScalingGroup' {} a -> s {context = a} :: UpdateAutoScalingGroup)

-- | /Only needed if you use simple scaling policies./
--
-- The amount of time, in seconds, between one scaling activity ending and
-- another one starting due to simple scaling policies. For more
-- information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/Cooldown.html Scaling cooldowns for Amazon EC2 Auto Scaling>
-- in the /Amazon EC2 Auto Scaling User Guide/.
updateAutoScalingGroup_defaultCooldown :: Lens.Lens' UpdateAutoScalingGroup (Prelude.Maybe Prelude.Int)
updateAutoScalingGroup_defaultCooldown = Lens.lens (\UpdateAutoScalingGroup' {defaultCooldown} -> defaultCooldown) (\s@UpdateAutoScalingGroup' {} a -> s {defaultCooldown = a} :: UpdateAutoScalingGroup)

-- | The amount of time, in seconds, until a new instance is considered to
-- have finished initializing and resource consumption to become stable
-- after it enters the @InService@ state.
--
-- During an instance refresh, Amazon EC2 Auto Scaling waits for the
-- warm-up period after it replaces an instance before it moves on to
-- replacing the next instance. Amazon EC2 Auto Scaling also waits for the
-- warm-up period before aggregating the metrics for new instances with
-- existing instances in the Amazon CloudWatch metrics that are used for
-- scaling, resulting in more reliable usage data. For more information,
-- see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-default-instance-warmup.html Set the default instance warmup for an Auto Scaling group>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- To manage various warm-up settings at the group level, we recommend that
-- you set the default instance warmup, /even if it is set to 0 seconds/.
-- To remove a value that you previously set, include the property but
-- specify @-1@ for the value. However, we strongly recommend keeping the
-- default instance warmup enabled by specifying a value of @0@ or other
-- nominal value.
updateAutoScalingGroup_defaultInstanceWarmup :: Lens.Lens' UpdateAutoScalingGroup (Prelude.Maybe Prelude.Int)
updateAutoScalingGroup_defaultInstanceWarmup = Lens.lens (\UpdateAutoScalingGroup' {defaultInstanceWarmup} -> defaultInstanceWarmup) (\s@UpdateAutoScalingGroup' {} a -> s {defaultInstanceWarmup = a} :: UpdateAutoScalingGroup)

-- | The desired capacity is the initial capacity of the Auto Scaling group
-- after this operation completes and the capacity it attempts to maintain.
-- This number must be greater than or equal to the minimum size of the
-- group and less than or equal to the maximum size of the group.
updateAutoScalingGroup_desiredCapacity :: Lens.Lens' UpdateAutoScalingGroup (Prelude.Maybe Prelude.Int)
updateAutoScalingGroup_desiredCapacity = Lens.lens (\UpdateAutoScalingGroup' {desiredCapacity} -> desiredCapacity) (\s@UpdateAutoScalingGroup' {} a -> s {desiredCapacity = a} :: UpdateAutoScalingGroup)

-- | The unit of measurement for the value specified for desired capacity.
-- Amazon EC2 Auto Scaling supports @DesiredCapacityType@ for
-- attribute-based instance type selection only. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-asg-instance-type-requirements.html Creating an Auto Scaling group using attribute-based instance type selection>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- By default, Amazon EC2 Auto Scaling specifies @units@, which translates
-- into number of instances.
--
-- Valid values: @units@ | @vcpu@ | @memory-mib@
updateAutoScalingGroup_desiredCapacityType :: Lens.Lens' UpdateAutoScalingGroup (Prelude.Maybe Prelude.Text)
updateAutoScalingGroup_desiredCapacityType = Lens.lens (\UpdateAutoScalingGroup' {desiredCapacityType} -> desiredCapacityType) (\s@UpdateAutoScalingGroup' {} a -> s {desiredCapacityType = a} :: UpdateAutoScalingGroup)

-- | The amount of time, in seconds, that Amazon EC2 Auto Scaling waits
-- before checking the health status of an EC2 instance that has come into
-- service and marking it unhealthy due to a failed health check. This is
-- useful if your instances do not immediately pass their health checks
-- after they enter the @InService@ state. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/health-check-grace-period.html Set the health check grace period for an Auto Scaling group>
-- in the /Amazon EC2 Auto Scaling User Guide/.
updateAutoScalingGroup_healthCheckGracePeriod :: Lens.Lens' UpdateAutoScalingGroup (Prelude.Maybe Prelude.Int)
updateAutoScalingGroup_healthCheckGracePeriod = Lens.lens (\UpdateAutoScalingGroup' {healthCheckGracePeriod} -> healthCheckGracePeriod) (\s@UpdateAutoScalingGroup' {} a -> s {healthCheckGracePeriod = a} :: UpdateAutoScalingGroup)

-- | A comma-separated value string of one or more health check types.
--
-- The valid values are @EC2@, @ELB@, and @VPC_LATTICE@. @EC2@ is the
-- default health check and cannot be disabled. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/healthcheck.html Health checks for Auto Scaling instances>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- Only specify @EC2@ if you must clear a value that was previously set.
updateAutoScalingGroup_healthCheckType :: Lens.Lens' UpdateAutoScalingGroup (Prelude.Maybe Prelude.Text)
updateAutoScalingGroup_healthCheckType = Lens.lens (\UpdateAutoScalingGroup' {healthCheckType} -> healthCheckType) (\s@UpdateAutoScalingGroup' {} a -> s {healthCheckType = a} :: UpdateAutoScalingGroup)

-- | The name of the launch configuration. If you specify
-- @LaunchConfigurationName@ in your update request, you can\'t specify
-- @LaunchTemplate@ or @MixedInstancesPolicy@.
updateAutoScalingGroup_launchConfigurationName :: Lens.Lens' UpdateAutoScalingGroup (Prelude.Maybe Prelude.Text)
updateAutoScalingGroup_launchConfigurationName = Lens.lens (\UpdateAutoScalingGroup' {launchConfigurationName} -> launchConfigurationName) (\s@UpdateAutoScalingGroup' {} a -> s {launchConfigurationName = a} :: UpdateAutoScalingGroup)

-- | The launch template and version to use to specify the updates. If you
-- specify @LaunchTemplate@ in your update request, you can\'t specify
-- @LaunchConfigurationName@ or @MixedInstancesPolicy@.
updateAutoScalingGroup_launchTemplate :: Lens.Lens' UpdateAutoScalingGroup (Prelude.Maybe LaunchTemplateSpecification)
updateAutoScalingGroup_launchTemplate = Lens.lens (\UpdateAutoScalingGroup' {launchTemplate} -> launchTemplate) (\s@UpdateAutoScalingGroup' {} a -> s {launchTemplate = a} :: UpdateAutoScalingGroup)

-- | The maximum amount of time, in seconds, that an instance can be in
-- service. The default is null. If specified, the value must be either 0
-- or a number equal to or greater than 86,400 seconds (1 day). To clear a
-- previously set value, specify a new value of 0. For more information,
-- see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-max-instance-lifetime.html Replacing Auto Scaling instances based on maximum instance lifetime>
-- in the /Amazon EC2 Auto Scaling User Guide/.
updateAutoScalingGroup_maxInstanceLifetime :: Lens.Lens' UpdateAutoScalingGroup (Prelude.Maybe Prelude.Int)
updateAutoScalingGroup_maxInstanceLifetime = Lens.lens (\UpdateAutoScalingGroup' {maxInstanceLifetime} -> maxInstanceLifetime) (\s@UpdateAutoScalingGroup' {} a -> s {maxInstanceLifetime = a} :: UpdateAutoScalingGroup)

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

-- | The minimum size of the Auto Scaling group.
updateAutoScalingGroup_minSize :: Lens.Lens' UpdateAutoScalingGroup (Prelude.Maybe Prelude.Int)
updateAutoScalingGroup_minSize = Lens.lens (\UpdateAutoScalingGroup' {minSize} -> minSize) (\s@UpdateAutoScalingGroup' {} a -> s {minSize = a} :: UpdateAutoScalingGroup)

-- | The mixed instances policy. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-mixed-instances-groups.html Auto Scaling groups with multiple instance types and purchase options>
-- in the /Amazon EC2 Auto Scaling User Guide/.
updateAutoScalingGroup_mixedInstancesPolicy :: Lens.Lens' UpdateAutoScalingGroup (Prelude.Maybe MixedInstancesPolicy)
updateAutoScalingGroup_mixedInstancesPolicy = Lens.lens (\UpdateAutoScalingGroup' {mixedInstancesPolicy} -> mixedInstancesPolicy) (\s@UpdateAutoScalingGroup' {} a -> s {mixedInstancesPolicy = a} :: UpdateAutoScalingGroup)

-- | Indicates whether newly launched instances are protected from
-- termination by Amazon EC2 Auto Scaling when scaling in. For more
-- information about preventing instances from terminating on scale in, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-instance-protection.html Using instance scale-in protection>
-- in the /Amazon EC2 Auto Scaling User Guide/.
updateAutoScalingGroup_newInstancesProtectedFromScaleIn :: Lens.Lens' UpdateAutoScalingGroup (Prelude.Maybe Prelude.Bool)
updateAutoScalingGroup_newInstancesProtectedFromScaleIn = Lens.lens (\UpdateAutoScalingGroup' {newInstancesProtectedFromScaleIn'} -> newInstancesProtectedFromScaleIn') (\s@UpdateAutoScalingGroup' {} a -> s {newInstancesProtectedFromScaleIn' = a} :: UpdateAutoScalingGroup)

-- | The name of an existing placement group into which to launch your
-- instances. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement groups>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- A /cluster/ placement group is a logical grouping of instances within a
-- single Availability Zone. You cannot specify multiple Availability Zones
-- and a cluster placement group.
updateAutoScalingGroup_placementGroup :: Lens.Lens' UpdateAutoScalingGroup (Prelude.Maybe Prelude.Text)
updateAutoScalingGroup_placementGroup = Lens.lens (\UpdateAutoScalingGroup' {placementGroup} -> placementGroup) (\s@UpdateAutoScalingGroup' {} a -> s {placementGroup = a} :: UpdateAutoScalingGroup)

-- | The Amazon Resource Name (ARN) of the service-linked role that the Auto
-- Scaling group uses to call other Amazon Web Services on your behalf. For
-- more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-service-linked-role.html Service-linked roles>
-- in the /Amazon EC2 Auto Scaling User Guide/.
updateAutoScalingGroup_serviceLinkedRoleARN :: Lens.Lens' UpdateAutoScalingGroup (Prelude.Maybe Prelude.Text)
updateAutoScalingGroup_serviceLinkedRoleARN = Lens.lens (\UpdateAutoScalingGroup' {serviceLinkedRoleARN} -> serviceLinkedRoleARN) (\s@UpdateAutoScalingGroup' {} a -> s {serviceLinkedRoleARN = a} :: UpdateAutoScalingGroup)

-- | A policy or a list of policies that are used to select the instances to
-- terminate. The policies are executed in the order that you list them.
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-termination-policies.html Work with Amazon EC2 Auto Scaling termination policies>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- Valid values: @Default@ | @AllocationStrategy@ |
-- @ClosestToNextInstanceHour@ | @NewestInstance@ | @OldestInstance@ |
-- @OldestLaunchConfiguration@ | @OldestLaunchTemplate@ |
-- @arn:aws:lambda:region:account-id:function:my-function:my-alias@
updateAutoScalingGroup_terminationPolicies :: Lens.Lens' UpdateAutoScalingGroup (Prelude.Maybe [Prelude.Text])
updateAutoScalingGroup_terminationPolicies = Lens.lens (\UpdateAutoScalingGroup' {terminationPolicies} -> terminationPolicies) (\s@UpdateAutoScalingGroup' {} a -> s {terminationPolicies = a} :: UpdateAutoScalingGroup) Prelude.. Lens.mapping Lens.coerced

-- | A comma-separated list of subnet IDs for a virtual private cloud (VPC).
-- If you specify @VPCZoneIdentifier@ with @AvailabilityZones@, the subnets
-- that you specify must reside in those Availability Zones.
updateAutoScalingGroup_vPCZoneIdentifier :: Lens.Lens' UpdateAutoScalingGroup (Prelude.Maybe Prelude.Text)
updateAutoScalingGroup_vPCZoneIdentifier = Lens.lens (\UpdateAutoScalingGroup' {vPCZoneIdentifier} -> vPCZoneIdentifier) (\s@UpdateAutoScalingGroup' {} a -> s {vPCZoneIdentifier = a} :: UpdateAutoScalingGroup)

-- | The name of the Auto Scaling group.
updateAutoScalingGroup_autoScalingGroupName :: Lens.Lens' UpdateAutoScalingGroup Prelude.Text
updateAutoScalingGroup_autoScalingGroupName = Lens.lens (\UpdateAutoScalingGroup' {autoScalingGroupName} -> autoScalingGroupName) (\s@UpdateAutoScalingGroup' {} a -> s {autoScalingGroupName = a} :: UpdateAutoScalingGroup)

instance Core.AWSRequest UpdateAutoScalingGroup where
  type
    AWSResponse UpdateAutoScalingGroup =
      UpdateAutoScalingGroupResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      UpdateAutoScalingGroupResponse'

instance Prelude.Hashable UpdateAutoScalingGroup where
  hashWithSalt _salt UpdateAutoScalingGroup' {..} =
    _salt
      `Prelude.hashWithSalt` availabilityZones
      `Prelude.hashWithSalt` capacityRebalance
      `Prelude.hashWithSalt` context
      `Prelude.hashWithSalt` defaultCooldown
      `Prelude.hashWithSalt` defaultInstanceWarmup
      `Prelude.hashWithSalt` desiredCapacity
      `Prelude.hashWithSalt` desiredCapacityType
      `Prelude.hashWithSalt` healthCheckGracePeriod
      `Prelude.hashWithSalt` healthCheckType
      `Prelude.hashWithSalt` launchConfigurationName
      `Prelude.hashWithSalt` launchTemplate
      `Prelude.hashWithSalt` maxInstanceLifetime
      `Prelude.hashWithSalt` maxSize
      `Prelude.hashWithSalt` minSize
      `Prelude.hashWithSalt` mixedInstancesPolicy
      `Prelude.hashWithSalt` newInstancesProtectedFromScaleIn'
      `Prelude.hashWithSalt` placementGroup
      `Prelude.hashWithSalt` serviceLinkedRoleARN
      `Prelude.hashWithSalt` terminationPolicies
      `Prelude.hashWithSalt` vPCZoneIdentifier
      `Prelude.hashWithSalt` autoScalingGroupName

instance Prelude.NFData UpdateAutoScalingGroup where
  rnf UpdateAutoScalingGroup' {..} =
    Prelude.rnf availabilityZones
      `Prelude.seq` Prelude.rnf capacityRebalance
      `Prelude.seq` Prelude.rnf context
      `Prelude.seq` Prelude.rnf defaultCooldown
      `Prelude.seq` Prelude.rnf defaultInstanceWarmup
      `Prelude.seq` Prelude.rnf desiredCapacity
      `Prelude.seq` Prelude.rnf desiredCapacityType
      `Prelude.seq` Prelude.rnf healthCheckGracePeriod
      `Prelude.seq` Prelude.rnf healthCheckType
      `Prelude.seq` Prelude.rnf launchConfigurationName
      `Prelude.seq` Prelude.rnf launchTemplate
      `Prelude.seq` Prelude.rnf maxInstanceLifetime
      `Prelude.seq` Prelude.rnf maxSize
      `Prelude.seq` Prelude.rnf minSize
      `Prelude.seq` Prelude.rnf mixedInstancesPolicy
      `Prelude.seq` Prelude.rnf
        newInstancesProtectedFromScaleIn'
      `Prelude.seq` Prelude.rnf placementGroup
      `Prelude.seq` Prelude.rnf serviceLinkedRoleARN
      `Prelude.seq` Prelude.rnf terminationPolicies
      `Prelude.seq` Prelude.rnf vPCZoneIdentifier
      `Prelude.seq` Prelude.rnf
        autoScalingGroupName

instance Data.ToHeaders UpdateAutoScalingGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath UpdateAutoScalingGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateAutoScalingGroup where
  toQuery UpdateAutoScalingGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("UpdateAutoScalingGroup" :: Prelude.ByteString),
        "Version"
          Data.=: ("2011-01-01" :: Prelude.ByteString),
        "AvailabilityZones"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> availabilityZones
            ),
        "CapacityRebalance" Data.=: capacityRebalance,
        "Context" Data.=: context,
        "DefaultCooldown" Data.=: defaultCooldown,
        "DefaultInstanceWarmup"
          Data.=: defaultInstanceWarmup,
        "DesiredCapacity" Data.=: desiredCapacity,
        "DesiredCapacityType" Data.=: desiredCapacityType,
        "HealthCheckGracePeriod"
          Data.=: healthCheckGracePeriod,
        "HealthCheckType" Data.=: healthCheckType,
        "LaunchConfigurationName"
          Data.=: launchConfigurationName,
        "LaunchTemplate" Data.=: launchTemplate,
        "MaxInstanceLifetime" Data.=: maxInstanceLifetime,
        "MaxSize" Data.=: maxSize,
        "MinSize" Data.=: minSize,
        "MixedInstancesPolicy" Data.=: mixedInstancesPolicy,
        "NewInstancesProtectedFromScaleIn"
          Data.=: newInstancesProtectedFromScaleIn',
        "PlacementGroup" Data.=: placementGroup,
        "ServiceLinkedRoleARN" Data.=: serviceLinkedRoleARN,
        "TerminationPolicies"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> terminationPolicies
            ),
        "VPCZoneIdentifier" Data.=: vPCZoneIdentifier,
        "AutoScalingGroupName" Data.=: autoScalingGroupName
      ]

-- | /See:/ 'newUpdateAutoScalingGroupResponse' smart constructor.
data UpdateAutoScalingGroupResponse = UpdateAutoScalingGroupResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf _ = ()
