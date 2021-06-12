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
-- Module      : Network.AWS.AutoScaling.CreateAutoScalingGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __We strongly recommend using a launch template when calling this
-- operation to ensure full functionality for Amazon EC2 Auto Scaling and
-- Amazon EC2.__
--
-- Creates an Auto Scaling group with the specified name and attributes.
--
-- If you exceed your maximum limit of Auto Scaling groups, the call fails.
-- To query this limit, call the DescribeAccountLimits API. For information
-- about updating this limit, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-account-limits.html Amazon EC2 Auto Scaling service quotas>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- For introductory exercises for creating an Auto Scaling group, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/GettingStartedTutorial.html Getting started with Amazon EC2 Auto Scaling>
-- and
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-register-lbs-with-asg.html Tutorial: Set up a scaled and load-balanced application>
-- in the /Amazon EC2 Auto Scaling User Guide/. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/AutoScalingGroup.html Auto Scaling groups>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- Every Auto Scaling group has three size parameters (@DesiredCapacity@,
-- @MaxSize@, and @MinSize@). Usually, you set these sizes based on a
-- specific number of instances. However, if you configure a mixed
-- instances policy that defines weights for the instance types, you must
-- specify these sizes with the same units that you use for weighting
-- instances.
module Network.AWS.AutoScaling.CreateAutoScalingGroup
  ( -- * Creating a Request
    CreateAutoScalingGroup (..),
    newCreateAutoScalingGroup,

    -- * Request Lenses
    createAutoScalingGroup_desiredCapacity,
    createAutoScalingGroup_instanceId,
    createAutoScalingGroup_availabilityZones,
    createAutoScalingGroup_placementGroup,
    createAutoScalingGroup_defaultCooldown,
    createAutoScalingGroup_maxInstanceLifetime,
    createAutoScalingGroup_launchTemplate,
    createAutoScalingGroup_launchConfigurationName,
    createAutoScalingGroup_healthCheckType,
    createAutoScalingGroup_mixedInstancesPolicy,
    createAutoScalingGroup_tags,
    createAutoScalingGroup_loadBalancerNames,
    createAutoScalingGroup_vPCZoneIdentifier,
    createAutoScalingGroup_targetGroupARNs,
    createAutoScalingGroup_capacityRebalance,
    createAutoScalingGroup_newInstancesProtectedFromScaleIn,
    createAutoScalingGroup_serviceLinkedRoleARN,
    createAutoScalingGroup_healthCheckGracePeriod,
    createAutoScalingGroup_terminationPolicies,
    createAutoScalingGroup_lifecycleHookSpecificationList,
    createAutoScalingGroup_autoScalingGroupName,
    createAutoScalingGroup_minSize,
    createAutoScalingGroup_maxSize,

    -- * Destructuring the Response
    CreateAutoScalingGroupResponse (..),
    newCreateAutoScalingGroupResponse,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateAutoScalingGroup' smart constructor.
data CreateAutoScalingGroup = CreateAutoScalingGroup'
  { -- | The desired capacity is the initial capacity of the Auto Scaling group
    -- at the time of its creation and the capacity it attempts to maintain. It
    -- can scale beyond this capacity if you configure auto scaling. This
    -- number must be greater than or equal to the minimum size of the group
    -- and less than or equal to the maximum size of the group. If you do not
    -- specify a desired capacity, the default is the minimum size of the
    -- group.
    desiredCapacity :: Core.Maybe Core.Int,
    -- | The ID of the instance used to base the launch configuration on. If
    -- specified, Amazon EC2 Auto Scaling uses the configuration values from
    -- the specified instance to create a new launch configuration. To get the
    -- instance ID, use the Amazon EC2
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeInstances.html DescribeInstances>
    -- API operation. For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-asg-from-instance.html Creating an Auto Scaling group using an EC2 instance>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    instanceId :: Core.Maybe Core.Text,
    -- | A list of Availability Zones where instances in the Auto Scaling group
    -- can be created. This parameter is optional if you specify one or more
    -- subnets for @VPCZoneIdentifier@.
    --
    -- Conditional: If your account supports EC2-Classic and VPC, this
    -- parameter is required to launch instances into EC2-Classic.
    availabilityZones :: Core.Maybe [Core.Text],
    -- | The name of an existing placement group into which to launch your
    -- instances, if any. A placement group is a logical grouping of instances
    -- within a single Availability Zone. You cannot specify multiple
    -- Availability Zones and a placement group. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement Groups>
    -- in the /Amazon EC2 User Guide for Linux Instances/.
    placementGroup :: Core.Maybe Core.Text,
    -- | The amount of time, in seconds, after a scaling activity completes
    -- before another scaling activity can start. The default value is @300@.
    -- This setting applies when using simple scaling policies, but not when
    -- using other scaling policies or scheduled scaling. For more information,
    -- see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/Cooldown.html Scaling cooldowns for Amazon EC2 Auto Scaling>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    defaultCooldown :: Core.Maybe Core.Int,
    -- | The maximum amount of time, in seconds, that an instance can be in
    -- service. The default is null. If specified, the value must be either 0
    -- or a number equal to or greater than 86,400 seconds (1 day). For more
    -- information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-max-instance-lifetime.html Replacing Auto Scaling instances based on maximum instance lifetime>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    maxInstanceLifetime :: Core.Maybe Core.Int,
    -- | Parameters used to specify the launch template and version to use to
    -- launch instances.
    --
    -- Conditional: You must specify either a launch template (@LaunchTemplate@
    -- or @MixedInstancesPolicy@) or a launch configuration
    -- (@LaunchConfigurationName@ or @InstanceId@).
    --
    -- The launch template that is specified must be configured for use with an
    -- Auto Scaling group. For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-launch-template.html Creating a launch template for an Auto Scaling group>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    launchTemplate :: Core.Maybe LaunchTemplateSpecification,
    -- | The name of the launch configuration to use to launch instances.
    --
    -- Conditional: You must specify either a launch template (@LaunchTemplate@
    -- or @MixedInstancesPolicy@) or a launch configuration
    -- (@LaunchConfigurationName@ or @InstanceId@).
    launchConfigurationName :: Core.Maybe Core.Text,
    -- | The service to use for the health checks. The valid values are @EC2@
    -- (default) and @ELB@. If you configure an Auto Scaling group to use load
    -- balancer (ELB) health checks, it considers the instance unhealthy if it
    -- fails either the EC2 status checks or the load balancer health checks.
    -- For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/healthcheck.html Health checks for Auto Scaling instances>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    healthCheckType :: Core.Maybe Core.Text,
    -- | An embedded object that specifies a mixed instances policy. The required
    -- parameters must be specified. If optional parameters are unspecified,
    -- their default values are used.
    --
    -- The policy includes parameters that not only define the distribution of
    -- On-Demand Instances and Spot Instances, the maximum price to pay for
    -- Spot Instances, and how the Auto Scaling group allocates instance types
    -- to fulfill On-Demand and Spot capacities, but also the parameters that
    -- specify the instance configuration information—the launch template and
    -- instance types. The policy can also include a weight for each instance
    -- type and different launch templates for individual instance types. For
    -- more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-purchase-options.html Auto Scaling groups with multiple instance types and purchase options>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    mixedInstancesPolicy :: Core.Maybe MixedInstancesPolicy,
    -- | One or more tags. You can tag your Auto Scaling group and propagate the
    -- tags to the Amazon EC2 instances it launches. Tags are not propagated to
    -- Amazon EBS volumes. To add tags to Amazon EBS volumes, specify the tags
    -- in a launch template but use caution. If the launch template specifies
    -- an instance tag with a key that is also specified for the Auto Scaling
    -- group, Amazon EC2 Auto Scaling overrides the value of that instance tag
    -- with the value specified by the Auto Scaling group. For more
    -- information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-tagging.html Tagging Auto Scaling groups and instances>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    tags :: Core.Maybe [Tag],
    -- | A list of Classic Load Balancers associated with this Auto Scaling
    -- group. For Application Load Balancers, Network Load Balancers, and
    -- Gateway Load Balancers, specify the @TargetGroupARNs@ property instead.
    loadBalancerNames :: Core.Maybe [Core.Text],
    -- | A comma-separated list of subnet IDs for a virtual private cloud (VPC)
    -- where instances in the Auto Scaling group can be created. If you specify
    -- @VPCZoneIdentifier@ with @AvailabilityZones@, the subnets that you
    -- specify for this parameter must reside in those Availability Zones.
    --
    -- Conditional: If your account supports EC2-Classic and VPC, this
    -- parameter is required to launch instances into a VPC.
    vPCZoneIdentifier :: Core.Maybe Core.Text,
    -- | The Amazon Resource Names (ARN) of the target groups to associate with
    -- the Auto Scaling group. Instances are registered as targets in a target
    -- group, and traffic is routed to the target group. For more information,
    -- see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-load-balancer.html Elastic Load Balancing and Amazon EC2 Auto Scaling>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    targetGroupARNs :: Core.Maybe [Core.Text],
    -- | Indicates whether Capacity Rebalancing is enabled. Otherwise, Capacity
    -- Rebalancing is disabled. When you turn on Capacity Rebalancing, Amazon
    -- EC2 Auto Scaling attempts to launch a Spot Instance whenever Amazon EC2
    -- notifies that a Spot Instance is at an elevated risk of interruption.
    -- After launching a new instance, it then terminates an old instance. For
    -- more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/capacity-rebalance.html Amazon EC2 Auto Scaling Capacity Rebalancing>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    capacityRebalance :: Core.Maybe Core.Bool,
    -- | Indicates whether newly launched instances are protected from
    -- termination by Amazon EC2 Auto Scaling when scaling in. For more
    -- information about preventing instances from terminating on scale in, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html#instance-protection Instance scale-in protection>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    newInstancesProtectedFromScaleIn' :: Core.Maybe Core.Bool,
    -- | The Amazon Resource Name (ARN) of the service-linked role that the Auto
    -- Scaling group uses to call other AWS services on your behalf. By
    -- default, Amazon EC2 Auto Scaling uses a service-linked role named
    -- AWSServiceRoleForAutoScaling, which it creates if it does not exist. For
    -- more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-service-linked-role.html Service-linked roles>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    serviceLinkedRoleARN :: Core.Maybe Core.Text,
    -- | The amount of time, in seconds, that Amazon EC2 Auto Scaling waits
    -- before checking the health status of an EC2 instance that has come into
    -- service. During this time, any health check failures for the instance
    -- are ignored. The default value is @0@. For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/healthcheck.html#health-check-grace-period Health check grace period>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    --
    -- Conditional: Required if you are adding an @ELB@ health check.
    healthCheckGracePeriod :: Core.Maybe Core.Int,
    -- | A policy or a list of policies that are used to select the instance to
    -- terminate. These policies are executed in the order that you list them.
    -- For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html Controlling which Auto Scaling instances terminate during scale in>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    terminationPolicies :: Core.Maybe [Core.Text],
    -- | One or more lifecycle hooks for the group, which specify actions to
    -- perform when Amazon EC2 Auto Scaling launches or terminates instances.
    lifecycleHookSpecificationList :: Core.Maybe [LifecycleHookSpecification],
    -- | The name of the Auto Scaling group. This name must be unique per Region
    -- per account.
    autoScalingGroupName :: Core.Text,
    -- | The minimum size of the group.
    minSize :: Core.Int,
    -- | The maximum size of the group.
    --
    -- With a mixed instances policy that uses instance weighting, Amazon EC2
    -- Auto Scaling may need to go above @MaxSize@ to meet your capacity
    -- requirements. In this event, Amazon EC2 Auto Scaling will never go above
    -- @MaxSize@ by more than your largest instance weight (weights that define
    -- how many units each instance contributes to the desired capacity of the
    -- group).
    maxSize :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateAutoScalingGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'desiredCapacity', 'createAutoScalingGroup_desiredCapacity' - The desired capacity is the initial capacity of the Auto Scaling group
-- at the time of its creation and the capacity it attempts to maintain. It
-- can scale beyond this capacity if you configure auto scaling. This
-- number must be greater than or equal to the minimum size of the group
-- and less than or equal to the maximum size of the group. If you do not
-- specify a desired capacity, the default is the minimum size of the
-- group.
--
-- 'instanceId', 'createAutoScalingGroup_instanceId' - The ID of the instance used to base the launch configuration on. If
-- specified, Amazon EC2 Auto Scaling uses the configuration values from
-- the specified instance to create a new launch configuration. To get the
-- instance ID, use the Amazon EC2
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeInstances.html DescribeInstances>
-- API operation. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-asg-from-instance.html Creating an Auto Scaling group using an EC2 instance>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- 'availabilityZones', 'createAutoScalingGroup_availabilityZones' - A list of Availability Zones where instances in the Auto Scaling group
-- can be created. This parameter is optional if you specify one or more
-- subnets for @VPCZoneIdentifier@.
--
-- Conditional: If your account supports EC2-Classic and VPC, this
-- parameter is required to launch instances into EC2-Classic.
--
-- 'placementGroup', 'createAutoScalingGroup_placementGroup' - The name of an existing placement group into which to launch your
-- instances, if any. A placement group is a logical grouping of instances
-- within a single Availability Zone. You cannot specify multiple
-- Availability Zones and a placement group. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement Groups>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- 'defaultCooldown', 'createAutoScalingGroup_defaultCooldown' - The amount of time, in seconds, after a scaling activity completes
-- before another scaling activity can start. The default value is @300@.
-- This setting applies when using simple scaling policies, but not when
-- using other scaling policies or scheduled scaling. For more information,
-- see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/Cooldown.html Scaling cooldowns for Amazon EC2 Auto Scaling>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- 'maxInstanceLifetime', 'createAutoScalingGroup_maxInstanceLifetime' - The maximum amount of time, in seconds, that an instance can be in
-- service. The default is null. If specified, the value must be either 0
-- or a number equal to or greater than 86,400 seconds (1 day). For more
-- information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-max-instance-lifetime.html Replacing Auto Scaling instances based on maximum instance lifetime>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- 'launchTemplate', 'createAutoScalingGroup_launchTemplate' - Parameters used to specify the launch template and version to use to
-- launch instances.
--
-- Conditional: You must specify either a launch template (@LaunchTemplate@
-- or @MixedInstancesPolicy@) or a launch configuration
-- (@LaunchConfigurationName@ or @InstanceId@).
--
-- The launch template that is specified must be configured for use with an
-- Auto Scaling group. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-launch-template.html Creating a launch template for an Auto Scaling group>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- 'launchConfigurationName', 'createAutoScalingGroup_launchConfigurationName' - The name of the launch configuration to use to launch instances.
--
-- Conditional: You must specify either a launch template (@LaunchTemplate@
-- or @MixedInstancesPolicy@) or a launch configuration
-- (@LaunchConfigurationName@ or @InstanceId@).
--
-- 'healthCheckType', 'createAutoScalingGroup_healthCheckType' - The service to use for the health checks. The valid values are @EC2@
-- (default) and @ELB@. If you configure an Auto Scaling group to use load
-- balancer (ELB) health checks, it considers the instance unhealthy if it
-- fails either the EC2 status checks or the load balancer health checks.
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/healthcheck.html Health checks for Auto Scaling instances>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- 'mixedInstancesPolicy', 'createAutoScalingGroup_mixedInstancesPolicy' - An embedded object that specifies a mixed instances policy. The required
-- parameters must be specified. If optional parameters are unspecified,
-- their default values are used.
--
-- The policy includes parameters that not only define the distribution of
-- On-Demand Instances and Spot Instances, the maximum price to pay for
-- Spot Instances, and how the Auto Scaling group allocates instance types
-- to fulfill On-Demand and Spot capacities, but also the parameters that
-- specify the instance configuration information—the launch template and
-- instance types. The policy can also include a weight for each instance
-- type and different launch templates for individual instance types. For
-- more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-purchase-options.html Auto Scaling groups with multiple instance types and purchase options>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- 'tags', 'createAutoScalingGroup_tags' - One or more tags. You can tag your Auto Scaling group and propagate the
-- tags to the Amazon EC2 instances it launches. Tags are not propagated to
-- Amazon EBS volumes. To add tags to Amazon EBS volumes, specify the tags
-- in a launch template but use caution. If the launch template specifies
-- an instance tag with a key that is also specified for the Auto Scaling
-- group, Amazon EC2 Auto Scaling overrides the value of that instance tag
-- with the value specified by the Auto Scaling group. For more
-- information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-tagging.html Tagging Auto Scaling groups and instances>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- 'loadBalancerNames', 'createAutoScalingGroup_loadBalancerNames' - A list of Classic Load Balancers associated with this Auto Scaling
-- group. For Application Load Balancers, Network Load Balancers, and
-- Gateway Load Balancers, specify the @TargetGroupARNs@ property instead.
--
-- 'vPCZoneIdentifier', 'createAutoScalingGroup_vPCZoneIdentifier' - A comma-separated list of subnet IDs for a virtual private cloud (VPC)
-- where instances in the Auto Scaling group can be created. If you specify
-- @VPCZoneIdentifier@ with @AvailabilityZones@, the subnets that you
-- specify for this parameter must reside in those Availability Zones.
--
-- Conditional: If your account supports EC2-Classic and VPC, this
-- parameter is required to launch instances into a VPC.
--
-- 'targetGroupARNs', 'createAutoScalingGroup_targetGroupARNs' - The Amazon Resource Names (ARN) of the target groups to associate with
-- the Auto Scaling group. Instances are registered as targets in a target
-- group, and traffic is routed to the target group. For more information,
-- see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-load-balancer.html Elastic Load Balancing and Amazon EC2 Auto Scaling>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- 'capacityRebalance', 'createAutoScalingGroup_capacityRebalance' - Indicates whether Capacity Rebalancing is enabled. Otherwise, Capacity
-- Rebalancing is disabled. When you turn on Capacity Rebalancing, Amazon
-- EC2 Auto Scaling attempts to launch a Spot Instance whenever Amazon EC2
-- notifies that a Spot Instance is at an elevated risk of interruption.
-- After launching a new instance, it then terminates an old instance. For
-- more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/capacity-rebalance.html Amazon EC2 Auto Scaling Capacity Rebalancing>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- 'newInstancesProtectedFromScaleIn'', 'createAutoScalingGroup_newInstancesProtectedFromScaleIn' - Indicates whether newly launched instances are protected from
-- termination by Amazon EC2 Auto Scaling when scaling in. For more
-- information about preventing instances from terminating on scale in, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html#instance-protection Instance scale-in protection>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- 'serviceLinkedRoleARN', 'createAutoScalingGroup_serviceLinkedRoleARN' - The Amazon Resource Name (ARN) of the service-linked role that the Auto
-- Scaling group uses to call other AWS services on your behalf. By
-- default, Amazon EC2 Auto Scaling uses a service-linked role named
-- AWSServiceRoleForAutoScaling, which it creates if it does not exist. For
-- more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-service-linked-role.html Service-linked roles>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- 'healthCheckGracePeriod', 'createAutoScalingGroup_healthCheckGracePeriod' - The amount of time, in seconds, that Amazon EC2 Auto Scaling waits
-- before checking the health status of an EC2 instance that has come into
-- service. During this time, any health check failures for the instance
-- are ignored. The default value is @0@. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/healthcheck.html#health-check-grace-period Health check grace period>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- Conditional: Required if you are adding an @ELB@ health check.
--
-- 'terminationPolicies', 'createAutoScalingGroup_terminationPolicies' - A policy or a list of policies that are used to select the instance to
-- terminate. These policies are executed in the order that you list them.
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html Controlling which Auto Scaling instances terminate during scale in>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- 'lifecycleHookSpecificationList', 'createAutoScalingGroup_lifecycleHookSpecificationList' - One or more lifecycle hooks for the group, which specify actions to
-- perform when Amazon EC2 Auto Scaling launches or terminates instances.
--
-- 'autoScalingGroupName', 'createAutoScalingGroup_autoScalingGroupName' - The name of the Auto Scaling group. This name must be unique per Region
-- per account.
--
-- 'minSize', 'createAutoScalingGroup_minSize' - The minimum size of the group.
--
-- 'maxSize', 'createAutoScalingGroup_maxSize' - The maximum size of the group.
--
-- With a mixed instances policy that uses instance weighting, Amazon EC2
-- Auto Scaling may need to go above @MaxSize@ to meet your capacity
-- requirements. In this event, Amazon EC2 Auto Scaling will never go above
-- @MaxSize@ by more than your largest instance weight (weights that define
-- how many units each instance contributes to the desired capacity of the
-- group).
newCreateAutoScalingGroup ::
  -- | 'autoScalingGroupName'
  Core.Text ->
  -- | 'minSize'
  Core.Int ->
  -- | 'maxSize'
  Core.Int ->
  CreateAutoScalingGroup
newCreateAutoScalingGroup
  pAutoScalingGroupName_
  pMinSize_
  pMaxSize_ =
    CreateAutoScalingGroup'
      { desiredCapacity =
          Core.Nothing,
        instanceId = Core.Nothing,
        availabilityZones = Core.Nothing,
        placementGroup = Core.Nothing,
        defaultCooldown = Core.Nothing,
        maxInstanceLifetime = Core.Nothing,
        launchTemplate = Core.Nothing,
        launchConfigurationName = Core.Nothing,
        healthCheckType = Core.Nothing,
        mixedInstancesPolicy = Core.Nothing,
        tags = Core.Nothing,
        loadBalancerNames = Core.Nothing,
        vPCZoneIdentifier = Core.Nothing,
        targetGroupARNs = Core.Nothing,
        capacityRebalance = Core.Nothing,
        newInstancesProtectedFromScaleIn' = Core.Nothing,
        serviceLinkedRoleARN = Core.Nothing,
        healthCheckGracePeriod = Core.Nothing,
        terminationPolicies = Core.Nothing,
        lifecycleHookSpecificationList = Core.Nothing,
        autoScalingGroupName = pAutoScalingGroupName_,
        minSize = pMinSize_,
        maxSize = pMaxSize_
      }

-- | The desired capacity is the initial capacity of the Auto Scaling group
-- at the time of its creation and the capacity it attempts to maintain. It
-- can scale beyond this capacity if you configure auto scaling. This
-- number must be greater than or equal to the minimum size of the group
-- and less than or equal to the maximum size of the group. If you do not
-- specify a desired capacity, the default is the minimum size of the
-- group.
createAutoScalingGroup_desiredCapacity :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe Core.Int)
createAutoScalingGroup_desiredCapacity = Lens.lens (\CreateAutoScalingGroup' {desiredCapacity} -> desiredCapacity) (\s@CreateAutoScalingGroup' {} a -> s {desiredCapacity = a} :: CreateAutoScalingGroup)

-- | The ID of the instance used to base the launch configuration on. If
-- specified, Amazon EC2 Auto Scaling uses the configuration values from
-- the specified instance to create a new launch configuration. To get the
-- instance ID, use the Amazon EC2
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeInstances.html DescribeInstances>
-- API operation. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-asg-from-instance.html Creating an Auto Scaling group using an EC2 instance>
-- in the /Amazon EC2 Auto Scaling User Guide/.
createAutoScalingGroup_instanceId :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe Core.Text)
createAutoScalingGroup_instanceId = Lens.lens (\CreateAutoScalingGroup' {instanceId} -> instanceId) (\s@CreateAutoScalingGroup' {} a -> s {instanceId = a} :: CreateAutoScalingGroup)

-- | A list of Availability Zones where instances in the Auto Scaling group
-- can be created. This parameter is optional if you specify one or more
-- subnets for @VPCZoneIdentifier@.
--
-- Conditional: If your account supports EC2-Classic and VPC, this
-- parameter is required to launch instances into EC2-Classic.
createAutoScalingGroup_availabilityZones :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe [Core.Text])
createAutoScalingGroup_availabilityZones = Lens.lens (\CreateAutoScalingGroup' {availabilityZones} -> availabilityZones) (\s@CreateAutoScalingGroup' {} a -> s {availabilityZones = a} :: CreateAutoScalingGroup) Core.. Lens.mapping Lens._Coerce

-- | The name of an existing placement group into which to launch your
-- instances, if any. A placement group is a logical grouping of instances
-- within a single Availability Zone. You cannot specify multiple
-- Availability Zones and a placement group. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement Groups>
-- in the /Amazon EC2 User Guide for Linux Instances/.
createAutoScalingGroup_placementGroup :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe Core.Text)
createAutoScalingGroup_placementGroup = Lens.lens (\CreateAutoScalingGroup' {placementGroup} -> placementGroup) (\s@CreateAutoScalingGroup' {} a -> s {placementGroup = a} :: CreateAutoScalingGroup)

-- | The amount of time, in seconds, after a scaling activity completes
-- before another scaling activity can start. The default value is @300@.
-- This setting applies when using simple scaling policies, but not when
-- using other scaling policies or scheduled scaling. For more information,
-- see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/Cooldown.html Scaling cooldowns for Amazon EC2 Auto Scaling>
-- in the /Amazon EC2 Auto Scaling User Guide/.
createAutoScalingGroup_defaultCooldown :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe Core.Int)
createAutoScalingGroup_defaultCooldown = Lens.lens (\CreateAutoScalingGroup' {defaultCooldown} -> defaultCooldown) (\s@CreateAutoScalingGroup' {} a -> s {defaultCooldown = a} :: CreateAutoScalingGroup)

-- | The maximum amount of time, in seconds, that an instance can be in
-- service. The default is null. If specified, the value must be either 0
-- or a number equal to or greater than 86,400 seconds (1 day). For more
-- information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-max-instance-lifetime.html Replacing Auto Scaling instances based on maximum instance lifetime>
-- in the /Amazon EC2 Auto Scaling User Guide/.
createAutoScalingGroup_maxInstanceLifetime :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe Core.Int)
createAutoScalingGroup_maxInstanceLifetime = Lens.lens (\CreateAutoScalingGroup' {maxInstanceLifetime} -> maxInstanceLifetime) (\s@CreateAutoScalingGroup' {} a -> s {maxInstanceLifetime = a} :: CreateAutoScalingGroup)

-- | Parameters used to specify the launch template and version to use to
-- launch instances.
--
-- Conditional: You must specify either a launch template (@LaunchTemplate@
-- or @MixedInstancesPolicy@) or a launch configuration
-- (@LaunchConfigurationName@ or @InstanceId@).
--
-- The launch template that is specified must be configured for use with an
-- Auto Scaling group. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-launch-template.html Creating a launch template for an Auto Scaling group>
-- in the /Amazon EC2 Auto Scaling User Guide/.
createAutoScalingGroup_launchTemplate :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe LaunchTemplateSpecification)
createAutoScalingGroup_launchTemplate = Lens.lens (\CreateAutoScalingGroup' {launchTemplate} -> launchTemplate) (\s@CreateAutoScalingGroup' {} a -> s {launchTemplate = a} :: CreateAutoScalingGroup)

-- | The name of the launch configuration to use to launch instances.
--
-- Conditional: You must specify either a launch template (@LaunchTemplate@
-- or @MixedInstancesPolicy@) or a launch configuration
-- (@LaunchConfigurationName@ or @InstanceId@).
createAutoScalingGroup_launchConfigurationName :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe Core.Text)
createAutoScalingGroup_launchConfigurationName = Lens.lens (\CreateAutoScalingGroup' {launchConfigurationName} -> launchConfigurationName) (\s@CreateAutoScalingGroup' {} a -> s {launchConfigurationName = a} :: CreateAutoScalingGroup)

-- | The service to use for the health checks. The valid values are @EC2@
-- (default) and @ELB@. If you configure an Auto Scaling group to use load
-- balancer (ELB) health checks, it considers the instance unhealthy if it
-- fails either the EC2 status checks or the load balancer health checks.
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/healthcheck.html Health checks for Auto Scaling instances>
-- in the /Amazon EC2 Auto Scaling User Guide/.
createAutoScalingGroup_healthCheckType :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe Core.Text)
createAutoScalingGroup_healthCheckType = Lens.lens (\CreateAutoScalingGroup' {healthCheckType} -> healthCheckType) (\s@CreateAutoScalingGroup' {} a -> s {healthCheckType = a} :: CreateAutoScalingGroup)

-- | An embedded object that specifies a mixed instances policy. The required
-- parameters must be specified. If optional parameters are unspecified,
-- their default values are used.
--
-- The policy includes parameters that not only define the distribution of
-- On-Demand Instances and Spot Instances, the maximum price to pay for
-- Spot Instances, and how the Auto Scaling group allocates instance types
-- to fulfill On-Demand and Spot capacities, but also the parameters that
-- specify the instance configuration information—the launch template and
-- instance types. The policy can also include a weight for each instance
-- type and different launch templates for individual instance types. For
-- more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-purchase-options.html Auto Scaling groups with multiple instance types and purchase options>
-- in the /Amazon EC2 Auto Scaling User Guide/.
createAutoScalingGroup_mixedInstancesPolicy :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe MixedInstancesPolicy)
createAutoScalingGroup_mixedInstancesPolicy = Lens.lens (\CreateAutoScalingGroup' {mixedInstancesPolicy} -> mixedInstancesPolicy) (\s@CreateAutoScalingGroup' {} a -> s {mixedInstancesPolicy = a} :: CreateAutoScalingGroup)

-- | One or more tags. You can tag your Auto Scaling group and propagate the
-- tags to the Amazon EC2 instances it launches. Tags are not propagated to
-- Amazon EBS volumes. To add tags to Amazon EBS volumes, specify the tags
-- in a launch template but use caution. If the launch template specifies
-- an instance tag with a key that is also specified for the Auto Scaling
-- group, Amazon EC2 Auto Scaling overrides the value of that instance tag
-- with the value specified by the Auto Scaling group. For more
-- information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-tagging.html Tagging Auto Scaling groups and instances>
-- in the /Amazon EC2 Auto Scaling User Guide/.
createAutoScalingGroup_tags :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe [Tag])
createAutoScalingGroup_tags = Lens.lens (\CreateAutoScalingGroup' {tags} -> tags) (\s@CreateAutoScalingGroup' {} a -> s {tags = a} :: CreateAutoScalingGroup) Core.. Lens.mapping Lens._Coerce

-- | A list of Classic Load Balancers associated with this Auto Scaling
-- group. For Application Load Balancers, Network Load Balancers, and
-- Gateway Load Balancers, specify the @TargetGroupARNs@ property instead.
createAutoScalingGroup_loadBalancerNames :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe [Core.Text])
createAutoScalingGroup_loadBalancerNames = Lens.lens (\CreateAutoScalingGroup' {loadBalancerNames} -> loadBalancerNames) (\s@CreateAutoScalingGroup' {} a -> s {loadBalancerNames = a} :: CreateAutoScalingGroup) Core.. Lens.mapping Lens._Coerce

-- | A comma-separated list of subnet IDs for a virtual private cloud (VPC)
-- where instances in the Auto Scaling group can be created. If you specify
-- @VPCZoneIdentifier@ with @AvailabilityZones@, the subnets that you
-- specify for this parameter must reside in those Availability Zones.
--
-- Conditional: If your account supports EC2-Classic and VPC, this
-- parameter is required to launch instances into a VPC.
createAutoScalingGroup_vPCZoneIdentifier :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe Core.Text)
createAutoScalingGroup_vPCZoneIdentifier = Lens.lens (\CreateAutoScalingGroup' {vPCZoneIdentifier} -> vPCZoneIdentifier) (\s@CreateAutoScalingGroup' {} a -> s {vPCZoneIdentifier = a} :: CreateAutoScalingGroup)

-- | The Amazon Resource Names (ARN) of the target groups to associate with
-- the Auto Scaling group. Instances are registered as targets in a target
-- group, and traffic is routed to the target group. For more information,
-- see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-load-balancer.html Elastic Load Balancing and Amazon EC2 Auto Scaling>
-- in the /Amazon EC2 Auto Scaling User Guide/.
createAutoScalingGroup_targetGroupARNs :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe [Core.Text])
createAutoScalingGroup_targetGroupARNs = Lens.lens (\CreateAutoScalingGroup' {targetGroupARNs} -> targetGroupARNs) (\s@CreateAutoScalingGroup' {} a -> s {targetGroupARNs = a} :: CreateAutoScalingGroup) Core.. Lens.mapping Lens._Coerce

-- | Indicates whether Capacity Rebalancing is enabled. Otherwise, Capacity
-- Rebalancing is disabled. When you turn on Capacity Rebalancing, Amazon
-- EC2 Auto Scaling attempts to launch a Spot Instance whenever Amazon EC2
-- notifies that a Spot Instance is at an elevated risk of interruption.
-- After launching a new instance, it then terminates an old instance. For
-- more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/capacity-rebalance.html Amazon EC2 Auto Scaling Capacity Rebalancing>
-- in the /Amazon EC2 Auto Scaling User Guide/.
createAutoScalingGroup_capacityRebalance :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe Core.Bool)
createAutoScalingGroup_capacityRebalance = Lens.lens (\CreateAutoScalingGroup' {capacityRebalance} -> capacityRebalance) (\s@CreateAutoScalingGroup' {} a -> s {capacityRebalance = a} :: CreateAutoScalingGroup)

-- | Indicates whether newly launched instances are protected from
-- termination by Amazon EC2 Auto Scaling when scaling in. For more
-- information about preventing instances from terminating on scale in, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html#instance-protection Instance scale-in protection>
-- in the /Amazon EC2 Auto Scaling User Guide/.
createAutoScalingGroup_newInstancesProtectedFromScaleIn :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe Core.Bool)
createAutoScalingGroup_newInstancesProtectedFromScaleIn = Lens.lens (\CreateAutoScalingGroup' {newInstancesProtectedFromScaleIn'} -> newInstancesProtectedFromScaleIn') (\s@CreateAutoScalingGroup' {} a -> s {newInstancesProtectedFromScaleIn' = a} :: CreateAutoScalingGroup)

-- | The Amazon Resource Name (ARN) of the service-linked role that the Auto
-- Scaling group uses to call other AWS services on your behalf. By
-- default, Amazon EC2 Auto Scaling uses a service-linked role named
-- AWSServiceRoleForAutoScaling, which it creates if it does not exist. For
-- more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-service-linked-role.html Service-linked roles>
-- in the /Amazon EC2 Auto Scaling User Guide/.
createAutoScalingGroup_serviceLinkedRoleARN :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe Core.Text)
createAutoScalingGroup_serviceLinkedRoleARN = Lens.lens (\CreateAutoScalingGroup' {serviceLinkedRoleARN} -> serviceLinkedRoleARN) (\s@CreateAutoScalingGroup' {} a -> s {serviceLinkedRoleARN = a} :: CreateAutoScalingGroup)

-- | The amount of time, in seconds, that Amazon EC2 Auto Scaling waits
-- before checking the health status of an EC2 instance that has come into
-- service. During this time, any health check failures for the instance
-- are ignored. The default value is @0@. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/healthcheck.html#health-check-grace-period Health check grace period>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- Conditional: Required if you are adding an @ELB@ health check.
createAutoScalingGroup_healthCheckGracePeriod :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe Core.Int)
createAutoScalingGroup_healthCheckGracePeriod = Lens.lens (\CreateAutoScalingGroup' {healthCheckGracePeriod} -> healthCheckGracePeriod) (\s@CreateAutoScalingGroup' {} a -> s {healthCheckGracePeriod = a} :: CreateAutoScalingGroup)

-- | A policy or a list of policies that are used to select the instance to
-- terminate. These policies are executed in the order that you list them.
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html Controlling which Auto Scaling instances terminate during scale in>
-- in the /Amazon EC2 Auto Scaling User Guide/.
createAutoScalingGroup_terminationPolicies :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe [Core.Text])
createAutoScalingGroup_terminationPolicies = Lens.lens (\CreateAutoScalingGroup' {terminationPolicies} -> terminationPolicies) (\s@CreateAutoScalingGroup' {} a -> s {terminationPolicies = a} :: CreateAutoScalingGroup) Core.. Lens.mapping Lens._Coerce

-- | One or more lifecycle hooks for the group, which specify actions to
-- perform when Amazon EC2 Auto Scaling launches or terminates instances.
createAutoScalingGroup_lifecycleHookSpecificationList :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe [LifecycleHookSpecification])
createAutoScalingGroup_lifecycleHookSpecificationList = Lens.lens (\CreateAutoScalingGroup' {lifecycleHookSpecificationList} -> lifecycleHookSpecificationList) (\s@CreateAutoScalingGroup' {} a -> s {lifecycleHookSpecificationList = a} :: CreateAutoScalingGroup) Core.. Lens.mapping Lens._Coerce

-- | The name of the Auto Scaling group. This name must be unique per Region
-- per account.
createAutoScalingGroup_autoScalingGroupName :: Lens.Lens' CreateAutoScalingGroup Core.Text
createAutoScalingGroup_autoScalingGroupName = Lens.lens (\CreateAutoScalingGroup' {autoScalingGroupName} -> autoScalingGroupName) (\s@CreateAutoScalingGroup' {} a -> s {autoScalingGroupName = a} :: CreateAutoScalingGroup)

-- | The minimum size of the group.
createAutoScalingGroup_minSize :: Lens.Lens' CreateAutoScalingGroup Core.Int
createAutoScalingGroup_minSize = Lens.lens (\CreateAutoScalingGroup' {minSize} -> minSize) (\s@CreateAutoScalingGroup' {} a -> s {minSize = a} :: CreateAutoScalingGroup)

-- | The maximum size of the group.
--
-- With a mixed instances policy that uses instance weighting, Amazon EC2
-- Auto Scaling may need to go above @MaxSize@ to meet your capacity
-- requirements. In this event, Amazon EC2 Auto Scaling will never go above
-- @MaxSize@ by more than your largest instance weight (weights that define
-- how many units each instance contributes to the desired capacity of the
-- group).
createAutoScalingGroup_maxSize :: Lens.Lens' CreateAutoScalingGroup Core.Int
createAutoScalingGroup_maxSize = Lens.lens (\CreateAutoScalingGroup' {maxSize} -> maxSize) (\s@CreateAutoScalingGroup' {} a -> s {maxSize = a} :: CreateAutoScalingGroup)

instance Core.AWSRequest CreateAutoScalingGroup where
  type
    AWSResponse CreateAutoScalingGroup =
      CreateAutoScalingGroupResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      CreateAutoScalingGroupResponse'

instance Core.Hashable CreateAutoScalingGroup

instance Core.NFData CreateAutoScalingGroup

instance Core.ToHeaders CreateAutoScalingGroup where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CreateAutoScalingGroup where
  toPath = Core.const "/"

instance Core.ToQuery CreateAutoScalingGroup where
  toQuery CreateAutoScalingGroup' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("CreateAutoScalingGroup" :: Core.ByteString),
        "Version" Core.=: ("2011-01-01" :: Core.ByteString),
        "DesiredCapacity" Core.=: desiredCapacity,
        "InstanceId" Core.=: instanceId,
        "AvailabilityZones"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Core.<$> availabilityZones
            ),
        "PlacementGroup" Core.=: placementGroup,
        "DefaultCooldown" Core.=: defaultCooldown,
        "MaxInstanceLifetime" Core.=: maxInstanceLifetime,
        "LaunchTemplate" Core.=: launchTemplate,
        "LaunchConfigurationName"
          Core.=: launchConfigurationName,
        "HealthCheckType" Core.=: healthCheckType,
        "MixedInstancesPolicy" Core.=: mixedInstancesPolicy,
        "Tags"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> tags),
        "LoadBalancerNames"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Core.<$> loadBalancerNames
            ),
        "VPCZoneIdentifier" Core.=: vPCZoneIdentifier,
        "TargetGroupARNs"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> targetGroupARNs),
        "CapacityRebalance" Core.=: capacityRebalance,
        "NewInstancesProtectedFromScaleIn"
          Core.=: newInstancesProtectedFromScaleIn',
        "ServiceLinkedRoleARN" Core.=: serviceLinkedRoleARN,
        "HealthCheckGracePeriod"
          Core.=: healthCheckGracePeriod,
        "TerminationPolicies"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Core.<$> terminationPolicies
            ),
        "LifecycleHookSpecificationList"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Core.<$> lifecycleHookSpecificationList
            ),
        "AutoScalingGroupName" Core.=: autoScalingGroupName,
        "MinSize" Core.=: minSize,
        "MaxSize" Core.=: maxSize
      ]

-- | /See:/ 'newCreateAutoScalingGroupResponse' smart constructor.
data CreateAutoScalingGroupResponse = CreateAutoScalingGroupResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateAutoScalingGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newCreateAutoScalingGroupResponse ::
  CreateAutoScalingGroupResponse
newCreateAutoScalingGroupResponse =
  CreateAutoScalingGroupResponse'

instance Core.NFData CreateAutoScalingGroupResponse
