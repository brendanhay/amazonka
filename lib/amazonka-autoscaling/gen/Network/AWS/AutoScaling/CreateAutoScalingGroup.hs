{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.CreateAutoScalingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Auto Scaling group with the specified name and attributes.
--
-- If you exceed your maximum limit of Auto Scaling groups, the call fails. To query this limit, call the 'DescribeAccountLimits' API. For information about updating this limit, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-account-limits.html Amazon EC2 Auto Scaling service quotas> in the /Amazon EC2 Auto Scaling User Guide/ .
-- For introductory exercises for creating an Auto Scaling group, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/GettingStartedTutorial.html Getting started with Amazon EC2 Auto Scaling> and <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-register-lbs-with-asg.html Tutorial: Set up a scaled and load-balanced application> in the /Amazon EC2 Auto Scaling User Guide/ . For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/AutoScalingGroup.html Auto Scaling groups> in the /Amazon EC2 Auto Scaling User Guide/ .
-- Every Auto Scaling group has three size parameters (@DesiredCapacity@ , @MaxSize@ , and @MinSize@ ). Usually, you set these sizes based on a specific number of instances. However, if you configure a mixed instances policy that defines weights for the instance types, you must specify these sizes with the same units that you use for weighting instances.
module Network.AWS.AutoScaling.CreateAutoScalingGroup
  ( -- * Creating a request
    CreateAutoScalingGroup (..),
    mkCreateAutoScalingGroup,

    -- ** Request lenses
    casgAutoScalingGroupName,
    casgMinSize,
    casgMaxSize,
    casgAvailabilityZones,
    casgCapacityRebalance,
    casgDefaultCooldown,
    casgDesiredCapacity,
    casgHealthCheckGracePeriod,
    casgHealthCheckType,
    casgInstanceId,
    casgLaunchConfigurationName,
    casgLaunchTemplate,
    casgLifecycleHookSpecificationList,
    casgLoadBalancerNames,
    casgMaxInstanceLifetime,
    casgMixedInstancesPolicy,
    casgNewInstancesProtectedFromScaleIn,
    casgPlacementGroup,
    casgServiceLinkedRoleARN,
    casgTags,
    casgTargetGroupARNs,
    casgTerminationPolicies,
    casgVPCZoneIdentifier,

    -- * Destructuring the response
    CreateAutoScalingGroupResponse (..),
    mkCreateAutoScalingGroupResponse,
  )
where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateAutoScalingGroup' smart constructor.
data CreateAutoScalingGroup = CreateAutoScalingGroup'
  { -- | The name of the Auto Scaling group. This name must be unique per Region per account.
    autoScalingGroupName :: Types.XmlStringMaxLen255,
    -- | The minimum size of the group.
    minSize :: Core.Int,
    -- | The maximum size of the group.
    maxSize :: Core.Int,
    -- | A list of Availability Zones where instances in the Auto Scaling group can be created. This parameter is optional if you specify one or more subnets for @VPCZoneIdentifier@ .
    --
    -- Conditional: If your account supports EC2-Classic and VPC, this parameter is required to launch instances into EC2-Classic.
    availabilityZones :: Core.Maybe (Core.NonEmpty Types.XmlStringMaxLen255),
    -- | Indicates whether Capacity Rebalancing is enabled. Otherwise, Capacity Rebalancing is disabled. When you turn on Capacity Rebalancing, Amazon EC2 Auto Scaling attempts to launch a Spot Instance whenever Amazon EC2 notifies that a Spot Instance is at an elevated risk of interruption. After launching a new instance, it then terminates an old instance. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/capacity-rebalance.html Amazon EC2 Auto Scaling Capacity Rebalancing> in the /Amazon EC2 Auto Scaling User Guide/ .
    capacityRebalance :: Core.Maybe Core.Bool,
    -- | The amount of time, in seconds, after a scaling activity completes before another scaling activity can start. The default value is @300@ . This setting applies when using simple scaling policies, but not when using other scaling policies or scheduled scaling. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/Cooldown.html Scaling cooldowns for Amazon EC2 Auto Scaling> in the /Amazon EC2 Auto Scaling User Guide/ .
    defaultCooldown :: Core.Maybe Core.Int,
    -- | The desired capacity is the initial capacity of the Auto Scaling group at the time of its creation and the capacity it attempts to maintain. It can scale beyond this capacity if you configure auto scaling. This number must be greater than or equal to the minimum size of the group and less than or equal to the maximum size of the group. If you do not specify a desired capacity, the default is the minimum size of the group.
    desiredCapacity :: Core.Maybe Core.Int,
    -- | The amount of time, in seconds, that Amazon EC2 Auto Scaling waits before checking the health status of an EC2 instance that has come into service. During this time, any health check failures for the instance are ignored. The default value is @0@ . For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/healthcheck.html#health-check-grace-period Health check grace period> in the /Amazon EC2 Auto Scaling User Guide/ .
    --
    -- Conditional: Required if you are adding an @ELB@ health check.
    healthCheckGracePeriod :: Core.Maybe Core.Int,
    -- | The service to use for the health checks. The valid values are @EC2@ (default) and @ELB@ . If you configure an Auto Scaling group to use load balancer (ELB) health checks, it considers the instance unhealthy if it fails either the EC2 status checks or the load balancer health checks. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/healthcheck.html Health checks for Auto Scaling instances> in the /Amazon EC2 Auto Scaling User Guide/ .
    healthCheckType :: Core.Maybe Types.HealthCheckType,
    -- | The ID of the instance used to base the launch configuration on. If specified, Amazon EC2 Auto Scaling uses the configuration values from the specified instance to create a new launch configuration. To get the instance ID, use the Amazon EC2 <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeInstances.html DescribeInstances> API operation. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-asg-from-instance.html Creating an Auto Scaling group using an EC2 instance> in the /Amazon EC2 Auto Scaling User Guide/ .
    instanceId :: Core.Maybe Types.InstanceId,
    -- | The name of the launch configuration to use to launch instances.
    --
    -- Conditional: You must specify either a launch template (@LaunchTemplate@ or @MixedInstancesPolicy@ ) or a launch configuration (@LaunchConfigurationName@ or @InstanceId@ ).
    launchConfigurationName :: Core.Maybe Types.ResourceName,
    -- | Parameters used to specify the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ec2-launchtemplate.html launch template> and version to use to launch instances.
    --
    -- Conditional: You must specify either a launch template (@LaunchTemplate@ or @MixedInstancesPolicy@ ) or a launch configuration (@LaunchConfigurationName@ or @InstanceId@ ).
    launchTemplate :: Core.Maybe Types.LaunchTemplateSpecification,
    -- | One or more lifecycle hooks for the group, which specify actions to perform when Amazon EC2 Auto Scaling launches or terminates instances.
    lifecycleHookSpecificationList :: Core.Maybe [Types.LifecycleHookSpecification],
    -- | A list of Classic Load Balancers associated with this Auto Scaling group. For Application Load Balancers, Network Load Balancers, and Gateway Load Balancers, specify the @TargetGroupARNs@ property instead.
    loadBalancerNames :: Core.Maybe [Types.XmlStringMaxLen255],
    -- | The maximum amount of time, in seconds, that an instance can be in service. The default is null. If specified, the value must be either 0 or a number equal to or greater than 86,400 seconds (1 day). For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-max-instance-lifetime.html Replacing Auto Scaling instances based on maximum instance lifetime> in the /Amazon EC2 Auto Scaling User Guide/ .
    maxInstanceLifetime :: Core.Maybe Core.Int,
    -- | An embedded object that specifies a mixed instances policy. The required parameters must be specified. If optional parameters are unspecified, their default values are used.
    --
    -- The policy includes parameters that not only define the distribution of On-Demand Instances and Spot Instances, the maximum price to pay for Spot Instances, and how the Auto Scaling group allocates instance types to fulfill On-Demand and Spot capacities, but also the parameters that specify the instance configuration information—the launch template and instance types. The policy can also include a weight for each instance type and different launch templates for individual instance types. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-purchase-options.html Auto Scaling groups with multiple instance types and purchase options> in the /Amazon EC2 Auto Scaling User Guide/ .
    mixedInstancesPolicy :: Core.Maybe Types.MixedInstancesPolicy,
    -- | Indicates whether newly launched instances are protected from termination by Amazon EC2 Auto Scaling when scaling in. For more information about preventing instances from terminating on scale in, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html#instance-protection Instance scale-in protection> in the /Amazon EC2 Auto Scaling User Guide/ .
    newInstancesProtectedFromScaleIn :: Core.Maybe Core.Bool,
    -- | The name of an existing placement group into which to launch your instances, if any. A placement group is a logical grouping of instances within a single Availability Zone. You cannot specify multiple Availability Zones and a placement group. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement Groups> in the /Amazon EC2 User Guide for Linux Instances/ .
    placementGroup :: Core.Maybe Types.XmlStringMaxLen255,
    -- | The Amazon Resource Name (ARN) of the service-linked role that the Auto Scaling group uses to call other AWS services on your behalf. By default, Amazon EC2 Auto Scaling uses a service-linked role named AWSServiceRoleForAutoScaling, which it creates if it does not exist. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-service-linked-role.html Service-linked roles> in the /Amazon EC2 Auto Scaling User Guide/ .
    serviceLinkedRoleARN :: Core.Maybe Types.ResourceName,
    -- | One or more tags. You can tag your Auto Scaling group and propagate the tags to the Amazon EC2 instances it launches. Tags are not propagated to Amazon EBS volumes. To add tags to Amazon EBS volumes, specify the tags in a launch template but use caution. If the launch template specifies an instance tag with a key that is also specified for the Auto Scaling group, Amazon EC2 Auto Scaling overrides the value of that instance tag with the value specified by the Auto Scaling group. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-tagging.html Tagging Auto Scaling groups and instances> in the /Amazon EC2 Auto Scaling User Guide/ .
    tags :: Core.Maybe [Types.Tag],
    -- | The Amazon Resource Names (ARN) of the target groups to associate with the Auto Scaling group. Instances are registered as targets in a target group, and traffic is routed to the target group. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-load-balancer.html Elastic Load Balancing and Amazon EC2 Auto Scaling> in the /Amazon EC2 Auto Scaling User Guide/ .
    targetGroupARNs :: Core.Maybe [Types.XmlStringMaxLen511],
    -- | A policy or a list of policies that are used to select the instance to terminate. These policies are executed in the order that you list them. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html Controlling which Auto Scaling instances terminate during scale in> in the /Amazon EC2 Auto Scaling User Guide/ .
    terminationPolicies :: Core.Maybe [Types.XmlStringMaxLen1600],
    -- | A comma-separated list of subnet IDs for a virtual private cloud (VPC) where instances in the Auto Scaling group can be created. If you specify @VPCZoneIdentifier@ with @AvailabilityZones@ , the subnets that you specify for this parameter must reside in those Availability Zones.
    --
    -- Conditional: If your account supports EC2-Classic and VPC, this parameter is required to launch instances into a VPC.
    vPCZoneIdentifier :: Core.Maybe Types.XmlStringMaxLen2047
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateAutoScalingGroup' value with any optional fields omitted.
mkCreateAutoScalingGroup ::
  -- | 'autoScalingGroupName'
  Types.XmlStringMaxLen255 ->
  -- | 'minSize'
  Core.Int ->
  -- | 'maxSize'
  Core.Int ->
  CreateAutoScalingGroup
mkCreateAutoScalingGroup autoScalingGroupName minSize maxSize =
  CreateAutoScalingGroup'
    { autoScalingGroupName,
      minSize,
      maxSize,
      availabilityZones = Core.Nothing,
      capacityRebalance = Core.Nothing,
      defaultCooldown = Core.Nothing,
      desiredCapacity = Core.Nothing,
      healthCheckGracePeriod = Core.Nothing,
      healthCheckType = Core.Nothing,
      instanceId = Core.Nothing,
      launchConfigurationName = Core.Nothing,
      launchTemplate = Core.Nothing,
      lifecycleHookSpecificationList = Core.Nothing,
      loadBalancerNames = Core.Nothing,
      maxInstanceLifetime = Core.Nothing,
      mixedInstancesPolicy = Core.Nothing,
      newInstancesProtectedFromScaleIn = Core.Nothing,
      placementGroup = Core.Nothing,
      serviceLinkedRoleARN = Core.Nothing,
      tags = Core.Nothing,
      targetGroupARNs = Core.Nothing,
      terminationPolicies = Core.Nothing,
      vPCZoneIdentifier = Core.Nothing
    }

-- | The name of the Auto Scaling group. This name must be unique per Region per account.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgAutoScalingGroupName :: Lens.Lens' CreateAutoScalingGroup Types.XmlStringMaxLen255
casgAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# DEPRECATED casgAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

-- | The minimum size of the group.
--
-- /Note:/ Consider using 'minSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgMinSize :: Lens.Lens' CreateAutoScalingGroup Core.Int
casgMinSize = Lens.field @"minSize"
{-# DEPRECATED casgMinSize "Use generic-lens or generic-optics with 'minSize' instead." #-}

-- | The maximum size of the group.
--
-- /Note:/ Consider using 'maxSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgMaxSize :: Lens.Lens' CreateAutoScalingGroup Core.Int
casgMaxSize = Lens.field @"maxSize"
{-# DEPRECATED casgMaxSize "Use generic-lens or generic-optics with 'maxSize' instead." #-}

-- | A list of Availability Zones where instances in the Auto Scaling group can be created. This parameter is optional if you specify one or more subnets for @VPCZoneIdentifier@ .
--
-- Conditional: If your account supports EC2-Classic and VPC, this parameter is required to launch instances into EC2-Classic.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgAvailabilityZones :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe (Core.NonEmpty Types.XmlStringMaxLen255))
casgAvailabilityZones = Lens.field @"availabilityZones"
{-# DEPRECATED casgAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | Indicates whether Capacity Rebalancing is enabled. Otherwise, Capacity Rebalancing is disabled. When you turn on Capacity Rebalancing, Amazon EC2 Auto Scaling attempts to launch a Spot Instance whenever Amazon EC2 notifies that a Spot Instance is at an elevated risk of interruption. After launching a new instance, it then terminates an old instance. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/capacity-rebalance.html Amazon EC2 Auto Scaling Capacity Rebalancing> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'capacityRebalance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgCapacityRebalance :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe Core.Bool)
casgCapacityRebalance = Lens.field @"capacityRebalance"
{-# DEPRECATED casgCapacityRebalance "Use generic-lens or generic-optics with 'capacityRebalance' instead." #-}

-- | The amount of time, in seconds, after a scaling activity completes before another scaling activity can start. The default value is @300@ . This setting applies when using simple scaling policies, but not when using other scaling policies or scheduled scaling. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/Cooldown.html Scaling cooldowns for Amazon EC2 Auto Scaling> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'defaultCooldown' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgDefaultCooldown :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe Core.Int)
casgDefaultCooldown = Lens.field @"defaultCooldown"
{-# DEPRECATED casgDefaultCooldown "Use generic-lens or generic-optics with 'defaultCooldown' instead." #-}

-- | The desired capacity is the initial capacity of the Auto Scaling group at the time of its creation and the capacity it attempts to maintain. It can scale beyond this capacity if you configure auto scaling. This number must be greater than or equal to the minimum size of the group and less than or equal to the maximum size of the group. If you do not specify a desired capacity, the default is the minimum size of the group.
--
-- /Note:/ Consider using 'desiredCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgDesiredCapacity :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe Core.Int)
casgDesiredCapacity = Lens.field @"desiredCapacity"
{-# DEPRECATED casgDesiredCapacity "Use generic-lens or generic-optics with 'desiredCapacity' instead." #-}

-- | The amount of time, in seconds, that Amazon EC2 Auto Scaling waits before checking the health status of an EC2 instance that has come into service. During this time, any health check failures for the instance are ignored. The default value is @0@ . For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/healthcheck.html#health-check-grace-period Health check grace period> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- Conditional: Required if you are adding an @ELB@ health check.
--
-- /Note:/ Consider using 'healthCheckGracePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgHealthCheckGracePeriod :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe Core.Int)
casgHealthCheckGracePeriod = Lens.field @"healthCheckGracePeriod"
{-# DEPRECATED casgHealthCheckGracePeriod "Use generic-lens or generic-optics with 'healthCheckGracePeriod' instead." #-}

-- | The service to use for the health checks. The valid values are @EC2@ (default) and @ELB@ . If you configure an Auto Scaling group to use load balancer (ELB) health checks, it considers the instance unhealthy if it fails either the EC2 status checks or the load balancer health checks. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/healthcheck.html Health checks for Auto Scaling instances> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'healthCheckType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgHealthCheckType :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe Types.HealthCheckType)
casgHealthCheckType = Lens.field @"healthCheckType"
{-# DEPRECATED casgHealthCheckType "Use generic-lens or generic-optics with 'healthCheckType' instead." #-}

-- | The ID of the instance used to base the launch configuration on. If specified, Amazon EC2 Auto Scaling uses the configuration values from the specified instance to create a new launch configuration. To get the instance ID, use the Amazon EC2 <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeInstances.html DescribeInstances> API operation. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-asg-from-instance.html Creating an Auto Scaling group using an EC2 instance> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgInstanceId :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe Types.InstanceId)
casgInstanceId = Lens.field @"instanceId"
{-# DEPRECATED casgInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The name of the launch configuration to use to launch instances.
--
-- Conditional: You must specify either a launch template (@LaunchTemplate@ or @MixedInstancesPolicy@ ) or a launch configuration (@LaunchConfigurationName@ or @InstanceId@ ).
--
-- /Note:/ Consider using 'launchConfigurationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgLaunchConfigurationName :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe Types.ResourceName)
casgLaunchConfigurationName = Lens.field @"launchConfigurationName"
{-# DEPRECATED casgLaunchConfigurationName "Use generic-lens or generic-optics with 'launchConfigurationName' instead." #-}

-- | Parameters used to specify the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ec2-launchtemplate.html launch template> and version to use to launch instances.
--
-- Conditional: You must specify either a launch template (@LaunchTemplate@ or @MixedInstancesPolicy@ ) or a launch configuration (@LaunchConfigurationName@ or @InstanceId@ ).
--
-- /Note:/ Consider using 'launchTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgLaunchTemplate :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe Types.LaunchTemplateSpecification)
casgLaunchTemplate = Lens.field @"launchTemplate"
{-# DEPRECATED casgLaunchTemplate "Use generic-lens or generic-optics with 'launchTemplate' instead." #-}

-- | One or more lifecycle hooks for the group, which specify actions to perform when Amazon EC2 Auto Scaling launches or terminates instances.
--
-- /Note:/ Consider using 'lifecycleHookSpecificationList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgLifecycleHookSpecificationList :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe [Types.LifecycleHookSpecification])
casgLifecycleHookSpecificationList = Lens.field @"lifecycleHookSpecificationList"
{-# DEPRECATED casgLifecycleHookSpecificationList "Use generic-lens or generic-optics with 'lifecycleHookSpecificationList' instead." #-}

-- | A list of Classic Load Balancers associated with this Auto Scaling group. For Application Load Balancers, Network Load Balancers, and Gateway Load Balancers, specify the @TargetGroupARNs@ property instead.
--
-- /Note:/ Consider using 'loadBalancerNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgLoadBalancerNames :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe [Types.XmlStringMaxLen255])
casgLoadBalancerNames = Lens.field @"loadBalancerNames"
{-# DEPRECATED casgLoadBalancerNames "Use generic-lens or generic-optics with 'loadBalancerNames' instead." #-}

-- | The maximum amount of time, in seconds, that an instance can be in service. The default is null. If specified, the value must be either 0 or a number equal to or greater than 86,400 seconds (1 day). For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-max-instance-lifetime.html Replacing Auto Scaling instances based on maximum instance lifetime> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'maxInstanceLifetime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgMaxInstanceLifetime :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe Core.Int)
casgMaxInstanceLifetime = Lens.field @"maxInstanceLifetime"
{-# DEPRECATED casgMaxInstanceLifetime "Use generic-lens or generic-optics with 'maxInstanceLifetime' instead." #-}

-- | An embedded object that specifies a mixed instances policy. The required parameters must be specified. If optional parameters are unspecified, their default values are used.
--
-- The policy includes parameters that not only define the distribution of On-Demand Instances and Spot Instances, the maximum price to pay for Spot Instances, and how the Auto Scaling group allocates instance types to fulfill On-Demand and Spot capacities, but also the parameters that specify the instance configuration information—the launch template and instance types. The policy can also include a weight for each instance type and different launch templates for individual instance types. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-purchase-options.html Auto Scaling groups with multiple instance types and purchase options> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'mixedInstancesPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgMixedInstancesPolicy :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe Types.MixedInstancesPolicy)
casgMixedInstancesPolicy = Lens.field @"mixedInstancesPolicy"
{-# DEPRECATED casgMixedInstancesPolicy "Use generic-lens or generic-optics with 'mixedInstancesPolicy' instead." #-}

-- | Indicates whether newly launched instances are protected from termination by Amazon EC2 Auto Scaling when scaling in. For more information about preventing instances from terminating on scale in, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html#instance-protection Instance scale-in protection> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'newInstancesProtectedFromScaleIn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgNewInstancesProtectedFromScaleIn :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe Core.Bool)
casgNewInstancesProtectedFromScaleIn = Lens.field @"newInstancesProtectedFromScaleIn"
{-# DEPRECATED casgNewInstancesProtectedFromScaleIn "Use generic-lens or generic-optics with 'newInstancesProtectedFromScaleIn' instead." #-}

-- | The name of an existing placement group into which to launch your instances, if any. A placement group is a logical grouping of instances within a single Availability Zone. You cannot specify multiple Availability Zones and a placement group. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement Groups> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- /Note:/ Consider using 'placementGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgPlacementGroup :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe Types.XmlStringMaxLen255)
casgPlacementGroup = Lens.field @"placementGroup"
{-# DEPRECATED casgPlacementGroup "Use generic-lens or generic-optics with 'placementGroup' instead." #-}

-- | The Amazon Resource Name (ARN) of the service-linked role that the Auto Scaling group uses to call other AWS services on your behalf. By default, Amazon EC2 Auto Scaling uses a service-linked role named AWSServiceRoleForAutoScaling, which it creates if it does not exist. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-service-linked-role.html Service-linked roles> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'serviceLinkedRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgServiceLinkedRoleARN :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe Types.ResourceName)
casgServiceLinkedRoleARN = Lens.field @"serviceLinkedRoleARN"
{-# DEPRECATED casgServiceLinkedRoleARN "Use generic-lens or generic-optics with 'serviceLinkedRoleARN' instead." #-}

-- | One or more tags. You can tag your Auto Scaling group and propagate the tags to the Amazon EC2 instances it launches. Tags are not propagated to Amazon EBS volumes. To add tags to Amazon EBS volumes, specify the tags in a launch template but use caution. If the launch template specifies an instance tag with a key that is also specified for the Auto Scaling group, Amazon EC2 Auto Scaling overrides the value of that instance tag with the value specified by the Auto Scaling group. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-tagging.html Tagging Auto Scaling groups and instances> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgTags :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe [Types.Tag])
casgTags = Lens.field @"tags"
{-# DEPRECATED casgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The Amazon Resource Names (ARN) of the target groups to associate with the Auto Scaling group. Instances are registered as targets in a target group, and traffic is routed to the target group. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-load-balancer.html Elastic Load Balancing and Amazon EC2 Auto Scaling> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'targetGroupARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgTargetGroupARNs :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe [Types.XmlStringMaxLen511])
casgTargetGroupARNs = Lens.field @"targetGroupARNs"
{-# DEPRECATED casgTargetGroupARNs "Use generic-lens or generic-optics with 'targetGroupARNs' instead." #-}

-- | A policy or a list of policies that are used to select the instance to terminate. These policies are executed in the order that you list them. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html Controlling which Auto Scaling instances terminate during scale in> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'terminationPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgTerminationPolicies :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe [Types.XmlStringMaxLen1600])
casgTerminationPolicies = Lens.field @"terminationPolicies"
{-# DEPRECATED casgTerminationPolicies "Use generic-lens or generic-optics with 'terminationPolicies' instead." #-}

-- | A comma-separated list of subnet IDs for a virtual private cloud (VPC) where instances in the Auto Scaling group can be created. If you specify @VPCZoneIdentifier@ with @AvailabilityZones@ , the subnets that you specify for this parameter must reside in those Availability Zones.
--
-- Conditional: If your account supports EC2-Classic and VPC, this parameter is required to launch instances into a VPC.
--
-- /Note:/ Consider using 'vPCZoneIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgVPCZoneIdentifier :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe Types.XmlStringMaxLen2047)
casgVPCZoneIdentifier = Lens.field @"vPCZoneIdentifier"
{-# DEPRECATED casgVPCZoneIdentifier "Use generic-lens or generic-optics with 'vPCZoneIdentifier' instead." #-}

instance Core.AWSRequest CreateAutoScalingGroup where
  type Rs CreateAutoScalingGroup = CreateAutoScalingGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "CreateAutoScalingGroup")
                Core.<> (Core.pure ("Version", "2011-01-01"))
                Core.<> (Core.toQueryValue "AutoScalingGroupName" autoScalingGroupName)
                Core.<> (Core.toQueryValue "MinSize" minSize)
                Core.<> (Core.toQueryValue "MaxSize" maxSize)
                Core.<> ( Core.toQueryValue
                            "AvailabilityZones"
                            (Core.toQueryList "member" Core.<$> availabilityZones)
                        )
                Core.<> (Core.toQueryValue "CapacityRebalance" Core.<$> capacityRebalance)
                Core.<> (Core.toQueryValue "DefaultCooldown" Core.<$> defaultCooldown)
                Core.<> (Core.toQueryValue "DesiredCapacity" Core.<$> desiredCapacity)
                Core.<> ( Core.toQueryValue "HealthCheckGracePeriod"
                            Core.<$> healthCheckGracePeriod
                        )
                Core.<> (Core.toQueryValue "HealthCheckType" Core.<$> healthCheckType)
                Core.<> (Core.toQueryValue "InstanceId" Core.<$> instanceId)
                Core.<> ( Core.toQueryValue "LaunchConfigurationName"
                            Core.<$> launchConfigurationName
                        )
                Core.<> (Core.toQueryValue "LaunchTemplate" Core.<$> launchTemplate)
                Core.<> ( Core.toQueryValue
                            "LifecycleHookSpecificationList"
                            ( Core.toQueryList "member"
                                Core.<$> lifecycleHookSpecificationList
                            )
                        )
                Core.<> ( Core.toQueryValue
                            "LoadBalancerNames"
                            (Core.toQueryList "member" Core.<$> loadBalancerNames)
                        )
                Core.<> ( Core.toQueryValue "MaxInstanceLifetime"
                            Core.<$> maxInstanceLifetime
                        )
                Core.<> ( Core.toQueryValue "MixedInstancesPolicy"
                            Core.<$> mixedInstancesPolicy
                        )
                Core.<> ( Core.toQueryValue "NewInstancesProtectedFromScaleIn"
                            Core.<$> newInstancesProtectedFromScaleIn
                        )
                Core.<> (Core.toQueryValue "PlacementGroup" Core.<$> placementGroup)
                Core.<> ( Core.toQueryValue "ServiceLinkedRoleARN"
                            Core.<$> serviceLinkedRoleARN
                        )
                Core.<> ( Core.toQueryValue
                            "Tags"
                            (Core.toQueryList "member" Core.<$> tags)
                        )
                Core.<> ( Core.toQueryValue
                            "TargetGroupARNs"
                            (Core.toQueryList "member" Core.<$> targetGroupARNs)
                        )
                Core.<> ( Core.toQueryValue
                            "TerminationPolicies"
                            (Core.toQueryList "member" Core.<$> terminationPolicies)
                        )
                Core.<> ( Core.toQueryValue "VPCZoneIdentifier"
                            Core.<$> vPCZoneIdentifier
                        )
            )
      }
  response = Response.receiveNull CreateAutoScalingGroupResponse'

-- | /See:/ 'mkCreateAutoScalingGroupResponse' smart constructor.
data CreateAutoScalingGroupResponse = CreateAutoScalingGroupResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateAutoScalingGroupResponse' value with any optional fields omitted.
mkCreateAutoScalingGroupResponse ::
  CreateAutoScalingGroupResponse
mkCreateAutoScalingGroupResponse = CreateAutoScalingGroupResponse'
