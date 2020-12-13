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
    casgInstanceId,
    casgTerminationPolicies,
    casgHealthCheckGracePeriod,
    casgServiceLinkedRoleARN,
    casgNewInstancesProtectedFromScaleIn,
    casgVPCZoneIdentifier,
    casgTargetGroupARNs,
    casgMaxInstanceLifetime,
    casgDefaultCooldown,
    casgMaxSize,
    casgAvailabilityZones,
    casgDesiredCapacity,
    casgMixedInstancesPolicy,
    casgMinSize,
    casgAutoScalingGroupName,
    casgLaunchConfigurationName,
    casgLifecycleHookSpecificationList,
    casgHealthCheckType,
    casgLaunchTemplate,
    casgCapacityRebalance,
    casgPlacementGroup,
    casgLoadBalancerNames,
    casgTags,

    -- * Destructuring the response
    CreateAutoScalingGroupResponse (..),
    mkCreateAutoScalingGroupResponse,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateAutoScalingGroup' smart constructor.
data CreateAutoScalingGroup = CreateAutoScalingGroup'
  { -- | The ID of the instance used to base the launch configuration on. If specified, Amazon EC2 Auto Scaling uses the configuration values from the specified instance to create a new launch configuration. To get the instance ID, use the Amazon EC2 <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeInstances.html DescribeInstances> API operation. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-asg-from-instance.html Creating an Auto Scaling group using an EC2 instance> in the /Amazon EC2 Auto Scaling User Guide/ .
    instanceId :: Lude.Maybe Lude.Text,
    -- | A policy or a list of policies that are used to select the instance to terminate. These policies are executed in the order that you list them. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html Controlling which Auto Scaling instances terminate during scale in> in the /Amazon EC2 Auto Scaling User Guide/ .
    terminationPolicies :: Lude.Maybe [Lude.Text],
    -- | The amount of time, in seconds, that Amazon EC2 Auto Scaling waits before checking the health status of an EC2 instance that has come into service. During this time, any health check failures for the instance are ignored. The default value is @0@ . For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/healthcheck.html#health-check-grace-period Health check grace period> in the /Amazon EC2 Auto Scaling User Guide/ .
    --
    -- Conditional: Required if you are adding an @ELB@ health check.
    healthCheckGracePeriod :: Lude.Maybe Lude.Int,
    -- | The Amazon Resource Name (ARN) of the service-linked role that the Auto Scaling group uses to call other AWS services on your behalf. By default, Amazon EC2 Auto Scaling uses a service-linked role named AWSServiceRoleForAutoScaling, which it creates if it does not exist. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-service-linked-role.html Service-linked roles> in the /Amazon EC2 Auto Scaling User Guide/ .
    serviceLinkedRoleARN :: Lude.Maybe Lude.Text,
    -- | Indicates whether newly launched instances are protected from termination by Amazon EC2 Auto Scaling when scaling in. For more information about preventing instances from terminating on scale in, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html#instance-protection Instance scale-in protection> in the /Amazon EC2 Auto Scaling User Guide/ .
    newInstancesProtectedFromScaleIn :: Lude.Maybe Lude.Bool,
    -- | A comma-separated list of subnet IDs for a virtual private cloud (VPC) where instances in the Auto Scaling group can be created. If you specify @VPCZoneIdentifier@ with @AvailabilityZones@ , the subnets that you specify for this parameter must reside in those Availability Zones.
    --
    -- Conditional: If your account supports EC2-Classic and VPC, this parameter is required to launch instances into a VPC.
    vpcZoneIdentifier :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Names (ARN) of the target groups to associate with the Auto Scaling group. Instances are registered as targets in a target group, and traffic is routed to the target group. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-load-balancer.html Elastic Load Balancing and Amazon EC2 Auto Scaling> in the /Amazon EC2 Auto Scaling User Guide/ .
    targetGroupARNs :: Lude.Maybe [Lude.Text],
    -- | The maximum amount of time, in seconds, that an instance can be in service. The default is null. If specified, the value must be either 0 or a number equal to or greater than 86,400 seconds (1 day). For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-max-instance-lifetime.html Replacing Auto Scaling instances based on maximum instance lifetime> in the /Amazon EC2 Auto Scaling User Guide/ .
    maxInstanceLifetime :: Lude.Maybe Lude.Int,
    -- | The amount of time, in seconds, after a scaling activity completes before another scaling activity can start. The default value is @300@ . This setting applies when using simple scaling policies, but not when using other scaling policies or scheduled scaling. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/Cooldown.html Scaling cooldowns for Amazon EC2 Auto Scaling> in the /Amazon EC2 Auto Scaling User Guide/ .
    defaultCooldown :: Lude.Maybe Lude.Int,
    -- | The maximum size of the group.
    maxSize :: Lude.Int,
    -- | A list of Availability Zones where instances in the Auto Scaling group can be created. This parameter is optional if you specify one or more subnets for @VPCZoneIdentifier@ .
    --
    -- Conditional: If your account supports EC2-Classic and VPC, this parameter is required to launch instances into EC2-Classic.
    availabilityZones :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    -- | The desired capacity is the initial capacity of the Auto Scaling group at the time of its creation and the capacity it attempts to maintain. It can scale beyond this capacity if you configure auto scaling. This number must be greater than or equal to the minimum size of the group and less than or equal to the maximum size of the group. If you do not specify a desired capacity, the default is the minimum size of the group.
    desiredCapacity :: Lude.Maybe Lude.Int,
    -- | An embedded object that specifies a mixed instances policy. The required parameters must be specified. If optional parameters are unspecified, their default values are used.
    --
    -- The policy includes parameters that not only define the distribution of On-Demand Instances and Spot Instances, the maximum price to pay for Spot Instances, and how the Auto Scaling group allocates instance types to fulfill On-Demand and Spot capacities, but also the parameters that specify the instance configuration information—the launch template and instance types. The policy can also include a weight for each instance type and different launch templates for individual instance types. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-purchase-options.html Auto Scaling groups with multiple instance types and purchase options> in the /Amazon EC2 Auto Scaling User Guide/ .
    mixedInstancesPolicy :: Lude.Maybe MixedInstancesPolicy,
    -- | The minimum size of the group.
    minSize :: Lude.Int,
    -- | The name of the Auto Scaling group. This name must be unique per Region per account.
    autoScalingGroupName :: Lude.Text,
    -- | The name of the launch configuration to use to launch instances.
    --
    -- Conditional: You must specify either a launch template (@LaunchTemplate@ or @MixedInstancesPolicy@ ) or a launch configuration (@LaunchConfigurationName@ or @InstanceId@ ).
    launchConfigurationName :: Lude.Maybe Lude.Text,
    -- | One or more lifecycle hooks for the group, which specify actions to perform when Amazon EC2 Auto Scaling launches or terminates instances.
    lifecycleHookSpecificationList :: Lude.Maybe [LifecycleHookSpecification],
    -- | The service to use for the health checks. The valid values are @EC2@ (default) and @ELB@ . If you configure an Auto Scaling group to use load balancer (ELB) health checks, it considers the instance unhealthy if it fails either the EC2 status checks or the load balancer health checks. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/healthcheck.html Health checks for Auto Scaling instances> in the /Amazon EC2 Auto Scaling User Guide/ .
    healthCheckType :: Lude.Maybe Lude.Text,
    -- | Parameters used to specify the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ec2-launchtemplate.html launch template> and version to use to launch instances.
    --
    -- Conditional: You must specify either a launch template (@LaunchTemplate@ or @MixedInstancesPolicy@ ) or a launch configuration (@LaunchConfigurationName@ or @InstanceId@ ).
    launchTemplate :: Lude.Maybe LaunchTemplateSpecification,
    -- | Indicates whether Capacity Rebalancing is enabled. Otherwise, Capacity Rebalancing is disabled. When you turn on Capacity Rebalancing, Amazon EC2 Auto Scaling attempts to launch a Spot Instance whenever Amazon EC2 notifies that a Spot Instance is at an elevated risk of interruption. After launching a new instance, it then terminates an old instance. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/capacity-rebalance.html Amazon EC2 Auto Scaling Capacity Rebalancing> in the /Amazon EC2 Auto Scaling User Guide/ .
    capacityRebalance :: Lude.Maybe Lude.Bool,
    -- | The name of an existing placement group into which to launch your instances, if any. A placement group is a logical grouping of instances within a single Availability Zone. You cannot specify multiple Availability Zones and a placement group. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement Groups> in the /Amazon EC2 User Guide for Linux Instances/ .
    placementGroup :: Lude.Maybe Lude.Text,
    -- | A list of Classic Load Balancers associated with this Auto Scaling group. For Application Load Balancers, Network Load Balancers, and Gateway Load Balancers, specify the @TargetGroupARNs@ property instead.
    loadBalancerNames :: Lude.Maybe [Lude.Text],
    -- | One or more tags. You can tag your Auto Scaling group and propagate the tags to the Amazon EC2 instances it launches. Tags are not propagated to Amazon EBS volumes. To add tags to Amazon EBS volumes, specify the tags in a launch template but use caution. If the launch template specifies an instance tag with a key that is also specified for the Auto Scaling group, Amazon EC2 Auto Scaling overrides the value of that instance tag with the value specified by the Auto Scaling group. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-tagging.html Tagging Auto Scaling groups and instances> in the /Amazon EC2 Auto Scaling User Guide/ .
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateAutoScalingGroup' with the minimum fields required to make a request.
--
-- * 'instanceId' - The ID of the instance used to base the launch configuration on. If specified, Amazon EC2 Auto Scaling uses the configuration values from the specified instance to create a new launch configuration. To get the instance ID, use the Amazon EC2 <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeInstances.html DescribeInstances> API operation. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-asg-from-instance.html Creating an Auto Scaling group using an EC2 instance> in the /Amazon EC2 Auto Scaling User Guide/ .
-- * 'terminationPolicies' - A policy or a list of policies that are used to select the instance to terminate. These policies are executed in the order that you list them. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html Controlling which Auto Scaling instances terminate during scale in> in the /Amazon EC2 Auto Scaling User Guide/ .
-- * 'healthCheckGracePeriod' - The amount of time, in seconds, that Amazon EC2 Auto Scaling waits before checking the health status of an EC2 instance that has come into service. During this time, any health check failures for the instance are ignored. The default value is @0@ . For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/healthcheck.html#health-check-grace-period Health check grace period> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- Conditional: Required if you are adding an @ELB@ health check.
-- * 'serviceLinkedRoleARN' - The Amazon Resource Name (ARN) of the service-linked role that the Auto Scaling group uses to call other AWS services on your behalf. By default, Amazon EC2 Auto Scaling uses a service-linked role named AWSServiceRoleForAutoScaling, which it creates if it does not exist. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-service-linked-role.html Service-linked roles> in the /Amazon EC2 Auto Scaling User Guide/ .
-- * 'newInstancesProtectedFromScaleIn' - Indicates whether newly launched instances are protected from termination by Amazon EC2 Auto Scaling when scaling in. For more information about preventing instances from terminating on scale in, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html#instance-protection Instance scale-in protection> in the /Amazon EC2 Auto Scaling User Guide/ .
-- * 'vpcZoneIdentifier' - A comma-separated list of subnet IDs for a virtual private cloud (VPC) where instances in the Auto Scaling group can be created. If you specify @VPCZoneIdentifier@ with @AvailabilityZones@ , the subnets that you specify for this parameter must reside in those Availability Zones.
--
-- Conditional: If your account supports EC2-Classic and VPC, this parameter is required to launch instances into a VPC.
-- * 'targetGroupARNs' - The Amazon Resource Names (ARN) of the target groups to associate with the Auto Scaling group. Instances are registered as targets in a target group, and traffic is routed to the target group. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-load-balancer.html Elastic Load Balancing and Amazon EC2 Auto Scaling> in the /Amazon EC2 Auto Scaling User Guide/ .
-- * 'maxInstanceLifetime' - The maximum amount of time, in seconds, that an instance can be in service. The default is null. If specified, the value must be either 0 or a number equal to or greater than 86,400 seconds (1 day). For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-max-instance-lifetime.html Replacing Auto Scaling instances based on maximum instance lifetime> in the /Amazon EC2 Auto Scaling User Guide/ .
-- * 'defaultCooldown' - The amount of time, in seconds, after a scaling activity completes before another scaling activity can start. The default value is @300@ . This setting applies when using simple scaling policies, but not when using other scaling policies or scheduled scaling. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/Cooldown.html Scaling cooldowns for Amazon EC2 Auto Scaling> in the /Amazon EC2 Auto Scaling User Guide/ .
-- * 'maxSize' - The maximum size of the group.
-- * 'availabilityZones' - A list of Availability Zones where instances in the Auto Scaling group can be created. This parameter is optional if you specify one or more subnets for @VPCZoneIdentifier@ .
--
-- Conditional: If your account supports EC2-Classic and VPC, this parameter is required to launch instances into EC2-Classic.
-- * 'desiredCapacity' - The desired capacity is the initial capacity of the Auto Scaling group at the time of its creation and the capacity it attempts to maintain. It can scale beyond this capacity if you configure auto scaling. This number must be greater than or equal to the minimum size of the group and less than or equal to the maximum size of the group. If you do not specify a desired capacity, the default is the minimum size of the group.
-- * 'mixedInstancesPolicy' - An embedded object that specifies a mixed instances policy. The required parameters must be specified. If optional parameters are unspecified, their default values are used.
--
-- The policy includes parameters that not only define the distribution of On-Demand Instances and Spot Instances, the maximum price to pay for Spot Instances, and how the Auto Scaling group allocates instance types to fulfill On-Demand and Spot capacities, but also the parameters that specify the instance configuration information—the launch template and instance types. The policy can also include a weight for each instance type and different launch templates for individual instance types. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-purchase-options.html Auto Scaling groups with multiple instance types and purchase options> in the /Amazon EC2 Auto Scaling User Guide/ .
-- * 'minSize' - The minimum size of the group.
-- * 'autoScalingGroupName' - The name of the Auto Scaling group. This name must be unique per Region per account.
-- * 'launchConfigurationName' - The name of the launch configuration to use to launch instances.
--
-- Conditional: You must specify either a launch template (@LaunchTemplate@ or @MixedInstancesPolicy@ ) or a launch configuration (@LaunchConfigurationName@ or @InstanceId@ ).
-- * 'lifecycleHookSpecificationList' - One or more lifecycle hooks for the group, which specify actions to perform when Amazon EC2 Auto Scaling launches or terminates instances.
-- * 'healthCheckType' - The service to use for the health checks. The valid values are @EC2@ (default) and @ELB@ . If you configure an Auto Scaling group to use load balancer (ELB) health checks, it considers the instance unhealthy if it fails either the EC2 status checks or the load balancer health checks. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/healthcheck.html Health checks for Auto Scaling instances> in the /Amazon EC2 Auto Scaling User Guide/ .
-- * 'launchTemplate' - Parameters used to specify the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ec2-launchtemplate.html launch template> and version to use to launch instances.
--
-- Conditional: You must specify either a launch template (@LaunchTemplate@ or @MixedInstancesPolicy@ ) or a launch configuration (@LaunchConfigurationName@ or @InstanceId@ ).
-- * 'capacityRebalance' - Indicates whether Capacity Rebalancing is enabled. Otherwise, Capacity Rebalancing is disabled. When you turn on Capacity Rebalancing, Amazon EC2 Auto Scaling attempts to launch a Spot Instance whenever Amazon EC2 notifies that a Spot Instance is at an elevated risk of interruption. After launching a new instance, it then terminates an old instance. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/capacity-rebalance.html Amazon EC2 Auto Scaling Capacity Rebalancing> in the /Amazon EC2 Auto Scaling User Guide/ .
-- * 'placementGroup' - The name of an existing placement group into which to launch your instances, if any. A placement group is a logical grouping of instances within a single Availability Zone. You cannot specify multiple Availability Zones and a placement group. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement Groups> in the /Amazon EC2 User Guide for Linux Instances/ .
-- * 'loadBalancerNames' - A list of Classic Load Balancers associated with this Auto Scaling group. For Application Load Balancers, Network Load Balancers, and Gateway Load Balancers, specify the @TargetGroupARNs@ property instead.
-- * 'tags' - One or more tags. You can tag your Auto Scaling group and propagate the tags to the Amazon EC2 instances it launches. Tags are not propagated to Amazon EBS volumes. To add tags to Amazon EBS volumes, specify the tags in a launch template but use caution. If the launch template specifies an instance tag with a key that is also specified for the Auto Scaling group, Amazon EC2 Auto Scaling overrides the value of that instance tag with the value specified by the Auto Scaling group. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-tagging.html Tagging Auto Scaling groups and instances> in the /Amazon EC2 Auto Scaling User Guide/ .
mkCreateAutoScalingGroup ::
  -- | 'maxSize'
  Lude.Int ->
  -- | 'minSize'
  Lude.Int ->
  -- | 'autoScalingGroupName'
  Lude.Text ->
  CreateAutoScalingGroup
mkCreateAutoScalingGroup pMaxSize_ pMinSize_ pAutoScalingGroupName_ =
  CreateAutoScalingGroup'
    { instanceId = Lude.Nothing,
      terminationPolicies = Lude.Nothing,
      healthCheckGracePeriod = Lude.Nothing,
      serviceLinkedRoleARN = Lude.Nothing,
      newInstancesProtectedFromScaleIn = Lude.Nothing,
      vpcZoneIdentifier = Lude.Nothing,
      targetGroupARNs = Lude.Nothing,
      maxInstanceLifetime = Lude.Nothing,
      defaultCooldown = Lude.Nothing,
      maxSize = pMaxSize_,
      availabilityZones = Lude.Nothing,
      desiredCapacity = Lude.Nothing,
      mixedInstancesPolicy = Lude.Nothing,
      minSize = pMinSize_,
      autoScalingGroupName = pAutoScalingGroupName_,
      launchConfigurationName = Lude.Nothing,
      lifecycleHookSpecificationList = Lude.Nothing,
      healthCheckType = Lude.Nothing,
      launchTemplate = Lude.Nothing,
      capacityRebalance = Lude.Nothing,
      placementGroup = Lude.Nothing,
      loadBalancerNames = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The ID of the instance used to base the launch configuration on. If specified, Amazon EC2 Auto Scaling uses the configuration values from the specified instance to create a new launch configuration. To get the instance ID, use the Amazon EC2 <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeInstances.html DescribeInstances> API operation. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-asg-from-instance.html Creating an Auto Scaling group using an EC2 instance> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgInstanceId :: Lens.Lens' CreateAutoScalingGroup (Lude.Maybe Lude.Text)
casgInstanceId = Lens.lens (instanceId :: CreateAutoScalingGroup -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: CreateAutoScalingGroup)
{-# DEPRECATED casgInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | A policy or a list of policies that are used to select the instance to terminate. These policies are executed in the order that you list them. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html Controlling which Auto Scaling instances terminate during scale in> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'terminationPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgTerminationPolicies :: Lens.Lens' CreateAutoScalingGroup (Lude.Maybe [Lude.Text])
casgTerminationPolicies = Lens.lens (terminationPolicies :: CreateAutoScalingGroup -> Lude.Maybe [Lude.Text]) (\s a -> s {terminationPolicies = a} :: CreateAutoScalingGroup)
{-# DEPRECATED casgTerminationPolicies "Use generic-lens or generic-optics with 'terminationPolicies' instead." #-}

-- | The amount of time, in seconds, that Amazon EC2 Auto Scaling waits before checking the health status of an EC2 instance that has come into service. During this time, any health check failures for the instance are ignored. The default value is @0@ . For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/healthcheck.html#health-check-grace-period Health check grace period> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- Conditional: Required if you are adding an @ELB@ health check.
--
-- /Note:/ Consider using 'healthCheckGracePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgHealthCheckGracePeriod :: Lens.Lens' CreateAutoScalingGroup (Lude.Maybe Lude.Int)
casgHealthCheckGracePeriod = Lens.lens (healthCheckGracePeriod :: CreateAutoScalingGroup -> Lude.Maybe Lude.Int) (\s a -> s {healthCheckGracePeriod = a} :: CreateAutoScalingGroup)
{-# DEPRECATED casgHealthCheckGracePeriod "Use generic-lens or generic-optics with 'healthCheckGracePeriod' instead." #-}

-- | The Amazon Resource Name (ARN) of the service-linked role that the Auto Scaling group uses to call other AWS services on your behalf. By default, Amazon EC2 Auto Scaling uses a service-linked role named AWSServiceRoleForAutoScaling, which it creates if it does not exist. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-service-linked-role.html Service-linked roles> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'serviceLinkedRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgServiceLinkedRoleARN :: Lens.Lens' CreateAutoScalingGroup (Lude.Maybe Lude.Text)
casgServiceLinkedRoleARN = Lens.lens (serviceLinkedRoleARN :: CreateAutoScalingGroup -> Lude.Maybe Lude.Text) (\s a -> s {serviceLinkedRoleARN = a} :: CreateAutoScalingGroup)
{-# DEPRECATED casgServiceLinkedRoleARN "Use generic-lens or generic-optics with 'serviceLinkedRoleARN' instead." #-}

-- | Indicates whether newly launched instances are protected from termination by Amazon EC2 Auto Scaling when scaling in. For more information about preventing instances from terminating on scale in, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html#instance-protection Instance scale-in protection> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'newInstancesProtectedFromScaleIn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgNewInstancesProtectedFromScaleIn :: Lens.Lens' CreateAutoScalingGroup (Lude.Maybe Lude.Bool)
casgNewInstancesProtectedFromScaleIn = Lens.lens (newInstancesProtectedFromScaleIn :: CreateAutoScalingGroup -> Lude.Maybe Lude.Bool) (\s a -> s {newInstancesProtectedFromScaleIn = a} :: CreateAutoScalingGroup)
{-# DEPRECATED casgNewInstancesProtectedFromScaleIn "Use generic-lens or generic-optics with 'newInstancesProtectedFromScaleIn' instead." #-}

-- | A comma-separated list of subnet IDs for a virtual private cloud (VPC) where instances in the Auto Scaling group can be created. If you specify @VPCZoneIdentifier@ with @AvailabilityZones@ , the subnets that you specify for this parameter must reside in those Availability Zones.
--
-- Conditional: If your account supports EC2-Classic and VPC, this parameter is required to launch instances into a VPC.
--
-- /Note:/ Consider using 'vpcZoneIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgVPCZoneIdentifier :: Lens.Lens' CreateAutoScalingGroup (Lude.Maybe Lude.Text)
casgVPCZoneIdentifier = Lens.lens (vpcZoneIdentifier :: CreateAutoScalingGroup -> Lude.Maybe Lude.Text) (\s a -> s {vpcZoneIdentifier = a} :: CreateAutoScalingGroup)
{-# DEPRECATED casgVPCZoneIdentifier "Use generic-lens or generic-optics with 'vpcZoneIdentifier' instead." #-}

-- | The Amazon Resource Names (ARN) of the target groups to associate with the Auto Scaling group. Instances are registered as targets in a target group, and traffic is routed to the target group. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-load-balancer.html Elastic Load Balancing and Amazon EC2 Auto Scaling> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'targetGroupARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgTargetGroupARNs :: Lens.Lens' CreateAutoScalingGroup (Lude.Maybe [Lude.Text])
casgTargetGroupARNs = Lens.lens (targetGroupARNs :: CreateAutoScalingGroup -> Lude.Maybe [Lude.Text]) (\s a -> s {targetGroupARNs = a} :: CreateAutoScalingGroup)
{-# DEPRECATED casgTargetGroupARNs "Use generic-lens or generic-optics with 'targetGroupARNs' instead." #-}

-- | The maximum amount of time, in seconds, that an instance can be in service. The default is null. If specified, the value must be either 0 or a number equal to or greater than 86,400 seconds (1 day). For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-max-instance-lifetime.html Replacing Auto Scaling instances based on maximum instance lifetime> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'maxInstanceLifetime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgMaxInstanceLifetime :: Lens.Lens' CreateAutoScalingGroup (Lude.Maybe Lude.Int)
casgMaxInstanceLifetime = Lens.lens (maxInstanceLifetime :: CreateAutoScalingGroup -> Lude.Maybe Lude.Int) (\s a -> s {maxInstanceLifetime = a} :: CreateAutoScalingGroup)
{-# DEPRECATED casgMaxInstanceLifetime "Use generic-lens or generic-optics with 'maxInstanceLifetime' instead." #-}

-- | The amount of time, in seconds, after a scaling activity completes before another scaling activity can start. The default value is @300@ . This setting applies when using simple scaling policies, but not when using other scaling policies or scheduled scaling. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/Cooldown.html Scaling cooldowns for Amazon EC2 Auto Scaling> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'defaultCooldown' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgDefaultCooldown :: Lens.Lens' CreateAutoScalingGroup (Lude.Maybe Lude.Int)
casgDefaultCooldown = Lens.lens (defaultCooldown :: CreateAutoScalingGroup -> Lude.Maybe Lude.Int) (\s a -> s {defaultCooldown = a} :: CreateAutoScalingGroup)
{-# DEPRECATED casgDefaultCooldown "Use generic-lens or generic-optics with 'defaultCooldown' instead." #-}

-- | The maximum size of the group.
--
-- /Note:/ Consider using 'maxSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgMaxSize :: Lens.Lens' CreateAutoScalingGroup Lude.Int
casgMaxSize = Lens.lens (maxSize :: CreateAutoScalingGroup -> Lude.Int) (\s a -> s {maxSize = a} :: CreateAutoScalingGroup)
{-# DEPRECATED casgMaxSize "Use generic-lens or generic-optics with 'maxSize' instead." #-}

-- | A list of Availability Zones where instances in the Auto Scaling group can be created. This parameter is optional if you specify one or more subnets for @VPCZoneIdentifier@ .
--
-- Conditional: If your account supports EC2-Classic and VPC, this parameter is required to launch instances into EC2-Classic.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgAvailabilityZones :: Lens.Lens' CreateAutoScalingGroup (Lude.Maybe (Lude.NonEmpty Lude.Text))
casgAvailabilityZones = Lens.lens (availabilityZones :: CreateAutoScalingGroup -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {availabilityZones = a} :: CreateAutoScalingGroup)
{-# DEPRECATED casgAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | The desired capacity is the initial capacity of the Auto Scaling group at the time of its creation and the capacity it attempts to maintain. It can scale beyond this capacity if you configure auto scaling. This number must be greater than or equal to the minimum size of the group and less than or equal to the maximum size of the group. If you do not specify a desired capacity, the default is the minimum size of the group.
--
-- /Note:/ Consider using 'desiredCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgDesiredCapacity :: Lens.Lens' CreateAutoScalingGroup (Lude.Maybe Lude.Int)
casgDesiredCapacity = Lens.lens (desiredCapacity :: CreateAutoScalingGroup -> Lude.Maybe Lude.Int) (\s a -> s {desiredCapacity = a} :: CreateAutoScalingGroup)
{-# DEPRECATED casgDesiredCapacity "Use generic-lens or generic-optics with 'desiredCapacity' instead." #-}

-- | An embedded object that specifies a mixed instances policy. The required parameters must be specified. If optional parameters are unspecified, their default values are used.
--
-- The policy includes parameters that not only define the distribution of On-Demand Instances and Spot Instances, the maximum price to pay for Spot Instances, and how the Auto Scaling group allocates instance types to fulfill On-Demand and Spot capacities, but also the parameters that specify the instance configuration information—the launch template and instance types. The policy can also include a weight for each instance type and different launch templates for individual instance types. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-purchase-options.html Auto Scaling groups with multiple instance types and purchase options> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'mixedInstancesPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgMixedInstancesPolicy :: Lens.Lens' CreateAutoScalingGroup (Lude.Maybe MixedInstancesPolicy)
casgMixedInstancesPolicy = Lens.lens (mixedInstancesPolicy :: CreateAutoScalingGroup -> Lude.Maybe MixedInstancesPolicy) (\s a -> s {mixedInstancesPolicy = a} :: CreateAutoScalingGroup)
{-# DEPRECATED casgMixedInstancesPolicy "Use generic-lens or generic-optics with 'mixedInstancesPolicy' instead." #-}

-- | The minimum size of the group.
--
-- /Note:/ Consider using 'minSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgMinSize :: Lens.Lens' CreateAutoScalingGroup Lude.Int
casgMinSize = Lens.lens (minSize :: CreateAutoScalingGroup -> Lude.Int) (\s a -> s {minSize = a} :: CreateAutoScalingGroup)
{-# DEPRECATED casgMinSize "Use generic-lens or generic-optics with 'minSize' instead." #-}

-- | The name of the Auto Scaling group. This name must be unique per Region per account.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgAutoScalingGroupName :: Lens.Lens' CreateAutoScalingGroup Lude.Text
casgAutoScalingGroupName = Lens.lens (autoScalingGroupName :: CreateAutoScalingGroup -> Lude.Text) (\s a -> s {autoScalingGroupName = a} :: CreateAutoScalingGroup)
{-# DEPRECATED casgAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

-- | The name of the launch configuration to use to launch instances.
--
-- Conditional: You must specify either a launch template (@LaunchTemplate@ or @MixedInstancesPolicy@ ) or a launch configuration (@LaunchConfigurationName@ or @InstanceId@ ).
--
-- /Note:/ Consider using 'launchConfigurationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgLaunchConfigurationName :: Lens.Lens' CreateAutoScalingGroup (Lude.Maybe Lude.Text)
casgLaunchConfigurationName = Lens.lens (launchConfigurationName :: CreateAutoScalingGroup -> Lude.Maybe Lude.Text) (\s a -> s {launchConfigurationName = a} :: CreateAutoScalingGroup)
{-# DEPRECATED casgLaunchConfigurationName "Use generic-lens or generic-optics with 'launchConfigurationName' instead." #-}

-- | One or more lifecycle hooks for the group, which specify actions to perform when Amazon EC2 Auto Scaling launches or terminates instances.
--
-- /Note:/ Consider using 'lifecycleHookSpecificationList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgLifecycleHookSpecificationList :: Lens.Lens' CreateAutoScalingGroup (Lude.Maybe [LifecycleHookSpecification])
casgLifecycleHookSpecificationList = Lens.lens (lifecycleHookSpecificationList :: CreateAutoScalingGroup -> Lude.Maybe [LifecycleHookSpecification]) (\s a -> s {lifecycleHookSpecificationList = a} :: CreateAutoScalingGroup)
{-# DEPRECATED casgLifecycleHookSpecificationList "Use generic-lens or generic-optics with 'lifecycleHookSpecificationList' instead." #-}

-- | The service to use for the health checks. The valid values are @EC2@ (default) and @ELB@ . If you configure an Auto Scaling group to use load balancer (ELB) health checks, it considers the instance unhealthy if it fails either the EC2 status checks or the load balancer health checks. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/healthcheck.html Health checks for Auto Scaling instances> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'healthCheckType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgHealthCheckType :: Lens.Lens' CreateAutoScalingGroup (Lude.Maybe Lude.Text)
casgHealthCheckType = Lens.lens (healthCheckType :: CreateAutoScalingGroup -> Lude.Maybe Lude.Text) (\s a -> s {healthCheckType = a} :: CreateAutoScalingGroup)
{-# DEPRECATED casgHealthCheckType "Use generic-lens or generic-optics with 'healthCheckType' instead." #-}

-- | Parameters used to specify the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ec2-launchtemplate.html launch template> and version to use to launch instances.
--
-- Conditional: You must specify either a launch template (@LaunchTemplate@ or @MixedInstancesPolicy@ ) or a launch configuration (@LaunchConfigurationName@ or @InstanceId@ ).
--
-- /Note:/ Consider using 'launchTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgLaunchTemplate :: Lens.Lens' CreateAutoScalingGroup (Lude.Maybe LaunchTemplateSpecification)
casgLaunchTemplate = Lens.lens (launchTemplate :: CreateAutoScalingGroup -> Lude.Maybe LaunchTemplateSpecification) (\s a -> s {launchTemplate = a} :: CreateAutoScalingGroup)
{-# DEPRECATED casgLaunchTemplate "Use generic-lens or generic-optics with 'launchTemplate' instead." #-}

-- | Indicates whether Capacity Rebalancing is enabled. Otherwise, Capacity Rebalancing is disabled. When you turn on Capacity Rebalancing, Amazon EC2 Auto Scaling attempts to launch a Spot Instance whenever Amazon EC2 notifies that a Spot Instance is at an elevated risk of interruption. After launching a new instance, it then terminates an old instance. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/capacity-rebalance.html Amazon EC2 Auto Scaling Capacity Rebalancing> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'capacityRebalance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgCapacityRebalance :: Lens.Lens' CreateAutoScalingGroup (Lude.Maybe Lude.Bool)
casgCapacityRebalance = Lens.lens (capacityRebalance :: CreateAutoScalingGroup -> Lude.Maybe Lude.Bool) (\s a -> s {capacityRebalance = a} :: CreateAutoScalingGroup)
{-# DEPRECATED casgCapacityRebalance "Use generic-lens or generic-optics with 'capacityRebalance' instead." #-}

-- | The name of an existing placement group into which to launch your instances, if any. A placement group is a logical grouping of instances within a single Availability Zone. You cannot specify multiple Availability Zones and a placement group. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement Groups> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- /Note:/ Consider using 'placementGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgPlacementGroup :: Lens.Lens' CreateAutoScalingGroup (Lude.Maybe Lude.Text)
casgPlacementGroup = Lens.lens (placementGroup :: CreateAutoScalingGroup -> Lude.Maybe Lude.Text) (\s a -> s {placementGroup = a} :: CreateAutoScalingGroup)
{-# DEPRECATED casgPlacementGroup "Use generic-lens or generic-optics with 'placementGroup' instead." #-}

-- | A list of Classic Load Balancers associated with this Auto Scaling group. For Application Load Balancers, Network Load Balancers, and Gateway Load Balancers, specify the @TargetGroupARNs@ property instead.
--
-- /Note:/ Consider using 'loadBalancerNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgLoadBalancerNames :: Lens.Lens' CreateAutoScalingGroup (Lude.Maybe [Lude.Text])
casgLoadBalancerNames = Lens.lens (loadBalancerNames :: CreateAutoScalingGroup -> Lude.Maybe [Lude.Text]) (\s a -> s {loadBalancerNames = a} :: CreateAutoScalingGroup)
{-# DEPRECATED casgLoadBalancerNames "Use generic-lens or generic-optics with 'loadBalancerNames' instead." #-}

-- | One or more tags. You can tag your Auto Scaling group and propagate the tags to the Amazon EC2 instances it launches. Tags are not propagated to Amazon EBS volumes. To add tags to Amazon EBS volumes, specify the tags in a launch template but use caution. If the launch template specifies an instance tag with a key that is also specified for the Auto Scaling group, Amazon EC2 Auto Scaling overrides the value of that instance tag with the value specified by the Auto Scaling group. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-tagging.html Tagging Auto Scaling groups and instances> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgTags :: Lens.Lens' CreateAutoScalingGroup (Lude.Maybe [Tag])
casgTags = Lens.lens (tags :: CreateAutoScalingGroup -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateAutoScalingGroup)
{-# DEPRECATED casgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateAutoScalingGroup where
  type Rs CreateAutoScalingGroup = CreateAutoScalingGroupResponse
  request = Req.postQuery autoScalingService
  response = Res.receiveNull CreateAutoScalingGroupResponse'

instance Lude.ToHeaders CreateAutoScalingGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateAutoScalingGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateAutoScalingGroup where
  toQuery CreateAutoScalingGroup' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateAutoScalingGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2011-01-01" :: Lude.ByteString),
        "InstanceId" Lude.=: instanceId,
        "TerminationPolicies"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "member" Lude.<$> terminationPolicies),
        "HealthCheckGracePeriod" Lude.=: healthCheckGracePeriod,
        "ServiceLinkedRoleARN" Lude.=: serviceLinkedRoleARN,
        "NewInstancesProtectedFromScaleIn"
          Lude.=: newInstancesProtectedFromScaleIn,
        "VPCZoneIdentifier" Lude.=: vpcZoneIdentifier,
        "TargetGroupARNs"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> targetGroupARNs),
        "MaxInstanceLifetime" Lude.=: maxInstanceLifetime,
        "DefaultCooldown" Lude.=: defaultCooldown,
        "MaxSize" Lude.=: maxSize,
        "AvailabilityZones"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "member" Lude.<$> availabilityZones),
        "DesiredCapacity" Lude.=: desiredCapacity,
        "MixedInstancesPolicy" Lude.=: mixedInstancesPolicy,
        "MinSize" Lude.=: minSize,
        "AutoScalingGroupName" Lude.=: autoScalingGroupName,
        "LaunchConfigurationName" Lude.=: launchConfigurationName,
        "LifecycleHookSpecificationList"
          Lude.=: Lude.toQuery
            ( Lude.toQueryList "member"
                Lude.<$> lifecycleHookSpecificationList
            ),
        "HealthCheckType" Lude.=: healthCheckType,
        "LaunchTemplate" Lude.=: launchTemplate,
        "CapacityRebalance" Lude.=: capacityRebalance,
        "PlacementGroup" Lude.=: placementGroup,
        "LoadBalancerNames"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "member" Lude.<$> loadBalancerNames),
        "Tags"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> tags)
      ]

-- | /See:/ 'mkCreateAutoScalingGroupResponse' smart constructor.
data CreateAutoScalingGroupResponse = CreateAutoScalingGroupResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateAutoScalingGroupResponse' with the minimum fields required to make a request.
mkCreateAutoScalingGroupResponse ::
  CreateAutoScalingGroupResponse
mkCreateAutoScalingGroupResponse = CreateAutoScalingGroupResponse'
