{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateAutoScalingGroup (..)
    , mkCreateAutoScalingGroup
    -- ** Request lenses
    , casgAutoScalingGroupName
    , casgMinSize
    , casgMaxSize
    , casgAvailabilityZones
    , casgCapacityRebalance
    , casgDefaultCooldown
    , casgDesiredCapacity
    , casgHealthCheckGracePeriod
    , casgHealthCheckType
    , casgInstanceId
    , casgLaunchConfigurationName
    , casgLaunchTemplate
    , casgLifecycleHookSpecificationList
    , casgLoadBalancerNames
    , casgMaxInstanceLifetime
    , casgMixedInstancesPolicy
    , casgNewInstancesProtectedFromScaleIn
    , casgPlacementGroup
    , casgServiceLinkedRoleARN
    , casgTags
    , casgTargetGroupARNs
    , casgTerminationPolicies
    , casgVPCZoneIdentifier

    -- * Destructuring the response
    , CreateAutoScalingGroupResponse (..)
    , mkCreateAutoScalingGroupResponse
    ) where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateAutoScalingGroup' smart constructor.
data CreateAutoScalingGroup = CreateAutoScalingGroup'
  { autoScalingGroupName :: Types.XmlStringMaxLen255
    -- ^ The name of the Auto Scaling group. This name must be unique per Region per account.
  , minSize :: Core.Int
    -- ^ The minimum size of the group.
  , maxSize :: Core.Int
    -- ^ The maximum size of the group.
  , availabilityZones :: Core.Maybe (Core.NonEmpty Types.XmlStringMaxLen255)
    -- ^ A list of Availability Zones where instances in the Auto Scaling group can be created. This parameter is optional if you specify one or more subnets for @VPCZoneIdentifier@ .
--
-- Conditional: If your account supports EC2-Classic and VPC, this parameter is required to launch instances into EC2-Classic.
  , capacityRebalance :: Core.Maybe Core.Bool
    -- ^ Indicates whether Capacity Rebalancing is enabled. Otherwise, Capacity Rebalancing is disabled. When you turn on Capacity Rebalancing, Amazon EC2 Auto Scaling attempts to launch a Spot Instance whenever Amazon EC2 notifies that a Spot Instance is at an elevated risk of interruption. After launching a new instance, it then terminates an old instance. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/capacity-rebalance.html Amazon EC2 Auto Scaling Capacity Rebalancing> in the /Amazon EC2 Auto Scaling User Guide/ .
  , defaultCooldown :: Core.Maybe Core.Int
    -- ^ The amount of time, in seconds, after a scaling activity completes before another scaling activity can start. The default value is @300@ . This setting applies when using simple scaling policies, but not when using other scaling policies or scheduled scaling. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/Cooldown.html Scaling cooldowns for Amazon EC2 Auto Scaling> in the /Amazon EC2 Auto Scaling User Guide/ .
  , desiredCapacity :: Core.Maybe Core.Int
    -- ^ The desired capacity is the initial capacity of the Auto Scaling group at the time of its creation and the capacity it attempts to maintain. It can scale beyond this capacity if you configure auto scaling. This number must be greater than or equal to the minimum size of the group and less than or equal to the maximum size of the group. If you do not specify a desired capacity, the default is the minimum size of the group.
  , healthCheckGracePeriod :: Core.Maybe Core.Int
    -- ^ The amount of time, in seconds, that Amazon EC2 Auto Scaling waits before checking the health status of an EC2 instance that has come into service. During this time, any health check failures for the instance are ignored. The default value is @0@ . For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/healthcheck.html#health-check-grace-period Health check grace period> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- Conditional: Required if you are adding an @ELB@ health check.
  , healthCheckType :: Core.Maybe Types.HealthCheckType
    -- ^ The service to use for the health checks. The valid values are @EC2@ (default) and @ELB@ . If you configure an Auto Scaling group to use load balancer (ELB) health checks, it considers the instance unhealthy if it fails either the EC2 status checks or the load balancer health checks. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/healthcheck.html Health checks for Auto Scaling instances> in the /Amazon EC2 Auto Scaling User Guide/ .
  , instanceId :: Core.Maybe Types.InstanceId
    -- ^ The ID of the instance used to base the launch configuration on. If specified, Amazon EC2 Auto Scaling uses the configuration values from the specified instance to create a new launch configuration. To get the instance ID, use the Amazon EC2 <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeInstances.html DescribeInstances> API operation. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-asg-from-instance.html Creating an Auto Scaling group using an EC2 instance> in the /Amazon EC2 Auto Scaling User Guide/ .
  , launchConfigurationName :: Core.Maybe Types.ResourceName
    -- ^ The name of the launch configuration to use to launch instances. 
--
-- Conditional: You must specify either a launch template (@LaunchTemplate@ or @MixedInstancesPolicy@ ) or a launch configuration (@LaunchConfigurationName@ or @InstanceId@ ).
  , launchTemplate :: Core.Maybe Types.LaunchTemplateSpecification
    -- ^ Parameters used to specify the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ec2-launchtemplate.html launch template> and version to use to launch instances. 
--
-- Conditional: You must specify either a launch template (@LaunchTemplate@ or @MixedInstancesPolicy@ ) or a launch configuration (@LaunchConfigurationName@ or @InstanceId@ ).
  , lifecycleHookSpecificationList :: Core.Maybe [Types.LifecycleHookSpecification]
    -- ^ One or more lifecycle hooks for the group, which specify actions to perform when Amazon EC2 Auto Scaling launches or terminates instances.
  , loadBalancerNames :: Core.Maybe [Types.XmlStringMaxLen255]
    -- ^ A list of Classic Load Balancers associated with this Auto Scaling group. For Application Load Balancers, Network Load Balancers, and Gateway Load Balancers, specify the @TargetGroupARNs@ property instead.
  , maxInstanceLifetime :: Core.Maybe Core.Int
    -- ^ The maximum amount of time, in seconds, that an instance can be in service. The default is null. If specified, the value must be either 0 or a number equal to or greater than 86,400 seconds (1 day). For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-max-instance-lifetime.html Replacing Auto Scaling instances based on maximum instance lifetime> in the /Amazon EC2 Auto Scaling User Guide/ .
  , mixedInstancesPolicy :: Core.Maybe Types.MixedInstancesPolicy
    -- ^ An embedded object that specifies a mixed instances policy. The required parameters must be specified. If optional parameters are unspecified, their default values are used.
--
-- The policy includes parameters that not only define the distribution of On-Demand Instances and Spot Instances, the maximum price to pay for Spot Instances, and how the Auto Scaling group allocates instance types to fulfill On-Demand and Spot capacities, but also the parameters that specify the instance configuration information—the launch template and instance types. The policy can also include a weight for each instance type and different launch templates for individual instance types. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-purchase-options.html Auto Scaling groups with multiple instance types and purchase options> in the /Amazon EC2 Auto Scaling User Guide/ .
  , newInstancesProtectedFromScaleIn :: Core.Maybe Core.Bool
    -- ^ Indicates whether newly launched instances are protected from termination by Amazon EC2 Auto Scaling when scaling in. For more information about preventing instances from terminating on scale in, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html#instance-protection Instance scale-in protection> in the /Amazon EC2 Auto Scaling User Guide/ .
  , placementGroup :: Core.Maybe Types.XmlStringMaxLen255
    -- ^ The name of an existing placement group into which to launch your instances, if any. A placement group is a logical grouping of instances within a single Availability Zone. You cannot specify multiple Availability Zones and a placement group. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement Groups> in the /Amazon EC2 User Guide for Linux Instances/ .
  , serviceLinkedRoleARN :: Core.Maybe Types.ResourceName
    -- ^ The Amazon Resource Name (ARN) of the service-linked role that the Auto Scaling group uses to call other AWS services on your behalf. By default, Amazon EC2 Auto Scaling uses a service-linked role named AWSServiceRoleForAutoScaling, which it creates if it does not exist. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-service-linked-role.html Service-linked roles> in the /Amazon EC2 Auto Scaling User Guide/ .
  , tags :: Core.Maybe [Types.Tag]
    -- ^ One or more tags. You can tag your Auto Scaling group and propagate the tags to the Amazon EC2 instances it launches. Tags are not propagated to Amazon EBS volumes. To add tags to Amazon EBS volumes, specify the tags in a launch template but use caution. If the launch template specifies an instance tag with a key that is also specified for the Auto Scaling group, Amazon EC2 Auto Scaling overrides the value of that instance tag with the value specified by the Auto Scaling group. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-tagging.html Tagging Auto Scaling groups and instances> in the /Amazon EC2 Auto Scaling User Guide/ .
  , targetGroupARNs :: Core.Maybe [Types.XmlStringMaxLen511]
    -- ^ The Amazon Resource Names (ARN) of the target groups to associate with the Auto Scaling group. Instances are registered as targets in a target group, and traffic is routed to the target group. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-load-balancer.html Elastic Load Balancing and Amazon EC2 Auto Scaling> in the /Amazon EC2 Auto Scaling User Guide/ .
  , terminationPolicies :: Core.Maybe [Types.XmlStringMaxLen1600]
    -- ^ A policy or a list of policies that are used to select the instance to terminate. These policies are executed in the order that you list them. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html Controlling which Auto Scaling instances terminate during scale in> in the /Amazon EC2 Auto Scaling User Guide/ .
  , vPCZoneIdentifier :: Core.Maybe Types.XmlStringMaxLen2047
    -- ^ A comma-separated list of subnet IDs for a virtual private cloud (VPC) where instances in the Auto Scaling group can be created. If you specify @VPCZoneIdentifier@ with @AvailabilityZones@ , the subnets that you specify for this parameter must reside in those Availability Zones.
--
-- Conditional: If your account supports EC2-Classic and VPC, this parameter is required to launch instances into a VPC.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateAutoScalingGroup' value with any optional fields omitted.
mkCreateAutoScalingGroup
    :: Types.XmlStringMaxLen255 -- ^ 'autoScalingGroupName'
    -> Core.Int -- ^ 'minSize'
    -> Core.Int -- ^ 'maxSize'
    -> CreateAutoScalingGroup
mkCreateAutoScalingGroup autoScalingGroupName minSize maxSize
  = CreateAutoScalingGroup'{autoScalingGroupName, minSize, maxSize,
                            availabilityZones = Core.Nothing, capacityRebalance = Core.Nothing,
                            defaultCooldown = Core.Nothing, desiredCapacity = Core.Nothing,
                            healthCheckGracePeriod = Core.Nothing,
                            healthCheckType = Core.Nothing, instanceId = Core.Nothing,
                            launchConfigurationName = Core.Nothing,
                            launchTemplate = Core.Nothing,
                            lifecycleHookSpecificationList = Core.Nothing,
                            loadBalancerNames = Core.Nothing,
                            maxInstanceLifetime = Core.Nothing,
                            mixedInstancesPolicy = Core.Nothing,
                            newInstancesProtectedFromScaleIn = Core.Nothing,
                            placementGroup = Core.Nothing, serviceLinkedRoleARN = Core.Nothing,
                            tags = Core.Nothing, targetGroupARNs = Core.Nothing,
                            terminationPolicies = Core.Nothing,
                            vPCZoneIdentifier = Core.Nothing}

-- | The name of the Auto Scaling group. This name must be unique per Region per account.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgAutoScalingGroupName :: Lens.Lens' CreateAutoScalingGroup Types.XmlStringMaxLen255
casgAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# INLINEABLE casgAutoScalingGroupName #-}
{-# DEPRECATED autoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead"  #-}

-- | The minimum size of the group.
--
-- /Note:/ Consider using 'minSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgMinSize :: Lens.Lens' CreateAutoScalingGroup Core.Int
casgMinSize = Lens.field @"minSize"
{-# INLINEABLE casgMinSize #-}
{-# DEPRECATED minSize "Use generic-lens or generic-optics with 'minSize' instead"  #-}

-- | The maximum size of the group.
--
-- /Note:/ Consider using 'maxSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgMaxSize :: Lens.Lens' CreateAutoScalingGroup Core.Int
casgMaxSize = Lens.field @"maxSize"
{-# INLINEABLE casgMaxSize #-}
{-# DEPRECATED maxSize "Use generic-lens or generic-optics with 'maxSize' instead"  #-}

-- | A list of Availability Zones where instances in the Auto Scaling group can be created. This parameter is optional if you specify one or more subnets for @VPCZoneIdentifier@ .
--
-- Conditional: If your account supports EC2-Classic and VPC, this parameter is required to launch instances into EC2-Classic.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgAvailabilityZones :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe (Core.NonEmpty Types.XmlStringMaxLen255))
casgAvailabilityZones = Lens.field @"availabilityZones"
{-# INLINEABLE casgAvailabilityZones #-}
{-# DEPRECATED availabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead"  #-}

-- | Indicates whether Capacity Rebalancing is enabled. Otherwise, Capacity Rebalancing is disabled. When you turn on Capacity Rebalancing, Amazon EC2 Auto Scaling attempts to launch a Spot Instance whenever Amazon EC2 notifies that a Spot Instance is at an elevated risk of interruption. After launching a new instance, it then terminates an old instance. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/capacity-rebalance.html Amazon EC2 Auto Scaling Capacity Rebalancing> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'capacityRebalance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgCapacityRebalance :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe Core.Bool)
casgCapacityRebalance = Lens.field @"capacityRebalance"
{-# INLINEABLE casgCapacityRebalance #-}
{-# DEPRECATED capacityRebalance "Use generic-lens or generic-optics with 'capacityRebalance' instead"  #-}

-- | The amount of time, in seconds, after a scaling activity completes before another scaling activity can start. The default value is @300@ . This setting applies when using simple scaling policies, but not when using other scaling policies or scheduled scaling. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/Cooldown.html Scaling cooldowns for Amazon EC2 Auto Scaling> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'defaultCooldown' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgDefaultCooldown :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe Core.Int)
casgDefaultCooldown = Lens.field @"defaultCooldown"
{-# INLINEABLE casgDefaultCooldown #-}
{-# DEPRECATED defaultCooldown "Use generic-lens or generic-optics with 'defaultCooldown' instead"  #-}

-- | The desired capacity is the initial capacity of the Auto Scaling group at the time of its creation and the capacity it attempts to maintain. It can scale beyond this capacity if you configure auto scaling. This number must be greater than or equal to the minimum size of the group and less than or equal to the maximum size of the group. If you do not specify a desired capacity, the default is the minimum size of the group.
--
-- /Note:/ Consider using 'desiredCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgDesiredCapacity :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe Core.Int)
casgDesiredCapacity = Lens.field @"desiredCapacity"
{-# INLINEABLE casgDesiredCapacity #-}
{-# DEPRECATED desiredCapacity "Use generic-lens or generic-optics with 'desiredCapacity' instead"  #-}

-- | The amount of time, in seconds, that Amazon EC2 Auto Scaling waits before checking the health status of an EC2 instance that has come into service. During this time, any health check failures for the instance are ignored. The default value is @0@ . For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/healthcheck.html#health-check-grace-period Health check grace period> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- Conditional: Required if you are adding an @ELB@ health check.
--
-- /Note:/ Consider using 'healthCheckGracePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgHealthCheckGracePeriod :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe Core.Int)
casgHealthCheckGracePeriod = Lens.field @"healthCheckGracePeriod"
{-# INLINEABLE casgHealthCheckGracePeriod #-}
{-# DEPRECATED healthCheckGracePeriod "Use generic-lens or generic-optics with 'healthCheckGracePeriod' instead"  #-}

-- | The service to use for the health checks. The valid values are @EC2@ (default) and @ELB@ . If you configure an Auto Scaling group to use load balancer (ELB) health checks, it considers the instance unhealthy if it fails either the EC2 status checks or the load balancer health checks. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/healthcheck.html Health checks for Auto Scaling instances> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'healthCheckType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgHealthCheckType :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe Types.HealthCheckType)
casgHealthCheckType = Lens.field @"healthCheckType"
{-# INLINEABLE casgHealthCheckType #-}
{-# DEPRECATED healthCheckType "Use generic-lens or generic-optics with 'healthCheckType' instead"  #-}

-- | The ID of the instance used to base the launch configuration on. If specified, Amazon EC2 Auto Scaling uses the configuration values from the specified instance to create a new launch configuration. To get the instance ID, use the Amazon EC2 <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeInstances.html DescribeInstances> API operation. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-asg-from-instance.html Creating an Auto Scaling group using an EC2 instance> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgInstanceId :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe Types.InstanceId)
casgInstanceId = Lens.field @"instanceId"
{-# INLINEABLE casgInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The name of the launch configuration to use to launch instances. 
--
-- Conditional: You must specify either a launch template (@LaunchTemplate@ or @MixedInstancesPolicy@ ) or a launch configuration (@LaunchConfigurationName@ or @InstanceId@ ).
--
-- /Note:/ Consider using 'launchConfigurationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgLaunchConfigurationName :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe Types.ResourceName)
casgLaunchConfigurationName = Lens.field @"launchConfigurationName"
{-# INLINEABLE casgLaunchConfigurationName #-}
{-# DEPRECATED launchConfigurationName "Use generic-lens or generic-optics with 'launchConfigurationName' instead"  #-}

-- | Parameters used to specify the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-ec2-launchtemplate.html launch template> and version to use to launch instances. 
--
-- Conditional: You must specify either a launch template (@LaunchTemplate@ or @MixedInstancesPolicy@ ) or a launch configuration (@LaunchConfigurationName@ or @InstanceId@ ).
--
-- /Note:/ Consider using 'launchTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgLaunchTemplate :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe Types.LaunchTemplateSpecification)
casgLaunchTemplate = Lens.field @"launchTemplate"
{-# INLINEABLE casgLaunchTemplate #-}
{-# DEPRECATED launchTemplate "Use generic-lens or generic-optics with 'launchTemplate' instead"  #-}

-- | One or more lifecycle hooks for the group, which specify actions to perform when Amazon EC2 Auto Scaling launches or terminates instances.
--
-- /Note:/ Consider using 'lifecycleHookSpecificationList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgLifecycleHookSpecificationList :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe [Types.LifecycleHookSpecification])
casgLifecycleHookSpecificationList = Lens.field @"lifecycleHookSpecificationList"
{-# INLINEABLE casgLifecycleHookSpecificationList #-}
{-# DEPRECATED lifecycleHookSpecificationList "Use generic-lens or generic-optics with 'lifecycleHookSpecificationList' instead"  #-}

-- | A list of Classic Load Balancers associated with this Auto Scaling group. For Application Load Balancers, Network Load Balancers, and Gateway Load Balancers, specify the @TargetGroupARNs@ property instead.
--
-- /Note:/ Consider using 'loadBalancerNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgLoadBalancerNames :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe [Types.XmlStringMaxLen255])
casgLoadBalancerNames = Lens.field @"loadBalancerNames"
{-# INLINEABLE casgLoadBalancerNames #-}
{-# DEPRECATED loadBalancerNames "Use generic-lens or generic-optics with 'loadBalancerNames' instead"  #-}

-- | The maximum amount of time, in seconds, that an instance can be in service. The default is null. If specified, the value must be either 0 or a number equal to or greater than 86,400 seconds (1 day). For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-max-instance-lifetime.html Replacing Auto Scaling instances based on maximum instance lifetime> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'maxInstanceLifetime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgMaxInstanceLifetime :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe Core.Int)
casgMaxInstanceLifetime = Lens.field @"maxInstanceLifetime"
{-# INLINEABLE casgMaxInstanceLifetime #-}
{-# DEPRECATED maxInstanceLifetime "Use generic-lens or generic-optics with 'maxInstanceLifetime' instead"  #-}

-- | An embedded object that specifies a mixed instances policy. The required parameters must be specified. If optional parameters are unspecified, their default values are used.
--
-- The policy includes parameters that not only define the distribution of On-Demand Instances and Spot Instances, the maximum price to pay for Spot Instances, and how the Auto Scaling group allocates instance types to fulfill On-Demand and Spot capacities, but also the parameters that specify the instance configuration information—the launch template and instance types. The policy can also include a weight for each instance type and different launch templates for individual instance types. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-purchase-options.html Auto Scaling groups with multiple instance types and purchase options> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'mixedInstancesPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgMixedInstancesPolicy :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe Types.MixedInstancesPolicy)
casgMixedInstancesPolicy = Lens.field @"mixedInstancesPolicy"
{-# INLINEABLE casgMixedInstancesPolicy #-}
{-# DEPRECATED mixedInstancesPolicy "Use generic-lens or generic-optics with 'mixedInstancesPolicy' instead"  #-}

-- | Indicates whether newly launched instances are protected from termination by Amazon EC2 Auto Scaling when scaling in. For more information about preventing instances from terminating on scale in, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html#instance-protection Instance scale-in protection> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'newInstancesProtectedFromScaleIn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgNewInstancesProtectedFromScaleIn :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe Core.Bool)
casgNewInstancesProtectedFromScaleIn = Lens.field @"newInstancesProtectedFromScaleIn"
{-# INLINEABLE casgNewInstancesProtectedFromScaleIn #-}
{-# DEPRECATED newInstancesProtectedFromScaleIn "Use generic-lens or generic-optics with 'newInstancesProtectedFromScaleIn' instead"  #-}

-- | The name of an existing placement group into which to launch your instances, if any. A placement group is a logical grouping of instances within a single Availability Zone. You cannot specify multiple Availability Zones and a placement group. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement Groups> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- /Note:/ Consider using 'placementGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgPlacementGroup :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe Types.XmlStringMaxLen255)
casgPlacementGroup = Lens.field @"placementGroup"
{-# INLINEABLE casgPlacementGroup #-}
{-# DEPRECATED placementGroup "Use generic-lens or generic-optics with 'placementGroup' instead"  #-}

-- | The Amazon Resource Name (ARN) of the service-linked role that the Auto Scaling group uses to call other AWS services on your behalf. By default, Amazon EC2 Auto Scaling uses a service-linked role named AWSServiceRoleForAutoScaling, which it creates if it does not exist. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-service-linked-role.html Service-linked roles> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'serviceLinkedRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgServiceLinkedRoleARN :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe Types.ResourceName)
casgServiceLinkedRoleARN = Lens.field @"serviceLinkedRoleARN"
{-# INLINEABLE casgServiceLinkedRoleARN #-}
{-# DEPRECATED serviceLinkedRoleARN "Use generic-lens or generic-optics with 'serviceLinkedRoleARN' instead"  #-}

-- | One or more tags. You can tag your Auto Scaling group and propagate the tags to the Amazon EC2 instances it launches. Tags are not propagated to Amazon EBS volumes. To add tags to Amazon EBS volumes, specify the tags in a launch template but use caution. If the launch template specifies an instance tag with a key that is also specified for the Auto Scaling group, Amazon EC2 Auto Scaling overrides the value of that instance tag with the value specified by the Auto Scaling group. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-tagging.html Tagging Auto Scaling groups and instances> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgTags :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe [Types.Tag])
casgTags = Lens.field @"tags"
{-# INLINEABLE casgTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The Amazon Resource Names (ARN) of the target groups to associate with the Auto Scaling group. Instances are registered as targets in a target group, and traffic is routed to the target group. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-load-balancer.html Elastic Load Balancing and Amazon EC2 Auto Scaling> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'targetGroupARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgTargetGroupARNs :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe [Types.XmlStringMaxLen511])
casgTargetGroupARNs = Lens.field @"targetGroupARNs"
{-# INLINEABLE casgTargetGroupARNs #-}
{-# DEPRECATED targetGroupARNs "Use generic-lens or generic-optics with 'targetGroupARNs' instead"  #-}

-- | A policy or a list of policies that are used to select the instance to terminate. These policies are executed in the order that you list them. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html Controlling which Auto Scaling instances terminate during scale in> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'terminationPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgTerminationPolicies :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe [Types.XmlStringMaxLen1600])
casgTerminationPolicies = Lens.field @"terminationPolicies"
{-# INLINEABLE casgTerminationPolicies #-}
{-# DEPRECATED terminationPolicies "Use generic-lens or generic-optics with 'terminationPolicies' instead"  #-}

-- | A comma-separated list of subnet IDs for a virtual private cloud (VPC) where instances in the Auto Scaling group can be created. If you specify @VPCZoneIdentifier@ with @AvailabilityZones@ , the subnets that you specify for this parameter must reside in those Availability Zones.
--
-- Conditional: If your account supports EC2-Classic and VPC, this parameter is required to launch instances into a VPC.
--
-- /Note:/ Consider using 'vPCZoneIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casgVPCZoneIdentifier :: Lens.Lens' CreateAutoScalingGroup (Core.Maybe Types.XmlStringMaxLen2047)
casgVPCZoneIdentifier = Lens.field @"vPCZoneIdentifier"
{-# INLINEABLE casgVPCZoneIdentifier #-}
{-# DEPRECATED vPCZoneIdentifier "Use generic-lens or generic-optics with 'vPCZoneIdentifier' instead"  #-}

instance Core.ToQuery CreateAutoScalingGroup where
        toQuery CreateAutoScalingGroup{..}
          = Core.toQueryPair "Action" ("CreateAutoScalingGroup" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2011-01-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "AutoScalingGroupName" autoScalingGroupName
              Core.<> Core.toQueryPair "MinSize" minSize
              Core.<> Core.toQueryPair "MaxSize" maxSize
              Core.<>
              Core.toQueryPair "AvailabilityZones"
                (Core.maybe Core.mempty (Core.toQueryList "member")
                   availabilityZones)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "CapacityRebalance")
                capacityRebalance
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DefaultCooldown")
                defaultCooldown
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DesiredCapacity")
                desiredCapacity
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "HealthCheckGracePeriod")
                healthCheckGracePeriod
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "HealthCheckType")
                healthCheckType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "InstanceId") instanceId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "LaunchConfigurationName")
                launchConfigurationName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "LaunchTemplate")
                launchTemplate
              Core.<>
              Core.toQueryPair "LifecycleHookSpecificationList"
                (Core.maybe Core.mempty (Core.toQueryList "member")
                   lifecycleHookSpecificationList)
              Core.<>
              Core.toQueryPair "LoadBalancerNames"
                (Core.maybe Core.mempty (Core.toQueryList "member")
                   loadBalancerNames)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxInstanceLifetime")
                maxInstanceLifetime
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MixedInstancesPolicy")
                mixedInstancesPolicy
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "NewInstancesProtectedFromScaleIn")
                newInstancesProtectedFromScaleIn
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PlacementGroup")
                placementGroup
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ServiceLinkedRoleARN")
                serviceLinkedRoleARN
              Core.<>
              Core.toQueryPair "Tags"
                (Core.maybe Core.mempty (Core.toQueryList "member") tags)
              Core.<>
              Core.toQueryPair "TargetGroupARNs"
                (Core.maybe Core.mempty (Core.toQueryList "member")
                   targetGroupARNs)
              Core.<>
              Core.toQueryPair "TerminationPolicies"
                (Core.maybe Core.mempty (Core.toQueryList "member")
                   terminationPolicies)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "VPCZoneIdentifier")
                vPCZoneIdentifier

instance Core.ToHeaders CreateAutoScalingGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateAutoScalingGroup where
        type Rs CreateAutoScalingGroup = CreateAutoScalingGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull CreateAutoScalingGroupResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateAutoScalingGroupResponse' smart constructor.
data CreateAutoScalingGroupResponse = CreateAutoScalingGroupResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateAutoScalingGroupResponse' value with any optional fields omitted.
mkCreateAutoScalingGroupResponse
    :: CreateAutoScalingGroupResponse
mkCreateAutoScalingGroupResponse = CreateAutoScalingGroupResponse'
