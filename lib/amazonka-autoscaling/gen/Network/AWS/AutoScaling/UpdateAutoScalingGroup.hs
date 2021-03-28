{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.UpdateAutoScalingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the configuration for the specified Auto Scaling group.
--
-- To update an Auto Scaling group, specify the name of the group and the parameter that you want to change. Any parameters that you don't specify are not changed by this update request. The new settings take effect on any scaling activities after this call returns. 
-- If you associate a new launch configuration or template with an Auto Scaling group, all new instances will get the updated configuration. Existing instances continue to run with the configuration that they were originally launched with. When you update a group to specify a mixed instances policy instead of a launch configuration or template, existing instances may be replaced to match the new purchasing options that you specified in the policy. For example, if the group currently has 100% On-Demand capacity and the policy specifies 50% Spot capacity, this means that half of your instances will be gradually terminated and relaunched as Spot Instances. When replacing instances, Amazon EC2 Auto Scaling launches new instances before terminating the old ones, so that updating your group does not compromise the performance or availability of your application.
-- Note the following about changing @DesiredCapacity@ , @MaxSize@ , or @MinSize@ :
--
--     * If a scale-in activity occurs as a result of a new @DesiredCapacity@ value that is lower than the current size of the group, the Auto Scaling group uses its termination policy to determine which instances to terminate.
--
--
--     * If you specify a new value for @MinSize@ without specifying a value for @DesiredCapacity@ , and the new @MinSize@ is larger than the current size of the group, this sets the group's @DesiredCapacity@ to the new @MinSize@ value.
--
--
--     * If you specify a new value for @MaxSize@ without specifying a value for @DesiredCapacity@ , and the new @MaxSize@ is smaller than the current size of the group, this sets the group's @DesiredCapacity@ to the new @MaxSize@ value.
--
--
-- To see which parameters have been set, call the 'DescribeAutoScalingGroups' API. To view the scaling policies for an Auto Scaling group, call the 'DescribePolicies' API. If the group has scaling policies, you can update them by calling the 'PutScalingPolicy' API.
module Network.AWS.AutoScaling.UpdateAutoScalingGroup
    (
    -- * Creating a request
      UpdateAutoScalingGroup (..)
    , mkUpdateAutoScalingGroup
    -- ** Request lenses
    , uasgAutoScalingGroupName
    , uasgAvailabilityZones
    , uasgCapacityRebalance
    , uasgDefaultCooldown
    , uasgDesiredCapacity
    , uasgHealthCheckGracePeriod
    , uasgHealthCheckType
    , uasgLaunchConfigurationName
    , uasgLaunchTemplate
    , uasgMaxInstanceLifetime
    , uasgMaxSize
    , uasgMinSize
    , uasgMixedInstancesPolicy
    , uasgNewInstancesProtectedFromScaleIn
    , uasgPlacementGroup
    , uasgServiceLinkedRoleARN
    , uasgTerminationPolicies
    , uasgVPCZoneIdentifier

    -- * Destructuring the response
    , UpdateAutoScalingGroupResponse (..)
    , mkUpdateAutoScalingGroupResponse
    ) where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateAutoScalingGroup' smart constructor.
data UpdateAutoScalingGroup = UpdateAutoScalingGroup'
  { autoScalingGroupName :: Types.AutoScalingGroupName
    -- ^ The name of the Auto Scaling group.
  , availabilityZones :: Core.Maybe (Core.NonEmpty Types.XmlStringMaxLen255)
    -- ^ One or more Availability Zones for the group.
  , capacityRebalance :: Core.Maybe Core.Bool
    -- ^ Enables or disables Capacity Rebalancing. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/capacity-rebalance.html Amazon EC2 Auto Scaling Capacity Rebalancing> in the /Amazon EC2 Auto Scaling User Guide/ .
  , defaultCooldown :: Core.Maybe Core.Int
    -- ^ The amount of time, in seconds, after a scaling activity completes before another scaling activity can start. The default value is @300@ . This setting applies when using simple scaling policies, but not when using other scaling policies or scheduled scaling. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/Cooldown.html Scaling cooldowns for Amazon EC2 Auto Scaling> in the /Amazon EC2 Auto Scaling User Guide/ .
  , desiredCapacity :: Core.Maybe Core.Int
    -- ^ The desired capacity is the initial capacity of the Auto Scaling group after this operation completes and the capacity it attempts to maintain. This number must be greater than or equal to the minimum size of the group and less than or equal to the maximum size of the group.
  , healthCheckGracePeriod :: Core.Maybe Core.Int
    -- ^ The amount of time, in seconds, that Amazon EC2 Auto Scaling waits before checking the health status of an EC2 instance that has come into service. The default value is @0@ . For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/healthcheck.html#health-check-grace-period Health check grace period> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- Conditional: Required if you are adding an @ELB@ health check.
  , healthCheckType :: Core.Maybe Types.HealthCheckType
    -- ^ The service to use for the health checks. The valid values are @EC2@ and @ELB@ . If you configure an Auto Scaling group to use ELB health checks, it considers the instance unhealthy if it fails either the EC2 status checks or the load balancer health checks.
  , launchConfigurationName :: Core.Maybe Types.LaunchConfigurationName
    -- ^ The name of the launch configuration. If you specify @LaunchConfigurationName@ in your update request, you can't specify @LaunchTemplate@ or @MixedInstancesPolicy@ .
  , launchTemplate :: Core.Maybe Types.LaunchTemplateSpecification
    -- ^ The launch template and version to use to specify the updates. If you specify @LaunchTemplate@ in your update request, you can't specify @LaunchConfigurationName@ or @MixedInstancesPolicy@ .
  , maxInstanceLifetime :: Core.Maybe Core.Int
    -- ^ The maximum amount of time, in seconds, that an instance can be in service. The default is null. If specified, the value must be either 0 or a number equal to or greater than 86,400 seconds (1 day). To clear a previously set value, specify a new value of 0. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-max-instance-lifetime.html Replacing Auto Scaling instances based on maximum instance lifetime> in the /Amazon EC2 Auto Scaling User Guide/ .
  , maxSize :: Core.Maybe Core.Int
    -- ^ The maximum size of the Auto Scaling group.
  , minSize :: Core.Maybe Core.Int
    -- ^ The minimum size of the Auto Scaling group.
  , mixedInstancesPolicy :: Core.Maybe Types.MixedInstancesPolicy
    -- ^ An embedded object that specifies a mixed instances policy. When you make changes to an existing policy, all optional parameters are left unchanged if not specified. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-purchase-options.html Auto Scaling groups with multiple instance types and purchase options> in the /Amazon EC2 Auto Scaling User Guide/ .
  , newInstancesProtectedFromScaleIn :: Core.Maybe Core.Bool
    -- ^ Indicates whether newly launched instances are protected from termination by Amazon EC2 Auto Scaling when scaling in. For more information about preventing instances from terminating on scale in, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html#instance-protection Instance scale-in protection> in the /Amazon EC2 Auto Scaling User Guide/ .
  , placementGroup :: Core.Maybe Types.PlacementGroup
    -- ^ The name of an existing placement group into which to launch your instances, if any. A placement group is a logical grouping of instances within a single Availability Zone. You cannot specify multiple Availability Zones and a placement group. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement Groups> in the /Amazon EC2 User Guide for Linux Instances/ .
  , serviceLinkedRoleARN :: Core.Maybe Types.ServiceLinkedRoleARN
    -- ^ The Amazon Resource Name (ARN) of the service-linked role that the Auto Scaling group uses to call other AWS services on your behalf. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-service-linked-role.html Service-linked roles> in the /Amazon EC2 Auto Scaling User Guide/ .
  , terminationPolicies :: Core.Maybe [Types.XmlStringMaxLen1600]
    -- ^ A policy or a list of policies that are used to select the instances to terminate. The policies are executed in the order that you list them. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html Controlling which Auto Scaling instances terminate during scale in> in the /Amazon EC2 Auto Scaling User Guide/ .
  , vPCZoneIdentifier :: Core.Maybe Types.VPCZoneIdentifier
    -- ^ A comma-separated list of subnet IDs for a virtual private cloud (VPC). If you specify @VPCZoneIdentifier@ with @AvailabilityZones@ , the subnets that you specify for this parameter must reside in those Availability Zones.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateAutoScalingGroup' value with any optional fields omitted.
mkUpdateAutoScalingGroup
    :: Types.AutoScalingGroupName -- ^ 'autoScalingGroupName'
    -> UpdateAutoScalingGroup
mkUpdateAutoScalingGroup autoScalingGroupName
  = UpdateAutoScalingGroup'{autoScalingGroupName,
                            availabilityZones = Core.Nothing, capacityRebalance = Core.Nothing,
                            defaultCooldown = Core.Nothing, desiredCapacity = Core.Nothing,
                            healthCheckGracePeriod = Core.Nothing,
                            healthCheckType = Core.Nothing,
                            launchConfigurationName = Core.Nothing,
                            launchTemplate = Core.Nothing, maxInstanceLifetime = Core.Nothing,
                            maxSize = Core.Nothing, minSize = Core.Nothing,
                            mixedInstancesPolicy = Core.Nothing,
                            newInstancesProtectedFromScaleIn = Core.Nothing,
                            placementGroup = Core.Nothing, serviceLinkedRoleARN = Core.Nothing,
                            terminationPolicies = Core.Nothing,
                            vPCZoneIdentifier = Core.Nothing}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasgAutoScalingGroupName :: Lens.Lens' UpdateAutoScalingGroup Types.AutoScalingGroupName
uasgAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# INLINEABLE uasgAutoScalingGroupName #-}
{-# DEPRECATED autoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead"  #-}

-- | One or more Availability Zones for the group.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasgAvailabilityZones :: Lens.Lens' UpdateAutoScalingGroup (Core.Maybe (Core.NonEmpty Types.XmlStringMaxLen255))
uasgAvailabilityZones = Lens.field @"availabilityZones"
{-# INLINEABLE uasgAvailabilityZones #-}
{-# DEPRECATED availabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead"  #-}

-- | Enables or disables Capacity Rebalancing. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/capacity-rebalance.html Amazon EC2 Auto Scaling Capacity Rebalancing> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'capacityRebalance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasgCapacityRebalance :: Lens.Lens' UpdateAutoScalingGroup (Core.Maybe Core.Bool)
uasgCapacityRebalance = Lens.field @"capacityRebalance"
{-# INLINEABLE uasgCapacityRebalance #-}
{-# DEPRECATED capacityRebalance "Use generic-lens or generic-optics with 'capacityRebalance' instead"  #-}

-- | The amount of time, in seconds, after a scaling activity completes before another scaling activity can start. The default value is @300@ . This setting applies when using simple scaling policies, but not when using other scaling policies or scheduled scaling. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/Cooldown.html Scaling cooldowns for Amazon EC2 Auto Scaling> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'defaultCooldown' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasgDefaultCooldown :: Lens.Lens' UpdateAutoScalingGroup (Core.Maybe Core.Int)
uasgDefaultCooldown = Lens.field @"defaultCooldown"
{-# INLINEABLE uasgDefaultCooldown #-}
{-# DEPRECATED defaultCooldown "Use generic-lens or generic-optics with 'defaultCooldown' instead"  #-}

-- | The desired capacity is the initial capacity of the Auto Scaling group after this operation completes and the capacity it attempts to maintain. This number must be greater than or equal to the minimum size of the group and less than or equal to the maximum size of the group.
--
-- /Note:/ Consider using 'desiredCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasgDesiredCapacity :: Lens.Lens' UpdateAutoScalingGroup (Core.Maybe Core.Int)
uasgDesiredCapacity = Lens.field @"desiredCapacity"
{-# INLINEABLE uasgDesiredCapacity #-}
{-# DEPRECATED desiredCapacity "Use generic-lens or generic-optics with 'desiredCapacity' instead"  #-}

-- | The amount of time, in seconds, that Amazon EC2 Auto Scaling waits before checking the health status of an EC2 instance that has come into service. The default value is @0@ . For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/healthcheck.html#health-check-grace-period Health check grace period> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- Conditional: Required if you are adding an @ELB@ health check.
--
-- /Note:/ Consider using 'healthCheckGracePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasgHealthCheckGracePeriod :: Lens.Lens' UpdateAutoScalingGroup (Core.Maybe Core.Int)
uasgHealthCheckGracePeriod = Lens.field @"healthCheckGracePeriod"
{-# INLINEABLE uasgHealthCheckGracePeriod #-}
{-# DEPRECATED healthCheckGracePeriod "Use generic-lens or generic-optics with 'healthCheckGracePeriod' instead"  #-}

-- | The service to use for the health checks. The valid values are @EC2@ and @ELB@ . If you configure an Auto Scaling group to use ELB health checks, it considers the instance unhealthy if it fails either the EC2 status checks or the load balancer health checks.
--
-- /Note:/ Consider using 'healthCheckType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasgHealthCheckType :: Lens.Lens' UpdateAutoScalingGroup (Core.Maybe Types.HealthCheckType)
uasgHealthCheckType = Lens.field @"healthCheckType"
{-# INLINEABLE uasgHealthCheckType #-}
{-# DEPRECATED healthCheckType "Use generic-lens or generic-optics with 'healthCheckType' instead"  #-}

-- | The name of the launch configuration. If you specify @LaunchConfigurationName@ in your update request, you can't specify @LaunchTemplate@ or @MixedInstancesPolicy@ .
--
-- /Note:/ Consider using 'launchConfigurationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasgLaunchConfigurationName :: Lens.Lens' UpdateAutoScalingGroup (Core.Maybe Types.LaunchConfigurationName)
uasgLaunchConfigurationName = Lens.field @"launchConfigurationName"
{-# INLINEABLE uasgLaunchConfigurationName #-}
{-# DEPRECATED launchConfigurationName "Use generic-lens or generic-optics with 'launchConfigurationName' instead"  #-}

-- | The launch template and version to use to specify the updates. If you specify @LaunchTemplate@ in your update request, you can't specify @LaunchConfigurationName@ or @MixedInstancesPolicy@ .
--
-- /Note:/ Consider using 'launchTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasgLaunchTemplate :: Lens.Lens' UpdateAutoScalingGroup (Core.Maybe Types.LaunchTemplateSpecification)
uasgLaunchTemplate = Lens.field @"launchTemplate"
{-# INLINEABLE uasgLaunchTemplate #-}
{-# DEPRECATED launchTemplate "Use generic-lens or generic-optics with 'launchTemplate' instead"  #-}

-- | The maximum amount of time, in seconds, that an instance can be in service. The default is null. If specified, the value must be either 0 or a number equal to or greater than 86,400 seconds (1 day). To clear a previously set value, specify a new value of 0. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-max-instance-lifetime.html Replacing Auto Scaling instances based on maximum instance lifetime> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'maxInstanceLifetime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasgMaxInstanceLifetime :: Lens.Lens' UpdateAutoScalingGroup (Core.Maybe Core.Int)
uasgMaxInstanceLifetime = Lens.field @"maxInstanceLifetime"
{-# INLINEABLE uasgMaxInstanceLifetime #-}
{-# DEPRECATED maxInstanceLifetime "Use generic-lens or generic-optics with 'maxInstanceLifetime' instead"  #-}

-- | The maximum size of the Auto Scaling group.
--
-- /Note:/ Consider using 'maxSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasgMaxSize :: Lens.Lens' UpdateAutoScalingGroup (Core.Maybe Core.Int)
uasgMaxSize = Lens.field @"maxSize"
{-# INLINEABLE uasgMaxSize #-}
{-# DEPRECATED maxSize "Use generic-lens or generic-optics with 'maxSize' instead"  #-}

-- | The minimum size of the Auto Scaling group.
--
-- /Note:/ Consider using 'minSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasgMinSize :: Lens.Lens' UpdateAutoScalingGroup (Core.Maybe Core.Int)
uasgMinSize = Lens.field @"minSize"
{-# INLINEABLE uasgMinSize #-}
{-# DEPRECATED minSize "Use generic-lens or generic-optics with 'minSize' instead"  #-}

-- | An embedded object that specifies a mixed instances policy. When you make changes to an existing policy, all optional parameters are left unchanged if not specified. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-purchase-options.html Auto Scaling groups with multiple instance types and purchase options> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'mixedInstancesPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasgMixedInstancesPolicy :: Lens.Lens' UpdateAutoScalingGroup (Core.Maybe Types.MixedInstancesPolicy)
uasgMixedInstancesPolicy = Lens.field @"mixedInstancesPolicy"
{-# INLINEABLE uasgMixedInstancesPolicy #-}
{-# DEPRECATED mixedInstancesPolicy "Use generic-lens or generic-optics with 'mixedInstancesPolicy' instead"  #-}

-- | Indicates whether newly launched instances are protected from termination by Amazon EC2 Auto Scaling when scaling in. For more information about preventing instances from terminating on scale in, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html#instance-protection Instance scale-in protection> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'newInstancesProtectedFromScaleIn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasgNewInstancesProtectedFromScaleIn :: Lens.Lens' UpdateAutoScalingGroup (Core.Maybe Core.Bool)
uasgNewInstancesProtectedFromScaleIn = Lens.field @"newInstancesProtectedFromScaleIn"
{-# INLINEABLE uasgNewInstancesProtectedFromScaleIn #-}
{-# DEPRECATED newInstancesProtectedFromScaleIn "Use generic-lens or generic-optics with 'newInstancesProtectedFromScaleIn' instead"  #-}

-- | The name of an existing placement group into which to launch your instances, if any. A placement group is a logical grouping of instances within a single Availability Zone. You cannot specify multiple Availability Zones and a placement group. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement Groups> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- /Note:/ Consider using 'placementGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasgPlacementGroup :: Lens.Lens' UpdateAutoScalingGroup (Core.Maybe Types.PlacementGroup)
uasgPlacementGroup = Lens.field @"placementGroup"
{-# INLINEABLE uasgPlacementGroup #-}
{-# DEPRECATED placementGroup "Use generic-lens or generic-optics with 'placementGroup' instead"  #-}

-- | The Amazon Resource Name (ARN) of the service-linked role that the Auto Scaling group uses to call other AWS services on your behalf. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-service-linked-role.html Service-linked roles> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'serviceLinkedRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasgServiceLinkedRoleARN :: Lens.Lens' UpdateAutoScalingGroup (Core.Maybe Types.ServiceLinkedRoleARN)
uasgServiceLinkedRoleARN = Lens.field @"serviceLinkedRoleARN"
{-# INLINEABLE uasgServiceLinkedRoleARN #-}
{-# DEPRECATED serviceLinkedRoleARN "Use generic-lens or generic-optics with 'serviceLinkedRoleARN' instead"  #-}

-- | A policy or a list of policies that are used to select the instances to terminate. The policies are executed in the order that you list them. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html Controlling which Auto Scaling instances terminate during scale in> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'terminationPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasgTerminationPolicies :: Lens.Lens' UpdateAutoScalingGroup (Core.Maybe [Types.XmlStringMaxLen1600])
uasgTerminationPolicies = Lens.field @"terminationPolicies"
{-# INLINEABLE uasgTerminationPolicies #-}
{-# DEPRECATED terminationPolicies "Use generic-lens or generic-optics with 'terminationPolicies' instead"  #-}

-- | A comma-separated list of subnet IDs for a virtual private cloud (VPC). If you specify @VPCZoneIdentifier@ with @AvailabilityZones@ , the subnets that you specify for this parameter must reside in those Availability Zones.
--
-- /Note:/ Consider using 'vPCZoneIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasgVPCZoneIdentifier :: Lens.Lens' UpdateAutoScalingGroup (Core.Maybe Types.VPCZoneIdentifier)
uasgVPCZoneIdentifier = Lens.field @"vPCZoneIdentifier"
{-# INLINEABLE uasgVPCZoneIdentifier #-}
{-# DEPRECATED vPCZoneIdentifier "Use generic-lens or generic-optics with 'vPCZoneIdentifier' instead"  #-}

instance Core.ToQuery UpdateAutoScalingGroup where
        toQuery UpdateAutoScalingGroup{..}
          = Core.toQueryPair "Action" ("UpdateAutoScalingGroup" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2011-01-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "AutoScalingGroupName" autoScalingGroupName
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
              Core.maybe Core.mempty (Core.toQueryPair "LaunchConfigurationName")
                launchConfigurationName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "LaunchTemplate")
                launchTemplate
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxInstanceLifetime")
                maxInstanceLifetime
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "MaxSize") maxSize
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "MinSize") minSize
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
              Core.toQueryPair "TerminationPolicies"
                (Core.maybe Core.mempty (Core.toQueryList "member")
                   terminationPolicies)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "VPCZoneIdentifier")
                vPCZoneIdentifier

instance Core.ToHeaders UpdateAutoScalingGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest UpdateAutoScalingGroup where
        type Rs UpdateAutoScalingGroup = UpdateAutoScalingGroupResponse
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
          = Response.receiveNull UpdateAutoScalingGroupResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateAutoScalingGroupResponse' smart constructor.
data UpdateAutoScalingGroupResponse = UpdateAutoScalingGroupResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateAutoScalingGroupResponse' value with any optional fields omitted.
mkUpdateAutoScalingGroupResponse
    :: UpdateAutoScalingGroupResponse
mkUpdateAutoScalingGroupResponse = UpdateAutoScalingGroupResponse'
