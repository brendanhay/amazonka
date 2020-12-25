{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    UpdateAutoScalingGroup (..),
    mkUpdateAutoScalingGroup,

    -- ** Request lenses
    uasgAutoScalingGroupName,
    uasgAvailabilityZones,
    uasgCapacityRebalance,
    uasgDefaultCooldown,
    uasgDesiredCapacity,
    uasgHealthCheckGracePeriod,
    uasgHealthCheckType,
    uasgLaunchConfigurationName,
    uasgLaunchTemplate,
    uasgMaxInstanceLifetime,
    uasgMaxSize,
    uasgMinSize,
    uasgMixedInstancesPolicy,
    uasgNewInstancesProtectedFromScaleIn,
    uasgPlacementGroup,
    uasgServiceLinkedRoleARN,
    uasgTerminationPolicies,
    uasgVPCZoneIdentifier,

    -- * Destructuring the response
    UpdateAutoScalingGroupResponse (..),
    mkUpdateAutoScalingGroupResponse,
  )
where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateAutoScalingGroup' smart constructor.
data UpdateAutoScalingGroup = UpdateAutoScalingGroup'
  { -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Types.AutoScalingGroupName,
    -- | One or more Availability Zones for the group.
    availabilityZones :: Core.Maybe (Core.NonEmpty Types.XmlStringMaxLen255),
    -- | Enables or disables Capacity Rebalancing. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/capacity-rebalance.html Amazon EC2 Auto Scaling Capacity Rebalancing> in the /Amazon EC2 Auto Scaling User Guide/ .
    capacityRebalance :: Core.Maybe Core.Bool,
    -- | The amount of time, in seconds, after a scaling activity completes before another scaling activity can start. The default value is @300@ . This setting applies when using simple scaling policies, but not when using other scaling policies or scheduled scaling. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/Cooldown.html Scaling cooldowns for Amazon EC2 Auto Scaling> in the /Amazon EC2 Auto Scaling User Guide/ .
    defaultCooldown :: Core.Maybe Core.Int,
    -- | The desired capacity is the initial capacity of the Auto Scaling group after this operation completes and the capacity it attempts to maintain. This number must be greater than or equal to the minimum size of the group and less than or equal to the maximum size of the group.
    desiredCapacity :: Core.Maybe Core.Int,
    -- | The amount of time, in seconds, that Amazon EC2 Auto Scaling waits before checking the health status of an EC2 instance that has come into service. The default value is @0@ . For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/healthcheck.html#health-check-grace-period Health check grace period> in the /Amazon EC2 Auto Scaling User Guide/ .
    --
    -- Conditional: Required if you are adding an @ELB@ health check.
    healthCheckGracePeriod :: Core.Maybe Core.Int,
    -- | The service to use for the health checks. The valid values are @EC2@ and @ELB@ . If you configure an Auto Scaling group to use ELB health checks, it considers the instance unhealthy if it fails either the EC2 status checks or the load balancer health checks.
    healthCheckType :: Core.Maybe Types.HealthCheckType,
    -- | The name of the launch configuration. If you specify @LaunchConfigurationName@ in your update request, you can't specify @LaunchTemplate@ or @MixedInstancesPolicy@ .
    launchConfigurationName :: Core.Maybe Types.LaunchConfigurationName,
    -- | The launch template and version to use to specify the updates. If you specify @LaunchTemplate@ in your update request, you can't specify @LaunchConfigurationName@ or @MixedInstancesPolicy@ .
    launchTemplate :: Core.Maybe Types.LaunchTemplateSpecification,
    -- | The maximum amount of time, in seconds, that an instance can be in service. The default is null. If specified, the value must be either 0 or a number equal to or greater than 86,400 seconds (1 day). To clear a previously set value, specify a new value of 0. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-max-instance-lifetime.html Replacing Auto Scaling instances based on maximum instance lifetime> in the /Amazon EC2 Auto Scaling User Guide/ .
    maxInstanceLifetime :: Core.Maybe Core.Int,
    -- | The maximum size of the Auto Scaling group.
    maxSize :: Core.Maybe Core.Int,
    -- | The minimum size of the Auto Scaling group.
    minSize :: Core.Maybe Core.Int,
    -- | An embedded object that specifies a mixed instances policy. When you make changes to an existing policy, all optional parameters are left unchanged if not specified. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-purchase-options.html Auto Scaling groups with multiple instance types and purchase options> in the /Amazon EC2 Auto Scaling User Guide/ .
    mixedInstancesPolicy :: Core.Maybe Types.MixedInstancesPolicy,
    -- | Indicates whether newly launched instances are protected from termination by Amazon EC2 Auto Scaling when scaling in. For more information about preventing instances from terminating on scale in, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html#instance-protection Instance scale-in protection> in the /Amazon EC2 Auto Scaling User Guide/ .
    newInstancesProtectedFromScaleIn :: Core.Maybe Core.Bool,
    -- | The name of an existing placement group into which to launch your instances, if any. A placement group is a logical grouping of instances within a single Availability Zone. You cannot specify multiple Availability Zones and a placement group. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement Groups> in the /Amazon EC2 User Guide for Linux Instances/ .
    placementGroup :: Core.Maybe Types.PlacementGroup,
    -- | The Amazon Resource Name (ARN) of the service-linked role that the Auto Scaling group uses to call other AWS services on your behalf. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-service-linked-role.html Service-linked roles> in the /Amazon EC2 Auto Scaling User Guide/ .
    serviceLinkedRoleARN :: Core.Maybe Types.ServiceLinkedRoleARN,
    -- | A policy or a list of policies that are used to select the instances to terminate. The policies are executed in the order that you list them. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html Controlling which Auto Scaling instances terminate during scale in> in the /Amazon EC2 Auto Scaling User Guide/ .
    terminationPolicies :: Core.Maybe [Types.XmlStringMaxLen1600],
    -- | A comma-separated list of subnet IDs for a virtual private cloud (VPC). If you specify @VPCZoneIdentifier@ with @AvailabilityZones@ , the subnets that you specify for this parameter must reside in those Availability Zones.
    vPCZoneIdentifier :: Core.Maybe Types.VPCZoneIdentifier
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateAutoScalingGroup' value with any optional fields omitted.
mkUpdateAutoScalingGroup ::
  -- | 'autoScalingGroupName'
  Types.AutoScalingGroupName ->
  UpdateAutoScalingGroup
mkUpdateAutoScalingGroup autoScalingGroupName =
  UpdateAutoScalingGroup'
    { autoScalingGroupName,
      availabilityZones = Core.Nothing,
      capacityRebalance = Core.Nothing,
      defaultCooldown = Core.Nothing,
      desiredCapacity = Core.Nothing,
      healthCheckGracePeriod = Core.Nothing,
      healthCheckType = Core.Nothing,
      launchConfigurationName = Core.Nothing,
      launchTemplate = Core.Nothing,
      maxInstanceLifetime = Core.Nothing,
      maxSize = Core.Nothing,
      minSize = Core.Nothing,
      mixedInstancesPolicy = Core.Nothing,
      newInstancesProtectedFromScaleIn = Core.Nothing,
      placementGroup = Core.Nothing,
      serviceLinkedRoleARN = Core.Nothing,
      terminationPolicies = Core.Nothing,
      vPCZoneIdentifier = Core.Nothing
    }

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasgAutoScalingGroupName :: Lens.Lens' UpdateAutoScalingGroup Types.AutoScalingGroupName
uasgAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# DEPRECATED uasgAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

-- | One or more Availability Zones for the group.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasgAvailabilityZones :: Lens.Lens' UpdateAutoScalingGroup (Core.Maybe (Core.NonEmpty Types.XmlStringMaxLen255))
uasgAvailabilityZones = Lens.field @"availabilityZones"
{-# DEPRECATED uasgAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | Enables or disables Capacity Rebalancing. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/capacity-rebalance.html Amazon EC2 Auto Scaling Capacity Rebalancing> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'capacityRebalance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasgCapacityRebalance :: Lens.Lens' UpdateAutoScalingGroup (Core.Maybe Core.Bool)
uasgCapacityRebalance = Lens.field @"capacityRebalance"
{-# DEPRECATED uasgCapacityRebalance "Use generic-lens or generic-optics with 'capacityRebalance' instead." #-}

-- | The amount of time, in seconds, after a scaling activity completes before another scaling activity can start. The default value is @300@ . This setting applies when using simple scaling policies, but not when using other scaling policies or scheduled scaling. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/Cooldown.html Scaling cooldowns for Amazon EC2 Auto Scaling> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'defaultCooldown' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasgDefaultCooldown :: Lens.Lens' UpdateAutoScalingGroup (Core.Maybe Core.Int)
uasgDefaultCooldown = Lens.field @"defaultCooldown"
{-# DEPRECATED uasgDefaultCooldown "Use generic-lens or generic-optics with 'defaultCooldown' instead." #-}

-- | The desired capacity is the initial capacity of the Auto Scaling group after this operation completes and the capacity it attempts to maintain. This number must be greater than or equal to the minimum size of the group and less than or equal to the maximum size of the group.
--
-- /Note:/ Consider using 'desiredCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasgDesiredCapacity :: Lens.Lens' UpdateAutoScalingGroup (Core.Maybe Core.Int)
uasgDesiredCapacity = Lens.field @"desiredCapacity"
{-# DEPRECATED uasgDesiredCapacity "Use generic-lens or generic-optics with 'desiredCapacity' instead." #-}

-- | The amount of time, in seconds, that Amazon EC2 Auto Scaling waits before checking the health status of an EC2 instance that has come into service. The default value is @0@ . For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/healthcheck.html#health-check-grace-period Health check grace period> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- Conditional: Required if you are adding an @ELB@ health check.
--
-- /Note:/ Consider using 'healthCheckGracePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasgHealthCheckGracePeriod :: Lens.Lens' UpdateAutoScalingGroup (Core.Maybe Core.Int)
uasgHealthCheckGracePeriod = Lens.field @"healthCheckGracePeriod"
{-# DEPRECATED uasgHealthCheckGracePeriod "Use generic-lens or generic-optics with 'healthCheckGracePeriod' instead." #-}

-- | The service to use for the health checks. The valid values are @EC2@ and @ELB@ . If you configure an Auto Scaling group to use ELB health checks, it considers the instance unhealthy if it fails either the EC2 status checks or the load balancer health checks.
--
-- /Note:/ Consider using 'healthCheckType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasgHealthCheckType :: Lens.Lens' UpdateAutoScalingGroup (Core.Maybe Types.HealthCheckType)
uasgHealthCheckType = Lens.field @"healthCheckType"
{-# DEPRECATED uasgHealthCheckType "Use generic-lens or generic-optics with 'healthCheckType' instead." #-}

-- | The name of the launch configuration. If you specify @LaunchConfigurationName@ in your update request, you can't specify @LaunchTemplate@ or @MixedInstancesPolicy@ .
--
-- /Note:/ Consider using 'launchConfigurationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasgLaunchConfigurationName :: Lens.Lens' UpdateAutoScalingGroup (Core.Maybe Types.LaunchConfigurationName)
uasgLaunchConfigurationName = Lens.field @"launchConfigurationName"
{-# DEPRECATED uasgLaunchConfigurationName "Use generic-lens or generic-optics with 'launchConfigurationName' instead." #-}

-- | The launch template and version to use to specify the updates. If you specify @LaunchTemplate@ in your update request, you can't specify @LaunchConfigurationName@ or @MixedInstancesPolicy@ .
--
-- /Note:/ Consider using 'launchTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasgLaunchTemplate :: Lens.Lens' UpdateAutoScalingGroup (Core.Maybe Types.LaunchTemplateSpecification)
uasgLaunchTemplate = Lens.field @"launchTemplate"
{-# DEPRECATED uasgLaunchTemplate "Use generic-lens or generic-optics with 'launchTemplate' instead." #-}

-- | The maximum amount of time, in seconds, that an instance can be in service. The default is null. If specified, the value must be either 0 or a number equal to or greater than 86,400 seconds (1 day). To clear a previously set value, specify a new value of 0. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-max-instance-lifetime.html Replacing Auto Scaling instances based on maximum instance lifetime> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'maxInstanceLifetime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasgMaxInstanceLifetime :: Lens.Lens' UpdateAutoScalingGroup (Core.Maybe Core.Int)
uasgMaxInstanceLifetime = Lens.field @"maxInstanceLifetime"
{-# DEPRECATED uasgMaxInstanceLifetime "Use generic-lens or generic-optics with 'maxInstanceLifetime' instead." #-}

-- | The maximum size of the Auto Scaling group.
--
-- /Note:/ Consider using 'maxSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasgMaxSize :: Lens.Lens' UpdateAutoScalingGroup (Core.Maybe Core.Int)
uasgMaxSize = Lens.field @"maxSize"
{-# DEPRECATED uasgMaxSize "Use generic-lens or generic-optics with 'maxSize' instead." #-}

-- | The minimum size of the Auto Scaling group.
--
-- /Note:/ Consider using 'minSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasgMinSize :: Lens.Lens' UpdateAutoScalingGroup (Core.Maybe Core.Int)
uasgMinSize = Lens.field @"minSize"
{-# DEPRECATED uasgMinSize "Use generic-lens or generic-optics with 'minSize' instead." #-}

-- | An embedded object that specifies a mixed instances policy. When you make changes to an existing policy, all optional parameters are left unchanged if not specified. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-purchase-options.html Auto Scaling groups with multiple instance types and purchase options> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'mixedInstancesPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasgMixedInstancesPolicy :: Lens.Lens' UpdateAutoScalingGroup (Core.Maybe Types.MixedInstancesPolicy)
uasgMixedInstancesPolicy = Lens.field @"mixedInstancesPolicy"
{-# DEPRECATED uasgMixedInstancesPolicy "Use generic-lens or generic-optics with 'mixedInstancesPolicy' instead." #-}

-- | Indicates whether newly launched instances are protected from termination by Amazon EC2 Auto Scaling when scaling in. For more information about preventing instances from terminating on scale in, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html#instance-protection Instance scale-in protection> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'newInstancesProtectedFromScaleIn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasgNewInstancesProtectedFromScaleIn :: Lens.Lens' UpdateAutoScalingGroup (Core.Maybe Core.Bool)
uasgNewInstancesProtectedFromScaleIn = Lens.field @"newInstancesProtectedFromScaleIn"
{-# DEPRECATED uasgNewInstancesProtectedFromScaleIn "Use generic-lens or generic-optics with 'newInstancesProtectedFromScaleIn' instead." #-}

-- | The name of an existing placement group into which to launch your instances, if any. A placement group is a logical grouping of instances within a single Availability Zone. You cannot specify multiple Availability Zones and a placement group. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement Groups> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- /Note:/ Consider using 'placementGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasgPlacementGroup :: Lens.Lens' UpdateAutoScalingGroup (Core.Maybe Types.PlacementGroup)
uasgPlacementGroup = Lens.field @"placementGroup"
{-# DEPRECATED uasgPlacementGroup "Use generic-lens or generic-optics with 'placementGroup' instead." #-}

-- | The Amazon Resource Name (ARN) of the service-linked role that the Auto Scaling group uses to call other AWS services on your behalf. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-service-linked-role.html Service-linked roles> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'serviceLinkedRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasgServiceLinkedRoleARN :: Lens.Lens' UpdateAutoScalingGroup (Core.Maybe Types.ServiceLinkedRoleARN)
uasgServiceLinkedRoleARN = Lens.field @"serviceLinkedRoleARN"
{-# DEPRECATED uasgServiceLinkedRoleARN "Use generic-lens or generic-optics with 'serviceLinkedRoleARN' instead." #-}

-- | A policy or a list of policies that are used to select the instances to terminate. The policies are executed in the order that you list them. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html Controlling which Auto Scaling instances terminate during scale in> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'terminationPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasgTerminationPolicies :: Lens.Lens' UpdateAutoScalingGroup (Core.Maybe [Types.XmlStringMaxLen1600])
uasgTerminationPolicies = Lens.field @"terminationPolicies"
{-# DEPRECATED uasgTerminationPolicies "Use generic-lens or generic-optics with 'terminationPolicies' instead." #-}

-- | A comma-separated list of subnet IDs for a virtual private cloud (VPC). If you specify @VPCZoneIdentifier@ with @AvailabilityZones@ , the subnets that you specify for this parameter must reside in those Availability Zones.
--
-- /Note:/ Consider using 'vPCZoneIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasgVPCZoneIdentifier :: Lens.Lens' UpdateAutoScalingGroup (Core.Maybe Types.VPCZoneIdentifier)
uasgVPCZoneIdentifier = Lens.field @"vPCZoneIdentifier"
{-# DEPRECATED uasgVPCZoneIdentifier "Use generic-lens or generic-optics with 'vPCZoneIdentifier' instead." #-}

instance Core.AWSRequest UpdateAutoScalingGroup where
  type Rs UpdateAutoScalingGroup = UpdateAutoScalingGroupResponse
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
            ( Core.pure ("Action", "UpdateAutoScalingGroup")
                Core.<> (Core.pure ("Version", "2011-01-01"))
                Core.<> (Core.toQueryValue "AutoScalingGroupName" autoScalingGroupName)
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
                Core.<> ( Core.toQueryValue "LaunchConfigurationName"
                            Core.<$> launchConfigurationName
                        )
                Core.<> (Core.toQueryValue "LaunchTemplate" Core.<$> launchTemplate)
                Core.<> ( Core.toQueryValue "MaxInstanceLifetime"
                            Core.<$> maxInstanceLifetime
                        )
                Core.<> (Core.toQueryValue "MaxSize" Core.<$> maxSize)
                Core.<> (Core.toQueryValue "MinSize" Core.<$> minSize)
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
                            "TerminationPolicies"
                            (Core.toQueryList "member" Core.<$> terminationPolicies)
                        )
                Core.<> ( Core.toQueryValue "VPCZoneIdentifier"
                            Core.<$> vPCZoneIdentifier
                        )
            )
      }
  response = Response.receiveNull UpdateAutoScalingGroupResponse'

-- | /See:/ 'mkUpdateAutoScalingGroupResponse' smart constructor.
data UpdateAutoScalingGroupResponse = UpdateAutoScalingGroupResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateAutoScalingGroupResponse' value with any optional fields omitted.
mkUpdateAutoScalingGroupResponse ::
  UpdateAutoScalingGroupResponse
mkUpdateAutoScalingGroupResponse = UpdateAutoScalingGroupResponse'
