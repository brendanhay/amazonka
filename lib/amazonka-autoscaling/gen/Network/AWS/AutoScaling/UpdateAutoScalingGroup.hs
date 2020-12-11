{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    uasgTerminationPolicies,
    uasgHealthCheckGracePeriod,
    uasgServiceLinkedRoleARN,
    uasgNewInstancesProtectedFromScaleIn,
    uasgVPCZoneIdentifier,
    uasgMaxInstanceLifetime,
    uasgDefaultCooldown,
    uasgMaxSize,
    uasgAvailabilityZones,
    uasgDesiredCapacity,
    uasgMixedInstancesPolicy,
    uasgMinSize,
    uasgLaunchConfigurationName,
    uasgHealthCheckType,
    uasgLaunchTemplate,
    uasgCapacityRebalance,
    uasgPlacementGroup,
    uasgAutoScalingGroupName,

    -- * Destructuring the response
    UpdateAutoScalingGroupResponse (..),
    mkUpdateAutoScalingGroupResponse,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateAutoScalingGroup' smart constructor.
data UpdateAutoScalingGroup = UpdateAutoScalingGroup'
  { terminationPolicies ::
      Lude.Maybe [Lude.Text],
    healthCheckGracePeriod :: Lude.Maybe Lude.Int,
    serviceLinkedRoleARN :: Lude.Maybe Lude.Text,
    newInstancesProtectedFromScaleIn ::
      Lude.Maybe Lude.Bool,
    vpcZoneIdentifier :: Lude.Maybe Lude.Text,
    maxInstanceLifetime :: Lude.Maybe Lude.Int,
    defaultCooldown :: Lude.Maybe Lude.Int,
    maxSize :: Lude.Maybe Lude.Int,
    availabilityZones ::
      Lude.Maybe (Lude.NonEmpty Lude.Text),
    desiredCapacity :: Lude.Maybe Lude.Int,
    mixedInstancesPolicy ::
      Lude.Maybe MixedInstancesPolicy,
    minSize :: Lude.Maybe Lude.Int,
    launchConfigurationName ::
      Lude.Maybe Lude.Text,
    healthCheckType :: Lude.Maybe Lude.Text,
    launchTemplate ::
      Lude.Maybe LaunchTemplateSpecification,
    capacityRebalance :: Lude.Maybe Lude.Bool,
    placementGroup :: Lude.Maybe Lude.Text,
    autoScalingGroupName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateAutoScalingGroup' with the minimum fields required to make a request.
--
-- * 'autoScalingGroupName' - The name of the Auto Scaling group.
-- * 'availabilityZones' - One or more Availability Zones for the group.
-- * 'capacityRebalance' - Enables or disables Capacity Rebalancing. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/capacity-rebalance.html Amazon EC2 Auto Scaling Capacity Rebalancing> in the /Amazon EC2 Auto Scaling User Guide/ .
-- * 'defaultCooldown' - The amount of time, in seconds, after a scaling activity completes before another scaling activity can start. The default value is @300@ . This setting applies when using simple scaling policies, but not when using other scaling policies or scheduled scaling. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/Cooldown.html Scaling cooldowns for Amazon EC2 Auto Scaling> in the /Amazon EC2 Auto Scaling User Guide/ .
-- * 'desiredCapacity' - The desired capacity is the initial capacity of the Auto Scaling group after this operation completes and the capacity it attempts to maintain. This number must be greater than or equal to the minimum size of the group and less than or equal to the maximum size of the group.
-- * 'healthCheckGracePeriod' - The amount of time, in seconds, that Amazon EC2 Auto Scaling waits before checking the health status of an EC2 instance that has come into service. The default value is @0@ . For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/healthcheck.html#health-check-grace-period Health check grace period> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- Conditional: Required if you are adding an @ELB@ health check.
-- * 'healthCheckType' - The service to use for the health checks. The valid values are @EC2@ and @ELB@ . If you configure an Auto Scaling group to use ELB health checks, it considers the instance unhealthy if it fails either the EC2 status checks or the load balancer health checks.
-- * 'launchConfigurationName' - The name of the launch configuration. If you specify @LaunchConfigurationName@ in your update request, you can't specify @LaunchTemplate@ or @MixedInstancesPolicy@ .
-- * 'launchTemplate' - The launch template and version to use to specify the updates. If you specify @LaunchTemplate@ in your update request, you can't specify @LaunchConfigurationName@ or @MixedInstancesPolicy@ .
-- * 'maxInstanceLifetime' - The maximum amount of time, in seconds, that an instance can be in service. The default is null. If specified, the value must be either 0 or a number equal to or greater than 86,400 seconds (1 day). To clear a previously set value, specify a new value of 0. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-max-instance-lifetime.html Replacing Auto Scaling instances based on maximum instance lifetime> in the /Amazon EC2 Auto Scaling User Guide/ .
-- * 'maxSize' - The maximum size of the Auto Scaling group.
-- * 'minSize' - The minimum size of the Auto Scaling group.
-- * 'mixedInstancesPolicy' - An embedded object that specifies a mixed instances policy. When you make changes to an existing policy, all optional parameters are left unchanged if not specified. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-purchase-options.html Auto Scaling groups with multiple instance types and purchase options> in the /Amazon EC2 Auto Scaling User Guide/ .
-- * 'newInstancesProtectedFromScaleIn' - Indicates whether newly launched instances are protected from termination by Amazon EC2 Auto Scaling when scaling in. For more information about preventing instances from terminating on scale in, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html#instance-protection Instance scale-in protection> in the /Amazon EC2 Auto Scaling User Guide/ .
-- * 'placementGroup' - The name of an existing placement group into which to launch your instances, if any. A placement group is a logical grouping of instances within a single Availability Zone. You cannot specify multiple Availability Zones and a placement group. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement Groups> in the /Amazon EC2 User Guide for Linux Instances/ .
-- * 'serviceLinkedRoleARN' - The Amazon Resource Name (ARN) of the service-linked role that the Auto Scaling group uses to call other AWS services on your behalf. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-service-linked-role.html Service-linked roles> in the /Amazon EC2 Auto Scaling User Guide/ .
-- * 'terminationPolicies' - A policy or a list of policies that are used to select the instances to terminate. The policies are executed in the order that you list them. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html Controlling which Auto Scaling instances terminate during scale in> in the /Amazon EC2 Auto Scaling User Guide/ .
-- * 'vpcZoneIdentifier' - A comma-separated list of subnet IDs for a virtual private cloud (VPC). If you specify @VPCZoneIdentifier@ with @AvailabilityZones@ , the subnets that you specify for this parameter must reside in those Availability Zones.
mkUpdateAutoScalingGroup ::
  -- | 'autoScalingGroupName'
  Lude.Text ->
  UpdateAutoScalingGroup
mkUpdateAutoScalingGroup pAutoScalingGroupName_ =
  UpdateAutoScalingGroup'
    { terminationPolicies = Lude.Nothing,
      healthCheckGracePeriod = Lude.Nothing,
      serviceLinkedRoleARN = Lude.Nothing,
      newInstancesProtectedFromScaleIn = Lude.Nothing,
      vpcZoneIdentifier = Lude.Nothing,
      maxInstanceLifetime = Lude.Nothing,
      defaultCooldown = Lude.Nothing,
      maxSize = Lude.Nothing,
      availabilityZones = Lude.Nothing,
      desiredCapacity = Lude.Nothing,
      mixedInstancesPolicy = Lude.Nothing,
      minSize = Lude.Nothing,
      launchConfigurationName = Lude.Nothing,
      healthCheckType = Lude.Nothing,
      launchTemplate = Lude.Nothing,
      capacityRebalance = Lude.Nothing,
      placementGroup = Lude.Nothing,
      autoScalingGroupName = pAutoScalingGroupName_
    }

-- | A policy or a list of policies that are used to select the instances to terminate. The policies are executed in the order that you list them. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html Controlling which Auto Scaling instances terminate during scale in> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'terminationPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasgTerminationPolicies :: Lens.Lens' UpdateAutoScalingGroup (Lude.Maybe [Lude.Text])
uasgTerminationPolicies = Lens.lens (terminationPolicies :: UpdateAutoScalingGroup -> Lude.Maybe [Lude.Text]) (\s a -> s {terminationPolicies = a} :: UpdateAutoScalingGroup)
{-# DEPRECATED uasgTerminationPolicies "Use generic-lens or generic-optics with 'terminationPolicies' instead." #-}

-- | The amount of time, in seconds, that Amazon EC2 Auto Scaling waits before checking the health status of an EC2 instance that has come into service. The default value is @0@ . For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/healthcheck.html#health-check-grace-period Health check grace period> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- Conditional: Required if you are adding an @ELB@ health check.
--
-- /Note:/ Consider using 'healthCheckGracePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasgHealthCheckGracePeriod :: Lens.Lens' UpdateAutoScalingGroup (Lude.Maybe Lude.Int)
uasgHealthCheckGracePeriod = Lens.lens (healthCheckGracePeriod :: UpdateAutoScalingGroup -> Lude.Maybe Lude.Int) (\s a -> s {healthCheckGracePeriod = a} :: UpdateAutoScalingGroup)
{-# DEPRECATED uasgHealthCheckGracePeriod "Use generic-lens or generic-optics with 'healthCheckGracePeriod' instead." #-}

-- | The Amazon Resource Name (ARN) of the service-linked role that the Auto Scaling group uses to call other AWS services on your behalf. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-service-linked-role.html Service-linked roles> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'serviceLinkedRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasgServiceLinkedRoleARN :: Lens.Lens' UpdateAutoScalingGroup (Lude.Maybe Lude.Text)
uasgServiceLinkedRoleARN = Lens.lens (serviceLinkedRoleARN :: UpdateAutoScalingGroup -> Lude.Maybe Lude.Text) (\s a -> s {serviceLinkedRoleARN = a} :: UpdateAutoScalingGroup)
{-# DEPRECATED uasgServiceLinkedRoleARN "Use generic-lens or generic-optics with 'serviceLinkedRoleARN' instead." #-}

-- | Indicates whether newly launched instances are protected from termination by Amazon EC2 Auto Scaling when scaling in. For more information about preventing instances from terminating on scale in, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html#instance-protection Instance scale-in protection> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'newInstancesProtectedFromScaleIn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasgNewInstancesProtectedFromScaleIn :: Lens.Lens' UpdateAutoScalingGroup (Lude.Maybe Lude.Bool)
uasgNewInstancesProtectedFromScaleIn = Lens.lens (newInstancesProtectedFromScaleIn :: UpdateAutoScalingGroup -> Lude.Maybe Lude.Bool) (\s a -> s {newInstancesProtectedFromScaleIn = a} :: UpdateAutoScalingGroup)
{-# DEPRECATED uasgNewInstancesProtectedFromScaleIn "Use generic-lens or generic-optics with 'newInstancesProtectedFromScaleIn' instead." #-}

-- | A comma-separated list of subnet IDs for a virtual private cloud (VPC). If you specify @VPCZoneIdentifier@ with @AvailabilityZones@ , the subnets that you specify for this parameter must reside in those Availability Zones.
--
-- /Note:/ Consider using 'vpcZoneIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasgVPCZoneIdentifier :: Lens.Lens' UpdateAutoScalingGroup (Lude.Maybe Lude.Text)
uasgVPCZoneIdentifier = Lens.lens (vpcZoneIdentifier :: UpdateAutoScalingGroup -> Lude.Maybe Lude.Text) (\s a -> s {vpcZoneIdentifier = a} :: UpdateAutoScalingGroup)
{-# DEPRECATED uasgVPCZoneIdentifier "Use generic-lens or generic-optics with 'vpcZoneIdentifier' instead." #-}

-- | The maximum amount of time, in seconds, that an instance can be in service. The default is null. If specified, the value must be either 0 or a number equal to or greater than 86,400 seconds (1 day). To clear a previously set value, specify a new value of 0. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-max-instance-lifetime.html Replacing Auto Scaling instances based on maximum instance lifetime> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'maxInstanceLifetime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasgMaxInstanceLifetime :: Lens.Lens' UpdateAutoScalingGroup (Lude.Maybe Lude.Int)
uasgMaxInstanceLifetime = Lens.lens (maxInstanceLifetime :: UpdateAutoScalingGroup -> Lude.Maybe Lude.Int) (\s a -> s {maxInstanceLifetime = a} :: UpdateAutoScalingGroup)
{-# DEPRECATED uasgMaxInstanceLifetime "Use generic-lens or generic-optics with 'maxInstanceLifetime' instead." #-}

-- | The amount of time, in seconds, after a scaling activity completes before another scaling activity can start. The default value is @300@ . This setting applies when using simple scaling policies, but not when using other scaling policies or scheduled scaling. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/Cooldown.html Scaling cooldowns for Amazon EC2 Auto Scaling> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'defaultCooldown' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasgDefaultCooldown :: Lens.Lens' UpdateAutoScalingGroup (Lude.Maybe Lude.Int)
uasgDefaultCooldown = Lens.lens (defaultCooldown :: UpdateAutoScalingGroup -> Lude.Maybe Lude.Int) (\s a -> s {defaultCooldown = a} :: UpdateAutoScalingGroup)
{-# DEPRECATED uasgDefaultCooldown "Use generic-lens or generic-optics with 'defaultCooldown' instead." #-}

-- | The maximum size of the Auto Scaling group.
--
-- /Note:/ Consider using 'maxSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasgMaxSize :: Lens.Lens' UpdateAutoScalingGroup (Lude.Maybe Lude.Int)
uasgMaxSize = Lens.lens (maxSize :: UpdateAutoScalingGroup -> Lude.Maybe Lude.Int) (\s a -> s {maxSize = a} :: UpdateAutoScalingGroup)
{-# DEPRECATED uasgMaxSize "Use generic-lens or generic-optics with 'maxSize' instead." #-}

-- | One or more Availability Zones for the group.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasgAvailabilityZones :: Lens.Lens' UpdateAutoScalingGroup (Lude.Maybe (Lude.NonEmpty Lude.Text))
uasgAvailabilityZones = Lens.lens (availabilityZones :: UpdateAutoScalingGroup -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {availabilityZones = a} :: UpdateAutoScalingGroup)
{-# DEPRECATED uasgAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | The desired capacity is the initial capacity of the Auto Scaling group after this operation completes and the capacity it attempts to maintain. This number must be greater than or equal to the minimum size of the group and less than or equal to the maximum size of the group.
--
-- /Note:/ Consider using 'desiredCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasgDesiredCapacity :: Lens.Lens' UpdateAutoScalingGroup (Lude.Maybe Lude.Int)
uasgDesiredCapacity = Lens.lens (desiredCapacity :: UpdateAutoScalingGroup -> Lude.Maybe Lude.Int) (\s a -> s {desiredCapacity = a} :: UpdateAutoScalingGroup)
{-# DEPRECATED uasgDesiredCapacity "Use generic-lens or generic-optics with 'desiredCapacity' instead." #-}

-- | An embedded object that specifies a mixed instances policy. When you make changes to an existing policy, all optional parameters are left unchanged if not specified. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-purchase-options.html Auto Scaling groups with multiple instance types and purchase options> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'mixedInstancesPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasgMixedInstancesPolicy :: Lens.Lens' UpdateAutoScalingGroup (Lude.Maybe MixedInstancesPolicy)
uasgMixedInstancesPolicy = Lens.lens (mixedInstancesPolicy :: UpdateAutoScalingGroup -> Lude.Maybe MixedInstancesPolicy) (\s a -> s {mixedInstancesPolicy = a} :: UpdateAutoScalingGroup)
{-# DEPRECATED uasgMixedInstancesPolicy "Use generic-lens or generic-optics with 'mixedInstancesPolicy' instead." #-}

-- | The minimum size of the Auto Scaling group.
--
-- /Note:/ Consider using 'minSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasgMinSize :: Lens.Lens' UpdateAutoScalingGroup (Lude.Maybe Lude.Int)
uasgMinSize = Lens.lens (minSize :: UpdateAutoScalingGroup -> Lude.Maybe Lude.Int) (\s a -> s {minSize = a} :: UpdateAutoScalingGroup)
{-# DEPRECATED uasgMinSize "Use generic-lens or generic-optics with 'minSize' instead." #-}

-- | The name of the launch configuration. If you specify @LaunchConfigurationName@ in your update request, you can't specify @LaunchTemplate@ or @MixedInstancesPolicy@ .
--
-- /Note:/ Consider using 'launchConfigurationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasgLaunchConfigurationName :: Lens.Lens' UpdateAutoScalingGroup (Lude.Maybe Lude.Text)
uasgLaunchConfigurationName = Lens.lens (launchConfigurationName :: UpdateAutoScalingGroup -> Lude.Maybe Lude.Text) (\s a -> s {launchConfigurationName = a} :: UpdateAutoScalingGroup)
{-# DEPRECATED uasgLaunchConfigurationName "Use generic-lens or generic-optics with 'launchConfigurationName' instead." #-}

-- | The service to use for the health checks. The valid values are @EC2@ and @ELB@ . If you configure an Auto Scaling group to use ELB health checks, it considers the instance unhealthy if it fails either the EC2 status checks or the load balancer health checks.
--
-- /Note:/ Consider using 'healthCheckType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasgHealthCheckType :: Lens.Lens' UpdateAutoScalingGroup (Lude.Maybe Lude.Text)
uasgHealthCheckType = Lens.lens (healthCheckType :: UpdateAutoScalingGroup -> Lude.Maybe Lude.Text) (\s a -> s {healthCheckType = a} :: UpdateAutoScalingGroup)
{-# DEPRECATED uasgHealthCheckType "Use generic-lens or generic-optics with 'healthCheckType' instead." #-}

-- | The launch template and version to use to specify the updates. If you specify @LaunchTemplate@ in your update request, you can't specify @LaunchConfigurationName@ or @MixedInstancesPolicy@ .
--
-- /Note:/ Consider using 'launchTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasgLaunchTemplate :: Lens.Lens' UpdateAutoScalingGroup (Lude.Maybe LaunchTemplateSpecification)
uasgLaunchTemplate = Lens.lens (launchTemplate :: UpdateAutoScalingGroup -> Lude.Maybe LaunchTemplateSpecification) (\s a -> s {launchTemplate = a} :: UpdateAutoScalingGroup)
{-# DEPRECATED uasgLaunchTemplate "Use generic-lens or generic-optics with 'launchTemplate' instead." #-}

-- | Enables or disables Capacity Rebalancing. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/capacity-rebalance.html Amazon EC2 Auto Scaling Capacity Rebalancing> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /Note:/ Consider using 'capacityRebalance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasgCapacityRebalance :: Lens.Lens' UpdateAutoScalingGroup (Lude.Maybe Lude.Bool)
uasgCapacityRebalance = Lens.lens (capacityRebalance :: UpdateAutoScalingGroup -> Lude.Maybe Lude.Bool) (\s a -> s {capacityRebalance = a} :: UpdateAutoScalingGroup)
{-# DEPRECATED uasgCapacityRebalance "Use generic-lens or generic-optics with 'capacityRebalance' instead." #-}

-- | The name of an existing placement group into which to launch your instances, if any. A placement group is a logical grouping of instances within a single Availability Zone. You cannot specify multiple Availability Zones and a placement group. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement Groups> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- /Note:/ Consider using 'placementGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasgPlacementGroup :: Lens.Lens' UpdateAutoScalingGroup (Lude.Maybe Lude.Text)
uasgPlacementGroup = Lens.lens (placementGroup :: UpdateAutoScalingGroup -> Lude.Maybe Lude.Text) (\s a -> s {placementGroup = a} :: UpdateAutoScalingGroup)
{-# DEPRECATED uasgPlacementGroup "Use generic-lens or generic-optics with 'placementGroup' instead." #-}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasgAutoScalingGroupName :: Lens.Lens' UpdateAutoScalingGroup Lude.Text
uasgAutoScalingGroupName = Lens.lens (autoScalingGroupName :: UpdateAutoScalingGroup -> Lude.Text) (\s a -> s {autoScalingGroupName = a} :: UpdateAutoScalingGroup)
{-# DEPRECATED uasgAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

instance Lude.AWSRequest UpdateAutoScalingGroup where
  type Rs UpdateAutoScalingGroup = UpdateAutoScalingGroupResponse
  request = Req.postQuery autoScalingService
  response = Res.receiveNull UpdateAutoScalingGroupResponse'

instance Lude.ToHeaders UpdateAutoScalingGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UpdateAutoScalingGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateAutoScalingGroup where
  toQuery UpdateAutoScalingGroup' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("UpdateAutoScalingGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2011-01-01" :: Lude.ByteString),
        "TerminationPolicies"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "member" Lude.<$> terminationPolicies),
        "HealthCheckGracePeriod" Lude.=: healthCheckGracePeriod,
        "ServiceLinkedRoleARN" Lude.=: serviceLinkedRoleARN,
        "NewInstancesProtectedFromScaleIn"
          Lude.=: newInstancesProtectedFromScaleIn,
        "VPCZoneIdentifier" Lude.=: vpcZoneIdentifier,
        "MaxInstanceLifetime" Lude.=: maxInstanceLifetime,
        "DefaultCooldown" Lude.=: defaultCooldown,
        "MaxSize" Lude.=: maxSize,
        "AvailabilityZones"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "member" Lude.<$> availabilityZones),
        "DesiredCapacity" Lude.=: desiredCapacity,
        "MixedInstancesPolicy" Lude.=: mixedInstancesPolicy,
        "MinSize" Lude.=: minSize,
        "LaunchConfigurationName" Lude.=: launchConfigurationName,
        "HealthCheckType" Lude.=: healthCheckType,
        "LaunchTemplate" Lude.=: launchTemplate,
        "CapacityRebalance" Lude.=: capacityRebalance,
        "PlacementGroup" Lude.=: placementGroup,
        "AutoScalingGroupName" Lude.=: autoScalingGroupName
      ]

-- | /See:/ 'mkUpdateAutoScalingGroupResponse' smart constructor.
data UpdateAutoScalingGroupResponse = UpdateAutoScalingGroupResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateAutoScalingGroupResponse' with the minimum fields required to make a request.
mkUpdateAutoScalingGroupResponse ::
  UpdateAutoScalingGroupResponse
mkUpdateAutoScalingGroupResponse = UpdateAutoScalingGroupResponse'
