{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.AutoScalingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScaling.Types.AutoScalingGroup
  ( AutoScalingGroup (..)
  -- * Smart constructor
  , mkAutoScalingGroup
  -- * Lenses
  , asgAutoScalingGroupName
  , asgMinSize
  , asgMaxSize
  , asgDesiredCapacity
  , asgDefaultCooldown
  , asgAvailabilityZones
  , asgHealthCheckType
  , asgCreatedTime
  , asgAutoScalingGroupARN
  , asgCapacityRebalance
  , asgEnabledMetrics
  , asgHealthCheckGracePeriod
  , asgInstances
  , asgLaunchConfigurationName
  , asgLaunchTemplate
  , asgLoadBalancerNames
  , asgMaxInstanceLifetime
  , asgMixedInstancesPolicy
  , asgNewInstancesProtectedFromScaleIn
  , asgPlacementGroup
  , asgServiceLinkedRoleARN
  , asgStatus
  , asgSuspendedProcesses
  , asgTags
  , asgTargetGroupARNs
  , asgTerminationPolicies
  , asgVPCZoneIdentifier
  ) where

import qualified Network.AWS.AutoScaling.Types.AutoScalingGroupARN as Types
import qualified Network.AWS.AutoScaling.Types.AutoScalingGroupName as Types
import qualified Network.AWS.AutoScaling.Types.EnabledMetric as Types
import qualified Network.AWS.AutoScaling.Types.HealthCheckType as Types
import qualified Network.AWS.AutoScaling.Types.Instance as Types
import qualified Network.AWS.AutoScaling.Types.LaunchConfigurationName as Types
import qualified Network.AWS.AutoScaling.Types.LaunchTemplateSpecification as Types
import qualified Network.AWS.AutoScaling.Types.MixedInstancesPolicy as Types
import qualified Network.AWS.AutoScaling.Types.PlacementGroup as Types
import qualified Network.AWS.AutoScaling.Types.ServiceLinkedRoleARN as Types
import qualified Network.AWS.AutoScaling.Types.Status as Types
import qualified Network.AWS.AutoScaling.Types.SuspendedProcess as Types
import qualified Network.AWS.AutoScaling.Types.TagDescription as Types
import qualified Network.AWS.AutoScaling.Types.VPCZoneIdentifier as Types
import qualified Network.AWS.AutoScaling.Types.XmlStringMaxLen1600 as Types
import qualified Network.AWS.AutoScaling.Types.XmlStringMaxLen255 as Types
import qualified Network.AWS.AutoScaling.Types.XmlStringMaxLen511 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an Auto Scaling group.
--
-- /See:/ 'mkAutoScalingGroup' smart constructor.
data AutoScalingGroup = AutoScalingGroup'
  { autoScalingGroupName :: Types.AutoScalingGroupName
    -- ^ The name of the Auto Scaling group.
  , minSize :: Core.Int
    -- ^ The minimum size of the group.
  , maxSize :: Core.Int
    -- ^ The maximum size of the group.
  , desiredCapacity :: Core.Int
    -- ^ The desired size of the group.
  , defaultCooldown :: Core.Int
    -- ^ The duration of the default cooldown period, in seconds.
  , availabilityZones :: Core.NonEmpty Types.XmlStringMaxLen255
    -- ^ One or more Availability Zones for the group.
  , healthCheckType :: Types.HealthCheckType
    -- ^ The service to use for the health checks. The valid values are @EC2@ and @ELB@ . If you configure an Auto Scaling group to use ELB health checks, it considers the instance unhealthy if it fails either the EC2 status checks or the load balancer health checks.
  , createdTime :: Core.UTCTime
    -- ^ The date and time the group was created.
  , autoScalingGroupARN :: Core.Maybe Types.AutoScalingGroupARN
    -- ^ The Amazon Resource Name (ARN) of the Auto Scaling group.
  , capacityRebalance :: Core.Maybe Core.Bool
    -- ^ Indicates whether Capacity Rebalancing is enabled.
  , enabledMetrics :: Core.Maybe [Types.EnabledMetric]
    -- ^ The metrics enabled for the group.
  , healthCheckGracePeriod :: Core.Maybe Core.Int
    -- ^ The amount of time, in seconds, that Amazon EC2 Auto Scaling waits before checking the health status of an EC2 instance that has come into service.
  , instances :: Core.Maybe [Types.Instance]
    -- ^ The EC2 instances associated with the group.
  , launchConfigurationName :: Core.Maybe Types.LaunchConfigurationName
    -- ^ The name of the associated launch configuration.
  , launchTemplate :: Core.Maybe Types.LaunchTemplateSpecification
    -- ^ The launch template for the group.
  , loadBalancerNames :: Core.Maybe [Types.XmlStringMaxLen255]
    -- ^ One or more load balancers associated with the group.
  , maxInstanceLifetime :: Core.Maybe Core.Int
    -- ^ The maximum amount of time, in seconds, that an instance can be in service.
--
-- Valid Range: Minimum value of 0.
  , mixedInstancesPolicy :: Core.Maybe Types.MixedInstancesPolicy
    -- ^ The mixed instances policy for the group.
  , newInstancesProtectedFromScaleIn :: Core.Maybe Core.Bool
    -- ^ Indicates whether newly launched instances are protected from termination by Amazon EC2 Auto Scaling when scaling in.
  , placementGroup :: Core.Maybe Types.PlacementGroup
    -- ^ The name of the placement group into which to launch your instances, if any.
  , serviceLinkedRoleARN :: Core.Maybe Types.ServiceLinkedRoleARN
    -- ^ The Amazon Resource Name (ARN) of the service-linked role that the Auto Scaling group uses to call other AWS services on your behalf.
  , status :: Core.Maybe Types.Status
    -- ^ The current state of the group when the 'DeleteAutoScalingGroup' operation is in progress.
  , suspendedProcesses :: Core.Maybe [Types.SuspendedProcess]
    -- ^ The suspended processes associated with the group.
  , tags :: Core.Maybe [Types.TagDescription]
    -- ^ The tags for the group.
  , targetGroupARNs :: Core.Maybe [Types.XmlStringMaxLen511]
    -- ^ The Amazon Resource Names (ARN) of the target groups for your load balancer.
  , terminationPolicies :: Core.Maybe [Types.XmlStringMaxLen1600]
    -- ^ The termination policies for the group.
  , vPCZoneIdentifier :: Core.Maybe Types.VPCZoneIdentifier
    -- ^ One or more subnet IDs, if applicable, separated by commas.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'AutoScalingGroup' value with any optional fields omitted.
mkAutoScalingGroup
    :: Types.AutoScalingGroupName -- ^ 'autoScalingGroupName'
    -> Core.Int -- ^ 'minSize'
    -> Core.Int -- ^ 'maxSize'
    -> Core.Int -- ^ 'desiredCapacity'
    -> Core.Int -- ^ 'defaultCooldown'
    -> Core.NonEmpty Types.XmlStringMaxLen255 -- ^ 'availabilityZones'
    -> Types.HealthCheckType -- ^ 'healthCheckType'
    -> Core.UTCTime -- ^ 'createdTime'
    -> AutoScalingGroup
mkAutoScalingGroup autoScalingGroupName minSize maxSize
  desiredCapacity defaultCooldown availabilityZones healthCheckType
  createdTime
  = AutoScalingGroup'{autoScalingGroupName, minSize, maxSize,
                      desiredCapacity, defaultCooldown, availabilityZones,
                      healthCheckType, createdTime, autoScalingGroupARN = Core.Nothing,
                      capacityRebalance = Core.Nothing, enabledMetrics = Core.Nothing,
                      healthCheckGracePeriod = Core.Nothing, instances = Core.Nothing,
                      launchConfigurationName = Core.Nothing,
                      launchTemplate = Core.Nothing, loadBalancerNames = Core.Nothing,
                      maxInstanceLifetime = Core.Nothing,
                      mixedInstancesPolicy = Core.Nothing,
                      newInstancesProtectedFromScaleIn = Core.Nothing,
                      placementGroup = Core.Nothing, serviceLinkedRoleARN = Core.Nothing,
                      status = Core.Nothing, suspendedProcesses = Core.Nothing,
                      tags = Core.Nothing, targetGroupARNs = Core.Nothing,
                      terminationPolicies = Core.Nothing,
                      vPCZoneIdentifier = Core.Nothing}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgAutoScalingGroupName :: Lens.Lens' AutoScalingGroup Types.AutoScalingGroupName
asgAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# INLINEABLE asgAutoScalingGroupName #-}
{-# DEPRECATED autoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead"  #-}

-- | The minimum size of the group.
--
-- /Note:/ Consider using 'minSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgMinSize :: Lens.Lens' AutoScalingGroup Core.Int
asgMinSize = Lens.field @"minSize"
{-# INLINEABLE asgMinSize #-}
{-# DEPRECATED minSize "Use generic-lens or generic-optics with 'minSize' instead"  #-}

-- | The maximum size of the group.
--
-- /Note:/ Consider using 'maxSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgMaxSize :: Lens.Lens' AutoScalingGroup Core.Int
asgMaxSize = Lens.field @"maxSize"
{-# INLINEABLE asgMaxSize #-}
{-# DEPRECATED maxSize "Use generic-lens or generic-optics with 'maxSize' instead"  #-}

-- | The desired size of the group.
--
-- /Note:/ Consider using 'desiredCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgDesiredCapacity :: Lens.Lens' AutoScalingGroup Core.Int
asgDesiredCapacity = Lens.field @"desiredCapacity"
{-# INLINEABLE asgDesiredCapacity #-}
{-# DEPRECATED desiredCapacity "Use generic-lens or generic-optics with 'desiredCapacity' instead"  #-}

-- | The duration of the default cooldown period, in seconds.
--
-- /Note:/ Consider using 'defaultCooldown' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgDefaultCooldown :: Lens.Lens' AutoScalingGroup Core.Int
asgDefaultCooldown = Lens.field @"defaultCooldown"
{-# INLINEABLE asgDefaultCooldown #-}
{-# DEPRECATED defaultCooldown "Use generic-lens or generic-optics with 'defaultCooldown' instead"  #-}

-- | One or more Availability Zones for the group.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgAvailabilityZones :: Lens.Lens' AutoScalingGroup (Core.NonEmpty Types.XmlStringMaxLen255)
asgAvailabilityZones = Lens.field @"availabilityZones"
{-# INLINEABLE asgAvailabilityZones #-}
{-# DEPRECATED availabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead"  #-}

-- | The service to use for the health checks. The valid values are @EC2@ and @ELB@ . If you configure an Auto Scaling group to use ELB health checks, it considers the instance unhealthy if it fails either the EC2 status checks or the load balancer health checks.
--
-- /Note:/ Consider using 'healthCheckType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgHealthCheckType :: Lens.Lens' AutoScalingGroup Types.HealthCheckType
asgHealthCheckType = Lens.field @"healthCheckType"
{-# INLINEABLE asgHealthCheckType #-}
{-# DEPRECATED healthCheckType "Use generic-lens or generic-optics with 'healthCheckType' instead"  #-}

-- | The date and time the group was created.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgCreatedTime :: Lens.Lens' AutoScalingGroup Core.UTCTime
asgCreatedTime = Lens.field @"createdTime"
{-# INLINEABLE asgCreatedTime #-}
{-# DEPRECATED createdTime "Use generic-lens or generic-optics with 'createdTime' instead"  #-}

-- | The Amazon Resource Name (ARN) of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgAutoScalingGroupARN :: Lens.Lens' AutoScalingGroup (Core.Maybe Types.AutoScalingGroupARN)
asgAutoScalingGroupARN = Lens.field @"autoScalingGroupARN"
{-# INLINEABLE asgAutoScalingGroupARN #-}
{-# DEPRECATED autoScalingGroupARN "Use generic-lens or generic-optics with 'autoScalingGroupARN' instead"  #-}

-- | Indicates whether Capacity Rebalancing is enabled.
--
-- /Note:/ Consider using 'capacityRebalance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgCapacityRebalance :: Lens.Lens' AutoScalingGroup (Core.Maybe Core.Bool)
asgCapacityRebalance = Lens.field @"capacityRebalance"
{-# INLINEABLE asgCapacityRebalance #-}
{-# DEPRECATED capacityRebalance "Use generic-lens or generic-optics with 'capacityRebalance' instead"  #-}

-- | The metrics enabled for the group.
--
-- /Note:/ Consider using 'enabledMetrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgEnabledMetrics :: Lens.Lens' AutoScalingGroup (Core.Maybe [Types.EnabledMetric])
asgEnabledMetrics = Lens.field @"enabledMetrics"
{-# INLINEABLE asgEnabledMetrics #-}
{-# DEPRECATED enabledMetrics "Use generic-lens or generic-optics with 'enabledMetrics' instead"  #-}

-- | The amount of time, in seconds, that Amazon EC2 Auto Scaling waits before checking the health status of an EC2 instance that has come into service.
--
-- /Note:/ Consider using 'healthCheckGracePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgHealthCheckGracePeriod :: Lens.Lens' AutoScalingGroup (Core.Maybe Core.Int)
asgHealthCheckGracePeriod = Lens.field @"healthCheckGracePeriod"
{-# INLINEABLE asgHealthCheckGracePeriod #-}
{-# DEPRECATED healthCheckGracePeriod "Use generic-lens or generic-optics with 'healthCheckGracePeriod' instead"  #-}

-- | The EC2 instances associated with the group.
--
-- /Note:/ Consider using 'instances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgInstances :: Lens.Lens' AutoScalingGroup (Core.Maybe [Types.Instance])
asgInstances = Lens.field @"instances"
{-# INLINEABLE asgInstances #-}
{-# DEPRECATED instances "Use generic-lens or generic-optics with 'instances' instead"  #-}

-- | The name of the associated launch configuration.
--
-- /Note:/ Consider using 'launchConfigurationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgLaunchConfigurationName :: Lens.Lens' AutoScalingGroup (Core.Maybe Types.LaunchConfigurationName)
asgLaunchConfigurationName = Lens.field @"launchConfigurationName"
{-# INLINEABLE asgLaunchConfigurationName #-}
{-# DEPRECATED launchConfigurationName "Use generic-lens or generic-optics with 'launchConfigurationName' instead"  #-}

-- | The launch template for the group.
--
-- /Note:/ Consider using 'launchTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgLaunchTemplate :: Lens.Lens' AutoScalingGroup (Core.Maybe Types.LaunchTemplateSpecification)
asgLaunchTemplate = Lens.field @"launchTemplate"
{-# INLINEABLE asgLaunchTemplate #-}
{-# DEPRECATED launchTemplate "Use generic-lens or generic-optics with 'launchTemplate' instead"  #-}

-- | One or more load balancers associated with the group.
--
-- /Note:/ Consider using 'loadBalancerNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgLoadBalancerNames :: Lens.Lens' AutoScalingGroup (Core.Maybe [Types.XmlStringMaxLen255])
asgLoadBalancerNames = Lens.field @"loadBalancerNames"
{-# INLINEABLE asgLoadBalancerNames #-}
{-# DEPRECATED loadBalancerNames "Use generic-lens or generic-optics with 'loadBalancerNames' instead"  #-}

-- | The maximum amount of time, in seconds, that an instance can be in service.
--
-- Valid Range: Minimum value of 0.
--
-- /Note:/ Consider using 'maxInstanceLifetime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgMaxInstanceLifetime :: Lens.Lens' AutoScalingGroup (Core.Maybe Core.Int)
asgMaxInstanceLifetime = Lens.field @"maxInstanceLifetime"
{-# INLINEABLE asgMaxInstanceLifetime #-}
{-# DEPRECATED maxInstanceLifetime "Use generic-lens or generic-optics with 'maxInstanceLifetime' instead"  #-}

-- | The mixed instances policy for the group.
--
-- /Note:/ Consider using 'mixedInstancesPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgMixedInstancesPolicy :: Lens.Lens' AutoScalingGroup (Core.Maybe Types.MixedInstancesPolicy)
asgMixedInstancesPolicy = Lens.field @"mixedInstancesPolicy"
{-# INLINEABLE asgMixedInstancesPolicy #-}
{-# DEPRECATED mixedInstancesPolicy "Use generic-lens or generic-optics with 'mixedInstancesPolicy' instead"  #-}

-- | Indicates whether newly launched instances are protected from termination by Amazon EC2 Auto Scaling when scaling in.
--
-- /Note:/ Consider using 'newInstancesProtectedFromScaleIn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgNewInstancesProtectedFromScaleIn :: Lens.Lens' AutoScalingGroup (Core.Maybe Core.Bool)
asgNewInstancesProtectedFromScaleIn = Lens.field @"newInstancesProtectedFromScaleIn"
{-# INLINEABLE asgNewInstancesProtectedFromScaleIn #-}
{-# DEPRECATED newInstancesProtectedFromScaleIn "Use generic-lens or generic-optics with 'newInstancesProtectedFromScaleIn' instead"  #-}

-- | The name of the placement group into which to launch your instances, if any.
--
-- /Note:/ Consider using 'placementGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgPlacementGroup :: Lens.Lens' AutoScalingGroup (Core.Maybe Types.PlacementGroup)
asgPlacementGroup = Lens.field @"placementGroup"
{-# INLINEABLE asgPlacementGroup #-}
{-# DEPRECATED placementGroup "Use generic-lens or generic-optics with 'placementGroup' instead"  #-}

-- | The Amazon Resource Name (ARN) of the service-linked role that the Auto Scaling group uses to call other AWS services on your behalf.
--
-- /Note:/ Consider using 'serviceLinkedRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgServiceLinkedRoleARN :: Lens.Lens' AutoScalingGroup (Core.Maybe Types.ServiceLinkedRoleARN)
asgServiceLinkedRoleARN = Lens.field @"serviceLinkedRoleARN"
{-# INLINEABLE asgServiceLinkedRoleARN #-}
{-# DEPRECATED serviceLinkedRoleARN "Use generic-lens or generic-optics with 'serviceLinkedRoleARN' instead"  #-}

-- | The current state of the group when the 'DeleteAutoScalingGroup' operation is in progress.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgStatus :: Lens.Lens' AutoScalingGroup (Core.Maybe Types.Status)
asgStatus = Lens.field @"status"
{-# INLINEABLE asgStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The suspended processes associated with the group.
--
-- /Note:/ Consider using 'suspendedProcesses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgSuspendedProcesses :: Lens.Lens' AutoScalingGroup (Core.Maybe [Types.SuspendedProcess])
asgSuspendedProcesses = Lens.field @"suspendedProcesses"
{-# INLINEABLE asgSuspendedProcesses #-}
{-# DEPRECATED suspendedProcesses "Use generic-lens or generic-optics with 'suspendedProcesses' instead"  #-}

-- | The tags for the group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgTags :: Lens.Lens' AutoScalingGroup (Core.Maybe [Types.TagDescription])
asgTags = Lens.field @"tags"
{-# INLINEABLE asgTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The Amazon Resource Names (ARN) of the target groups for your load balancer.
--
-- /Note:/ Consider using 'targetGroupARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgTargetGroupARNs :: Lens.Lens' AutoScalingGroup (Core.Maybe [Types.XmlStringMaxLen511])
asgTargetGroupARNs = Lens.field @"targetGroupARNs"
{-# INLINEABLE asgTargetGroupARNs #-}
{-# DEPRECATED targetGroupARNs "Use generic-lens or generic-optics with 'targetGroupARNs' instead"  #-}

-- | The termination policies for the group.
--
-- /Note:/ Consider using 'terminationPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgTerminationPolicies :: Lens.Lens' AutoScalingGroup (Core.Maybe [Types.XmlStringMaxLen1600])
asgTerminationPolicies = Lens.field @"terminationPolicies"
{-# INLINEABLE asgTerminationPolicies #-}
{-# DEPRECATED terminationPolicies "Use generic-lens or generic-optics with 'terminationPolicies' instead"  #-}

-- | One or more subnet IDs, if applicable, separated by commas.
--
-- /Note:/ Consider using 'vPCZoneIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgVPCZoneIdentifier :: Lens.Lens' AutoScalingGroup (Core.Maybe Types.VPCZoneIdentifier)
asgVPCZoneIdentifier = Lens.field @"vPCZoneIdentifier"
{-# INLINEABLE asgVPCZoneIdentifier #-}
{-# DEPRECATED vPCZoneIdentifier "Use generic-lens or generic-optics with 'vPCZoneIdentifier' instead"  #-}

instance Core.FromXML AutoScalingGroup where
        parseXML x
          = AutoScalingGroup' Core.<$>
              (x Core..@ "AutoScalingGroupName") Core.<*> x Core..@ "MinSize"
                Core.<*> x Core..@ "MaxSize"
                Core.<*> x Core..@ "DesiredCapacity"
                Core.<*> x Core..@ "DefaultCooldown"
                Core.<*>
                x Core..@ "AvailabilityZones" Core..<@>
                  Core.parseXMLNonEmpty "member"
                Core.<*> x Core..@ "HealthCheckType"
                Core.<*> x Core..@ "CreatedTime"
                Core.<*> x Core..@? "AutoScalingGroupARN"
                Core.<*> x Core..@? "CapacityRebalance"
                Core.<*>
                x Core..@? "EnabledMetrics" Core..<@> Core.parseXMLList "member"
                Core.<*> x Core..@? "HealthCheckGracePeriod"
                Core.<*>
                x Core..@? "Instances" Core..<@> Core.parseXMLList "member"
                Core.<*> x Core..@? "LaunchConfigurationName"
                Core.<*> x Core..@? "LaunchTemplate"
                Core.<*>
                x Core..@? "LoadBalancerNames" Core..<@> Core.parseXMLList "member"
                Core.<*> x Core..@? "MaxInstanceLifetime"
                Core.<*> x Core..@? "MixedInstancesPolicy"
                Core.<*> x Core..@? "NewInstancesProtectedFromScaleIn"
                Core.<*> x Core..@? "PlacementGroup"
                Core.<*> x Core..@? "ServiceLinkedRoleARN"
                Core.<*> x Core..@? "Status"
                Core.<*>
                x Core..@? "SuspendedProcesses" Core..<@>
                  Core.parseXMLList "member"
                Core.<*> x Core..@? "Tags" Core..<@> Core.parseXMLList "member"
                Core.<*>
                x Core..@? "TargetGroupARNs" Core..<@> Core.parseXMLList "member"
                Core.<*>
                x Core..@? "TerminationPolicies" Core..<@>
                  Core.parseXMLList "member"
                Core.<*> x Core..@? "VPCZoneIdentifier"
