{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotFleetRequestConfigData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SpotFleetRequestConfigData
  ( SpotFleetRequestConfigData (..),

    -- * Smart constructor
    mkSpotFleetRequestConfigData,

    -- * Lenses
    sfrcdIamFleetRole,
    sfrcdTargetCapacity,
    sfrcdAllocationStrategy,
    sfrcdClientToken,
    sfrcdExcessCapacityTerminationPolicy,
    sfrcdFulfilledCapacity,
    sfrcdInstanceInterruptionBehavior,
    sfrcdInstancePoolsToUseCount,
    sfrcdLaunchSpecifications,
    sfrcdLaunchTemplateConfigs,
    sfrcdLoadBalancersConfig,
    sfrcdOnDemandAllocationStrategy,
    sfrcdOnDemandFulfilledCapacity,
    sfrcdOnDemandMaxTotalPrice,
    sfrcdOnDemandTargetCapacity,
    sfrcdReplaceUnhealthyInstances,
    sfrcdSpotMaintenanceStrategies,
    sfrcdSpotMaxTotalPrice,
    sfrcdSpotPrice,
    sfrcdTagSpecifications,
    sfrcdTerminateInstancesWithExpiration,
    sfrcdType,
    sfrcdValidFrom,
    sfrcdValidUntil,
  )
where

import qualified Network.AWS.EC2.Types.AllocationStrategy as Types
import qualified Network.AWS.EC2.Types.ExcessCapacityTerminationPolicy as Types
import qualified Network.AWS.EC2.Types.FleetType as Types
import qualified Network.AWS.EC2.Types.InstanceInterruptionBehavior as Types
import qualified Network.AWS.EC2.Types.LaunchTemplateConfig as Types
import qualified Network.AWS.EC2.Types.LoadBalancersConfig as Types
import qualified Network.AWS.EC2.Types.OnDemandAllocationStrategy as Types
import qualified Network.AWS.EC2.Types.SpotFleetLaunchSpecification as Types
import qualified Network.AWS.EC2.Types.SpotMaintenanceStrategies as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.EC2.Types.TagSpecification as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the configuration of a Spot Fleet request.
--
-- /See:/ 'mkSpotFleetRequestConfigData' smart constructor.
data SpotFleetRequestConfigData = SpotFleetRequestConfigData'
  { -- | The Amazon Resource Name (ARN) of an AWS Identity and Access Management (IAM) role that grants the Spot Fleet the permission to request, launch, terminate, and tag instances on your behalf. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-fleet-requests.html#spot-fleet-prerequisites Spot Fleet prerequisites> in the /Amazon EC2 User Guide for Linux Instances/ . Spot Fleet can terminate Spot Instances on your behalf when you cancel its Spot Fleet request using <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CancelSpotFleetRequests CancelSpotFleetRequests> or when the Spot Fleet request expires, if you set @TerminateInstancesWithExpiration@ .
    iamFleetRole :: Types.String,
    -- | The number of units to request for the Spot Fleet. You can choose to set the target capacity in terms of instances or a performance characteristic that is important to your application workload, such as vCPUs, memory, or I/O. If the request type is @maintain@ , you can specify a target capacity of 0 and add capacity later.
    targetCapacity :: Core.Int,
    -- | Indicates how to allocate the target Spot Instance capacity across the Spot Instance pools specified by the Spot Fleet request.
    --
    -- If the allocation strategy is @lowestPrice@ , Spot Fleet launches instances from the Spot Instance pools with the lowest price. This is the default allocation strategy.
    -- If the allocation strategy is @diversified@ , Spot Fleet launches instances from all the Spot Instance pools that you specify.
    -- If the allocation strategy is @capacityOptimized@ , Spot Fleet launches instances from Spot Instance pools with optimal capacity for the number of instances that are launching.
    allocationStrategy :: Core.Maybe Types.AllocationStrategy,
    -- | A unique, case-sensitive identifier that you provide to ensure the idempotency of your listings. This helps to avoid duplicate listings. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
    clientToken :: Core.Maybe Types.String,
    -- | Indicates whether running Spot Instances should be terminated if you decrease the target capacity of the Spot Fleet request below the current size of the Spot Fleet.
    excessCapacityTerminationPolicy :: Core.Maybe Types.ExcessCapacityTerminationPolicy,
    -- | The number of units fulfilled by this request compared to the set target capacity. You cannot set this value.
    fulfilledCapacity :: Core.Maybe Core.Double,
    -- | The behavior when a Spot Instance is interrupted. The default is @terminate@ .
    instanceInterruptionBehavior :: Core.Maybe Types.InstanceInterruptionBehavior,
    -- | The number of Spot pools across which to allocate your target Spot capacity. Valid only when Spot __AllocationStrategy__ is set to @lowest-price@ . Spot Fleet selects the cheapest Spot pools and evenly allocates your target Spot capacity across the number of Spot pools that you specify.
    instancePoolsToUseCount :: Core.Maybe Core.Int,
    -- | The launch specifications for the Spot Fleet request. If you specify @LaunchSpecifications@ , you can't specify @LaunchTemplateConfigs@ . If you include On-Demand capacity in your request, you must use @LaunchTemplateConfigs@ .
    launchSpecifications :: Core.Maybe [Types.SpotFleetLaunchSpecification],
    -- | The launch template and overrides. If you specify @LaunchTemplateConfigs@ , you can't specify @LaunchSpecifications@ . If you include On-Demand capacity in your request, you must use @LaunchTemplateConfigs@ .
    launchTemplateConfigs :: Core.Maybe [Types.LaunchTemplateConfig],
    -- | One or more Classic Load Balancers and target groups to attach to the Spot Fleet request. Spot Fleet registers the running Spot Instances with the specified Classic Load Balancers and target groups.
    --
    -- With Network Load Balancers, Spot Fleet cannot register instances that have the following instance types: C1, CC1, CC2, CG1, CG2, CR1, CS1, G1, G2, HI1, HS1, M1, M2, M3, and T1.
    loadBalancersConfig :: Core.Maybe Types.LoadBalancersConfig,
    -- | The order of the launch template overrides to use in fulfilling On-Demand capacity. If you specify @lowestPrice@ , Spot Fleet uses price to determine the order, launching the lowest price first. If you specify @prioritized@ , Spot Fleet uses the priority that you assign to each Spot Fleet launch template override, launching the highest priority first. If you do not specify a value, Spot Fleet defaults to @lowestPrice@ .
    onDemandAllocationStrategy :: Core.Maybe Types.OnDemandAllocationStrategy,
    -- | The number of On-Demand units fulfilled by this request compared to the set target On-Demand capacity.
    onDemandFulfilledCapacity :: Core.Maybe Core.Double,
    -- | The maximum amount per hour for On-Demand Instances that you're willing to pay. You can use the @onDemandMaxTotalPrice@ parameter, the @spotMaxTotalPrice@ parameter, or both parameters to ensure that your fleet cost does not exceed your budget. If you set a maximum price per hour for the On-Demand Instances and Spot Instances in your request, Spot Fleet will launch instances until it reaches the maximum amount you're willing to pay. When the maximum amount you're willing to pay is reached, the fleet stops launching instances even if it hasn’t met the target capacity.
    onDemandMaxTotalPrice :: Core.Maybe Types.String,
    -- | The number of On-Demand units to request. You can choose to set the target capacity in terms of instances or a performance characteristic that is important to your application workload, such as vCPUs, memory, or I/O. If the request type is @maintain@ , you can specify a target capacity of 0 and add capacity later.
    onDemandTargetCapacity :: Core.Maybe Core.Int,
    -- | Indicates whether Spot Fleet should replace unhealthy instances.
    replaceUnhealthyInstances :: Core.Maybe Core.Bool,
    -- | The strategies for managing your Spot Instances that are at an elevated risk of being interrupted.
    spotMaintenanceStrategies :: Core.Maybe Types.SpotMaintenanceStrategies,
    -- | The maximum amount per hour for Spot Instances that you're willing to pay. You can use the @spotdMaxTotalPrice@ parameter, the @onDemandMaxTotalPrice@ parameter, or both parameters to ensure that your fleet cost does not exceed your budget. If you set a maximum price per hour for the On-Demand Instances and Spot Instances in your request, Spot Fleet will launch instances until it reaches the maximum amount you're willing to pay. When the maximum amount you're willing to pay is reached, the fleet stops launching instances even if it hasn’t met the target capacity.
    spotMaxTotalPrice :: Core.Maybe Types.String,
    -- | The maximum price per unit hour that you are willing to pay for a Spot Instance. The default is the On-Demand price.
    spotPrice :: Core.Maybe Types.String,
    -- | The key-value pair for tagging the Spot Fleet request on creation. The value for @ResourceType@ must be @spot-fleet-request@ , otherwise the Spot Fleet request fails. To tag instances at launch, specify the tags in the <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-launch-templates.html#create-launch-template launch template> (valid only if you use @LaunchTemplateConfigs@ ) or in the <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_SpotFleetTagSpecification.html @SpotFleetTagSpecification@ > (valid only if you use @LaunchSpecifications@ ). For information about tagging after launch, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html#tag-resources Tagging Your Resources> .
    tagSpecifications :: Core.Maybe [Types.TagSpecification],
    -- | Indicates whether running Spot Instances are terminated when the Spot Fleet request expires.
    terminateInstancesWithExpiration :: Core.Maybe Core.Bool,
    -- | The type of request. Indicates whether the Spot Fleet only requests the target capacity or also attempts to maintain it. When this value is @request@ , the Spot Fleet only places the required requests. It does not attempt to replenish Spot Instances if capacity is diminished, nor does it submit requests in alternative Spot pools if capacity is not available. When this value is @maintain@ , the Spot Fleet maintains the target capacity. The Spot Fleet places the required requests to meet capacity and automatically replenishes any interrupted instances. Default: @maintain@ . @instant@ is listed but is not used by Spot Fleet.
    type' :: Core.Maybe Types.FleetType,
    -- | The start date and time of the request, in UTC format (/YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). By default, Amazon EC2 starts fulfilling the request immediately.
    validFrom :: Core.Maybe Core.UTCTime,
    -- | The end date and time of the request, in UTC format (/YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). After the end date and time, no new Spot Instance requests are placed or able to fulfill the request. If no value is specified, the Spot Fleet request remains until you cancel it.
    validUntil :: Core.Maybe Core.UTCTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'SpotFleetRequestConfigData' value with any optional fields omitted.
mkSpotFleetRequestConfigData ::
  -- | 'iamFleetRole'
  Types.String ->
  -- | 'targetCapacity'
  Core.Int ->
  SpotFleetRequestConfigData
mkSpotFleetRequestConfigData iamFleetRole targetCapacity =
  SpotFleetRequestConfigData'
    { iamFleetRole,
      targetCapacity,
      allocationStrategy = Core.Nothing,
      clientToken = Core.Nothing,
      excessCapacityTerminationPolicy = Core.Nothing,
      fulfilledCapacity = Core.Nothing,
      instanceInterruptionBehavior = Core.Nothing,
      instancePoolsToUseCount = Core.Nothing,
      launchSpecifications = Core.Nothing,
      launchTemplateConfigs = Core.Nothing,
      loadBalancersConfig = Core.Nothing,
      onDemandAllocationStrategy = Core.Nothing,
      onDemandFulfilledCapacity = Core.Nothing,
      onDemandMaxTotalPrice = Core.Nothing,
      onDemandTargetCapacity = Core.Nothing,
      replaceUnhealthyInstances = Core.Nothing,
      spotMaintenanceStrategies = Core.Nothing,
      spotMaxTotalPrice = Core.Nothing,
      spotPrice = Core.Nothing,
      tagSpecifications = Core.Nothing,
      terminateInstancesWithExpiration = Core.Nothing,
      type' = Core.Nothing,
      validFrom = Core.Nothing,
      validUntil = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of an AWS Identity and Access Management (IAM) role that grants the Spot Fleet the permission to request, launch, terminate, and tag instances on your behalf. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-fleet-requests.html#spot-fleet-prerequisites Spot Fleet prerequisites> in the /Amazon EC2 User Guide for Linux Instances/ . Spot Fleet can terminate Spot Instances on your behalf when you cancel its Spot Fleet request using <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CancelSpotFleetRequests CancelSpotFleetRequests> or when the Spot Fleet request expires, if you set @TerminateInstancesWithExpiration@ .
--
-- /Note:/ Consider using 'iamFleetRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcdIamFleetRole :: Lens.Lens' SpotFleetRequestConfigData Types.String
sfrcdIamFleetRole = Lens.field @"iamFleetRole"
{-# DEPRECATED sfrcdIamFleetRole "Use generic-lens or generic-optics with 'iamFleetRole' instead." #-}

-- | The number of units to request for the Spot Fleet. You can choose to set the target capacity in terms of instances or a performance characteristic that is important to your application workload, such as vCPUs, memory, or I/O. If the request type is @maintain@ , you can specify a target capacity of 0 and add capacity later.
--
-- /Note:/ Consider using 'targetCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcdTargetCapacity :: Lens.Lens' SpotFleetRequestConfigData Core.Int
sfrcdTargetCapacity = Lens.field @"targetCapacity"
{-# DEPRECATED sfrcdTargetCapacity "Use generic-lens or generic-optics with 'targetCapacity' instead." #-}

-- | Indicates how to allocate the target Spot Instance capacity across the Spot Instance pools specified by the Spot Fleet request.
--
-- If the allocation strategy is @lowestPrice@ , Spot Fleet launches instances from the Spot Instance pools with the lowest price. This is the default allocation strategy.
-- If the allocation strategy is @diversified@ , Spot Fleet launches instances from all the Spot Instance pools that you specify.
-- If the allocation strategy is @capacityOptimized@ , Spot Fleet launches instances from Spot Instance pools with optimal capacity for the number of instances that are launching.
--
-- /Note:/ Consider using 'allocationStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcdAllocationStrategy :: Lens.Lens' SpotFleetRequestConfigData (Core.Maybe Types.AllocationStrategy)
sfrcdAllocationStrategy = Lens.field @"allocationStrategy"
{-# DEPRECATED sfrcdAllocationStrategy "Use generic-lens or generic-optics with 'allocationStrategy' instead." #-}

-- | A unique, case-sensitive identifier that you provide to ensure the idempotency of your listings. This helps to avoid duplicate listings. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcdClientToken :: Lens.Lens' SpotFleetRequestConfigData (Core.Maybe Types.String)
sfrcdClientToken = Lens.field @"clientToken"
{-# DEPRECATED sfrcdClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | Indicates whether running Spot Instances should be terminated if you decrease the target capacity of the Spot Fleet request below the current size of the Spot Fleet.
--
-- /Note:/ Consider using 'excessCapacityTerminationPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcdExcessCapacityTerminationPolicy :: Lens.Lens' SpotFleetRequestConfigData (Core.Maybe Types.ExcessCapacityTerminationPolicy)
sfrcdExcessCapacityTerminationPolicy = Lens.field @"excessCapacityTerminationPolicy"
{-# DEPRECATED sfrcdExcessCapacityTerminationPolicy "Use generic-lens or generic-optics with 'excessCapacityTerminationPolicy' instead." #-}

-- | The number of units fulfilled by this request compared to the set target capacity. You cannot set this value.
--
-- /Note:/ Consider using 'fulfilledCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcdFulfilledCapacity :: Lens.Lens' SpotFleetRequestConfigData (Core.Maybe Core.Double)
sfrcdFulfilledCapacity = Lens.field @"fulfilledCapacity"
{-# DEPRECATED sfrcdFulfilledCapacity "Use generic-lens or generic-optics with 'fulfilledCapacity' instead." #-}

-- | The behavior when a Spot Instance is interrupted. The default is @terminate@ .
--
-- /Note:/ Consider using 'instanceInterruptionBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcdInstanceInterruptionBehavior :: Lens.Lens' SpotFleetRequestConfigData (Core.Maybe Types.InstanceInterruptionBehavior)
sfrcdInstanceInterruptionBehavior = Lens.field @"instanceInterruptionBehavior"
{-# DEPRECATED sfrcdInstanceInterruptionBehavior "Use generic-lens or generic-optics with 'instanceInterruptionBehavior' instead." #-}

-- | The number of Spot pools across which to allocate your target Spot capacity. Valid only when Spot __AllocationStrategy__ is set to @lowest-price@ . Spot Fleet selects the cheapest Spot pools and evenly allocates your target Spot capacity across the number of Spot pools that you specify.
--
-- /Note:/ Consider using 'instancePoolsToUseCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcdInstancePoolsToUseCount :: Lens.Lens' SpotFleetRequestConfigData (Core.Maybe Core.Int)
sfrcdInstancePoolsToUseCount = Lens.field @"instancePoolsToUseCount"
{-# DEPRECATED sfrcdInstancePoolsToUseCount "Use generic-lens or generic-optics with 'instancePoolsToUseCount' instead." #-}

-- | The launch specifications for the Spot Fleet request. If you specify @LaunchSpecifications@ , you can't specify @LaunchTemplateConfigs@ . If you include On-Demand capacity in your request, you must use @LaunchTemplateConfigs@ .
--
-- /Note:/ Consider using 'launchSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcdLaunchSpecifications :: Lens.Lens' SpotFleetRequestConfigData (Core.Maybe [Types.SpotFleetLaunchSpecification])
sfrcdLaunchSpecifications = Lens.field @"launchSpecifications"
{-# DEPRECATED sfrcdLaunchSpecifications "Use generic-lens or generic-optics with 'launchSpecifications' instead." #-}

-- | The launch template and overrides. If you specify @LaunchTemplateConfigs@ , you can't specify @LaunchSpecifications@ . If you include On-Demand capacity in your request, you must use @LaunchTemplateConfigs@ .
--
-- /Note:/ Consider using 'launchTemplateConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcdLaunchTemplateConfigs :: Lens.Lens' SpotFleetRequestConfigData (Core.Maybe [Types.LaunchTemplateConfig])
sfrcdLaunchTemplateConfigs = Lens.field @"launchTemplateConfigs"
{-# DEPRECATED sfrcdLaunchTemplateConfigs "Use generic-lens or generic-optics with 'launchTemplateConfigs' instead." #-}

-- | One or more Classic Load Balancers and target groups to attach to the Spot Fleet request. Spot Fleet registers the running Spot Instances with the specified Classic Load Balancers and target groups.
--
-- With Network Load Balancers, Spot Fleet cannot register instances that have the following instance types: C1, CC1, CC2, CG1, CG2, CR1, CS1, G1, G2, HI1, HS1, M1, M2, M3, and T1.
--
-- /Note:/ Consider using 'loadBalancersConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcdLoadBalancersConfig :: Lens.Lens' SpotFleetRequestConfigData (Core.Maybe Types.LoadBalancersConfig)
sfrcdLoadBalancersConfig = Lens.field @"loadBalancersConfig"
{-# DEPRECATED sfrcdLoadBalancersConfig "Use generic-lens or generic-optics with 'loadBalancersConfig' instead." #-}

-- | The order of the launch template overrides to use in fulfilling On-Demand capacity. If you specify @lowestPrice@ , Spot Fleet uses price to determine the order, launching the lowest price first. If you specify @prioritized@ , Spot Fleet uses the priority that you assign to each Spot Fleet launch template override, launching the highest priority first. If you do not specify a value, Spot Fleet defaults to @lowestPrice@ .
--
-- /Note:/ Consider using 'onDemandAllocationStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcdOnDemandAllocationStrategy :: Lens.Lens' SpotFleetRequestConfigData (Core.Maybe Types.OnDemandAllocationStrategy)
sfrcdOnDemandAllocationStrategy = Lens.field @"onDemandAllocationStrategy"
{-# DEPRECATED sfrcdOnDemandAllocationStrategy "Use generic-lens or generic-optics with 'onDemandAllocationStrategy' instead." #-}

-- | The number of On-Demand units fulfilled by this request compared to the set target On-Demand capacity.
--
-- /Note:/ Consider using 'onDemandFulfilledCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcdOnDemandFulfilledCapacity :: Lens.Lens' SpotFleetRequestConfigData (Core.Maybe Core.Double)
sfrcdOnDemandFulfilledCapacity = Lens.field @"onDemandFulfilledCapacity"
{-# DEPRECATED sfrcdOnDemandFulfilledCapacity "Use generic-lens or generic-optics with 'onDemandFulfilledCapacity' instead." #-}

-- | The maximum amount per hour for On-Demand Instances that you're willing to pay. You can use the @onDemandMaxTotalPrice@ parameter, the @spotMaxTotalPrice@ parameter, or both parameters to ensure that your fleet cost does not exceed your budget. If you set a maximum price per hour for the On-Demand Instances and Spot Instances in your request, Spot Fleet will launch instances until it reaches the maximum amount you're willing to pay. When the maximum amount you're willing to pay is reached, the fleet stops launching instances even if it hasn’t met the target capacity.
--
-- /Note:/ Consider using 'onDemandMaxTotalPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcdOnDemandMaxTotalPrice :: Lens.Lens' SpotFleetRequestConfigData (Core.Maybe Types.String)
sfrcdOnDemandMaxTotalPrice = Lens.field @"onDemandMaxTotalPrice"
{-# DEPRECATED sfrcdOnDemandMaxTotalPrice "Use generic-lens or generic-optics with 'onDemandMaxTotalPrice' instead." #-}

-- | The number of On-Demand units to request. You can choose to set the target capacity in terms of instances or a performance characteristic that is important to your application workload, such as vCPUs, memory, or I/O. If the request type is @maintain@ , you can specify a target capacity of 0 and add capacity later.
--
-- /Note:/ Consider using 'onDemandTargetCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcdOnDemandTargetCapacity :: Lens.Lens' SpotFleetRequestConfigData (Core.Maybe Core.Int)
sfrcdOnDemandTargetCapacity = Lens.field @"onDemandTargetCapacity"
{-# DEPRECATED sfrcdOnDemandTargetCapacity "Use generic-lens or generic-optics with 'onDemandTargetCapacity' instead." #-}

-- | Indicates whether Spot Fleet should replace unhealthy instances.
--
-- /Note:/ Consider using 'replaceUnhealthyInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcdReplaceUnhealthyInstances :: Lens.Lens' SpotFleetRequestConfigData (Core.Maybe Core.Bool)
sfrcdReplaceUnhealthyInstances = Lens.field @"replaceUnhealthyInstances"
{-# DEPRECATED sfrcdReplaceUnhealthyInstances "Use generic-lens or generic-optics with 'replaceUnhealthyInstances' instead." #-}

-- | The strategies for managing your Spot Instances that are at an elevated risk of being interrupted.
--
-- /Note:/ Consider using 'spotMaintenanceStrategies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcdSpotMaintenanceStrategies :: Lens.Lens' SpotFleetRequestConfigData (Core.Maybe Types.SpotMaintenanceStrategies)
sfrcdSpotMaintenanceStrategies = Lens.field @"spotMaintenanceStrategies"
{-# DEPRECATED sfrcdSpotMaintenanceStrategies "Use generic-lens or generic-optics with 'spotMaintenanceStrategies' instead." #-}

-- | The maximum amount per hour for Spot Instances that you're willing to pay. You can use the @spotdMaxTotalPrice@ parameter, the @onDemandMaxTotalPrice@ parameter, or both parameters to ensure that your fleet cost does not exceed your budget. If you set a maximum price per hour for the On-Demand Instances and Spot Instances in your request, Spot Fleet will launch instances until it reaches the maximum amount you're willing to pay. When the maximum amount you're willing to pay is reached, the fleet stops launching instances even if it hasn’t met the target capacity.
--
-- /Note:/ Consider using 'spotMaxTotalPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcdSpotMaxTotalPrice :: Lens.Lens' SpotFleetRequestConfigData (Core.Maybe Types.String)
sfrcdSpotMaxTotalPrice = Lens.field @"spotMaxTotalPrice"
{-# DEPRECATED sfrcdSpotMaxTotalPrice "Use generic-lens or generic-optics with 'spotMaxTotalPrice' instead." #-}

-- | The maximum price per unit hour that you are willing to pay for a Spot Instance. The default is the On-Demand price.
--
-- /Note:/ Consider using 'spotPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcdSpotPrice :: Lens.Lens' SpotFleetRequestConfigData (Core.Maybe Types.String)
sfrcdSpotPrice = Lens.field @"spotPrice"
{-# DEPRECATED sfrcdSpotPrice "Use generic-lens or generic-optics with 'spotPrice' instead." #-}

-- | The key-value pair for tagging the Spot Fleet request on creation. The value for @ResourceType@ must be @spot-fleet-request@ , otherwise the Spot Fleet request fails. To tag instances at launch, specify the tags in the <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-launch-templates.html#create-launch-template launch template> (valid only if you use @LaunchTemplateConfigs@ ) or in the <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_SpotFleetTagSpecification.html @SpotFleetTagSpecification@ > (valid only if you use @LaunchSpecifications@ ). For information about tagging after launch, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html#tag-resources Tagging Your Resources> .
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcdTagSpecifications :: Lens.Lens' SpotFleetRequestConfigData (Core.Maybe [Types.TagSpecification])
sfrcdTagSpecifications = Lens.field @"tagSpecifications"
{-# DEPRECATED sfrcdTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | Indicates whether running Spot Instances are terminated when the Spot Fleet request expires.
--
-- /Note:/ Consider using 'terminateInstancesWithExpiration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcdTerminateInstancesWithExpiration :: Lens.Lens' SpotFleetRequestConfigData (Core.Maybe Core.Bool)
sfrcdTerminateInstancesWithExpiration = Lens.field @"terminateInstancesWithExpiration"
{-# DEPRECATED sfrcdTerminateInstancesWithExpiration "Use generic-lens or generic-optics with 'terminateInstancesWithExpiration' instead." #-}

-- | The type of request. Indicates whether the Spot Fleet only requests the target capacity or also attempts to maintain it. When this value is @request@ , the Spot Fleet only places the required requests. It does not attempt to replenish Spot Instances if capacity is diminished, nor does it submit requests in alternative Spot pools if capacity is not available. When this value is @maintain@ , the Spot Fleet maintains the target capacity. The Spot Fleet places the required requests to meet capacity and automatically replenishes any interrupted instances. Default: @maintain@ . @instant@ is listed but is not used by Spot Fleet.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcdType :: Lens.Lens' SpotFleetRequestConfigData (Core.Maybe Types.FleetType)
sfrcdType = Lens.field @"type'"
{-# DEPRECATED sfrcdType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The start date and time of the request, in UTC format (/YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). By default, Amazon EC2 starts fulfilling the request immediately.
--
-- /Note:/ Consider using 'validFrom' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcdValidFrom :: Lens.Lens' SpotFleetRequestConfigData (Core.Maybe Core.UTCTime)
sfrcdValidFrom = Lens.field @"validFrom"
{-# DEPRECATED sfrcdValidFrom "Use generic-lens or generic-optics with 'validFrom' instead." #-}

-- | The end date and time of the request, in UTC format (/YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). After the end date and time, no new Spot Instance requests are placed or able to fulfill the request. If no value is specified, the Spot Fleet request remains until you cancel it.
--
-- /Note:/ Consider using 'validUntil' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcdValidUntil :: Lens.Lens' SpotFleetRequestConfigData (Core.Maybe Core.UTCTime)
sfrcdValidUntil = Lens.field @"validUntil"
{-# DEPRECATED sfrcdValidUntil "Use generic-lens or generic-optics with 'validUntil' instead." #-}

instance Core.FromXML SpotFleetRequestConfigData where
  parseXML x =
    SpotFleetRequestConfigData'
      Core.<$> (x Core..@ "iamFleetRole")
      Core.<*> (x Core..@ "targetCapacity")
      Core.<*> (x Core..@? "allocationStrategy")
      Core.<*> (x Core..@? "clientToken")
      Core.<*> (x Core..@? "excessCapacityTerminationPolicy")
      Core.<*> (x Core..@? "fulfilledCapacity")
      Core.<*> (x Core..@? "instanceInterruptionBehavior")
      Core.<*> (x Core..@? "instancePoolsToUseCount")
      Core.<*> ( x Core..@? "launchSpecifications"
                   Core..<@> Core.parseXMLList "item"
               )
      Core.<*> ( x Core..@? "launchTemplateConfigs"
                   Core..<@> Core.parseXMLList "item"
               )
      Core.<*> (x Core..@? "loadBalancersConfig")
      Core.<*> (x Core..@? "onDemandAllocationStrategy")
      Core.<*> (x Core..@? "onDemandFulfilledCapacity")
      Core.<*> (x Core..@? "onDemandMaxTotalPrice")
      Core.<*> (x Core..@? "onDemandTargetCapacity")
      Core.<*> (x Core..@? "replaceUnhealthyInstances")
      Core.<*> (x Core..@? "spotMaintenanceStrategies")
      Core.<*> (x Core..@? "spotMaxTotalPrice")
      Core.<*> (x Core..@? "spotPrice")
      Core.<*> (x Core..@? "TagSpecification" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@? "terminateInstancesWithExpiration")
      Core.<*> (x Core..@? "type")
      Core.<*> (x Core..@? "validFrom")
      Core.<*> (x Core..@? "validUntil")
