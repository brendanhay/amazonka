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
    sfrcdClientToken,
    sfrcdInstanceInterruptionBehavior,
    sfrcdOnDemandMaxTotalPrice,
    sfrcdSpotPrice,
    sfrcdSpotMaintenanceStrategies,
    sfrcdLoadBalancersConfig,
    sfrcdExcessCapacityTerminationPolicy,
    sfrcdOnDemandTargetCapacity,
    sfrcdLaunchTemplateConfigs,
    sfrcdTagSpecifications,
    sfrcdValidUntil,
    sfrcdTerminateInstancesWithExpiration,
    sfrcdOnDemandAllocationStrategy,
    sfrcdInstancePoolsToUseCount,
    sfrcdFulfilledCapacity,
    sfrcdType,
    sfrcdValidFrom,
    sfrcdReplaceUnhealthyInstances,
    sfrcdLaunchSpecifications,
    sfrcdOnDemandFulfilledCapacity,
    sfrcdSpotMaxTotalPrice,
    sfrcdAllocationStrategy,
    sfrcdIAMFleetRole,
    sfrcdTargetCapacity,
  )
where

import Network.AWS.EC2.Types.AllocationStrategy
import Network.AWS.EC2.Types.ExcessCapacityTerminationPolicy
import Network.AWS.EC2.Types.FleetType
import Network.AWS.EC2.Types.InstanceInterruptionBehavior
import Network.AWS.EC2.Types.LaunchTemplateConfig
import Network.AWS.EC2.Types.LoadBalancersConfig
import Network.AWS.EC2.Types.OnDemandAllocationStrategy
import Network.AWS.EC2.Types.SpotFleetLaunchSpecification
import Network.AWS.EC2.Types.SpotMaintenanceStrategies
import Network.AWS.EC2.Types.TagSpecification
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the configuration of a Spot Fleet request.
--
-- /See:/ 'mkSpotFleetRequestConfigData' smart constructor.
data SpotFleetRequestConfigData = SpotFleetRequestConfigData'
  { clientToken ::
      Lude.Maybe Lude.Text,
    instanceInterruptionBehavior ::
      Lude.Maybe
        InstanceInterruptionBehavior,
    onDemandMaxTotalPrice ::
      Lude.Maybe Lude.Text,
    spotPrice :: Lude.Maybe Lude.Text,
    spotMaintenanceStrategies ::
      Lude.Maybe SpotMaintenanceStrategies,
    loadBalancersConfig ::
      Lude.Maybe LoadBalancersConfig,
    excessCapacityTerminationPolicy ::
      Lude.Maybe
        ExcessCapacityTerminationPolicy,
    onDemandTargetCapacity ::
      Lude.Maybe Lude.Int,
    launchTemplateConfigs ::
      Lude.Maybe [LaunchTemplateConfig],
    tagSpecifications ::
      Lude.Maybe [TagSpecification],
    validUntil ::
      Lude.Maybe Lude.DateTime,
    terminateInstancesWithExpiration ::
      Lude.Maybe Lude.Bool,
    onDemandAllocationStrategy ::
      Lude.Maybe OnDemandAllocationStrategy,
    instancePoolsToUseCount ::
      Lude.Maybe Lude.Int,
    fulfilledCapacity ::
      Lude.Maybe Lude.Double,
    type' :: Lude.Maybe FleetType,
    validFrom :: Lude.Maybe Lude.DateTime,
    replaceUnhealthyInstances ::
      Lude.Maybe Lude.Bool,
    launchSpecifications ::
      Lude.Maybe
        [SpotFleetLaunchSpecification],
    onDemandFulfilledCapacity ::
      Lude.Maybe Lude.Double,
    spotMaxTotalPrice ::
      Lude.Maybe Lude.Text,
    allocationStrategy ::
      Lude.Maybe AllocationStrategy,
    iamFleetRole :: Lude.Text,
    targetCapacity :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SpotFleetRequestConfigData' with the minimum fields required to make a request.
--
-- * 'allocationStrategy' - Indicates how to allocate the target Spot Instance capacity across the Spot Instance pools specified by the Spot Fleet request.
--
-- If the allocation strategy is @lowestPrice@ , Spot Fleet launches instances from the Spot Instance pools with the lowest price. This is the default allocation strategy.
-- If the allocation strategy is @diversified@ , Spot Fleet launches instances from all the Spot Instance pools that you specify.
-- If the allocation strategy is @capacityOptimized@ , Spot Fleet launches instances from Spot Instance pools with optimal capacity for the number of instances that are launching.
-- * 'clientToken' - A unique, case-sensitive identifier that you provide to ensure the idempotency of your listings. This helps to avoid duplicate listings. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
-- * 'excessCapacityTerminationPolicy' - Indicates whether running Spot Instances should be terminated if you decrease the target capacity of the Spot Fleet request below the current size of the Spot Fleet.
-- * 'fulfilledCapacity' - The number of units fulfilled by this request compared to the set target capacity. You cannot set this value.
-- * 'iamFleetRole' - The Amazon Resource Name (ARN) of an AWS Identity and Access Management (IAM) role that grants the Spot Fleet the permission to request, launch, terminate, and tag instances on your behalf. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-fleet-requests.html#spot-fleet-prerequisites Spot Fleet prerequisites> in the /Amazon EC2 User Guide for Linux Instances/ . Spot Fleet can terminate Spot Instances on your behalf when you cancel its Spot Fleet request using <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CancelSpotFleetRequests CancelSpotFleetRequests> or when the Spot Fleet request expires, if you set @TerminateInstancesWithExpiration@ .
-- * 'instanceInterruptionBehavior' - The behavior when a Spot Instance is interrupted. The default is @terminate@ .
-- * 'instancePoolsToUseCount' - The number of Spot pools across which to allocate your target Spot capacity. Valid only when Spot __AllocationStrategy__ is set to @lowest-price@ . Spot Fleet selects the cheapest Spot pools and evenly allocates your target Spot capacity across the number of Spot pools that you specify.
-- * 'launchSpecifications' - The launch specifications for the Spot Fleet request. If you specify @LaunchSpecifications@ , you can't specify @LaunchTemplateConfigs@ . If you include On-Demand capacity in your request, you must use @LaunchTemplateConfigs@ .
-- * 'launchTemplateConfigs' - The launch template and overrides. If you specify @LaunchTemplateConfigs@ , you can't specify @LaunchSpecifications@ . If you include On-Demand capacity in your request, you must use @LaunchTemplateConfigs@ .
-- * 'loadBalancersConfig' - One or more Classic Load Balancers and target groups to attach to the Spot Fleet request. Spot Fleet registers the running Spot Instances with the specified Classic Load Balancers and target groups.
--
-- With Network Load Balancers, Spot Fleet cannot register instances that have the following instance types: C1, CC1, CC2, CG1, CG2, CR1, CS1, G1, G2, HI1, HS1, M1, M2, M3, and T1.
-- * 'onDemandAllocationStrategy' - The order of the launch template overrides to use in fulfilling On-Demand capacity. If you specify @lowestPrice@ , Spot Fleet uses price to determine the order, launching the lowest price first. If you specify @prioritized@ , Spot Fleet uses the priority that you assign to each Spot Fleet launch template override, launching the highest priority first. If you do not specify a value, Spot Fleet defaults to @lowestPrice@ .
-- * 'onDemandFulfilledCapacity' - The number of On-Demand units fulfilled by this request compared to the set target On-Demand capacity.
-- * 'onDemandMaxTotalPrice' - The maximum amount per hour for On-Demand Instances that you're willing to pay. You can use the @onDemandMaxTotalPrice@ parameter, the @spotMaxTotalPrice@ parameter, or both parameters to ensure that your fleet cost does not exceed your budget. If you set a maximum price per hour for the On-Demand Instances and Spot Instances in your request, Spot Fleet will launch instances until it reaches the maximum amount you're willing to pay. When the maximum amount you're willing to pay is reached, the fleet stops launching instances even if it hasn’t met the target capacity.
-- * 'onDemandTargetCapacity' - The number of On-Demand units to request. You can choose to set the target capacity in terms of instances or a performance characteristic that is important to your application workload, such as vCPUs, memory, or I/O. If the request type is @maintain@ , you can specify a target capacity of 0 and add capacity later.
-- * 'replaceUnhealthyInstances' - Indicates whether Spot Fleet should replace unhealthy instances.
-- * 'spotMaintenanceStrategies' - The strategies for managing your Spot Instances that are at an elevated risk of being interrupted.
-- * 'spotMaxTotalPrice' - The maximum amount per hour for Spot Instances that you're willing to pay. You can use the @spotdMaxTotalPrice@ parameter, the @onDemandMaxTotalPrice@ parameter, or both parameters to ensure that your fleet cost does not exceed your budget. If you set a maximum price per hour for the On-Demand Instances and Spot Instances in your request, Spot Fleet will launch instances until it reaches the maximum amount you're willing to pay. When the maximum amount you're willing to pay is reached, the fleet stops launching instances even if it hasn’t met the target capacity.
-- * 'spotPrice' - The maximum price per unit hour that you are willing to pay for a Spot Instance. The default is the On-Demand price.
-- * 'tagSpecifications' - The key-value pair for tagging the Spot Fleet request on creation. The value for @ResourceType@ must be @spot-fleet-request@ , otherwise the Spot Fleet request fails. To tag instances at launch, specify the tags in the <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-launch-templates.html#create-launch-template launch template> (valid only if you use @LaunchTemplateConfigs@ ) or in the <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_SpotFleetTagSpecification.html @SpotFleetTagSpecification@ > (valid only if you use @LaunchSpecifications@ ). For information about tagging after launch, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html#tag-resources Tagging Your Resources> .
-- * 'targetCapacity' - The number of units to request for the Spot Fleet. You can choose to set the target capacity in terms of instances or a performance characteristic that is important to your application workload, such as vCPUs, memory, or I/O. If the request type is @maintain@ , you can specify a target capacity of 0 and add capacity later.
-- * 'terminateInstancesWithExpiration' - Indicates whether running Spot Instances are terminated when the Spot Fleet request expires.
-- * 'type'' - The type of request. Indicates whether the Spot Fleet only requests the target capacity or also attempts to maintain it. When this value is @request@ , the Spot Fleet only places the required requests. It does not attempt to replenish Spot Instances if capacity is diminished, nor does it submit requests in alternative Spot pools if capacity is not available. When this value is @maintain@ , the Spot Fleet maintains the target capacity. The Spot Fleet places the required requests to meet capacity and automatically replenishes any interrupted instances. Default: @maintain@ . @instant@ is listed but is not used by Spot Fleet.
-- * 'validFrom' - The start date and time of the request, in UTC format (/YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). By default, Amazon EC2 starts fulfilling the request immediately.
-- * 'validUntil' - The end date and time of the request, in UTC format (/YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). After the end date and time, no new Spot Instance requests are placed or able to fulfill the request. If no value is specified, the Spot Fleet request remains until you cancel it.
mkSpotFleetRequestConfigData ::
  -- | 'iamFleetRole'
  Lude.Text ->
  -- | 'targetCapacity'
  Lude.Int ->
  SpotFleetRequestConfigData
mkSpotFleetRequestConfigData pIAMFleetRole_ pTargetCapacity_ =
  SpotFleetRequestConfigData'
    { clientToken = Lude.Nothing,
      instanceInterruptionBehavior = Lude.Nothing,
      onDemandMaxTotalPrice = Lude.Nothing,
      spotPrice = Lude.Nothing,
      spotMaintenanceStrategies = Lude.Nothing,
      loadBalancersConfig = Lude.Nothing,
      excessCapacityTerminationPolicy = Lude.Nothing,
      onDemandTargetCapacity = Lude.Nothing,
      launchTemplateConfigs = Lude.Nothing,
      tagSpecifications = Lude.Nothing,
      validUntil = Lude.Nothing,
      terminateInstancesWithExpiration = Lude.Nothing,
      onDemandAllocationStrategy = Lude.Nothing,
      instancePoolsToUseCount = Lude.Nothing,
      fulfilledCapacity = Lude.Nothing,
      type' = Lude.Nothing,
      validFrom = Lude.Nothing,
      replaceUnhealthyInstances = Lude.Nothing,
      launchSpecifications = Lude.Nothing,
      onDemandFulfilledCapacity = Lude.Nothing,
      spotMaxTotalPrice = Lude.Nothing,
      allocationStrategy = Lude.Nothing,
      iamFleetRole = pIAMFleetRole_,
      targetCapacity = pTargetCapacity_
    }

-- | A unique, case-sensitive identifier that you provide to ensure the idempotency of your listings. This helps to avoid duplicate listings. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcdClientToken :: Lens.Lens' SpotFleetRequestConfigData (Lude.Maybe Lude.Text)
sfrcdClientToken = Lens.lens (clientToken :: SpotFleetRequestConfigData -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: SpotFleetRequestConfigData)
{-# DEPRECATED sfrcdClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The behavior when a Spot Instance is interrupted. The default is @terminate@ .
--
-- /Note:/ Consider using 'instanceInterruptionBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcdInstanceInterruptionBehavior :: Lens.Lens' SpotFleetRequestConfigData (Lude.Maybe InstanceInterruptionBehavior)
sfrcdInstanceInterruptionBehavior = Lens.lens (instanceInterruptionBehavior :: SpotFleetRequestConfigData -> Lude.Maybe InstanceInterruptionBehavior) (\s a -> s {instanceInterruptionBehavior = a} :: SpotFleetRequestConfigData)
{-# DEPRECATED sfrcdInstanceInterruptionBehavior "Use generic-lens or generic-optics with 'instanceInterruptionBehavior' instead." #-}

-- | The maximum amount per hour for On-Demand Instances that you're willing to pay. You can use the @onDemandMaxTotalPrice@ parameter, the @spotMaxTotalPrice@ parameter, or both parameters to ensure that your fleet cost does not exceed your budget. If you set a maximum price per hour for the On-Demand Instances and Spot Instances in your request, Spot Fleet will launch instances until it reaches the maximum amount you're willing to pay. When the maximum amount you're willing to pay is reached, the fleet stops launching instances even if it hasn’t met the target capacity.
--
-- /Note:/ Consider using 'onDemandMaxTotalPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcdOnDemandMaxTotalPrice :: Lens.Lens' SpotFleetRequestConfigData (Lude.Maybe Lude.Text)
sfrcdOnDemandMaxTotalPrice = Lens.lens (onDemandMaxTotalPrice :: SpotFleetRequestConfigData -> Lude.Maybe Lude.Text) (\s a -> s {onDemandMaxTotalPrice = a} :: SpotFleetRequestConfigData)
{-# DEPRECATED sfrcdOnDemandMaxTotalPrice "Use generic-lens or generic-optics with 'onDemandMaxTotalPrice' instead." #-}

-- | The maximum price per unit hour that you are willing to pay for a Spot Instance. The default is the On-Demand price.
--
-- /Note:/ Consider using 'spotPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcdSpotPrice :: Lens.Lens' SpotFleetRequestConfigData (Lude.Maybe Lude.Text)
sfrcdSpotPrice = Lens.lens (spotPrice :: SpotFleetRequestConfigData -> Lude.Maybe Lude.Text) (\s a -> s {spotPrice = a} :: SpotFleetRequestConfigData)
{-# DEPRECATED sfrcdSpotPrice "Use generic-lens or generic-optics with 'spotPrice' instead." #-}

-- | The strategies for managing your Spot Instances that are at an elevated risk of being interrupted.
--
-- /Note:/ Consider using 'spotMaintenanceStrategies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcdSpotMaintenanceStrategies :: Lens.Lens' SpotFleetRequestConfigData (Lude.Maybe SpotMaintenanceStrategies)
sfrcdSpotMaintenanceStrategies = Lens.lens (spotMaintenanceStrategies :: SpotFleetRequestConfigData -> Lude.Maybe SpotMaintenanceStrategies) (\s a -> s {spotMaintenanceStrategies = a} :: SpotFleetRequestConfigData)
{-# DEPRECATED sfrcdSpotMaintenanceStrategies "Use generic-lens or generic-optics with 'spotMaintenanceStrategies' instead." #-}

-- | One or more Classic Load Balancers and target groups to attach to the Spot Fleet request. Spot Fleet registers the running Spot Instances with the specified Classic Load Balancers and target groups.
--
-- With Network Load Balancers, Spot Fleet cannot register instances that have the following instance types: C1, CC1, CC2, CG1, CG2, CR1, CS1, G1, G2, HI1, HS1, M1, M2, M3, and T1.
--
-- /Note:/ Consider using 'loadBalancersConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcdLoadBalancersConfig :: Lens.Lens' SpotFleetRequestConfigData (Lude.Maybe LoadBalancersConfig)
sfrcdLoadBalancersConfig = Lens.lens (loadBalancersConfig :: SpotFleetRequestConfigData -> Lude.Maybe LoadBalancersConfig) (\s a -> s {loadBalancersConfig = a} :: SpotFleetRequestConfigData)
{-# DEPRECATED sfrcdLoadBalancersConfig "Use generic-lens or generic-optics with 'loadBalancersConfig' instead." #-}

-- | Indicates whether running Spot Instances should be terminated if you decrease the target capacity of the Spot Fleet request below the current size of the Spot Fleet.
--
-- /Note:/ Consider using 'excessCapacityTerminationPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcdExcessCapacityTerminationPolicy :: Lens.Lens' SpotFleetRequestConfigData (Lude.Maybe ExcessCapacityTerminationPolicy)
sfrcdExcessCapacityTerminationPolicy = Lens.lens (excessCapacityTerminationPolicy :: SpotFleetRequestConfigData -> Lude.Maybe ExcessCapacityTerminationPolicy) (\s a -> s {excessCapacityTerminationPolicy = a} :: SpotFleetRequestConfigData)
{-# DEPRECATED sfrcdExcessCapacityTerminationPolicy "Use generic-lens or generic-optics with 'excessCapacityTerminationPolicy' instead." #-}

-- | The number of On-Demand units to request. You can choose to set the target capacity in terms of instances or a performance characteristic that is important to your application workload, such as vCPUs, memory, or I/O. If the request type is @maintain@ , you can specify a target capacity of 0 and add capacity later.
--
-- /Note:/ Consider using 'onDemandTargetCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcdOnDemandTargetCapacity :: Lens.Lens' SpotFleetRequestConfigData (Lude.Maybe Lude.Int)
sfrcdOnDemandTargetCapacity = Lens.lens (onDemandTargetCapacity :: SpotFleetRequestConfigData -> Lude.Maybe Lude.Int) (\s a -> s {onDemandTargetCapacity = a} :: SpotFleetRequestConfigData)
{-# DEPRECATED sfrcdOnDemandTargetCapacity "Use generic-lens or generic-optics with 'onDemandTargetCapacity' instead." #-}

-- | The launch template and overrides. If you specify @LaunchTemplateConfigs@ , you can't specify @LaunchSpecifications@ . If you include On-Demand capacity in your request, you must use @LaunchTemplateConfigs@ .
--
-- /Note:/ Consider using 'launchTemplateConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcdLaunchTemplateConfigs :: Lens.Lens' SpotFleetRequestConfigData (Lude.Maybe [LaunchTemplateConfig])
sfrcdLaunchTemplateConfigs = Lens.lens (launchTemplateConfigs :: SpotFleetRequestConfigData -> Lude.Maybe [LaunchTemplateConfig]) (\s a -> s {launchTemplateConfigs = a} :: SpotFleetRequestConfigData)
{-# DEPRECATED sfrcdLaunchTemplateConfigs "Use generic-lens or generic-optics with 'launchTemplateConfigs' instead." #-}

-- | The key-value pair for tagging the Spot Fleet request on creation. The value for @ResourceType@ must be @spot-fleet-request@ , otherwise the Spot Fleet request fails. To tag instances at launch, specify the tags in the <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-launch-templates.html#create-launch-template launch template> (valid only if you use @LaunchTemplateConfigs@ ) or in the <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_SpotFleetTagSpecification.html @SpotFleetTagSpecification@ > (valid only if you use @LaunchSpecifications@ ). For information about tagging after launch, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html#tag-resources Tagging Your Resources> .
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcdTagSpecifications :: Lens.Lens' SpotFleetRequestConfigData (Lude.Maybe [TagSpecification])
sfrcdTagSpecifications = Lens.lens (tagSpecifications :: SpotFleetRequestConfigData -> Lude.Maybe [TagSpecification]) (\s a -> s {tagSpecifications = a} :: SpotFleetRequestConfigData)
{-# DEPRECATED sfrcdTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | The end date and time of the request, in UTC format (/YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). After the end date and time, no new Spot Instance requests are placed or able to fulfill the request. If no value is specified, the Spot Fleet request remains until you cancel it.
--
-- /Note:/ Consider using 'validUntil' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcdValidUntil :: Lens.Lens' SpotFleetRequestConfigData (Lude.Maybe Lude.DateTime)
sfrcdValidUntil = Lens.lens (validUntil :: SpotFleetRequestConfigData -> Lude.Maybe Lude.DateTime) (\s a -> s {validUntil = a} :: SpotFleetRequestConfigData)
{-# DEPRECATED sfrcdValidUntil "Use generic-lens or generic-optics with 'validUntil' instead." #-}

-- | Indicates whether running Spot Instances are terminated when the Spot Fleet request expires.
--
-- /Note:/ Consider using 'terminateInstancesWithExpiration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcdTerminateInstancesWithExpiration :: Lens.Lens' SpotFleetRequestConfigData (Lude.Maybe Lude.Bool)
sfrcdTerminateInstancesWithExpiration = Lens.lens (terminateInstancesWithExpiration :: SpotFleetRequestConfigData -> Lude.Maybe Lude.Bool) (\s a -> s {terminateInstancesWithExpiration = a} :: SpotFleetRequestConfigData)
{-# DEPRECATED sfrcdTerminateInstancesWithExpiration "Use generic-lens or generic-optics with 'terminateInstancesWithExpiration' instead." #-}

-- | The order of the launch template overrides to use in fulfilling On-Demand capacity. If you specify @lowestPrice@ , Spot Fleet uses price to determine the order, launching the lowest price first. If you specify @prioritized@ , Spot Fleet uses the priority that you assign to each Spot Fleet launch template override, launching the highest priority first. If you do not specify a value, Spot Fleet defaults to @lowestPrice@ .
--
-- /Note:/ Consider using 'onDemandAllocationStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcdOnDemandAllocationStrategy :: Lens.Lens' SpotFleetRequestConfigData (Lude.Maybe OnDemandAllocationStrategy)
sfrcdOnDemandAllocationStrategy = Lens.lens (onDemandAllocationStrategy :: SpotFleetRequestConfigData -> Lude.Maybe OnDemandAllocationStrategy) (\s a -> s {onDemandAllocationStrategy = a} :: SpotFleetRequestConfigData)
{-# DEPRECATED sfrcdOnDemandAllocationStrategy "Use generic-lens or generic-optics with 'onDemandAllocationStrategy' instead." #-}

-- | The number of Spot pools across which to allocate your target Spot capacity. Valid only when Spot __AllocationStrategy__ is set to @lowest-price@ . Spot Fleet selects the cheapest Spot pools and evenly allocates your target Spot capacity across the number of Spot pools that you specify.
--
-- /Note:/ Consider using 'instancePoolsToUseCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcdInstancePoolsToUseCount :: Lens.Lens' SpotFleetRequestConfigData (Lude.Maybe Lude.Int)
sfrcdInstancePoolsToUseCount = Lens.lens (instancePoolsToUseCount :: SpotFleetRequestConfigData -> Lude.Maybe Lude.Int) (\s a -> s {instancePoolsToUseCount = a} :: SpotFleetRequestConfigData)
{-# DEPRECATED sfrcdInstancePoolsToUseCount "Use generic-lens or generic-optics with 'instancePoolsToUseCount' instead." #-}

-- | The number of units fulfilled by this request compared to the set target capacity. You cannot set this value.
--
-- /Note:/ Consider using 'fulfilledCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcdFulfilledCapacity :: Lens.Lens' SpotFleetRequestConfigData (Lude.Maybe Lude.Double)
sfrcdFulfilledCapacity = Lens.lens (fulfilledCapacity :: SpotFleetRequestConfigData -> Lude.Maybe Lude.Double) (\s a -> s {fulfilledCapacity = a} :: SpotFleetRequestConfigData)
{-# DEPRECATED sfrcdFulfilledCapacity "Use generic-lens or generic-optics with 'fulfilledCapacity' instead." #-}

-- | The type of request. Indicates whether the Spot Fleet only requests the target capacity or also attempts to maintain it. When this value is @request@ , the Spot Fleet only places the required requests. It does not attempt to replenish Spot Instances if capacity is diminished, nor does it submit requests in alternative Spot pools if capacity is not available. When this value is @maintain@ , the Spot Fleet maintains the target capacity. The Spot Fleet places the required requests to meet capacity and automatically replenishes any interrupted instances. Default: @maintain@ . @instant@ is listed but is not used by Spot Fleet.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcdType :: Lens.Lens' SpotFleetRequestConfigData (Lude.Maybe FleetType)
sfrcdType = Lens.lens (type' :: SpotFleetRequestConfigData -> Lude.Maybe FleetType) (\s a -> s {type' = a} :: SpotFleetRequestConfigData)
{-# DEPRECATED sfrcdType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The start date and time of the request, in UTC format (/YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). By default, Amazon EC2 starts fulfilling the request immediately.
--
-- /Note:/ Consider using 'validFrom' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcdValidFrom :: Lens.Lens' SpotFleetRequestConfigData (Lude.Maybe Lude.DateTime)
sfrcdValidFrom = Lens.lens (validFrom :: SpotFleetRequestConfigData -> Lude.Maybe Lude.DateTime) (\s a -> s {validFrom = a} :: SpotFleetRequestConfigData)
{-# DEPRECATED sfrcdValidFrom "Use generic-lens or generic-optics with 'validFrom' instead." #-}

-- | Indicates whether Spot Fleet should replace unhealthy instances.
--
-- /Note:/ Consider using 'replaceUnhealthyInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcdReplaceUnhealthyInstances :: Lens.Lens' SpotFleetRequestConfigData (Lude.Maybe Lude.Bool)
sfrcdReplaceUnhealthyInstances = Lens.lens (replaceUnhealthyInstances :: SpotFleetRequestConfigData -> Lude.Maybe Lude.Bool) (\s a -> s {replaceUnhealthyInstances = a} :: SpotFleetRequestConfigData)
{-# DEPRECATED sfrcdReplaceUnhealthyInstances "Use generic-lens or generic-optics with 'replaceUnhealthyInstances' instead." #-}

-- | The launch specifications for the Spot Fleet request. If you specify @LaunchSpecifications@ , you can't specify @LaunchTemplateConfigs@ . If you include On-Demand capacity in your request, you must use @LaunchTemplateConfigs@ .
--
-- /Note:/ Consider using 'launchSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcdLaunchSpecifications :: Lens.Lens' SpotFleetRequestConfigData (Lude.Maybe [SpotFleetLaunchSpecification])
sfrcdLaunchSpecifications = Lens.lens (launchSpecifications :: SpotFleetRequestConfigData -> Lude.Maybe [SpotFleetLaunchSpecification]) (\s a -> s {launchSpecifications = a} :: SpotFleetRequestConfigData)
{-# DEPRECATED sfrcdLaunchSpecifications "Use generic-lens or generic-optics with 'launchSpecifications' instead." #-}

-- | The number of On-Demand units fulfilled by this request compared to the set target On-Demand capacity.
--
-- /Note:/ Consider using 'onDemandFulfilledCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcdOnDemandFulfilledCapacity :: Lens.Lens' SpotFleetRequestConfigData (Lude.Maybe Lude.Double)
sfrcdOnDemandFulfilledCapacity = Lens.lens (onDemandFulfilledCapacity :: SpotFleetRequestConfigData -> Lude.Maybe Lude.Double) (\s a -> s {onDemandFulfilledCapacity = a} :: SpotFleetRequestConfigData)
{-# DEPRECATED sfrcdOnDemandFulfilledCapacity "Use generic-lens or generic-optics with 'onDemandFulfilledCapacity' instead." #-}

-- | The maximum amount per hour for Spot Instances that you're willing to pay. You can use the @spotdMaxTotalPrice@ parameter, the @onDemandMaxTotalPrice@ parameter, or both parameters to ensure that your fleet cost does not exceed your budget. If you set a maximum price per hour for the On-Demand Instances and Spot Instances in your request, Spot Fleet will launch instances until it reaches the maximum amount you're willing to pay. When the maximum amount you're willing to pay is reached, the fleet stops launching instances even if it hasn’t met the target capacity.
--
-- /Note:/ Consider using 'spotMaxTotalPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcdSpotMaxTotalPrice :: Lens.Lens' SpotFleetRequestConfigData (Lude.Maybe Lude.Text)
sfrcdSpotMaxTotalPrice = Lens.lens (spotMaxTotalPrice :: SpotFleetRequestConfigData -> Lude.Maybe Lude.Text) (\s a -> s {spotMaxTotalPrice = a} :: SpotFleetRequestConfigData)
{-# DEPRECATED sfrcdSpotMaxTotalPrice "Use generic-lens or generic-optics with 'spotMaxTotalPrice' instead." #-}

-- | Indicates how to allocate the target Spot Instance capacity across the Spot Instance pools specified by the Spot Fleet request.
--
-- If the allocation strategy is @lowestPrice@ , Spot Fleet launches instances from the Spot Instance pools with the lowest price. This is the default allocation strategy.
-- If the allocation strategy is @diversified@ , Spot Fleet launches instances from all the Spot Instance pools that you specify.
-- If the allocation strategy is @capacityOptimized@ , Spot Fleet launches instances from Spot Instance pools with optimal capacity for the number of instances that are launching.
--
-- /Note:/ Consider using 'allocationStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcdAllocationStrategy :: Lens.Lens' SpotFleetRequestConfigData (Lude.Maybe AllocationStrategy)
sfrcdAllocationStrategy = Lens.lens (allocationStrategy :: SpotFleetRequestConfigData -> Lude.Maybe AllocationStrategy) (\s a -> s {allocationStrategy = a} :: SpotFleetRequestConfigData)
{-# DEPRECATED sfrcdAllocationStrategy "Use generic-lens or generic-optics with 'allocationStrategy' instead." #-}

-- | The Amazon Resource Name (ARN) of an AWS Identity and Access Management (IAM) role that grants the Spot Fleet the permission to request, launch, terminate, and tag instances on your behalf. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-fleet-requests.html#spot-fleet-prerequisites Spot Fleet prerequisites> in the /Amazon EC2 User Guide for Linux Instances/ . Spot Fleet can terminate Spot Instances on your behalf when you cancel its Spot Fleet request using <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CancelSpotFleetRequests CancelSpotFleetRequests> or when the Spot Fleet request expires, if you set @TerminateInstancesWithExpiration@ .
--
-- /Note:/ Consider using 'iamFleetRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcdIAMFleetRole :: Lens.Lens' SpotFleetRequestConfigData Lude.Text
sfrcdIAMFleetRole = Lens.lens (iamFleetRole :: SpotFleetRequestConfigData -> Lude.Text) (\s a -> s {iamFleetRole = a} :: SpotFleetRequestConfigData)
{-# DEPRECATED sfrcdIAMFleetRole "Use generic-lens or generic-optics with 'iamFleetRole' instead." #-}

-- | The number of units to request for the Spot Fleet. You can choose to set the target capacity in terms of instances or a performance characteristic that is important to your application workload, such as vCPUs, memory, or I/O. If the request type is @maintain@ , you can specify a target capacity of 0 and add capacity later.
--
-- /Note:/ Consider using 'targetCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfrcdTargetCapacity :: Lens.Lens' SpotFleetRequestConfigData Lude.Int
sfrcdTargetCapacity = Lens.lens (targetCapacity :: SpotFleetRequestConfigData -> Lude.Int) (\s a -> s {targetCapacity = a} :: SpotFleetRequestConfigData)
{-# DEPRECATED sfrcdTargetCapacity "Use generic-lens or generic-optics with 'targetCapacity' instead." #-}

instance Lude.FromXML SpotFleetRequestConfigData where
  parseXML x =
    SpotFleetRequestConfigData'
      Lude.<$> (x Lude..@? "clientToken")
      Lude.<*> (x Lude..@? "instanceInterruptionBehavior")
      Lude.<*> (x Lude..@? "onDemandMaxTotalPrice")
      Lude.<*> (x Lude..@? "spotPrice")
      Lude.<*> (x Lude..@? "spotMaintenanceStrategies")
      Lude.<*> (x Lude..@? "loadBalancersConfig")
      Lude.<*> (x Lude..@? "excessCapacityTerminationPolicy")
      Lude.<*> (x Lude..@? "onDemandTargetCapacity")
      Lude.<*> ( x Lude..@? "launchTemplateConfigs" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> ( x Lude..@? "TagSpecification" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "validUntil")
      Lude.<*> (x Lude..@? "terminateInstancesWithExpiration")
      Lude.<*> (x Lude..@? "onDemandAllocationStrategy")
      Lude.<*> (x Lude..@? "instancePoolsToUseCount")
      Lude.<*> (x Lude..@? "fulfilledCapacity")
      Lude.<*> (x Lude..@? "type")
      Lude.<*> (x Lude..@? "validFrom")
      Lude.<*> (x Lude..@? "replaceUnhealthyInstances")
      Lude.<*> ( x Lude..@? "launchSpecifications" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "onDemandFulfilledCapacity")
      Lude.<*> (x Lude..@? "spotMaxTotalPrice")
      Lude.<*> (x Lude..@? "allocationStrategy")
      Lude.<*> (x Lude..@ "iamFleetRole")
      Lude.<*> (x Lude..@ "targetCapacity")

instance Lude.ToQuery SpotFleetRequestConfigData where
  toQuery SpotFleetRequestConfigData' {..} =
    Lude.mconcat
      [ "ClientToken" Lude.=: clientToken,
        "InstanceInterruptionBehavior"
          Lude.=: instanceInterruptionBehavior,
        "OnDemandMaxTotalPrice" Lude.=: onDemandMaxTotalPrice,
        "SpotPrice" Lude.=: spotPrice,
        "SpotMaintenanceStrategies" Lude.=: spotMaintenanceStrategies,
        "LoadBalancersConfig" Lude.=: loadBalancersConfig,
        "ExcessCapacityTerminationPolicy"
          Lude.=: excessCapacityTerminationPolicy,
        "OnDemandTargetCapacity" Lude.=: onDemandTargetCapacity,
        Lude.toQuery
          ( Lude.toQueryList "LaunchTemplateConfigs"
              Lude.<$> launchTemplateConfigs
          ),
        Lude.toQuery
          (Lude.toQueryList "TagSpecification" Lude.<$> tagSpecifications),
        "ValidUntil" Lude.=: validUntil,
        "TerminateInstancesWithExpiration"
          Lude.=: terminateInstancesWithExpiration,
        "OnDemandAllocationStrategy" Lude.=: onDemandAllocationStrategy,
        "InstancePoolsToUseCount" Lude.=: instancePoolsToUseCount,
        "FulfilledCapacity" Lude.=: fulfilledCapacity,
        "Type" Lude.=: type',
        "ValidFrom" Lude.=: validFrom,
        "ReplaceUnhealthyInstances" Lude.=: replaceUnhealthyInstances,
        Lude.toQuery
          ( Lude.toQueryList "LaunchSpecifications"
              Lude.<$> launchSpecifications
          ),
        "OnDemandFulfilledCapacity" Lude.=: onDemandFulfilledCapacity,
        "SpotMaxTotalPrice" Lude.=: spotMaxTotalPrice,
        "AllocationStrategy" Lude.=: allocationStrategy,
        "IamFleetRole" Lude.=: iamFleetRole,
        "TargetCapacity" Lude.=: targetCapacity
      ]
