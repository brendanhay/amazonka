{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotFleetRequestConfigData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SpotFleetRequestConfigData where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
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

-- | Describes the configuration of a Spot Fleet request.
--
-- /See:/ 'newSpotFleetRequestConfigData' smart constructor.
data SpotFleetRequestConfigData = SpotFleetRequestConfigData'
  { -- | The launch template and overrides. If you specify
    -- @LaunchTemplateConfigs@, you can\'t specify @LaunchSpecifications@. If
    -- you include On-Demand capacity in your request, you must use
    -- @LaunchTemplateConfigs@.
    launchTemplateConfigs :: Core.Maybe [LaunchTemplateConfig],
    -- | The key-value pair for tagging the Spot Fleet request on creation. The
    -- value for @ResourceType@ must be @spot-fleet-request@, otherwise the
    -- Spot Fleet request fails. To tag instances at launch, specify the tags
    -- in the
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-launch-templates.html#create-launch-template launch template>
    -- (valid only if you use @LaunchTemplateConfigs@) or in the
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_SpotFleetTagSpecification.html SpotFleetTagSpecification>
    -- (valid only if you use @LaunchSpecifications@). For information about
    -- tagging after launch, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html#tag-resources Tagging Your Resources>.
    tagSpecifications :: Core.Maybe [TagSpecification],
    -- | The maximum amount per hour for Spot Instances that you\'re willing to
    -- pay. You can use the @spotdMaxTotalPrice@ parameter, the
    -- @onDemandMaxTotalPrice@ parameter, or both parameters to ensure that
    -- your fleet cost does not exceed your budget. If you set a maximum price
    -- per hour for the On-Demand Instances and Spot Instances in your request,
    -- Spot Fleet will launch instances until it reaches the maximum amount
    -- you\'re willing to pay. When the maximum amount you\'re willing to pay
    -- is reached, the fleet stops launching instances even if it hasn’t met
    -- the target capacity.
    spotMaxTotalPrice :: Core.Maybe Core.Text,
    -- | The number of On-Demand units to request. You can choose to set the
    -- target capacity in terms of instances or a performance characteristic
    -- that is important to your application workload, such as vCPUs, memory,
    -- or I\/O. If the request type is @maintain@, you can specify a target
    -- capacity of 0 and add capacity later.
    onDemandTargetCapacity :: Core.Maybe Core.Int,
    -- | The number of On-Demand units fulfilled by this request compared to the
    -- set target On-Demand capacity.
    onDemandFulfilledCapacity :: Core.Maybe Core.Double,
    -- | The start date and time of the request, in UTC format
    -- (/YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). By default, Amazon EC2 starts
    -- fulfilling the request immediately.
    validFrom :: Core.Maybe Core.ISO8601,
    -- | Indicates whether Spot Fleet should replace unhealthy instances.
    replaceUnhealthyInstances :: Core.Maybe Core.Bool,
    -- | The order of the launch template overrides to use in fulfilling
    -- On-Demand capacity. If you specify @lowestPrice@, Spot Fleet uses price
    -- to determine the order, launching the lowest price first. If you specify
    -- @prioritized@, Spot Fleet uses the priority that you assign to each Spot
    -- Fleet launch template override, launching the highest priority first. If
    -- you do not specify a value, Spot Fleet defaults to @lowestPrice@.
    onDemandAllocationStrategy :: Core.Maybe OnDemandAllocationStrategy,
    -- | The maximum price per unit hour that you are willing to pay for a Spot
    -- Instance. The default is the On-Demand price.
    spotPrice :: Core.Maybe Core.Text,
    -- | The maximum amount per hour for On-Demand Instances that you\'re willing
    -- to pay. You can use the @onDemandMaxTotalPrice@ parameter, the
    -- @spotMaxTotalPrice@ parameter, or both parameters to ensure that your
    -- fleet cost does not exceed your budget. If you set a maximum price per
    -- hour for the On-Demand Instances and Spot Instances in your request,
    -- Spot Fleet will launch instances until it reaches the maximum amount
    -- you\'re willing to pay. When the maximum amount you\'re willing to pay
    -- is reached, the fleet stops launching instances even if it hasn’t met
    -- the target capacity.
    onDemandMaxTotalPrice :: Core.Maybe Core.Text,
    -- | The behavior when a Spot Instance is interrupted. The default is
    -- @terminate@.
    instanceInterruptionBehavior :: Core.Maybe InstanceInterruptionBehavior,
    -- | The end date and time of the request, in UTC format
    -- (/YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). After the end date and time, no new
    -- Spot Instance requests are placed or able to fulfill the request. If no
    -- value is specified, the Spot Fleet request remains until you cancel it.
    validUntil :: Core.Maybe Core.ISO8601,
    -- | One or more Classic Load Balancers and target groups to attach to the
    -- Spot Fleet request. Spot Fleet registers the running Spot Instances with
    -- the specified Classic Load Balancers and target groups.
    --
    -- With Network Load Balancers, Spot Fleet cannot register instances that
    -- have the following instance types: C1, CC1, CC2, CG1, CG2, CR1, CS1, G1,
    -- G2, HI1, HS1, M1, M2, M3, and T1.
    loadBalancersConfig :: Core.Maybe LoadBalancersConfig,
    -- | Indicates whether running Spot Instances should be terminated if you
    -- decrease the target capacity of the Spot Fleet request below the current
    -- size of the Spot Fleet.
    excessCapacityTerminationPolicy :: Core.Maybe ExcessCapacityTerminationPolicy,
    -- | Indicates how to allocate the target Spot Instance capacity across the
    -- Spot Instance pools specified by the Spot Fleet request.
    --
    -- If the allocation strategy is @lowestPrice@, Spot Fleet launches
    -- instances from the Spot Instance pools with the lowest price. This is
    -- the default allocation strategy.
    --
    -- If the allocation strategy is @diversified@, Spot Fleet launches
    -- instances from all the Spot Instance pools that you specify.
    --
    -- If the allocation strategy is @capacityOptimized@, Spot Fleet launches
    -- instances from Spot Instance pools with optimal capacity for the number
    -- of instances that are launching.
    allocationStrategy :: Core.Maybe AllocationStrategy,
    -- | The launch specifications for the Spot Fleet request. If you specify
    -- @LaunchSpecifications@, you can\'t specify @LaunchTemplateConfigs@. If
    -- you include On-Demand capacity in your request, you must use
    -- @LaunchTemplateConfigs@.
    launchSpecifications :: Core.Maybe [SpotFleetLaunchSpecification],
    -- | The type of request. Indicates whether the Spot Fleet only requests the
    -- target capacity or also attempts to maintain it. When this value is
    -- @request@, the Spot Fleet only places the required requests. It does not
    -- attempt to replenish Spot Instances if capacity is diminished, nor does
    -- it submit requests in alternative Spot pools if capacity is not
    -- available. When this value is @maintain@, the Spot Fleet maintains the
    -- target capacity. The Spot Fleet places the required requests to meet
    -- capacity and automatically replenishes any interrupted instances.
    -- Default: @maintain@. @instant@ is listed but is not used by Spot Fleet.
    type' :: Core.Maybe FleetType,
    -- | The strategies for managing your Spot Instances that are at an elevated
    -- risk of being interrupted.
    spotMaintenanceStrategies :: Core.Maybe SpotMaintenanceStrategies,
    -- | The number of Spot pools across which to allocate your target Spot
    -- capacity. Valid only when Spot __AllocationStrategy__ is set to
    -- @lowest-price@. Spot Fleet selects the cheapest Spot pools and evenly
    -- allocates your target Spot capacity across the number of Spot pools that
    -- you specify.
    instancePoolsToUseCount :: Core.Maybe Core.Int,
    -- | The number of units fulfilled by this request compared to the set target
    -- capacity. You cannot set this value.
    fulfilledCapacity :: Core.Maybe Core.Double,
    -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of your listings. This helps to avoid duplicate listings.
    -- For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
    clientToken :: Core.Maybe Core.Text,
    -- | Indicates whether running Spot Instances are terminated when the Spot
    -- Fleet request expires.
    terminateInstancesWithExpiration :: Core.Maybe Core.Bool,
    -- | The Amazon Resource Name (ARN) of an AWS Identity and Access Management
    -- (IAM) role that grants the Spot Fleet the permission to request, launch,
    -- terminate, and tag instances on your behalf. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-fleet-requests.html#spot-fleet-prerequisites Spot Fleet prerequisites>
    -- in the /Amazon EC2 User Guide for Linux Instances/. Spot Fleet can
    -- terminate Spot Instances on your behalf when you cancel its Spot Fleet
    -- request using
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CancelSpotFleetRequests CancelSpotFleetRequests>
    -- or when the Spot Fleet request expires, if you set
    -- @TerminateInstancesWithExpiration@.
    iamFleetRole :: Core.Text,
    -- | The number of units to request for the Spot Fleet. You can choose to set
    -- the target capacity in terms of instances or a performance
    -- characteristic that is important to your application workload, such as
    -- vCPUs, memory, or I\/O. If the request type is @maintain@, you can
    -- specify a target capacity of 0 and add capacity later.
    targetCapacity :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SpotFleetRequestConfigData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launchTemplateConfigs', 'spotFleetRequestConfigData_launchTemplateConfigs' - The launch template and overrides. If you specify
-- @LaunchTemplateConfigs@, you can\'t specify @LaunchSpecifications@. If
-- you include On-Demand capacity in your request, you must use
-- @LaunchTemplateConfigs@.
--
-- 'tagSpecifications', 'spotFleetRequestConfigData_tagSpecifications' - The key-value pair for tagging the Spot Fleet request on creation. The
-- value for @ResourceType@ must be @spot-fleet-request@, otherwise the
-- Spot Fleet request fails. To tag instances at launch, specify the tags
-- in the
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-launch-templates.html#create-launch-template launch template>
-- (valid only if you use @LaunchTemplateConfigs@) or in the
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_SpotFleetTagSpecification.html SpotFleetTagSpecification>
-- (valid only if you use @LaunchSpecifications@). For information about
-- tagging after launch, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html#tag-resources Tagging Your Resources>.
--
-- 'spotMaxTotalPrice', 'spotFleetRequestConfigData_spotMaxTotalPrice' - The maximum amount per hour for Spot Instances that you\'re willing to
-- pay. You can use the @spotdMaxTotalPrice@ parameter, the
-- @onDemandMaxTotalPrice@ parameter, or both parameters to ensure that
-- your fleet cost does not exceed your budget. If you set a maximum price
-- per hour for the On-Demand Instances and Spot Instances in your request,
-- Spot Fleet will launch instances until it reaches the maximum amount
-- you\'re willing to pay. When the maximum amount you\'re willing to pay
-- is reached, the fleet stops launching instances even if it hasn’t met
-- the target capacity.
--
-- 'onDemandTargetCapacity', 'spotFleetRequestConfigData_onDemandTargetCapacity' - The number of On-Demand units to request. You can choose to set the
-- target capacity in terms of instances or a performance characteristic
-- that is important to your application workload, such as vCPUs, memory,
-- or I\/O. If the request type is @maintain@, you can specify a target
-- capacity of 0 and add capacity later.
--
-- 'onDemandFulfilledCapacity', 'spotFleetRequestConfigData_onDemandFulfilledCapacity' - The number of On-Demand units fulfilled by this request compared to the
-- set target On-Demand capacity.
--
-- 'validFrom', 'spotFleetRequestConfigData_validFrom' - The start date and time of the request, in UTC format
-- (/YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). By default, Amazon EC2 starts
-- fulfilling the request immediately.
--
-- 'replaceUnhealthyInstances', 'spotFleetRequestConfigData_replaceUnhealthyInstances' - Indicates whether Spot Fleet should replace unhealthy instances.
--
-- 'onDemandAllocationStrategy', 'spotFleetRequestConfigData_onDemandAllocationStrategy' - The order of the launch template overrides to use in fulfilling
-- On-Demand capacity. If you specify @lowestPrice@, Spot Fleet uses price
-- to determine the order, launching the lowest price first. If you specify
-- @prioritized@, Spot Fleet uses the priority that you assign to each Spot
-- Fleet launch template override, launching the highest priority first. If
-- you do not specify a value, Spot Fleet defaults to @lowestPrice@.
--
-- 'spotPrice', 'spotFleetRequestConfigData_spotPrice' - The maximum price per unit hour that you are willing to pay for a Spot
-- Instance. The default is the On-Demand price.
--
-- 'onDemandMaxTotalPrice', 'spotFleetRequestConfigData_onDemandMaxTotalPrice' - The maximum amount per hour for On-Demand Instances that you\'re willing
-- to pay. You can use the @onDemandMaxTotalPrice@ parameter, the
-- @spotMaxTotalPrice@ parameter, or both parameters to ensure that your
-- fleet cost does not exceed your budget. If you set a maximum price per
-- hour for the On-Demand Instances and Spot Instances in your request,
-- Spot Fleet will launch instances until it reaches the maximum amount
-- you\'re willing to pay. When the maximum amount you\'re willing to pay
-- is reached, the fleet stops launching instances even if it hasn’t met
-- the target capacity.
--
-- 'instanceInterruptionBehavior', 'spotFleetRequestConfigData_instanceInterruptionBehavior' - The behavior when a Spot Instance is interrupted. The default is
-- @terminate@.
--
-- 'validUntil', 'spotFleetRequestConfigData_validUntil' - The end date and time of the request, in UTC format
-- (/YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). After the end date and time, no new
-- Spot Instance requests are placed or able to fulfill the request. If no
-- value is specified, the Spot Fleet request remains until you cancel it.
--
-- 'loadBalancersConfig', 'spotFleetRequestConfigData_loadBalancersConfig' - One or more Classic Load Balancers and target groups to attach to the
-- Spot Fleet request. Spot Fleet registers the running Spot Instances with
-- the specified Classic Load Balancers and target groups.
--
-- With Network Load Balancers, Spot Fleet cannot register instances that
-- have the following instance types: C1, CC1, CC2, CG1, CG2, CR1, CS1, G1,
-- G2, HI1, HS1, M1, M2, M3, and T1.
--
-- 'excessCapacityTerminationPolicy', 'spotFleetRequestConfigData_excessCapacityTerminationPolicy' - Indicates whether running Spot Instances should be terminated if you
-- decrease the target capacity of the Spot Fleet request below the current
-- size of the Spot Fleet.
--
-- 'allocationStrategy', 'spotFleetRequestConfigData_allocationStrategy' - Indicates how to allocate the target Spot Instance capacity across the
-- Spot Instance pools specified by the Spot Fleet request.
--
-- If the allocation strategy is @lowestPrice@, Spot Fleet launches
-- instances from the Spot Instance pools with the lowest price. This is
-- the default allocation strategy.
--
-- If the allocation strategy is @diversified@, Spot Fleet launches
-- instances from all the Spot Instance pools that you specify.
--
-- If the allocation strategy is @capacityOptimized@, Spot Fleet launches
-- instances from Spot Instance pools with optimal capacity for the number
-- of instances that are launching.
--
-- 'launchSpecifications', 'spotFleetRequestConfigData_launchSpecifications' - The launch specifications for the Spot Fleet request. If you specify
-- @LaunchSpecifications@, you can\'t specify @LaunchTemplateConfigs@. If
-- you include On-Demand capacity in your request, you must use
-- @LaunchTemplateConfigs@.
--
-- 'type'', 'spotFleetRequestConfigData_type' - The type of request. Indicates whether the Spot Fleet only requests the
-- target capacity or also attempts to maintain it. When this value is
-- @request@, the Spot Fleet only places the required requests. It does not
-- attempt to replenish Spot Instances if capacity is diminished, nor does
-- it submit requests in alternative Spot pools if capacity is not
-- available. When this value is @maintain@, the Spot Fleet maintains the
-- target capacity. The Spot Fleet places the required requests to meet
-- capacity and automatically replenishes any interrupted instances.
-- Default: @maintain@. @instant@ is listed but is not used by Spot Fleet.
--
-- 'spotMaintenanceStrategies', 'spotFleetRequestConfigData_spotMaintenanceStrategies' - The strategies for managing your Spot Instances that are at an elevated
-- risk of being interrupted.
--
-- 'instancePoolsToUseCount', 'spotFleetRequestConfigData_instancePoolsToUseCount' - The number of Spot pools across which to allocate your target Spot
-- capacity. Valid only when Spot __AllocationStrategy__ is set to
-- @lowest-price@. Spot Fleet selects the cheapest Spot pools and evenly
-- allocates your target Spot capacity across the number of Spot pools that
-- you specify.
--
-- 'fulfilledCapacity', 'spotFleetRequestConfigData_fulfilledCapacity' - The number of units fulfilled by this request compared to the set target
-- capacity. You cannot set this value.
--
-- 'clientToken', 'spotFleetRequestConfigData_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of your listings. This helps to avoid duplicate listings.
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
--
-- 'terminateInstancesWithExpiration', 'spotFleetRequestConfigData_terminateInstancesWithExpiration' - Indicates whether running Spot Instances are terminated when the Spot
-- Fleet request expires.
--
-- 'iamFleetRole', 'spotFleetRequestConfigData_iamFleetRole' - The Amazon Resource Name (ARN) of an AWS Identity and Access Management
-- (IAM) role that grants the Spot Fleet the permission to request, launch,
-- terminate, and tag instances on your behalf. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-fleet-requests.html#spot-fleet-prerequisites Spot Fleet prerequisites>
-- in the /Amazon EC2 User Guide for Linux Instances/. Spot Fleet can
-- terminate Spot Instances on your behalf when you cancel its Spot Fleet
-- request using
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CancelSpotFleetRequests CancelSpotFleetRequests>
-- or when the Spot Fleet request expires, if you set
-- @TerminateInstancesWithExpiration@.
--
-- 'targetCapacity', 'spotFleetRequestConfigData_targetCapacity' - The number of units to request for the Spot Fleet. You can choose to set
-- the target capacity in terms of instances or a performance
-- characteristic that is important to your application workload, such as
-- vCPUs, memory, or I\/O. If the request type is @maintain@, you can
-- specify a target capacity of 0 and add capacity later.
newSpotFleetRequestConfigData ::
  -- | 'iamFleetRole'
  Core.Text ->
  -- | 'targetCapacity'
  Core.Int ->
  SpotFleetRequestConfigData
newSpotFleetRequestConfigData
  pIamFleetRole_
  pTargetCapacity_ =
    SpotFleetRequestConfigData'
      { launchTemplateConfigs =
          Core.Nothing,
        tagSpecifications = Core.Nothing,
        spotMaxTotalPrice = Core.Nothing,
        onDemandTargetCapacity = Core.Nothing,
        onDemandFulfilledCapacity = Core.Nothing,
        validFrom = Core.Nothing,
        replaceUnhealthyInstances = Core.Nothing,
        onDemandAllocationStrategy = Core.Nothing,
        spotPrice = Core.Nothing,
        onDemandMaxTotalPrice = Core.Nothing,
        instanceInterruptionBehavior = Core.Nothing,
        validUntil = Core.Nothing,
        loadBalancersConfig = Core.Nothing,
        excessCapacityTerminationPolicy = Core.Nothing,
        allocationStrategy = Core.Nothing,
        launchSpecifications = Core.Nothing,
        type' = Core.Nothing,
        spotMaintenanceStrategies = Core.Nothing,
        instancePoolsToUseCount = Core.Nothing,
        fulfilledCapacity = Core.Nothing,
        clientToken = Core.Nothing,
        terminateInstancesWithExpiration = Core.Nothing,
        iamFleetRole = pIamFleetRole_,
        targetCapacity = pTargetCapacity_
      }

-- | The launch template and overrides. If you specify
-- @LaunchTemplateConfigs@, you can\'t specify @LaunchSpecifications@. If
-- you include On-Demand capacity in your request, you must use
-- @LaunchTemplateConfigs@.
spotFleetRequestConfigData_launchTemplateConfigs :: Lens.Lens' SpotFleetRequestConfigData (Core.Maybe [LaunchTemplateConfig])
spotFleetRequestConfigData_launchTemplateConfigs = Lens.lens (\SpotFleetRequestConfigData' {launchTemplateConfigs} -> launchTemplateConfigs) (\s@SpotFleetRequestConfigData' {} a -> s {launchTemplateConfigs = a} :: SpotFleetRequestConfigData) Core.. Lens.mapping Lens._Coerce

-- | The key-value pair for tagging the Spot Fleet request on creation. The
-- value for @ResourceType@ must be @spot-fleet-request@, otherwise the
-- Spot Fleet request fails. To tag instances at launch, specify the tags
-- in the
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-launch-templates.html#create-launch-template launch template>
-- (valid only if you use @LaunchTemplateConfigs@) or in the
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_SpotFleetTagSpecification.html SpotFleetTagSpecification>
-- (valid only if you use @LaunchSpecifications@). For information about
-- tagging after launch, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html#tag-resources Tagging Your Resources>.
spotFleetRequestConfigData_tagSpecifications :: Lens.Lens' SpotFleetRequestConfigData (Core.Maybe [TagSpecification])
spotFleetRequestConfigData_tagSpecifications = Lens.lens (\SpotFleetRequestConfigData' {tagSpecifications} -> tagSpecifications) (\s@SpotFleetRequestConfigData' {} a -> s {tagSpecifications = a} :: SpotFleetRequestConfigData) Core.. Lens.mapping Lens._Coerce

-- | The maximum amount per hour for Spot Instances that you\'re willing to
-- pay. You can use the @spotdMaxTotalPrice@ parameter, the
-- @onDemandMaxTotalPrice@ parameter, or both parameters to ensure that
-- your fleet cost does not exceed your budget. If you set a maximum price
-- per hour for the On-Demand Instances and Spot Instances in your request,
-- Spot Fleet will launch instances until it reaches the maximum amount
-- you\'re willing to pay. When the maximum amount you\'re willing to pay
-- is reached, the fleet stops launching instances even if it hasn’t met
-- the target capacity.
spotFleetRequestConfigData_spotMaxTotalPrice :: Lens.Lens' SpotFleetRequestConfigData (Core.Maybe Core.Text)
spotFleetRequestConfigData_spotMaxTotalPrice = Lens.lens (\SpotFleetRequestConfigData' {spotMaxTotalPrice} -> spotMaxTotalPrice) (\s@SpotFleetRequestConfigData' {} a -> s {spotMaxTotalPrice = a} :: SpotFleetRequestConfigData)

-- | The number of On-Demand units to request. You can choose to set the
-- target capacity in terms of instances or a performance characteristic
-- that is important to your application workload, such as vCPUs, memory,
-- or I\/O. If the request type is @maintain@, you can specify a target
-- capacity of 0 and add capacity later.
spotFleetRequestConfigData_onDemandTargetCapacity :: Lens.Lens' SpotFleetRequestConfigData (Core.Maybe Core.Int)
spotFleetRequestConfigData_onDemandTargetCapacity = Lens.lens (\SpotFleetRequestConfigData' {onDemandTargetCapacity} -> onDemandTargetCapacity) (\s@SpotFleetRequestConfigData' {} a -> s {onDemandTargetCapacity = a} :: SpotFleetRequestConfigData)

-- | The number of On-Demand units fulfilled by this request compared to the
-- set target On-Demand capacity.
spotFleetRequestConfigData_onDemandFulfilledCapacity :: Lens.Lens' SpotFleetRequestConfigData (Core.Maybe Core.Double)
spotFleetRequestConfigData_onDemandFulfilledCapacity = Lens.lens (\SpotFleetRequestConfigData' {onDemandFulfilledCapacity} -> onDemandFulfilledCapacity) (\s@SpotFleetRequestConfigData' {} a -> s {onDemandFulfilledCapacity = a} :: SpotFleetRequestConfigData)

-- | The start date and time of the request, in UTC format
-- (/YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). By default, Amazon EC2 starts
-- fulfilling the request immediately.
spotFleetRequestConfigData_validFrom :: Lens.Lens' SpotFleetRequestConfigData (Core.Maybe Core.UTCTime)
spotFleetRequestConfigData_validFrom = Lens.lens (\SpotFleetRequestConfigData' {validFrom} -> validFrom) (\s@SpotFleetRequestConfigData' {} a -> s {validFrom = a} :: SpotFleetRequestConfigData) Core.. Lens.mapping Core._Time

-- | Indicates whether Spot Fleet should replace unhealthy instances.
spotFleetRequestConfigData_replaceUnhealthyInstances :: Lens.Lens' SpotFleetRequestConfigData (Core.Maybe Core.Bool)
spotFleetRequestConfigData_replaceUnhealthyInstances = Lens.lens (\SpotFleetRequestConfigData' {replaceUnhealthyInstances} -> replaceUnhealthyInstances) (\s@SpotFleetRequestConfigData' {} a -> s {replaceUnhealthyInstances = a} :: SpotFleetRequestConfigData)

-- | The order of the launch template overrides to use in fulfilling
-- On-Demand capacity. If you specify @lowestPrice@, Spot Fleet uses price
-- to determine the order, launching the lowest price first. If you specify
-- @prioritized@, Spot Fleet uses the priority that you assign to each Spot
-- Fleet launch template override, launching the highest priority first. If
-- you do not specify a value, Spot Fleet defaults to @lowestPrice@.
spotFleetRequestConfigData_onDemandAllocationStrategy :: Lens.Lens' SpotFleetRequestConfigData (Core.Maybe OnDemandAllocationStrategy)
spotFleetRequestConfigData_onDemandAllocationStrategy = Lens.lens (\SpotFleetRequestConfigData' {onDemandAllocationStrategy} -> onDemandAllocationStrategy) (\s@SpotFleetRequestConfigData' {} a -> s {onDemandAllocationStrategy = a} :: SpotFleetRequestConfigData)

-- | The maximum price per unit hour that you are willing to pay for a Spot
-- Instance. The default is the On-Demand price.
spotFleetRequestConfigData_spotPrice :: Lens.Lens' SpotFleetRequestConfigData (Core.Maybe Core.Text)
spotFleetRequestConfigData_spotPrice = Lens.lens (\SpotFleetRequestConfigData' {spotPrice} -> spotPrice) (\s@SpotFleetRequestConfigData' {} a -> s {spotPrice = a} :: SpotFleetRequestConfigData)

-- | The maximum amount per hour for On-Demand Instances that you\'re willing
-- to pay. You can use the @onDemandMaxTotalPrice@ parameter, the
-- @spotMaxTotalPrice@ parameter, or both parameters to ensure that your
-- fleet cost does not exceed your budget. If you set a maximum price per
-- hour for the On-Demand Instances and Spot Instances in your request,
-- Spot Fleet will launch instances until it reaches the maximum amount
-- you\'re willing to pay. When the maximum amount you\'re willing to pay
-- is reached, the fleet stops launching instances even if it hasn’t met
-- the target capacity.
spotFleetRequestConfigData_onDemandMaxTotalPrice :: Lens.Lens' SpotFleetRequestConfigData (Core.Maybe Core.Text)
spotFleetRequestConfigData_onDemandMaxTotalPrice = Lens.lens (\SpotFleetRequestConfigData' {onDemandMaxTotalPrice} -> onDemandMaxTotalPrice) (\s@SpotFleetRequestConfigData' {} a -> s {onDemandMaxTotalPrice = a} :: SpotFleetRequestConfigData)

-- | The behavior when a Spot Instance is interrupted. The default is
-- @terminate@.
spotFleetRequestConfigData_instanceInterruptionBehavior :: Lens.Lens' SpotFleetRequestConfigData (Core.Maybe InstanceInterruptionBehavior)
spotFleetRequestConfigData_instanceInterruptionBehavior = Lens.lens (\SpotFleetRequestConfigData' {instanceInterruptionBehavior} -> instanceInterruptionBehavior) (\s@SpotFleetRequestConfigData' {} a -> s {instanceInterruptionBehavior = a} :: SpotFleetRequestConfigData)

-- | The end date and time of the request, in UTC format
-- (/YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). After the end date and time, no new
-- Spot Instance requests are placed or able to fulfill the request. If no
-- value is specified, the Spot Fleet request remains until you cancel it.
spotFleetRequestConfigData_validUntil :: Lens.Lens' SpotFleetRequestConfigData (Core.Maybe Core.UTCTime)
spotFleetRequestConfigData_validUntil = Lens.lens (\SpotFleetRequestConfigData' {validUntil} -> validUntil) (\s@SpotFleetRequestConfigData' {} a -> s {validUntil = a} :: SpotFleetRequestConfigData) Core.. Lens.mapping Core._Time

-- | One or more Classic Load Balancers and target groups to attach to the
-- Spot Fleet request. Spot Fleet registers the running Spot Instances with
-- the specified Classic Load Balancers and target groups.
--
-- With Network Load Balancers, Spot Fleet cannot register instances that
-- have the following instance types: C1, CC1, CC2, CG1, CG2, CR1, CS1, G1,
-- G2, HI1, HS1, M1, M2, M3, and T1.
spotFleetRequestConfigData_loadBalancersConfig :: Lens.Lens' SpotFleetRequestConfigData (Core.Maybe LoadBalancersConfig)
spotFleetRequestConfigData_loadBalancersConfig = Lens.lens (\SpotFleetRequestConfigData' {loadBalancersConfig} -> loadBalancersConfig) (\s@SpotFleetRequestConfigData' {} a -> s {loadBalancersConfig = a} :: SpotFleetRequestConfigData)

-- | Indicates whether running Spot Instances should be terminated if you
-- decrease the target capacity of the Spot Fleet request below the current
-- size of the Spot Fleet.
spotFleetRequestConfigData_excessCapacityTerminationPolicy :: Lens.Lens' SpotFleetRequestConfigData (Core.Maybe ExcessCapacityTerminationPolicy)
spotFleetRequestConfigData_excessCapacityTerminationPolicy = Lens.lens (\SpotFleetRequestConfigData' {excessCapacityTerminationPolicy} -> excessCapacityTerminationPolicy) (\s@SpotFleetRequestConfigData' {} a -> s {excessCapacityTerminationPolicy = a} :: SpotFleetRequestConfigData)

-- | Indicates how to allocate the target Spot Instance capacity across the
-- Spot Instance pools specified by the Spot Fleet request.
--
-- If the allocation strategy is @lowestPrice@, Spot Fleet launches
-- instances from the Spot Instance pools with the lowest price. This is
-- the default allocation strategy.
--
-- If the allocation strategy is @diversified@, Spot Fleet launches
-- instances from all the Spot Instance pools that you specify.
--
-- If the allocation strategy is @capacityOptimized@, Spot Fleet launches
-- instances from Spot Instance pools with optimal capacity for the number
-- of instances that are launching.
spotFleetRequestConfigData_allocationStrategy :: Lens.Lens' SpotFleetRequestConfigData (Core.Maybe AllocationStrategy)
spotFleetRequestConfigData_allocationStrategy = Lens.lens (\SpotFleetRequestConfigData' {allocationStrategy} -> allocationStrategy) (\s@SpotFleetRequestConfigData' {} a -> s {allocationStrategy = a} :: SpotFleetRequestConfigData)

-- | The launch specifications for the Spot Fleet request. If you specify
-- @LaunchSpecifications@, you can\'t specify @LaunchTemplateConfigs@. If
-- you include On-Demand capacity in your request, you must use
-- @LaunchTemplateConfigs@.
spotFleetRequestConfigData_launchSpecifications :: Lens.Lens' SpotFleetRequestConfigData (Core.Maybe [SpotFleetLaunchSpecification])
spotFleetRequestConfigData_launchSpecifications = Lens.lens (\SpotFleetRequestConfigData' {launchSpecifications} -> launchSpecifications) (\s@SpotFleetRequestConfigData' {} a -> s {launchSpecifications = a} :: SpotFleetRequestConfigData) Core.. Lens.mapping Lens._Coerce

-- | The type of request. Indicates whether the Spot Fleet only requests the
-- target capacity or also attempts to maintain it. When this value is
-- @request@, the Spot Fleet only places the required requests. It does not
-- attempt to replenish Spot Instances if capacity is diminished, nor does
-- it submit requests in alternative Spot pools if capacity is not
-- available. When this value is @maintain@, the Spot Fleet maintains the
-- target capacity. The Spot Fleet places the required requests to meet
-- capacity and automatically replenishes any interrupted instances.
-- Default: @maintain@. @instant@ is listed but is not used by Spot Fleet.
spotFleetRequestConfigData_type :: Lens.Lens' SpotFleetRequestConfigData (Core.Maybe FleetType)
spotFleetRequestConfigData_type = Lens.lens (\SpotFleetRequestConfigData' {type'} -> type') (\s@SpotFleetRequestConfigData' {} a -> s {type' = a} :: SpotFleetRequestConfigData)

-- | The strategies for managing your Spot Instances that are at an elevated
-- risk of being interrupted.
spotFleetRequestConfigData_spotMaintenanceStrategies :: Lens.Lens' SpotFleetRequestConfigData (Core.Maybe SpotMaintenanceStrategies)
spotFleetRequestConfigData_spotMaintenanceStrategies = Lens.lens (\SpotFleetRequestConfigData' {spotMaintenanceStrategies} -> spotMaintenanceStrategies) (\s@SpotFleetRequestConfigData' {} a -> s {spotMaintenanceStrategies = a} :: SpotFleetRequestConfigData)

-- | The number of Spot pools across which to allocate your target Spot
-- capacity. Valid only when Spot __AllocationStrategy__ is set to
-- @lowest-price@. Spot Fleet selects the cheapest Spot pools and evenly
-- allocates your target Spot capacity across the number of Spot pools that
-- you specify.
spotFleetRequestConfigData_instancePoolsToUseCount :: Lens.Lens' SpotFleetRequestConfigData (Core.Maybe Core.Int)
spotFleetRequestConfigData_instancePoolsToUseCount = Lens.lens (\SpotFleetRequestConfigData' {instancePoolsToUseCount} -> instancePoolsToUseCount) (\s@SpotFleetRequestConfigData' {} a -> s {instancePoolsToUseCount = a} :: SpotFleetRequestConfigData)

-- | The number of units fulfilled by this request compared to the set target
-- capacity. You cannot set this value.
spotFleetRequestConfigData_fulfilledCapacity :: Lens.Lens' SpotFleetRequestConfigData (Core.Maybe Core.Double)
spotFleetRequestConfigData_fulfilledCapacity = Lens.lens (\SpotFleetRequestConfigData' {fulfilledCapacity} -> fulfilledCapacity) (\s@SpotFleetRequestConfigData' {} a -> s {fulfilledCapacity = a} :: SpotFleetRequestConfigData)

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of your listings. This helps to avoid duplicate listings.
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
spotFleetRequestConfigData_clientToken :: Lens.Lens' SpotFleetRequestConfigData (Core.Maybe Core.Text)
spotFleetRequestConfigData_clientToken = Lens.lens (\SpotFleetRequestConfigData' {clientToken} -> clientToken) (\s@SpotFleetRequestConfigData' {} a -> s {clientToken = a} :: SpotFleetRequestConfigData)

-- | Indicates whether running Spot Instances are terminated when the Spot
-- Fleet request expires.
spotFleetRequestConfigData_terminateInstancesWithExpiration :: Lens.Lens' SpotFleetRequestConfigData (Core.Maybe Core.Bool)
spotFleetRequestConfigData_terminateInstancesWithExpiration = Lens.lens (\SpotFleetRequestConfigData' {terminateInstancesWithExpiration} -> terminateInstancesWithExpiration) (\s@SpotFleetRequestConfigData' {} a -> s {terminateInstancesWithExpiration = a} :: SpotFleetRequestConfigData)

-- | The Amazon Resource Name (ARN) of an AWS Identity and Access Management
-- (IAM) role that grants the Spot Fleet the permission to request, launch,
-- terminate, and tag instances on your behalf. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-fleet-requests.html#spot-fleet-prerequisites Spot Fleet prerequisites>
-- in the /Amazon EC2 User Guide for Linux Instances/. Spot Fleet can
-- terminate Spot Instances on your behalf when you cancel its Spot Fleet
-- request using
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CancelSpotFleetRequests CancelSpotFleetRequests>
-- or when the Spot Fleet request expires, if you set
-- @TerminateInstancesWithExpiration@.
spotFleetRequestConfigData_iamFleetRole :: Lens.Lens' SpotFleetRequestConfigData Core.Text
spotFleetRequestConfigData_iamFleetRole = Lens.lens (\SpotFleetRequestConfigData' {iamFleetRole} -> iamFleetRole) (\s@SpotFleetRequestConfigData' {} a -> s {iamFleetRole = a} :: SpotFleetRequestConfigData)

-- | The number of units to request for the Spot Fleet. You can choose to set
-- the target capacity in terms of instances or a performance
-- characteristic that is important to your application workload, such as
-- vCPUs, memory, or I\/O. If the request type is @maintain@, you can
-- specify a target capacity of 0 and add capacity later.
spotFleetRequestConfigData_targetCapacity :: Lens.Lens' SpotFleetRequestConfigData Core.Int
spotFleetRequestConfigData_targetCapacity = Lens.lens (\SpotFleetRequestConfigData' {targetCapacity} -> targetCapacity) (\s@SpotFleetRequestConfigData' {} a -> s {targetCapacity = a} :: SpotFleetRequestConfigData)

instance Core.FromXML SpotFleetRequestConfigData where
  parseXML x =
    SpotFleetRequestConfigData'
      Core.<$> ( x Core..@? "launchTemplateConfigs"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> ( x Core..@? "TagSpecification" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "spotMaxTotalPrice")
      Core.<*> (x Core..@? "onDemandTargetCapacity")
      Core.<*> (x Core..@? "onDemandFulfilledCapacity")
      Core.<*> (x Core..@? "validFrom")
      Core.<*> (x Core..@? "replaceUnhealthyInstances")
      Core.<*> (x Core..@? "onDemandAllocationStrategy")
      Core.<*> (x Core..@? "spotPrice")
      Core.<*> (x Core..@? "onDemandMaxTotalPrice")
      Core.<*> (x Core..@? "instanceInterruptionBehavior")
      Core.<*> (x Core..@? "validUntil")
      Core.<*> (x Core..@? "loadBalancersConfig")
      Core.<*> (x Core..@? "excessCapacityTerminationPolicy")
      Core.<*> (x Core..@? "allocationStrategy")
      Core.<*> ( x Core..@? "launchSpecifications"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "type")
      Core.<*> (x Core..@? "spotMaintenanceStrategies")
      Core.<*> (x Core..@? "instancePoolsToUseCount")
      Core.<*> (x Core..@? "fulfilledCapacity")
      Core.<*> (x Core..@? "clientToken")
      Core.<*> (x Core..@? "terminateInstancesWithExpiration")
      Core.<*> (x Core..@ "iamFleetRole")
      Core.<*> (x Core..@ "targetCapacity")

instance Core.Hashable SpotFleetRequestConfigData

instance Core.NFData SpotFleetRequestConfigData

instance Core.ToQuery SpotFleetRequestConfigData where
  toQuery SpotFleetRequestConfigData' {..} =
    Core.mconcat
      [ Core.toQuery
          ( Core.toQueryList "LaunchTemplateConfigs"
              Core.<$> launchTemplateConfigs
          ),
        Core.toQuery
          ( Core.toQueryList "TagSpecification"
              Core.<$> tagSpecifications
          ),
        "SpotMaxTotalPrice" Core.=: spotMaxTotalPrice,
        "OnDemandTargetCapacity"
          Core.=: onDemandTargetCapacity,
        "OnDemandFulfilledCapacity"
          Core.=: onDemandFulfilledCapacity,
        "ValidFrom" Core.=: validFrom,
        "ReplaceUnhealthyInstances"
          Core.=: replaceUnhealthyInstances,
        "OnDemandAllocationStrategy"
          Core.=: onDemandAllocationStrategy,
        "SpotPrice" Core.=: spotPrice,
        "OnDemandMaxTotalPrice"
          Core.=: onDemandMaxTotalPrice,
        "InstanceInterruptionBehavior"
          Core.=: instanceInterruptionBehavior,
        "ValidUntil" Core.=: validUntil,
        "LoadBalancersConfig" Core.=: loadBalancersConfig,
        "ExcessCapacityTerminationPolicy"
          Core.=: excessCapacityTerminationPolicy,
        "AllocationStrategy" Core.=: allocationStrategy,
        Core.toQuery
          ( Core.toQueryList "LaunchSpecifications"
              Core.<$> launchSpecifications
          ),
        "Type" Core.=: type',
        "SpotMaintenanceStrategies"
          Core.=: spotMaintenanceStrategies,
        "InstancePoolsToUseCount"
          Core.=: instancePoolsToUseCount,
        "FulfilledCapacity" Core.=: fulfilledCapacity,
        "ClientToken" Core.=: clientToken,
        "TerminateInstancesWithExpiration"
          Core.=: terminateInstancesWithExpiration,
        "IamFleetRole" Core.=: iamFleetRole,
        "TargetCapacity" Core.=: targetCapacity
      ]
