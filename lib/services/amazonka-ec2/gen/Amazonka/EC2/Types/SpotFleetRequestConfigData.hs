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
-- Module      : Amazonka.EC2.Types.SpotFleetRequestConfigData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.SpotFleetRequestConfigData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.AllocationStrategy
import Amazonka.EC2.Types.ExcessCapacityTerminationPolicy
import Amazonka.EC2.Types.FleetType
import Amazonka.EC2.Types.InstanceInterruptionBehavior
import Amazonka.EC2.Types.LaunchTemplateConfig
import Amazonka.EC2.Types.LoadBalancersConfig
import Amazonka.EC2.Types.OnDemandAllocationStrategy
import Amazonka.EC2.Types.SpotFleetLaunchSpecification
import Amazonka.EC2.Types.SpotMaintenanceStrategies
import Amazonka.EC2.Types.TagSpecification
import Amazonka.EC2.Types.TargetCapacityUnitType
import qualified Amazonka.Prelude as Prelude

-- | Describes the configuration of a Spot Fleet request.
--
-- /See:/ 'newSpotFleetRequestConfigData' smart constructor.
data SpotFleetRequestConfigData = SpotFleetRequestConfigData'
  { -- | The strategy that determines how to allocate the target Spot Instance
    -- capacity across the Spot Instance pools specified by the Spot Fleet
    -- launch configuration. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-fleet-allocation-strategy.html Allocation strategies for Spot Instances>
    -- in the /Amazon EC2 User Guide/.
    --
    -- [priceCapacityOptimized (recommended)]
    --     Spot Fleet identifies the pools with the highest capacity
    --     availability for the number of instances that are launching. This
    --     means that we will request Spot Instances from the pools that we
    --     believe have the lowest chance of interruption in the near term.
    --     Spot Fleet then requests Spot Instances from the lowest priced of
    --     these pools.
    --
    -- [capacityOptimized]
    --     Spot Fleet identifies the pools with the highest capacity
    --     availability for the number of instances that are launching. This
    --     means that we will request Spot Instances from the pools that we
    --     believe have the lowest chance of interruption in the near term. To
    --     give certain instance types a higher chance of launching first, use
    --     @capacityOptimizedPrioritized@. Set a priority for each instance
    --     type by using the @Priority@ parameter for
    --     @LaunchTemplateOverrides@. You can assign the same priority to
    --     different @LaunchTemplateOverrides@. EC2 implements the priorities
    --     on a best-effort basis, but optimizes for capacity first.
    --     @capacityOptimizedPrioritized@ is supported only if your Spot Fleet
    --     uses a launch template. Note that if the
    --     @OnDemandAllocationStrategy@ is set to @prioritized@, the same
    --     priority is applied when fulfilling On-Demand capacity.
    --
    -- [diversified]
    --     Spot Fleet requests instances from all of the Spot Instance pools
    --     that you specify.
    --
    -- [lowestPrice]
    --     Spot Fleet requests instances from the lowest priced Spot Instance
    --     pool that has available capacity. If the lowest priced pool doesn\'t
    --     have available capacity, the Spot Instances come from the next
    --     lowest priced pool that has available capacity. If a pool runs out
    --     of capacity before fulfilling your desired capacity, Spot Fleet will
    --     continue to fulfill your request by drawing from the next lowest
    --     priced pool. To ensure that your desired capacity is met, you might
    --     receive Spot Instances from several pools. Because this strategy
    --     only considers instance price and not capacity availability, it
    --     might lead to high interruption rates.
    --
    -- Default: @lowestPrice@
    allocationStrategy :: Prelude.Maybe AllocationStrategy,
    -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of your listings. This helps to avoid duplicate listings.
    -- For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Reserved.
    context :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether running Spot Instances should be terminated if you
    -- decrease the target capacity of the Spot Fleet request below the current
    -- size of the Spot Fleet.
    excessCapacityTerminationPolicy :: Prelude.Maybe ExcessCapacityTerminationPolicy,
    -- | The number of units fulfilled by this request compared to the set target
    -- capacity. You cannot set this value.
    fulfilledCapacity :: Prelude.Maybe Prelude.Double,
    -- | The behavior when a Spot Instance is interrupted. The default is
    -- @terminate@.
    instanceInterruptionBehavior :: Prelude.Maybe InstanceInterruptionBehavior,
    -- | The number of Spot pools across which to allocate your target Spot
    -- capacity. Valid only when Spot __AllocationStrategy__ is set to
    -- @lowest-price@. Spot Fleet selects the cheapest Spot pools and evenly
    -- allocates your target Spot capacity across the number of Spot pools that
    -- you specify.
    --
    -- Note that Spot Fleet attempts to draw Spot Instances from the number of
    -- pools that you specify on a best effort basis. If a pool runs out of
    -- Spot capacity before fulfilling your target capacity, Spot Fleet will
    -- continue to fulfill your request by drawing from the next cheapest pool.
    -- To ensure that your target capacity is met, you might receive Spot
    -- Instances from more than the number of pools that you specified.
    -- Similarly, if most of the pools have no Spot capacity, you might receive
    -- your full target capacity from fewer than the number of pools that you
    -- specified.
    instancePoolsToUseCount :: Prelude.Maybe Prelude.Int,
    -- | The launch specifications for the Spot Fleet request. If you specify
    -- @LaunchSpecifications@, you can\'t specify @LaunchTemplateConfigs@. If
    -- you include On-Demand capacity in your request, you must use
    -- @LaunchTemplateConfigs@.
    launchSpecifications :: Prelude.Maybe [SpotFleetLaunchSpecification],
    -- | The launch template and overrides. If you specify
    -- @LaunchTemplateConfigs@, you can\'t specify @LaunchSpecifications@. If
    -- you include On-Demand capacity in your request, you must use
    -- @LaunchTemplateConfigs@.
    launchTemplateConfigs :: Prelude.Maybe [LaunchTemplateConfig],
    -- | One or more Classic Load Balancers and target groups to attach to the
    -- Spot Fleet request. Spot Fleet registers the running Spot Instances with
    -- the specified Classic Load Balancers and target groups.
    --
    -- With Network Load Balancers, Spot Fleet cannot register instances that
    -- have the following instance types: C1, CC1, CC2, CG1, CG2, CR1, CS1, G1,
    -- G2, HI1, HS1, M1, M2, M3, and T1.
    loadBalancersConfig :: Prelude.Maybe LoadBalancersConfig,
    -- | The order of the launch template overrides to use in fulfilling
    -- On-Demand capacity. If you specify @lowestPrice@, Spot Fleet uses price
    -- to determine the order, launching the lowest price first. If you specify
    -- @prioritized@, Spot Fleet uses the priority that you assign to each Spot
    -- Fleet launch template override, launching the highest priority first. If
    -- you do not specify a value, Spot Fleet defaults to @lowestPrice@.
    onDemandAllocationStrategy :: Prelude.Maybe OnDemandAllocationStrategy,
    -- | The number of On-Demand units fulfilled by this request compared to the
    -- set target On-Demand capacity.
    onDemandFulfilledCapacity :: Prelude.Maybe Prelude.Double,
    -- | The maximum amount per hour for On-Demand Instances that you\'re willing
    -- to pay. You can use the @onDemandMaxTotalPrice@ parameter, the
    -- @spotMaxTotalPrice@ parameter, or both parameters to ensure that your
    -- fleet cost does not exceed your budget. If you set a maximum price per
    -- hour for the On-Demand Instances and Spot Instances in your request,
    -- Spot Fleet will launch instances until it reaches the maximum amount
    -- you\'re willing to pay. When the maximum amount you\'re willing to pay
    -- is reached, the fleet stops launching instances even if it hasn’t met
    -- the target capacity.
    onDemandMaxTotalPrice :: Prelude.Maybe Prelude.Text,
    -- | The number of On-Demand units to request. You can choose to set the
    -- target capacity in terms of instances or a performance characteristic
    -- that is important to your application workload, such as vCPUs, memory,
    -- or I\/O. If the request type is @maintain@, you can specify a target
    -- capacity of 0 and add capacity later.
    onDemandTargetCapacity :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether Spot Fleet should replace unhealthy instances.
    replaceUnhealthyInstances :: Prelude.Maybe Prelude.Bool,
    -- | The strategies for managing your Spot Instances that are at an elevated
    -- risk of being interrupted.
    spotMaintenanceStrategies :: Prelude.Maybe SpotMaintenanceStrategies,
    -- | The maximum amount per hour for Spot Instances that you\'re willing to
    -- pay. You can use the @spotdMaxTotalPrice@ parameter, the
    -- @onDemandMaxTotalPrice@ parameter, or both parameters to ensure that
    -- your fleet cost does not exceed your budget. If you set a maximum price
    -- per hour for the On-Demand Instances and Spot Instances in your request,
    -- Spot Fleet will launch instances until it reaches the maximum amount
    -- you\'re willing to pay. When the maximum amount you\'re willing to pay
    -- is reached, the fleet stops launching instances even if it hasn’t met
    -- the target capacity.
    spotMaxTotalPrice :: Prelude.Maybe Prelude.Text,
    -- | The maximum price per unit hour that you are willing to pay for a Spot
    -- Instance. We do not recommend using this parameter because it can lead
    -- to increased interruptions. If you do not specify this parameter, you
    -- will pay the current Spot price.
    --
    -- If you specify a maximum price, your instances will be interrupted more
    -- frequently than if you do not specify this parameter.
    spotPrice :: Prelude.Maybe Prelude.Text,
    -- | The key-value pair for tagging the Spot Fleet request on creation. The
    -- value for @ResourceType@ must be @spot-fleet-request@, otherwise the
    -- Spot Fleet request fails. To tag instances at launch, specify the tags
    -- in the
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-launch-templates.html#create-launch-template launch template>
    -- (valid only if you use @LaunchTemplateConfigs@) or in the
    -- @ @<https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_SpotFleetTagSpecification.html SpotFleetTagSpecification>@ @
    -- (valid only if you use @LaunchSpecifications@). For information about
    -- tagging after launch, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html#tag-resources Tagging Your Resources>.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | The unit for the target capacity.
    --
    -- Default: @units@ (translates to number of instances)
    targetCapacityUnitType :: Prelude.Maybe TargetCapacityUnitType,
    -- | Indicates whether running Spot Instances are terminated when the Spot
    -- Fleet request expires.
    terminateInstancesWithExpiration :: Prelude.Maybe Prelude.Bool,
    -- | The type of request. Indicates whether the Spot Fleet only requests the
    -- target capacity or also attempts to maintain it. When this value is
    -- @request@, the Spot Fleet only places the required requests. It does not
    -- attempt to replenish Spot Instances if capacity is diminished, nor does
    -- it submit requests in alternative Spot pools if capacity is not
    -- available. When this value is @maintain@, the Spot Fleet maintains the
    -- target capacity. The Spot Fleet places the required requests to meet
    -- capacity and automatically replenishes any interrupted instances.
    -- Default: @maintain@. @instant@ is listed but is not used by Spot Fleet.
    type' :: Prelude.Maybe FleetType,
    -- | The start date and time of the request, in UTC format
    -- (/YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). By default, Amazon EC2 starts
    -- fulfilling the request immediately.
    validFrom :: Prelude.Maybe Data.ISO8601,
    -- | The end date and time of the request, in UTC format
    -- (/YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). After the end date and time, no new
    -- Spot Instance requests are placed or able to fulfill the request. If no
    -- value is specified, the Spot Fleet request remains until you cancel it.
    validUntil :: Prelude.Maybe Data.ISO8601,
    -- | The Amazon Resource Name (ARN) of an Identity and Access Management
    -- (IAM) role that grants the Spot Fleet the permission to request, launch,
    -- terminate, and tag instances on your behalf. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-fleet-requests.html#spot-fleet-prerequisites Spot Fleet prerequisites>
    -- in the /Amazon EC2 User Guide/. Spot Fleet can terminate Spot Instances
    -- on your behalf when you cancel its Spot Fleet request using
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CancelSpotFleetRequests CancelSpotFleetRequests>
    -- or when the Spot Fleet request expires, if you set
    -- @TerminateInstancesWithExpiration@.
    iamFleetRole :: Prelude.Text,
    -- | The number of units to request for the Spot Fleet. You can choose to set
    -- the target capacity in terms of instances or a performance
    -- characteristic that is important to your application workload, such as
    -- vCPUs, memory, or I\/O. If the request type is @maintain@, you can
    -- specify a target capacity of 0 and add capacity later.
    targetCapacity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SpotFleetRequestConfigData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allocationStrategy', 'spotFleetRequestConfigData_allocationStrategy' - The strategy that determines how to allocate the target Spot Instance
-- capacity across the Spot Instance pools specified by the Spot Fleet
-- launch configuration. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-fleet-allocation-strategy.html Allocation strategies for Spot Instances>
-- in the /Amazon EC2 User Guide/.
--
-- [priceCapacityOptimized (recommended)]
--     Spot Fleet identifies the pools with the highest capacity
--     availability for the number of instances that are launching. This
--     means that we will request Spot Instances from the pools that we
--     believe have the lowest chance of interruption in the near term.
--     Spot Fleet then requests Spot Instances from the lowest priced of
--     these pools.
--
-- [capacityOptimized]
--     Spot Fleet identifies the pools with the highest capacity
--     availability for the number of instances that are launching. This
--     means that we will request Spot Instances from the pools that we
--     believe have the lowest chance of interruption in the near term. To
--     give certain instance types a higher chance of launching first, use
--     @capacityOptimizedPrioritized@. Set a priority for each instance
--     type by using the @Priority@ parameter for
--     @LaunchTemplateOverrides@. You can assign the same priority to
--     different @LaunchTemplateOverrides@. EC2 implements the priorities
--     on a best-effort basis, but optimizes for capacity first.
--     @capacityOptimizedPrioritized@ is supported only if your Spot Fleet
--     uses a launch template. Note that if the
--     @OnDemandAllocationStrategy@ is set to @prioritized@, the same
--     priority is applied when fulfilling On-Demand capacity.
--
-- [diversified]
--     Spot Fleet requests instances from all of the Spot Instance pools
--     that you specify.
--
-- [lowestPrice]
--     Spot Fleet requests instances from the lowest priced Spot Instance
--     pool that has available capacity. If the lowest priced pool doesn\'t
--     have available capacity, the Spot Instances come from the next
--     lowest priced pool that has available capacity. If a pool runs out
--     of capacity before fulfilling your desired capacity, Spot Fleet will
--     continue to fulfill your request by drawing from the next lowest
--     priced pool. To ensure that your desired capacity is met, you might
--     receive Spot Instances from several pools. Because this strategy
--     only considers instance price and not capacity availability, it
--     might lead to high interruption rates.
--
-- Default: @lowestPrice@
--
-- 'clientToken', 'spotFleetRequestConfigData_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of your listings. This helps to avoid duplicate listings.
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
--
-- 'context', 'spotFleetRequestConfigData_context' - Reserved.
--
-- 'excessCapacityTerminationPolicy', 'spotFleetRequestConfigData_excessCapacityTerminationPolicy' - Indicates whether running Spot Instances should be terminated if you
-- decrease the target capacity of the Spot Fleet request below the current
-- size of the Spot Fleet.
--
-- 'fulfilledCapacity', 'spotFleetRequestConfigData_fulfilledCapacity' - The number of units fulfilled by this request compared to the set target
-- capacity. You cannot set this value.
--
-- 'instanceInterruptionBehavior', 'spotFleetRequestConfigData_instanceInterruptionBehavior' - The behavior when a Spot Instance is interrupted. The default is
-- @terminate@.
--
-- 'instancePoolsToUseCount', 'spotFleetRequestConfigData_instancePoolsToUseCount' - The number of Spot pools across which to allocate your target Spot
-- capacity. Valid only when Spot __AllocationStrategy__ is set to
-- @lowest-price@. Spot Fleet selects the cheapest Spot pools and evenly
-- allocates your target Spot capacity across the number of Spot pools that
-- you specify.
--
-- Note that Spot Fleet attempts to draw Spot Instances from the number of
-- pools that you specify on a best effort basis. If a pool runs out of
-- Spot capacity before fulfilling your target capacity, Spot Fleet will
-- continue to fulfill your request by drawing from the next cheapest pool.
-- To ensure that your target capacity is met, you might receive Spot
-- Instances from more than the number of pools that you specified.
-- Similarly, if most of the pools have no Spot capacity, you might receive
-- your full target capacity from fewer than the number of pools that you
-- specified.
--
-- 'launchSpecifications', 'spotFleetRequestConfigData_launchSpecifications' - The launch specifications for the Spot Fleet request. If you specify
-- @LaunchSpecifications@, you can\'t specify @LaunchTemplateConfigs@. If
-- you include On-Demand capacity in your request, you must use
-- @LaunchTemplateConfigs@.
--
-- 'launchTemplateConfigs', 'spotFleetRequestConfigData_launchTemplateConfigs' - The launch template and overrides. If you specify
-- @LaunchTemplateConfigs@, you can\'t specify @LaunchSpecifications@. If
-- you include On-Demand capacity in your request, you must use
-- @LaunchTemplateConfigs@.
--
-- 'loadBalancersConfig', 'spotFleetRequestConfigData_loadBalancersConfig' - One or more Classic Load Balancers and target groups to attach to the
-- Spot Fleet request. Spot Fleet registers the running Spot Instances with
-- the specified Classic Load Balancers and target groups.
--
-- With Network Load Balancers, Spot Fleet cannot register instances that
-- have the following instance types: C1, CC1, CC2, CG1, CG2, CR1, CS1, G1,
-- G2, HI1, HS1, M1, M2, M3, and T1.
--
-- 'onDemandAllocationStrategy', 'spotFleetRequestConfigData_onDemandAllocationStrategy' - The order of the launch template overrides to use in fulfilling
-- On-Demand capacity. If you specify @lowestPrice@, Spot Fleet uses price
-- to determine the order, launching the lowest price first. If you specify
-- @prioritized@, Spot Fleet uses the priority that you assign to each Spot
-- Fleet launch template override, launching the highest priority first. If
-- you do not specify a value, Spot Fleet defaults to @lowestPrice@.
--
-- 'onDemandFulfilledCapacity', 'spotFleetRequestConfigData_onDemandFulfilledCapacity' - The number of On-Demand units fulfilled by this request compared to the
-- set target On-Demand capacity.
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
-- 'onDemandTargetCapacity', 'spotFleetRequestConfigData_onDemandTargetCapacity' - The number of On-Demand units to request. You can choose to set the
-- target capacity in terms of instances or a performance characteristic
-- that is important to your application workload, such as vCPUs, memory,
-- or I\/O. If the request type is @maintain@, you can specify a target
-- capacity of 0 and add capacity later.
--
-- 'replaceUnhealthyInstances', 'spotFleetRequestConfigData_replaceUnhealthyInstances' - Indicates whether Spot Fleet should replace unhealthy instances.
--
-- 'spotMaintenanceStrategies', 'spotFleetRequestConfigData_spotMaintenanceStrategies' - The strategies for managing your Spot Instances that are at an elevated
-- risk of being interrupted.
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
-- 'spotPrice', 'spotFleetRequestConfigData_spotPrice' - The maximum price per unit hour that you are willing to pay for a Spot
-- Instance. We do not recommend using this parameter because it can lead
-- to increased interruptions. If you do not specify this parameter, you
-- will pay the current Spot price.
--
-- If you specify a maximum price, your instances will be interrupted more
-- frequently than if you do not specify this parameter.
--
-- 'tagSpecifications', 'spotFleetRequestConfigData_tagSpecifications' - The key-value pair for tagging the Spot Fleet request on creation. The
-- value for @ResourceType@ must be @spot-fleet-request@, otherwise the
-- Spot Fleet request fails. To tag instances at launch, specify the tags
-- in the
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-launch-templates.html#create-launch-template launch template>
-- (valid only if you use @LaunchTemplateConfigs@) or in the
-- @ @<https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_SpotFleetTagSpecification.html SpotFleetTagSpecification>@ @
-- (valid only if you use @LaunchSpecifications@). For information about
-- tagging after launch, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html#tag-resources Tagging Your Resources>.
--
-- 'targetCapacityUnitType', 'spotFleetRequestConfigData_targetCapacityUnitType' - The unit for the target capacity.
--
-- Default: @units@ (translates to number of instances)
--
-- 'terminateInstancesWithExpiration', 'spotFleetRequestConfigData_terminateInstancesWithExpiration' - Indicates whether running Spot Instances are terminated when the Spot
-- Fleet request expires.
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
-- 'validFrom', 'spotFleetRequestConfigData_validFrom' - The start date and time of the request, in UTC format
-- (/YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). By default, Amazon EC2 starts
-- fulfilling the request immediately.
--
-- 'validUntil', 'spotFleetRequestConfigData_validUntil' - The end date and time of the request, in UTC format
-- (/YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). After the end date and time, no new
-- Spot Instance requests are placed or able to fulfill the request. If no
-- value is specified, the Spot Fleet request remains until you cancel it.
--
-- 'iamFleetRole', 'spotFleetRequestConfigData_iamFleetRole' - The Amazon Resource Name (ARN) of an Identity and Access Management
-- (IAM) role that grants the Spot Fleet the permission to request, launch,
-- terminate, and tag instances on your behalf. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-fleet-requests.html#spot-fleet-prerequisites Spot Fleet prerequisites>
-- in the /Amazon EC2 User Guide/. Spot Fleet can terminate Spot Instances
-- on your behalf when you cancel its Spot Fleet request using
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
  Prelude.Text ->
  -- | 'targetCapacity'
  Prelude.Int ->
  SpotFleetRequestConfigData
newSpotFleetRequestConfigData
  pIamFleetRole_
  pTargetCapacity_ =
    SpotFleetRequestConfigData'
      { allocationStrategy =
          Prelude.Nothing,
        clientToken = Prelude.Nothing,
        context = Prelude.Nothing,
        excessCapacityTerminationPolicy =
          Prelude.Nothing,
        fulfilledCapacity = Prelude.Nothing,
        instanceInterruptionBehavior = Prelude.Nothing,
        instancePoolsToUseCount = Prelude.Nothing,
        launchSpecifications = Prelude.Nothing,
        launchTemplateConfigs = Prelude.Nothing,
        loadBalancersConfig = Prelude.Nothing,
        onDemandAllocationStrategy = Prelude.Nothing,
        onDemandFulfilledCapacity = Prelude.Nothing,
        onDemandMaxTotalPrice = Prelude.Nothing,
        onDemandTargetCapacity = Prelude.Nothing,
        replaceUnhealthyInstances = Prelude.Nothing,
        spotMaintenanceStrategies = Prelude.Nothing,
        spotMaxTotalPrice = Prelude.Nothing,
        spotPrice = Prelude.Nothing,
        tagSpecifications = Prelude.Nothing,
        targetCapacityUnitType = Prelude.Nothing,
        terminateInstancesWithExpiration =
          Prelude.Nothing,
        type' = Prelude.Nothing,
        validFrom = Prelude.Nothing,
        validUntil = Prelude.Nothing,
        iamFleetRole = pIamFleetRole_,
        targetCapacity = pTargetCapacity_
      }

-- | The strategy that determines how to allocate the target Spot Instance
-- capacity across the Spot Instance pools specified by the Spot Fleet
-- launch configuration. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-fleet-allocation-strategy.html Allocation strategies for Spot Instances>
-- in the /Amazon EC2 User Guide/.
--
-- [priceCapacityOptimized (recommended)]
--     Spot Fleet identifies the pools with the highest capacity
--     availability for the number of instances that are launching. This
--     means that we will request Spot Instances from the pools that we
--     believe have the lowest chance of interruption in the near term.
--     Spot Fleet then requests Spot Instances from the lowest priced of
--     these pools.
--
-- [capacityOptimized]
--     Spot Fleet identifies the pools with the highest capacity
--     availability for the number of instances that are launching. This
--     means that we will request Spot Instances from the pools that we
--     believe have the lowest chance of interruption in the near term. To
--     give certain instance types a higher chance of launching first, use
--     @capacityOptimizedPrioritized@. Set a priority for each instance
--     type by using the @Priority@ parameter for
--     @LaunchTemplateOverrides@. You can assign the same priority to
--     different @LaunchTemplateOverrides@. EC2 implements the priorities
--     on a best-effort basis, but optimizes for capacity first.
--     @capacityOptimizedPrioritized@ is supported only if your Spot Fleet
--     uses a launch template. Note that if the
--     @OnDemandAllocationStrategy@ is set to @prioritized@, the same
--     priority is applied when fulfilling On-Demand capacity.
--
-- [diversified]
--     Spot Fleet requests instances from all of the Spot Instance pools
--     that you specify.
--
-- [lowestPrice]
--     Spot Fleet requests instances from the lowest priced Spot Instance
--     pool that has available capacity. If the lowest priced pool doesn\'t
--     have available capacity, the Spot Instances come from the next
--     lowest priced pool that has available capacity. If a pool runs out
--     of capacity before fulfilling your desired capacity, Spot Fleet will
--     continue to fulfill your request by drawing from the next lowest
--     priced pool. To ensure that your desired capacity is met, you might
--     receive Spot Instances from several pools. Because this strategy
--     only considers instance price and not capacity availability, it
--     might lead to high interruption rates.
--
-- Default: @lowestPrice@
spotFleetRequestConfigData_allocationStrategy :: Lens.Lens' SpotFleetRequestConfigData (Prelude.Maybe AllocationStrategy)
spotFleetRequestConfigData_allocationStrategy = Lens.lens (\SpotFleetRequestConfigData' {allocationStrategy} -> allocationStrategy) (\s@SpotFleetRequestConfigData' {} a -> s {allocationStrategy = a} :: SpotFleetRequestConfigData)

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of your listings. This helps to avoid duplicate listings.
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
spotFleetRequestConfigData_clientToken :: Lens.Lens' SpotFleetRequestConfigData (Prelude.Maybe Prelude.Text)
spotFleetRequestConfigData_clientToken = Lens.lens (\SpotFleetRequestConfigData' {clientToken} -> clientToken) (\s@SpotFleetRequestConfigData' {} a -> s {clientToken = a} :: SpotFleetRequestConfigData)

-- | Reserved.
spotFleetRequestConfigData_context :: Lens.Lens' SpotFleetRequestConfigData (Prelude.Maybe Prelude.Text)
spotFleetRequestConfigData_context = Lens.lens (\SpotFleetRequestConfigData' {context} -> context) (\s@SpotFleetRequestConfigData' {} a -> s {context = a} :: SpotFleetRequestConfigData)

-- | Indicates whether running Spot Instances should be terminated if you
-- decrease the target capacity of the Spot Fleet request below the current
-- size of the Spot Fleet.
spotFleetRequestConfigData_excessCapacityTerminationPolicy :: Lens.Lens' SpotFleetRequestConfigData (Prelude.Maybe ExcessCapacityTerminationPolicy)
spotFleetRequestConfigData_excessCapacityTerminationPolicy = Lens.lens (\SpotFleetRequestConfigData' {excessCapacityTerminationPolicy} -> excessCapacityTerminationPolicy) (\s@SpotFleetRequestConfigData' {} a -> s {excessCapacityTerminationPolicy = a} :: SpotFleetRequestConfigData)

-- | The number of units fulfilled by this request compared to the set target
-- capacity. You cannot set this value.
spotFleetRequestConfigData_fulfilledCapacity :: Lens.Lens' SpotFleetRequestConfigData (Prelude.Maybe Prelude.Double)
spotFleetRequestConfigData_fulfilledCapacity = Lens.lens (\SpotFleetRequestConfigData' {fulfilledCapacity} -> fulfilledCapacity) (\s@SpotFleetRequestConfigData' {} a -> s {fulfilledCapacity = a} :: SpotFleetRequestConfigData)

-- | The behavior when a Spot Instance is interrupted. The default is
-- @terminate@.
spotFleetRequestConfigData_instanceInterruptionBehavior :: Lens.Lens' SpotFleetRequestConfigData (Prelude.Maybe InstanceInterruptionBehavior)
spotFleetRequestConfigData_instanceInterruptionBehavior = Lens.lens (\SpotFleetRequestConfigData' {instanceInterruptionBehavior} -> instanceInterruptionBehavior) (\s@SpotFleetRequestConfigData' {} a -> s {instanceInterruptionBehavior = a} :: SpotFleetRequestConfigData)

-- | The number of Spot pools across which to allocate your target Spot
-- capacity. Valid only when Spot __AllocationStrategy__ is set to
-- @lowest-price@. Spot Fleet selects the cheapest Spot pools and evenly
-- allocates your target Spot capacity across the number of Spot pools that
-- you specify.
--
-- Note that Spot Fleet attempts to draw Spot Instances from the number of
-- pools that you specify on a best effort basis. If a pool runs out of
-- Spot capacity before fulfilling your target capacity, Spot Fleet will
-- continue to fulfill your request by drawing from the next cheapest pool.
-- To ensure that your target capacity is met, you might receive Spot
-- Instances from more than the number of pools that you specified.
-- Similarly, if most of the pools have no Spot capacity, you might receive
-- your full target capacity from fewer than the number of pools that you
-- specified.
spotFleetRequestConfigData_instancePoolsToUseCount :: Lens.Lens' SpotFleetRequestConfigData (Prelude.Maybe Prelude.Int)
spotFleetRequestConfigData_instancePoolsToUseCount = Lens.lens (\SpotFleetRequestConfigData' {instancePoolsToUseCount} -> instancePoolsToUseCount) (\s@SpotFleetRequestConfigData' {} a -> s {instancePoolsToUseCount = a} :: SpotFleetRequestConfigData)

-- | The launch specifications for the Spot Fleet request. If you specify
-- @LaunchSpecifications@, you can\'t specify @LaunchTemplateConfigs@. If
-- you include On-Demand capacity in your request, you must use
-- @LaunchTemplateConfigs@.
spotFleetRequestConfigData_launchSpecifications :: Lens.Lens' SpotFleetRequestConfigData (Prelude.Maybe [SpotFleetLaunchSpecification])
spotFleetRequestConfigData_launchSpecifications = Lens.lens (\SpotFleetRequestConfigData' {launchSpecifications} -> launchSpecifications) (\s@SpotFleetRequestConfigData' {} a -> s {launchSpecifications = a} :: SpotFleetRequestConfigData) Prelude.. Lens.mapping Lens.coerced

-- | The launch template and overrides. If you specify
-- @LaunchTemplateConfigs@, you can\'t specify @LaunchSpecifications@. If
-- you include On-Demand capacity in your request, you must use
-- @LaunchTemplateConfigs@.
spotFleetRequestConfigData_launchTemplateConfigs :: Lens.Lens' SpotFleetRequestConfigData (Prelude.Maybe [LaunchTemplateConfig])
spotFleetRequestConfigData_launchTemplateConfigs = Lens.lens (\SpotFleetRequestConfigData' {launchTemplateConfigs} -> launchTemplateConfigs) (\s@SpotFleetRequestConfigData' {} a -> s {launchTemplateConfigs = a} :: SpotFleetRequestConfigData) Prelude.. Lens.mapping Lens.coerced

-- | One or more Classic Load Balancers and target groups to attach to the
-- Spot Fleet request. Spot Fleet registers the running Spot Instances with
-- the specified Classic Load Balancers and target groups.
--
-- With Network Load Balancers, Spot Fleet cannot register instances that
-- have the following instance types: C1, CC1, CC2, CG1, CG2, CR1, CS1, G1,
-- G2, HI1, HS1, M1, M2, M3, and T1.
spotFleetRequestConfigData_loadBalancersConfig :: Lens.Lens' SpotFleetRequestConfigData (Prelude.Maybe LoadBalancersConfig)
spotFleetRequestConfigData_loadBalancersConfig = Lens.lens (\SpotFleetRequestConfigData' {loadBalancersConfig} -> loadBalancersConfig) (\s@SpotFleetRequestConfigData' {} a -> s {loadBalancersConfig = a} :: SpotFleetRequestConfigData)

-- | The order of the launch template overrides to use in fulfilling
-- On-Demand capacity. If you specify @lowestPrice@, Spot Fleet uses price
-- to determine the order, launching the lowest price first. If you specify
-- @prioritized@, Spot Fleet uses the priority that you assign to each Spot
-- Fleet launch template override, launching the highest priority first. If
-- you do not specify a value, Spot Fleet defaults to @lowestPrice@.
spotFleetRequestConfigData_onDemandAllocationStrategy :: Lens.Lens' SpotFleetRequestConfigData (Prelude.Maybe OnDemandAllocationStrategy)
spotFleetRequestConfigData_onDemandAllocationStrategy = Lens.lens (\SpotFleetRequestConfigData' {onDemandAllocationStrategy} -> onDemandAllocationStrategy) (\s@SpotFleetRequestConfigData' {} a -> s {onDemandAllocationStrategy = a} :: SpotFleetRequestConfigData)

-- | The number of On-Demand units fulfilled by this request compared to the
-- set target On-Demand capacity.
spotFleetRequestConfigData_onDemandFulfilledCapacity :: Lens.Lens' SpotFleetRequestConfigData (Prelude.Maybe Prelude.Double)
spotFleetRequestConfigData_onDemandFulfilledCapacity = Lens.lens (\SpotFleetRequestConfigData' {onDemandFulfilledCapacity} -> onDemandFulfilledCapacity) (\s@SpotFleetRequestConfigData' {} a -> s {onDemandFulfilledCapacity = a} :: SpotFleetRequestConfigData)

-- | The maximum amount per hour for On-Demand Instances that you\'re willing
-- to pay. You can use the @onDemandMaxTotalPrice@ parameter, the
-- @spotMaxTotalPrice@ parameter, or both parameters to ensure that your
-- fleet cost does not exceed your budget. If you set a maximum price per
-- hour for the On-Demand Instances and Spot Instances in your request,
-- Spot Fleet will launch instances until it reaches the maximum amount
-- you\'re willing to pay. When the maximum amount you\'re willing to pay
-- is reached, the fleet stops launching instances even if it hasn’t met
-- the target capacity.
spotFleetRequestConfigData_onDemandMaxTotalPrice :: Lens.Lens' SpotFleetRequestConfigData (Prelude.Maybe Prelude.Text)
spotFleetRequestConfigData_onDemandMaxTotalPrice = Lens.lens (\SpotFleetRequestConfigData' {onDemandMaxTotalPrice} -> onDemandMaxTotalPrice) (\s@SpotFleetRequestConfigData' {} a -> s {onDemandMaxTotalPrice = a} :: SpotFleetRequestConfigData)

-- | The number of On-Demand units to request. You can choose to set the
-- target capacity in terms of instances or a performance characteristic
-- that is important to your application workload, such as vCPUs, memory,
-- or I\/O. If the request type is @maintain@, you can specify a target
-- capacity of 0 and add capacity later.
spotFleetRequestConfigData_onDemandTargetCapacity :: Lens.Lens' SpotFleetRequestConfigData (Prelude.Maybe Prelude.Int)
spotFleetRequestConfigData_onDemandTargetCapacity = Lens.lens (\SpotFleetRequestConfigData' {onDemandTargetCapacity} -> onDemandTargetCapacity) (\s@SpotFleetRequestConfigData' {} a -> s {onDemandTargetCapacity = a} :: SpotFleetRequestConfigData)

-- | Indicates whether Spot Fleet should replace unhealthy instances.
spotFleetRequestConfigData_replaceUnhealthyInstances :: Lens.Lens' SpotFleetRequestConfigData (Prelude.Maybe Prelude.Bool)
spotFleetRequestConfigData_replaceUnhealthyInstances = Lens.lens (\SpotFleetRequestConfigData' {replaceUnhealthyInstances} -> replaceUnhealthyInstances) (\s@SpotFleetRequestConfigData' {} a -> s {replaceUnhealthyInstances = a} :: SpotFleetRequestConfigData)

-- | The strategies for managing your Spot Instances that are at an elevated
-- risk of being interrupted.
spotFleetRequestConfigData_spotMaintenanceStrategies :: Lens.Lens' SpotFleetRequestConfigData (Prelude.Maybe SpotMaintenanceStrategies)
spotFleetRequestConfigData_spotMaintenanceStrategies = Lens.lens (\SpotFleetRequestConfigData' {spotMaintenanceStrategies} -> spotMaintenanceStrategies) (\s@SpotFleetRequestConfigData' {} a -> s {spotMaintenanceStrategies = a} :: SpotFleetRequestConfigData)

-- | The maximum amount per hour for Spot Instances that you\'re willing to
-- pay. You can use the @spotdMaxTotalPrice@ parameter, the
-- @onDemandMaxTotalPrice@ parameter, or both parameters to ensure that
-- your fleet cost does not exceed your budget. If you set a maximum price
-- per hour for the On-Demand Instances and Spot Instances in your request,
-- Spot Fleet will launch instances until it reaches the maximum amount
-- you\'re willing to pay. When the maximum amount you\'re willing to pay
-- is reached, the fleet stops launching instances even if it hasn’t met
-- the target capacity.
spotFleetRequestConfigData_spotMaxTotalPrice :: Lens.Lens' SpotFleetRequestConfigData (Prelude.Maybe Prelude.Text)
spotFleetRequestConfigData_spotMaxTotalPrice = Lens.lens (\SpotFleetRequestConfigData' {spotMaxTotalPrice} -> spotMaxTotalPrice) (\s@SpotFleetRequestConfigData' {} a -> s {spotMaxTotalPrice = a} :: SpotFleetRequestConfigData)

-- | The maximum price per unit hour that you are willing to pay for a Spot
-- Instance. We do not recommend using this parameter because it can lead
-- to increased interruptions. If you do not specify this parameter, you
-- will pay the current Spot price.
--
-- If you specify a maximum price, your instances will be interrupted more
-- frequently than if you do not specify this parameter.
spotFleetRequestConfigData_spotPrice :: Lens.Lens' SpotFleetRequestConfigData (Prelude.Maybe Prelude.Text)
spotFleetRequestConfigData_spotPrice = Lens.lens (\SpotFleetRequestConfigData' {spotPrice} -> spotPrice) (\s@SpotFleetRequestConfigData' {} a -> s {spotPrice = a} :: SpotFleetRequestConfigData)

-- | The key-value pair for tagging the Spot Fleet request on creation. The
-- value for @ResourceType@ must be @spot-fleet-request@, otherwise the
-- Spot Fleet request fails. To tag instances at launch, specify the tags
-- in the
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-launch-templates.html#create-launch-template launch template>
-- (valid only if you use @LaunchTemplateConfigs@) or in the
-- @ @<https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_SpotFleetTagSpecification.html SpotFleetTagSpecification>@ @
-- (valid only if you use @LaunchSpecifications@). For information about
-- tagging after launch, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html#tag-resources Tagging Your Resources>.
spotFleetRequestConfigData_tagSpecifications :: Lens.Lens' SpotFleetRequestConfigData (Prelude.Maybe [TagSpecification])
spotFleetRequestConfigData_tagSpecifications = Lens.lens (\SpotFleetRequestConfigData' {tagSpecifications} -> tagSpecifications) (\s@SpotFleetRequestConfigData' {} a -> s {tagSpecifications = a} :: SpotFleetRequestConfigData) Prelude.. Lens.mapping Lens.coerced

-- | The unit for the target capacity.
--
-- Default: @units@ (translates to number of instances)
spotFleetRequestConfigData_targetCapacityUnitType :: Lens.Lens' SpotFleetRequestConfigData (Prelude.Maybe TargetCapacityUnitType)
spotFleetRequestConfigData_targetCapacityUnitType = Lens.lens (\SpotFleetRequestConfigData' {targetCapacityUnitType} -> targetCapacityUnitType) (\s@SpotFleetRequestConfigData' {} a -> s {targetCapacityUnitType = a} :: SpotFleetRequestConfigData)

-- | Indicates whether running Spot Instances are terminated when the Spot
-- Fleet request expires.
spotFleetRequestConfigData_terminateInstancesWithExpiration :: Lens.Lens' SpotFleetRequestConfigData (Prelude.Maybe Prelude.Bool)
spotFleetRequestConfigData_terminateInstancesWithExpiration = Lens.lens (\SpotFleetRequestConfigData' {terminateInstancesWithExpiration} -> terminateInstancesWithExpiration) (\s@SpotFleetRequestConfigData' {} a -> s {terminateInstancesWithExpiration = a} :: SpotFleetRequestConfigData)

-- | The type of request. Indicates whether the Spot Fleet only requests the
-- target capacity or also attempts to maintain it. When this value is
-- @request@, the Spot Fleet only places the required requests. It does not
-- attempt to replenish Spot Instances if capacity is diminished, nor does
-- it submit requests in alternative Spot pools if capacity is not
-- available. When this value is @maintain@, the Spot Fleet maintains the
-- target capacity. The Spot Fleet places the required requests to meet
-- capacity and automatically replenishes any interrupted instances.
-- Default: @maintain@. @instant@ is listed but is not used by Spot Fleet.
spotFleetRequestConfigData_type :: Lens.Lens' SpotFleetRequestConfigData (Prelude.Maybe FleetType)
spotFleetRequestConfigData_type = Lens.lens (\SpotFleetRequestConfigData' {type'} -> type') (\s@SpotFleetRequestConfigData' {} a -> s {type' = a} :: SpotFleetRequestConfigData)

-- | The start date and time of the request, in UTC format
-- (/YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). By default, Amazon EC2 starts
-- fulfilling the request immediately.
spotFleetRequestConfigData_validFrom :: Lens.Lens' SpotFleetRequestConfigData (Prelude.Maybe Prelude.UTCTime)
spotFleetRequestConfigData_validFrom = Lens.lens (\SpotFleetRequestConfigData' {validFrom} -> validFrom) (\s@SpotFleetRequestConfigData' {} a -> s {validFrom = a} :: SpotFleetRequestConfigData) Prelude.. Lens.mapping Data._Time

-- | The end date and time of the request, in UTC format
-- (/YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). After the end date and time, no new
-- Spot Instance requests are placed or able to fulfill the request. If no
-- value is specified, the Spot Fleet request remains until you cancel it.
spotFleetRequestConfigData_validUntil :: Lens.Lens' SpotFleetRequestConfigData (Prelude.Maybe Prelude.UTCTime)
spotFleetRequestConfigData_validUntil = Lens.lens (\SpotFleetRequestConfigData' {validUntil} -> validUntil) (\s@SpotFleetRequestConfigData' {} a -> s {validUntil = a} :: SpotFleetRequestConfigData) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of an Identity and Access Management
-- (IAM) role that grants the Spot Fleet the permission to request, launch,
-- terminate, and tag instances on your behalf. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-fleet-requests.html#spot-fleet-prerequisites Spot Fleet prerequisites>
-- in the /Amazon EC2 User Guide/. Spot Fleet can terminate Spot Instances
-- on your behalf when you cancel its Spot Fleet request using
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CancelSpotFleetRequests CancelSpotFleetRequests>
-- or when the Spot Fleet request expires, if you set
-- @TerminateInstancesWithExpiration@.
spotFleetRequestConfigData_iamFleetRole :: Lens.Lens' SpotFleetRequestConfigData Prelude.Text
spotFleetRequestConfigData_iamFleetRole = Lens.lens (\SpotFleetRequestConfigData' {iamFleetRole} -> iamFleetRole) (\s@SpotFleetRequestConfigData' {} a -> s {iamFleetRole = a} :: SpotFleetRequestConfigData)

-- | The number of units to request for the Spot Fleet. You can choose to set
-- the target capacity in terms of instances or a performance
-- characteristic that is important to your application workload, such as
-- vCPUs, memory, or I\/O. If the request type is @maintain@, you can
-- specify a target capacity of 0 and add capacity later.
spotFleetRequestConfigData_targetCapacity :: Lens.Lens' SpotFleetRequestConfigData Prelude.Int
spotFleetRequestConfigData_targetCapacity = Lens.lens (\SpotFleetRequestConfigData' {targetCapacity} -> targetCapacity) (\s@SpotFleetRequestConfigData' {} a -> s {targetCapacity = a} :: SpotFleetRequestConfigData)

instance Data.FromXML SpotFleetRequestConfigData where
  parseXML x =
    SpotFleetRequestConfigData'
      Prelude.<$> (x Data..@? "allocationStrategy")
      Prelude.<*> (x Data..@? "clientToken")
      Prelude.<*> (x Data..@? "context")
      Prelude.<*> (x Data..@? "excessCapacityTerminationPolicy")
      Prelude.<*> (x Data..@? "fulfilledCapacity")
      Prelude.<*> (x Data..@? "instanceInterruptionBehavior")
      Prelude.<*> (x Data..@? "instancePoolsToUseCount")
      Prelude.<*> ( x
                      Data..@? "launchSpecifications"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> ( x
                      Data..@? "launchTemplateConfigs"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "loadBalancersConfig")
      Prelude.<*> (x Data..@? "onDemandAllocationStrategy")
      Prelude.<*> (x Data..@? "onDemandFulfilledCapacity")
      Prelude.<*> (x Data..@? "onDemandMaxTotalPrice")
      Prelude.<*> (x Data..@? "onDemandTargetCapacity")
      Prelude.<*> (x Data..@? "replaceUnhealthyInstances")
      Prelude.<*> (x Data..@? "spotMaintenanceStrategies")
      Prelude.<*> (x Data..@? "spotMaxTotalPrice")
      Prelude.<*> (x Data..@? "spotPrice")
      Prelude.<*> ( x
                      Data..@? "TagSpecification"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "targetCapacityUnitType")
      Prelude.<*> (x Data..@? "terminateInstancesWithExpiration")
      Prelude.<*> (x Data..@? "type")
      Prelude.<*> (x Data..@? "validFrom")
      Prelude.<*> (x Data..@? "validUntil")
      Prelude.<*> (x Data..@ "iamFleetRole")
      Prelude.<*> (x Data..@ "targetCapacity")

instance Prelude.Hashable SpotFleetRequestConfigData where
  hashWithSalt _salt SpotFleetRequestConfigData' {..} =
    _salt
      `Prelude.hashWithSalt` allocationStrategy
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` context
      `Prelude.hashWithSalt` excessCapacityTerminationPolicy
      `Prelude.hashWithSalt` fulfilledCapacity
      `Prelude.hashWithSalt` instanceInterruptionBehavior
      `Prelude.hashWithSalt` instancePoolsToUseCount
      `Prelude.hashWithSalt` launchSpecifications
      `Prelude.hashWithSalt` launchTemplateConfigs
      `Prelude.hashWithSalt` loadBalancersConfig
      `Prelude.hashWithSalt` onDemandAllocationStrategy
      `Prelude.hashWithSalt` onDemandFulfilledCapacity
      `Prelude.hashWithSalt` onDemandMaxTotalPrice
      `Prelude.hashWithSalt` onDemandTargetCapacity
      `Prelude.hashWithSalt` replaceUnhealthyInstances
      `Prelude.hashWithSalt` spotMaintenanceStrategies
      `Prelude.hashWithSalt` spotMaxTotalPrice
      `Prelude.hashWithSalt` spotPrice
      `Prelude.hashWithSalt` tagSpecifications
      `Prelude.hashWithSalt` targetCapacityUnitType
      `Prelude.hashWithSalt` terminateInstancesWithExpiration
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` validFrom
      `Prelude.hashWithSalt` validUntil
      `Prelude.hashWithSalt` iamFleetRole
      `Prelude.hashWithSalt` targetCapacity

instance Prelude.NFData SpotFleetRequestConfigData where
  rnf SpotFleetRequestConfigData' {..} =
    Prelude.rnf allocationStrategy
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf context
      `Prelude.seq` Prelude.rnf excessCapacityTerminationPolicy
      `Prelude.seq` Prelude.rnf fulfilledCapacity
      `Prelude.seq` Prelude.rnf instanceInterruptionBehavior
      `Prelude.seq` Prelude.rnf instancePoolsToUseCount
      `Prelude.seq` Prelude.rnf launchSpecifications
      `Prelude.seq` Prelude.rnf launchTemplateConfigs
      `Prelude.seq` Prelude.rnf loadBalancersConfig
      `Prelude.seq` Prelude.rnf onDemandAllocationStrategy
      `Prelude.seq` Prelude.rnf onDemandFulfilledCapacity
      `Prelude.seq` Prelude.rnf onDemandMaxTotalPrice
      `Prelude.seq` Prelude.rnf onDemandTargetCapacity
      `Prelude.seq` Prelude.rnf replaceUnhealthyInstances
      `Prelude.seq` Prelude.rnf spotMaintenanceStrategies
      `Prelude.seq` Prelude.rnf spotMaxTotalPrice
      `Prelude.seq` Prelude.rnf spotPrice
      `Prelude.seq` Prelude.rnf tagSpecifications
      `Prelude.seq` Prelude.rnf
        targetCapacityUnitType
      `Prelude.seq` Prelude.rnf
        terminateInstancesWithExpiration
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf validFrom
      `Prelude.seq` Prelude.rnf validUntil
      `Prelude.seq` Prelude.rnf
        iamFleetRole
      `Prelude.seq` Prelude.rnf
        targetCapacity

instance Data.ToQuery SpotFleetRequestConfigData where
  toQuery SpotFleetRequestConfigData' {..} =
    Prelude.mconcat
      [ "AllocationStrategy" Data.=: allocationStrategy,
        "ClientToken" Data.=: clientToken,
        "Context" Data.=: context,
        "ExcessCapacityTerminationPolicy"
          Data.=: excessCapacityTerminationPolicy,
        "FulfilledCapacity" Data.=: fulfilledCapacity,
        "InstanceInterruptionBehavior"
          Data.=: instanceInterruptionBehavior,
        "InstancePoolsToUseCount"
          Data.=: instancePoolsToUseCount,
        Data.toQuery
          ( Data.toQueryList "LaunchSpecifications"
              Prelude.<$> launchSpecifications
          ),
        Data.toQuery
          ( Data.toQueryList "LaunchTemplateConfigs"
              Prelude.<$> launchTemplateConfigs
          ),
        "LoadBalancersConfig" Data.=: loadBalancersConfig,
        "OnDemandAllocationStrategy"
          Data.=: onDemandAllocationStrategy,
        "OnDemandFulfilledCapacity"
          Data.=: onDemandFulfilledCapacity,
        "OnDemandMaxTotalPrice"
          Data.=: onDemandMaxTotalPrice,
        "OnDemandTargetCapacity"
          Data.=: onDemandTargetCapacity,
        "ReplaceUnhealthyInstances"
          Data.=: replaceUnhealthyInstances,
        "SpotMaintenanceStrategies"
          Data.=: spotMaintenanceStrategies,
        "SpotMaxTotalPrice" Data.=: spotMaxTotalPrice,
        "SpotPrice" Data.=: spotPrice,
        Data.toQuery
          ( Data.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "TargetCapacityUnitType"
          Data.=: targetCapacityUnitType,
        "TerminateInstancesWithExpiration"
          Data.=: terminateInstancesWithExpiration,
        "Type" Data.=: type',
        "ValidFrom" Data.=: validFrom,
        "ValidUntil" Data.=: validUntil,
        "IamFleetRole" Data.=: iamFleetRole,
        "TargetCapacity" Data.=: targetCapacity
      ]
