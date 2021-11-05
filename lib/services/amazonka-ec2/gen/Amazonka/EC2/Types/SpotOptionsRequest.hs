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
-- Module      : Amazonka.EC2.Types.SpotOptionsRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.SpotOptionsRequest where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.FleetSpotMaintenanceStrategiesRequest
import Amazonka.EC2.Types.SpotAllocationStrategy
import Amazonka.EC2.Types.SpotInstanceInterruptionBehavior
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the configuration of Spot Instances in an EC2 Fleet request.
--
-- /See:/ 'newSpotOptionsRequest' smart constructor.
data SpotOptionsRequest = SpotOptionsRequest'
  { -- | The behavior when a Spot Instance is interrupted. The default is
    -- @terminate@.
    instanceInterruptionBehavior :: Prelude.Maybe SpotInstanceInterruptionBehavior,
    -- | Indicates that the fleet launches all Spot Instances into a single
    -- Availability Zone. Supported only for fleets of type @instant@.
    singleAvailabilityZone :: Prelude.Maybe Prelude.Bool,
    -- | The maximum amount per hour for Spot Instances that you\'re willing to
    -- pay.
    maxTotalPrice :: Prelude.Maybe Prelude.Text,
    -- | The minimum target capacity for Spot Instances in the fleet. If the
    -- minimum target capacity is not reached, the fleet launches no instances.
    minTargetCapacity :: Prelude.Maybe Prelude.Int,
    -- | The number of Spot pools across which to allocate your target Spot
    -- capacity. Valid only when Spot __AllocationStrategy__ is set to
    -- @lowest-price@. EC2 Fleet selects the cheapest Spot pools and evenly
    -- allocates your target Spot capacity across the number of Spot pools that
    -- you specify.
    --
    -- Note that EC2 Fleet attempts to draw Spot Instances from the number of
    -- pools that you specify on a best effort basis. If a pool runs out of
    -- Spot capacity before fulfilling your target capacity, EC2 Fleet will
    -- continue to fulfill your request by drawing from the next cheapest pool.
    -- To ensure that your target capacity is met, you might receive Spot
    -- Instances from more than the number of pools that you specified.
    -- Similarly, if most of the pools have no Spot capacity, you might receive
    -- your full target capacity from fewer than the number of pools that you
    -- specified.
    instancePoolsToUseCount :: Prelude.Maybe Prelude.Int,
    -- | The strategies for managing your Spot Instances that are at an elevated
    -- risk of being interrupted.
    maintenanceStrategies :: Prelude.Maybe FleetSpotMaintenanceStrategiesRequest,
    -- | Indicates that the fleet uses a single instance type to launch all Spot
    -- Instances in the fleet. Supported only for fleets of type @instant@.
    singleInstanceType :: Prelude.Maybe Prelude.Bool,
    -- | Indicates how to allocate the target Spot Instance capacity across the
    -- Spot Instance pools specified by the EC2 Fleet.
    --
    -- If the allocation strategy is @lowest-price@, EC2 Fleet launches
    -- instances from the Spot Instance pools with the lowest price. This is
    -- the default allocation strategy.
    --
    -- If the allocation strategy is @diversified@, EC2 Fleet launches
    -- instances from all of the Spot Instance pools that you specify.
    --
    -- If the allocation strategy is @capacity-optimized@ (recommended), EC2
    -- Fleet launches instances from Spot Instance pools with optimal capacity
    -- for the number of instances that are launching. To give certain instance
    -- types a higher chance of launching first, use
    -- @capacity-optimized-prioritized@. Set a priority for each instance type
    -- by using the @Priority@ parameter for @LaunchTemplateOverrides@. You can
    -- assign the same priority to different @LaunchTemplateOverrides@. EC2
    -- implements the priorities on a best-effort basis, but optimizes for
    -- capacity first. @capacity-optimized-prioritized@ is supported only if
    -- your fleet uses a launch template. Note that if the On-Demand
    -- @AllocationStrategy@ is set to @prioritized@, the same priority is
    -- applied when fulfilling On-Demand capacity.
    allocationStrategy :: Prelude.Maybe SpotAllocationStrategy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SpotOptionsRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceInterruptionBehavior', 'spotOptionsRequest_instanceInterruptionBehavior' - The behavior when a Spot Instance is interrupted. The default is
-- @terminate@.
--
-- 'singleAvailabilityZone', 'spotOptionsRequest_singleAvailabilityZone' - Indicates that the fleet launches all Spot Instances into a single
-- Availability Zone. Supported only for fleets of type @instant@.
--
-- 'maxTotalPrice', 'spotOptionsRequest_maxTotalPrice' - The maximum amount per hour for Spot Instances that you\'re willing to
-- pay.
--
-- 'minTargetCapacity', 'spotOptionsRequest_minTargetCapacity' - The minimum target capacity for Spot Instances in the fleet. If the
-- minimum target capacity is not reached, the fleet launches no instances.
--
-- 'instancePoolsToUseCount', 'spotOptionsRequest_instancePoolsToUseCount' - The number of Spot pools across which to allocate your target Spot
-- capacity. Valid only when Spot __AllocationStrategy__ is set to
-- @lowest-price@. EC2 Fleet selects the cheapest Spot pools and evenly
-- allocates your target Spot capacity across the number of Spot pools that
-- you specify.
--
-- Note that EC2 Fleet attempts to draw Spot Instances from the number of
-- pools that you specify on a best effort basis. If a pool runs out of
-- Spot capacity before fulfilling your target capacity, EC2 Fleet will
-- continue to fulfill your request by drawing from the next cheapest pool.
-- To ensure that your target capacity is met, you might receive Spot
-- Instances from more than the number of pools that you specified.
-- Similarly, if most of the pools have no Spot capacity, you might receive
-- your full target capacity from fewer than the number of pools that you
-- specified.
--
-- 'maintenanceStrategies', 'spotOptionsRequest_maintenanceStrategies' - The strategies for managing your Spot Instances that are at an elevated
-- risk of being interrupted.
--
-- 'singleInstanceType', 'spotOptionsRequest_singleInstanceType' - Indicates that the fleet uses a single instance type to launch all Spot
-- Instances in the fleet. Supported only for fleets of type @instant@.
--
-- 'allocationStrategy', 'spotOptionsRequest_allocationStrategy' - Indicates how to allocate the target Spot Instance capacity across the
-- Spot Instance pools specified by the EC2 Fleet.
--
-- If the allocation strategy is @lowest-price@, EC2 Fleet launches
-- instances from the Spot Instance pools with the lowest price. This is
-- the default allocation strategy.
--
-- If the allocation strategy is @diversified@, EC2 Fleet launches
-- instances from all of the Spot Instance pools that you specify.
--
-- If the allocation strategy is @capacity-optimized@ (recommended), EC2
-- Fleet launches instances from Spot Instance pools with optimal capacity
-- for the number of instances that are launching. To give certain instance
-- types a higher chance of launching first, use
-- @capacity-optimized-prioritized@. Set a priority for each instance type
-- by using the @Priority@ parameter for @LaunchTemplateOverrides@. You can
-- assign the same priority to different @LaunchTemplateOverrides@. EC2
-- implements the priorities on a best-effort basis, but optimizes for
-- capacity first. @capacity-optimized-prioritized@ is supported only if
-- your fleet uses a launch template. Note that if the On-Demand
-- @AllocationStrategy@ is set to @prioritized@, the same priority is
-- applied when fulfilling On-Demand capacity.
newSpotOptionsRequest ::
  SpotOptionsRequest
newSpotOptionsRequest =
  SpotOptionsRequest'
    { instanceInterruptionBehavior =
        Prelude.Nothing,
      singleAvailabilityZone = Prelude.Nothing,
      maxTotalPrice = Prelude.Nothing,
      minTargetCapacity = Prelude.Nothing,
      instancePoolsToUseCount = Prelude.Nothing,
      maintenanceStrategies = Prelude.Nothing,
      singleInstanceType = Prelude.Nothing,
      allocationStrategy = Prelude.Nothing
    }

-- | The behavior when a Spot Instance is interrupted. The default is
-- @terminate@.
spotOptionsRequest_instanceInterruptionBehavior :: Lens.Lens' SpotOptionsRequest (Prelude.Maybe SpotInstanceInterruptionBehavior)
spotOptionsRequest_instanceInterruptionBehavior = Lens.lens (\SpotOptionsRequest' {instanceInterruptionBehavior} -> instanceInterruptionBehavior) (\s@SpotOptionsRequest' {} a -> s {instanceInterruptionBehavior = a} :: SpotOptionsRequest)

-- | Indicates that the fleet launches all Spot Instances into a single
-- Availability Zone. Supported only for fleets of type @instant@.
spotOptionsRequest_singleAvailabilityZone :: Lens.Lens' SpotOptionsRequest (Prelude.Maybe Prelude.Bool)
spotOptionsRequest_singleAvailabilityZone = Lens.lens (\SpotOptionsRequest' {singleAvailabilityZone} -> singleAvailabilityZone) (\s@SpotOptionsRequest' {} a -> s {singleAvailabilityZone = a} :: SpotOptionsRequest)

-- | The maximum amount per hour for Spot Instances that you\'re willing to
-- pay.
spotOptionsRequest_maxTotalPrice :: Lens.Lens' SpotOptionsRequest (Prelude.Maybe Prelude.Text)
spotOptionsRequest_maxTotalPrice = Lens.lens (\SpotOptionsRequest' {maxTotalPrice} -> maxTotalPrice) (\s@SpotOptionsRequest' {} a -> s {maxTotalPrice = a} :: SpotOptionsRequest)

-- | The minimum target capacity for Spot Instances in the fleet. If the
-- minimum target capacity is not reached, the fleet launches no instances.
spotOptionsRequest_minTargetCapacity :: Lens.Lens' SpotOptionsRequest (Prelude.Maybe Prelude.Int)
spotOptionsRequest_minTargetCapacity = Lens.lens (\SpotOptionsRequest' {minTargetCapacity} -> minTargetCapacity) (\s@SpotOptionsRequest' {} a -> s {minTargetCapacity = a} :: SpotOptionsRequest)

-- | The number of Spot pools across which to allocate your target Spot
-- capacity. Valid only when Spot __AllocationStrategy__ is set to
-- @lowest-price@. EC2 Fleet selects the cheapest Spot pools and evenly
-- allocates your target Spot capacity across the number of Spot pools that
-- you specify.
--
-- Note that EC2 Fleet attempts to draw Spot Instances from the number of
-- pools that you specify on a best effort basis. If a pool runs out of
-- Spot capacity before fulfilling your target capacity, EC2 Fleet will
-- continue to fulfill your request by drawing from the next cheapest pool.
-- To ensure that your target capacity is met, you might receive Spot
-- Instances from more than the number of pools that you specified.
-- Similarly, if most of the pools have no Spot capacity, you might receive
-- your full target capacity from fewer than the number of pools that you
-- specified.
spotOptionsRequest_instancePoolsToUseCount :: Lens.Lens' SpotOptionsRequest (Prelude.Maybe Prelude.Int)
spotOptionsRequest_instancePoolsToUseCount = Lens.lens (\SpotOptionsRequest' {instancePoolsToUseCount} -> instancePoolsToUseCount) (\s@SpotOptionsRequest' {} a -> s {instancePoolsToUseCount = a} :: SpotOptionsRequest)

-- | The strategies for managing your Spot Instances that are at an elevated
-- risk of being interrupted.
spotOptionsRequest_maintenanceStrategies :: Lens.Lens' SpotOptionsRequest (Prelude.Maybe FleetSpotMaintenanceStrategiesRequest)
spotOptionsRequest_maintenanceStrategies = Lens.lens (\SpotOptionsRequest' {maintenanceStrategies} -> maintenanceStrategies) (\s@SpotOptionsRequest' {} a -> s {maintenanceStrategies = a} :: SpotOptionsRequest)

-- | Indicates that the fleet uses a single instance type to launch all Spot
-- Instances in the fleet. Supported only for fleets of type @instant@.
spotOptionsRequest_singleInstanceType :: Lens.Lens' SpotOptionsRequest (Prelude.Maybe Prelude.Bool)
spotOptionsRequest_singleInstanceType = Lens.lens (\SpotOptionsRequest' {singleInstanceType} -> singleInstanceType) (\s@SpotOptionsRequest' {} a -> s {singleInstanceType = a} :: SpotOptionsRequest)

-- | Indicates how to allocate the target Spot Instance capacity across the
-- Spot Instance pools specified by the EC2 Fleet.
--
-- If the allocation strategy is @lowest-price@, EC2 Fleet launches
-- instances from the Spot Instance pools with the lowest price. This is
-- the default allocation strategy.
--
-- If the allocation strategy is @diversified@, EC2 Fleet launches
-- instances from all of the Spot Instance pools that you specify.
--
-- If the allocation strategy is @capacity-optimized@ (recommended), EC2
-- Fleet launches instances from Spot Instance pools with optimal capacity
-- for the number of instances that are launching. To give certain instance
-- types a higher chance of launching first, use
-- @capacity-optimized-prioritized@. Set a priority for each instance type
-- by using the @Priority@ parameter for @LaunchTemplateOverrides@. You can
-- assign the same priority to different @LaunchTemplateOverrides@. EC2
-- implements the priorities on a best-effort basis, but optimizes for
-- capacity first. @capacity-optimized-prioritized@ is supported only if
-- your fleet uses a launch template. Note that if the On-Demand
-- @AllocationStrategy@ is set to @prioritized@, the same priority is
-- applied when fulfilling On-Demand capacity.
spotOptionsRequest_allocationStrategy :: Lens.Lens' SpotOptionsRequest (Prelude.Maybe SpotAllocationStrategy)
spotOptionsRequest_allocationStrategy = Lens.lens (\SpotOptionsRequest' {allocationStrategy} -> allocationStrategy) (\s@SpotOptionsRequest' {} a -> s {allocationStrategy = a} :: SpotOptionsRequest)

instance Prelude.Hashable SpotOptionsRequest

instance Prelude.NFData SpotOptionsRequest

instance Core.ToQuery SpotOptionsRequest where
  toQuery SpotOptionsRequest' {..} =
    Prelude.mconcat
      [ "InstanceInterruptionBehavior"
          Core.=: instanceInterruptionBehavior,
        "SingleAvailabilityZone"
          Core.=: singleAvailabilityZone,
        "MaxTotalPrice" Core.=: maxTotalPrice,
        "MinTargetCapacity" Core.=: minTargetCapacity,
        "InstancePoolsToUseCount"
          Core.=: instancePoolsToUseCount,
        "MaintenanceStrategies"
          Core.=: maintenanceStrategies,
        "SingleInstanceType" Core.=: singleInstanceType,
        "AllocationStrategy" Core.=: allocationStrategy
      ]
