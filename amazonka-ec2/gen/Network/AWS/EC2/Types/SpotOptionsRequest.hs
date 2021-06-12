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
-- Module      : Network.AWS.EC2.Types.SpotOptionsRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SpotOptionsRequest where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.FleetSpotMaintenanceStrategiesRequest
import Network.AWS.EC2.Types.SpotAllocationStrategy
import Network.AWS.EC2.Types.SpotInstanceInterruptionBehavior
import qualified Network.AWS.Lens as Lens

-- | Describes the configuration of Spot Instances in an EC2 Fleet request.
--
-- /See:/ 'newSpotOptionsRequest' smart constructor.
data SpotOptionsRequest = SpotOptionsRequest'
  { -- | The minimum target capacity for Spot Instances in the fleet. If the
    -- minimum target capacity is not reached, the fleet launches no instances.
    minTargetCapacity :: Core.Maybe Core.Int,
    -- | The strategies for managing your Spot Instances that are at an elevated
    -- risk of being interrupted.
    maintenanceStrategies :: Core.Maybe FleetSpotMaintenanceStrategiesRequest,
    -- | The behavior when a Spot Instance is interrupted. The default is
    -- @terminate@.
    instanceInterruptionBehavior :: Core.Maybe SpotInstanceInterruptionBehavior,
    -- | Indicates that the fleet uses a single instance type to launch all Spot
    -- Instances in the fleet. Supported only for fleets of type @instant@.
    singleInstanceType :: Core.Maybe Core.Bool,
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
    -- If the allocation strategy is @capacity-optimized@, EC2 Fleet launches
    -- instances from Spot Instance pools with optimal capacity for the number
    -- of instances that are launching.
    allocationStrategy :: Core.Maybe SpotAllocationStrategy,
    -- | The maximum amount per hour for Spot Instances that you\'re willing to
    -- pay.
    maxTotalPrice :: Core.Maybe Core.Text,
    -- | The number of Spot pools across which to allocate your target Spot
    -- capacity. Valid only when Spot __AllocationStrategy__ is set to
    -- @lowest-price@. EC2 Fleet selects the cheapest Spot pools and evenly
    -- allocates your target Spot capacity across the number of Spot pools that
    -- you specify.
    instancePoolsToUseCount :: Core.Maybe Core.Int,
    -- | Indicates that the fleet launches all Spot Instances into a single
    -- Availability Zone. Supported only for fleets of type @instant@.
    singleAvailabilityZone :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SpotOptionsRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'minTargetCapacity', 'spotOptionsRequest_minTargetCapacity' - The minimum target capacity for Spot Instances in the fleet. If the
-- minimum target capacity is not reached, the fleet launches no instances.
--
-- 'maintenanceStrategies', 'spotOptionsRequest_maintenanceStrategies' - The strategies for managing your Spot Instances that are at an elevated
-- risk of being interrupted.
--
-- 'instanceInterruptionBehavior', 'spotOptionsRequest_instanceInterruptionBehavior' - The behavior when a Spot Instance is interrupted. The default is
-- @terminate@.
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
-- If the allocation strategy is @capacity-optimized@, EC2 Fleet launches
-- instances from Spot Instance pools with optimal capacity for the number
-- of instances that are launching.
--
-- 'maxTotalPrice', 'spotOptionsRequest_maxTotalPrice' - The maximum amount per hour for Spot Instances that you\'re willing to
-- pay.
--
-- 'instancePoolsToUseCount', 'spotOptionsRequest_instancePoolsToUseCount' - The number of Spot pools across which to allocate your target Spot
-- capacity. Valid only when Spot __AllocationStrategy__ is set to
-- @lowest-price@. EC2 Fleet selects the cheapest Spot pools and evenly
-- allocates your target Spot capacity across the number of Spot pools that
-- you specify.
--
-- 'singleAvailabilityZone', 'spotOptionsRequest_singleAvailabilityZone' - Indicates that the fleet launches all Spot Instances into a single
-- Availability Zone. Supported only for fleets of type @instant@.
newSpotOptionsRequest ::
  SpotOptionsRequest
newSpotOptionsRequest =
  SpotOptionsRequest'
    { minTargetCapacity =
        Core.Nothing,
      maintenanceStrategies = Core.Nothing,
      instanceInterruptionBehavior = Core.Nothing,
      singleInstanceType = Core.Nothing,
      allocationStrategy = Core.Nothing,
      maxTotalPrice = Core.Nothing,
      instancePoolsToUseCount = Core.Nothing,
      singleAvailabilityZone = Core.Nothing
    }

-- | The minimum target capacity for Spot Instances in the fleet. If the
-- minimum target capacity is not reached, the fleet launches no instances.
spotOptionsRequest_minTargetCapacity :: Lens.Lens' SpotOptionsRequest (Core.Maybe Core.Int)
spotOptionsRequest_minTargetCapacity = Lens.lens (\SpotOptionsRequest' {minTargetCapacity} -> minTargetCapacity) (\s@SpotOptionsRequest' {} a -> s {minTargetCapacity = a} :: SpotOptionsRequest)

-- | The strategies for managing your Spot Instances that are at an elevated
-- risk of being interrupted.
spotOptionsRequest_maintenanceStrategies :: Lens.Lens' SpotOptionsRequest (Core.Maybe FleetSpotMaintenanceStrategiesRequest)
spotOptionsRequest_maintenanceStrategies = Lens.lens (\SpotOptionsRequest' {maintenanceStrategies} -> maintenanceStrategies) (\s@SpotOptionsRequest' {} a -> s {maintenanceStrategies = a} :: SpotOptionsRequest)

-- | The behavior when a Spot Instance is interrupted. The default is
-- @terminate@.
spotOptionsRequest_instanceInterruptionBehavior :: Lens.Lens' SpotOptionsRequest (Core.Maybe SpotInstanceInterruptionBehavior)
spotOptionsRequest_instanceInterruptionBehavior = Lens.lens (\SpotOptionsRequest' {instanceInterruptionBehavior} -> instanceInterruptionBehavior) (\s@SpotOptionsRequest' {} a -> s {instanceInterruptionBehavior = a} :: SpotOptionsRequest)

-- | Indicates that the fleet uses a single instance type to launch all Spot
-- Instances in the fleet. Supported only for fleets of type @instant@.
spotOptionsRequest_singleInstanceType :: Lens.Lens' SpotOptionsRequest (Core.Maybe Core.Bool)
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
-- If the allocation strategy is @capacity-optimized@, EC2 Fleet launches
-- instances from Spot Instance pools with optimal capacity for the number
-- of instances that are launching.
spotOptionsRequest_allocationStrategy :: Lens.Lens' SpotOptionsRequest (Core.Maybe SpotAllocationStrategy)
spotOptionsRequest_allocationStrategy = Lens.lens (\SpotOptionsRequest' {allocationStrategy} -> allocationStrategy) (\s@SpotOptionsRequest' {} a -> s {allocationStrategy = a} :: SpotOptionsRequest)

-- | The maximum amount per hour for Spot Instances that you\'re willing to
-- pay.
spotOptionsRequest_maxTotalPrice :: Lens.Lens' SpotOptionsRequest (Core.Maybe Core.Text)
spotOptionsRequest_maxTotalPrice = Lens.lens (\SpotOptionsRequest' {maxTotalPrice} -> maxTotalPrice) (\s@SpotOptionsRequest' {} a -> s {maxTotalPrice = a} :: SpotOptionsRequest)

-- | The number of Spot pools across which to allocate your target Spot
-- capacity. Valid only when Spot __AllocationStrategy__ is set to
-- @lowest-price@. EC2 Fleet selects the cheapest Spot pools and evenly
-- allocates your target Spot capacity across the number of Spot pools that
-- you specify.
spotOptionsRequest_instancePoolsToUseCount :: Lens.Lens' SpotOptionsRequest (Core.Maybe Core.Int)
spotOptionsRequest_instancePoolsToUseCount = Lens.lens (\SpotOptionsRequest' {instancePoolsToUseCount} -> instancePoolsToUseCount) (\s@SpotOptionsRequest' {} a -> s {instancePoolsToUseCount = a} :: SpotOptionsRequest)

-- | Indicates that the fleet launches all Spot Instances into a single
-- Availability Zone. Supported only for fleets of type @instant@.
spotOptionsRequest_singleAvailabilityZone :: Lens.Lens' SpotOptionsRequest (Core.Maybe Core.Bool)
spotOptionsRequest_singleAvailabilityZone = Lens.lens (\SpotOptionsRequest' {singleAvailabilityZone} -> singleAvailabilityZone) (\s@SpotOptionsRequest' {} a -> s {singleAvailabilityZone = a} :: SpotOptionsRequest)

instance Core.Hashable SpotOptionsRequest

instance Core.NFData SpotOptionsRequest

instance Core.ToQuery SpotOptionsRequest where
  toQuery SpotOptionsRequest' {..} =
    Core.mconcat
      [ "MinTargetCapacity" Core.=: minTargetCapacity,
        "MaintenanceStrategies"
          Core.=: maintenanceStrategies,
        "InstanceInterruptionBehavior"
          Core.=: instanceInterruptionBehavior,
        "SingleInstanceType" Core.=: singleInstanceType,
        "AllocationStrategy" Core.=: allocationStrategy,
        "MaxTotalPrice" Core.=: maxTotalPrice,
        "InstancePoolsToUseCount"
          Core.=: instancePoolsToUseCount,
        "SingleAvailabilityZone"
          Core.=: singleAvailabilityZone
      ]
