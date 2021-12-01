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
-- Module      : Amazonka.EC2.Types.SpotOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.SpotOptions where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.FleetSpotMaintenanceStrategies
import Amazonka.EC2.Types.SpotAllocationStrategy
import Amazonka.EC2.Types.SpotInstanceInterruptionBehavior
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the configuration of Spot Instances in an EC2 Fleet.
--
-- /See:/ 'newSpotOptions' smart constructor.
data SpotOptions = SpotOptions'
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
    -- capacity. Valid only when __AllocationStrategy__ is set to
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
    -- | The strategies for managing your workloads on your Spot Instances that
    -- will be interrupted. Currently only the capacity rebalance strategy is
    -- available.
    maintenanceStrategies :: Prelude.Maybe FleetSpotMaintenanceStrategies,
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
-- Create a value of 'SpotOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceInterruptionBehavior', 'spotOptions_instanceInterruptionBehavior' - The behavior when a Spot Instance is interrupted. The default is
-- @terminate@.
--
-- 'singleAvailabilityZone', 'spotOptions_singleAvailabilityZone' - Indicates that the fleet launches all Spot Instances into a single
-- Availability Zone. Supported only for fleets of type @instant@.
--
-- 'maxTotalPrice', 'spotOptions_maxTotalPrice' - The maximum amount per hour for Spot Instances that you\'re willing to
-- pay.
--
-- 'minTargetCapacity', 'spotOptions_minTargetCapacity' - The minimum target capacity for Spot Instances in the fleet. If the
-- minimum target capacity is not reached, the fleet launches no instances.
--
-- 'instancePoolsToUseCount', 'spotOptions_instancePoolsToUseCount' - The number of Spot pools across which to allocate your target Spot
-- capacity. Valid only when __AllocationStrategy__ is set to
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
-- 'maintenanceStrategies', 'spotOptions_maintenanceStrategies' - The strategies for managing your workloads on your Spot Instances that
-- will be interrupted. Currently only the capacity rebalance strategy is
-- available.
--
-- 'singleInstanceType', 'spotOptions_singleInstanceType' - Indicates that the fleet uses a single instance type to launch all Spot
-- Instances in the fleet. Supported only for fleets of type @instant@.
--
-- 'allocationStrategy', 'spotOptions_allocationStrategy' - Indicates how to allocate the target Spot Instance capacity across the
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
newSpotOptions ::
  SpotOptions
newSpotOptions =
  SpotOptions'
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
spotOptions_instanceInterruptionBehavior :: Lens.Lens' SpotOptions (Prelude.Maybe SpotInstanceInterruptionBehavior)
spotOptions_instanceInterruptionBehavior = Lens.lens (\SpotOptions' {instanceInterruptionBehavior} -> instanceInterruptionBehavior) (\s@SpotOptions' {} a -> s {instanceInterruptionBehavior = a} :: SpotOptions)

-- | Indicates that the fleet launches all Spot Instances into a single
-- Availability Zone. Supported only for fleets of type @instant@.
spotOptions_singleAvailabilityZone :: Lens.Lens' SpotOptions (Prelude.Maybe Prelude.Bool)
spotOptions_singleAvailabilityZone = Lens.lens (\SpotOptions' {singleAvailabilityZone} -> singleAvailabilityZone) (\s@SpotOptions' {} a -> s {singleAvailabilityZone = a} :: SpotOptions)

-- | The maximum amount per hour for Spot Instances that you\'re willing to
-- pay.
spotOptions_maxTotalPrice :: Lens.Lens' SpotOptions (Prelude.Maybe Prelude.Text)
spotOptions_maxTotalPrice = Lens.lens (\SpotOptions' {maxTotalPrice} -> maxTotalPrice) (\s@SpotOptions' {} a -> s {maxTotalPrice = a} :: SpotOptions)

-- | The minimum target capacity for Spot Instances in the fleet. If the
-- minimum target capacity is not reached, the fleet launches no instances.
spotOptions_minTargetCapacity :: Lens.Lens' SpotOptions (Prelude.Maybe Prelude.Int)
spotOptions_minTargetCapacity = Lens.lens (\SpotOptions' {minTargetCapacity} -> minTargetCapacity) (\s@SpotOptions' {} a -> s {minTargetCapacity = a} :: SpotOptions)

-- | The number of Spot pools across which to allocate your target Spot
-- capacity. Valid only when __AllocationStrategy__ is set to
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
spotOptions_instancePoolsToUseCount :: Lens.Lens' SpotOptions (Prelude.Maybe Prelude.Int)
spotOptions_instancePoolsToUseCount = Lens.lens (\SpotOptions' {instancePoolsToUseCount} -> instancePoolsToUseCount) (\s@SpotOptions' {} a -> s {instancePoolsToUseCount = a} :: SpotOptions)

-- | The strategies for managing your workloads on your Spot Instances that
-- will be interrupted. Currently only the capacity rebalance strategy is
-- available.
spotOptions_maintenanceStrategies :: Lens.Lens' SpotOptions (Prelude.Maybe FleetSpotMaintenanceStrategies)
spotOptions_maintenanceStrategies = Lens.lens (\SpotOptions' {maintenanceStrategies} -> maintenanceStrategies) (\s@SpotOptions' {} a -> s {maintenanceStrategies = a} :: SpotOptions)

-- | Indicates that the fleet uses a single instance type to launch all Spot
-- Instances in the fleet. Supported only for fleets of type @instant@.
spotOptions_singleInstanceType :: Lens.Lens' SpotOptions (Prelude.Maybe Prelude.Bool)
spotOptions_singleInstanceType = Lens.lens (\SpotOptions' {singleInstanceType} -> singleInstanceType) (\s@SpotOptions' {} a -> s {singleInstanceType = a} :: SpotOptions)

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
spotOptions_allocationStrategy :: Lens.Lens' SpotOptions (Prelude.Maybe SpotAllocationStrategy)
spotOptions_allocationStrategy = Lens.lens (\SpotOptions' {allocationStrategy} -> allocationStrategy) (\s@SpotOptions' {} a -> s {allocationStrategy = a} :: SpotOptions)

instance Core.FromXML SpotOptions where
  parseXML x =
    SpotOptions'
      Prelude.<$> (x Core..@? "instanceInterruptionBehavior")
      Prelude.<*> (x Core..@? "singleAvailabilityZone")
      Prelude.<*> (x Core..@? "maxTotalPrice")
      Prelude.<*> (x Core..@? "minTargetCapacity")
      Prelude.<*> (x Core..@? "instancePoolsToUseCount")
      Prelude.<*> (x Core..@? "maintenanceStrategies")
      Prelude.<*> (x Core..@? "singleInstanceType")
      Prelude.<*> (x Core..@? "allocationStrategy")

instance Prelude.Hashable SpotOptions where
  hashWithSalt salt' SpotOptions' {..} =
    salt' `Prelude.hashWithSalt` allocationStrategy
      `Prelude.hashWithSalt` singleInstanceType
      `Prelude.hashWithSalt` maintenanceStrategies
      `Prelude.hashWithSalt` instancePoolsToUseCount
      `Prelude.hashWithSalt` minTargetCapacity
      `Prelude.hashWithSalt` maxTotalPrice
      `Prelude.hashWithSalt` singleAvailabilityZone
      `Prelude.hashWithSalt` instanceInterruptionBehavior

instance Prelude.NFData SpotOptions where
  rnf SpotOptions' {..} =
    Prelude.rnf instanceInterruptionBehavior
      `Prelude.seq` Prelude.rnf allocationStrategy
      `Prelude.seq` Prelude.rnf singleInstanceType
      `Prelude.seq` Prelude.rnf maintenanceStrategies
      `Prelude.seq` Prelude.rnf instancePoolsToUseCount
      `Prelude.seq` Prelude.rnf minTargetCapacity
      `Prelude.seq` Prelude.rnf maxTotalPrice
      `Prelude.seq` Prelude.rnf singleAvailabilityZone
