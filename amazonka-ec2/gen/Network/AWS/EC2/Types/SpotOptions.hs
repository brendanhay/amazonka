{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.Types.SpotOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SpotOptions where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.FleetSpotMaintenanceStrategies
import Network.AWS.EC2.Types.SpotAllocationStrategy
import Network.AWS.EC2.Types.SpotInstanceInterruptionBehavior
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the configuration of Spot Instances in an EC2 Fleet.
--
-- /See:/ 'newSpotOptions' smart constructor.
data SpotOptions = SpotOptions'
  { -- | The minimum target capacity for Spot Instances in the fleet. If the
    -- minimum target capacity is not reached, the fleet launches no instances.
    minTargetCapacity :: Prelude.Maybe Prelude.Int,
    -- | The strategies for managing your workloads on your Spot Instances that
    -- will be interrupted. Currently only the capacity rebalance strategy is
    -- available.
    maintenanceStrategies :: Prelude.Maybe FleetSpotMaintenanceStrategies,
    -- | The behavior when a Spot Instance is interrupted. The default is
    -- @terminate@.
    instanceInterruptionBehavior :: Prelude.Maybe SpotInstanceInterruptionBehavior,
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
    -- If the allocation strategy is @capacity-optimized@, EC2 Fleet launches
    -- instances from Spot Instance pools with optimal capacity for the number
    -- of instances that are launching.
    allocationStrategy :: Prelude.Maybe SpotAllocationStrategy,
    -- | The maximum amount per hour for Spot Instances that you\'re willing to
    -- pay.
    maxTotalPrice :: Prelude.Maybe Prelude.Text,
    -- | The number of Spot pools across which to allocate your target Spot
    -- capacity. Valid only when __AllocationStrategy__ is set to
    -- @lowest-price@. EC2 Fleet selects the cheapest Spot pools and evenly
    -- allocates your target Spot capacity across the number of Spot pools that
    -- you specify.
    instancePoolsToUseCount :: Prelude.Maybe Prelude.Int,
    -- | Indicates that the fleet launches all Spot Instances into a single
    -- Availability Zone. Supported only for fleets of type @instant@.
    singleAvailabilityZone :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SpotOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'minTargetCapacity', 'spotOptions_minTargetCapacity' - The minimum target capacity for Spot Instances in the fleet. If the
-- minimum target capacity is not reached, the fleet launches no instances.
--
-- 'maintenanceStrategies', 'spotOptions_maintenanceStrategies' - The strategies for managing your workloads on your Spot Instances that
-- will be interrupted. Currently only the capacity rebalance strategy is
-- available.
--
-- 'instanceInterruptionBehavior', 'spotOptions_instanceInterruptionBehavior' - The behavior when a Spot Instance is interrupted. The default is
-- @terminate@.
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
-- If the allocation strategy is @capacity-optimized@, EC2 Fleet launches
-- instances from Spot Instance pools with optimal capacity for the number
-- of instances that are launching.
--
-- 'maxTotalPrice', 'spotOptions_maxTotalPrice' - The maximum amount per hour for Spot Instances that you\'re willing to
-- pay.
--
-- 'instancePoolsToUseCount', 'spotOptions_instancePoolsToUseCount' - The number of Spot pools across which to allocate your target Spot
-- capacity. Valid only when __AllocationStrategy__ is set to
-- @lowest-price@. EC2 Fleet selects the cheapest Spot pools and evenly
-- allocates your target Spot capacity across the number of Spot pools that
-- you specify.
--
-- 'singleAvailabilityZone', 'spotOptions_singleAvailabilityZone' - Indicates that the fleet launches all Spot Instances into a single
-- Availability Zone. Supported only for fleets of type @instant@.
newSpotOptions ::
  SpotOptions
newSpotOptions =
  SpotOptions'
    { minTargetCapacity = Prelude.Nothing,
      maintenanceStrategies = Prelude.Nothing,
      instanceInterruptionBehavior = Prelude.Nothing,
      singleInstanceType = Prelude.Nothing,
      allocationStrategy = Prelude.Nothing,
      maxTotalPrice = Prelude.Nothing,
      instancePoolsToUseCount = Prelude.Nothing,
      singleAvailabilityZone = Prelude.Nothing
    }

-- | The minimum target capacity for Spot Instances in the fleet. If the
-- minimum target capacity is not reached, the fleet launches no instances.
spotOptions_minTargetCapacity :: Lens.Lens' SpotOptions (Prelude.Maybe Prelude.Int)
spotOptions_minTargetCapacity = Lens.lens (\SpotOptions' {minTargetCapacity} -> minTargetCapacity) (\s@SpotOptions' {} a -> s {minTargetCapacity = a} :: SpotOptions)

-- | The strategies for managing your workloads on your Spot Instances that
-- will be interrupted. Currently only the capacity rebalance strategy is
-- available.
spotOptions_maintenanceStrategies :: Lens.Lens' SpotOptions (Prelude.Maybe FleetSpotMaintenanceStrategies)
spotOptions_maintenanceStrategies = Lens.lens (\SpotOptions' {maintenanceStrategies} -> maintenanceStrategies) (\s@SpotOptions' {} a -> s {maintenanceStrategies = a} :: SpotOptions)

-- | The behavior when a Spot Instance is interrupted. The default is
-- @terminate@.
spotOptions_instanceInterruptionBehavior :: Lens.Lens' SpotOptions (Prelude.Maybe SpotInstanceInterruptionBehavior)
spotOptions_instanceInterruptionBehavior = Lens.lens (\SpotOptions' {instanceInterruptionBehavior} -> instanceInterruptionBehavior) (\s@SpotOptions' {} a -> s {instanceInterruptionBehavior = a} :: SpotOptions)

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
-- If the allocation strategy is @capacity-optimized@, EC2 Fleet launches
-- instances from Spot Instance pools with optimal capacity for the number
-- of instances that are launching.
spotOptions_allocationStrategy :: Lens.Lens' SpotOptions (Prelude.Maybe SpotAllocationStrategy)
spotOptions_allocationStrategy = Lens.lens (\SpotOptions' {allocationStrategy} -> allocationStrategy) (\s@SpotOptions' {} a -> s {allocationStrategy = a} :: SpotOptions)

-- | The maximum amount per hour for Spot Instances that you\'re willing to
-- pay.
spotOptions_maxTotalPrice :: Lens.Lens' SpotOptions (Prelude.Maybe Prelude.Text)
spotOptions_maxTotalPrice = Lens.lens (\SpotOptions' {maxTotalPrice} -> maxTotalPrice) (\s@SpotOptions' {} a -> s {maxTotalPrice = a} :: SpotOptions)

-- | The number of Spot pools across which to allocate your target Spot
-- capacity. Valid only when __AllocationStrategy__ is set to
-- @lowest-price@. EC2 Fleet selects the cheapest Spot pools and evenly
-- allocates your target Spot capacity across the number of Spot pools that
-- you specify.
spotOptions_instancePoolsToUseCount :: Lens.Lens' SpotOptions (Prelude.Maybe Prelude.Int)
spotOptions_instancePoolsToUseCount = Lens.lens (\SpotOptions' {instancePoolsToUseCount} -> instancePoolsToUseCount) (\s@SpotOptions' {} a -> s {instancePoolsToUseCount = a} :: SpotOptions)

-- | Indicates that the fleet launches all Spot Instances into a single
-- Availability Zone. Supported only for fleets of type @instant@.
spotOptions_singleAvailabilityZone :: Lens.Lens' SpotOptions (Prelude.Maybe Prelude.Bool)
spotOptions_singleAvailabilityZone = Lens.lens (\SpotOptions' {singleAvailabilityZone} -> singleAvailabilityZone) (\s@SpotOptions' {} a -> s {singleAvailabilityZone = a} :: SpotOptions)

instance Prelude.FromXML SpotOptions where
  parseXML x =
    SpotOptions'
      Prelude.<$> (x Prelude..@? "minTargetCapacity")
      Prelude.<*> (x Prelude..@? "maintenanceStrategies")
      Prelude.<*> (x Prelude..@? "instanceInterruptionBehavior")
      Prelude.<*> (x Prelude..@? "singleInstanceType")
      Prelude.<*> (x Prelude..@? "allocationStrategy")
      Prelude.<*> (x Prelude..@? "maxTotalPrice")
      Prelude.<*> (x Prelude..@? "instancePoolsToUseCount")
      Prelude.<*> (x Prelude..@? "singleAvailabilityZone")

instance Prelude.Hashable SpotOptions

instance Prelude.NFData SpotOptions
