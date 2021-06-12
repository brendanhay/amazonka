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
-- Module      : Network.AWS.EC2.Types.OnDemandOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.OnDemandOptions where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.CapacityReservationOptions
import Network.AWS.EC2.Types.FleetOnDemandAllocationStrategy
import qualified Network.AWS.Lens as Lens

-- | Describes the configuration of On-Demand Instances in an EC2 Fleet.
--
-- /See:/ 'newOnDemandOptions' smart constructor.
data OnDemandOptions = OnDemandOptions'
  { -- | The minimum target capacity for On-Demand Instances in the fleet. If the
    -- minimum target capacity is not reached, the fleet launches no instances.
    minTargetCapacity :: Core.Maybe Core.Int,
    -- | The strategy for using unused Capacity Reservations for fulfilling
    -- On-Demand capacity. Supported only for fleets of type @instant@.
    capacityReservationOptions :: Core.Maybe CapacityReservationOptions,
    -- | Indicates that the fleet uses a single instance type to launch all
    -- On-Demand Instances in the fleet. Supported only for fleets of type
    -- @instant@.
    singleInstanceType :: Core.Maybe Core.Bool,
    -- | The order of the launch template overrides to use in fulfilling
    -- On-Demand capacity. If you specify @lowest-price@, EC2 Fleet uses price
    -- to determine the order, launching the lowest price first. If you specify
    -- @prioritized@, EC2 Fleet uses the priority that you assigned to each
    -- launch template override, launching the highest priority first. If you
    -- do not specify a value, EC2 Fleet defaults to @lowest-price@.
    allocationStrategy :: Core.Maybe FleetOnDemandAllocationStrategy,
    -- | The maximum amount per hour for On-Demand Instances that you\'re willing
    -- to pay.
    maxTotalPrice :: Core.Maybe Core.Text,
    -- | Indicates that the fleet launches all On-Demand Instances into a single
    -- Availability Zone. Supported only for fleets of type @instant@.
    singleAvailabilityZone :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OnDemandOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'minTargetCapacity', 'onDemandOptions_minTargetCapacity' - The minimum target capacity for On-Demand Instances in the fleet. If the
-- minimum target capacity is not reached, the fleet launches no instances.
--
-- 'capacityReservationOptions', 'onDemandOptions_capacityReservationOptions' - The strategy for using unused Capacity Reservations for fulfilling
-- On-Demand capacity. Supported only for fleets of type @instant@.
--
-- 'singleInstanceType', 'onDemandOptions_singleInstanceType' - Indicates that the fleet uses a single instance type to launch all
-- On-Demand Instances in the fleet. Supported only for fleets of type
-- @instant@.
--
-- 'allocationStrategy', 'onDemandOptions_allocationStrategy' - The order of the launch template overrides to use in fulfilling
-- On-Demand capacity. If you specify @lowest-price@, EC2 Fleet uses price
-- to determine the order, launching the lowest price first. If you specify
-- @prioritized@, EC2 Fleet uses the priority that you assigned to each
-- launch template override, launching the highest priority first. If you
-- do not specify a value, EC2 Fleet defaults to @lowest-price@.
--
-- 'maxTotalPrice', 'onDemandOptions_maxTotalPrice' - The maximum amount per hour for On-Demand Instances that you\'re willing
-- to pay.
--
-- 'singleAvailabilityZone', 'onDemandOptions_singleAvailabilityZone' - Indicates that the fleet launches all On-Demand Instances into a single
-- Availability Zone. Supported only for fleets of type @instant@.
newOnDemandOptions ::
  OnDemandOptions
newOnDemandOptions =
  OnDemandOptions'
    { minTargetCapacity = Core.Nothing,
      capacityReservationOptions = Core.Nothing,
      singleInstanceType = Core.Nothing,
      allocationStrategy = Core.Nothing,
      maxTotalPrice = Core.Nothing,
      singleAvailabilityZone = Core.Nothing
    }

-- | The minimum target capacity for On-Demand Instances in the fleet. If the
-- minimum target capacity is not reached, the fleet launches no instances.
onDemandOptions_minTargetCapacity :: Lens.Lens' OnDemandOptions (Core.Maybe Core.Int)
onDemandOptions_minTargetCapacity = Lens.lens (\OnDemandOptions' {minTargetCapacity} -> minTargetCapacity) (\s@OnDemandOptions' {} a -> s {minTargetCapacity = a} :: OnDemandOptions)

-- | The strategy for using unused Capacity Reservations for fulfilling
-- On-Demand capacity. Supported only for fleets of type @instant@.
onDemandOptions_capacityReservationOptions :: Lens.Lens' OnDemandOptions (Core.Maybe CapacityReservationOptions)
onDemandOptions_capacityReservationOptions = Lens.lens (\OnDemandOptions' {capacityReservationOptions} -> capacityReservationOptions) (\s@OnDemandOptions' {} a -> s {capacityReservationOptions = a} :: OnDemandOptions)

-- | Indicates that the fleet uses a single instance type to launch all
-- On-Demand Instances in the fleet. Supported only for fleets of type
-- @instant@.
onDemandOptions_singleInstanceType :: Lens.Lens' OnDemandOptions (Core.Maybe Core.Bool)
onDemandOptions_singleInstanceType = Lens.lens (\OnDemandOptions' {singleInstanceType} -> singleInstanceType) (\s@OnDemandOptions' {} a -> s {singleInstanceType = a} :: OnDemandOptions)

-- | The order of the launch template overrides to use in fulfilling
-- On-Demand capacity. If you specify @lowest-price@, EC2 Fleet uses price
-- to determine the order, launching the lowest price first. If you specify
-- @prioritized@, EC2 Fleet uses the priority that you assigned to each
-- launch template override, launching the highest priority first. If you
-- do not specify a value, EC2 Fleet defaults to @lowest-price@.
onDemandOptions_allocationStrategy :: Lens.Lens' OnDemandOptions (Core.Maybe FleetOnDemandAllocationStrategy)
onDemandOptions_allocationStrategy = Lens.lens (\OnDemandOptions' {allocationStrategy} -> allocationStrategy) (\s@OnDemandOptions' {} a -> s {allocationStrategy = a} :: OnDemandOptions)

-- | The maximum amount per hour for On-Demand Instances that you\'re willing
-- to pay.
onDemandOptions_maxTotalPrice :: Lens.Lens' OnDemandOptions (Core.Maybe Core.Text)
onDemandOptions_maxTotalPrice = Lens.lens (\OnDemandOptions' {maxTotalPrice} -> maxTotalPrice) (\s@OnDemandOptions' {} a -> s {maxTotalPrice = a} :: OnDemandOptions)

-- | Indicates that the fleet launches all On-Demand Instances into a single
-- Availability Zone. Supported only for fleets of type @instant@.
onDemandOptions_singleAvailabilityZone :: Lens.Lens' OnDemandOptions (Core.Maybe Core.Bool)
onDemandOptions_singleAvailabilityZone = Lens.lens (\OnDemandOptions' {singleAvailabilityZone} -> singleAvailabilityZone) (\s@OnDemandOptions' {} a -> s {singleAvailabilityZone = a} :: OnDemandOptions)

instance Core.FromXML OnDemandOptions where
  parseXML x =
    OnDemandOptions'
      Core.<$> (x Core..@? "minTargetCapacity")
      Core.<*> (x Core..@? "capacityReservationOptions")
      Core.<*> (x Core..@? "singleInstanceType")
      Core.<*> (x Core..@? "allocationStrategy")
      Core.<*> (x Core..@? "maxTotalPrice")
      Core.<*> (x Core..@? "singleAvailabilityZone")

instance Core.Hashable OnDemandOptions

instance Core.NFData OnDemandOptions
