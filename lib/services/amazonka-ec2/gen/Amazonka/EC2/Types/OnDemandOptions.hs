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
-- Module      : Amazonka.EC2.Types.OnDemandOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.OnDemandOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.CapacityReservationOptions
import Amazonka.EC2.Types.FleetOnDemandAllocationStrategy
import qualified Amazonka.Prelude as Prelude

-- | Describes the configuration of On-Demand Instances in an EC2 Fleet.
--
-- /See:/ 'newOnDemandOptions' smart constructor.
data OnDemandOptions = OnDemandOptions'
  { -- | Indicates that the fleet uses a single instance type to launch all
    -- On-Demand Instances in the fleet.
    --
    -- Supported only for fleets of type @instant@.
    singleInstanceType :: Prelude.Maybe Prelude.Bool,
    -- | Indicates that the fleet launches all On-Demand Instances into a single
    -- Availability Zone.
    --
    -- Supported only for fleets of type @instant@.
    singleAvailabilityZone :: Prelude.Maybe Prelude.Bool,
    -- | The minimum target capacity for On-Demand Instances in the fleet. If the
    -- minimum target capacity is not reached, the fleet launches no instances.
    --
    -- Supported only for fleets of type @instant@.
    --
    -- At least one of the following must be specified:
    -- @SingleAvailabilityZone@ | @SingleInstanceType@
    minTargetCapacity :: Prelude.Maybe Prelude.Int,
    -- | The strategy for using unused Capacity Reservations for fulfilling
    -- On-Demand capacity.
    --
    -- Supported only for fleets of type @instant@.
    capacityReservationOptions :: Prelude.Maybe CapacityReservationOptions,
    -- | The strategy that determines the order of the launch template overrides
    -- to use in fulfilling On-Demand capacity.
    --
    -- @lowest-price@ - EC2 Fleet uses price to determine the order, launching
    -- the lowest price first.
    --
    -- @prioritized@ - EC2 Fleet uses the priority that you assigned to each
    -- launch template override, launching the highest priority first.
    --
    -- Default: @lowest-price@
    allocationStrategy :: Prelude.Maybe FleetOnDemandAllocationStrategy,
    -- | The maximum amount per hour for On-Demand Instances that you\'re willing
    -- to pay.
    maxTotalPrice :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OnDemandOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'singleInstanceType', 'onDemandOptions_singleInstanceType' - Indicates that the fleet uses a single instance type to launch all
-- On-Demand Instances in the fleet.
--
-- Supported only for fleets of type @instant@.
--
-- 'singleAvailabilityZone', 'onDemandOptions_singleAvailabilityZone' - Indicates that the fleet launches all On-Demand Instances into a single
-- Availability Zone.
--
-- Supported only for fleets of type @instant@.
--
-- 'minTargetCapacity', 'onDemandOptions_minTargetCapacity' - The minimum target capacity for On-Demand Instances in the fleet. If the
-- minimum target capacity is not reached, the fleet launches no instances.
--
-- Supported only for fleets of type @instant@.
--
-- At least one of the following must be specified:
-- @SingleAvailabilityZone@ | @SingleInstanceType@
--
-- 'capacityReservationOptions', 'onDemandOptions_capacityReservationOptions' - The strategy for using unused Capacity Reservations for fulfilling
-- On-Demand capacity.
--
-- Supported only for fleets of type @instant@.
--
-- 'allocationStrategy', 'onDemandOptions_allocationStrategy' - The strategy that determines the order of the launch template overrides
-- to use in fulfilling On-Demand capacity.
--
-- @lowest-price@ - EC2 Fleet uses price to determine the order, launching
-- the lowest price first.
--
-- @prioritized@ - EC2 Fleet uses the priority that you assigned to each
-- launch template override, launching the highest priority first.
--
-- Default: @lowest-price@
--
-- 'maxTotalPrice', 'onDemandOptions_maxTotalPrice' - The maximum amount per hour for On-Demand Instances that you\'re willing
-- to pay.
newOnDemandOptions ::
  OnDemandOptions
newOnDemandOptions =
  OnDemandOptions'
    { singleInstanceType =
        Prelude.Nothing,
      singleAvailabilityZone = Prelude.Nothing,
      minTargetCapacity = Prelude.Nothing,
      capacityReservationOptions = Prelude.Nothing,
      allocationStrategy = Prelude.Nothing,
      maxTotalPrice = Prelude.Nothing
    }

-- | Indicates that the fleet uses a single instance type to launch all
-- On-Demand Instances in the fleet.
--
-- Supported only for fleets of type @instant@.
onDemandOptions_singleInstanceType :: Lens.Lens' OnDemandOptions (Prelude.Maybe Prelude.Bool)
onDemandOptions_singleInstanceType = Lens.lens (\OnDemandOptions' {singleInstanceType} -> singleInstanceType) (\s@OnDemandOptions' {} a -> s {singleInstanceType = a} :: OnDemandOptions)

-- | Indicates that the fleet launches all On-Demand Instances into a single
-- Availability Zone.
--
-- Supported only for fleets of type @instant@.
onDemandOptions_singleAvailabilityZone :: Lens.Lens' OnDemandOptions (Prelude.Maybe Prelude.Bool)
onDemandOptions_singleAvailabilityZone = Lens.lens (\OnDemandOptions' {singleAvailabilityZone} -> singleAvailabilityZone) (\s@OnDemandOptions' {} a -> s {singleAvailabilityZone = a} :: OnDemandOptions)

-- | The minimum target capacity for On-Demand Instances in the fleet. If the
-- minimum target capacity is not reached, the fleet launches no instances.
--
-- Supported only for fleets of type @instant@.
--
-- At least one of the following must be specified:
-- @SingleAvailabilityZone@ | @SingleInstanceType@
onDemandOptions_minTargetCapacity :: Lens.Lens' OnDemandOptions (Prelude.Maybe Prelude.Int)
onDemandOptions_minTargetCapacity = Lens.lens (\OnDemandOptions' {minTargetCapacity} -> minTargetCapacity) (\s@OnDemandOptions' {} a -> s {minTargetCapacity = a} :: OnDemandOptions)

-- | The strategy for using unused Capacity Reservations for fulfilling
-- On-Demand capacity.
--
-- Supported only for fleets of type @instant@.
onDemandOptions_capacityReservationOptions :: Lens.Lens' OnDemandOptions (Prelude.Maybe CapacityReservationOptions)
onDemandOptions_capacityReservationOptions = Lens.lens (\OnDemandOptions' {capacityReservationOptions} -> capacityReservationOptions) (\s@OnDemandOptions' {} a -> s {capacityReservationOptions = a} :: OnDemandOptions)

-- | The strategy that determines the order of the launch template overrides
-- to use in fulfilling On-Demand capacity.
--
-- @lowest-price@ - EC2 Fleet uses price to determine the order, launching
-- the lowest price first.
--
-- @prioritized@ - EC2 Fleet uses the priority that you assigned to each
-- launch template override, launching the highest priority first.
--
-- Default: @lowest-price@
onDemandOptions_allocationStrategy :: Lens.Lens' OnDemandOptions (Prelude.Maybe FleetOnDemandAllocationStrategy)
onDemandOptions_allocationStrategy = Lens.lens (\OnDemandOptions' {allocationStrategy} -> allocationStrategy) (\s@OnDemandOptions' {} a -> s {allocationStrategy = a} :: OnDemandOptions)

-- | The maximum amount per hour for On-Demand Instances that you\'re willing
-- to pay.
onDemandOptions_maxTotalPrice :: Lens.Lens' OnDemandOptions (Prelude.Maybe Prelude.Text)
onDemandOptions_maxTotalPrice = Lens.lens (\OnDemandOptions' {maxTotalPrice} -> maxTotalPrice) (\s@OnDemandOptions' {} a -> s {maxTotalPrice = a} :: OnDemandOptions)

instance Core.FromXML OnDemandOptions where
  parseXML x =
    OnDemandOptions'
      Prelude.<$> (x Core..@? "singleInstanceType")
      Prelude.<*> (x Core..@? "singleAvailabilityZone")
      Prelude.<*> (x Core..@? "minTargetCapacity")
      Prelude.<*> (x Core..@? "capacityReservationOptions")
      Prelude.<*> (x Core..@? "allocationStrategy")
      Prelude.<*> (x Core..@? "maxTotalPrice")

instance Prelude.Hashable OnDemandOptions where
  hashWithSalt _salt OnDemandOptions' {..} =
    _salt `Prelude.hashWithSalt` singleInstanceType
      `Prelude.hashWithSalt` singleAvailabilityZone
      `Prelude.hashWithSalt` minTargetCapacity
      `Prelude.hashWithSalt` capacityReservationOptions
      `Prelude.hashWithSalt` allocationStrategy
      `Prelude.hashWithSalt` maxTotalPrice

instance Prelude.NFData OnDemandOptions where
  rnf OnDemandOptions' {..} =
    Prelude.rnf singleInstanceType
      `Prelude.seq` Prelude.rnf singleAvailabilityZone
      `Prelude.seq` Prelude.rnf minTargetCapacity
      `Prelude.seq` Prelude.rnf capacityReservationOptions
      `Prelude.seq` Prelude.rnf allocationStrategy
      `Prelude.seq` Prelude.rnf maxTotalPrice
