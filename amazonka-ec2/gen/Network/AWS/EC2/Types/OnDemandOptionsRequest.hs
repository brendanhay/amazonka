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
-- Module      : Network.AWS.EC2.Types.OnDemandOptionsRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.OnDemandOptionsRequest where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.CapacityReservationOptionsRequest
import Network.AWS.EC2.Types.FleetOnDemandAllocationStrategy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the configuration of On-Demand Instances in an EC2 Fleet.
--
-- /See:/ 'newOnDemandOptionsRequest' smart constructor.
data OnDemandOptionsRequest = OnDemandOptionsRequest'
  { -- | The minimum target capacity for On-Demand Instances in the fleet. If the
    -- minimum target capacity is not reached, the fleet launches no instances.
    minTargetCapacity :: Prelude.Maybe Prelude.Int,
    -- | The strategy for using unused Capacity Reservations for fulfilling
    -- On-Demand capacity. Supported only for fleets of type @instant@.
    capacityReservationOptions :: Prelude.Maybe CapacityReservationOptionsRequest,
    -- | Indicates that the fleet uses a single instance type to launch all
    -- On-Demand Instances in the fleet. Supported only for fleets of type
    -- @instant@.
    singleInstanceType :: Prelude.Maybe Prelude.Bool,
    -- | The order of the launch template overrides to use in fulfilling
    -- On-Demand capacity. If you specify @lowest-price@, EC2 Fleet uses price
    -- to determine the order, launching the lowest price first. If you specify
    -- @prioritized@, EC2 Fleet uses the priority that you assigned to each
    -- launch template override, launching the highest priority first. If you
    -- do not specify a value, EC2 Fleet defaults to @lowest-price@.
    allocationStrategy :: Prelude.Maybe FleetOnDemandAllocationStrategy,
    -- | The maximum amount per hour for On-Demand Instances that you\'re willing
    -- to pay.
    maxTotalPrice :: Prelude.Maybe Prelude.Text,
    -- | Indicates that the fleet launches all On-Demand Instances into a single
    -- Availability Zone. Supported only for fleets of type @instant@.
    singleAvailabilityZone :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'OnDemandOptionsRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'minTargetCapacity', 'onDemandOptionsRequest_minTargetCapacity' - The minimum target capacity for On-Demand Instances in the fleet. If the
-- minimum target capacity is not reached, the fleet launches no instances.
--
-- 'capacityReservationOptions', 'onDemandOptionsRequest_capacityReservationOptions' - The strategy for using unused Capacity Reservations for fulfilling
-- On-Demand capacity. Supported only for fleets of type @instant@.
--
-- 'singleInstanceType', 'onDemandOptionsRequest_singleInstanceType' - Indicates that the fleet uses a single instance type to launch all
-- On-Demand Instances in the fleet. Supported only for fleets of type
-- @instant@.
--
-- 'allocationStrategy', 'onDemandOptionsRequest_allocationStrategy' - The order of the launch template overrides to use in fulfilling
-- On-Demand capacity. If you specify @lowest-price@, EC2 Fleet uses price
-- to determine the order, launching the lowest price first. If you specify
-- @prioritized@, EC2 Fleet uses the priority that you assigned to each
-- launch template override, launching the highest priority first. If you
-- do not specify a value, EC2 Fleet defaults to @lowest-price@.
--
-- 'maxTotalPrice', 'onDemandOptionsRequest_maxTotalPrice' - The maximum amount per hour for On-Demand Instances that you\'re willing
-- to pay.
--
-- 'singleAvailabilityZone', 'onDemandOptionsRequest_singleAvailabilityZone' - Indicates that the fleet launches all On-Demand Instances into a single
-- Availability Zone. Supported only for fleets of type @instant@.
newOnDemandOptionsRequest ::
  OnDemandOptionsRequest
newOnDemandOptionsRequest =
  OnDemandOptionsRequest'
    { minTargetCapacity =
        Prelude.Nothing,
      capacityReservationOptions = Prelude.Nothing,
      singleInstanceType = Prelude.Nothing,
      allocationStrategy = Prelude.Nothing,
      maxTotalPrice = Prelude.Nothing,
      singleAvailabilityZone = Prelude.Nothing
    }

-- | The minimum target capacity for On-Demand Instances in the fleet. If the
-- minimum target capacity is not reached, the fleet launches no instances.
onDemandOptionsRequest_minTargetCapacity :: Lens.Lens' OnDemandOptionsRequest (Prelude.Maybe Prelude.Int)
onDemandOptionsRequest_minTargetCapacity = Lens.lens (\OnDemandOptionsRequest' {minTargetCapacity} -> minTargetCapacity) (\s@OnDemandOptionsRequest' {} a -> s {minTargetCapacity = a} :: OnDemandOptionsRequest)

-- | The strategy for using unused Capacity Reservations for fulfilling
-- On-Demand capacity. Supported only for fleets of type @instant@.
onDemandOptionsRequest_capacityReservationOptions :: Lens.Lens' OnDemandOptionsRequest (Prelude.Maybe CapacityReservationOptionsRequest)
onDemandOptionsRequest_capacityReservationOptions = Lens.lens (\OnDemandOptionsRequest' {capacityReservationOptions} -> capacityReservationOptions) (\s@OnDemandOptionsRequest' {} a -> s {capacityReservationOptions = a} :: OnDemandOptionsRequest)

-- | Indicates that the fleet uses a single instance type to launch all
-- On-Demand Instances in the fleet. Supported only for fleets of type
-- @instant@.
onDemandOptionsRequest_singleInstanceType :: Lens.Lens' OnDemandOptionsRequest (Prelude.Maybe Prelude.Bool)
onDemandOptionsRequest_singleInstanceType = Lens.lens (\OnDemandOptionsRequest' {singleInstanceType} -> singleInstanceType) (\s@OnDemandOptionsRequest' {} a -> s {singleInstanceType = a} :: OnDemandOptionsRequest)

-- | The order of the launch template overrides to use in fulfilling
-- On-Demand capacity. If you specify @lowest-price@, EC2 Fleet uses price
-- to determine the order, launching the lowest price first. If you specify
-- @prioritized@, EC2 Fleet uses the priority that you assigned to each
-- launch template override, launching the highest priority first. If you
-- do not specify a value, EC2 Fleet defaults to @lowest-price@.
onDemandOptionsRequest_allocationStrategy :: Lens.Lens' OnDemandOptionsRequest (Prelude.Maybe FleetOnDemandAllocationStrategy)
onDemandOptionsRequest_allocationStrategy = Lens.lens (\OnDemandOptionsRequest' {allocationStrategy} -> allocationStrategy) (\s@OnDemandOptionsRequest' {} a -> s {allocationStrategy = a} :: OnDemandOptionsRequest)

-- | The maximum amount per hour for On-Demand Instances that you\'re willing
-- to pay.
onDemandOptionsRequest_maxTotalPrice :: Lens.Lens' OnDemandOptionsRequest (Prelude.Maybe Prelude.Text)
onDemandOptionsRequest_maxTotalPrice = Lens.lens (\OnDemandOptionsRequest' {maxTotalPrice} -> maxTotalPrice) (\s@OnDemandOptionsRequest' {} a -> s {maxTotalPrice = a} :: OnDemandOptionsRequest)

-- | Indicates that the fleet launches all On-Demand Instances into a single
-- Availability Zone. Supported only for fleets of type @instant@.
onDemandOptionsRequest_singleAvailabilityZone :: Lens.Lens' OnDemandOptionsRequest (Prelude.Maybe Prelude.Bool)
onDemandOptionsRequest_singleAvailabilityZone = Lens.lens (\OnDemandOptionsRequest' {singleAvailabilityZone} -> singleAvailabilityZone) (\s@OnDemandOptionsRequest' {} a -> s {singleAvailabilityZone = a} :: OnDemandOptionsRequest)

instance Prelude.Hashable OnDemandOptionsRequest

instance Prelude.NFData OnDemandOptionsRequest

instance Prelude.ToQuery OnDemandOptionsRequest where
  toQuery OnDemandOptionsRequest' {..} =
    Prelude.mconcat
      [ "MinTargetCapacity" Prelude.=: minTargetCapacity,
        "CapacityReservationOptions"
          Prelude.=: capacityReservationOptions,
        "SingleInstanceType" Prelude.=: singleInstanceType,
        "AllocationStrategy" Prelude.=: allocationStrategy,
        "MaxTotalPrice" Prelude.=: maxTotalPrice,
        "SingleAvailabilityZone"
          Prelude.=: singleAvailabilityZone
      ]
