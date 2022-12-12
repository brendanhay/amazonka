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
-- Module      : Amazonka.EC2.Types.CapacityReservationFleet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.CapacityReservationFleet where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.CapacityReservationFleetState
import Amazonka.EC2.Types.FleetCapacityReservation
import Amazonka.EC2.Types.FleetCapacityReservationTenancy
import Amazonka.EC2.Types.FleetInstanceMatchCriteria
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Information about a Capacity Reservation Fleet.
--
-- /See:/ 'newCapacityReservationFleet' smart constructor.
data CapacityReservationFleet = CapacityReservationFleet'
  { -- | The strategy used by the Capacity Reservation Fleet to determine which
    -- of the specified instance types to use. For more information, see For
    -- more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/crfleet-concepts.html#allocation-strategy Allocation strategy>
    -- in the Amazon EC2 User Guide.
    allocationStrategy :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the Capacity Reservation Fleet.
    capacityReservationFleetArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Capacity Reservation Fleet.
    capacityReservationFleetId :: Prelude.Maybe Prelude.Text,
    -- | The date and time at which the Capacity Reservation Fleet was created.
    createTime :: Prelude.Maybe Data.ISO8601,
    -- | The date and time at which the Capacity Reservation Fleet expires.
    endDate :: Prelude.Maybe Data.ISO8601,
    -- | Indicates the type of instance launches that the Capacity Reservation
    -- Fleet accepts. All Capacity Reservations in the Fleet inherit this
    -- instance matching criteria.
    --
    -- Currently, Capacity Reservation Fleets support @open@ instance matching
    -- criteria only. This means that instances that have matching attributes
    -- (instance type, platform, and Availability Zone) run in the Capacity
    -- Reservations automatically. Instances do not need to explicitly target a
    -- Capacity Reservation Fleet to use its reserved capacity.
    instanceMatchCriteria :: Prelude.Maybe FleetInstanceMatchCriteria,
    -- | Information about the instance types for which to reserve the capacity.
    instanceTypeSpecifications :: Prelude.Maybe [FleetCapacityReservation],
    -- | The state of the Capacity Reservation Fleet. Possible states include:
    --
    -- -   @submitted@ - The Capacity Reservation Fleet request has been
    --     submitted and Amazon Elastic Compute Cloud is preparing to create
    --     the Capacity Reservations.
    --
    -- -   @modifying@ - The Capacity Reservation Fleet is being modified. The
    --     Fleet remains in this state until the modification is complete.
    --
    -- -   @active@ - The Capacity Reservation Fleet has fulfilled its total
    --     target capacity and it is attempting to maintain this capacity. The
    --     Fleet remains in this state until it is modified or deleted.
    --
    -- -   @partially_fulfilled@ - The Capacity Reservation Fleet has partially
    --     fulfilled its total target capacity. There is insufficient Amazon
    --     EC2 to fulfill the total target capacity. The Fleet is attempting to
    --     asynchronously fulfill its total target capacity.
    --
    -- -   @expiring@ - The Capacity Reservation Fleet has reach its end date
    --     and it is in the process of expiring. One or more of its Capacity
    --     reservations might still be active.
    --
    -- -   @expired@ - The Capacity Reservation Fleet has reach its end date.
    --     The Fleet and its Capacity Reservations are expired. The Fleet
    --     can\'t create new Capacity Reservations.
    --
    -- -   @cancelling@ - The Capacity Reservation Fleet is in the process of
    --     being cancelled. One or more of its Capacity reservations might
    --     still be active.
    --
    -- -   @cancelled@ - The Capacity Reservation Fleet has been manually
    --     cancelled. The Fleet and its Capacity Reservations are cancelled and
    --     the Fleet can\'t create new Capacity Reservations.
    --
    -- -   @failed@ - The Capacity Reservation Fleet failed to reserve capacity
    --     for the specified instance types.
    state :: Prelude.Maybe CapacityReservationFleetState,
    -- | The tags assigned to the Capacity Reservation Fleet.
    tags :: Prelude.Maybe [Tag],
    -- | The tenancy of the Capacity Reservation Fleet. Tenancies include:
    --
    -- -   @default@ - The Capacity Reservation Fleet is created on hardware
    --     that is shared with other Amazon Web Services accounts.
    --
    -- -   @dedicated@ - The Capacity Reservation Fleet is created on
    --     single-tenant hardware that is dedicated to a single Amazon Web
    --     Services account.
    tenancy :: Prelude.Maybe FleetCapacityReservationTenancy,
    -- | The capacity units that have been fulfilled.
    totalFulfilledCapacity :: Prelude.Maybe Prelude.Double,
    -- | The total number of capacity units for which the Capacity Reservation
    -- Fleet reserves capacity. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/crfleet-concepts.html#target-capacity Total target capacity>
    -- in the Amazon EC2 User Guide.
    totalTargetCapacity :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CapacityReservationFleet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allocationStrategy', 'capacityReservationFleet_allocationStrategy' - The strategy used by the Capacity Reservation Fleet to determine which
-- of the specified instance types to use. For more information, see For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/crfleet-concepts.html#allocation-strategy Allocation strategy>
-- in the Amazon EC2 User Guide.
--
-- 'capacityReservationFleetArn', 'capacityReservationFleet_capacityReservationFleetArn' - The ARN of the Capacity Reservation Fleet.
--
-- 'capacityReservationFleetId', 'capacityReservationFleet_capacityReservationFleetId' - The ID of the Capacity Reservation Fleet.
--
-- 'createTime', 'capacityReservationFleet_createTime' - The date and time at which the Capacity Reservation Fleet was created.
--
-- 'endDate', 'capacityReservationFleet_endDate' - The date and time at which the Capacity Reservation Fleet expires.
--
-- 'instanceMatchCriteria', 'capacityReservationFleet_instanceMatchCriteria' - Indicates the type of instance launches that the Capacity Reservation
-- Fleet accepts. All Capacity Reservations in the Fleet inherit this
-- instance matching criteria.
--
-- Currently, Capacity Reservation Fleets support @open@ instance matching
-- criteria only. This means that instances that have matching attributes
-- (instance type, platform, and Availability Zone) run in the Capacity
-- Reservations automatically. Instances do not need to explicitly target a
-- Capacity Reservation Fleet to use its reserved capacity.
--
-- 'instanceTypeSpecifications', 'capacityReservationFleet_instanceTypeSpecifications' - Information about the instance types for which to reserve the capacity.
--
-- 'state', 'capacityReservationFleet_state' - The state of the Capacity Reservation Fleet. Possible states include:
--
-- -   @submitted@ - The Capacity Reservation Fleet request has been
--     submitted and Amazon Elastic Compute Cloud is preparing to create
--     the Capacity Reservations.
--
-- -   @modifying@ - The Capacity Reservation Fleet is being modified. The
--     Fleet remains in this state until the modification is complete.
--
-- -   @active@ - The Capacity Reservation Fleet has fulfilled its total
--     target capacity and it is attempting to maintain this capacity. The
--     Fleet remains in this state until it is modified or deleted.
--
-- -   @partially_fulfilled@ - The Capacity Reservation Fleet has partially
--     fulfilled its total target capacity. There is insufficient Amazon
--     EC2 to fulfill the total target capacity. The Fleet is attempting to
--     asynchronously fulfill its total target capacity.
--
-- -   @expiring@ - The Capacity Reservation Fleet has reach its end date
--     and it is in the process of expiring. One or more of its Capacity
--     reservations might still be active.
--
-- -   @expired@ - The Capacity Reservation Fleet has reach its end date.
--     The Fleet and its Capacity Reservations are expired. The Fleet
--     can\'t create new Capacity Reservations.
--
-- -   @cancelling@ - The Capacity Reservation Fleet is in the process of
--     being cancelled. One or more of its Capacity reservations might
--     still be active.
--
-- -   @cancelled@ - The Capacity Reservation Fleet has been manually
--     cancelled. The Fleet and its Capacity Reservations are cancelled and
--     the Fleet can\'t create new Capacity Reservations.
--
-- -   @failed@ - The Capacity Reservation Fleet failed to reserve capacity
--     for the specified instance types.
--
-- 'tags', 'capacityReservationFleet_tags' - The tags assigned to the Capacity Reservation Fleet.
--
-- 'tenancy', 'capacityReservationFleet_tenancy' - The tenancy of the Capacity Reservation Fleet. Tenancies include:
--
-- -   @default@ - The Capacity Reservation Fleet is created on hardware
--     that is shared with other Amazon Web Services accounts.
--
-- -   @dedicated@ - The Capacity Reservation Fleet is created on
--     single-tenant hardware that is dedicated to a single Amazon Web
--     Services account.
--
-- 'totalFulfilledCapacity', 'capacityReservationFleet_totalFulfilledCapacity' - The capacity units that have been fulfilled.
--
-- 'totalTargetCapacity', 'capacityReservationFleet_totalTargetCapacity' - The total number of capacity units for which the Capacity Reservation
-- Fleet reserves capacity. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/crfleet-concepts.html#target-capacity Total target capacity>
-- in the Amazon EC2 User Guide.
newCapacityReservationFleet ::
  CapacityReservationFleet
newCapacityReservationFleet =
  CapacityReservationFleet'
    { allocationStrategy =
        Prelude.Nothing,
      capacityReservationFleetArn = Prelude.Nothing,
      capacityReservationFleetId = Prelude.Nothing,
      createTime = Prelude.Nothing,
      endDate = Prelude.Nothing,
      instanceMatchCriteria = Prelude.Nothing,
      instanceTypeSpecifications = Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing,
      tenancy = Prelude.Nothing,
      totalFulfilledCapacity = Prelude.Nothing,
      totalTargetCapacity = Prelude.Nothing
    }

-- | The strategy used by the Capacity Reservation Fleet to determine which
-- of the specified instance types to use. For more information, see For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/crfleet-concepts.html#allocation-strategy Allocation strategy>
-- in the Amazon EC2 User Guide.
capacityReservationFleet_allocationStrategy :: Lens.Lens' CapacityReservationFleet (Prelude.Maybe Prelude.Text)
capacityReservationFleet_allocationStrategy = Lens.lens (\CapacityReservationFleet' {allocationStrategy} -> allocationStrategy) (\s@CapacityReservationFleet' {} a -> s {allocationStrategy = a} :: CapacityReservationFleet)

-- | The ARN of the Capacity Reservation Fleet.
capacityReservationFleet_capacityReservationFleetArn :: Lens.Lens' CapacityReservationFleet (Prelude.Maybe Prelude.Text)
capacityReservationFleet_capacityReservationFleetArn = Lens.lens (\CapacityReservationFleet' {capacityReservationFleetArn} -> capacityReservationFleetArn) (\s@CapacityReservationFleet' {} a -> s {capacityReservationFleetArn = a} :: CapacityReservationFleet)

-- | The ID of the Capacity Reservation Fleet.
capacityReservationFleet_capacityReservationFleetId :: Lens.Lens' CapacityReservationFleet (Prelude.Maybe Prelude.Text)
capacityReservationFleet_capacityReservationFleetId = Lens.lens (\CapacityReservationFleet' {capacityReservationFleetId} -> capacityReservationFleetId) (\s@CapacityReservationFleet' {} a -> s {capacityReservationFleetId = a} :: CapacityReservationFleet)

-- | The date and time at which the Capacity Reservation Fleet was created.
capacityReservationFleet_createTime :: Lens.Lens' CapacityReservationFleet (Prelude.Maybe Prelude.UTCTime)
capacityReservationFleet_createTime = Lens.lens (\CapacityReservationFleet' {createTime} -> createTime) (\s@CapacityReservationFleet' {} a -> s {createTime = a} :: CapacityReservationFleet) Prelude.. Lens.mapping Data._Time

-- | The date and time at which the Capacity Reservation Fleet expires.
capacityReservationFleet_endDate :: Lens.Lens' CapacityReservationFleet (Prelude.Maybe Prelude.UTCTime)
capacityReservationFleet_endDate = Lens.lens (\CapacityReservationFleet' {endDate} -> endDate) (\s@CapacityReservationFleet' {} a -> s {endDate = a} :: CapacityReservationFleet) Prelude.. Lens.mapping Data._Time

-- | Indicates the type of instance launches that the Capacity Reservation
-- Fleet accepts. All Capacity Reservations in the Fleet inherit this
-- instance matching criteria.
--
-- Currently, Capacity Reservation Fleets support @open@ instance matching
-- criteria only. This means that instances that have matching attributes
-- (instance type, platform, and Availability Zone) run in the Capacity
-- Reservations automatically. Instances do not need to explicitly target a
-- Capacity Reservation Fleet to use its reserved capacity.
capacityReservationFleet_instanceMatchCriteria :: Lens.Lens' CapacityReservationFleet (Prelude.Maybe FleetInstanceMatchCriteria)
capacityReservationFleet_instanceMatchCriteria = Lens.lens (\CapacityReservationFleet' {instanceMatchCriteria} -> instanceMatchCriteria) (\s@CapacityReservationFleet' {} a -> s {instanceMatchCriteria = a} :: CapacityReservationFleet)

-- | Information about the instance types for which to reserve the capacity.
capacityReservationFleet_instanceTypeSpecifications :: Lens.Lens' CapacityReservationFleet (Prelude.Maybe [FleetCapacityReservation])
capacityReservationFleet_instanceTypeSpecifications = Lens.lens (\CapacityReservationFleet' {instanceTypeSpecifications} -> instanceTypeSpecifications) (\s@CapacityReservationFleet' {} a -> s {instanceTypeSpecifications = a} :: CapacityReservationFleet) Prelude.. Lens.mapping Lens.coerced

-- | The state of the Capacity Reservation Fleet. Possible states include:
--
-- -   @submitted@ - The Capacity Reservation Fleet request has been
--     submitted and Amazon Elastic Compute Cloud is preparing to create
--     the Capacity Reservations.
--
-- -   @modifying@ - The Capacity Reservation Fleet is being modified. The
--     Fleet remains in this state until the modification is complete.
--
-- -   @active@ - The Capacity Reservation Fleet has fulfilled its total
--     target capacity and it is attempting to maintain this capacity. The
--     Fleet remains in this state until it is modified or deleted.
--
-- -   @partially_fulfilled@ - The Capacity Reservation Fleet has partially
--     fulfilled its total target capacity. There is insufficient Amazon
--     EC2 to fulfill the total target capacity. The Fleet is attempting to
--     asynchronously fulfill its total target capacity.
--
-- -   @expiring@ - The Capacity Reservation Fleet has reach its end date
--     and it is in the process of expiring. One or more of its Capacity
--     reservations might still be active.
--
-- -   @expired@ - The Capacity Reservation Fleet has reach its end date.
--     The Fleet and its Capacity Reservations are expired. The Fleet
--     can\'t create new Capacity Reservations.
--
-- -   @cancelling@ - The Capacity Reservation Fleet is in the process of
--     being cancelled. One or more of its Capacity reservations might
--     still be active.
--
-- -   @cancelled@ - The Capacity Reservation Fleet has been manually
--     cancelled. The Fleet and its Capacity Reservations are cancelled and
--     the Fleet can\'t create new Capacity Reservations.
--
-- -   @failed@ - The Capacity Reservation Fleet failed to reserve capacity
--     for the specified instance types.
capacityReservationFleet_state :: Lens.Lens' CapacityReservationFleet (Prelude.Maybe CapacityReservationFleetState)
capacityReservationFleet_state = Lens.lens (\CapacityReservationFleet' {state} -> state) (\s@CapacityReservationFleet' {} a -> s {state = a} :: CapacityReservationFleet)

-- | The tags assigned to the Capacity Reservation Fleet.
capacityReservationFleet_tags :: Lens.Lens' CapacityReservationFleet (Prelude.Maybe [Tag])
capacityReservationFleet_tags = Lens.lens (\CapacityReservationFleet' {tags} -> tags) (\s@CapacityReservationFleet' {} a -> s {tags = a} :: CapacityReservationFleet) Prelude.. Lens.mapping Lens.coerced

-- | The tenancy of the Capacity Reservation Fleet. Tenancies include:
--
-- -   @default@ - The Capacity Reservation Fleet is created on hardware
--     that is shared with other Amazon Web Services accounts.
--
-- -   @dedicated@ - The Capacity Reservation Fleet is created on
--     single-tenant hardware that is dedicated to a single Amazon Web
--     Services account.
capacityReservationFleet_tenancy :: Lens.Lens' CapacityReservationFleet (Prelude.Maybe FleetCapacityReservationTenancy)
capacityReservationFleet_tenancy = Lens.lens (\CapacityReservationFleet' {tenancy} -> tenancy) (\s@CapacityReservationFleet' {} a -> s {tenancy = a} :: CapacityReservationFleet)

-- | The capacity units that have been fulfilled.
capacityReservationFleet_totalFulfilledCapacity :: Lens.Lens' CapacityReservationFleet (Prelude.Maybe Prelude.Double)
capacityReservationFleet_totalFulfilledCapacity = Lens.lens (\CapacityReservationFleet' {totalFulfilledCapacity} -> totalFulfilledCapacity) (\s@CapacityReservationFleet' {} a -> s {totalFulfilledCapacity = a} :: CapacityReservationFleet)

-- | The total number of capacity units for which the Capacity Reservation
-- Fleet reserves capacity. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/crfleet-concepts.html#target-capacity Total target capacity>
-- in the Amazon EC2 User Guide.
capacityReservationFleet_totalTargetCapacity :: Lens.Lens' CapacityReservationFleet (Prelude.Maybe Prelude.Int)
capacityReservationFleet_totalTargetCapacity = Lens.lens (\CapacityReservationFleet' {totalTargetCapacity} -> totalTargetCapacity) (\s@CapacityReservationFleet' {} a -> s {totalTargetCapacity = a} :: CapacityReservationFleet)

instance Data.FromXML CapacityReservationFleet where
  parseXML x =
    CapacityReservationFleet'
      Prelude.<$> (x Data..@? "allocationStrategy")
      Prelude.<*> (x Data..@? "capacityReservationFleetArn")
      Prelude.<*> (x Data..@? "capacityReservationFleetId")
      Prelude.<*> (x Data..@? "createTime")
      Prelude.<*> (x Data..@? "endDate")
      Prelude.<*> (x Data..@? "instanceMatchCriteria")
      Prelude.<*> ( x Data..@? "instanceTypeSpecificationSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "state")
      Prelude.<*> ( x Data..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "tenancy")
      Prelude.<*> (x Data..@? "totalFulfilledCapacity")
      Prelude.<*> (x Data..@? "totalTargetCapacity")

instance Prelude.Hashable CapacityReservationFleet where
  hashWithSalt _salt CapacityReservationFleet' {..} =
    _salt `Prelude.hashWithSalt` allocationStrategy
      `Prelude.hashWithSalt` capacityReservationFleetArn
      `Prelude.hashWithSalt` capacityReservationFleetId
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` endDate
      `Prelude.hashWithSalt` instanceMatchCriteria
      `Prelude.hashWithSalt` instanceTypeSpecifications
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` tenancy
      `Prelude.hashWithSalt` totalFulfilledCapacity
      `Prelude.hashWithSalt` totalTargetCapacity

instance Prelude.NFData CapacityReservationFleet where
  rnf CapacityReservationFleet' {..} =
    Prelude.rnf allocationStrategy
      `Prelude.seq` Prelude.rnf capacityReservationFleetArn
      `Prelude.seq` Prelude.rnf capacityReservationFleetId
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf endDate
      `Prelude.seq` Prelude.rnf instanceMatchCriteria
      `Prelude.seq` Prelude.rnf instanceTypeSpecifications
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf tenancy
      `Prelude.seq` Prelude.rnf totalFulfilledCapacity
      `Prelude.seq` Prelude.rnf totalTargetCapacity
