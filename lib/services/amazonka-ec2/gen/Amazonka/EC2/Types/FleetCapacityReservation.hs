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
-- Module      : Amazonka.EC2.Types.FleetCapacityReservation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.FleetCapacityReservation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.CapacityReservationInstancePlatform
import Amazonka.EC2.Types.InstanceType
import qualified Amazonka.Prelude as Prelude

-- | Information about a Capacity Reservation in a Capacity Reservation
-- Fleet.
--
-- /See:/ 'newFleetCapacityReservation' smart constructor.
data FleetCapacityReservation = FleetCapacityReservation'
  { -- | The Availability Zone in which the Capacity Reservation reserves
    -- capacity.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Availability Zone in which the Capacity Reservation
    -- reserves capacity.
    availabilityZoneId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Capacity Reservation.
    capacityReservationId :: Prelude.Maybe Prelude.Text,
    -- | The date and time at which the Capacity Reservation was created.
    createDate :: Prelude.Maybe Data.ISO8601,
    -- | Indicates whether the Capacity Reservation reserves capacity for
    -- EBS-optimized instance types.
    ebsOptimized :: Prelude.Maybe Prelude.Bool,
    -- | The number of capacity units fulfilled by the Capacity Reservation. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/crfleet-concepts.html#target-capacity Total target capacity>
    -- in the Amazon EC2 User Guide.
    fulfilledCapacity :: Prelude.Maybe Prelude.Double,
    -- | The type of operating system for which the Capacity Reservation reserves
    -- capacity.
    instancePlatform :: Prelude.Maybe CapacityReservationInstancePlatform,
    -- | The instance type for which the Capacity Reservation reserves capacity.
    instanceType :: Prelude.Maybe InstanceType,
    -- | The priority of the instance type in the Capacity Reservation Fleet. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/crfleet-concepts.html#instance-priority Instance type priority>
    -- in the Amazon EC2 User Guide.
    priority :: Prelude.Maybe Prelude.Natural,
    -- | The total number of instances for which the Capacity Reservation
    -- reserves capacity.
    totalInstanceCount :: Prelude.Maybe Prelude.Int,
    -- | The weight of the instance type in the Capacity Reservation Fleet. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/crfleet-concepts.html#instance-weight Instance type weight>
    -- in the Amazon EC2 User Guide.
    weight :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FleetCapacityReservation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZone', 'fleetCapacityReservation_availabilityZone' - The Availability Zone in which the Capacity Reservation reserves
-- capacity.
--
-- 'availabilityZoneId', 'fleetCapacityReservation_availabilityZoneId' - The ID of the Availability Zone in which the Capacity Reservation
-- reserves capacity.
--
-- 'capacityReservationId', 'fleetCapacityReservation_capacityReservationId' - The ID of the Capacity Reservation.
--
-- 'createDate', 'fleetCapacityReservation_createDate' - The date and time at which the Capacity Reservation was created.
--
-- 'ebsOptimized', 'fleetCapacityReservation_ebsOptimized' - Indicates whether the Capacity Reservation reserves capacity for
-- EBS-optimized instance types.
--
-- 'fulfilledCapacity', 'fleetCapacityReservation_fulfilledCapacity' - The number of capacity units fulfilled by the Capacity Reservation. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/crfleet-concepts.html#target-capacity Total target capacity>
-- in the Amazon EC2 User Guide.
--
-- 'instancePlatform', 'fleetCapacityReservation_instancePlatform' - The type of operating system for which the Capacity Reservation reserves
-- capacity.
--
-- 'instanceType', 'fleetCapacityReservation_instanceType' - The instance type for which the Capacity Reservation reserves capacity.
--
-- 'priority', 'fleetCapacityReservation_priority' - The priority of the instance type in the Capacity Reservation Fleet. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/crfleet-concepts.html#instance-priority Instance type priority>
-- in the Amazon EC2 User Guide.
--
-- 'totalInstanceCount', 'fleetCapacityReservation_totalInstanceCount' - The total number of instances for which the Capacity Reservation
-- reserves capacity.
--
-- 'weight', 'fleetCapacityReservation_weight' - The weight of the instance type in the Capacity Reservation Fleet. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/crfleet-concepts.html#instance-weight Instance type weight>
-- in the Amazon EC2 User Guide.
newFleetCapacityReservation ::
  FleetCapacityReservation
newFleetCapacityReservation =
  FleetCapacityReservation'
    { availabilityZone =
        Prelude.Nothing,
      availabilityZoneId = Prelude.Nothing,
      capacityReservationId = Prelude.Nothing,
      createDate = Prelude.Nothing,
      ebsOptimized = Prelude.Nothing,
      fulfilledCapacity = Prelude.Nothing,
      instancePlatform = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      priority = Prelude.Nothing,
      totalInstanceCount = Prelude.Nothing,
      weight = Prelude.Nothing
    }

-- | The Availability Zone in which the Capacity Reservation reserves
-- capacity.
fleetCapacityReservation_availabilityZone :: Lens.Lens' FleetCapacityReservation (Prelude.Maybe Prelude.Text)
fleetCapacityReservation_availabilityZone = Lens.lens (\FleetCapacityReservation' {availabilityZone} -> availabilityZone) (\s@FleetCapacityReservation' {} a -> s {availabilityZone = a} :: FleetCapacityReservation)

-- | The ID of the Availability Zone in which the Capacity Reservation
-- reserves capacity.
fleetCapacityReservation_availabilityZoneId :: Lens.Lens' FleetCapacityReservation (Prelude.Maybe Prelude.Text)
fleetCapacityReservation_availabilityZoneId = Lens.lens (\FleetCapacityReservation' {availabilityZoneId} -> availabilityZoneId) (\s@FleetCapacityReservation' {} a -> s {availabilityZoneId = a} :: FleetCapacityReservation)

-- | The ID of the Capacity Reservation.
fleetCapacityReservation_capacityReservationId :: Lens.Lens' FleetCapacityReservation (Prelude.Maybe Prelude.Text)
fleetCapacityReservation_capacityReservationId = Lens.lens (\FleetCapacityReservation' {capacityReservationId} -> capacityReservationId) (\s@FleetCapacityReservation' {} a -> s {capacityReservationId = a} :: FleetCapacityReservation)

-- | The date and time at which the Capacity Reservation was created.
fleetCapacityReservation_createDate :: Lens.Lens' FleetCapacityReservation (Prelude.Maybe Prelude.UTCTime)
fleetCapacityReservation_createDate = Lens.lens (\FleetCapacityReservation' {createDate} -> createDate) (\s@FleetCapacityReservation' {} a -> s {createDate = a} :: FleetCapacityReservation) Prelude.. Lens.mapping Data._Time

-- | Indicates whether the Capacity Reservation reserves capacity for
-- EBS-optimized instance types.
fleetCapacityReservation_ebsOptimized :: Lens.Lens' FleetCapacityReservation (Prelude.Maybe Prelude.Bool)
fleetCapacityReservation_ebsOptimized = Lens.lens (\FleetCapacityReservation' {ebsOptimized} -> ebsOptimized) (\s@FleetCapacityReservation' {} a -> s {ebsOptimized = a} :: FleetCapacityReservation)

-- | The number of capacity units fulfilled by the Capacity Reservation. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/crfleet-concepts.html#target-capacity Total target capacity>
-- in the Amazon EC2 User Guide.
fleetCapacityReservation_fulfilledCapacity :: Lens.Lens' FleetCapacityReservation (Prelude.Maybe Prelude.Double)
fleetCapacityReservation_fulfilledCapacity = Lens.lens (\FleetCapacityReservation' {fulfilledCapacity} -> fulfilledCapacity) (\s@FleetCapacityReservation' {} a -> s {fulfilledCapacity = a} :: FleetCapacityReservation)

-- | The type of operating system for which the Capacity Reservation reserves
-- capacity.
fleetCapacityReservation_instancePlatform :: Lens.Lens' FleetCapacityReservation (Prelude.Maybe CapacityReservationInstancePlatform)
fleetCapacityReservation_instancePlatform = Lens.lens (\FleetCapacityReservation' {instancePlatform} -> instancePlatform) (\s@FleetCapacityReservation' {} a -> s {instancePlatform = a} :: FleetCapacityReservation)

-- | The instance type for which the Capacity Reservation reserves capacity.
fleetCapacityReservation_instanceType :: Lens.Lens' FleetCapacityReservation (Prelude.Maybe InstanceType)
fleetCapacityReservation_instanceType = Lens.lens (\FleetCapacityReservation' {instanceType} -> instanceType) (\s@FleetCapacityReservation' {} a -> s {instanceType = a} :: FleetCapacityReservation)

-- | The priority of the instance type in the Capacity Reservation Fleet. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/crfleet-concepts.html#instance-priority Instance type priority>
-- in the Amazon EC2 User Guide.
fleetCapacityReservation_priority :: Lens.Lens' FleetCapacityReservation (Prelude.Maybe Prelude.Natural)
fleetCapacityReservation_priority = Lens.lens (\FleetCapacityReservation' {priority} -> priority) (\s@FleetCapacityReservation' {} a -> s {priority = a} :: FleetCapacityReservation)

-- | The total number of instances for which the Capacity Reservation
-- reserves capacity.
fleetCapacityReservation_totalInstanceCount :: Lens.Lens' FleetCapacityReservation (Prelude.Maybe Prelude.Int)
fleetCapacityReservation_totalInstanceCount = Lens.lens (\FleetCapacityReservation' {totalInstanceCount} -> totalInstanceCount) (\s@FleetCapacityReservation' {} a -> s {totalInstanceCount = a} :: FleetCapacityReservation)

-- | The weight of the instance type in the Capacity Reservation Fleet. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/crfleet-concepts.html#instance-weight Instance type weight>
-- in the Amazon EC2 User Guide.
fleetCapacityReservation_weight :: Lens.Lens' FleetCapacityReservation (Prelude.Maybe Prelude.Double)
fleetCapacityReservation_weight = Lens.lens (\FleetCapacityReservation' {weight} -> weight) (\s@FleetCapacityReservation' {} a -> s {weight = a} :: FleetCapacityReservation)

instance Data.FromXML FleetCapacityReservation where
  parseXML x =
    FleetCapacityReservation'
      Prelude.<$> (x Data..@? "availabilityZone")
      Prelude.<*> (x Data..@? "availabilityZoneId")
      Prelude.<*> (x Data..@? "capacityReservationId")
      Prelude.<*> (x Data..@? "createDate")
      Prelude.<*> (x Data..@? "ebsOptimized")
      Prelude.<*> (x Data..@? "fulfilledCapacity")
      Prelude.<*> (x Data..@? "instancePlatform")
      Prelude.<*> (x Data..@? "instanceType")
      Prelude.<*> (x Data..@? "priority")
      Prelude.<*> (x Data..@? "totalInstanceCount")
      Prelude.<*> (x Data..@? "weight")

instance Prelude.Hashable FleetCapacityReservation where
  hashWithSalt _salt FleetCapacityReservation' {..} =
    _salt `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` availabilityZoneId
      `Prelude.hashWithSalt` capacityReservationId
      `Prelude.hashWithSalt` createDate
      `Prelude.hashWithSalt` ebsOptimized
      `Prelude.hashWithSalt` fulfilledCapacity
      `Prelude.hashWithSalt` instancePlatform
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` priority
      `Prelude.hashWithSalt` totalInstanceCount
      `Prelude.hashWithSalt` weight

instance Prelude.NFData FleetCapacityReservation where
  rnf FleetCapacityReservation' {..} =
    Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf availabilityZoneId
      `Prelude.seq` Prelude.rnf capacityReservationId
      `Prelude.seq` Prelude.rnf createDate
      `Prelude.seq` Prelude.rnf ebsOptimized
      `Prelude.seq` Prelude.rnf fulfilledCapacity
      `Prelude.seq` Prelude.rnf instancePlatform
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf priority
      `Prelude.seq` Prelude.rnf totalInstanceCount
      `Prelude.seq` Prelude.rnf weight
