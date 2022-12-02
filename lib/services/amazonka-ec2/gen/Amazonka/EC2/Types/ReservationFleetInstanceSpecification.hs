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
-- Module      : Amazonka.EC2.Types.ReservationFleetInstanceSpecification
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ReservationFleetInstanceSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.CapacityReservationInstancePlatform
import Amazonka.EC2.Types.InstanceType
import qualified Amazonka.Prelude as Prelude

-- | Information about an instance type to use in a Capacity Reservation
-- Fleet.
--
-- /See:/ 'newReservationFleetInstanceSpecification' smart constructor.
data ReservationFleetInstanceSpecification = ReservationFleetInstanceSpecification'
  { -- | Indicates whether the Capacity Reservation Fleet supports EBS-optimized
    -- instances types. This optimization provides dedicated throughput to
    -- Amazon EBS and an optimized configuration stack to provide optimal I\/O
    -- performance. This optimization isn\'t available with all instance types.
    -- Additional usage charges apply when using EBS-optimized instance types.
    ebsOptimized :: Prelude.Maybe Prelude.Bool,
    -- | The Availability Zone in which the Capacity Reservation Fleet reserves
    -- the capacity. A Capacity Reservation Fleet can\'t span Availability
    -- Zones. All instance type specifications that you specify for the Fleet
    -- must use the same Availability Zone.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The instance type for which the Capacity Reservation Fleet reserves
    -- capacity.
    instanceType :: Prelude.Maybe InstanceType,
    -- | The type of operating system for which the Capacity Reservation Fleet
    -- reserves capacity.
    instancePlatform :: Prelude.Maybe CapacityReservationInstancePlatform,
    -- | The priority to assign to the instance type. This value is used to
    -- determine which of the instance types specified for the Fleet should be
    -- prioritized for use. A lower value indicates a high priority. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/crfleet-concepts.html#instance-priority Instance type priority>
    -- in the Amazon EC2 User Guide.
    priority :: Prelude.Maybe Prelude.Natural,
    -- | The number of capacity units provided by the specified instance type.
    -- This value, together with the total target capacity that you specify for
    -- the Fleet determine the number of instances for which the Fleet reserves
    -- capacity. Both values are based on units that make sense for your
    -- workload. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/crfleet-concepts.html#target-capacity Total target capacity>
    -- in the Amazon EC2 User Guide.
    weight :: Prelude.Maybe Prelude.Double,
    -- | The ID of the Availability Zone in which the Capacity Reservation Fleet
    -- reserves the capacity. A Capacity Reservation Fleet can\'t span
    -- Availability Zones. All instance type specifications that you specify
    -- for the Fleet must use the same Availability Zone.
    availabilityZoneId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReservationFleetInstanceSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ebsOptimized', 'reservationFleetInstanceSpecification_ebsOptimized' - Indicates whether the Capacity Reservation Fleet supports EBS-optimized
-- instances types. This optimization provides dedicated throughput to
-- Amazon EBS and an optimized configuration stack to provide optimal I\/O
-- performance. This optimization isn\'t available with all instance types.
-- Additional usage charges apply when using EBS-optimized instance types.
--
-- 'availabilityZone', 'reservationFleetInstanceSpecification_availabilityZone' - The Availability Zone in which the Capacity Reservation Fleet reserves
-- the capacity. A Capacity Reservation Fleet can\'t span Availability
-- Zones. All instance type specifications that you specify for the Fleet
-- must use the same Availability Zone.
--
-- 'instanceType', 'reservationFleetInstanceSpecification_instanceType' - The instance type for which the Capacity Reservation Fleet reserves
-- capacity.
--
-- 'instancePlatform', 'reservationFleetInstanceSpecification_instancePlatform' - The type of operating system for which the Capacity Reservation Fleet
-- reserves capacity.
--
-- 'priority', 'reservationFleetInstanceSpecification_priority' - The priority to assign to the instance type. This value is used to
-- determine which of the instance types specified for the Fleet should be
-- prioritized for use. A lower value indicates a high priority. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/crfleet-concepts.html#instance-priority Instance type priority>
-- in the Amazon EC2 User Guide.
--
-- 'weight', 'reservationFleetInstanceSpecification_weight' - The number of capacity units provided by the specified instance type.
-- This value, together with the total target capacity that you specify for
-- the Fleet determine the number of instances for which the Fleet reserves
-- capacity. Both values are based on units that make sense for your
-- workload. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/crfleet-concepts.html#target-capacity Total target capacity>
-- in the Amazon EC2 User Guide.
--
-- 'availabilityZoneId', 'reservationFleetInstanceSpecification_availabilityZoneId' - The ID of the Availability Zone in which the Capacity Reservation Fleet
-- reserves the capacity. A Capacity Reservation Fleet can\'t span
-- Availability Zones. All instance type specifications that you specify
-- for the Fleet must use the same Availability Zone.
newReservationFleetInstanceSpecification ::
  ReservationFleetInstanceSpecification
newReservationFleetInstanceSpecification =
  ReservationFleetInstanceSpecification'
    { ebsOptimized =
        Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      instancePlatform = Prelude.Nothing,
      priority = Prelude.Nothing,
      weight = Prelude.Nothing,
      availabilityZoneId = Prelude.Nothing
    }

-- | Indicates whether the Capacity Reservation Fleet supports EBS-optimized
-- instances types. This optimization provides dedicated throughput to
-- Amazon EBS and an optimized configuration stack to provide optimal I\/O
-- performance. This optimization isn\'t available with all instance types.
-- Additional usage charges apply when using EBS-optimized instance types.
reservationFleetInstanceSpecification_ebsOptimized :: Lens.Lens' ReservationFleetInstanceSpecification (Prelude.Maybe Prelude.Bool)
reservationFleetInstanceSpecification_ebsOptimized = Lens.lens (\ReservationFleetInstanceSpecification' {ebsOptimized} -> ebsOptimized) (\s@ReservationFleetInstanceSpecification' {} a -> s {ebsOptimized = a} :: ReservationFleetInstanceSpecification)

-- | The Availability Zone in which the Capacity Reservation Fleet reserves
-- the capacity. A Capacity Reservation Fleet can\'t span Availability
-- Zones. All instance type specifications that you specify for the Fleet
-- must use the same Availability Zone.
reservationFleetInstanceSpecification_availabilityZone :: Lens.Lens' ReservationFleetInstanceSpecification (Prelude.Maybe Prelude.Text)
reservationFleetInstanceSpecification_availabilityZone = Lens.lens (\ReservationFleetInstanceSpecification' {availabilityZone} -> availabilityZone) (\s@ReservationFleetInstanceSpecification' {} a -> s {availabilityZone = a} :: ReservationFleetInstanceSpecification)

-- | The instance type for which the Capacity Reservation Fleet reserves
-- capacity.
reservationFleetInstanceSpecification_instanceType :: Lens.Lens' ReservationFleetInstanceSpecification (Prelude.Maybe InstanceType)
reservationFleetInstanceSpecification_instanceType = Lens.lens (\ReservationFleetInstanceSpecification' {instanceType} -> instanceType) (\s@ReservationFleetInstanceSpecification' {} a -> s {instanceType = a} :: ReservationFleetInstanceSpecification)

-- | The type of operating system for which the Capacity Reservation Fleet
-- reserves capacity.
reservationFleetInstanceSpecification_instancePlatform :: Lens.Lens' ReservationFleetInstanceSpecification (Prelude.Maybe CapacityReservationInstancePlatform)
reservationFleetInstanceSpecification_instancePlatform = Lens.lens (\ReservationFleetInstanceSpecification' {instancePlatform} -> instancePlatform) (\s@ReservationFleetInstanceSpecification' {} a -> s {instancePlatform = a} :: ReservationFleetInstanceSpecification)

-- | The priority to assign to the instance type. This value is used to
-- determine which of the instance types specified for the Fleet should be
-- prioritized for use. A lower value indicates a high priority. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/crfleet-concepts.html#instance-priority Instance type priority>
-- in the Amazon EC2 User Guide.
reservationFleetInstanceSpecification_priority :: Lens.Lens' ReservationFleetInstanceSpecification (Prelude.Maybe Prelude.Natural)
reservationFleetInstanceSpecification_priority = Lens.lens (\ReservationFleetInstanceSpecification' {priority} -> priority) (\s@ReservationFleetInstanceSpecification' {} a -> s {priority = a} :: ReservationFleetInstanceSpecification)

-- | The number of capacity units provided by the specified instance type.
-- This value, together with the total target capacity that you specify for
-- the Fleet determine the number of instances for which the Fleet reserves
-- capacity. Both values are based on units that make sense for your
-- workload. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/crfleet-concepts.html#target-capacity Total target capacity>
-- in the Amazon EC2 User Guide.
reservationFleetInstanceSpecification_weight :: Lens.Lens' ReservationFleetInstanceSpecification (Prelude.Maybe Prelude.Double)
reservationFleetInstanceSpecification_weight = Lens.lens (\ReservationFleetInstanceSpecification' {weight} -> weight) (\s@ReservationFleetInstanceSpecification' {} a -> s {weight = a} :: ReservationFleetInstanceSpecification)

-- | The ID of the Availability Zone in which the Capacity Reservation Fleet
-- reserves the capacity. A Capacity Reservation Fleet can\'t span
-- Availability Zones. All instance type specifications that you specify
-- for the Fleet must use the same Availability Zone.
reservationFleetInstanceSpecification_availabilityZoneId :: Lens.Lens' ReservationFleetInstanceSpecification (Prelude.Maybe Prelude.Text)
reservationFleetInstanceSpecification_availabilityZoneId = Lens.lens (\ReservationFleetInstanceSpecification' {availabilityZoneId} -> availabilityZoneId) (\s@ReservationFleetInstanceSpecification' {} a -> s {availabilityZoneId = a} :: ReservationFleetInstanceSpecification)

instance
  Prelude.Hashable
    ReservationFleetInstanceSpecification
  where
  hashWithSalt
    _salt
    ReservationFleetInstanceSpecification' {..} =
      _salt `Prelude.hashWithSalt` ebsOptimized
        `Prelude.hashWithSalt` availabilityZone
        `Prelude.hashWithSalt` instanceType
        `Prelude.hashWithSalt` instancePlatform
        `Prelude.hashWithSalt` priority
        `Prelude.hashWithSalt` weight
        `Prelude.hashWithSalt` availabilityZoneId

instance
  Prelude.NFData
    ReservationFleetInstanceSpecification
  where
  rnf ReservationFleetInstanceSpecification' {..} =
    Prelude.rnf ebsOptimized
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf instancePlatform
      `Prelude.seq` Prelude.rnf priority
      `Prelude.seq` Prelude.rnf weight
      `Prelude.seq` Prelude.rnf availabilityZoneId

instance
  Data.ToQuery
    ReservationFleetInstanceSpecification
  where
  toQuery ReservationFleetInstanceSpecification' {..} =
    Prelude.mconcat
      [ "EbsOptimized" Data.=: ebsOptimized,
        "AvailabilityZone" Data.=: availabilityZone,
        "InstanceType" Data.=: instanceType,
        "InstancePlatform" Data.=: instancePlatform,
        "Priority" Data.=: priority,
        "Weight" Data.=: weight,
        "AvailabilityZoneId" Data.=: availabilityZoneId
      ]
