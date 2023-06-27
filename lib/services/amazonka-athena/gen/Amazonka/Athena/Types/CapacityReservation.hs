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
-- Module      : Amazonka.Athena.Types.CapacityReservation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.CapacityReservation where

import Amazonka.Athena.Types.CapacityAllocation
import Amazonka.Athena.Types.CapacityReservationStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A reservation for a specified number of data processing units (DPUs).
-- When a reservation is initially created, it has no DPUs. Athena
-- allocates DPUs until the allocated amount equals the requested amount.
--
-- /See:/ 'newCapacityReservation' smart constructor.
data CapacityReservation = CapacityReservation'
  { lastAllocation :: Prelude.Maybe CapacityAllocation,
    -- | The time of the most recent capacity allocation that succeeded.
    lastSuccessfulAllocationTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the capacity reservation.
    name :: Prelude.Text,
    -- | The status of the capacity reservation.
    status :: CapacityReservationStatus,
    -- | The number of data processing units requested.
    targetDpus :: Prelude.Natural,
    -- | The number of data processing units currently allocated.
    allocatedDpus :: Prelude.Natural,
    -- | The time in UTC epoch millis when the capacity reservation was created.
    creationTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CapacityReservation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastAllocation', 'capacityReservation_lastAllocation' - Undocumented member.
--
-- 'lastSuccessfulAllocationTime', 'capacityReservation_lastSuccessfulAllocationTime' - The time of the most recent capacity allocation that succeeded.
--
-- 'name', 'capacityReservation_name' - The name of the capacity reservation.
--
-- 'status', 'capacityReservation_status' - The status of the capacity reservation.
--
-- 'targetDpus', 'capacityReservation_targetDpus' - The number of data processing units requested.
--
-- 'allocatedDpus', 'capacityReservation_allocatedDpus' - The number of data processing units currently allocated.
--
-- 'creationTime', 'capacityReservation_creationTime' - The time in UTC epoch millis when the capacity reservation was created.
newCapacityReservation ::
  -- | 'name'
  Prelude.Text ->
  -- | 'status'
  CapacityReservationStatus ->
  -- | 'targetDpus'
  Prelude.Natural ->
  -- | 'allocatedDpus'
  Prelude.Natural ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  CapacityReservation
newCapacityReservation
  pName_
  pStatus_
  pTargetDpus_
  pAllocatedDpus_
  pCreationTime_ =
    CapacityReservation'
      { lastAllocation =
          Prelude.Nothing,
        lastSuccessfulAllocationTime = Prelude.Nothing,
        name = pName_,
        status = pStatus_,
        targetDpus = pTargetDpus_,
        allocatedDpus = pAllocatedDpus_,
        creationTime = Data._Time Lens.# pCreationTime_
      }

-- | Undocumented member.
capacityReservation_lastAllocation :: Lens.Lens' CapacityReservation (Prelude.Maybe CapacityAllocation)
capacityReservation_lastAllocation = Lens.lens (\CapacityReservation' {lastAllocation} -> lastAllocation) (\s@CapacityReservation' {} a -> s {lastAllocation = a} :: CapacityReservation)

-- | The time of the most recent capacity allocation that succeeded.
capacityReservation_lastSuccessfulAllocationTime :: Lens.Lens' CapacityReservation (Prelude.Maybe Prelude.UTCTime)
capacityReservation_lastSuccessfulAllocationTime = Lens.lens (\CapacityReservation' {lastSuccessfulAllocationTime} -> lastSuccessfulAllocationTime) (\s@CapacityReservation' {} a -> s {lastSuccessfulAllocationTime = a} :: CapacityReservation) Prelude.. Lens.mapping Data._Time

-- | The name of the capacity reservation.
capacityReservation_name :: Lens.Lens' CapacityReservation Prelude.Text
capacityReservation_name = Lens.lens (\CapacityReservation' {name} -> name) (\s@CapacityReservation' {} a -> s {name = a} :: CapacityReservation)

-- | The status of the capacity reservation.
capacityReservation_status :: Lens.Lens' CapacityReservation CapacityReservationStatus
capacityReservation_status = Lens.lens (\CapacityReservation' {status} -> status) (\s@CapacityReservation' {} a -> s {status = a} :: CapacityReservation)

-- | The number of data processing units requested.
capacityReservation_targetDpus :: Lens.Lens' CapacityReservation Prelude.Natural
capacityReservation_targetDpus = Lens.lens (\CapacityReservation' {targetDpus} -> targetDpus) (\s@CapacityReservation' {} a -> s {targetDpus = a} :: CapacityReservation)

-- | The number of data processing units currently allocated.
capacityReservation_allocatedDpus :: Lens.Lens' CapacityReservation Prelude.Natural
capacityReservation_allocatedDpus = Lens.lens (\CapacityReservation' {allocatedDpus} -> allocatedDpus) (\s@CapacityReservation' {} a -> s {allocatedDpus = a} :: CapacityReservation)

-- | The time in UTC epoch millis when the capacity reservation was created.
capacityReservation_creationTime :: Lens.Lens' CapacityReservation Prelude.UTCTime
capacityReservation_creationTime = Lens.lens (\CapacityReservation' {creationTime} -> creationTime) (\s@CapacityReservation' {} a -> s {creationTime = a} :: CapacityReservation) Prelude.. Data._Time

instance Data.FromJSON CapacityReservation where
  parseJSON =
    Data.withObject
      "CapacityReservation"
      ( \x ->
          CapacityReservation'
            Prelude.<$> (x Data..:? "LastAllocation")
            Prelude.<*> (x Data..:? "LastSuccessfulAllocationTime")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "Status")
            Prelude.<*> (x Data..: "TargetDpus")
            Prelude.<*> (x Data..: "AllocatedDpus")
            Prelude.<*> (x Data..: "CreationTime")
      )

instance Prelude.Hashable CapacityReservation where
  hashWithSalt _salt CapacityReservation' {..} =
    _salt
      `Prelude.hashWithSalt` lastAllocation
      `Prelude.hashWithSalt` lastSuccessfulAllocationTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` targetDpus
      `Prelude.hashWithSalt` allocatedDpus
      `Prelude.hashWithSalt` creationTime

instance Prelude.NFData CapacityReservation where
  rnf CapacityReservation' {..} =
    Prelude.rnf lastAllocation
      `Prelude.seq` Prelude.rnf lastSuccessfulAllocationTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf targetDpus
      `Prelude.seq` Prelude.rnf allocatedDpus
      `Prelude.seq` Prelude.rnf creationTime
