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
-- Module      : Amazonka.EC2.Types.CapacityReservationFleetCancellationState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.CapacityReservationFleetCancellationState where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.CapacityReservationFleetState
import qualified Amazonka.Prelude as Prelude

-- | Describes a Capacity Reservation Fleet that was successfully cancelled.
--
-- /See:/ 'newCapacityReservationFleetCancellationState' smart constructor.
data CapacityReservationFleetCancellationState = CapacityReservationFleetCancellationState'
  { -- | The ID of the Capacity Reservation Fleet that was successfully
    -- cancelled.
    capacityReservationFleetId :: Prelude.Maybe Prelude.Text,
    -- | The current state of the Capacity Reservation Fleet.
    currentFleetState :: Prelude.Maybe CapacityReservationFleetState,
    -- | The previous state of the Capacity Reservation Fleet.
    previousFleetState :: Prelude.Maybe CapacityReservationFleetState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CapacityReservationFleetCancellationState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capacityReservationFleetId', 'capacityReservationFleetCancellationState_capacityReservationFleetId' - The ID of the Capacity Reservation Fleet that was successfully
-- cancelled.
--
-- 'currentFleetState', 'capacityReservationFleetCancellationState_currentFleetState' - The current state of the Capacity Reservation Fleet.
--
-- 'previousFleetState', 'capacityReservationFleetCancellationState_previousFleetState' - The previous state of the Capacity Reservation Fleet.
newCapacityReservationFleetCancellationState ::
  CapacityReservationFleetCancellationState
newCapacityReservationFleetCancellationState =
  CapacityReservationFleetCancellationState'
    { capacityReservationFleetId =
        Prelude.Nothing,
      currentFleetState =
        Prelude.Nothing,
      previousFleetState =
        Prelude.Nothing
    }

-- | The ID of the Capacity Reservation Fleet that was successfully
-- cancelled.
capacityReservationFleetCancellationState_capacityReservationFleetId :: Lens.Lens' CapacityReservationFleetCancellationState (Prelude.Maybe Prelude.Text)
capacityReservationFleetCancellationState_capacityReservationFleetId = Lens.lens (\CapacityReservationFleetCancellationState' {capacityReservationFleetId} -> capacityReservationFleetId) (\s@CapacityReservationFleetCancellationState' {} a -> s {capacityReservationFleetId = a} :: CapacityReservationFleetCancellationState)

-- | The current state of the Capacity Reservation Fleet.
capacityReservationFleetCancellationState_currentFleetState :: Lens.Lens' CapacityReservationFleetCancellationState (Prelude.Maybe CapacityReservationFleetState)
capacityReservationFleetCancellationState_currentFleetState = Lens.lens (\CapacityReservationFleetCancellationState' {currentFleetState} -> currentFleetState) (\s@CapacityReservationFleetCancellationState' {} a -> s {currentFleetState = a} :: CapacityReservationFleetCancellationState)

-- | The previous state of the Capacity Reservation Fleet.
capacityReservationFleetCancellationState_previousFleetState :: Lens.Lens' CapacityReservationFleetCancellationState (Prelude.Maybe CapacityReservationFleetState)
capacityReservationFleetCancellationState_previousFleetState = Lens.lens (\CapacityReservationFleetCancellationState' {previousFleetState} -> previousFleetState) (\s@CapacityReservationFleetCancellationState' {} a -> s {previousFleetState = a} :: CapacityReservationFleetCancellationState)

instance
  Data.FromXML
    CapacityReservationFleetCancellationState
  where
  parseXML x =
    CapacityReservationFleetCancellationState'
      Prelude.<$> (x Data..@? "capacityReservationFleetId")
        Prelude.<*> (x Data..@? "currentFleetState")
        Prelude.<*> (x Data..@? "previousFleetState")

instance
  Prelude.Hashable
    CapacityReservationFleetCancellationState
  where
  hashWithSalt
    _salt
    CapacityReservationFleetCancellationState' {..} =
      _salt
        `Prelude.hashWithSalt` capacityReservationFleetId
        `Prelude.hashWithSalt` currentFleetState
        `Prelude.hashWithSalt` previousFleetState

instance
  Prelude.NFData
    CapacityReservationFleetCancellationState
  where
  rnf CapacityReservationFleetCancellationState' {..} =
    Prelude.rnf capacityReservationFleetId
      `Prelude.seq` Prelude.rnf currentFleetState
      `Prelude.seq` Prelude.rnf previousFleetState
