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
-- Module      : Amazonka.EC2.Types.FailedCapacityReservationFleetCancellationResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.FailedCapacityReservationFleetCancellationResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.CancelCapacityReservationFleetError
import qualified Amazonka.Prelude as Prelude

-- | Describes a Capacity Reservation Fleet that could not be cancelled.
--
-- /See:/ 'newFailedCapacityReservationFleetCancellationResult' smart constructor.
data FailedCapacityReservationFleetCancellationResult = FailedCapacityReservationFleetCancellationResult'
  { -- | Information about the Capacity Reservation Fleet cancellation error.
    cancelCapacityReservationFleetError :: Prelude.Maybe CancelCapacityReservationFleetError,
    -- | The ID of the Capacity Reservation Fleet that could not be cancelled.
    capacityReservationFleetId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FailedCapacityReservationFleetCancellationResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cancelCapacityReservationFleetError', 'failedCapacityReservationFleetCancellationResult_cancelCapacityReservationFleetError' - Information about the Capacity Reservation Fleet cancellation error.
--
-- 'capacityReservationFleetId', 'failedCapacityReservationFleetCancellationResult_capacityReservationFleetId' - The ID of the Capacity Reservation Fleet that could not be cancelled.
newFailedCapacityReservationFleetCancellationResult ::
  FailedCapacityReservationFleetCancellationResult
newFailedCapacityReservationFleetCancellationResult =
  FailedCapacityReservationFleetCancellationResult'
    { cancelCapacityReservationFleetError =
        Prelude.Nothing,
      capacityReservationFleetId =
        Prelude.Nothing
    }

-- | Information about the Capacity Reservation Fleet cancellation error.
failedCapacityReservationFleetCancellationResult_cancelCapacityReservationFleetError :: Lens.Lens' FailedCapacityReservationFleetCancellationResult (Prelude.Maybe CancelCapacityReservationFleetError)
failedCapacityReservationFleetCancellationResult_cancelCapacityReservationFleetError = Lens.lens (\FailedCapacityReservationFleetCancellationResult' {cancelCapacityReservationFleetError} -> cancelCapacityReservationFleetError) (\s@FailedCapacityReservationFleetCancellationResult' {} a -> s {cancelCapacityReservationFleetError = a} :: FailedCapacityReservationFleetCancellationResult)

-- | The ID of the Capacity Reservation Fleet that could not be cancelled.
failedCapacityReservationFleetCancellationResult_capacityReservationFleetId :: Lens.Lens' FailedCapacityReservationFleetCancellationResult (Prelude.Maybe Prelude.Text)
failedCapacityReservationFleetCancellationResult_capacityReservationFleetId = Lens.lens (\FailedCapacityReservationFleetCancellationResult' {capacityReservationFleetId} -> capacityReservationFleetId) (\s@FailedCapacityReservationFleetCancellationResult' {} a -> s {capacityReservationFleetId = a} :: FailedCapacityReservationFleetCancellationResult)

instance
  Data.FromXML
    FailedCapacityReservationFleetCancellationResult
  where
  parseXML x =
    FailedCapacityReservationFleetCancellationResult'
      Prelude.<$> (x Data..@? "cancelCapacityReservationFleetError")
      Prelude.<*> (x Data..@? "capacityReservationFleetId")

instance
  Prelude.Hashable
    FailedCapacityReservationFleetCancellationResult
  where
  hashWithSalt
    _salt
    FailedCapacityReservationFleetCancellationResult' {..} =
      _salt
        `Prelude.hashWithSalt` cancelCapacityReservationFleetError
        `Prelude.hashWithSalt` capacityReservationFleetId

instance
  Prelude.NFData
    FailedCapacityReservationFleetCancellationResult
  where
  rnf
    FailedCapacityReservationFleetCancellationResult' {..} =
      Prelude.rnf cancelCapacityReservationFleetError
        `Prelude.seq` Prelude.rnf capacityReservationFleetId
