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
-- Module      : Network.AWS.EC2.Types.FailedCapacityReservationFleetCancellationResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FailedCapacityReservationFleetCancellationResult where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.CancelCapacityReservationFleetError
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a Capacity Reservation Fleet that could not be cancelled.
--
-- /See:/ 'newFailedCapacityReservationFleetCancellationResult' smart constructor.
data FailedCapacityReservationFleetCancellationResult = FailedCapacityReservationFleetCancellationResult'
  { -- | The ID of the Capacity Reservation Fleet that could not be cancelled.
    capacityReservationFleetId :: Prelude.Maybe Prelude.Text,
    -- | Information about the Capacity Reservation Fleet cancellation error.
    cancelCapacityReservationFleetError :: Prelude.Maybe CancelCapacityReservationFleetError
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
-- 'capacityReservationFleetId', 'failedCapacityReservationFleetCancellationResult_capacityReservationFleetId' - The ID of the Capacity Reservation Fleet that could not be cancelled.
--
-- 'cancelCapacityReservationFleetError', 'failedCapacityReservationFleetCancellationResult_cancelCapacityReservationFleetError' - Information about the Capacity Reservation Fleet cancellation error.
newFailedCapacityReservationFleetCancellationResult ::
  FailedCapacityReservationFleetCancellationResult
newFailedCapacityReservationFleetCancellationResult =
  FailedCapacityReservationFleetCancellationResult'
    { capacityReservationFleetId =
        Prelude.Nothing,
      cancelCapacityReservationFleetError =
        Prelude.Nothing
    }

-- | The ID of the Capacity Reservation Fleet that could not be cancelled.
failedCapacityReservationFleetCancellationResult_capacityReservationFleetId :: Lens.Lens' FailedCapacityReservationFleetCancellationResult (Prelude.Maybe Prelude.Text)
failedCapacityReservationFleetCancellationResult_capacityReservationFleetId = Lens.lens (\FailedCapacityReservationFleetCancellationResult' {capacityReservationFleetId} -> capacityReservationFleetId) (\s@FailedCapacityReservationFleetCancellationResult' {} a -> s {capacityReservationFleetId = a} :: FailedCapacityReservationFleetCancellationResult)

-- | Information about the Capacity Reservation Fleet cancellation error.
failedCapacityReservationFleetCancellationResult_cancelCapacityReservationFleetError :: Lens.Lens' FailedCapacityReservationFleetCancellationResult (Prelude.Maybe CancelCapacityReservationFleetError)
failedCapacityReservationFleetCancellationResult_cancelCapacityReservationFleetError = Lens.lens (\FailedCapacityReservationFleetCancellationResult' {cancelCapacityReservationFleetError} -> cancelCapacityReservationFleetError) (\s@FailedCapacityReservationFleetCancellationResult' {} a -> s {cancelCapacityReservationFleetError = a} :: FailedCapacityReservationFleetCancellationResult)

instance
  Core.FromXML
    FailedCapacityReservationFleetCancellationResult
  where
  parseXML x =
    FailedCapacityReservationFleetCancellationResult'
      Prelude.<$> (x Core..@? "capacityReservationFleetId")
        Prelude.<*> (x Core..@? "cancelCapacityReservationFleetError")

instance
  Prelude.Hashable
    FailedCapacityReservationFleetCancellationResult

instance
  Prelude.NFData
    FailedCapacityReservationFleetCancellationResult
