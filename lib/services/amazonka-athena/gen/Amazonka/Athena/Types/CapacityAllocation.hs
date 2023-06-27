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
-- Module      : Amazonka.Athena.Types.CapacityAllocation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.CapacityAllocation where

import Amazonka.Athena.Types.CapacityAllocationStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the submission time of a single allocation request for a
-- capacity reservation and the most recent status of the attempted
-- allocation.
--
-- /See:/ 'newCapacityAllocation' smart constructor.
data CapacityAllocation = CapacityAllocation'
  { -- | The time when the capacity allocation request was completed.
    requestCompletionTime :: Prelude.Maybe Data.POSIX,
    -- | The status message of the capacity allocation.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The status of the capacity allocation.
    status :: CapacityAllocationStatus,
    -- | The time when the capacity allocation was requested.
    requestTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CapacityAllocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestCompletionTime', 'capacityAllocation_requestCompletionTime' - The time when the capacity allocation request was completed.
--
-- 'statusMessage', 'capacityAllocation_statusMessage' - The status message of the capacity allocation.
--
-- 'status', 'capacityAllocation_status' - The status of the capacity allocation.
--
-- 'requestTime', 'capacityAllocation_requestTime' - The time when the capacity allocation was requested.
newCapacityAllocation ::
  -- | 'status'
  CapacityAllocationStatus ->
  -- | 'requestTime'
  Prelude.UTCTime ->
  CapacityAllocation
newCapacityAllocation pStatus_ pRequestTime_ =
  CapacityAllocation'
    { requestCompletionTime =
        Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      status = pStatus_,
      requestTime = Data._Time Lens.# pRequestTime_
    }

-- | The time when the capacity allocation request was completed.
capacityAllocation_requestCompletionTime :: Lens.Lens' CapacityAllocation (Prelude.Maybe Prelude.UTCTime)
capacityAllocation_requestCompletionTime = Lens.lens (\CapacityAllocation' {requestCompletionTime} -> requestCompletionTime) (\s@CapacityAllocation' {} a -> s {requestCompletionTime = a} :: CapacityAllocation) Prelude.. Lens.mapping Data._Time

-- | The status message of the capacity allocation.
capacityAllocation_statusMessage :: Lens.Lens' CapacityAllocation (Prelude.Maybe Prelude.Text)
capacityAllocation_statusMessage = Lens.lens (\CapacityAllocation' {statusMessage} -> statusMessage) (\s@CapacityAllocation' {} a -> s {statusMessage = a} :: CapacityAllocation)

-- | The status of the capacity allocation.
capacityAllocation_status :: Lens.Lens' CapacityAllocation CapacityAllocationStatus
capacityAllocation_status = Lens.lens (\CapacityAllocation' {status} -> status) (\s@CapacityAllocation' {} a -> s {status = a} :: CapacityAllocation)

-- | The time when the capacity allocation was requested.
capacityAllocation_requestTime :: Lens.Lens' CapacityAllocation Prelude.UTCTime
capacityAllocation_requestTime = Lens.lens (\CapacityAllocation' {requestTime} -> requestTime) (\s@CapacityAllocation' {} a -> s {requestTime = a} :: CapacityAllocation) Prelude.. Data._Time

instance Data.FromJSON CapacityAllocation where
  parseJSON =
    Data.withObject
      "CapacityAllocation"
      ( \x ->
          CapacityAllocation'
            Prelude.<$> (x Data..:? "RequestCompletionTime")
            Prelude.<*> (x Data..:? "StatusMessage")
            Prelude.<*> (x Data..: "Status")
            Prelude.<*> (x Data..: "RequestTime")
      )

instance Prelude.Hashable CapacityAllocation where
  hashWithSalt _salt CapacityAllocation' {..} =
    _salt
      `Prelude.hashWithSalt` requestCompletionTime
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` requestTime

instance Prelude.NFData CapacityAllocation where
  rnf CapacityAllocation' {..} =
    Prelude.rnf requestCompletionTime
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf requestTime
