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
-- Module      : Amazonka.Athena.Types.CalculationStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.CalculationStatus where

import Amazonka.Athena.Types.CalculationExecutionState
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the status of a notebook calculation.
--
-- /See:/ 'newCalculationStatus' smart constructor.
data CalculationStatus = CalculationStatus'
  { -- | The date and time the calculation completed processing.
    completionDateTime :: Prelude.Maybe Data.POSIX,
    -- | The state of the calculation execution. A description of each state
    -- follows.
    --
    -- @CREATING@ - The calculation is in the process of being created.
    --
    -- @CREATED@ - The calculation has been created and is ready to run.
    --
    -- @QUEUED@ - The calculation has been queued for processing.
    --
    -- @RUNNING@ - The calculation is running.
    --
    -- @CANCELING@ - A request to cancel the calculation has been received and
    -- the system is working to stop it.
    --
    -- @CANCELED@ - The calculation is no longer running as the result of a
    -- cancel request.
    --
    -- @COMPLETED@ - The calculation has completed without error.
    --
    -- @FAILED@ - The calculation failed and is no longer running.
    state :: Prelude.Maybe CalculationExecutionState,
    -- | The reason for the calculation state change (for example, the
    -- calculation was canceled because the session was terminated).
    stateChangeReason :: Prelude.Maybe Prelude.Text,
    -- | The date and time the calculation was submitted for processing.
    submissionDateTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CalculationStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'completionDateTime', 'calculationStatus_completionDateTime' - The date and time the calculation completed processing.
--
-- 'state', 'calculationStatus_state' - The state of the calculation execution. A description of each state
-- follows.
--
-- @CREATING@ - The calculation is in the process of being created.
--
-- @CREATED@ - The calculation has been created and is ready to run.
--
-- @QUEUED@ - The calculation has been queued for processing.
--
-- @RUNNING@ - The calculation is running.
--
-- @CANCELING@ - A request to cancel the calculation has been received and
-- the system is working to stop it.
--
-- @CANCELED@ - The calculation is no longer running as the result of a
-- cancel request.
--
-- @COMPLETED@ - The calculation has completed without error.
--
-- @FAILED@ - The calculation failed and is no longer running.
--
-- 'stateChangeReason', 'calculationStatus_stateChangeReason' - The reason for the calculation state change (for example, the
-- calculation was canceled because the session was terminated).
--
-- 'submissionDateTime', 'calculationStatus_submissionDateTime' - The date and time the calculation was submitted for processing.
newCalculationStatus ::
  CalculationStatus
newCalculationStatus =
  CalculationStatus'
    { completionDateTime =
        Prelude.Nothing,
      state = Prelude.Nothing,
      stateChangeReason = Prelude.Nothing,
      submissionDateTime = Prelude.Nothing
    }

-- | The date and time the calculation completed processing.
calculationStatus_completionDateTime :: Lens.Lens' CalculationStatus (Prelude.Maybe Prelude.UTCTime)
calculationStatus_completionDateTime = Lens.lens (\CalculationStatus' {completionDateTime} -> completionDateTime) (\s@CalculationStatus' {} a -> s {completionDateTime = a} :: CalculationStatus) Prelude.. Lens.mapping Data._Time

-- | The state of the calculation execution. A description of each state
-- follows.
--
-- @CREATING@ - The calculation is in the process of being created.
--
-- @CREATED@ - The calculation has been created and is ready to run.
--
-- @QUEUED@ - The calculation has been queued for processing.
--
-- @RUNNING@ - The calculation is running.
--
-- @CANCELING@ - A request to cancel the calculation has been received and
-- the system is working to stop it.
--
-- @CANCELED@ - The calculation is no longer running as the result of a
-- cancel request.
--
-- @COMPLETED@ - The calculation has completed without error.
--
-- @FAILED@ - The calculation failed and is no longer running.
calculationStatus_state :: Lens.Lens' CalculationStatus (Prelude.Maybe CalculationExecutionState)
calculationStatus_state = Lens.lens (\CalculationStatus' {state} -> state) (\s@CalculationStatus' {} a -> s {state = a} :: CalculationStatus)

-- | The reason for the calculation state change (for example, the
-- calculation was canceled because the session was terminated).
calculationStatus_stateChangeReason :: Lens.Lens' CalculationStatus (Prelude.Maybe Prelude.Text)
calculationStatus_stateChangeReason = Lens.lens (\CalculationStatus' {stateChangeReason} -> stateChangeReason) (\s@CalculationStatus' {} a -> s {stateChangeReason = a} :: CalculationStatus)

-- | The date and time the calculation was submitted for processing.
calculationStatus_submissionDateTime :: Lens.Lens' CalculationStatus (Prelude.Maybe Prelude.UTCTime)
calculationStatus_submissionDateTime = Lens.lens (\CalculationStatus' {submissionDateTime} -> submissionDateTime) (\s@CalculationStatus' {} a -> s {submissionDateTime = a} :: CalculationStatus) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON CalculationStatus where
  parseJSON =
    Data.withObject
      "CalculationStatus"
      ( \x ->
          CalculationStatus'
            Prelude.<$> (x Data..:? "CompletionDateTime")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "StateChangeReason")
            Prelude.<*> (x Data..:? "SubmissionDateTime")
      )

instance Prelude.Hashable CalculationStatus where
  hashWithSalt _salt CalculationStatus' {..} =
    _salt `Prelude.hashWithSalt` completionDateTime
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` stateChangeReason
      `Prelude.hashWithSalt` submissionDateTime

instance Prelude.NFData CalculationStatus where
  rnf CalculationStatus' {..} =
    Prelude.rnf completionDateTime
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf stateChangeReason
      `Prelude.seq` Prelude.rnf submissionDateTime
