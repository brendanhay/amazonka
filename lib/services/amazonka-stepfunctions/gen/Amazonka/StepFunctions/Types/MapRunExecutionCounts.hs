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
-- Module      : Amazonka.StepFunctions.Types.MapRunExecutionCounts
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StepFunctions.Types.MapRunExecutionCounts where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains details about all of the child workflow executions started by a
-- Map Run.
--
-- /See:/ 'newMapRunExecutionCounts' smart constructor.
data MapRunExecutionCounts = MapRunExecutionCounts'
  { -- | The total number of child workflow executions that were started by a Map
    -- Run, but haven\'t started executing yet.
    pending :: Prelude.Natural,
    -- | The total number of child workflow executions that were started by a Map
    -- Run and are currently in-progress.
    running :: Prelude.Natural,
    -- | The total number of child workflow executions that were started by a Map
    -- Run and have completed successfully.
    succeeded :: Prelude.Natural,
    -- | The total number of child workflow executions that were started by a Map
    -- Run, but have failed.
    failed :: Prelude.Natural,
    -- | The total number of child workflow executions that were started by a Map
    -- Run and have timed out.
    timedOut :: Prelude.Natural,
    -- | The total number of child workflow executions that were started by a Map
    -- Run and were running, but were either stopped by the user or by Step
    -- Functions because the Map Run failed.
    aborted :: Prelude.Natural,
    -- | The total number of child workflow executions that were started by a Map
    -- Run.
    total :: Prelude.Natural,
    -- | Returns the count of child workflow executions whose results were
    -- written by @ResultWriter@. For more information, see
    -- <https://docs.aws.amazon.com/step-functions/latest/dg/input-output-resultwriter.html ResultWriter>
    -- in the /Step Functions Developer Guide/.
    resultsWritten :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MapRunExecutionCounts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pending', 'mapRunExecutionCounts_pending' - The total number of child workflow executions that were started by a Map
-- Run, but haven\'t started executing yet.
--
-- 'running', 'mapRunExecutionCounts_running' - The total number of child workflow executions that were started by a Map
-- Run and are currently in-progress.
--
-- 'succeeded', 'mapRunExecutionCounts_succeeded' - The total number of child workflow executions that were started by a Map
-- Run and have completed successfully.
--
-- 'failed', 'mapRunExecutionCounts_failed' - The total number of child workflow executions that were started by a Map
-- Run, but have failed.
--
-- 'timedOut', 'mapRunExecutionCounts_timedOut' - The total number of child workflow executions that were started by a Map
-- Run and have timed out.
--
-- 'aborted', 'mapRunExecutionCounts_aborted' - The total number of child workflow executions that were started by a Map
-- Run and were running, but were either stopped by the user or by Step
-- Functions because the Map Run failed.
--
-- 'total', 'mapRunExecutionCounts_total' - The total number of child workflow executions that were started by a Map
-- Run.
--
-- 'resultsWritten', 'mapRunExecutionCounts_resultsWritten' - Returns the count of child workflow executions whose results were
-- written by @ResultWriter@. For more information, see
-- <https://docs.aws.amazon.com/step-functions/latest/dg/input-output-resultwriter.html ResultWriter>
-- in the /Step Functions Developer Guide/.
newMapRunExecutionCounts ::
  -- | 'pending'
  Prelude.Natural ->
  -- | 'running'
  Prelude.Natural ->
  -- | 'succeeded'
  Prelude.Natural ->
  -- | 'failed'
  Prelude.Natural ->
  -- | 'timedOut'
  Prelude.Natural ->
  -- | 'aborted'
  Prelude.Natural ->
  -- | 'total'
  Prelude.Natural ->
  -- | 'resultsWritten'
  Prelude.Natural ->
  MapRunExecutionCounts
newMapRunExecutionCounts
  pPending_
  pRunning_
  pSucceeded_
  pFailed_
  pTimedOut_
  pAborted_
  pTotal_
  pResultsWritten_ =
    MapRunExecutionCounts'
      { pending = pPending_,
        running = pRunning_,
        succeeded = pSucceeded_,
        failed = pFailed_,
        timedOut = pTimedOut_,
        aborted = pAborted_,
        total = pTotal_,
        resultsWritten = pResultsWritten_
      }

-- | The total number of child workflow executions that were started by a Map
-- Run, but haven\'t started executing yet.
mapRunExecutionCounts_pending :: Lens.Lens' MapRunExecutionCounts Prelude.Natural
mapRunExecutionCounts_pending = Lens.lens (\MapRunExecutionCounts' {pending} -> pending) (\s@MapRunExecutionCounts' {} a -> s {pending = a} :: MapRunExecutionCounts)

-- | The total number of child workflow executions that were started by a Map
-- Run and are currently in-progress.
mapRunExecutionCounts_running :: Lens.Lens' MapRunExecutionCounts Prelude.Natural
mapRunExecutionCounts_running = Lens.lens (\MapRunExecutionCounts' {running} -> running) (\s@MapRunExecutionCounts' {} a -> s {running = a} :: MapRunExecutionCounts)

-- | The total number of child workflow executions that were started by a Map
-- Run and have completed successfully.
mapRunExecutionCounts_succeeded :: Lens.Lens' MapRunExecutionCounts Prelude.Natural
mapRunExecutionCounts_succeeded = Lens.lens (\MapRunExecutionCounts' {succeeded} -> succeeded) (\s@MapRunExecutionCounts' {} a -> s {succeeded = a} :: MapRunExecutionCounts)

-- | The total number of child workflow executions that were started by a Map
-- Run, but have failed.
mapRunExecutionCounts_failed :: Lens.Lens' MapRunExecutionCounts Prelude.Natural
mapRunExecutionCounts_failed = Lens.lens (\MapRunExecutionCounts' {failed} -> failed) (\s@MapRunExecutionCounts' {} a -> s {failed = a} :: MapRunExecutionCounts)

-- | The total number of child workflow executions that were started by a Map
-- Run and have timed out.
mapRunExecutionCounts_timedOut :: Lens.Lens' MapRunExecutionCounts Prelude.Natural
mapRunExecutionCounts_timedOut = Lens.lens (\MapRunExecutionCounts' {timedOut} -> timedOut) (\s@MapRunExecutionCounts' {} a -> s {timedOut = a} :: MapRunExecutionCounts)

-- | The total number of child workflow executions that were started by a Map
-- Run and were running, but were either stopped by the user or by Step
-- Functions because the Map Run failed.
mapRunExecutionCounts_aborted :: Lens.Lens' MapRunExecutionCounts Prelude.Natural
mapRunExecutionCounts_aborted = Lens.lens (\MapRunExecutionCounts' {aborted} -> aborted) (\s@MapRunExecutionCounts' {} a -> s {aborted = a} :: MapRunExecutionCounts)

-- | The total number of child workflow executions that were started by a Map
-- Run.
mapRunExecutionCounts_total :: Lens.Lens' MapRunExecutionCounts Prelude.Natural
mapRunExecutionCounts_total = Lens.lens (\MapRunExecutionCounts' {total} -> total) (\s@MapRunExecutionCounts' {} a -> s {total = a} :: MapRunExecutionCounts)

-- | Returns the count of child workflow executions whose results were
-- written by @ResultWriter@. For more information, see
-- <https://docs.aws.amazon.com/step-functions/latest/dg/input-output-resultwriter.html ResultWriter>
-- in the /Step Functions Developer Guide/.
mapRunExecutionCounts_resultsWritten :: Lens.Lens' MapRunExecutionCounts Prelude.Natural
mapRunExecutionCounts_resultsWritten = Lens.lens (\MapRunExecutionCounts' {resultsWritten} -> resultsWritten) (\s@MapRunExecutionCounts' {} a -> s {resultsWritten = a} :: MapRunExecutionCounts)

instance Data.FromJSON MapRunExecutionCounts where
  parseJSON =
    Data.withObject
      "MapRunExecutionCounts"
      ( \x ->
          MapRunExecutionCounts'
            Prelude.<$> (x Data..: "pending")
            Prelude.<*> (x Data..: "running")
            Prelude.<*> (x Data..: "succeeded")
            Prelude.<*> (x Data..: "failed")
            Prelude.<*> (x Data..: "timedOut")
            Prelude.<*> (x Data..: "aborted")
            Prelude.<*> (x Data..: "total")
            Prelude.<*> (x Data..: "resultsWritten")
      )

instance Prelude.Hashable MapRunExecutionCounts where
  hashWithSalt _salt MapRunExecutionCounts' {..} =
    _salt
      `Prelude.hashWithSalt` pending
      `Prelude.hashWithSalt` running
      `Prelude.hashWithSalt` succeeded
      `Prelude.hashWithSalt` failed
      `Prelude.hashWithSalt` timedOut
      `Prelude.hashWithSalt` aborted
      `Prelude.hashWithSalt` total
      `Prelude.hashWithSalt` resultsWritten

instance Prelude.NFData MapRunExecutionCounts where
  rnf MapRunExecutionCounts' {..} =
    Prelude.rnf pending
      `Prelude.seq` Prelude.rnf running
      `Prelude.seq` Prelude.rnf succeeded
      `Prelude.seq` Prelude.rnf failed
      `Prelude.seq` Prelude.rnf timedOut
      `Prelude.seq` Prelude.rnf aborted
      `Prelude.seq` Prelude.rnf total
      `Prelude.seq` Prelude.rnf resultsWritten
