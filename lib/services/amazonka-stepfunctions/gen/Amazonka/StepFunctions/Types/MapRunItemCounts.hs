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
-- Module      : Amazonka.StepFunctions.Types.MapRunItemCounts
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StepFunctions.Types.MapRunItemCounts where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains details about items that were processed in all of the child
-- workflow executions that were started by a Map Run.
--
-- /See:/ 'newMapRunItemCounts' smart constructor.
data MapRunItemCounts = MapRunItemCounts'
  { -- | The total number of items to process in child workflow executions that
    -- haven\'t started running yet.
    pending :: Prelude.Natural,
    -- | The total number of items being processed in child workflow executions
    -- that are currently in-progress.
    running :: Prelude.Natural,
    -- | The total number of items processed in child workflow executions that
    -- have completed successfully.
    succeeded :: Prelude.Natural,
    -- | The total number of items processed in child workflow executions that
    -- have failed.
    failed :: Prelude.Natural,
    -- | The total number of items processed in child workflow executions that
    -- have timed out.
    timedOut :: Prelude.Natural,
    -- | The total number of items processed in child workflow executions that
    -- were either stopped by the user or by Step Functions, because the Map
    -- Run failed.
    aborted :: Prelude.Natural,
    -- | The total number of items processed in all the child workflow executions
    -- started by a Map Run.
    total :: Prelude.Natural,
    -- | Returns the count of items whose results were written by @ResultWriter@.
    -- For more information, see
    -- <https://docs.aws.amazon.com/step-functions/latest/dg/input-output-resultwriter.html ResultWriter>
    -- in the /Step Functions Developer Guide/.
    resultsWritten :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MapRunItemCounts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pending', 'mapRunItemCounts_pending' - The total number of items to process in child workflow executions that
-- haven\'t started running yet.
--
-- 'running', 'mapRunItemCounts_running' - The total number of items being processed in child workflow executions
-- that are currently in-progress.
--
-- 'succeeded', 'mapRunItemCounts_succeeded' - The total number of items processed in child workflow executions that
-- have completed successfully.
--
-- 'failed', 'mapRunItemCounts_failed' - The total number of items processed in child workflow executions that
-- have failed.
--
-- 'timedOut', 'mapRunItemCounts_timedOut' - The total number of items processed in child workflow executions that
-- have timed out.
--
-- 'aborted', 'mapRunItemCounts_aborted' - The total number of items processed in child workflow executions that
-- were either stopped by the user or by Step Functions, because the Map
-- Run failed.
--
-- 'total', 'mapRunItemCounts_total' - The total number of items processed in all the child workflow executions
-- started by a Map Run.
--
-- 'resultsWritten', 'mapRunItemCounts_resultsWritten' - Returns the count of items whose results were written by @ResultWriter@.
-- For more information, see
-- <https://docs.aws.amazon.com/step-functions/latest/dg/input-output-resultwriter.html ResultWriter>
-- in the /Step Functions Developer Guide/.
newMapRunItemCounts ::
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
  MapRunItemCounts
newMapRunItemCounts
  pPending_
  pRunning_
  pSucceeded_
  pFailed_
  pTimedOut_
  pAborted_
  pTotal_
  pResultsWritten_ =
    MapRunItemCounts'
      { pending = pPending_,
        running = pRunning_,
        succeeded = pSucceeded_,
        failed = pFailed_,
        timedOut = pTimedOut_,
        aborted = pAborted_,
        total = pTotal_,
        resultsWritten = pResultsWritten_
      }

-- | The total number of items to process in child workflow executions that
-- haven\'t started running yet.
mapRunItemCounts_pending :: Lens.Lens' MapRunItemCounts Prelude.Natural
mapRunItemCounts_pending = Lens.lens (\MapRunItemCounts' {pending} -> pending) (\s@MapRunItemCounts' {} a -> s {pending = a} :: MapRunItemCounts)

-- | The total number of items being processed in child workflow executions
-- that are currently in-progress.
mapRunItemCounts_running :: Lens.Lens' MapRunItemCounts Prelude.Natural
mapRunItemCounts_running = Lens.lens (\MapRunItemCounts' {running} -> running) (\s@MapRunItemCounts' {} a -> s {running = a} :: MapRunItemCounts)

-- | The total number of items processed in child workflow executions that
-- have completed successfully.
mapRunItemCounts_succeeded :: Lens.Lens' MapRunItemCounts Prelude.Natural
mapRunItemCounts_succeeded = Lens.lens (\MapRunItemCounts' {succeeded} -> succeeded) (\s@MapRunItemCounts' {} a -> s {succeeded = a} :: MapRunItemCounts)

-- | The total number of items processed in child workflow executions that
-- have failed.
mapRunItemCounts_failed :: Lens.Lens' MapRunItemCounts Prelude.Natural
mapRunItemCounts_failed = Lens.lens (\MapRunItemCounts' {failed} -> failed) (\s@MapRunItemCounts' {} a -> s {failed = a} :: MapRunItemCounts)

-- | The total number of items processed in child workflow executions that
-- have timed out.
mapRunItemCounts_timedOut :: Lens.Lens' MapRunItemCounts Prelude.Natural
mapRunItemCounts_timedOut = Lens.lens (\MapRunItemCounts' {timedOut} -> timedOut) (\s@MapRunItemCounts' {} a -> s {timedOut = a} :: MapRunItemCounts)

-- | The total number of items processed in child workflow executions that
-- were either stopped by the user or by Step Functions, because the Map
-- Run failed.
mapRunItemCounts_aborted :: Lens.Lens' MapRunItemCounts Prelude.Natural
mapRunItemCounts_aborted = Lens.lens (\MapRunItemCounts' {aborted} -> aborted) (\s@MapRunItemCounts' {} a -> s {aborted = a} :: MapRunItemCounts)

-- | The total number of items processed in all the child workflow executions
-- started by a Map Run.
mapRunItemCounts_total :: Lens.Lens' MapRunItemCounts Prelude.Natural
mapRunItemCounts_total = Lens.lens (\MapRunItemCounts' {total} -> total) (\s@MapRunItemCounts' {} a -> s {total = a} :: MapRunItemCounts)

-- | Returns the count of items whose results were written by @ResultWriter@.
-- For more information, see
-- <https://docs.aws.amazon.com/step-functions/latest/dg/input-output-resultwriter.html ResultWriter>
-- in the /Step Functions Developer Guide/.
mapRunItemCounts_resultsWritten :: Lens.Lens' MapRunItemCounts Prelude.Natural
mapRunItemCounts_resultsWritten = Lens.lens (\MapRunItemCounts' {resultsWritten} -> resultsWritten) (\s@MapRunItemCounts' {} a -> s {resultsWritten = a} :: MapRunItemCounts)

instance Data.FromJSON MapRunItemCounts where
  parseJSON =
    Data.withObject
      "MapRunItemCounts"
      ( \x ->
          MapRunItemCounts'
            Prelude.<$> (x Data..: "pending")
            Prelude.<*> (x Data..: "running")
            Prelude.<*> (x Data..: "succeeded")
            Prelude.<*> (x Data..: "failed")
            Prelude.<*> (x Data..: "timedOut")
            Prelude.<*> (x Data..: "aborted")
            Prelude.<*> (x Data..: "total")
            Prelude.<*> (x Data..: "resultsWritten")
      )

instance Prelude.Hashable MapRunItemCounts where
  hashWithSalt _salt MapRunItemCounts' {..} =
    _salt `Prelude.hashWithSalt` pending
      `Prelude.hashWithSalt` running
      `Prelude.hashWithSalt` succeeded
      `Prelude.hashWithSalt` failed
      `Prelude.hashWithSalt` timedOut
      `Prelude.hashWithSalt` aborted
      `Prelude.hashWithSalt` total
      `Prelude.hashWithSalt` resultsWritten

instance Prelude.NFData MapRunItemCounts where
  rnf MapRunItemCounts' {..} =
    Prelude.rnf pending
      `Prelude.seq` Prelude.rnf running
      `Prelude.seq` Prelude.rnf succeeded
      `Prelude.seq` Prelude.rnf failed
      `Prelude.seq` Prelude.rnf timedOut
      `Prelude.seq` Prelude.rnf aborted
      `Prelude.seq` Prelude.rnf total
      `Prelude.seq` Prelude.rnf resultsWritten
