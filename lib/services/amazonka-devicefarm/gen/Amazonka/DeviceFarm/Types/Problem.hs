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
-- Module      : Amazonka.DeviceFarm.Types.Problem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types.Problem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DeviceFarm.Types.Device
import Amazonka.DeviceFarm.Types.ExecutionResult
import Amazonka.DeviceFarm.Types.ProblemDetail
import qualified Amazonka.Prelude as Prelude

-- | Represents a specific warning or failure.
--
-- /See:/ 'newProblem' smart constructor.
data Problem = Problem'
  { -- | A message about the problem\'s result.
    message :: Prelude.Maybe Prelude.Text,
    -- | Information about the associated suite.
    suite :: Prelude.Maybe ProblemDetail,
    -- | Information about the associated device.
    device :: Prelude.Maybe Device,
    -- | Information about the associated run.
    run :: Prelude.Maybe ProblemDetail,
    -- | Information about the associated job.
    job :: Prelude.Maybe ProblemDetail,
    -- | Information about the associated test.
    test :: Prelude.Maybe ProblemDetail,
    -- | The problem\'s result.
    --
    -- Allowed values include:
    --
    -- -   PENDING
    --
    -- -   PASSED
    --
    -- -   WARNED
    --
    -- -   FAILED
    --
    -- -   SKIPPED
    --
    -- -   ERRORED
    --
    -- -   STOPPED
    result :: Prelude.Maybe ExecutionResult
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Problem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'problem_message' - A message about the problem\'s result.
--
-- 'suite', 'problem_suite' - Information about the associated suite.
--
-- 'device', 'problem_device' - Information about the associated device.
--
-- 'run', 'problem_run' - Information about the associated run.
--
-- 'job', 'problem_job' - Information about the associated job.
--
-- 'test', 'problem_test' - Information about the associated test.
--
-- 'result', 'problem_result' - The problem\'s result.
--
-- Allowed values include:
--
-- -   PENDING
--
-- -   PASSED
--
-- -   WARNED
--
-- -   FAILED
--
-- -   SKIPPED
--
-- -   ERRORED
--
-- -   STOPPED
newProblem ::
  Problem
newProblem =
  Problem'
    { message = Prelude.Nothing,
      suite = Prelude.Nothing,
      device = Prelude.Nothing,
      run = Prelude.Nothing,
      job = Prelude.Nothing,
      test = Prelude.Nothing,
      result = Prelude.Nothing
    }

-- | A message about the problem\'s result.
problem_message :: Lens.Lens' Problem (Prelude.Maybe Prelude.Text)
problem_message = Lens.lens (\Problem' {message} -> message) (\s@Problem' {} a -> s {message = a} :: Problem)

-- | Information about the associated suite.
problem_suite :: Lens.Lens' Problem (Prelude.Maybe ProblemDetail)
problem_suite = Lens.lens (\Problem' {suite} -> suite) (\s@Problem' {} a -> s {suite = a} :: Problem)

-- | Information about the associated device.
problem_device :: Lens.Lens' Problem (Prelude.Maybe Device)
problem_device = Lens.lens (\Problem' {device} -> device) (\s@Problem' {} a -> s {device = a} :: Problem)

-- | Information about the associated run.
problem_run :: Lens.Lens' Problem (Prelude.Maybe ProblemDetail)
problem_run = Lens.lens (\Problem' {run} -> run) (\s@Problem' {} a -> s {run = a} :: Problem)

-- | Information about the associated job.
problem_job :: Lens.Lens' Problem (Prelude.Maybe ProblemDetail)
problem_job = Lens.lens (\Problem' {job} -> job) (\s@Problem' {} a -> s {job = a} :: Problem)

-- | Information about the associated test.
problem_test :: Lens.Lens' Problem (Prelude.Maybe ProblemDetail)
problem_test = Lens.lens (\Problem' {test} -> test) (\s@Problem' {} a -> s {test = a} :: Problem)

-- | The problem\'s result.
--
-- Allowed values include:
--
-- -   PENDING
--
-- -   PASSED
--
-- -   WARNED
--
-- -   FAILED
--
-- -   SKIPPED
--
-- -   ERRORED
--
-- -   STOPPED
problem_result :: Lens.Lens' Problem (Prelude.Maybe ExecutionResult)
problem_result = Lens.lens (\Problem' {result} -> result) (\s@Problem' {} a -> s {result = a} :: Problem)

instance Data.FromJSON Problem where
  parseJSON =
    Data.withObject
      "Problem"
      ( \x ->
          Problem'
            Prelude.<$> (x Data..:? "message")
            Prelude.<*> (x Data..:? "suite")
            Prelude.<*> (x Data..:? "device")
            Prelude.<*> (x Data..:? "run")
            Prelude.<*> (x Data..:? "job")
            Prelude.<*> (x Data..:? "test")
            Prelude.<*> (x Data..:? "result")
      )

instance Prelude.Hashable Problem where
  hashWithSalt _salt Problem' {..} =
    _salt `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` suite
      `Prelude.hashWithSalt` device
      `Prelude.hashWithSalt` run
      `Prelude.hashWithSalt` job
      `Prelude.hashWithSalt` test
      `Prelude.hashWithSalt` result

instance Prelude.NFData Problem where
  rnf Problem' {..} =
    Prelude.rnf message
      `Prelude.seq` Prelude.rnf suite
      `Prelude.seq` Prelude.rnf device
      `Prelude.seq` Prelude.rnf run
      `Prelude.seq` Prelude.rnf job
      `Prelude.seq` Prelude.rnf test
      `Prelude.seq` Prelude.rnf result
