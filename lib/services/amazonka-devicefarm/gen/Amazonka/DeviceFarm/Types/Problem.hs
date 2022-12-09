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
  { -- | Information about the associated device.
    device :: Prelude.Maybe Device,
    -- | Information about the associated job.
    job :: Prelude.Maybe ProblemDetail,
    -- | A message about the problem\'s result.
    message :: Prelude.Maybe Prelude.Text,
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
    result :: Prelude.Maybe ExecutionResult,
    -- | Information about the associated run.
    run :: Prelude.Maybe ProblemDetail,
    -- | Information about the associated suite.
    suite :: Prelude.Maybe ProblemDetail,
    -- | Information about the associated test.
    test :: Prelude.Maybe ProblemDetail
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
-- 'device', 'problem_device' - Information about the associated device.
--
-- 'job', 'problem_job' - Information about the associated job.
--
-- 'message', 'problem_message' - A message about the problem\'s result.
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
--
-- 'run', 'problem_run' - Information about the associated run.
--
-- 'suite', 'problem_suite' - Information about the associated suite.
--
-- 'test', 'problem_test' - Information about the associated test.
newProblem ::
  Problem
newProblem =
  Problem'
    { device = Prelude.Nothing,
      job = Prelude.Nothing,
      message = Prelude.Nothing,
      result = Prelude.Nothing,
      run = Prelude.Nothing,
      suite = Prelude.Nothing,
      test = Prelude.Nothing
    }

-- | Information about the associated device.
problem_device :: Lens.Lens' Problem (Prelude.Maybe Device)
problem_device = Lens.lens (\Problem' {device} -> device) (\s@Problem' {} a -> s {device = a} :: Problem)

-- | Information about the associated job.
problem_job :: Lens.Lens' Problem (Prelude.Maybe ProblemDetail)
problem_job = Lens.lens (\Problem' {job} -> job) (\s@Problem' {} a -> s {job = a} :: Problem)

-- | A message about the problem\'s result.
problem_message :: Lens.Lens' Problem (Prelude.Maybe Prelude.Text)
problem_message = Lens.lens (\Problem' {message} -> message) (\s@Problem' {} a -> s {message = a} :: Problem)

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

-- | Information about the associated run.
problem_run :: Lens.Lens' Problem (Prelude.Maybe ProblemDetail)
problem_run = Lens.lens (\Problem' {run} -> run) (\s@Problem' {} a -> s {run = a} :: Problem)

-- | Information about the associated suite.
problem_suite :: Lens.Lens' Problem (Prelude.Maybe ProblemDetail)
problem_suite = Lens.lens (\Problem' {suite} -> suite) (\s@Problem' {} a -> s {suite = a} :: Problem)

-- | Information about the associated test.
problem_test :: Lens.Lens' Problem (Prelude.Maybe ProblemDetail)
problem_test = Lens.lens (\Problem' {test} -> test) (\s@Problem' {} a -> s {test = a} :: Problem)

instance Data.FromJSON Problem where
  parseJSON =
    Data.withObject
      "Problem"
      ( \x ->
          Problem'
            Prelude.<$> (x Data..:? "device")
            Prelude.<*> (x Data..:? "job")
            Prelude.<*> (x Data..:? "message")
            Prelude.<*> (x Data..:? "result")
            Prelude.<*> (x Data..:? "run")
            Prelude.<*> (x Data..:? "suite")
            Prelude.<*> (x Data..:? "test")
      )

instance Prelude.Hashable Problem where
  hashWithSalt _salt Problem' {..} =
    _salt `Prelude.hashWithSalt` device
      `Prelude.hashWithSalt` job
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` result
      `Prelude.hashWithSalt` run
      `Prelude.hashWithSalt` suite
      `Prelude.hashWithSalt` test

instance Prelude.NFData Problem where
  rnf Problem' {..} =
    Prelude.rnf device
      `Prelude.seq` Prelude.rnf job
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf result
      `Prelude.seq` Prelude.rnf run
      `Prelude.seq` Prelude.rnf suite
      `Prelude.seq` Prelude.rnf test
