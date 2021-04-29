{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.DeviceFarm.Types.Problem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.Problem where

import Network.AWS.DeviceFarm.Types.Device
import Network.AWS.DeviceFarm.Types.ExecutionResult
import Network.AWS.DeviceFarm.Types.ProblemDetail
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents a specific warning or failure.
--
-- /See:/ 'newProblem' smart constructor.
data Problem = Problem'
  { -- | Information about the associated job.
    job :: Prelude.Maybe ProblemDetail,
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
    -- | A message about the problem\'s result.
    message :: Prelude.Maybe Prelude.Text,
    -- | Information about the associated device.
    device :: Prelude.Maybe Device,
    -- | Information about the associated run.
    run :: Prelude.Maybe ProblemDetail,
    -- | Information about the associated test.
    test :: Prelude.Maybe ProblemDetail,
    -- | Information about the associated suite.
    suite :: Prelude.Maybe ProblemDetail
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Problem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'job', 'problem_job' - Information about the associated job.
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
-- 'message', 'problem_message' - A message about the problem\'s result.
--
-- 'device', 'problem_device' - Information about the associated device.
--
-- 'run', 'problem_run' - Information about the associated run.
--
-- 'test', 'problem_test' - Information about the associated test.
--
-- 'suite', 'problem_suite' - Information about the associated suite.
newProblem ::
  Problem
newProblem =
  Problem'
    { job = Prelude.Nothing,
      result = Prelude.Nothing,
      message = Prelude.Nothing,
      device = Prelude.Nothing,
      run = Prelude.Nothing,
      test = Prelude.Nothing,
      suite = Prelude.Nothing
    }

-- | Information about the associated job.
problem_job :: Lens.Lens' Problem (Prelude.Maybe ProblemDetail)
problem_job = Lens.lens (\Problem' {job} -> job) (\s@Problem' {} a -> s {job = a} :: Problem)

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

-- | A message about the problem\'s result.
problem_message :: Lens.Lens' Problem (Prelude.Maybe Prelude.Text)
problem_message = Lens.lens (\Problem' {message} -> message) (\s@Problem' {} a -> s {message = a} :: Problem)

-- | Information about the associated device.
problem_device :: Lens.Lens' Problem (Prelude.Maybe Device)
problem_device = Lens.lens (\Problem' {device} -> device) (\s@Problem' {} a -> s {device = a} :: Problem)

-- | Information about the associated run.
problem_run :: Lens.Lens' Problem (Prelude.Maybe ProblemDetail)
problem_run = Lens.lens (\Problem' {run} -> run) (\s@Problem' {} a -> s {run = a} :: Problem)

-- | Information about the associated test.
problem_test :: Lens.Lens' Problem (Prelude.Maybe ProblemDetail)
problem_test = Lens.lens (\Problem' {test} -> test) (\s@Problem' {} a -> s {test = a} :: Problem)

-- | Information about the associated suite.
problem_suite :: Lens.Lens' Problem (Prelude.Maybe ProblemDetail)
problem_suite = Lens.lens (\Problem' {suite} -> suite) (\s@Problem' {} a -> s {suite = a} :: Problem)

instance Prelude.FromJSON Problem where
  parseJSON =
    Prelude.withObject
      "Problem"
      ( \x ->
          Problem'
            Prelude.<$> (x Prelude..:? "job")
            Prelude.<*> (x Prelude..:? "result")
            Prelude.<*> (x Prelude..:? "message")
            Prelude.<*> (x Prelude..:? "device")
            Prelude.<*> (x Prelude..:? "run")
            Prelude.<*> (x Prelude..:? "test")
            Prelude.<*> (x Prelude..:? "suite")
      )

instance Prelude.Hashable Problem

instance Prelude.NFData Problem
