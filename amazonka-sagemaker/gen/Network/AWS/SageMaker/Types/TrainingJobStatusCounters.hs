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
-- Module      : Network.AWS.SageMaker.Types.TrainingJobStatusCounters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrainingJobStatusCounters where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The numbers of training jobs launched by a hyperparameter tuning job,
-- categorized by status.
--
-- /See:/ 'newTrainingJobStatusCounters' smart constructor.
data TrainingJobStatusCounters = TrainingJobStatusCounters'
  { -- | The number of training jobs launched by a hyperparameter tuning job that
    -- were manually stopped.
    stopped :: Core.Maybe Core.Natural,
    -- | The number of completed training jobs launched by the hyperparameter
    -- tuning job.
    completed :: Core.Maybe Core.Natural,
    -- | The number of training jobs that failed and can\'t be retried. A failed
    -- training job can\'t be retried if it failed because a client error
    -- occurred.
    nonRetryableError :: Core.Maybe Core.Natural,
    -- | The number of in-progress training jobs launched by a hyperparameter
    -- tuning job.
    inProgress :: Core.Maybe Core.Natural,
    -- | The number of training jobs that failed, but can be retried. A failed
    -- training job can be retried only if it failed because an internal
    -- service error occurred.
    retryableError :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TrainingJobStatusCounters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stopped', 'trainingJobStatusCounters_stopped' - The number of training jobs launched by a hyperparameter tuning job that
-- were manually stopped.
--
-- 'completed', 'trainingJobStatusCounters_completed' - The number of completed training jobs launched by the hyperparameter
-- tuning job.
--
-- 'nonRetryableError', 'trainingJobStatusCounters_nonRetryableError' - The number of training jobs that failed and can\'t be retried. A failed
-- training job can\'t be retried if it failed because a client error
-- occurred.
--
-- 'inProgress', 'trainingJobStatusCounters_inProgress' - The number of in-progress training jobs launched by a hyperparameter
-- tuning job.
--
-- 'retryableError', 'trainingJobStatusCounters_retryableError' - The number of training jobs that failed, but can be retried. A failed
-- training job can be retried only if it failed because an internal
-- service error occurred.
newTrainingJobStatusCounters ::
  TrainingJobStatusCounters
newTrainingJobStatusCounters =
  TrainingJobStatusCounters'
    { stopped = Core.Nothing,
      completed = Core.Nothing,
      nonRetryableError = Core.Nothing,
      inProgress = Core.Nothing,
      retryableError = Core.Nothing
    }

-- | The number of training jobs launched by a hyperparameter tuning job that
-- were manually stopped.
trainingJobStatusCounters_stopped :: Lens.Lens' TrainingJobStatusCounters (Core.Maybe Core.Natural)
trainingJobStatusCounters_stopped = Lens.lens (\TrainingJobStatusCounters' {stopped} -> stopped) (\s@TrainingJobStatusCounters' {} a -> s {stopped = a} :: TrainingJobStatusCounters)

-- | The number of completed training jobs launched by the hyperparameter
-- tuning job.
trainingJobStatusCounters_completed :: Lens.Lens' TrainingJobStatusCounters (Core.Maybe Core.Natural)
trainingJobStatusCounters_completed = Lens.lens (\TrainingJobStatusCounters' {completed} -> completed) (\s@TrainingJobStatusCounters' {} a -> s {completed = a} :: TrainingJobStatusCounters)

-- | The number of training jobs that failed and can\'t be retried. A failed
-- training job can\'t be retried if it failed because a client error
-- occurred.
trainingJobStatusCounters_nonRetryableError :: Lens.Lens' TrainingJobStatusCounters (Core.Maybe Core.Natural)
trainingJobStatusCounters_nonRetryableError = Lens.lens (\TrainingJobStatusCounters' {nonRetryableError} -> nonRetryableError) (\s@TrainingJobStatusCounters' {} a -> s {nonRetryableError = a} :: TrainingJobStatusCounters)

-- | The number of in-progress training jobs launched by a hyperparameter
-- tuning job.
trainingJobStatusCounters_inProgress :: Lens.Lens' TrainingJobStatusCounters (Core.Maybe Core.Natural)
trainingJobStatusCounters_inProgress = Lens.lens (\TrainingJobStatusCounters' {inProgress} -> inProgress) (\s@TrainingJobStatusCounters' {} a -> s {inProgress = a} :: TrainingJobStatusCounters)

-- | The number of training jobs that failed, but can be retried. A failed
-- training job can be retried only if it failed because an internal
-- service error occurred.
trainingJobStatusCounters_retryableError :: Lens.Lens' TrainingJobStatusCounters (Core.Maybe Core.Natural)
trainingJobStatusCounters_retryableError = Lens.lens (\TrainingJobStatusCounters' {retryableError} -> retryableError) (\s@TrainingJobStatusCounters' {} a -> s {retryableError = a} :: TrainingJobStatusCounters)

instance Core.FromJSON TrainingJobStatusCounters where
  parseJSON =
    Core.withObject
      "TrainingJobStatusCounters"
      ( \x ->
          TrainingJobStatusCounters'
            Core.<$> (x Core..:? "Stopped")
            Core.<*> (x Core..:? "Completed")
            Core.<*> (x Core..:? "NonRetryableError")
            Core.<*> (x Core..:? "InProgress")
            Core.<*> (x Core..:? "RetryableError")
      )

instance Core.Hashable TrainingJobStatusCounters

instance Core.NFData TrainingJobStatusCounters
