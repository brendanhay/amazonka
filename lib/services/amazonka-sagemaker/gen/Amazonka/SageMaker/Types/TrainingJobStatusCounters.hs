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
-- Module      : Amazonka.SageMaker.Types.TrainingJobStatusCounters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.TrainingJobStatusCounters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The numbers of training jobs launched by a hyperparameter tuning job,
-- categorized by status.
--
-- /See:/ 'newTrainingJobStatusCounters' smart constructor.
data TrainingJobStatusCounters = TrainingJobStatusCounters'
  { -- | The number of completed training jobs launched by the hyperparameter
    -- tuning job.
    completed :: Prelude.Maybe Prelude.Natural,
    -- | The number of in-progress training jobs launched by a hyperparameter
    -- tuning job.
    inProgress :: Prelude.Maybe Prelude.Natural,
    -- | The number of training jobs that failed and can\'t be retried. A failed
    -- training job can\'t be retried if it failed because a client error
    -- occurred.
    nonRetryableError :: Prelude.Maybe Prelude.Natural,
    -- | The number of training jobs that failed, but can be retried. A failed
    -- training job can be retried only if it failed because an internal
    -- service error occurred.
    retryableError :: Prelude.Maybe Prelude.Natural,
    -- | The number of training jobs launched by a hyperparameter tuning job that
    -- were manually stopped.
    stopped :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrainingJobStatusCounters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'completed', 'trainingJobStatusCounters_completed' - The number of completed training jobs launched by the hyperparameter
-- tuning job.
--
-- 'inProgress', 'trainingJobStatusCounters_inProgress' - The number of in-progress training jobs launched by a hyperparameter
-- tuning job.
--
-- 'nonRetryableError', 'trainingJobStatusCounters_nonRetryableError' - The number of training jobs that failed and can\'t be retried. A failed
-- training job can\'t be retried if it failed because a client error
-- occurred.
--
-- 'retryableError', 'trainingJobStatusCounters_retryableError' - The number of training jobs that failed, but can be retried. A failed
-- training job can be retried only if it failed because an internal
-- service error occurred.
--
-- 'stopped', 'trainingJobStatusCounters_stopped' - The number of training jobs launched by a hyperparameter tuning job that
-- were manually stopped.
newTrainingJobStatusCounters ::
  TrainingJobStatusCounters
newTrainingJobStatusCounters =
  TrainingJobStatusCounters'
    { completed =
        Prelude.Nothing,
      inProgress = Prelude.Nothing,
      nonRetryableError = Prelude.Nothing,
      retryableError = Prelude.Nothing,
      stopped = Prelude.Nothing
    }

-- | The number of completed training jobs launched by the hyperparameter
-- tuning job.
trainingJobStatusCounters_completed :: Lens.Lens' TrainingJobStatusCounters (Prelude.Maybe Prelude.Natural)
trainingJobStatusCounters_completed = Lens.lens (\TrainingJobStatusCounters' {completed} -> completed) (\s@TrainingJobStatusCounters' {} a -> s {completed = a} :: TrainingJobStatusCounters)

-- | The number of in-progress training jobs launched by a hyperparameter
-- tuning job.
trainingJobStatusCounters_inProgress :: Lens.Lens' TrainingJobStatusCounters (Prelude.Maybe Prelude.Natural)
trainingJobStatusCounters_inProgress = Lens.lens (\TrainingJobStatusCounters' {inProgress} -> inProgress) (\s@TrainingJobStatusCounters' {} a -> s {inProgress = a} :: TrainingJobStatusCounters)

-- | The number of training jobs that failed and can\'t be retried. A failed
-- training job can\'t be retried if it failed because a client error
-- occurred.
trainingJobStatusCounters_nonRetryableError :: Lens.Lens' TrainingJobStatusCounters (Prelude.Maybe Prelude.Natural)
trainingJobStatusCounters_nonRetryableError = Lens.lens (\TrainingJobStatusCounters' {nonRetryableError} -> nonRetryableError) (\s@TrainingJobStatusCounters' {} a -> s {nonRetryableError = a} :: TrainingJobStatusCounters)

-- | The number of training jobs that failed, but can be retried. A failed
-- training job can be retried only if it failed because an internal
-- service error occurred.
trainingJobStatusCounters_retryableError :: Lens.Lens' TrainingJobStatusCounters (Prelude.Maybe Prelude.Natural)
trainingJobStatusCounters_retryableError = Lens.lens (\TrainingJobStatusCounters' {retryableError} -> retryableError) (\s@TrainingJobStatusCounters' {} a -> s {retryableError = a} :: TrainingJobStatusCounters)

-- | The number of training jobs launched by a hyperparameter tuning job that
-- were manually stopped.
trainingJobStatusCounters_stopped :: Lens.Lens' TrainingJobStatusCounters (Prelude.Maybe Prelude.Natural)
trainingJobStatusCounters_stopped = Lens.lens (\TrainingJobStatusCounters' {stopped} -> stopped) (\s@TrainingJobStatusCounters' {} a -> s {stopped = a} :: TrainingJobStatusCounters)

instance Data.FromJSON TrainingJobStatusCounters where
  parseJSON =
    Data.withObject
      "TrainingJobStatusCounters"
      ( \x ->
          TrainingJobStatusCounters'
            Prelude.<$> (x Data..:? "Completed")
            Prelude.<*> (x Data..:? "InProgress")
            Prelude.<*> (x Data..:? "NonRetryableError")
            Prelude.<*> (x Data..:? "RetryableError")
            Prelude.<*> (x Data..:? "Stopped")
      )

instance Prelude.Hashable TrainingJobStatusCounters where
  hashWithSalt _salt TrainingJobStatusCounters' {..} =
    _salt
      `Prelude.hashWithSalt` completed
      `Prelude.hashWithSalt` inProgress
      `Prelude.hashWithSalt` nonRetryableError
      `Prelude.hashWithSalt` retryableError
      `Prelude.hashWithSalt` stopped

instance Prelude.NFData TrainingJobStatusCounters where
  rnf TrainingJobStatusCounters' {..} =
    Prelude.rnf completed
      `Prelude.seq` Prelude.rnf inProgress
      `Prelude.seq` Prelude.rnf nonRetryableError
      `Prelude.seq` Prelude.rnf retryableError
      `Prelude.seq` Prelude.rnf stopped
