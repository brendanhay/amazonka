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
-- Module      : Amazonka.SageMaker.Types.AutoMLJobCompletionCriteria
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.AutoMLJobCompletionCriteria where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | How long a job is allowed to run, or how many candidates a job is
-- allowed to generate.
--
-- /See:/ 'newAutoMLJobCompletionCriteria' smart constructor.
data AutoMLJobCompletionCriteria = AutoMLJobCompletionCriteria'
  { -- | The maximum runtime, in seconds, an AutoML job has to complete.
    --
    -- If an AutoML job exceeds the maximum runtime, the job is stopped
    -- automatically and its processing is ended gracefully. The AutoML job
    -- identifies the best model whose training was completed and marks it as
    -- the best-performing model. Any unfinished steps of the job, such as
    -- automatic one-click Autopilot model deployment, will not be completed.
    maxAutoMLJobRuntimeInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The maximum number of times a training job is allowed to run.
    maxCandidates :: Prelude.Maybe Prelude.Natural,
    -- | The maximum time, in seconds, that each training job executed inside
    -- hyperparameter tuning is allowed to run as part of a hyperparameter
    -- tuning job. For more information, see the used by the action.
    maxRuntimePerTrainingJobInSeconds :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoMLJobCompletionCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxAutoMLJobRuntimeInSeconds', 'autoMLJobCompletionCriteria_maxAutoMLJobRuntimeInSeconds' - The maximum runtime, in seconds, an AutoML job has to complete.
--
-- If an AutoML job exceeds the maximum runtime, the job is stopped
-- automatically and its processing is ended gracefully. The AutoML job
-- identifies the best model whose training was completed and marks it as
-- the best-performing model. Any unfinished steps of the job, such as
-- automatic one-click Autopilot model deployment, will not be completed.
--
-- 'maxCandidates', 'autoMLJobCompletionCriteria_maxCandidates' - The maximum number of times a training job is allowed to run.
--
-- 'maxRuntimePerTrainingJobInSeconds', 'autoMLJobCompletionCriteria_maxRuntimePerTrainingJobInSeconds' - The maximum time, in seconds, that each training job executed inside
-- hyperparameter tuning is allowed to run as part of a hyperparameter
-- tuning job. For more information, see the used by the action.
newAutoMLJobCompletionCriteria ::
  AutoMLJobCompletionCriteria
newAutoMLJobCompletionCriteria =
  AutoMLJobCompletionCriteria'
    { maxAutoMLJobRuntimeInSeconds =
        Prelude.Nothing,
      maxCandidates = Prelude.Nothing,
      maxRuntimePerTrainingJobInSeconds =
        Prelude.Nothing
    }

-- | The maximum runtime, in seconds, an AutoML job has to complete.
--
-- If an AutoML job exceeds the maximum runtime, the job is stopped
-- automatically and its processing is ended gracefully. The AutoML job
-- identifies the best model whose training was completed and marks it as
-- the best-performing model. Any unfinished steps of the job, such as
-- automatic one-click Autopilot model deployment, will not be completed.
autoMLJobCompletionCriteria_maxAutoMLJobRuntimeInSeconds :: Lens.Lens' AutoMLJobCompletionCriteria (Prelude.Maybe Prelude.Natural)
autoMLJobCompletionCriteria_maxAutoMLJobRuntimeInSeconds = Lens.lens (\AutoMLJobCompletionCriteria' {maxAutoMLJobRuntimeInSeconds} -> maxAutoMLJobRuntimeInSeconds) (\s@AutoMLJobCompletionCriteria' {} a -> s {maxAutoMLJobRuntimeInSeconds = a} :: AutoMLJobCompletionCriteria)

-- | The maximum number of times a training job is allowed to run.
autoMLJobCompletionCriteria_maxCandidates :: Lens.Lens' AutoMLJobCompletionCriteria (Prelude.Maybe Prelude.Natural)
autoMLJobCompletionCriteria_maxCandidates = Lens.lens (\AutoMLJobCompletionCriteria' {maxCandidates} -> maxCandidates) (\s@AutoMLJobCompletionCriteria' {} a -> s {maxCandidates = a} :: AutoMLJobCompletionCriteria)

-- | The maximum time, in seconds, that each training job executed inside
-- hyperparameter tuning is allowed to run as part of a hyperparameter
-- tuning job. For more information, see the used by the action.
autoMLJobCompletionCriteria_maxRuntimePerTrainingJobInSeconds :: Lens.Lens' AutoMLJobCompletionCriteria (Prelude.Maybe Prelude.Natural)
autoMLJobCompletionCriteria_maxRuntimePerTrainingJobInSeconds = Lens.lens (\AutoMLJobCompletionCriteria' {maxRuntimePerTrainingJobInSeconds} -> maxRuntimePerTrainingJobInSeconds) (\s@AutoMLJobCompletionCriteria' {} a -> s {maxRuntimePerTrainingJobInSeconds = a} :: AutoMLJobCompletionCriteria)

instance Data.FromJSON AutoMLJobCompletionCriteria where
  parseJSON =
    Data.withObject
      "AutoMLJobCompletionCriteria"
      ( \x ->
          AutoMLJobCompletionCriteria'
            Prelude.<$> (x Data..:? "MaxAutoMLJobRuntimeInSeconds")
            Prelude.<*> (x Data..:? "MaxCandidates")
            Prelude.<*> (x Data..:? "MaxRuntimePerTrainingJobInSeconds")
      )

instance Prelude.Hashable AutoMLJobCompletionCriteria where
  hashWithSalt _salt AutoMLJobCompletionCriteria' {..} =
    _salt
      `Prelude.hashWithSalt` maxAutoMLJobRuntimeInSeconds
      `Prelude.hashWithSalt` maxCandidates
      `Prelude.hashWithSalt` maxRuntimePerTrainingJobInSeconds

instance Prelude.NFData AutoMLJobCompletionCriteria where
  rnf AutoMLJobCompletionCriteria' {..} =
    Prelude.rnf maxAutoMLJobRuntimeInSeconds
      `Prelude.seq` Prelude.rnf maxCandidates
      `Prelude.seq` Prelude.rnf maxRuntimePerTrainingJobInSeconds

instance Data.ToJSON AutoMLJobCompletionCriteria where
  toJSON AutoMLJobCompletionCriteria' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxAutoMLJobRuntimeInSeconds" Data..=)
              Prelude.<$> maxAutoMLJobRuntimeInSeconds,
            ("MaxCandidates" Data..=) Prelude.<$> maxCandidates,
            ("MaxRuntimePerTrainingJobInSeconds" Data..=)
              Prelude.<$> maxRuntimePerTrainingJobInSeconds
          ]
      )
