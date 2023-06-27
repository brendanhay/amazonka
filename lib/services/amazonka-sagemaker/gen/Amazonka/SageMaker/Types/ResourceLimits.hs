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
-- Module      : Amazonka.SageMaker.Types.ResourceLimits
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ResourceLimits where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the maximum number of training jobs and parallel training jobs
-- that a hyperparameter tuning job can launch.
--
-- /See:/ 'newResourceLimits' smart constructor.
data ResourceLimits = ResourceLimits'
  { -- | The maximum number of training jobs that a hyperparameter tuning job can
    -- launch.
    maxNumberOfTrainingJobs :: Prelude.Maybe Prelude.Natural,
    -- | The maximum time in seconds that a hyperparameter tuning job can run.
    maxRuntimeInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The maximum number of concurrent training jobs that a hyperparameter
    -- tuning job can launch.
    maxParallelTrainingJobs :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceLimits' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxNumberOfTrainingJobs', 'resourceLimits_maxNumberOfTrainingJobs' - The maximum number of training jobs that a hyperparameter tuning job can
-- launch.
--
-- 'maxRuntimeInSeconds', 'resourceLimits_maxRuntimeInSeconds' - The maximum time in seconds that a hyperparameter tuning job can run.
--
-- 'maxParallelTrainingJobs', 'resourceLimits_maxParallelTrainingJobs' - The maximum number of concurrent training jobs that a hyperparameter
-- tuning job can launch.
newResourceLimits ::
  -- | 'maxParallelTrainingJobs'
  Prelude.Natural ->
  ResourceLimits
newResourceLimits pMaxParallelTrainingJobs_ =
  ResourceLimits'
    { maxNumberOfTrainingJobs =
        Prelude.Nothing,
      maxRuntimeInSeconds = Prelude.Nothing,
      maxParallelTrainingJobs = pMaxParallelTrainingJobs_
    }

-- | The maximum number of training jobs that a hyperparameter tuning job can
-- launch.
resourceLimits_maxNumberOfTrainingJobs :: Lens.Lens' ResourceLimits (Prelude.Maybe Prelude.Natural)
resourceLimits_maxNumberOfTrainingJobs = Lens.lens (\ResourceLimits' {maxNumberOfTrainingJobs} -> maxNumberOfTrainingJobs) (\s@ResourceLimits' {} a -> s {maxNumberOfTrainingJobs = a} :: ResourceLimits)

-- | The maximum time in seconds that a hyperparameter tuning job can run.
resourceLimits_maxRuntimeInSeconds :: Lens.Lens' ResourceLimits (Prelude.Maybe Prelude.Natural)
resourceLimits_maxRuntimeInSeconds = Lens.lens (\ResourceLimits' {maxRuntimeInSeconds} -> maxRuntimeInSeconds) (\s@ResourceLimits' {} a -> s {maxRuntimeInSeconds = a} :: ResourceLimits)

-- | The maximum number of concurrent training jobs that a hyperparameter
-- tuning job can launch.
resourceLimits_maxParallelTrainingJobs :: Lens.Lens' ResourceLimits Prelude.Natural
resourceLimits_maxParallelTrainingJobs = Lens.lens (\ResourceLimits' {maxParallelTrainingJobs} -> maxParallelTrainingJobs) (\s@ResourceLimits' {} a -> s {maxParallelTrainingJobs = a} :: ResourceLimits)

instance Data.FromJSON ResourceLimits where
  parseJSON =
    Data.withObject
      "ResourceLimits"
      ( \x ->
          ResourceLimits'
            Prelude.<$> (x Data..:? "MaxNumberOfTrainingJobs")
            Prelude.<*> (x Data..:? "MaxRuntimeInSeconds")
            Prelude.<*> (x Data..: "MaxParallelTrainingJobs")
      )

instance Prelude.Hashable ResourceLimits where
  hashWithSalt _salt ResourceLimits' {..} =
    _salt
      `Prelude.hashWithSalt` maxNumberOfTrainingJobs
      `Prelude.hashWithSalt` maxRuntimeInSeconds
      `Prelude.hashWithSalt` maxParallelTrainingJobs

instance Prelude.NFData ResourceLimits where
  rnf ResourceLimits' {..} =
    Prelude.rnf maxNumberOfTrainingJobs
      `Prelude.seq` Prelude.rnf maxRuntimeInSeconds
      `Prelude.seq` Prelude.rnf maxParallelTrainingJobs

instance Data.ToJSON ResourceLimits where
  toJSON ResourceLimits' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxNumberOfTrainingJobs" Data..=)
              Prelude.<$> maxNumberOfTrainingJobs,
            ("MaxRuntimeInSeconds" Data..=)
              Prelude.<$> maxRuntimeInSeconds,
            Prelude.Just
              ( "MaxParallelTrainingJobs"
                  Data..= maxParallelTrainingJobs
              )
          ]
      )
