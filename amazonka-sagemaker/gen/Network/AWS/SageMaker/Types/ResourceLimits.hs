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
-- Module      : Network.AWS.SageMaker.Types.ResourceLimits
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ResourceLimits where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the maximum number of training jobs and parallel training jobs
-- that a hyperparameter tuning job can launch.
--
-- /See:/ 'newResourceLimits' smart constructor.
data ResourceLimits = ResourceLimits'
  { -- | The maximum number of training jobs that a hyperparameter tuning job can
    -- launch.
    maxNumberOfTrainingJobs :: Prelude.Natural,
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
-- 'maxParallelTrainingJobs', 'resourceLimits_maxParallelTrainingJobs' - The maximum number of concurrent training jobs that a hyperparameter
-- tuning job can launch.
newResourceLimits ::
  -- | 'maxNumberOfTrainingJobs'
  Prelude.Natural ->
  -- | 'maxParallelTrainingJobs'
  Prelude.Natural ->
  ResourceLimits
newResourceLimits
  pMaxNumberOfTrainingJobs_
  pMaxParallelTrainingJobs_ =
    ResourceLimits'
      { maxNumberOfTrainingJobs =
          pMaxNumberOfTrainingJobs_,
        maxParallelTrainingJobs = pMaxParallelTrainingJobs_
      }

-- | The maximum number of training jobs that a hyperparameter tuning job can
-- launch.
resourceLimits_maxNumberOfTrainingJobs :: Lens.Lens' ResourceLimits Prelude.Natural
resourceLimits_maxNumberOfTrainingJobs = Lens.lens (\ResourceLimits' {maxNumberOfTrainingJobs} -> maxNumberOfTrainingJobs) (\s@ResourceLimits' {} a -> s {maxNumberOfTrainingJobs = a} :: ResourceLimits)

-- | The maximum number of concurrent training jobs that a hyperparameter
-- tuning job can launch.
resourceLimits_maxParallelTrainingJobs :: Lens.Lens' ResourceLimits Prelude.Natural
resourceLimits_maxParallelTrainingJobs = Lens.lens (\ResourceLimits' {maxParallelTrainingJobs} -> maxParallelTrainingJobs) (\s@ResourceLimits' {} a -> s {maxParallelTrainingJobs = a} :: ResourceLimits)

instance Core.FromJSON ResourceLimits where
  parseJSON =
    Core.withObject
      "ResourceLimits"
      ( \x ->
          ResourceLimits'
            Prelude.<$> (x Core..: "MaxNumberOfTrainingJobs")
            Prelude.<*> (x Core..: "MaxParallelTrainingJobs")
      )

instance Prelude.Hashable ResourceLimits

instance Prelude.NFData ResourceLimits

instance Core.ToJSON ResourceLimits where
  toJSON ResourceLimits' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "MaxNumberOfTrainingJobs"
                  Core..= maxNumberOfTrainingJobs
              ),
            Prelude.Just
              ( "MaxParallelTrainingJobs"
                  Core..= maxParallelTrainingJobs
              )
          ]
      )
