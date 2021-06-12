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

-- | Specifies the maximum number of training jobs and parallel training jobs
-- that a hyperparameter tuning job can launch.
--
-- /See:/ 'newResourceLimits' smart constructor.
data ResourceLimits = ResourceLimits'
  { -- | The maximum number of training jobs that a hyperparameter tuning job can
    -- launch.
    maxNumberOfTrainingJobs :: Core.Natural,
    -- | The maximum number of concurrent training jobs that a hyperparameter
    -- tuning job can launch.
    maxParallelTrainingJobs :: Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Natural ->
  -- | 'maxParallelTrainingJobs'
  Core.Natural ->
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
resourceLimits_maxNumberOfTrainingJobs :: Lens.Lens' ResourceLimits Core.Natural
resourceLimits_maxNumberOfTrainingJobs = Lens.lens (\ResourceLimits' {maxNumberOfTrainingJobs} -> maxNumberOfTrainingJobs) (\s@ResourceLimits' {} a -> s {maxNumberOfTrainingJobs = a} :: ResourceLimits)

-- | The maximum number of concurrent training jobs that a hyperparameter
-- tuning job can launch.
resourceLimits_maxParallelTrainingJobs :: Lens.Lens' ResourceLimits Core.Natural
resourceLimits_maxParallelTrainingJobs = Lens.lens (\ResourceLimits' {maxParallelTrainingJobs} -> maxParallelTrainingJobs) (\s@ResourceLimits' {} a -> s {maxParallelTrainingJobs = a} :: ResourceLimits)

instance Core.FromJSON ResourceLimits where
  parseJSON =
    Core.withObject
      "ResourceLimits"
      ( \x ->
          ResourceLimits'
            Core.<$> (x Core..: "MaxNumberOfTrainingJobs")
            Core.<*> (x Core..: "MaxParallelTrainingJobs")
      )

instance Core.Hashable ResourceLimits

instance Core.NFData ResourceLimits

instance Core.ToJSON ResourceLimits where
  toJSON ResourceLimits' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "MaxNumberOfTrainingJobs"
                  Core..= maxNumberOfTrainingJobs
              ),
            Core.Just
              ( "MaxParallelTrainingJobs"
                  Core..= maxParallelTrainingJobs
              )
          ]
      )
