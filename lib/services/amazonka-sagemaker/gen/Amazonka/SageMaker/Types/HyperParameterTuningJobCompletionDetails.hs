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
-- Module      : Amazonka.SageMaker.Types.HyperParameterTuningJobCompletionDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.HyperParameterTuningJobCompletionDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains runtime information about both current and
-- completed hyperparameter tuning jobs.
--
-- /See:/ 'newHyperParameterTuningJobCompletionDetails' smart constructor.
data HyperParameterTuningJobCompletionDetails = HyperParameterTuningJobCompletionDetails'
  { -- | The time in timestamp format that AMT detected model convergence, as
    -- defined by a lack of significant improvement over time based on criteria
    -- developed over a wide range of diverse benchmarking tests.
    convergenceDetectedTime :: Prelude.Maybe Data.POSIX,
    -- | The number of training jobs launched by a tuning job that are not
    -- improving (1% or less) as measured by model performance evaluated
    -- against an objective function.
    numberOfTrainingJobsObjectiveNotImproving :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HyperParameterTuningJobCompletionDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'convergenceDetectedTime', 'hyperParameterTuningJobCompletionDetails_convergenceDetectedTime' - The time in timestamp format that AMT detected model convergence, as
-- defined by a lack of significant improvement over time based on criteria
-- developed over a wide range of diverse benchmarking tests.
--
-- 'numberOfTrainingJobsObjectiveNotImproving', 'hyperParameterTuningJobCompletionDetails_numberOfTrainingJobsObjectiveNotImproving' - The number of training jobs launched by a tuning job that are not
-- improving (1% or less) as measured by model performance evaluated
-- against an objective function.
newHyperParameterTuningJobCompletionDetails ::
  HyperParameterTuningJobCompletionDetails
newHyperParameterTuningJobCompletionDetails =
  HyperParameterTuningJobCompletionDetails'
    { convergenceDetectedTime =
        Prelude.Nothing,
      numberOfTrainingJobsObjectiveNotImproving =
        Prelude.Nothing
    }

-- | The time in timestamp format that AMT detected model convergence, as
-- defined by a lack of significant improvement over time based on criteria
-- developed over a wide range of diverse benchmarking tests.
hyperParameterTuningJobCompletionDetails_convergenceDetectedTime :: Lens.Lens' HyperParameterTuningJobCompletionDetails (Prelude.Maybe Prelude.UTCTime)
hyperParameterTuningJobCompletionDetails_convergenceDetectedTime = Lens.lens (\HyperParameterTuningJobCompletionDetails' {convergenceDetectedTime} -> convergenceDetectedTime) (\s@HyperParameterTuningJobCompletionDetails' {} a -> s {convergenceDetectedTime = a} :: HyperParameterTuningJobCompletionDetails) Prelude.. Lens.mapping Data._Time

-- | The number of training jobs launched by a tuning job that are not
-- improving (1% or less) as measured by model performance evaluated
-- against an objective function.
hyperParameterTuningJobCompletionDetails_numberOfTrainingJobsObjectiveNotImproving :: Lens.Lens' HyperParameterTuningJobCompletionDetails (Prelude.Maybe Prelude.Int)
hyperParameterTuningJobCompletionDetails_numberOfTrainingJobsObjectiveNotImproving = Lens.lens (\HyperParameterTuningJobCompletionDetails' {numberOfTrainingJobsObjectiveNotImproving} -> numberOfTrainingJobsObjectiveNotImproving) (\s@HyperParameterTuningJobCompletionDetails' {} a -> s {numberOfTrainingJobsObjectiveNotImproving = a} :: HyperParameterTuningJobCompletionDetails)

instance
  Data.FromJSON
    HyperParameterTuningJobCompletionDetails
  where
  parseJSON =
    Data.withObject
      "HyperParameterTuningJobCompletionDetails"
      ( \x ->
          HyperParameterTuningJobCompletionDetails'
            Prelude.<$> (x Data..:? "ConvergenceDetectedTime")
            Prelude.<*> ( x
                            Data..:? "NumberOfTrainingJobsObjectiveNotImproving"
                        )
      )

instance
  Prelude.Hashable
    HyperParameterTuningJobCompletionDetails
  where
  hashWithSalt
    _salt
    HyperParameterTuningJobCompletionDetails' {..} =
      _salt
        `Prelude.hashWithSalt` convergenceDetectedTime
        `Prelude.hashWithSalt` numberOfTrainingJobsObjectiveNotImproving

instance
  Prelude.NFData
    HyperParameterTuningJobCompletionDetails
  where
  rnf HyperParameterTuningJobCompletionDetails' {..} =
    Prelude.rnf convergenceDetectedTime
      `Prelude.seq` Prelude.rnf numberOfTrainingJobsObjectiveNotImproving
