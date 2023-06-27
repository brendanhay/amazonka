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
-- Module      : Amazonka.SageMaker.Types.TuningJobCompletionCriteria
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.TuningJobCompletionCriteria where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.BestObjectiveNotImproving
import Amazonka.SageMaker.Types.ConvergenceDetected

-- | The job completion criteria.
--
-- /See:/ 'newTuningJobCompletionCriteria' smart constructor.
data TuningJobCompletionCriteria = TuningJobCompletionCriteria'
  { -- | A flag to stop your hyperparameter tuning job if model performance fails
    -- to improve as evaluated against an objective function.
    bestObjectiveNotImproving :: Prelude.Maybe BestObjectiveNotImproving,
    -- | A flag to top your hyperparameter tuning job if automatic model tuning
    -- (AMT) has detected that your model has converged as evaluated against
    -- your objective function.
    convergenceDetected :: Prelude.Maybe ConvergenceDetected,
    -- | The value of the objective metric.
    targetObjectiveMetricValue :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TuningJobCompletionCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bestObjectiveNotImproving', 'tuningJobCompletionCriteria_bestObjectiveNotImproving' - A flag to stop your hyperparameter tuning job if model performance fails
-- to improve as evaluated against an objective function.
--
-- 'convergenceDetected', 'tuningJobCompletionCriteria_convergenceDetected' - A flag to top your hyperparameter tuning job if automatic model tuning
-- (AMT) has detected that your model has converged as evaluated against
-- your objective function.
--
-- 'targetObjectiveMetricValue', 'tuningJobCompletionCriteria_targetObjectiveMetricValue' - The value of the objective metric.
newTuningJobCompletionCriteria ::
  TuningJobCompletionCriteria
newTuningJobCompletionCriteria =
  TuningJobCompletionCriteria'
    { bestObjectiveNotImproving =
        Prelude.Nothing,
      convergenceDetected = Prelude.Nothing,
      targetObjectiveMetricValue = Prelude.Nothing
    }

-- | A flag to stop your hyperparameter tuning job if model performance fails
-- to improve as evaluated against an objective function.
tuningJobCompletionCriteria_bestObjectiveNotImproving :: Lens.Lens' TuningJobCompletionCriteria (Prelude.Maybe BestObjectiveNotImproving)
tuningJobCompletionCriteria_bestObjectiveNotImproving = Lens.lens (\TuningJobCompletionCriteria' {bestObjectiveNotImproving} -> bestObjectiveNotImproving) (\s@TuningJobCompletionCriteria' {} a -> s {bestObjectiveNotImproving = a} :: TuningJobCompletionCriteria)

-- | A flag to top your hyperparameter tuning job if automatic model tuning
-- (AMT) has detected that your model has converged as evaluated against
-- your objective function.
tuningJobCompletionCriteria_convergenceDetected :: Lens.Lens' TuningJobCompletionCriteria (Prelude.Maybe ConvergenceDetected)
tuningJobCompletionCriteria_convergenceDetected = Lens.lens (\TuningJobCompletionCriteria' {convergenceDetected} -> convergenceDetected) (\s@TuningJobCompletionCriteria' {} a -> s {convergenceDetected = a} :: TuningJobCompletionCriteria)

-- | The value of the objective metric.
tuningJobCompletionCriteria_targetObjectiveMetricValue :: Lens.Lens' TuningJobCompletionCriteria (Prelude.Maybe Prelude.Double)
tuningJobCompletionCriteria_targetObjectiveMetricValue = Lens.lens (\TuningJobCompletionCriteria' {targetObjectiveMetricValue} -> targetObjectiveMetricValue) (\s@TuningJobCompletionCriteria' {} a -> s {targetObjectiveMetricValue = a} :: TuningJobCompletionCriteria)

instance Data.FromJSON TuningJobCompletionCriteria where
  parseJSON =
    Data.withObject
      "TuningJobCompletionCriteria"
      ( \x ->
          TuningJobCompletionCriteria'
            Prelude.<$> (x Data..:? "BestObjectiveNotImproving")
            Prelude.<*> (x Data..:? "ConvergenceDetected")
            Prelude.<*> (x Data..:? "TargetObjectiveMetricValue")
      )

instance Prelude.Hashable TuningJobCompletionCriteria where
  hashWithSalt _salt TuningJobCompletionCriteria' {..} =
    _salt
      `Prelude.hashWithSalt` bestObjectiveNotImproving
      `Prelude.hashWithSalt` convergenceDetected
      `Prelude.hashWithSalt` targetObjectiveMetricValue

instance Prelude.NFData TuningJobCompletionCriteria where
  rnf TuningJobCompletionCriteria' {..} =
    Prelude.rnf bestObjectiveNotImproving
      `Prelude.seq` Prelude.rnf convergenceDetected
      `Prelude.seq` Prelude.rnf targetObjectiveMetricValue

instance Data.ToJSON TuningJobCompletionCriteria where
  toJSON TuningJobCompletionCriteria' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BestObjectiveNotImproving" Data..=)
              Prelude.<$> bestObjectiveNotImproving,
            ("ConvergenceDetected" Data..=)
              Prelude.<$> convergenceDetected,
            ("TargetObjectiveMetricValue" Data..=)
              Prelude.<$> targetObjectiveMetricValue
          ]
      )
