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
-- Module      : Amazonka.Comprehend.Types.FlywheelModelEvaluationMetrics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.FlywheelModelEvaluationMetrics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The evaluation metrics associated with the evaluated model.
--
-- /See:/ 'newFlywheelModelEvaluationMetrics' smart constructor.
data FlywheelModelEvaluationMetrics = FlywheelModelEvaluationMetrics'
  { -- | Average accuracy metric for the model.
    averageAccuracy :: Prelude.Maybe Prelude.Double,
    -- | The average F1 score from the evaluation metrics.
    averageF1Score :: Prelude.Maybe Prelude.Double,
    -- | Average precision metric for the model.
    averagePrecision :: Prelude.Maybe Prelude.Double,
    -- | Average recall metric for the model.
    averageRecall :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FlywheelModelEvaluationMetrics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'averageAccuracy', 'flywheelModelEvaluationMetrics_averageAccuracy' - Average accuracy metric for the model.
--
-- 'averageF1Score', 'flywheelModelEvaluationMetrics_averageF1Score' - The average F1 score from the evaluation metrics.
--
-- 'averagePrecision', 'flywheelModelEvaluationMetrics_averagePrecision' - Average precision metric for the model.
--
-- 'averageRecall', 'flywheelModelEvaluationMetrics_averageRecall' - Average recall metric for the model.
newFlywheelModelEvaluationMetrics ::
  FlywheelModelEvaluationMetrics
newFlywheelModelEvaluationMetrics =
  FlywheelModelEvaluationMetrics'
    { averageAccuracy =
        Prelude.Nothing,
      averageF1Score = Prelude.Nothing,
      averagePrecision = Prelude.Nothing,
      averageRecall = Prelude.Nothing
    }

-- | Average accuracy metric for the model.
flywheelModelEvaluationMetrics_averageAccuracy :: Lens.Lens' FlywheelModelEvaluationMetrics (Prelude.Maybe Prelude.Double)
flywheelModelEvaluationMetrics_averageAccuracy = Lens.lens (\FlywheelModelEvaluationMetrics' {averageAccuracy} -> averageAccuracy) (\s@FlywheelModelEvaluationMetrics' {} a -> s {averageAccuracy = a} :: FlywheelModelEvaluationMetrics)

-- | The average F1 score from the evaluation metrics.
flywheelModelEvaluationMetrics_averageF1Score :: Lens.Lens' FlywheelModelEvaluationMetrics (Prelude.Maybe Prelude.Double)
flywheelModelEvaluationMetrics_averageF1Score = Lens.lens (\FlywheelModelEvaluationMetrics' {averageF1Score} -> averageF1Score) (\s@FlywheelModelEvaluationMetrics' {} a -> s {averageF1Score = a} :: FlywheelModelEvaluationMetrics)

-- | Average precision metric for the model.
flywheelModelEvaluationMetrics_averagePrecision :: Lens.Lens' FlywheelModelEvaluationMetrics (Prelude.Maybe Prelude.Double)
flywheelModelEvaluationMetrics_averagePrecision = Lens.lens (\FlywheelModelEvaluationMetrics' {averagePrecision} -> averagePrecision) (\s@FlywheelModelEvaluationMetrics' {} a -> s {averagePrecision = a} :: FlywheelModelEvaluationMetrics)

-- | Average recall metric for the model.
flywheelModelEvaluationMetrics_averageRecall :: Lens.Lens' FlywheelModelEvaluationMetrics (Prelude.Maybe Prelude.Double)
flywheelModelEvaluationMetrics_averageRecall = Lens.lens (\FlywheelModelEvaluationMetrics' {averageRecall} -> averageRecall) (\s@FlywheelModelEvaluationMetrics' {} a -> s {averageRecall = a} :: FlywheelModelEvaluationMetrics)

instance Data.FromJSON FlywheelModelEvaluationMetrics where
  parseJSON =
    Data.withObject
      "FlywheelModelEvaluationMetrics"
      ( \x ->
          FlywheelModelEvaluationMetrics'
            Prelude.<$> (x Data..:? "AverageAccuracy")
            Prelude.<*> (x Data..:? "AverageF1Score")
            Prelude.<*> (x Data..:? "AveragePrecision")
            Prelude.<*> (x Data..:? "AverageRecall")
      )

instance
  Prelude.Hashable
    FlywheelModelEvaluationMetrics
  where
  hashWithSalt
    _salt
    FlywheelModelEvaluationMetrics' {..} =
      _salt
        `Prelude.hashWithSalt` averageAccuracy
        `Prelude.hashWithSalt` averageF1Score
        `Prelude.hashWithSalt` averagePrecision
        `Prelude.hashWithSalt` averageRecall

instance
  Prelude.NFData
    FlywheelModelEvaluationMetrics
  where
  rnf FlywheelModelEvaluationMetrics' {..} =
    Prelude.rnf averageAccuracy
      `Prelude.seq` Prelude.rnf averageF1Score
      `Prelude.seq` Prelude.rnf averagePrecision
      `Prelude.seq` Prelude.rnf averageRecall
