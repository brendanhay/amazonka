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
-- Module      : Amazonka.FraudDetector.Types.TrainingMetrics
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.TrainingMetrics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FraudDetector.Types.MetricDataPoint
import qualified Amazonka.Prelude as Prelude

-- | The training metric details.
--
-- /See:/ 'newTrainingMetrics' smart constructor.
data TrainingMetrics = TrainingMetrics'
  { -- | The data points details.
    metricDataPoints :: Prelude.Maybe [MetricDataPoint],
    -- | The area under the curve. This summarizes true positive rate (TPR) and
    -- false positive rate (FPR) across all possible model score thresholds. A
    -- model with no predictive power has an AUC of 0.5, whereas a perfect
    -- model has a score of 1.0.
    auc :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrainingMetrics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricDataPoints', 'trainingMetrics_metricDataPoints' - The data points details.
--
-- 'auc', 'trainingMetrics_auc' - The area under the curve. This summarizes true positive rate (TPR) and
-- false positive rate (FPR) across all possible model score thresholds. A
-- model with no predictive power has an AUC of 0.5, whereas a perfect
-- model has a score of 1.0.
newTrainingMetrics ::
  TrainingMetrics
newTrainingMetrics =
  TrainingMetrics'
    { metricDataPoints =
        Prelude.Nothing,
      auc = Prelude.Nothing
    }

-- | The data points details.
trainingMetrics_metricDataPoints :: Lens.Lens' TrainingMetrics (Prelude.Maybe [MetricDataPoint])
trainingMetrics_metricDataPoints = Lens.lens (\TrainingMetrics' {metricDataPoints} -> metricDataPoints) (\s@TrainingMetrics' {} a -> s {metricDataPoints = a} :: TrainingMetrics) Prelude.. Lens.mapping Lens.coerced

-- | The area under the curve. This summarizes true positive rate (TPR) and
-- false positive rate (FPR) across all possible model score thresholds. A
-- model with no predictive power has an AUC of 0.5, whereas a perfect
-- model has a score of 1.0.
trainingMetrics_auc :: Lens.Lens' TrainingMetrics (Prelude.Maybe Prelude.Double)
trainingMetrics_auc = Lens.lens (\TrainingMetrics' {auc} -> auc) (\s@TrainingMetrics' {} a -> s {auc = a} :: TrainingMetrics)

instance Core.FromJSON TrainingMetrics where
  parseJSON =
    Core.withObject
      "TrainingMetrics"
      ( \x ->
          TrainingMetrics'
            Prelude.<$> ( x Core..:? "metricDataPoints"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "auc")
      )

instance Prelude.Hashable TrainingMetrics where
  hashWithSalt _salt TrainingMetrics' {..} =
    _salt `Prelude.hashWithSalt` metricDataPoints
      `Prelude.hashWithSalt` auc

instance Prelude.NFData TrainingMetrics where
  rnf TrainingMetrics' {..} =
    Prelude.rnf metricDataPoints
      `Prelude.seq` Prelude.rnf auc
