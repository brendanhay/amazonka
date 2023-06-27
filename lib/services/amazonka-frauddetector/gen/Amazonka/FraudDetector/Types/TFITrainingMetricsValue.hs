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
-- Module      : Amazonka.FraudDetector.Types.TFITrainingMetricsValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.TFITrainingMetricsValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types.TFIMetricDataPoint
import Amazonka.FraudDetector.Types.TFIModelPerformance
import qualified Amazonka.Prelude as Prelude

-- | The Transaction Fraud Insights (TFI) model training metric details.
--
-- /See:/ 'newTFITrainingMetricsValue' smart constructor.
data TFITrainingMetricsValue = TFITrainingMetricsValue'
  { -- | The model\'s performance metrics data points.
    metricDataPoints :: Prelude.Maybe [TFIMetricDataPoint],
    -- | The model performance score.
    modelPerformance :: Prelude.Maybe TFIModelPerformance
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TFITrainingMetricsValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricDataPoints', 'tFITrainingMetricsValue_metricDataPoints' - The model\'s performance metrics data points.
--
-- 'modelPerformance', 'tFITrainingMetricsValue_modelPerformance' - The model performance score.
newTFITrainingMetricsValue ::
  TFITrainingMetricsValue
newTFITrainingMetricsValue =
  TFITrainingMetricsValue'
    { metricDataPoints =
        Prelude.Nothing,
      modelPerformance = Prelude.Nothing
    }

-- | The model\'s performance metrics data points.
tFITrainingMetricsValue_metricDataPoints :: Lens.Lens' TFITrainingMetricsValue (Prelude.Maybe [TFIMetricDataPoint])
tFITrainingMetricsValue_metricDataPoints = Lens.lens (\TFITrainingMetricsValue' {metricDataPoints} -> metricDataPoints) (\s@TFITrainingMetricsValue' {} a -> s {metricDataPoints = a} :: TFITrainingMetricsValue) Prelude.. Lens.mapping Lens.coerced

-- | The model performance score.
tFITrainingMetricsValue_modelPerformance :: Lens.Lens' TFITrainingMetricsValue (Prelude.Maybe TFIModelPerformance)
tFITrainingMetricsValue_modelPerformance = Lens.lens (\TFITrainingMetricsValue' {modelPerformance} -> modelPerformance) (\s@TFITrainingMetricsValue' {} a -> s {modelPerformance = a} :: TFITrainingMetricsValue)

instance Data.FromJSON TFITrainingMetricsValue where
  parseJSON =
    Data.withObject
      "TFITrainingMetricsValue"
      ( \x ->
          TFITrainingMetricsValue'
            Prelude.<$> ( x
                            Data..:? "metricDataPoints"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "modelPerformance")
      )

instance Prelude.Hashable TFITrainingMetricsValue where
  hashWithSalt _salt TFITrainingMetricsValue' {..} =
    _salt
      `Prelude.hashWithSalt` metricDataPoints
      `Prelude.hashWithSalt` modelPerformance

instance Prelude.NFData TFITrainingMetricsValue where
  rnf TFITrainingMetricsValue' {..} =
    Prelude.rnf metricDataPoints
      `Prelude.seq` Prelude.rnf modelPerformance
