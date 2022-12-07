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
-- Module      : Amazonka.Forecast.Types.Metrics
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.Metrics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types.ErrorMetric
import Amazonka.Forecast.Types.WeightedQuantileLoss
import qualified Amazonka.Prelude as Prelude

-- | Provides metrics that are used to evaluate the performance of a
-- predictor. This object is part of the WindowSummary object.
--
-- /See:/ 'newMetrics' smart constructor.
data Metrics = Metrics'
  { -- | The average value of all weighted quantile losses.
    averageWeightedQuantileLoss :: Prelude.Maybe Prelude.Double,
    -- | An array of weighted quantile losses. Quantiles divide a probability
    -- distribution into regions of equal probability. The distribution in this
    -- case is the loss function.
    weightedQuantileLosses :: Prelude.Maybe [WeightedQuantileLoss],
    -- | Provides detailed error metrics for each forecast type. Metrics include
    -- root-mean square-error (RMSE), mean absolute percentage error (MAPE),
    -- mean absolute scaled error (MASE), and weighted average percentage error
    -- (WAPE).
    errorMetrics :: Prelude.Maybe [ErrorMetric],
    -- | The root-mean-square error (RMSE).
    rmse :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Metrics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'averageWeightedQuantileLoss', 'metrics_averageWeightedQuantileLoss' - The average value of all weighted quantile losses.
--
-- 'weightedQuantileLosses', 'metrics_weightedQuantileLosses' - An array of weighted quantile losses. Quantiles divide a probability
-- distribution into regions of equal probability. The distribution in this
-- case is the loss function.
--
-- 'errorMetrics', 'metrics_errorMetrics' - Provides detailed error metrics for each forecast type. Metrics include
-- root-mean square-error (RMSE), mean absolute percentage error (MAPE),
-- mean absolute scaled error (MASE), and weighted average percentage error
-- (WAPE).
--
-- 'rmse', 'metrics_rmse' - The root-mean-square error (RMSE).
newMetrics ::
  Metrics
newMetrics =
  Metrics'
    { averageWeightedQuantileLoss =
        Prelude.Nothing,
      weightedQuantileLosses = Prelude.Nothing,
      errorMetrics = Prelude.Nothing,
      rmse = Prelude.Nothing
    }

-- | The average value of all weighted quantile losses.
metrics_averageWeightedQuantileLoss :: Lens.Lens' Metrics (Prelude.Maybe Prelude.Double)
metrics_averageWeightedQuantileLoss = Lens.lens (\Metrics' {averageWeightedQuantileLoss} -> averageWeightedQuantileLoss) (\s@Metrics' {} a -> s {averageWeightedQuantileLoss = a} :: Metrics)

-- | An array of weighted quantile losses. Quantiles divide a probability
-- distribution into regions of equal probability. The distribution in this
-- case is the loss function.
metrics_weightedQuantileLosses :: Lens.Lens' Metrics (Prelude.Maybe [WeightedQuantileLoss])
metrics_weightedQuantileLosses = Lens.lens (\Metrics' {weightedQuantileLosses} -> weightedQuantileLosses) (\s@Metrics' {} a -> s {weightedQuantileLosses = a} :: Metrics) Prelude.. Lens.mapping Lens.coerced

-- | Provides detailed error metrics for each forecast type. Metrics include
-- root-mean square-error (RMSE), mean absolute percentage error (MAPE),
-- mean absolute scaled error (MASE), and weighted average percentage error
-- (WAPE).
metrics_errorMetrics :: Lens.Lens' Metrics (Prelude.Maybe [ErrorMetric])
metrics_errorMetrics = Lens.lens (\Metrics' {errorMetrics} -> errorMetrics) (\s@Metrics' {} a -> s {errorMetrics = a} :: Metrics) Prelude.. Lens.mapping Lens.coerced

-- | The root-mean-square error (RMSE).
metrics_rmse :: Lens.Lens' Metrics (Prelude.Maybe Prelude.Double)
metrics_rmse = Lens.lens (\Metrics' {rmse} -> rmse) (\s@Metrics' {} a -> s {rmse = a} :: Metrics)

instance Data.FromJSON Metrics where
  parseJSON =
    Data.withObject
      "Metrics"
      ( \x ->
          Metrics'
            Prelude.<$> (x Data..:? "AverageWeightedQuantileLoss")
            Prelude.<*> ( x Data..:? "WeightedQuantileLosses"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ErrorMetrics" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "RMSE")
      )

instance Prelude.Hashable Metrics where
  hashWithSalt _salt Metrics' {..} =
    _salt
      `Prelude.hashWithSalt` averageWeightedQuantileLoss
      `Prelude.hashWithSalt` weightedQuantileLosses
      `Prelude.hashWithSalt` errorMetrics
      `Prelude.hashWithSalt` rmse

instance Prelude.NFData Metrics where
  rnf Metrics' {..} =
    Prelude.rnf averageWeightedQuantileLoss
      `Prelude.seq` Prelude.rnf weightedQuantileLosses
      `Prelude.seq` Prelude.rnf errorMetrics
      `Prelude.seq` Prelude.rnf rmse
