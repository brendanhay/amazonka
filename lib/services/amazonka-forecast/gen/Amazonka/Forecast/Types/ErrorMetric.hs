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
-- Module      : Amazonka.Forecast.Types.ErrorMetric
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.ErrorMetric where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides detailed error metrics to evaluate the performance of a
-- predictor. This object is part of the Metrics object.
--
-- /See:/ 'newErrorMetric' smart constructor.
data ErrorMetric = ErrorMetric'
  { -- | The Mean Absolute Scaled Error (MASE)
    mase :: Prelude.Maybe Prelude.Double,
    -- | The weighted absolute percentage error (WAPE).
    wape :: Prelude.Maybe Prelude.Double,
    -- | The Mean Absolute Percentage Error (MAPE)
    mape :: Prelude.Maybe Prelude.Double,
    -- | The root-mean-square error (RMSE).
    rmse :: Prelude.Maybe Prelude.Double,
    -- | The Forecast type used to compute WAPE, MAPE, MASE, and RMSE.
    forecastType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ErrorMetric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mase', 'errorMetric_mase' - The Mean Absolute Scaled Error (MASE)
--
-- 'wape', 'errorMetric_wape' - The weighted absolute percentage error (WAPE).
--
-- 'mape', 'errorMetric_mape' - The Mean Absolute Percentage Error (MAPE)
--
-- 'rmse', 'errorMetric_rmse' - The root-mean-square error (RMSE).
--
-- 'forecastType', 'errorMetric_forecastType' - The Forecast type used to compute WAPE, MAPE, MASE, and RMSE.
newErrorMetric ::
  ErrorMetric
newErrorMetric =
  ErrorMetric'
    { mase = Prelude.Nothing,
      wape = Prelude.Nothing,
      mape = Prelude.Nothing,
      rmse = Prelude.Nothing,
      forecastType = Prelude.Nothing
    }

-- | The Mean Absolute Scaled Error (MASE)
errorMetric_mase :: Lens.Lens' ErrorMetric (Prelude.Maybe Prelude.Double)
errorMetric_mase = Lens.lens (\ErrorMetric' {mase} -> mase) (\s@ErrorMetric' {} a -> s {mase = a} :: ErrorMetric)

-- | The weighted absolute percentage error (WAPE).
errorMetric_wape :: Lens.Lens' ErrorMetric (Prelude.Maybe Prelude.Double)
errorMetric_wape = Lens.lens (\ErrorMetric' {wape} -> wape) (\s@ErrorMetric' {} a -> s {wape = a} :: ErrorMetric)

-- | The Mean Absolute Percentage Error (MAPE)
errorMetric_mape :: Lens.Lens' ErrorMetric (Prelude.Maybe Prelude.Double)
errorMetric_mape = Lens.lens (\ErrorMetric' {mape} -> mape) (\s@ErrorMetric' {} a -> s {mape = a} :: ErrorMetric)

-- | The root-mean-square error (RMSE).
errorMetric_rmse :: Lens.Lens' ErrorMetric (Prelude.Maybe Prelude.Double)
errorMetric_rmse = Lens.lens (\ErrorMetric' {rmse} -> rmse) (\s@ErrorMetric' {} a -> s {rmse = a} :: ErrorMetric)

-- | The Forecast type used to compute WAPE, MAPE, MASE, and RMSE.
errorMetric_forecastType :: Lens.Lens' ErrorMetric (Prelude.Maybe Prelude.Text)
errorMetric_forecastType = Lens.lens (\ErrorMetric' {forecastType} -> forecastType) (\s@ErrorMetric' {} a -> s {forecastType = a} :: ErrorMetric)

instance Core.FromJSON ErrorMetric where
  parseJSON =
    Core.withObject
      "ErrorMetric"
      ( \x ->
          ErrorMetric'
            Prelude.<$> (x Core..:? "MASE")
            Prelude.<*> (x Core..:? "WAPE")
            Prelude.<*> (x Core..:? "MAPE")
            Prelude.<*> (x Core..:? "RMSE")
            Prelude.<*> (x Core..:? "ForecastType")
      )

instance Prelude.Hashable ErrorMetric

instance Prelude.NFData ErrorMetric
