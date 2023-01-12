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
-- Module      : Amazonka.CostExplorer.Types.ForecastResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.ForecastResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types.DateInterval
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The forecast that\'s created for your query.
--
-- /See:/ 'newForecastResult' smart constructor.
data ForecastResult = ForecastResult'
  { -- | The mean value of the forecast.
    meanValue :: Prelude.Maybe Prelude.Text,
    -- | The lower limit for the prediction interval.
    predictionIntervalLowerBound :: Prelude.Maybe Prelude.Text,
    -- | The upper limit for the prediction interval.
    predictionIntervalUpperBound :: Prelude.Maybe Prelude.Text,
    -- | The period of time that the forecast covers.
    timePeriod :: Prelude.Maybe DateInterval
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ForecastResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'meanValue', 'forecastResult_meanValue' - The mean value of the forecast.
--
-- 'predictionIntervalLowerBound', 'forecastResult_predictionIntervalLowerBound' - The lower limit for the prediction interval.
--
-- 'predictionIntervalUpperBound', 'forecastResult_predictionIntervalUpperBound' - The upper limit for the prediction interval.
--
-- 'timePeriod', 'forecastResult_timePeriod' - The period of time that the forecast covers.
newForecastResult ::
  ForecastResult
newForecastResult =
  ForecastResult'
    { meanValue = Prelude.Nothing,
      predictionIntervalLowerBound = Prelude.Nothing,
      predictionIntervalUpperBound = Prelude.Nothing,
      timePeriod = Prelude.Nothing
    }

-- | The mean value of the forecast.
forecastResult_meanValue :: Lens.Lens' ForecastResult (Prelude.Maybe Prelude.Text)
forecastResult_meanValue = Lens.lens (\ForecastResult' {meanValue} -> meanValue) (\s@ForecastResult' {} a -> s {meanValue = a} :: ForecastResult)

-- | The lower limit for the prediction interval.
forecastResult_predictionIntervalLowerBound :: Lens.Lens' ForecastResult (Prelude.Maybe Prelude.Text)
forecastResult_predictionIntervalLowerBound = Lens.lens (\ForecastResult' {predictionIntervalLowerBound} -> predictionIntervalLowerBound) (\s@ForecastResult' {} a -> s {predictionIntervalLowerBound = a} :: ForecastResult)

-- | The upper limit for the prediction interval.
forecastResult_predictionIntervalUpperBound :: Lens.Lens' ForecastResult (Prelude.Maybe Prelude.Text)
forecastResult_predictionIntervalUpperBound = Lens.lens (\ForecastResult' {predictionIntervalUpperBound} -> predictionIntervalUpperBound) (\s@ForecastResult' {} a -> s {predictionIntervalUpperBound = a} :: ForecastResult)

-- | The period of time that the forecast covers.
forecastResult_timePeriod :: Lens.Lens' ForecastResult (Prelude.Maybe DateInterval)
forecastResult_timePeriod = Lens.lens (\ForecastResult' {timePeriod} -> timePeriod) (\s@ForecastResult' {} a -> s {timePeriod = a} :: ForecastResult)

instance Data.FromJSON ForecastResult where
  parseJSON =
    Data.withObject
      "ForecastResult"
      ( \x ->
          ForecastResult'
            Prelude.<$> (x Data..:? "MeanValue")
            Prelude.<*> (x Data..:? "PredictionIntervalLowerBound")
            Prelude.<*> (x Data..:? "PredictionIntervalUpperBound")
            Prelude.<*> (x Data..:? "TimePeriod")
      )

instance Prelude.Hashable ForecastResult where
  hashWithSalt _salt ForecastResult' {..} =
    _salt `Prelude.hashWithSalt` meanValue
      `Prelude.hashWithSalt` predictionIntervalLowerBound
      `Prelude.hashWithSalt` predictionIntervalUpperBound
      `Prelude.hashWithSalt` timePeriod

instance Prelude.NFData ForecastResult where
  rnf ForecastResult' {..} =
    Prelude.rnf meanValue
      `Prelude.seq` Prelude.rnf predictionIntervalLowerBound
      `Prelude.seq` Prelude.rnf predictionIntervalUpperBound
      `Prelude.seq` Prelude.rnf timePeriod
