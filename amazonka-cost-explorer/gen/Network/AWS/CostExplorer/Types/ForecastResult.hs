{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CostExplorer.Types.ForecastResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.ForecastResult where

import Network.AWS.CostExplorer.Types.DateInterval
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The forecast created for your query.
--
-- /See:/ 'newForecastResult' smart constructor.
data ForecastResult = ForecastResult'
  { -- | The mean value of the forecast.
    meanValue :: Prelude.Maybe Prelude.Text,
    -- | The period of time that the forecast covers.
    timePeriod :: Prelude.Maybe DateInterval,
    -- | The lower limit for the prediction interval.
    predictionIntervalLowerBound :: Prelude.Maybe Prelude.Text,
    -- | The upper limit for the prediction interval.
    predictionIntervalUpperBound :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'timePeriod', 'forecastResult_timePeriod' - The period of time that the forecast covers.
--
-- 'predictionIntervalLowerBound', 'forecastResult_predictionIntervalLowerBound' - The lower limit for the prediction interval.
--
-- 'predictionIntervalUpperBound', 'forecastResult_predictionIntervalUpperBound' - The upper limit for the prediction interval.
newForecastResult ::
  ForecastResult
newForecastResult =
  ForecastResult'
    { meanValue = Prelude.Nothing,
      timePeriod = Prelude.Nothing,
      predictionIntervalLowerBound = Prelude.Nothing,
      predictionIntervalUpperBound = Prelude.Nothing
    }

-- | The mean value of the forecast.
forecastResult_meanValue :: Lens.Lens' ForecastResult (Prelude.Maybe Prelude.Text)
forecastResult_meanValue = Lens.lens (\ForecastResult' {meanValue} -> meanValue) (\s@ForecastResult' {} a -> s {meanValue = a} :: ForecastResult)

-- | The period of time that the forecast covers.
forecastResult_timePeriod :: Lens.Lens' ForecastResult (Prelude.Maybe DateInterval)
forecastResult_timePeriod = Lens.lens (\ForecastResult' {timePeriod} -> timePeriod) (\s@ForecastResult' {} a -> s {timePeriod = a} :: ForecastResult)

-- | The lower limit for the prediction interval.
forecastResult_predictionIntervalLowerBound :: Lens.Lens' ForecastResult (Prelude.Maybe Prelude.Text)
forecastResult_predictionIntervalLowerBound = Lens.lens (\ForecastResult' {predictionIntervalLowerBound} -> predictionIntervalLowerBound) (\s@ForecastResult' {} a -> s {predictionIntervalLowerBound = a} :: ForecastResult)

-- | The upper limit for the prediction interval.
forecastResult_predictionIntervalUpperBound :: Lens.Lens' ForecastResult (Prelude.Maybe Prelude.Text)
forecastResult_predictionIntervalUpperBound = Lens.lens (\ForecastResult' {predictionIntervalUpperBound} -> predictionIntervalUpperBound) (\s@ForecastResult' {} a -> s {predictionIntervalUpperBound = a} :: ForecastResult)

instance Prelude.FromJSON ForecastResult where
  parseJSON =
    Prelude.withObject
      "ForecastResult"
      ( \x ->
          ForecastResult'
            Prelude.<$> (x Prelude..:? "MeanValue")
            Prelude.<*> (x Prelude..:? "TimePeriod")
            Prelude.<*> (x Prelude..:? "PredictionIntervalLowerBound")
            Prelude.<*> (x Prelude..:? "PredictionIntervalUpperBound")
      )

instance Prelude.Hashable ForecastResult

instance Prelude.NFData ForecastResult
