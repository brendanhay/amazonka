{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.ForecastResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.ForecastResult
  ( ForecastResult (..),

    -- * Smart constructor
    mkForecastResult,

    -- * Lenses
    frTimePeriod,
    frMeanValue,
    frPredictionIntervalUpperBound,
    frPredictionIntervalLowerBound,
  )
where

import Network.AWS.CostExplorer.Types.DateInterval
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The forecast created for your query.
--
-- /See:/ 'mkForecastResult' smart constructor.
data ForecastResult = ForecastResult'
  { timePeriod ::
      Lude.Maybe DateInterval,
    meanValue :: Lude.Maybe Lude.Text,
    predictionIntervalUpperBound :: Lude.Maybe Lude.Text,
    predictionIntervalLowerBound :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ForecastResult' with the minimum fields required to make a request.
--
-- * 'meanValue' - The mean value of the forecast.
-- * 'predictionIntervalLowerBound' - The lower limit for the prediction interval.
-- * 'predictionIntervalUpperBound' - The upper limit for the prediction interval.
-- * 'timePeriod' - The period of time that the forecast covers.
mkForecastResult ::
  ForecastResult
mkForecastResult =
  ForecastResult'
    { timePeriod = Lude.Nothing,
      meanValue = Lude.Nothing,
      predictionIntervalUpperBound = Lude.Nothing,
      predictionIntervalLowerBound = Lude.Nothing
    }

-- | The period of time that the forecast covers.
--
-- /Note:/ Consider using 'timePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
frTimePeriod :: Lens.Lens' ForecastResult (Lude.Maybe DateInterval)
frTimePeriod = Lens.lens (timePeriod :: ForecastResult -> Lude.Maybe DateInterval) (\s a -> s {timePeriod = a} :: ForecastResult)
{-# DEPRECATED frTimePeriod "Use generic-lens or generic-optics with 'timePeriod' instead." #-}

-- | The mean value of the forecast.
--
-- /Note:/ Consider using 'meanValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
frMeanValue :: Lens.Lens' ForecastResult (Lude.Maybe Lude.Text)
frMeanValue = Lens.lens (meanValue :: ForecastResult -> Lude.Maybe Lude.Text) (\s a -> s {meanValue = a} :: ForecastResult)
{-# DEPRECATED frMeanValue "Use generic-lens or generic-optics with 'meanValue' instead." #-}

-- | The upper limit for the prediction interval.
--
-- /Note:/ Consider using 'predictionIntervalUpperBound' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
frPredictionIntervalUpperBound :: Lens.Lens' ForecastResult (Lude.Maybe Lude.Text)
frPredictionIntervalUpperBound = Lens.lens (predictionIntervalUpperBound :: ForecastResult -> Lude.Maybe Lude.Text) (\s a -> s {predictionIntervalUpperBound = a} :: ForecastResult)
{-# DEPRECATED frPredictionIntervalUpperBound "Use generic-lens or generic-optics with 'predictionIntervalUpperBound' instead." #-}

-- | The lower limit for the prediction interval.
--
-- /Note:/ Consider using 'predictionIntervalLowerBound' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
frPredictionIntervalLowerBound :: Lens.Lens' ForecastResult (Lude.Maybe Lude.Text)
frPredictionIntervalLowerBound = Lens.lens (predictionIntervalLowerBound :: ForecastResult -> Lude.Maybe Lude.Text) (\s a -> s {predictionIntervalLowerBound = a} :: ForecastResult)
{-# DEPRECATED frPredictionIntervalLowerBound "Use generic-lens or generic-optics with 'predictionIntervalLowerBound' instead." #-}

instance Lude.FromJSON ForecastResult where
  parseJSON =
    Lude.withObject
      "ForecastResult"
      ( \x ->
          ForecastResult'
            Lude.<$> (x Lude..:? "TimePeriod")
            Lude.<*> (x Lude..:? "MeanValue")
            Lude.<*> (x Lude..:? "PredictionIntervalUpperBound")
            Lude.<*> (x Lude..:? "PredictionIntervalLowerBound")
      )
