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
    frMeanValue,
    frPredictionIntervalLowerBound,
    frPredictionIntervalUpperBound,
    frTimePeriod,
  )
where

import qualified Network.AWS.CostExplorer.Types.DateInterval as Types
import qualified Network.AWS.CostExplorer.Types.MeanValue as Types
import qualified Network.AWS.CostExplorer.Types.PredictionIntervalLowerBound as Types
import qualified Network.AWS.CostExplorer.Types.PredictionIntervalUpperBound as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The forecast created for your query.
--
-- /See:/ 'mkForecastResult' smart constructor.
data ForecastResult = ForecastResult'
  { -- | The mean value of the forecast.
    meanValue :: Core.Maybe Types.MeanValue,
    -- | The lower limit for the prediction interval.
    predictionIntervalLowerBound :: Core.Maybe Types.PredictionIntervalLowerBound,
    -- | The upper limit for the prediction interval.
    predictionIntervalUpperBound :: Core.Maybe Types.PredictionIntervalUpperBound,
    -- | The period of time that the forecast covers.
    timePeriod :: Core.Maybe Types.DateInterval
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ForecastResult' value with any optional fields omitted.
mkForecastResult ::
  ForecastResult
mkForecastResult =
  ForecastResult'
    { meanValue = Core.Nothing,
      predictionIntervalLowerBound = Core.Nothing,
      predictionIntervalUpperBound = Core.Nothing,
      timePeriod = Core.Nothing
    }

-- | The mean value of the forecast.
--
-- /Note:/ Consider using 'meanValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
frMeanValue :: Lens.Lens' ForecastResult (Core.Maybe Types.MeanValue)
frMeanValue = Lens.field @"meanValue"
{-# DEPRECATED frMeanValue "Use generic-lens or generic-optics with 'meanValue' instead." #-}

-- | The lower limit for the prediction interval.
--
-- /Note:/ Consider using 'predictionIntervalLowerBound' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
frPredictionIntervalLowerBound :: Lens.Lens' ForecastResult (Core.Maybe Types.PredictionIntervalLowerBound)
frPredictionIntervalLowerBound = Lens.field @"predictionIntervalLowerBound"
{-# DEPRECATED frPredictionIntervalLowerBound "Use generic-lens or generic-optics with 'predictionIntervalLowerBound' instead." #-}

-- | The upper limit for the prediction interval.
--
-- /Note:/ Consider using 'predictionIntervalUpperBound' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
frPredictionIntervalUpperBound :: Lens.Lens' ForecastResult (Core.Maybe Types.PredictionIntervalUpperBound)
frPredictionIntervalUpperBound = Lens.field @"predictionIntervalUpperBound"
{-# DEPRECATED frPredictionIntervalUpperBound "Use generic-lens or generic-optics with 'predictionIntervalUpperBound' instead." #-}

-- | The period of time that the forecast covers.
--
-- /Note:/ Consider using 'timePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
frTimePeriod :: Lens.Lens' ForecastResult (Core.Maybe Types.DateInterval)
frTimePeriod = Lens.field @"timePeriod"
{-# DEPRECATED frTimePeriod "Use generic-lens or generic-optics with 'timePeriod' instead." #-}

instance Core.FromJSON ForecastResult where
  parseJSON =
    Core.withObject "ForecastResult" Core.$
      \x ->
        ForecastResult'
          Core.<$> (x Core..:? "MeanValue")
          Core.<*> (x Core..:? "PredictionIntervalLowerBound")
          Core.<*> (x Core..:? "PredictionIntervalUpperBound")
          Core.<*> (x Core..:? "TimePeriod")
