{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.TimeSeriesServiceStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.XRay.Types.TimeSeriesServiceStatistics
  ( TimeSeriesServiceStatistics (..)
  -- * Smart constructor
  , mkTimeSeriesServiceStatistics
  -- * Lenses
  , tsssEdgeSummaryStatistics
  , tsssResponseTimeHistogram
  , tsssServiceForecastStatistics
  , tsssServiceSummaryStatistics
  , tsssTimestamp
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.XRay.Types.EdgeStatistics as Types
import qualified Network.AWS.XRay.Types.ForecastStatistics as Types
import qualified Network.AWS.XRay.Types.HistogramEntry as Types
import qualified Network.AWS.XRay.Types.ServiceStatistics as Types

-- | A list of TimeSeriesStatistic structures.
--
-- /See:/ 'mkTimeSeriesServiceStatistics' smart constructor.
data TimeSeriesServiceStatistics = TimeSeriesServiceStatistics'
  { edgeSummaryStatistics :: Core.Maybe Types.EdgeStatistics
  , responseTimeHistogram :: Core.Maybe [Types.HistogramEntry]
    -- ^ The response time histogram for the selected entities.
  , serviceForecastStatistics :: Core.Maybe Types.ForecastStatistics
    -- ^ The forecasted high and low fault count values.
  , serviceSummaryStatistics :: Core.Maybe Types.ServiceStatistics
  , timestamp :: Core.Maybe Core.NominalDiffTime
    -- ^ Timestamp of the window for which statistics are aggregated.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'TimeSeriesServiceStatistics' value with any optional fields omitted.
mkTimeSeriesServiceStatistics
    :: TimeSeriesServiceStatistics
mkTimeSeriesServiceStatistics
  = TimeSeriesServiceStatistics'{edgeSummaryStatistics =
                                   Core.Nothing,
                                 responseTimeHistogram = Core.Nothing,
                                 serviceForecastStatistics = Core.Nothing,
                                 serviceSummaryStatistics = Core.Nothing, timestamp = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'edgeSummaryStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsssEdgeSummaryStatistics :: Lens.Lens' TimeSeriesServiceStatistics (Core.Maybe Types.EdgeStatistics)
tsssEdgeSummaryStatistics = Lens.field @"edgeSummaryStatistics"
{-# INLINEABLE tsssEdgeSummaryStatistics #-}
{-# DEPRECATED edgeSummaryStatistics "Use generic-lens or generic-optics with 'edgeSummaryStatistics' instead"  #-}

-- | The response time histogram for the selected entities.
--
-- /Note:/ Consider using 'responseTimeHistogram' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsssResponseTimeHistogram :: Lens.Lens' TimeSeriesServiceStatistics (Core.Maybe [Types.HistogramEntry])
tsssResponseTimeHistogram = Lens.field @"responseTimeHistogram"
{-# INLINEABLE tsssResponseTimeHistogram #-}
{-# DEPRECATED responseTimeHistogram "Use generic-lens or generic-optics with 'responseTimeHistogram' instead"  #-}

-- | The forecasted high and low fault count values.
--
-- /Note:/ Consider using 'serviceForecastStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsssServiceForecastStatistics :: Lens.Lens' TimeSeriesServiceStatistics (Core.Maybe Types.ForecastStatistics)
tsssServiceForecastStatistics = Lens.field @"serviceForecastStatistics"
{-# INLINEABLE tsssServiceForecastStatistics #-}
{-# DEPRECATED serviceForecastStatistics "Use generic-lens or generic-optics with 'serviceForecastStatistics' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'serviceSummaryStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsssServiceSummaryStatistics :: Lens.Lens' TimeSeriesServiceStatistics (Core.Maybe Types.ServiceStatistics)
tsssServiceSummaryStatistics = Lens.field @"serviceSummaryStatistics"
{-# INLINEABLE tsssServiceSummaryStatistics #-}
{-# DEPRECATED serviceSummaryStatistics "Use generic-lens or generic-optics with 'serviceSummaryStatistics' instead"  #-}

-- | Timestamp of the window for which statistics are aggregated.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsssTimestamp :: Lens.Lens' TimeSeriesServiceStatistics (Core.Maybe Core.NominalDiffTime)
tsssTimestamp = Lens.field @"timestamp"
{-# INLINEABLE tsssTimestamp #-}
{-# DEPRECATED timestamp "Use generic-lens or generic-optics with 'timestamp' instead"  #-}

instance Core.FromJSON TimeSeriesServiceStatistics where
        parseJSON
          = Core.withObject "TimeSeriesServiceStatistics" Core.$
              \ x ->
                TimeSeriesServiceStatistics' Core.<$>
                  (x Core..:? "EdgeSummaryStatistics") Core.<*>
                    x Core..:? "ResponseTimeHistogram"
                    Core.<*> x Core..:? "ServiceForecastStatistics"
                    Core.<*> x Core..:? "ServiceSummaryStatistics"
                    Core.<*> x Core..:? "Timestamp"
