{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.TimeSeriesServiceStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.TimeSeriesServiceStatistics
  ( TimeSeriesServiceStatistics (..),

    -- * Smart constructor
    mkTimeSeriesServiceStatistics,

    -- * Lenses
    tsssServiceSummaryStatistics,
    tsssResponseTimeHistogram,
    tsssEdgeSummaryStatistics,
    tsssServiceForecastStatistics,
    tsssTimestamp,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.XRay.Types.EdgeStatistics
import Network.AWS.XRay.Types.ForecastStatistics
import Network.AWS.XRay.Types.HistogramEntry
import Network.AWS.XRay.Types.ServiceStatistics

-- | A list of TimeSeriesStatistic structures.
--
-- /See:/ 'mkTimeSeriesServiceStatistics' smart constructor.
data TimeSeriesServiceStatistics = TimeSeriesServiceStatistics'
  { serviceSummaryStatistics :: Lude.Maybe ServiceStatistics,
    -- | The response time histogram for the selected entities.
    responseTimeHistogram :: Lude.Maybe [HistogramEntry],
    edgeSummaryStatistics :: Lude.Maybe EdgeStatistics,
    -- | The forecasted high and low fault count values.
    serviceForecastStatistics :: Lude.Maybe ForecastStatistics,
    -- | Timestamp of the window for which statistics are aggregated.
    timestamp :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TimeSeriesServiceStatistics' with the minimum fields required to make a request.
--
-- * 'serviceSummaryStatistics' -
-- * 'responseTimeHistogram' - The response time histogram for the selected entities.
-- * 'edgeSummaryStatistics' -
-- * 'serviceForecastStatistics' - The forecasted high and low fault count values.
-- * 'timestamp' - Timestamp of the window for which statistics are aggregated.
mkTimeSeriesServiceStatistics ::
  TimeSeriesServiceStatistics
mkTimeSeriesServiceStatistics =
  TimeSeriesServiceStatistics'
    { serviceSummaryStatistics =
        Lude.Nothing,
      responseTimeHistogram = Lude.Nothing,
      edgeSummaryStatistics = Lude.Nothing,
      serviceForecastStatistics = Lude.Nothing,
      timestamp = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'serviceSummaryStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsssServiceSummaryStatistics :: Lens.Lens' TimeSeriesServiceStatistics (Lude.Maybe ServiceStatistics)
tsssServiceSummaryStatistics = Lens.lens (serviceSummaryStatistics :: TimeSeriesServiceStatistics -> Lude.Maybe ServiceStatistics) (\s a -> s {serviceSummaryStatistics = a} :: TimeSeriesServiceStatistics)
{-# DEPRECATED tsssServiceSummaryStatistics "Use generic-lens or generic-optics with 'serviceSummaryStatistics' instead." #-}

-- | The response time histogram for the selected entities.
--
-- /Note:/ Consider using 'responseTimeHistogram' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsssResponseTimeHistogram :: Lens.Lens' TimeSeriesServiceStatistics (Lude.Maybe [HistogramEntry])
tsssResponseTimeHistogram = Lens.lens (responseTimeHistogram :: TimeSeriesServiceStatistics -> Lude.Maybe [HistogramEntry]) (\s a -> s {responseTimeHistogram = a} :: TimeSeriesServiceStatistics)
{-# DEPRECATED tsssResponseTimeHistogram "Use generic-lens or generic-optics with 'responseTimeHistogram' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'edgeSummaryStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsssEdgeSummaryStatistics :: Lens.Lens' TimeSeriesServiceStatistics (Lude.Maybe EdgeStatistics)
tsssEdgeSummaryStatistics = Lens.lens (edgeSummaryStatistics :: TimeSeriesServiceStatistics -> Lude.Maybe EdgeStatistics) (\s a -> s {edgeSummaryStatistics = a} :: TimeSeriesServiceStatistics)
{-# DEPRECATED tsssEdgeSummaryStatistics "Use generic-lens or generic-optics with 'edgeSummaryStatistics' instead." #-}

-- | The forecasted high and low fault count values.
--
-- /Note:/ Consider using 'serviceForecastStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsssServiceForecastStatistics :: Lens.Lens' TimeSeriesServiceStatistics (Lude.Maybe ForecastStatistics)
tsssServiceForecastStatistics = Lens.lens (serviceForecastStatistics :: TimeSeriesServiceStatistics -> Lude.Maybe ForecastStatistics) (\s a -> s {serviceForecastStatistics = a} :: TimeSeriesServiceStatistics)
{-# DEPRECATED tsssServiceForecastStatistics "Use generic-lens or generic-optics with 'serviceForecastStatistics' instead." #-}

-- | Timestamp of the window for which statistics are aggregated.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsssTimestamp :: Lens.Lens' TimeSeriesServiceStatistics (Lude.Maybe Lude.Timestamp)
tsssTimestamp = Lens.lens (timestamp :: TimeSeriesServiceStatistics -> Lude.Maybe Lude.Timestamp) (\s a -> s {timestamp = a} :: TimeSeriesServiceStatistics)
{-# DEPRECATED tsssTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

instance Lude.FromJSON TimeSeriesServiceStatistics where
  parseJSON =
    Lude.withObject
      "TimeSeriesServiceStatistics"
      ( \x ->
          TimeSeriesServiceStatistics'
            Lude.<$> (x Lude..:? "ServiceSummaryStatistics")
            Lude.<*> (x Lude..:? "ResponseTimeHistogram" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "EdgeSummaryStatistics")
            Lude.<*> (x Lude..:? "ServiceForecastStatistics")
            Lude.<*> (x Lude..:? "Timestamp")
      )
