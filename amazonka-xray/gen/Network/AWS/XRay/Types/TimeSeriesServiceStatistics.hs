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
-- Module      : Network.AWS.XRay.Types.TimeSeriesServiceStatistics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.TimeSeriesServiceStatistics where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.XRay.Types.EdgeStatistics
import Network.AWS.XRay.Types.ForecastStatistics
import Network.AWS.XRay.Types.HistogramEntry
import Network.AWS.XRay.Types.ServiceStatistics

-- | A list of TimeSeriesStatistic structures.
--
-- /See:/ 'newTimeSeriesServiceStatistics' smart constructor.
data TimeSeriesServiceStatistics = TimeSeriesServiceStatistics'
  { serviceSummaryStatistics :: Core.Maybe ServiceStatistics,
    -- | The response time histogram for the selected entities.
    responseTimeHistogram :: Core.Maybe [HistogramEntry],
    -- | The forecasted high and low fault count values.
    serviceForecastStatistics :: Core.Maybe ForecastStatistics,
    edgeSummaryStatistics :: Core.Maybe EdgeStatistics,
    -- | Timestamp of the window for which statistics are aggregated.
    timestamp :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TimeSeriesServiceStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceSummaryStatistics', 'timeSeriesServiceStatistics_serviceSummaryStatistics' - Undocumented member.
--
-- 'responseTimeHistogram', 'timeSeriesServiceStatistics_responseTimeHistogram' - The response time histogram for the selected entities.
--
-- 'serviceForecastStatistics', 'timeSeriesServiceStatistics_serviceForecastStatistics' - The forecasted high and low fault count values.
--
-- 'edgeSummaryStatistics', 'timeSeriesServiceStatistics_edgeSummaryStatistics' - Undocumented member.
--
-- 'timestamp', 'timeSeriesServiceStatistics_timestamp' - Timestamp of the window for which statistics are aggregated.
newTimeSeriesServiceStatistics ::
  TimeSeriesServiceStatistics
newTimeSeriesServiceStatistics =
  TimeSeriesServiceStatistics'
    { serviceSummaryStatistics =
        Core.Nothing,
      responseTimeHistogram = Core.Nothing,
      serviceForecastStatistics = Core.Nothing,
      edgeSummaryStatistics = Core.Nothing,
      timestamp = Core.Nothing
    }

-- | Undocumented member.
timeSeriesServiceStatistics_serviceSummaryStatistics :: Lens.Lens' TimeSeriesServiceStatistics (Core.Maybe ServiceStatistics)
timeSeriesServiceStatistics_serviceSummaryStatistics = Lens.lens (\TimeSeriesServiceStatistics' {serviceSummaryStatistics} -> serviceSummaryStatistics) (\s@TimeSeriesServiceStatistics' {} a -> s {serviceSummaryStatistics = a} :: TimeSeriesServiceStatistics)

-- | The response time histogram for the selected entities.
timeSeriesServiceStatistics_responseTimeHistogram :: Lens.Lens' TimeSeriesServiceStatistics (Core.Maybe [HistogramEntry])
timeSeriesServiceStatistics_responseTimeHistogram = Lens.lens (\TimeSeriesServiceStatistics' {responseTimeHistogram} -> responseTimeHistogram) (\s@TimeSeriesServiceStatistics' {} a -> s {responseTimeHistogram = a} :: TimeSeriesServiceStatistics) Core.. Lens.mapping Lens._Coerce

-- | The forecasted high and low fault count values.
timeSeriesServiceStatistics_serviceForecastStatistics :: Lens.Lens' TimeSeriesServiceStatistics (Core.Maybe ForecastStatistics)
timeSeriesServiceStatistics_serviceForecastStatistics = Lens.lens (\TimeSeriesServiceStatistics' {serviceForecastStatistics} -> serviceForecastStatistics) (\s@TimeSeriesServiceStatistics' {} a -> s {serviceForecastStatistics = a} :: TimeSeriesServiceStatistics)

-- | Undocumented member.
timeSeriesServiceStatistics_edgeSummaryStatistics :: Lens.Lens' TimeSeriesServiceStatistics (Core.Maybe EdgeStatistics)
timeSeriesServiceStatistics_edgeSummaryStatistics = Lens.lens (\TimeSeriesServiceStatistics' {edgeSummaryStatistics} -> edgeSummaryStatistics) (\s@TimeSeriesServiceStatistics' {} a -> s {edgeSummaryStatistics = a} :: TimeSeriesServiceStatistics)

-- | Timestamp of the window for which statistics are aggregated.
timeSeriesServiceStatistics_timestamp :: Lens.Lens' TimeSeriesServiceStatistics (Core.Maybe Core.UTCTime)
timeSeriesServiceStatistics_timestamp = Lens.lens (\TimeSeriesServiceStatistics' {timestamp} -> timestamp) (\s@TimeSeriesServiceStatistics' {} a -> s {timestamp = a} :: TimeSeriesServiceStatistics) Core.. Lens.mapping Core._Time

instance Core.FromJSON TimeSeriesServiceStatistics where
  parseJSON =
    Core.withObject
      "TimeSeriesServiceStatistics"
      ( \x ->
          TimeSeriesServiceStatistics'
            Core.<$> (x Core..:? "ServiceSummaryStatistics")
            Core.<*> ( x Core..:? "ResponseTimeHistogram"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "ServiceForecastStatistics")
            Core.<*> (x Core..:? "EdgeSummaryStatistics")
            Core.<*> (x Core..:? "Timestamp")
      )

instance Core.Hashable TimeSeriesServiceStatistics

instance Core.NFData TimeSeriesServiceStatistics
