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
-- Module      : Amazonka.XRay.Types.TimeSeriesServiceStatistics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types.TimeSeriesServiceStatistics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.XRay.Types.EdgeStatistics
import Amazonka.XRay.Types.ForecastStatistics
import Amazonka.XRay.Types.HistogramEntry
import Amazonka.XRay.Types.ServiceStatistics

-- | A list of TimeSeriesStatistic structures.
--
-- /See:/ 'newTimeSeriesServiceStatistics' smart constructor.
data TimeSeriesServiceStatistics = TimeSeriesServiceStatistics'
  { edgeSummaryStatistics :: Prelude.Maybe EdgeStatistics,
    -- | The response time histogram for the selected entities.
    responseTimeHistogram :: Prelude.Maybe [HistogramEntry],
    -- | The forecasted high and low fault count values.
    serviceForecastStatistics :: Prelude.Maybe ForecastStatistics,
    serviceSummaryStatistics :: Prelude.Maybe ServiceStatistics,
    -- | Timestamp of the window for which statistics are aggregated.
    timestamp :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TimeSeriesServiceStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'edgeSummaryStatistics', 'timeSeriesServiceStatistics_edgeSummaryStatistics' - Undocumented member.
--
-- 'responseTimeHistogram', 'timeSeriesServiceStatistics_responseTimeHistogram' - The response time histogram for the selected entities.
--
-- 'serviceForecastStatistics', 'timeSeriesServiceStatistics_serviceForecastStatistics' - The forecasted high and low fault count values.
--
-- 'serviceSummaryStatistics', 'timeSeriesServiceStatistics_serviceSummaryStatistics' - Undocumented member.
--
-- 'timestamp', 'timeSeriesServiceStatistics_timestamp' - Timestamp of the window for which statistics are aggregated.
newTimeSeriesServiceStatistics ::
  TimeSeriesServiceStatistics
newTimeSeriesServiceStatistics =
  TimeSeriesServiceStatistics'
    { edgeSummaryStatistics =
        Prelude.Nothing,
      responseTimeHistogram = Prelude.Nothing,
      serviceForecastStatistics = Prelude.Nothing,
      serviceSummaryStatistics = Prelude.Nothing,
      timestamp = Prelude.Nothing
    }

-- | Undocumented member.
timeSeriesServiceStatistics_edgeSummaryStatistics :: Lens.Lens' TimeSeriesServiceStatistics (Prelude.Maybe EdgeStatistics)
timeSeriesServiceStatistics_edgeSummaryStatistics = Lens.lens (\TimeSeriesServiceStatistics' {edgeSummaryStatistics} -> edgeSummaryStatistics) (\s@TimeSeriesServiceStatistics' {} a -> s {edgeSummaryStatistics = a} :: TimeSeriesServiceStatistics)

-- | The response time histogram for the selected entities.
timeSeriesServiceStatistics_responseTimeHistogram :: Lens.Lens' TimeSeriesServiceStatistics (Prelude.Maybe [HistogramEntry])
timeSeriesServiceStatistics_responseTimeHistogram = Lens.lens (\TimeSeriesServiceStatistics' {responseTimeHistogram} -> responseTimeHistogram) (\s@TimeSeriesServiceStatistics' {} a -> s {responseTimeHistogram = a} :: TimeSeriesServiceStatistics) Prelude.. Lens.mapping Lens.coerced

-- | The forecasted high and low fault count values.
timeSeriesServiceStatistics_serviceForecastStatistics :: Lens.Lens' TimeSeriesServiceStatistics (Prelude.Maybe ForecastStatistics)
timeSeriesServiceStatistics_serviceForecastStatistics = Lens.lens (\TimeSeriesServiceStatistics' {serviceForecastStatistics} -> serviceForecastStatistics) (\s@TimeSeriesServiceStatistics' {} a -> s {serviceForecastStatistics = a} :: TimeSeriesServiceStatistics)

-- | Undocumented member.
timeSeriesServiceStatistics_serviceSummaryStatistics :: Lens.Lens' TimeSeriesServiceStatistics (Prelude.Maybe ServiceStatistics)
timeSeriesServiceStatistics_serviceSummaryStatistics = Lens.lens (\TimeSeriesServiceStatistics' {serviceSummaryStatistics} -> serviceSummaryStatistics) (\s@TimeSeriesServiceStatistics' {} a -> s {serviceSummaryStatistics = a} :: TimeSeriesServiceStatistics)

-- | Timestamp of the window for which statistics are aggregated.
timeSeriesServiceStatistics_timestamp :: Lens.Lens' TimeSeriesServiceStatistics (Prelude.Maybe Prelude.UTCTime)
timeSeriesServiceStatistics_timestamp = Lens.lens (\TimeSeriesServiceStatistics' {timestamp} -> timestamp) (\s@TimeSeriesServiceStatistics' {} a -> s {timestamp = a} :: TimeSeriesServiceStatistics) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON TimeSeriesServiceStatistics where
  parseJSON =
    Data.withObject
      "TimeSeriesServiceStatistics"
      ( \x ->
          TimeSeriesServiceStatistics'
            Prelude.<$> (x Data..:? "EdgeSummaryStatistics")
            Prelude.<*> ( x
                            Data..:? "ResponseTimeHistogram"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ServiceForecastStatistics")
            Prelude.<*> (x Data..:? "ServiceSummaryStatistics")
            Prelude.<*> (x Data..:? "Timestamp")
      )

instance Prelude.Hashable TimeSeriesServiceStatistics where
  hashWithSalt _salt TimeSeriesServiceStatistics' {..} =
    _salt
      `Prelude.hashWithSalt` edgeSummaryStatistics
      `Prelude.hashWithSalt` responseTimeHistogram
      `Prelude.hashWithSalt` serviceForecastStatistics
      `Prelude.hashWithSalt` serviceSummaryStatistics
      `Prelude.hashWithSalt` timestamp

instance Prelude.NFData TimeSeriesServiceStatistics where
  rnf TimeSeriesServiceStatistics' {..} =
    Prelude.rnf edgeSummaryStatistics
      `Prelude.seq` Prelude.rnf responseTimeHistogram
      `Prelude.seq` Prelude.rnf serviceForecastStatistics
      `Prelude.seq` Prelude.rnf serviceSummaryStatistics
      `Prelude.seq` Prelude.rnf timestamp
