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
-- Module      : Amazonka.DevOpsGuru.Types.PerformanceInsightsMetricsDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.PerformanceInsightsMetricsDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DevOpsGuru.Types.PerformanceInsightsMetricQuery
import Amazonka.DevOpsGuru.Types.PerformanceInsightsReferenceData
import Amazonka.DevOpsGuru.Types.PerformanceInsightsStat
import qualified Amazonka.Prelude as Prelude

-- | Details about Performance Insights metrics.
--
-- Amazon RDS Performance Insights enables you to monitor and explore
-- different dimensions of database load based on data captured from a
-- running DB instance. DB load is measured as average active sessions.
-- Performance Insights provides the data to API consumers as a
-- two-dimensional time-series dataset. The time dimension provides DB load
-- data for each time point in the queried time range. Each time point
-- decomposes overall load in relation to the requested dimensions,
-- measured at that time point. Examples include SQL, Wait event, User, and
-- Host.
--
-- -   To learn more about Performance Insights and Amazon Aurora DB
--     instances, go to the
--     <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_PerfInsights.html Amazon Aurora User Guide>.
--
-- -   To learn more about Performance Insights and Amazon RDS DB
--     instances, go to the
--     <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Amazon RDS User Guide>.
--
-- /See:/ 'newPerformanceInsightsMetricsDetail' smart constructor.
data PerformanceInsightsMetricsDetail = PerformanceInsightsMetricsDetail'
  { -- | The name used for a specific Performance Insights metric.
    metricDisplayName :: Prelude.Maybe Prelude.Text,
    -- | For more information, see @ PerformanceInsightsReferenceData @.
    referenceData :: Prelude.Maybe [PerformanceInsightsReferenceData],
    -- | The metric statistics during the anomalous period detected by DevOps
    -- Guru;
    statsAtAnomaly :: Prelude.Maybe [PerformanceInsightsStat],
    -- | The unit of measure for a metric. For example, a session or a process.
    unit :: Prelude.Maybe Prelude.Text,
    -- | Typical metric statistics that are not considered anomalous. When DevOps
    -- Guru analyzes metrics, it compares them to @StatsAtBaseline@ to help
    -- determine if they are anomalous.
    statsAtBaseline :: Prelude.Maybe [PerformanceInsightsStat],
    -- | A single query to be processed for the metric. For more information, see
    -- @ PerformanceInsightsMetricQuery @.
    metricQuery :: Prelude.Maybe PerformanceInsightsMetricQuery
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PerformanceInsightsMetricsDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricDisplayName', 'performanceInsightsMetricsDetail_metricDisplayName' - The name used for a specific Performance Insights metric.
--
-- 'referenceData', 'performanceInsightsMetricsDetail_referenceData' - For more information, see @ PerformanceInsightsReferenceData @.
--
-- 'statsAtAnomaly', 'performanceInsightsMetricsDetail_statsAtAnomaly' - The metric statistics during the anomalous period detected by DevOps
-- Guru;
--
-- 'unit', 'performanceInsightsMetricsDetail_unit' - The unit of measure for a metric. For example, a session or a process.
--
-- 'statsAtBaseline', 'performanceInsightsMetricsDetail_statsAtBaseline' - Typical metric statistics that are not considered anomalous. When DevOps
-- Guru analyzes metrics, it compares them to @StatsAtBaseline@ to help
-- determine if they are anomalous.
--
-- 'metricQuery', 'performanceInsightsMetricsDetail_metricQuery' - A single query to be processed for the metric. For more information, see
-- @ PerformanceInsightsMetricQuery @.
newPerformanceInsightsMetricsDetail ::
  PerformanceInsightsMetricsDetail
newPerformanceInsightsMetricsDetail =
  PerformanceInsightsMetricsDetail'
    { metricDisplayName =
        Prelude.Nothing,
      referenceData = Prelude.Nothing,
      statsAtAnomaly = Prelude.Nothing,
      unit = Prelude.Nothing,
      statsAtBaseline = Prelude.Nothing,
      metricQuery = Prelude.Nothing
    }

-- | The name used for a specific Performance Insights metric.
performanceInsightsMetricsDetail_metricDisplayName :: Lens.Lens' PerformanceInsightsMetricsDetail (Prelude.Maybe Prelude.Text)
performanceInsightsMetricsDetail_metricDisplayName = Lens.lens (\PerformanceInsightsMetricsDetail' {metricDisplayName} -> metricDisplayName) (\s@PerformanceInsightsMetricsDetail' {} a -> s {metricDisplayName = a} :: PerformanceInsightsMetricsDetail)

-- | For more information, see @ PerformanceInsightsReferenceData @.
performanceInsightsMetricsDetail_referenceData :: Lens.Lens' PerformanceInsightsMetricsDetail (Prelude.Maybe [PerformanceInsightsReferenceData])
performanceInsightsMetricsDetail_referenceData = Lens.lens (\PerformanceInsightsMetricsDetail' {referenceData} -> referenceData) (\s@PerformanceInsightsMetricsDetail' {} a -> s {referenceData = a} :: PerformanceInsightsMetricsDetail) Prelude.. Lens.mapping Lens.coerced

-- | The metric statistics during the anomalous period detected by DevOps
-- Guru;
performanceInsightsMetricsDetail_statsAtAnomaly :: Lens.Lens' PerformanceInsightsMetricsDetail (Prelude.Maybe [PerformanceInsightsStat])
performanceInsightsMetricsDetail_statsAtAnomaly = Lens.lens (\PerformanceInsightsMetricsDetail' {statsAtAnomaly} -> statsAtAnomaly) (\s@PerformanceInsightsMetricsDetail' {} a -> s {statsAtAnomaly = a} :: PerformanceInsightsMetricsDetail) Prelude.. Lens.mapping Lens.coerced

-- | The unit of measure for a metric. For example, a session or a process.
performanceInsightsMetricsDetail_unit :: Lens.Lens' PerformanceInsightsMetricsDetail (Prelude.Maybe Prelude.Text)
performanceInsightsMetricsDetail_unit = Lens.lens (\PerformanceInsightsMetricsDetail' {unit} -> unit) (\s@PerformanceInsightsMetricsDetail' {} a -> s {unit = a} :: PerformanceInsightsMetricsDetail)

-- | Typical metric statistics that are not considered anomalous. When DevOps
-- Guru analyzes metrics, it compares them to @StatsAtBaseline@ to help
-- determine if they are anomalous.
performanceInsightsMetricsDetail_statsAtBaseline :: Lens.Lens' PerformanceInsightsMetricsDetail (Prelude.Maybe [PerformanceInsightsStat])
performanceInsightsMetricsDetail_statsAtBaseline = Lens.lens (\PerformanceInsightsMetricsDetail' {statsAtBaseline} -> statsAtBaseline) (\s@PerformanceInsightsMetricsDetail' {} a -> s {statsAtBaseline = a} :: PerformanceInsightsMetricsDetail) Prelude.. Lens.mapping Lens.coerced

-- | A single query to be processed for the metric. For more information, see
-- @ PerformanceInsightsMetricQuery @.
performanceInsightsMetricsDetail_metricQuery :: Lens.Lens' PerformanceInsightsMetricsDetail (Prelude.Maybe PerformanceInsightsMetricQuery)
performanceInsightsMetricsDetail_metricQuery = Lens.lens (\PerformanceInsightsMetricsDetail' {metricQuery} -> metricQuery) (\s@PerformanceInsightsMetricsDetail' {} a -> s {metricQuery = a} :: PerformanceInsightsMetricsDetail)

instance
  Core.FromJSON
    PerformanceInsightsMetricsDetail
  where
  parseJSON =
    Core.withObject
      "PerformanceInsightsMetricsDetail"
      ( \x ->
          PerformanceInsightsMetricsDetail'
            Prelude.<$> (x Core..:? "MetricDisplayName")
            Prelude.<*> (x Core..:? "ReferenceData" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "StatsAtAnomaly" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Unit")
            Prelude.<*> ( x Core..:? "StatsAtBaseline"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "MetricQuery")
      )

instance
  Prelude.Hashable
    PerformanceInsightsMetricsDetail
  where
  hashWithSalt
    _salt
    PerformanceInsightsMetricsDetail' {..} =
      _salt `Prelude.hashWithSalt` metricDisplayName
        `Prelude.hashWithSalt` referenceData
        `Prelude.hashWithSalt` statsAtAnomaly
        `Prelude.hashWithSalt` unit
        `Prelude.hashWithSalt` statsAtBaseline
        `Prelude.hashWithSalt` metricQuery

instance
  Prelude.NFData
    PerformanceInsightsMetricsDetail
  where
  rnf PerformanceInsightsMetricsDetail' {..} =
    Prelude.rnf metricDisplayName
      `Prelude.seq` Prelude.rnf referenceData
      `Prelude.seq` Prelude.rnf statsAtAnomaly
      `Prelude.seq` Prelude.rnf unit
      `Prelude.seq` Prelude.rnf statsAtBaseline
      `Prelude.seq` Prelude.rnf metricQuery
