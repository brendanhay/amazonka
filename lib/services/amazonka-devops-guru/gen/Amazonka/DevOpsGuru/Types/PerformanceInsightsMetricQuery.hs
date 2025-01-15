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
-- Module      : Amazonka.DevOpsGuru.Types.PerformanceInsightsMetricQuery
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.PerformanceInsightsMetricQuery where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types.PerformanceInsightsMetricDimensionGroup
import qualified Amazonka.Prelude as Prelude

-- | A single query to be processed. Use these parameters to query the
-- Performance Insights @GetResourceMetrics@ API to retrieve the metrics
-- for an anomaly. For more information, see
-- @ @<https://docs.aws.amazon.com/performance-insights/latest/APIReference/API_GetResourceMetrics.html GetResourceMetrics>@ @
-- in the /Amazon RDS Performance Insights API Reference/.
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
-- /See:/ 'newPerformanceInsightsMetricQuery' smart constructor.
data PerformanceInsightsMetricQuery = PerformanceInsightsMetricQuery'
  { -- | One or more filters to apply to a Performance Insights
    -- @GetResourceMetrics@ API query. Restrictions:
    --
    -- -   Any number of filters by the same dimension, as specified in the
    --     @GroupBy@ parameter.
    --
    -- -   A single filter for any other dimension in this dimension group.
    filter' :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The specification for how to aggregate the data points from a
    -- Performance Insights @GetResourceMetrics@ API query. The Performance
    -- Insights query returns all of the dimensions within that group, unless
    -- you provide the names of specific dimensions within that group. You can
    -- also request that Performance Insights return a limited number of values
    -- for a dimension.
    groupBy :: Prelude.Maybe PerformanceInsightsMetricDimensionGroup,
    -- | The name of the meteric used used when querying an Performance Insights
    -- @GetResourceMetrics@ API for anomaly metrics.
    --
    -- Valid values for @Metric@ are:
    --
    -- -   @db.load.avg@ - a scaled representation of the number of active
    --     sessions for the database engine.
    --
    -- -   @db.sampledload.avg@ - the raw number of active sessions for the
    --     database engine.
    --
    -- If the number of active sessions is less than an internal Performance
    -- Insights threshold, @db.load.avg@ and @db.sampledload.avg@ are the same
    -- value. If the number of active sessions is greater than the internal
    -- threshold, Performance Insights samples the active sessions, with
    -- @db.load.avg@ showing the scaled values, @db.sampledload.avg@ showing
    -- the raw values, and @db.sampledload.avg@ less than @db.load.avg@. For
    -- most use cases, you can query @db.load.avg@ only.
    metric :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PerformanceInsightsMetricQuery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filter'', 'performanceInsightsMetricQuery_filter' - One or more filters to apply to a Performance Insights
-- @GetResourceMetrics@ API query. Restrictions:
--
-- -   Any number of filters by the same dimension, as specified in the
--     @GroupBy@ parameter.
--
-- -   A single filter for any other dimension in this dimension group.
--
-- 'groupBy', 'performanceInsightsMetricQuery_groupBy' - The specification for how to aggregate the data points from a
-- Performance Insights @GetResourceMetrics@ API query. The Performance
-- Insights query returns all of the dimensions within that group, unless
-- you provide the names of specific dimensions within that group. You can
-- also request that Performance Insights return a limited number of values
-- for a dimension.
--
-- 'metric', 'performanceInsightsMetricQuery_metric' - The name of the meteric used used when querying an Performance Insights
-- @GetResourceMetrics@ API for anomaly metrics.
--
-- Valid values for @Metric@ are:
--
-- -   @db.load.avg@ - a scaled representation of the number of active
--     sessions for the database engine.
--
-- -   @db.sampledload.avg@ - the raw number of active sessions for the
--     database engine.
--
-- If the number of active sessions is less than an internal Performance
-- Insights threshold, @db.load.avg@ and @db.sampledload.avg@ are the same
-- value. If the number of active sessions is greater than the internal
-- threshold, Performance Insights samples the active sessions, with
-- @db.load.avg@ showing the scaled values, @db.sampledload.avg@ showing
-- the raw values, and @db.sampledload.avg@ less than @db.load.avg@. For
-- most use cases, you can query @db.load.avg@ only.
newPerformanceInsightsMetricQuery ::
  PerformanceInsightsMetricQuery
newPerformanceInsightsMetricQuery =
  PerformanceInsightsMetricQuery'
    { filter' =
        Prelude.Nothing,
      groupBy = Prelude.Nothing,
      metric = Prelude.Nothing
    }

-- | One or more filters to apply to a Performance Insights
-- @GetResourceMetrics@ API query. Restrictions:
--
-- -   Any number of filters by the same dimension, as specified in the
--     @GroupBy@ parameter.
--
-- -   A single filter for any other dimension in this dimension group.
performanceInsightsMetricQuery_filter :: Lens.Lens' PerformanceInsightsMetricQuery (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
performanceInsightsMetricQuery_filter = Lens.lens (\PerformanceInsightsMetricQuery' {filter'} -> filter') (\s@PerformanceInsightsMetricQuery' {} a -> s {filter' = a} :: PerformanceInsightsMetricQuery) Prelude.. Lens.mapping Lens.coerced

-- | The specification for how to aggregate the data points from a
-- Performance Insights @GetResourceMetrics@ API query. The Performance
-- Insights query returns all of the dimensions within that group, unless
-- you provide the names of specific dimensions within that group. You can
-- also request that Performance Insights return a limited number of values
-- for a dimension.
performanceInsightsMetricQuery_groupBy :: Lens.Lens' PerformanceInsightsMetricQuery (Prelude.Maybe PerformanceInsightsMetricDimensionGroup)
performanceInsightsMetricQuery_groupBy = Lens.lens (\PerformanceInsightsMetricQuery' {groupBy} -> groupBy) (\s@PerformanceInsightsMetricQuery' {} a -> s {groupBy = a} :: PerformanceInsightsMetricQuery)

-- | The name of the meteric used used when querying an Performance Insights
-- @GetResourceMetrics@ API for anomaly metrics.
--
-- Valid values for @Metric@ are:
--
-- -   @db.load.avg@ - a scaled representation of the number of active
--     sessions for the database engine.
--
-- -   @db.sampledload.avg@ - the raw number of active sessions for the
--     database engine.
--
-- If the number of active sessions is less than an internal Performance
-- Insights threshold, @db.load.avg@ and @db.sampledload.avg@ are the same
-- value. If the number of active sessions is greater than the internal
-- threshold, Performance Insights samples the active sessions, with
-- @db.load.avg@ showing the scaled values, @db.sampledload.avg@ showing
-- the raw values, and @db.sampledload.avg@ less than @db.load.avg@. For
-- most use cases, you can query @db.load.avg@ only.
performanceInsightsMetricQuery_metric :: Lens.Lens' PerformanceInsightsMetricQuery (Prelude.Maybe Prelude.Text)
performanceInsightsMetricQuery_metric = Lens.lens (\PerformanceInsightsMetricQuery' {metric} -> metric) (\s@PerformanceInsightsMetricQuery' {} a -> s {metric = a} :: PerformanceInsightsMetricQuery)

instance Data.FromJSON PerformanceInsightsMetricQuery where
  parseJSON =
    Data.withObject
      "PerformanceInsightsMetricQuery"
      ( \x ->
          PerformanceInsightsMetricQuery'
            Prelude.<$> (x Data..:? "Filter" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "GroupBy")
            Prelude.<*> (x Data..:? "Metric")
      )

instance
  Prelude.Hashable
    PerformanceInsightsMetricQuery
  where
  hashWithSalt
    _salt
    PerformanceInsightsMetricQuery' {..} =
      _salt
        `Prelude.hashWithSalt` filter'
        `Prelude.hashWithSalt` groupBy
        `Prelude.hashWithSalt` metric

instance
  Prelude.NFData
    PerformanceInsightsMetricQuery
  where
  rnf PerformanceInsightsMetricQuery' {..} =
    Prelude.rnf filter' `Prelude.seq`
      Prelude.rnf groupBy `Prelude.seq`
        Prelude.rnf metric
