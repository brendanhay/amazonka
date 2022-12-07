{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CloudWatch.GetInsightRuleReport
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns the time series data collected by a Contributor
-- Insights rule. The data includes the identity and number of contributors
-- to the log group.
--
-- You can also optionally return one or more statistics about each data
-- point in the time series. These statistics can include the following:
--
-- -   @UniqueContributors@ -- the number of unique contributors for each
--     data point.
--
-- -   @MaxContributorValue@ -- the value of the top contributor for each
--     data point. The identity of the contributor might change for each
--     data point in the graph.
--
--     If this rule aggregates by COUNT, the top contributor for each data
--     point is the contributor with the most occurrences in that period.
--     If the rule aggregates by SUM, the top contributor is the
--     contributor with the highest sum in the log field specified by the
--     rule\'s @Value@, during that period.
--
-- -   @SampleCount@ -- the number of data points matched by the rule.
--
-- -   @Sum@ -- the sum of the values from all contributors during the time
--     period represented by that data point.
--
-- -   @Minimum@ -- the minimum value from a single observation during the
--     time period represented by that data point.
--
-- -   @Maximum@ -- the maximum value from a single observation during the
--     time period represented by that data point.
--
-- -   @Average@ -- the average value from all contributors during the time
--     period represented by that data point.
module Amazonka.CloudWatch.GetInsightRuleReport
  ( -- * Creating a Request
    GetInsightRuleReport (..),
    newGetInsightRuleReport,

    -- * Request Lenses
    getInsightRuleReport_metrics,
    getInsightRuleReport_maxContributorCount,
    getInsightRuleReport_orderBy,
    getInsightRuleReport_ruleName,
    getInsightRuleReport_startTime,
    getInsightRuleReport_endTime,
    getInsightRuleReport_period,

    -- * Destructuring the Response
    GetInsightRuleReportResponse (..),
    newGetInsightRuleReportResponse,

    -- * Response Lenses
    getInsightRuleReportResponse_aggregationStatistic,
    getInsightRuleReportResponse_metricDatapoints,
    getInsightRuleReportResponse_aggregateValue,
    getInsightRuleReportResponse_keyLabels,
    getInsightRuleReportResponse_approximateUniqueCount,
    getInsightRuleReportResponse_contributors,
    getInsightRuleReportResponse_httpStatus,
  )
where

import Amazonka.CloudWatch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetInsightRuleReport' smart constructor.
data GetInsightRuleReport = GetInsightRuleReport'
  { -- | Specifies which metrics to use for aggregation of contributor values for
    -- the report. You can specify one or more of the following metrics:
    --
    -- -   @UniqueContributors@ -- the number of unique contributors for each
    --     data point.
    --
    -- -   @MaxContributorValue@ -- the value of the top contributor for each
    --     data point. The identity of the contributor might change for each
    --     data point in the graph.
    --
    --     If this rule aggregates by COUNT, the top contributor for each data
    --     point is the contributor with the most occurrences in that period.
    --     If the rule aggregates by SUM, the top contributor is the
    --     contributor with the highest sum in the log field specified by the
    --     rule\'s @Value@, during that period.
    --
    -- -   @SampleCount@ -- the number of data points matched by the rule.
    --
    -- -   @Sum@ -- the sum of the values from all contributors during the time
    --     period represented by that data point.
    --
    -- -   @Minimum@ -- the minimum value from a single observation during the
    --     time period represented by that data point.
    --
    -- -   @Maximum@ -- the maximum value from a single observation during the
    --     time period represented by that data point.
    --
    -- -   @Average@ -- the average value from all contributors during the time
    --     period represented by that data point.
    metrics :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of contributors to include in the report. The range
    -- is 1 to 100. If you omit this, the default of 10 is used.
    maxContributorCount :: Prelude.Maybe Prelude.Int,
    -- | Determines what statistic to use to rank the contributors. Valid values
    -- are SUM and MAXIMUM.
    orderBy :: Prelude.Maybe Prelude.Text,
    -- | The name of the rule that you want to see data from.
    ruleName :: Prelude.Text,
    -- | The start time of the data to use in the report. When used in a raw HTTP
    -- Query API, it is formatted as @yyyy-MM-dd\'T\'HH:mm:ss@. For example,
    -- @2019-07-01T23:59:59@.
    startTime :: Data.ISO8601,
    -- | The end time of the data to use in the report. When used in a raw HTTP
    -- Query API, it is formatted as @yyyy-MM-dd\'T\'HH:mm:ss@. For example,
    -- @2019-07-01T23:59:59@.
    endTime :: Data.ISO8601,
    -- | The period, in seconds, to use for the statistics in the
    -- @InsightRuleMetricDatapoint@ results.
    period :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInsightRuleReport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metrics', 'getInsightRuleReport_metrics' - Specifies which metrics to use for aggregation of contributor values for
-- the report. You can specify one or more of the following metrics:
--
-- -   @UniqueContributors@ -- the number of unique contributors for each
--     data point.
--
-- -   @MaxContributorValue@ -- the value of the top contributor for each
--     data point. The identity of the contributor might change for each
--     data point in the graph.
--
--     If this rule aggregates by COUNT, the top contributor for each data
--     point is the contributor with the most occurrences in that period.
--     If the rule aggregates by SUM, the top contributor is the
--     contributor with the highest sum in the log field specified by the
--     rule\'s @Value@, during that period.
--
-- -   @SampleCount@ -- the number of data points matched by the rule.
--
-- -   @Sum@ -- the sum of the values from all contributors during the time
--     period represented by that data point.
--
-- -   @Minimum@ -- the minimum value from a single observation during the
--     time period represented by that data point.
--
-- -   @Maximum@ -- the maximum value from a single observation during the
--     time period represented by that data point.
--
-- -   @Average@ -- the average value from all contributors during the time
--     period represented by that data point.
--
-- 'maxContributorCount', 'getInsightRuleReport_maxContributorCount' - The maximum number of contributors to include in the report. The range
-- is 1 to 100. If you omit this, the default of 10 is used.
--
-- 'orderBy', 'getInsightRuleReport_orderBy' - Determines what statistic to use to rank the contributors. Valid values
-- are SUM and MAXIMUM.
--
-- 'ruleName', 'getInsightRuleReport_ruleName' - The name of the rule that you want to see data from.
--
-- 'startTime', 'getInsightRuleReport_startTime' - The start time of the data to use in the report. When used in a raw HTTP
-- Query API, it is formatted as @yyyy-MM-dd\'T\'HH:mm:ss@. For example,
-- @2019-07-01T23:59:59@.
--
-- 'endTime', 'getInsightRuleReport_endTime' - The end time of the data to use in the report. When used in a raw HTTP
-- Query API, it is formatted as @yyyy-MM-dd\'T\'HH:mm:ss@. For example,
-- @2019-07-01T23:59:59@.
--
-- 'period', 'getInsightRuleReport_period' - The period, in seconds, to use for the statistics in the
-- @InsightRuleMetricDatapoint@ results.
newGetInsightRuleReport ::
  -- | 'ruleName'
  Prelude.Text ->
  -- | 'startTime'
  Prelude.UTCTime ->
  -- | 'endTime'
  Prelude.UTCTime ->
  -- | 'period'
  Prelude.Natural ->
  GetInsightRuleReport
newGetInsightRuleReport
  pRuleName_
  pStartTime_
  pEndTime_
  pPeriod_ =
    GetInsightRuleReport'
      { metrics = Prelude.Nothing,
        maxContributorCount = Prelude.Nothing,
        orderBy = Prelude.Nothing,
        ruleName = pRuleName_,
        startTime = Data._Time Lens.# pStartTime_,
        endTime = Data._Time Lens.# pEndTime_,
        period = pPeriod_
      }

-- | Specifies which metrics to use for aggregation of contributor values for
-- the report. You can specify one or more of the following metrics:
--
-- -   @UniqueContributors@ -- the number of unique contributors for each
--     data point.
--
-- -   @MaxContributorValue@ -- the value of the top contributor for each
--     data point. The identity of the contributor might change for each
--     data point in the graph.
--
--     If this rule aggregates by COUNT, the top contributor for each data
--     point is the contributor with the most occurrences in that period.
--     If the rule aggregates by SUM, the top contributor is the
--     contributor with the highest sum in the log field specified by the
--     rule\'s @Value@, during that period.
--
-- -   @SampleCount@ -- the number of data points matched by the rule.
--
-- -   @Sum@ -- the sum of the values from all contributors during the time
--     period represented by that data point.
--
-- -   @Minimum@ -- the minimum value from a single observation during the
--     time period represented by that data point.
--
-- -   @Maximum@ -- the maximum value from a single observation during the
--     time period represented by that data point.
--
-- -   @Average@ -- the average value from all contributors during the time
--     period represented by that data point.
getInsightRuleReport_metrics :: Lens.Lens' GetInsightRuleReport (Prelude.Maybe [Prelude.Text])
getInsightRuleReport_metrics = Lens.lens (\GetInsightRuleReport' {metrics} -> metrics) (\s@GetInsightRuleReport' {} a -> s {metrics = a} :: GetInsightRuleReport) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of contributors to include in the report. The range
-- is 1 to 100. If you omit this, the default of 10 is used.
getInsightRuleReport_maxContributorCount :: Lens.Lens' GetInsightRuleReport (Prelude.Maybe Prelude.Int)
getInsightRuleReport_maxContributorCount = Lens.lens (\GetInsightRuleReport' {maxContributorCount} -> maxContributorCount) (\s@GetInsightRuleReport' {} a -> s {maxContributorCount = a} :: GetInsightRuleReport)

-- | Determines what statistic to use to rank the contributors. Valid values
-- are SUM and MAXIMUM.
getInsightRuleReport_orderBy :: Lens.Lens' GetInsightRuleReport (Prelude.Maybe Prelude.Text)
getInsightRuleReport_orderBy = Lens.lens (\GetInsightRuleReport' {orderBy} -> orderBy) (\s@GetInsightRuleReport' {} a -> s {orderBy = a} :: GetInsightRuleReport)

-- | The name of the rule that you want to see data from.
getInsightRuleReport_ruleName :: Lens.Lens' GetInsightRuleReport Prelude.Text
getInsightRuleReport_ruleName = Lens.lens (\GetInsightRuleReport' {ruleName} -> ruleName) (\s@GetInsightRuleReport' {} a -> s {ruleName = a} :: GetInsightRuleReport)

-- | The start time of the data to use in the report. When used in a raw HTTP
-- Query API, it is formatted as @yyyy-MM-dd\'T\'HH:mm:ss@. For example,
-- @2019-07-01T23:59:59@.
getInsightRuleReport_startTime :: Lens.Lens' GetInsightRuleReport Prelude.UTCTime
getInsightRuleReport_startTime = Lens.lens (\GetInsightRuleReport' {startTime} -> startTime) (\s@GetInsightRuleReport' {} a -> s {startTime = a} :: GetInsightRuleReport) Prelude.. Data._Time

-- | The end time of the data to use in the report. When used in a raw HTTP
-- Query API, it is formatted as @yyyy-MM-dd\'T\'HH:mm:ss@. For example,
-- @2019-07-01T23:59:59@.
getInsightRuleReport_endTime :: Lens.Lens' GetInsightRuleReport Prelude.UTCTime
getInsightRuleReport_endTime = Lens.lens (\GetInsightRuleReport' {endTime} -> endTime) (\s@GetInsightRuleReport' {} a -> s {endTime = a} :: GetInsightRuleReport) Prelude.. Data._Time

-- | The period, in seconds, to use for the statistics in the
-- @InsightRuleMetricDatapoint@ results.
getInsightRuleReport_period :: Lens.Lens' GetInsightRuleReport Prelude.Natural
getInsightRuleReport_period = Lens.lens (\GetInsightRuleReport' {period} -> period) (\s@GetInsightRuleReport' {} a -> s {period = a} :: GetInsightRuleReport)

instance Core.AWSRequest GetInsightRuleReport where
  type
    AWSResponse GetInsightRuleReport =
      GetInsightRuleReportResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "GetInsightRuleReportResult"
      ( \s h x ->
          GetInsightRuleReportResponse'
            Prelude.<$> (x Data..@? "AggregationStatistic")
            Prelude.<*> ( x Data..@? "MetricDatapoints"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (x Data..@? "AggregateValue")
            Prelude.<*> ( x Data..@? "KeyLabels" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (x Data..@? "ApproximateUniqueCount")
            Prelude.<*> ( x Data..@? "Contributors" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetInsightRuleReport where
  hashWithSalt _salt GetInsightRuleReport' {..} =
    _salt `Prelude.hashWithSalt` metrics
      `Prelude.hashWithSalt` maxContributorCount
      `Prelude.hashWithSalt` orderBy
      `Prelude.hashWithSalt` ruleName
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` period

instance Prelude.NFData GetInsightRuleReport where
  rnf GetInsightRuleReport' {..} =
    Prelude.rnf metrics
      `Prelude.seq` Prelude.rnf maxContributorCount
      `Prelude.seq` Prelude.rnf orderBy
      `Prelude.seq` Prelude.rnf ruleName
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf period

instance Data.ToHeaders GetInsightRuleReport where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetInsightRuleReport where
  toPath = Prelude.const "/"

instance Data.ToQuery GetInsightRuleReport where
  toQuery GetInsightRuleReport' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("GetInsightRuleReport" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-08-01" :: Prelude.ByteString),
        "Metrics"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> metrics),
        "MaxContributorCount" Data.=: maxContributorCount,
        "OrderBy" Data.=: orderBy,
        "RuleName" Data.=: ruleName,
        "StartTime" Data.=: startTime,
        "EndTime" Data.=: endTime,
        "Period" Data.=: period
      ]

-- | /See:/ 'newGetInsightRuleReportResponse' smart constructor.
data GetInsightRuleReportResponse = GetInsightRuleReportResponse'
  { -- | Specifies whether this rule aggregates contributor data by COUNT or SUM.
    aggregationStatistic :: Prelude.Maybe Prelude.Text,
    -- | A time series of metric data points that matches the time period in the
    -- rule request.
    metricDatapoints :: Prelude.Maybe [InsightRuleMetricDatapoint],
    -- | The sum of the values from all individual contributors that match the
    -- rule.
    aggregateValue :: Prelude.Maybe Prelude.Double,
    -- | An array of the strings used as the keys for this rule. The keys are the
    -- dimensions used to classify contributors. If the rule contains more than
    -- one key, then each unique combination of values for the keys is counted
    -- as a unique contributor.
    keyLabels :: Prelude.Maybe [Prelude.Text],
    -- | An approximate count of the unique contributors found by this rule in
    -- this time period.
    approximateUniqueCount :: Prelude.Maybe Prelude.Integer,
    -- | An array of the unique contributors found by this rule in this time
    -- period. If the rule contains multiple keys, each combination of values
    -- for the keys counts as a unique contributor.
    contributors :: Prelude.Maybe [InsightRuleContributor],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInsightRuleReportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aggregationStatistic', 'getInsightRuleReportResponse_aggregationStatistic' - Specifies whether this rule aggregates contributor data by COUNT or SUM.
--
-- 'metricDatapoints', 'getInsightRuleReportResponse_metricDatapoints' - A time series of metric data points that matches the time period in the
-- rule request.
--
-- 'aggregateValue', 'getInsightRuleReportResponse_aggregateValue' - The sum of the values from all individual contributors that match the
-- rule.
--
-- 'keyLabels', 'getInsightRuleReportResponse_keyLabels' - An array of the strings used as the keys for this rule. The keys are the
-- dimensions used to classify contributors. If the rule contains more than
-- one key, then each unique combination of values for the keys is counted
-- as a unique contributor.
--
-- 'approximateUniqueCount', 'getInsightRuleReportResponse_approximateUniqueCount' - An approximate count of the unique contributors found by this rule in
-- this time period.
--
-- 'contributors', 'getInsightRuleReportResponse_contributors' - An array of the unique contributors found by this rule in this time
-- period. If the rule contains multiple keys, each combination of values
-- for the keys counts as a unique contributor.
--
-- 'httpStatus', 'getInsightRuleReportResponse_httpStatus' - The response's http status code.
newGetInsightRuleReportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetInsightRuleReportResponse
newGetInsightRuleReportResponse pHttpStatus_ =
  GetInsightRuleReportResponse'
    { aggregationStatistic =
        Prelude.Nothing,
      metricDatapoints = Prelude.Nothing,
      aggregateValue = Prelude.Nothing,
      keyLabels = Prelude.Nothing,
      approximateUniqueCount = Prelude.Nothing,
      contributors = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Specifies whether this rule aggregates contributor data by COUNT or SUM.
getInsightRuleReportResponse_aggregationStatistic :: Lens.Lens' GetInsightRuleReportResponse (Prelude.Maybe Prelude.Text)
getInsightRuleReportResponse_aggregationStatistic = Lens.lens (\GetInsightRuleReportResponse' {aggregationStatistic} -> aggregationStatistic) (\s@GetInsightRuleReportResponse' {} a -> s {aggregationStatistic = a} :: GetInsightRuleReportResponse)

-- | A time series of metric data points that matches the time period in the
-- rule request.
getInsightRuleReportResponse_metricDatapoints :: Lens.Lens' GetInsightRuleReportResponse (Prelude.Maybe [InsightRuleMetricDatapoint])
getInsightRuleReportResponse_metricDatapoints = Lens.lens (\GetInsightRuleReportResponse' {metricDatapoints} -> metricDatapoints) (\s@GetInsightRuleReportResponse' {} a -> s {metricDatapoints = a} :: GetInsightRuleReportResponse) Prelude.. Lens.mapping Lens.coerced

-- | The sum of the values from all individual contributors that match the
-- rule.
getInsightRuleReportResponse_aggregateValue :: Lens.Lens' GetInsightRuleReportResponse (Prelude.Maybe Prelude.Double)
getInsightRuleReportResponse_aggregateValue = Lens.lens (\GetInsightRuleReportResponse' {aggregateValue} -> aggregateValue) (\s@GetInsightRuleReportResponse' {} a -> s {aggregateValue = a} :: GetInsightRuleReportResponse)

-- | An array of the strings used as the keys for this rule. The keys are the
-- dimensions used to classify contributors. If the rule contains more than
-- one key, then each unique combination of values for the keys is counted
-- as a unique contributor.
getInsightRuleReportResponse_keyLabels :: Lens.Lens' GetInsightRuleReportResponse (Prelude.Maybe [Prelude.Text])
getInsightRuleReportResponse_keyLabels = Lens.lens (\GetInsightRuleReportResponse' {keyLabels} -> keyLabels) (\s@GetInsightRuleReportResponse' {} a -> s {keyLabels = a} :: GetInsightRuleReportResponse) Prelude.. Lens.mapping Lens.coerced

-- | An approximate count of the unique contributors found by this rule in
-- this time period.
getInsightRuleReportResponse_approximateUniqueCount :: Lens.Lens' GetInsightRuleReportResponse (Prelude.Maybe Prelude.Integer)
getInsightRuleReportResponse_approximateUniqueCount = Lens.lens (\GetInsightRuleReportResponse' {approximateUniqueCount} -> approximateUniqueCount) (\s@GetInsightRuleReportResponse' {} a -> s {approximateUniqueCount = a} :: GetInsightRuleReportResponse)

-- | An array of the unique contributors found by this rule in this time
-- period. If the rule contains multiple keys, each combination of values
-- for the keys counts as a unique contributor.
getInsightRuleReportResponse_contributors :: Lens.Lens' GetInsightRuleReportResponse (Prelude.Maybe [InsightRuleContributor])
getInsightRuleReportResponse_contributors = Lens.lens (\GetInsightRuleReportResponse' {contributors} -> contributors) (\s@GetInsightRuleReportResponse' {} a -> s {contributors = a} :: GetInsightRuleReportResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getInsightRuleReportResponse_httpStatus :: Lens.Lens' GetInsightRuleReportResponse Prelude.Int
getInsightRuleReportResponse_httpStatus = Lens.lens (\GetInsightRuleReportResponse' {httpStatus} -> httpStatus) (\s@GetInsightRuleReportResponse' {} a -> s {httpStatus = a} :: GetInsightRuleReportResponse)

instance Prelude.NFData GetInsightRuleReportResponse where
  rnf GetInsightRuleReportResponse' {..} =
    Prelude.rnf aggregationStatistic
      `Prelude.seq` Prelude.rnf metricDatapoints
      `Prelude.seq` Prelude.rnf aggregateValue
      `Prelude.seq` Prelude.rnf keyLabels
      `Prelude.seq` Prelude.rnf approximateUniqueCount
      `Prelude.seq` Prelude.rnf contributors
      `Prelude.seq` Prelude.rnf httpStatus
