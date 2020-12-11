{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.GetInsightRuleReport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns the time series data collected by a Contributor Insights rule. The data includes the identity and number of contributors to the log group.
--
-- You can also optionally return one or more statistics about each data point in the time series. These statistics can include the following:
--
--     * @UniqueContributors@ -- the number of unique contributors for each data point.
--
--
--     * @MaxContributorValue@ -- the value of the top contributor for each data point. The identity of the contributor might change for each data point in the graph.
-- If this rule aggregates by COUNT, the top contributor for each data point is the contributor with the most occurrences in that period. If the rule aggregates by SUM, the top contributor is the contributor with the highest sum in the log field specified by the rule's @Value@ , during that period.
--
--
--     * @SampleCount@ -- the number of data points matched by the rule.
--
--
--     * @Sum@ -- the sum of the values from all contributors during the time period represented by that data point.
--
--
--     * @Minimum@ -- the minimum value from a single observation during the time period represented by that data point.
--
--
--     * @Maximum@ -- the maximum value from a single observation during the time period represented by that data point.
--
--
--     * @Average@ -- the average value from all contributors during the time period represented by that data point.
module Network.AWS.CloudWatch.GetInsightRuleReport
  ( -- * Creating a request
    GetInsightRuleReport (..),
    mkGetInsightRuleReport,

    -- ** Request lenses
    girrMaxContributorCount,
    girrMetrics,
    girrOrderBy,
    girrRuleName,
    girrStartTime,
    girrEndTime,
    girrPeriod,

    -- * Destructuring the response
    GetInsightRuleReportResponse (..),
    mkGetInsightRuleReportResponse,

    -- ** Response lenses
    girrrsKeyLabels,
    girrrsApproximateUniqueCount,
    girrrsAggregationStatistic,
    girrrsAggregateValue,
    girrrsContributors,
    girrrsMetricDatapoints,
    girrrsResponseStatus,
  )
where

import Network.AWS.CloudWatch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetInsightRuleReport' smart constructor.
data GetInsightRuleReport = GetInsightRuleReport'
  { maxContributorCount ::
      Lude.Maybe Lude.Int,
    metrics :: Lude.Maybe [Lude.Text],
    orderBy :: Lude.Maybe Lude.Text,
    ruleName :: Lude.Text,
    startTime :: Lude.ISO8601,
    endTime :: Lude.ISO8601,
    period :: Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetInsightRuleReport' with the minimum fields required to make a request.
--
-- * 'endTime' - The end time of the data to use in the report. When used in a raw HTTP Query API, it is formatted as @yyyy-MM-dd'T'HH:mm:ss@ . For example, @2019-07-01T23:59:59@ .
-- * 'maxContributorCount' - The maximum number of contributors to include in the report. The range is 1 to 100. If you omit this, the default of 10 is used.
-- * 'metrics' - Specifies which metrics to use for aggregation of contributor values for the report. You can specify one or more of the following metrics:
--
--
--     * @UniqueContributors@ -- the number of unique contributors for each data point.
--
--
--     * @MaxContributorValue@ -- the value of the top contributor for each data point. The identity of the contributor might change for each data point in the graph.
-- If this rule aggregates by COUNT, the top contributor for each data point is the contributor with the most occurrences in that period. If the rule aggregates by SUM, the top contributor is the contributor with the highest sum in the log field specified by the rule's @Value@ , during that period.
--
--
--     * @SampleCount@ -- the number of data points matched by the rule.
--
--
--     * @Sum@ -- the sum of the values from all contributors during the time period represented by that data point.
--
--
--     * @Minimum@ -- the minimum value from a single observation during the time period represented by that data point.
--
--
--     * @Maximum@ -- the maximum value from a single observation during the time period represented by that data point.
--
--
--     * @Average@ -- the average value from all contributors during the time period represented by that data point.
--
--
-- * 'orderBy' - Determines what statistic to use to rank the contributors. Valid values are SUM and MAXIMUM.
-- * 'period' - The period, in seconds, to use for the statistics in the @InsightRuleMetricDatapoint@ results.
-- * 'ruleName' - The name of the rule that you want to see data from.
-- * 'startTime' - The start time of the data to use in the report. When used in a raw HTTP Query API, it is formatted as @yyyy-MM-dd'T'HH:mm:ss@ . For example, @2019-07-01T23:59:59@ .
mkGetInsightRuleReport ::
  -- | 'ruleName'
  Lude.Text ->
  -- | 'startTime'
  Lude.ISO8601 ->
  -- | 'endTime'
  Lude.ISO8601 ->
  -- | 'period'
  Lude.Natural ->
  GetInsightRuleReport
mkGetInsightRuleReport pRuleName_ pStartTime_ pEndTime_ pPeriod_ =
  GetInsightRuleReport'
    { maxContributorCount = Lude.Nothing,
      metrics = Lude.Nothing,
      orderBy = Lude.Nothing,
      ruleName = pRuleName_,
      startTime = pStartTime_,
      endTime = pEndTime_,
      period = pPeriod_
    }

-- | The maximum number of contributors to include in the report. The range is 1 to 100. If you omit this, the default of 10 is used.
--
-- /Note:/ Consider using 'maxContributorCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrMaxContributorCount :: Lens.Lens' GetInsightRuleReport (Lude.Maybe Lude.Int)
girrMaxContributorCount = Lens.lens (maxContributorCount :: GetInsightRuleReport -> Lude.Maybe Lude.Int) (\s a -> s {maxContributorCount = a} :: GetInsightRuleReport)
{-# DEPRECATED girrMaxContributorCount "Use generic-lens or generic-optics with 'maxContributorCount' instead." #-}

-- | Specifies which metrics to use for aggregation of contributor values for the report. You can specify one or more of the following metrics:
--
--
--     * @UniqueContributors@ -- the number of unique contributors for each data point.
--
--
--     * @MaxContributorValue@ -- the value of the top contributor for each data point. The identity of the contributor might change for each data point in the graph.
-- If this rule aggregates by COUNT, the top contributor for each data point is the contributor with the most occurrences in that period. If the rule aggregates by SUM, the top contributor is the contributor with the highest sum in the log field specified by the rule's @Value@ , during that period.
--
--
--     * @SampleCount@ -- the number of data points matched by the rule.
--
--
--     * @Sum@ -- the sum of the values from all contributors during the time period represented by that data point.
--
--
--     * @Minimum@ -- the minimum value from a single observation during the time period represented by that data point.
--
--
--     * @Maximum@ -- the maximum value from a single observation during the time period represented by that data point.
--
--
--     * @Average@ -- the average value from all contributors during the time period represented by that data point.
--
--
--
-- /Note:/ Consider using 'metrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrMetrics :: Lens.Lens' GetInsightRuleReport (Lude.Maybe [Lude.Text])
girrMetrics = Lens.lens (metrics :: GetInsightRuleReport -> Lude.Maybe [Lude.Text]) (\s a -> s {metrics = a} :: GetInsightRuleReport)
{-# DEPRECATED girrMetrics "Use generic-lens or generic-optics with 'metrics' instead." #-}

-- | Determines what statistic to use to rank the contributors. Valid values are SUM and MAXIMUM.
--
-- /Note:/ Consider using 'orderBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrOrderBy :: Lens.Lens' GetInsightRuleReport (Lude.Maybe Lude.Text)
girrOrderBy = Lens.lens (orderBy :: GetInsightRuleReport -> Lude.Maybe Lude.Text) (\s a -> s {orderBy = a} :: GetInsightRuleReport)
{-# DEPRECATED girrOrderBy "Use generic-lens or generic-optics with 'orderBy' instead." #-}

-- | The name of the rule that you want to see data from.
--
-- /Note:/ Consider using 'ruleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrRuleName :: Lens.Lens' GetInsightRuleReport Lude.Text
girrRuleName = Lens.lens (ruleName :: GetInsightRuleReport -> Lude.Text) (\s a -> s {ruleName = a} :: GetInsightRuleReport)
{-# DEPRECATED girrRuleName "Use generic-lens or generic-optics with 'ruleName' instead." #-}

-- | The start time of the data to use in the report. When used in a raw HTTP Query API, it is formatted as @yyyy-MM-dd'T'HH:mm:ss@ . For example, @2019-07-01T23:59:59@ .
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrStartTime :: Lens.Lens' GetInsightRuleReport Lude.ISO8601
girrStartTime = Lens.lens (startTime :: GetInsightRuleReport -> Lude.ISO8601) (\s a -> s {startTime = a} :: GetInsightRuleReport)
{-# DEPRECATED girrStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The end time of the data to use in the report. When used in a raw HTTP Query API, it is formatted as @yyyy-MM-dd'T'HH:mm:ss@ . For example, @2019-07-01T23:59:59@ .
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrEndTime :: Lens.Lens' GetInsightRuleReport Lude.ISO8601
girrEndTime = Lens.lens (endTime :: GetInsightRuleReport -> Lude.ISO8601) (\s a -> s {endTime = a} :: GetInsightRuleReport)
{-# DEPRECATED girrEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The period, in seconds, to use for the statistics in the @InsightRuleMetricDatapoint@ results.
--
-- /Note:/ Consider using 'period' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrPeriod :: Lens.Lens' GetInsightRuleReport Lude.Natural
girrPeriod = Lens.lens (period :: GetInsightRuleReport -> Lude.Natural) (\s a -> s {period = a} :: GetInsightRuleReport)
{-# DEPRECATED girrPeriod "Use generic-lens or generic-optics with 'period' instead." #-}

instance Lude.AWSRequest GetInsightRuleReport where
  type Rs GetInsightRuleReport = GetInsightRuleReportResponse
  request = Req.postQuery cloudWatchService
  response =
    Res.receiveXMLWrapper
      "GetInsightRuleReportResult"
      ( \s h x ->
          GetInsightRuleReportResponse'
            Lude.<$> ( x Lude..@? "KeyLabels" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "ApproximateUniqueCount")
            Lude.<*> (x Lude..@? "AggregationStatistic")
            Lude.<*> (x Lude..@? "AggregateValue")
            Lude.<*> ( x Lude..@? "Contributors" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> ( x Lude..@? "MetricDatapoints" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetInsightRuleReport where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetInsightRuleReport where
  toPath = Lude.const "/"

instance Lude.ToQuery GetInsightRuleReport where
  toQuery GetInsightRuleReport' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("GetInsightRuleReport" :: Lude.ByteString),
        "Version" Lude.=: ("2010-08-01" :: Lude.ByteString),
        "MaxContributorCount" Lude.=: maxContributorCount,
        "Metrics"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> metrics),
        "OrderBy" Lude.=: orderBy,
        "RuleName" Lude.=: ruleName,
        "StartTime" Lude.=: startTime,
        "EndTime" Lude.=: endTime,
        "Period" Lude.=: period
      ]

-- | /See:/ 'mkGetInsightRuleReportResponse' smart constructor.
data GetInsightRuleReportResponse = GetInsightRuleReportResponse'
  { keyLabels ::
      Lude.Maybe [Lude.Text],
    approximateUniqueCount ::
      Lude.Maybe Lude.Integer,
    aggregationStatistic ::
      Lude.Maybe Lude.Text,
    aggregateValue ::
      Lude.Maybe Lude.Double,
    contributors ::
      Lude.Maybe
        [InsightRuleContributor],
    metricDatapoints ::
      Lude.Maybe
        [InsightRuleMetricDatapoint],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetInsightRuleReportResponse' with the minimum fields required to make a request.
--
-- * 'aggregateValue' - The sum of the values from all individual contributors that match the rule.
-- * 'aggregationStatistic' - Specifies whether this rule aggregates contributor data by COUNT or SUM.
-- * 'approximateUniqueCount' - An approximate count of the unique contributors found by this rule in this time period.
-- * 'contributors' - An array of the unique contributors found by this rule in this time period. If the rule contains multiple keys, each combination of values for the keys counts as a unique contributor.
-- * 'keyLabels' - An array of the strings used as the keys for this rule. The keys are the dimensions used to classify contributors. If the rule contains more than one key, then each unique combination of values for the keys is counted as a unique contributor.
-- * 'metricDatapoints' - A time series of metric data points that matches the time period in the rule request.
-- * 'responseStatus' - The response status code.
mkGetInsightRuleReportResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetInsightRuleReportResponse
mkGetInsightRuleReportResponse pResponseStatus_ =
  GetInsightRuleReportResponse'
    { keyLabels = Lude.Nothing,
      approximateUniqueCount = Lude.Nothing,
      aggregationStatistic = Lude.Nothing,
      aggregateValue = Lude.Nothing,
      contributors = Lude.Nothing,
      metricDatapoints = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of the strings used as the keys for this rule. The keys are the dimensions used to classify contributors. If the rule contains more than one key, then each unique combination of values for the keys is counted as a unique contributor.
--
-- /Note:/ Consider using 'keyLabels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrrsKeyLabels :: Lens.Lens' GetInsightRuleReportResponse (Lude.Maybe [Lude.Text])
girrrsKeyLabels = Lens.lens (keyLabels :: GetInsightRuleReportResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {keyLabels = a} :: GetInsightRuleReportResponse)
{-# DEPRECATED girrrsKeyLabels "Use generic-lens or generic-optics with 'keyLabels' instead." #-}

-- | An approximate count of the unique contributors found by this rule in this time period.
--
-- /Note:/ Consider using 'approximateUniqueCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrrsApproximateUniqueCount :: Lens.Lens' GetInsightRuleReportResponse (Lude.Maybe Lude.Integer)
girrrsApproximateUniqueCount = Lens.lens (approximateUniqueCount :: GetInsightRuleReportResponse -> Lude.Maybe Lude.Integer) (\s a -> s {approximateUniqueCount = a} :: GetInsightRuleReportResponse)
{-# DEPRECATED girrrsApproximateUniqueCount "Use generic-lens or generic-optics with 'approximateUniqueCount' instead." #-}

-- | Specifies whether this rule aggregates contributor data by COUNT or SUM.
--
-- /Note:/ Consider using 'aggregationStatistic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrrsAggregationStatistic :: Lens.Lens' GetInsightRuleReportResponse (Lude.Maybe Lude.Text)
girrrsAggregationStatistic = Lens.lens (aggregationStatistic :: GetInsightRuleReportResponse -> Lude.Maybe Lude.Text) (\s a -> s {aggregationStatistic = a} :: GetInsightRuleReportResponse)
{-# DEPRECATED girrrsAggregationStatistic "Use generic-lens or generic-optics with 'aggregationStatistic' instead." #-}

-- | The sum of the values from all individual contributors that match the rule.
--
-- /Note:/ Consider using 'aggregateValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrrsAggregateValue :: Lens.Lens' GetInsightRuleReportResponse (Lude.Maybe Lude.Double)
girrrsAggregateValue = Lens.lens (aggregateValue :: GetInsightRuleReportResponse -> Lude.Maybe Lude.Double) (\s a -> s {aggregateValue = a} :: GetInsightRuleReportResponse)
{-# DEPRECATED girrrsAggregateValue "Use generic-lens or generic-optics with 'aggregateValue' instead." #-}

-- | An array of the unique contributors found by this rule in this time period. If the rule contains multiple keys, each combination of values for the keys counts as a unique contributor.
--
-- /Note:/ Consider using 'contributors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrrsContributors :: Lens.Lens' GetInsightRuleReportResponse (Lude.Maybe [InsightRuleContributor])
girrrsContributors = Lens.lens (contributors :: GetInsightRuleReportResponse -> Lude.Maybe [InsightRuleContributor]) (\s a -> s {contributors = a} :: GetInsightRuleReportResponse)
{-# DEPRECATED girrrsContributors "Use generic-lens or generic-optics with 'contributors' instead." #-}

-- | A time series of metric data points that matches the time period in the rule request.
--
-- /Note:/ Consider using 'metricDatapoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrrsMetricDatapoints :: Lens.Lens' GetInsightRuleReportResponse (Lude.Maybe [InsightRuleMetricDatapoint])
girrrsMetricDatapoints = Lens.lens (metricDatapoints :: GetInsightRuleReportResponse -> Lude.Maybe [InsightRuleMetricDatapoint]) (\s a -> s {metricDatapoints = a} :: GetInsightRuleReportResponse)
{-# DEPRECATED girrrsMetricDatapoints "Use generic-lens or generic-optics with 'metricDatapoints' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrrsResponseStatus :: Lens.Lens' GetInsightRuleReportResponse Lude.Int
girrrsResponseStatus = Lens.lens (responseStatus :: GetInsightRuleReportResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetInsightRuleReportResponse)
{-# DEPRECATED girrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
