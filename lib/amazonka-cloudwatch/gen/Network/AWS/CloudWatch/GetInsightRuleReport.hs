{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
--
--
module Network.AWS.CloudWatch.GetInsightRuleReport
    (
    -- * Creating a request
      GetInsightRuleReport (..)
    , mkGetInsightRuleReport
    -- ** Request lenses
    , girrRuleName
    , girrStartTime
    , girrEndTime
    , girrPeriod
    , girrMaxContributorCount
    , girrMetrics
    , girrOrderBy

    -- * Destructuring the response
    , GetInsightRuleReportResponse (..)
    , mkGetInsightRuleReportResponse
    -- ** Response lenses
    , girrrrsAggregateValue
    , girrrrsAggregationStatistic
    , girrrrsApproximateUniqueCount
    , girrrrsContributors
    , girrrrsKeyLabels
    , girrrrsMetricDatapoints
    , girrrrsResponseStatus
    ) where

import qualified Network.AWS.CloudWatch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetInsightRuleReport' smart constructor.
data GetInsightRuleReport = GetInsightRuleReport'
  { ruleName :: Types.RuleName
    -- ^ The name of the rule that you want to see data from.
  , startTime :: Core.UTCTime
    -- ^ The start time of the data to use in the report. When used in a raw HTTP Query API, it is formatted as @yyyy-MM-dd'T'HH:mm:ss@ . For example, @2019-07-01T23:59:59@ .
  , endTime :: Core.UTCTime
    -- ^ The end time of the data to use in the report. When used in a raw HTTP Query API, it is formatted as @yyyy-MM-dd'T'HH:mm:ss@ . For example, @2019-07-01T23:59:59@ .
  , period :: Core.Natural
    -- ^ The period, in seconds, to use for the statistics in the @InsightRuleMetricDatapoint@ results.
  , maxContributorCount :: Core.Maybe Core.Int
    -- ^ The maximum number of contributors to include in the report. The range is 1 to 100. If you omit this, the default of 10 is used.
  , metrics :: Core.Maybe [Types.InsightRuleMetricName]
    -- ^ Specifies which metrics to use for aggregation of contributor values for the report. You can specify one or more of the following metrics:
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
  , orderBy :: Core.Maybe Types.InsightRuleOrderBy
    -- ^ Determines what statistic to use to rank the contributors. Valid values are SUM and MAXIMUM.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetInsightRuleReport' value with any optional fields omitted.
mkGetInsightRuleReport
    :: Types.RuleName -- ^ 'ruleName'
    -> Core.UTCTime -- ^ 'startTime'
    -> Core.UTCTime -- ^ 'endTime'
    -> Core.Natural -- ^ 'period'
    -> GetInsightRuleReport
mkGetInsightRuleReport ruleName startTime endTime period
  = GetInsightRuleReport'{ruleName, startTime, endTime, period,
                          maxContributorCount = Core.Nothing, metrics = Core.Nothing,
                          orderBy = Core.Nothing}

-- | The name of the rule that you want to see data from.
--
-- /Note:/ Consider using 'ruleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrRuleName :: Lens.Lens' GetInsightRuleReport Types.RuleName
girrRuleName = Lens.field @"ruleName"
{-# INLINEABLE girrRuleName #-}
{-# DEPRECATED ruleName "Use generic-lens or generic-optics with 'ruleName' instead"  #-}

-- | The start time of the data to use in the report. When used in a raw HTTP Query API, it is formatted as @yyyy-MM-dd'T'HH:mm:ss@ . For example, @2019-07-01T23:59:59@ .
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrStartTime :: Lens.Lens' GetInsightRuleReport Core.UTCTime
girrStartTime = Lens.field @"startTime"
{-# INLINEABLE girrStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

-- | The end time of the data to use in the report. When used in a raw HTTP Query API, it is formatted as @yyyy-MM-dd'T'HH:mm:ss@ . For example, @2019-07-01T23:59:59@ .
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrEndTime :: Lens.Lens' GetInsightRuleReport Core.UTCTime
girrEndTime = Lens.field @"endTime"
{-# INLINEABLE girrEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | The period, in seconds, to use for the statistics in the @InsightRuleMetricDatapoint@ results.
--
-- /Note:/ Consider using 'period' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrPeriod :: Lens.Lens' GetInsightRuleReport Core.Natural
girrPeriod = Lens.field @"period"
{-# INLINEABLE girrPeriod #-}
{-# DEPRECATED period "Use generic-lens or generic-optics with 'period' instead"  #-}

-- | The maximum number of contributors to include in the report. The range is 1 to 100. If you omit this, the default of 10 is used.
--
-- /Note:/ Consider using 'maxContributorCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrMaxContributorCount :: Lens.Lens' GetInsightRuleReport (Core.Maybe Core.Int)
girrMaxContributorCount = Lens.field @"maxContributorCount"
{-# INLINEABLE girrMaxContributorCount #-}
{-# DEPRECATED maxContributorCount "Use generic-lens or generic-optics with 'maxContributorCount' instead"  #-}

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
girrMetrics :: Lens.Lens' GetInsightRuleReport (Core.Maybe [Types.InsightRuleMetricName])
girrMetrics = Lens.field @"metrics"
{-# INLINEABLE girrMetrics #-}
{-# DEPRECATED metrics "Use generic-lens or generic-optics with 'metrics' instead"  #-}

-- | Determines what statistic to use to rank the contributors. Valid values are SUM and MAXIMUM.
--
-- /Note:/ Consider using 'orderBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrOrderBy :: Lens.Lens' GetInsightRuleReport (Core.Maybe Types.InsightRuleOrderBy)
girrOrderBy = Lens.field @"orderBy"
{-# INLINEABLE girrOrderBy #-}
{-# DEPRECATED orderBy "Use generic-lens or generic-optics with 'orderBy' instead"  #-}

instance Core.ToQuery GetInsightRuleReport where
        toQuery GetInsightRuleReport{..}
          = Core.toQueryPair "Action" ("GetInsightRuleReport" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-08-01" :: Core.Text)
              Core.<> Core.toQueryPair "RuleName" ruleName
              Core.<> Core.toQueryPair "StartTime" startTime
              Core.<> Core.toQueryPair "EndTime" endTime
              Core.<> Core.toQueryPair "Period" period
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxContributorCount")
                maxContributorCount
              Core.<>
              Core.toQueryPair "Metrics"
                (Core.maybe Core.mempty (Core.toQueryList "member") metrics)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "OrderBy") orderBy

instance Core.ToHeaders GetInsightRuleReport where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetInsightRuleReport where
        type Rs GetInsightRuleReport = GetInsightRuleReportResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "GetInsightRuleReportResult"
              (\ s h x ->
                 GetInsightRuleReportResponse' Core.<$>
                   (x Core..@? "AggregateValue") Core.<*>
                     x Core..@? "AggregationStatistic"
                     Core.<*> x Core..@? "ApproximateUniqueCount"
                     Core.<*>
                     x Core..@? "Contributors" Core..<@> Core.parseXMLList "member"
                     Core.<*>
                     x Core..@? "KeyLabels" Core..<@> Core.parseXMLList "member"
                     Core.<*>
                     x Core..@? "MetricDatapoints" Core..<@> Core.parseXMLList "member"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetInsightRuleReportResponse' smart constructor.
data GetInsightRuleReportResponse = GetInsightRuleReportResponse'
  { aggregateValue :: Core.Maybe Core.Double
    -- ^ The sum of the values from all individual contributors that match the rule.
  , aggregationStatistic :: Core.Maybe Types.AggregationStatistic
    -- ^ Specifies whether this rule aggregates contributor data by COUNT or SUM.
  , approximateUniqueCount :: Core.Maybe Core.Integer
    -- ^ An approximate count of the unique contributors found by this rule in this time period.
  , contributors :: Core.Maybe [Types.InsightRuleContributor]
    -- ^ An array of the unique contributors found by this rule in this time period. If the rule contains multiple keys, each combination of values for the keys counts as a unique contributor.
  , keyLabels :: Core.Maybe [Types.InsightRuleContributorKeyLabel]
    -- ^ An array of the strings used as the keys for this rule. The keys are the dimensions used to classify contributors. If the rule contains more than one key, then each unique combination of values for the keys is counted as a unique contributor.
  , metricDatapoints :: Core.Maybe [Types.InsightRuleMetricDatapoint]
    -- ^ A time series of metric data points that matches the time period in the rule request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetInsightRuleReportResponse' value with any optional fields omitted.
mkGetInsightRuleReportResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetInsightRuleReportResponse
mkGetInsightRuleReportResponse responseStatus
  = GetInsightRuleReportResponse'{aggregateValue = Core.Nothing,
                                  aggregationStatistic = Core.Nothing,
                                  approximateUniqueCount = Core.Nothing,
                                  contributors = Core.Nothing, keyLabels = Core.Nothing,
                                  metricDatapoints = Core.Nothing, responseStatus}

-- | The sum of the values from all individual contributors that match the rule.
--
-- /Note:/ Consider using 'aggregateValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrrrsAggregateValue :: Lens.Lens' GetInsightRuleReportResponse (Core.Maybe Core.Double)
girrrrsAggregateValue = Lens.field @"aggregateValue"
{-# INLINEABLE girrrrsAggregateValue #-}
{-# DEPRECATED aggregateValue "Use generic-lens or generic-optics with 'aggregateValue' instead"  #-}

-- | Specifies whether this rule aggregates contributor data by COUNT or SUM.
--
-- /Note:/ Consider using 'aggregationStatistic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrrrsAggregationStatistic :: Lens.Lens' GetInsightRuleReportResponse (Core.Maybe Types.AggregationStatistic)
girrrrsAggregationStatistic = Lens.field @"aggregationStatistic"
{-# INLINEABLE girrrrsAggregationStatistic #-}
{-# DEPRECATED aggregationStatistic "Use generic-lens or generic-optics with 'aggregationStatistic' instead"  #-}

-- | An approximate count of the unique contributors found by this rule in this time period.
--
-- /Note:/ Consider using 'approximateUniqueCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrrrsApproximateUniqueCount :: Lens.Lens' GetInsightRuleReportResponse (Core.Maybe Core.Integer)
girrrrsApproximateUniqueCount = Lens.field @"approximateUniqueCount"
{-# INLINEABLE girrrrsApproximateUniqueCount #-}
{-# DEPRECATED approximateUniqueCount "Use generic-lens or generic-optics with 'approximateUniqueCount' instead"  #-}

-- | An array of the unique contributors found by this rule in this time period. If the rule contains multiple keys, each combination of values for the keys counts as a unique contributor.
--
-- /Note:/ Consider using 'contributors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrrrsContributors :: Lens.Lens' GetInsightRuleReportResponse (Core.Maybe [Types.InsightRuleContributor])
girrrrsContributors = Lens.field @"contributors"
{-# INLINEABLE girrrrsContributors #-}
{-# DEPRECATED contributors "Use generic-lens or generic-optics with 'contributors' instead"  #-}

-- | An array of the strings used as the keys for this rule. The keys are the dimensions used to classify contributors. If the rule contains more than one key, then each unique combination of values for the keys is counted as a unique contributor.
--
-- /Note:/ Consider using 'keyLabels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrrrsKeyLabels :: Lens.Lens' GetInsightRuleReportResponse (Core.Maybe [Types.InsightRuleContributorKeyLabel])
girrrrsKeyLabels = Lens.field @"keyLabels"
{-# INLINEABLE girrrrsKeyLabels #-}
{-# DEPRECATED keyLabels "Use generic-lens or generic-optics with 'keyLabels' instead"  #-}

-- | A time series of metric data points that matches the time period in the rule request.
--
-- /Note:/ Consider using 'metricDatapoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrrrsMetricDatapoints :: Lens.Lens' GetInsightRuleReportResponse (Core.Maybe [Types.InsightRuleMetricDatapoint])
girrrrsMetricDatapoints = Lens.field @"metricDatapoints"
{-# INLINEABLE girrrrsMetricDatapoints #-}
{-# DEPRECATED metricDatapoints "Use generic-lens or generic-optics with 'metricDatapoints' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrrrsResponseStatus :: Lens.Lens' GetInsightRuleReportResponse Core.Int
girrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE girrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
