{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.GetMetricData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets historical metric data from the specified Amazon Connect instance.
--
-- For a description of each historical metric, see <https://docs.aws.amazon.com/connect/latest/adminguide/historical-metrics-definitions.html Historical Metrics Definitions> in the /Amazon Connect Administrator Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.Connect.GetMetricData
  ( -- * Creating a request
    GetMetricData (..),
    mkGetMetricData,

    -- ** Request lenses
    gmdInstanceId,
    gmdStartTime,
    gmdEndTime,
    gmdFilters,
    gmdHistoricalMetrics,
    gmdGroupings,
    gmdMaxResults,
    gmdNextToken,

    -- * Destructuring the response
    GetMetricDataResponse (..),
    mkGetMetricDataResponse,

    -- ** Response lenses
    gmdrrsMetricResults,
    gmdrrsNextToken,
    gmdrrsResponseStatus,
  )
where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetMetricData' smart constructor.
data GetMetricData = GetMetricData'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Types.InstanceId,
    -- | The timestamp, in UNIX Epoch time format, at which to start the reporting interval for the retrieval of historical metrics data. The time must be specified using a multiple of 5 minutes, such as 10:05, 10:10, 10:15.
    --
    -- The start time cannot be earlier than 24 hours before the time of the request. Historical metrics are available only for 24 hours.
    startTime :: Core.NominalDiffTime,
    -- | The timestamp, in UNIX Epoch time format, at which to end the reporting interval for the retrieval of historical metrics data. The time must be specified using an interval of 5 minutes, such as 11:00, 11:05, 11:10, and must be later than the start time timestamp.
    --
    -- The time range between the start and end time must be less than 24 hours.
    endTime :: Core.NominalDiffTime,
    -- | The queues, up to 100, or channels, to use to filter the metrics returned. Metric data is retrieved only for the resources associated with the queues or channels included in the filter. You can include both queue IDs and queue ARNs in the same request. Both @VOICE@ and @CHAT@ channels are supported.
    filters :: Types.Filters,
    -- | The metrics to retrieve. Specify the name, unit, and statistic for each metric. The following historical metrics are available. For a description of each metric, see <https://docs.aws.amazon.com/connect/latest/adminguide/historical-metrics-definitions.html Historical Metrics Definitions> in the /Amazon Connect Administrator Guide/ .
    --
    --
    --     * ABANDON_TIME
    --
    --     * Unit: SECONDS
    -- Statistic: AVG
    --
    --
    --     * AFTER_CONTACT_WORK_TIME
    --
    --     * Unit: SECONDS
    -- Statistic: AVG
    --
    --
    --     * API_CONTACTS_HANDLED
    --
    --     * Unit: COUNT
    -- Statistic: SUM
    --
    --
    --     * CALLBACK_CONTACTS_HANDLED
    --
    --     * Unit: COUNT
    -- Statistic: SUM
    --
    --
    --     * CONTACTS_ABANDONED
    --
    --     * Unit: COUNT
    -- Statistic: SUM
    --
    --
    --     * CONTACTS_AGENT_HUNG_UP_FIRST
    --
    --     * Unit: COUNT
    -- Statistic: SUM
    --
    --
    --     * CONTACTS_CONSULTED
    --
    --     * Unit: COUNT
    -- Statistic: SUM
    --
    --
    --     * CONTACTS_HANDLED
    --
    --     * Unit: COUNT
    -- Statistic: SUM
    --
    --
    --     * CONTACTS_HANDLED_INCOMING
    --
    --     * Unit: COUNT
    -- Statistic: SUM
    --
    --
    --     * CONTACTS_HANDLED_OUTBOUND
    --
    --     * Unit: COUNT
    -- Statistic: SUM
    --
    --
    --     * CONTACTS_HOLD_ABANDONS
    --
    --     * Unit: COUNT
    -- Statistic: SUM
    --
    --
    --     * CONTACTS_MISSED
    --
    --     * Unit: COUNT
    -- Statistic: SUM
    --
    --
    --     * CONTACTS_QUEUED
    --
    --     * Unit: COUNT
    -- Statistic: SUM
    --
    --
    --     * CONTACTS_TRANSFERRED_IN
    --
    --     * Unit: COUNT
    -- Statistic: SUM
    --
    --
    --     * CONTACTS_TRANSFERRED_IN_FROM_QUEUE
    --
    --     * Unit: COUNT
    -- Statistic: SUM
    --
    --
    --     * CONTACTS_TRANSFERRED_OUT
    --
    --     * Unit: COUNT
    -- Statistic: SUM
    --
    --
    --     * CONTACTS_TRANSFERRED_OUT_FROM_QUEUE
    --
    --     * Unit: COUNT
    -- Statistic: SUM
    --
    --
    --     * HANDLE_TIME
    --
    --     * Unit: SECONDS
    -- Statistic: AVG
    --
    --
    --     * HOLD_TIME
    --
    --     * Unit: SECONDS
    -- Statistic: AVG
    --
    --
    --     * INTERACTION_AND_HOLD_TIME
    --
    --     * Unit: SECONDS
    -- Statistic: AVG
    --
    --
    --     * INTERACTION_TIME
    --
    --     * Unit: SECONDS
    -- Statistic: AVG
    --
    --
    --     * OCCUPANCY
    --
    --     * Unit: PERCENT
    -- Statistic: AVG
    --
    --
    --     * QUEUE_ANSWER_TIME
    --
    --     * Unit: SECONDS
    -- Statistic: AVG
    --
    --
    --     * QUEUED_TIME
    --
    --     * Unit: SECONDS
    -- Statistic: MAX
    --
    --
    --     * SERVICE_LEVEL
    --
    --     * Unit: PERCENT
    -- Statistic: AVG
    -- Threshold: Only "Less than" comparisons are supported, with the following service level thresholds: 15, 20, 25, 30, 45, 60, 90, 120, 180, 240, 300, 600
    historicalMetrics :: [Types.HistoricalMetric],
    -- | The grouping applied to the metrics returned. For example, when results are grouped by queue, the metrics returned are grouped by queue. The values returned apply to the metrics for each queue rather than aggregated for all queues.
    --
    -- The only supported grouping is @QUEUE@ .
    -- If no grouping is specified, a summary of metrics for all queues is returned.
    groupings :: Core.Maybe [Types.Grouping],
    -- | The maximimum number of results to return per page.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetMetricData' value with any optional fields omitted.
mkGetMetricData ::
  -- | 'instanceId'
  Types.InstanceId ->
  -- | 'startTime'
  Core.NominalDiffTime ->
  -- | 'endTime'
  Core.NominalDiffTime ->
  -- | 'filters'
  Types.Filters ->
  GetMetricData
mkGetMetricData instanceId startTime endTime filters =
  GetMetricData'
    { instanceId,
      startTime,
      endTime,
      filters,
      historicalMetrics = Core.mempty,
      groupings = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdInstanceId :: Lens.Lens' GetMetricData Types.InstanceId
gmdInstanceId = Lens.field @"instanceId"
{-# DEPRECATED gmdInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The timestamp, in UNIX Epoch time format, at which to start the reporting interval for the retrieval of historical metrics data. The time must be specified using a multiple of 5 minutes, such as 10:05, 10:10, 10:15.
--
-- The start time cannot be earlier than 24 hours before the time of the request. Historical metrics are available only for 24 hours.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdStartTime :: Lens.Lens' GetMetricData Core.NominalDiffTime
gmdStartTime = Lens.field @"startTime"
{-# DEPRECATED gmdStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The timestamp, in UNIX Epoch time format, at which to end the reporting interval for the retrieval of historical metrics data. The time must be specified using an interval of 5 minutes, such as 11:00, 11:05, 11:10, and must be later than the start time timestamp.
--
-- The time range between the start and end time must be less than 24 hours.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdEndTime :: Lens.Lens' GetMetricData Core.NominalDiffTime
gmdEndTime = Lens.field @"endTime"
{-# DEPRECATED gmdEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The queues, up to 100, or channels, to use to filter the metrics returned. Metric data is retrieved only for the resources associated with the queues or channels included in the filter. You can include both queue IDs and queue ARNs in the same request. Both @VOICE@ and @CHAT@ channels are supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdFilters :: Lens.Lens' GetMetricData Types.Filters
gmdFilters = Lens.field @"filters"
{-# DEPRECATED gmdFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The metrics to retrieve. Specify the name, unit, and statistic for each metric. The following historical metrics are available. For a description of each metric, see <https://docs.aws.amazon.com/connect/latest/adminguide/historical-metrics-definitions.html Historical Metrics Definitions> in the /Amazon Connect Administrator Guide/ .
--
--
--     * ABANDON_TIME
--
--     * Unit: SECONDS
-- Statistic: AVG
--
--
--     * AFTER_CONTACT_WORK_TIME
--
--     * Unit: SECONDS
-- Statistic: AVG
--
--
--     * API_CONTACTS_HANDLED
--
--     * Unit: COUNT
-- Statistic: SUM
--
--
--     * CALLBACK_CONTACTS_HANDLED
--
--     * Unit: COUNT
-- Statistic: SUM
--
--
--     * CONTACTS_ABANDONED
--
--     * Unit: COUNT
-- Statistic: SUM
--
--
--     * CONTACTS_AGENT_HUNG_UP_FIRST
--
--     * Unit: COUNT
-- Statistic: SUM
--
--
--     * CONTACTS_CONSULTED
--
--     * Unit: COUNT
-- Statistic: SUM
--
--
--     * CONTACTS_HANDLED
--
--     * Unit: COUNT
-- Statistic: SUM
--
--
--     * CONTACTS_HANDLED_INCOMING
--
--     * Unit: COUNT
-- Statistic: SUM
--
--
--     * CONTACTS_HANDLED_OUTBOUND
--
--     * Unit: COUNT
-- Statistic: SUM
--
--
--     * CONTACTS_HOLD_ABANDONS
--
--     * Unit: COUNT
-- Statistic: SUM
--
--
--     * CONTACTS_MISSED
--
--     * Unit: COUNT
-- Statistic: SUM
--
--
--     * CONTACTS_QUEUED
--
--     * Unit: COUNT
-- Statistic: SUM
--
--
--     * CONTACTS_TRANSFERRED_IN
--
--     * Unit: COUNT
-- Statistic: SUM
--
--
--     * CONTACTS_TRANSFERRED_IN_FROM_QUEUE
--
--     * Unit: COUNT
-- Statistic: SUM
--
--
--     * CONTACTS_TRANSFERRED_OUT
--
--     * Unit: COUNT
-- Statistic: SUM
--
--
--     * CONTACTS_TRANSFERRED_OUT_FROM_QUEUE
--
--     * Unit: COUNT
-- Statistic: SUM
--
--
--     * HANDLE_TIME
--
--     * Unit: SECONDS
-- Statistic: AVG
--
--
--     * HOLD_TIME
--
--     * Unit: SECONDS
-- Statistic: AVG
--
--
--     * INTERACTION_AND_HOLD_TIME
--
--     * Unit: SECONDS
-- Statistic: AVG
--
--
--     * INTERACTION_TIME
--
--     * Unit: SECONDS
-- Statistic: AVG
--
--
--     * OCCUPANCY
--
--     * Unit: PERCENT
-- Statistic: AVG
--
--
--     * QUEUE_ANSWER_TIME
--
--     * Unit: SECONDS
-- Statistic: AVG
--
--
--     * QUEUED_TIME
--
--     * Unit: SECONDS
-- Statistic: MAX
--
--
--     * SERVICE_LEVEL
--
--     * Unit: PERCENT
-- Statistic: AVG
-- Threshold: Only "Less than" comparisons are supported, with the following service level thresholds: 15, 20, 25, 30, 45, 60, 90, 120, 180, 240, 300, 600
--
--
--
-- /Note:/ Consider using 'historicalMetrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdHistoricalMetrics :: Lens.Lens' GetMetricData [Types.HistoricalMetric]
gmdHistoricalMetrics = Lens.field @"historicalMetrics"
{-# DEPRECATED gmdHistoricalMetrics "Use generic-lens or generic-optics with 'historicalMetrics' instead." #-}

-- | The grouping applied to the metrics returned. For example, when results are grouped by queue, the metrics returned are grouped by queue. The values returned apply to the metrics for each queue rather than aggregated for all queues.
--
-- The only supported grouping is @QUEUE@ .
-- If no grouping is specified, a summary of metrics for all queues is returned.
--
-- /Note:/ Consider using 'groupings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdGroupings :: Lens.Lens' GetMetricData (Core.Maybe [Types.Grouping])
gmdGroupings = Lens.field @"groupings"
{-# DEPRECATED gmdGroupings "Use generic-lens or generic-optics with 'groupings' instead." #-}

-- | The maximimum number of results to return per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdMaxResults :: Lens.Lens' GetMetricData (Core.Maybe Core.Natural)
gmdMaxResults = Lens.field @"maxResults"
{-# DEPRECATED gmdMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdNextToken :: Lens.Lens' GetMetricData (Core.Maybe Types.NextToken)
gmdNextToken = Lens.field @"nextToken"
{-# DEPRECATED gmdNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON GetMetricData where
  toJSON GetMetricData {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("StartTime" Core..= startTime),
            Core.Just ("EndTime" Core..= endTime),
            Core.Just ("Filters" Core..= filters),
            Core.Just ("HistoricalMetrics" Core..= historicalMetrics),
            ("Groupings" Core..=) Core.<$> groupings,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest GetMetricData where
  type Rs GetMetricData = GetMetricDataResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ("/metrics/historical/" Core.<> (Core.toText instanceId)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMetricDataResponse'
            Core.<$> (x Core..:? "MetricResults")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager GetMetricData where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"metricResults" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkGetMetricDataResponse' smart constructor.
data GetMetricDataResponse = GetMetricDataResponse'
  { -- | Information about the historical metrics.
    --
    -- If no grouping is specified, a summary of metric data is returned.
    metricResults :: Core.Maybe [Types.HistoricalMetricResult],
    -- | If there are additional results, this is the token for the next set of results.
    --
    -- The token expires after 5 minutes from the time it is created. Subsequent requests that use the token must use the same request parameters as the request that generated the token.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetMetricDataResponse' value with any optional fields omitted.
mkGetMetricDataResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetMetricDataResponse
mkGetMetricDataResponse responseStatus =
  GetMetricDataResponse'
    { metricResults = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Information about the historical metrics.
--
-- If no grouping is specified, a summary of metric data is returned.
--
-- /Note:/ Consider using 'metricResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdrrsMetricResults :: Lens.Lens' GetMetricDataResponse (Core.Maybe [Types.HistoricalMetricResult])
gmdrrsMetricResults = Lens.field @"metricResults"
{-# DEPRECATED gmdrrsMetricResults "Use generic-lens or generic-optics with 'metricResults' instead." #-}

-- | If there are additional results, this is the token for the next set of results.
--
-- The token expires after 5 minutes from the time it is created. Subsequent requests that use the token must use the same request parameters as the request that generated the token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdrrsNextToken :: Lens.Lens' GetMetricDataResponse (Core.Maybe Types.NextToken)
gmdrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gmdrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdrrsResponseStatus :: Lens.Lens' GetMetricDataResponse Core.Int
gmdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gmdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
