{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    gmdNextToken,
    gmdGroupings,
    gmdMaxResults,
    gmdInstanceId,
    gmdStartTime,
    gmdEndTime,
    gmdFilters,
    gmdHistoricalMetrics,

    -- * Destructuring the response
    GetMetricDataResponse (..),
    mkGetMetricDataResponse,

    -- ** Response lenses
    gmdrsMetricResults,
    gmdrsNextToken,
    gmdrsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetMetricData' smart constructor.
data GetMetricData = GetMetricData'
  { nextToken ::
      Lude.Maybe Lude.Text,
    groupings :: Lude.Maybe [Grouping],
    maxResults :: Lude.Maybe Lude.Natural,
    instanceId :: Lude.Text,
    startTime :: Lude.Timestamp,
    endTime :: Lude.Timestamp,
    filters :: Filters,
    historicalMetrics :: [HistoricalMetric]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetMetricData' with the minimum fields required to make a request.
--
-- * 'endTime' - The timestamp, in UNIX Epoch time format, at which to end the reporting interval for the retrieval of historical metrics data. The time must be specified using an interval of 5 minutes, such as 11:00, 11:05, 11:10, and must be later than the start time timestamp.
--
-- The time range between the start and end time must be less than 24 hours.
-- * 'filters' - The queues, up to 100, or channels, to use to filter the metrics returned. Metric data is retrieved only for the resources associated with the queues or channels included in the filter. You can include both queue IDs and queue ARNs in the same request. Both @VOICE@ and @CHAT@ channels are supported.
-- * 'groupings' - The grouping applied to the metrics returned. For example, when results are grouped by queue, the metrics returned are grouped by queue. The values returned apply to the metrics for each queue rather than aggregated for all queues.
--
-- The only supported grouping is @QUEUE@ .
-- If no grouping is specified, a summary of metrics for all queues is returned.
-- * 'historicalMetrics' - The metrics to retrieve. Specify the name, unit, and statistic for each metric. The following historical metrics are available. For a description of each metric, see <https://docs.aws.amazon.com/connect/latest/adminguide/historical-metrics-definitions.html Historical Metrics Definitions> in the /Amazon Connect Administrator Guide/ .
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
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'maxResults' - The maximimum number of results to return per page.
-- * 'nextToken' - The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
-- * 'startTime' - The timestamp, in UNIX Epoch time format, at which to start the reporting interval for the retrieval of historical metrics data. The time must be specified using a multiple of 5 minutes, such as 10:05, 10:10, 10:15.
--
-- The start time cannot be earlier than 24 hours before the time of the request. Historical metrics are available only for 24 hours.
mkGetMetricData ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'startTime'
  Lude.Timestamp ->
  -- | 'endTime'
  Lude.Timestamp ->
  -- | 'filters'
  Filters ->
  GetMetricData
mkGetMetricData pInstanceId_ pStartTime_ pEndTime_ pFilters_ =
  GetMetricData'
    { nextToken = Lude.Nothing,
      groupings = Lude.Nothing,
      maxResults = Lude.Nothing,
      instanceId = pInstanceId_,
      startTime = pStartTime_,
      endTime = pEndTime_,
      filters = pFilters_,
      historicalMetrics = Lude.mempty
    }

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdNextToken :: Lens.Lens' GetMetricData (Lude.Maybe Lude.Text)
gmdNextToken = Lens.lens (nextToken :: GetMetricData -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetMetricData)
{-# DEPRECATED gmdNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The grouping applied to the metrics returned. For example, when results are grouped by queue, the metrics returned are grouped by queue. The values returned apply to the metrics for each queue rather than aggregated for all queues.
--
-- The only supported grouping is @QUEUE@ .
-- If no grouping is specified, a summary of metrics for all queues is returned.
--
-- /Note:/ Consider using 'groupings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdGroupings :: Lens.Lens' GetMetricData (Lude.Maybe [Grouping])
gmdGroupings = Lens.lens (groupings :: GetMetricData -> Lude.Maybe [Grouping]) (\s a -> s {groupings = a} :: GetMetricData)
{-# DEPRECATED gmdGroupings "Use generic-lens or generic-optics with 'groupings' instead." #-}

-- | The maximimum number of results to return per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdMaxResults :: Lens.Lens' GetMetricData (Lude.Maybe Lude.Natural)
gmdMaxResults = Lens.lens (maxResults :: GetMetricData -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetMetricData)
{-# DEPRECATED gmdMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdInstanceId :: Lens.Lens' GetMetricData Lude.Text
gmdInstanceId = Lens.lens (instanceId :: GetMetricData -> Lude.Text) (\s a -> s {instanceId = a} :: GetMetricData)
{-# DEPRECATED gmdInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The timestamp, in UNIX Epoch time format, at which to start the reporting interval for the retrieval of historical metrics data. The time must be specified using a multiple of 5 minutes, such as 10:05, 10:10, 10:15.
--
-- The start time cannot be earlier than 24 hours before the time of the request. Historical metrics are available only for 24 hours.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdStartTime :: Lens.Lens' GetMetricData Lude.Timestamp
gmdStartTime = Lens.lens (startTime :: GetMetricData -> Lude.Timestamp) (\s a -> s {startTime = a} :: GetMetricData)
{-# DEPRECATED gmdStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The timestamp, in UNIX Epoch time format, at which to end the reporting interval for the retrieval of historical metrics data. The time must be specified using an interval of 5 minutes, such as 11:00, 11:05, 11:10, and must be later than the start time timestamp.
--
-- The time range between the start and end time must be less than 24 hours.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdEndTime :: Lens.Lens' GetMetricData Lude.Timestamp
gmdEndTime = Lens.lens (endTime :: GetMetricData -> Lude.Timestamp) (\s a -> s {endTime = a} :: GetMetricData)
{-# DEPRECATED gmdEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The queues, up to 100, or channels, to use to filter the metrics returned. Metric data is retrieved only for the resources associated with the queues or channels included in the filter. You can include both queue IDs and queue ARNs in the same request. Both @VOICE@ and @CHAT@ channels are supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdFilters :: Lens.Lens' GetMetricData Filters
gmdFilters = Lens.lens (filters :: GetMetricData -> Filters) (\s a -> s {filters = a} :: GetMetricData)
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
gmdHistoricalMetrics :: Lens.Lens' GetMetricData [HistoricalMetric]
gmdHistoricalMetrics = Lens.lens (historicalMetrics :: GetMetricData -> [HistoricalMetric]) (\s a -> s {historicalMetrics = a} :: GetMetricData)
{-# DEPRECATED gmdHistoricalMetrics "Use generic-lens or generic-optics with 'historicalMetrics' instead." #-}

instance Page.AWSPager GetMetricData where
  page rq rs
    | Page.stop (rs Lens.^. gmdrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gmdrsMetricResults) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gmdNextToken Lens..~ rs Lens.^. gmdrsNextToken

instance Lude.AWSRequest GetMetricData where
  type Rs GetMetricData = GetMetricDataResponse
  request = Req.postJSON connectService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetMetricDataResponse'
            Lude.<$> (x Lude..?> "MetricResults" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetMetricData where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetMetricData where
  toJSON GetMetricData' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Groupings" Lude..=) Lude.<$> groupings,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("StartTime" Lude..= startTime),
            Lude.Just ("EndTime" Lude..= endTime),
            Lude.Just ("Filters" Lude..= filters),
            Lude.Just ("HistoricalMetrics" Lude..= historicalMetrics)
          ]
      )

instance Lude.ToPath GetMetricData where
  toPath GetMetricData' {..} =
    Lude.mconcat ["/metrics/historical/", Lude.toBS instanceId]

instance Lude.ToQuery GetMetricData where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetMetricDataResponse' smart constructor.
data GetMetricDataResponse = GetMetricDataResponse'
  { metricResults ::
      Lude.Maybe [HistoricalMetricResult],
    nextToken :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'GetMetricDataResponse' with the minimum fields required to make a request.
--
-- * 'metricResults' - Information about the historical metrics.
--
-- If no grouping is specified, a summary of metric data is returned.
-- * 'nextToken' - If there are additional results, this is the token for the next set of results.
--
-- The token expires after 5 minutes from the time it is created. Subsequent requests that use the token must use the same request parameters as the request that generated the token.
-- * 'responseStatus' - The response status code.
mkGetMetricDataResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetMetricDataResponse
mkGetMetricDataResponse pResponseStatus_ =
  GetMetricDataResponse'
    { metricResults = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the historical metrics.
--
-- If no grouping is specified, a summary of metric data is returned.
--
-- /Note:/ Consider using 'metricResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdrsMetricResults :: Lens.Lens' GetMetricDataResponse (Lude.Maybe [HistoricalMetricResult])
gmdrsMetricResults = Lens.lens (metricResults :: GetMetricDataResponse -> Lude.Maybe [HistoricalMetricResult]) (\s a -> s {metricResults = a} :: GetMetricDataResponse)
{-# DEPRECATED gmdrsMetricResults "Use generic-lens or generic-optics with 'metricResults' instead." #-}

-- | If there are additional results, this is the token for the next set of results.
--
-- The token expires after 5 minutes from the time it is created. Subsequent requests that use the token must use the same request parameters as the request that generated the token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdrsNextToken :: Lens.Lens' GetMetricDataResponse (Lude.Maybe Lude.Text)
gmdrsNextToken = Lens.lens (nextToken :: GetMetricDataResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetMetricDataResponse)
{-# DEPRECATED gmdrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdrsResponseStatus :: Lens.Lens' GetMetricDataResponse Lude.Int
gmdrsResponseStatus = Lens.lens (responseStatus :: GetMetricDataResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetMetricDataResponse)
{-# DEPRECATED gmdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
