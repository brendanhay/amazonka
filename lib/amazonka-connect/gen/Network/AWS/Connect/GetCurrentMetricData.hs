{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.GetCurrentMetricData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the real-time metric data from the specified Amazon Connect instance.
--
-- For a description of each metric, see <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html Real-time Metrics Definitions> in the /Amazon Connect Administrator Guide/ .
module Network.AWS.Connect.GetCurrentMetricData
  ( -- * Creating a request
    GetCurrentMetricData (..),
    mkGetCurrentMetricData,

    -- ** Request lenses
    gcmdNextToken,
    gcmdGroupings,
    gcmdMaxResults,
    gcmdInstanceId,
    gcmdFilters,
    gcmdCurrentMetrics,

    -- * Destructuring the response
    GetCurrentMetricDataResponse (..),
    mkGetCurrentMetricDataResponse,

    -- ** Response lenses
    gcmdrsMetricResults,
    gcmdrsDataSnapshotTime,
    gcmdrsNextToken,
    gcmdrsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetCurrentMetricData' smart constructor.
data GetCurrentMetricData = GetCurrentMetricData'
  { nextToken ::
      Lude.Maybe Lude.Text,
    groupings :: Lude.Maybe [Grouping],
    maxResults :: Lude.Maybe Lude.Natural,
    instanceId :: Lude.Text,
    filters :: Filters,
    currentMetrics :: [CurrentMetric]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCurrentMetricData' with the minimum fields required to make a request.
--
-- * 'currentMetrics' - The metrics to retrieve. Specify the name and unit for each metric. The following metrics are available. For a description of all the metrics, see <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html Real-time Metrics Definitions> in the /Amazon Connect Administrator Guide/ .
--
--
--     * AGENTS_AFTER_CONTACT_WORK
--
--     * Unit: COUNT
-- Name in real-time metrics report: <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#aftercallwork-real-time ACW>
--
--
--     * AGENTS_AVAILABLE
--
--     * Unit: COUNT
-- Name in real-time metrics report: <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#available-real-time Available>
--
--
--     * AGENTS_ERROR
--
--     * Unit: COUNT
-- Name in real-time metrics report: <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#error-real-time Error>
--
--
--     * AGENTS_NON_PRODUCTIVE
--
--     * Unit: COUNT
-- Name in real-time metrics report: <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#non-productive-time-real-time NPT (Non-Productive Time)>
--
--
--     * AGENTS_ON_CALL
--
--     * Unit: COUNT
-- Name in real-time metrics report: <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#on-call-real-time On contact>
--
--
--     * AGENTS_ON_CONTACT
--
--     * Unit: COUNT
-- Name in real-time metrics report: <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#on-call-real-time On contact>
--
--
--     * AGENTS_ONLINE
--
--     * Unit: COUNT
-- Name in real-time metrics report: <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#online-real-time Online>
--
--
--     * AGENTS_STAFFED
--
--     * Unit: COUNT
-- Name in real-time metrics report: <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#staffed-real-time Staffed>
--
--
--     * CONTACTS_IN_QUEUE
--
--     * Unit: COUNT
-- Name in real-time metrics report: <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#in-queue-real-time In queue>
--
--
--     * CONTACTS_SCHEDULED
--
--     * Unit: COUNT
-- Name in real-time metrics report: <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#scheduled-real-time Scheduled>
--
--
--     * OLDEST_CONTACT_AGE
--
--     * Unit: SECONDS
-- When you use groupings, Unit says SECONDS but the Value is returned in MILLISECONDS. For example, if you get a response like this:
-- @{ "Metric": { "Name": "OLDEST_CONTACT_AGE", "Unit": "SECONDS" }, "Value": 24113.0 @ }
-- The actual OLDEST_CONTACT_AGE is 24 seconds.
-- Name in real-time metrics report: <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#oldest-real-time Oldest>
--
--
--     * SLOTS_ACTIVE
--
--     * Unit: COUNT
-- Name in real-time metrics report: <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#active-real-time Active>
--
--
--     * SLOTS_AVAILABLE
--
--     * Unit: COUNT
-- Name in real-time metrics report: <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#availability-real-time Availability>
--
--
-- * 'filters' - The queues, up to 100, or channels, to use to filter the metrics returned. Metric data is retrieved only for the resources associated with the queues or channels included in the filter. You can include both queue IDs and queue ARNs in the same request. Both @VOICE@ and @CHAT@ channels are supported.
-- * 'groupings' - The grouping applied to the metrics returned. For example, when grouped by @QUEUE@ , the metrics returned apply to each queue rather than aggregated for all queues. If you group by @CHANNEL@ , you should include a Channels filter. Both @VOICE@ and @CHAT@ channels are supported.
--
-- If no @Grouping@ is included in the request, a summary of metrics is returned.
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'maxResults' - The maximimum number of results to return per page.
-- * 'nextToken' - The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- The token expires after 5 minutes from the time it is created. Subsequent requests that use the token must use the same request parameters as the request that generated the token.
mkGetCurrentMetricData ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'filters'
  Filters ->
  GetCurrentMetricData
mkGetCurrentMetricData pInstanceId_ pFilters_ =
  GetCurrentMetricData'
    { nextToken = Lude.Nothing,
      groupings = Lude.Nothing,
      maxResults = Lude.Nothing,
      instanceId = pInstanceId_,
      filters = pFilters_,
      currentMetrics = Lude.mempty
    }

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- The token expires after 5 minutes from the time it is created. Subsequent requests that use the token must use the same request parameters as the request that generated the token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmdNextToken :: Lens.Lens' GetCurrentMetricData (Lude.Maybe Lude.Text)
gcmdNextToken = Lens.lens (nextToken :: GetCurrentMetricData -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetCurrentMetricData)
{-# DEPRECATED gcmdNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The grouping applied to the metrics returned. For example, when grouped by @QUEUE@ , the metrics returned apply to each queue rather than aggregated for all queues. If you group by @CHANNEL@ , you should include a Channels filter. Both @VOICE@ and @CHAT@ channels are supported.
--
-- If no @Grouping@ is included in the request, a summary of metrics is returned.
--
-- /Note:/ Consider using 'groupings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmdGroupings :: Lens.Lens' GetCurrentMetricData (Lude.Maybe [Grouping])
gcmdGroupings = Lens.lens (groupings :: GetCurrentMetricData -> Lude.Maybe [Grouping]) (\s a -> s {groupings = a} :: GetCurrentMetricData)
{-# DEPRECATED gcmdGroupings "Use generic-lens or generic-optics with 'groupings' instead." #-}

-- | The maximimum number of results to return per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmdMaxResults :: Lens.Lens' GetCurrentMetricData (Lude.Maybe Lude.Natural)
gcmdMaxResults = Lens.lens (maxResults :: GetCurrentMetricData -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetCurrentMetricData)
{-# DEPRECATED gcmdMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmdInstanceId :: Lens.Lens' GetCurrentMetricData Lude.Text
gcmdInstanceId = Lens.lens (instanceId :: GetCurrentMetricData -> Lude.Text) (\s a -> s {instanceId = a} :: GetCurrentMetricData)
{-# DEPRECATED gcmdInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The queues, up to 100, or channels, to use to filter the metrics returned. Metric data is retrieved only for the resources associated with the queues or channels included in the filter. You can include both queue IDs and queue ARNs in the same request. Both @VOICE@ and @CHAT@ channels are supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmdFilters :: Lens.Lens' GetCurrentMetricData Filters
gcmdFilters = Lens.lens (filters :: GetCurrentMetricData -> Filters) (\s a -> s {filters = a} :: GetCurrentMetricData)
{-# DEPRECATED gcmdFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The metrics to retrieve. Specify the name and unit for each metric. The following metrics are available. For a description of all the metrics, see <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html Real-time Metrics Definitions> in the /Amazon Connect Administrator Guide/ .
--
--
--     * AGENTS_AFTER_CONTACT_WORK
--
--     * Unit: COUNT
-- Name in real-time metrics report: <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#aftercallwork-real-time ACW>
--
--
--     * AGENTS_AVAILABLE
--
--     * Unit: COUNT
-- Name in real-time metrics report: <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#available-real-time Available>
--
--
--     * AGENTS_ERROR
--
--     * Unit: COUNT
-- Name in real-time metrics report: <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#error-real-time Error>
--
--
--     * AGENTS_NON_PRODUCTIVE
--
--     * Unit: COUNT
-- Name in real-time metrics report: <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#non-productive-time-real-time NPT (Non-Productive Time)>
--
--
--     * AGENTS_ON_CALL
--
--     * Unit: COUNT
-- Name in real-time metrics report: <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#on-call-real-time On contact>
--
--
--     * AGENTS_ON_CONTACT
--
--     * Unit: COUNT
-- Name in real-time metrics report: <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#on-call-real-time On contact>
--
--
--     * AGENTS_ONLINE
--
--     * Unit: COUNT
-- Name in real-time metrics report: <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#online-real-time Online>
--
--
--     * AGENTS_STAFFED
--
--     * Unit: COUNT
-- Name in real-time metrics report: <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#staffed-real-time Staffed>
--
--
--     * CONTACTS_IN_QUEUE
--
--     * Unit: COUNT
-- Name in real-time metrics report: <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#in-queue-real-time In queue>
--
--
--     * CONTACTS_SCHEDULED
--
--     * Unit: COUNT
-- Name in real-time metrics report: <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#scheduled-real-time Scheduled>
--
--
--     * OLDEST_CONTACT_AGE
--
--     * Unit: SECONDS
-- When you use groupings, Unit says SECONDS but the Value is returned in MILLISECONDS. For example, if you get a response like this:
-- @{ "Metric": { "Name": "OLDEST_CONTACT_AGE", "Unit": "SECONDS" }, "Value": 24113.0 @ }
-- The actual OLDEST_CONTACT_AGE is 24 seconds.
-- Name in real-time metrics report: <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#oldest-real-time Oldest>
--
--
--     * SLOTS_ACTIVE
--
--     * Unit: COUNT
-- Name in real-time metrics report: <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#active-real-time Active>
--
--
--     * SLOTS_AVAILABLE
--
--     * Unit: COUNT
-- Name in real-time metrics report: <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#availability-real-time Availability>
--
--
--
-- /Note:/ Consider using 'currentMetrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmdCurrentMetrics :: Lens.Lens' GetCurrentMetricData [CurrentMetric]
gcmdCurrentMetrics = Lens.lens (currentMetrics :: GetCurrentMetricData -> [CurrentMetric]) (\s a -> s {currentMetrics = a} :: GetCurrentMetricData)
{-# DEPRECATED gcmdCurrentMetrics "Use generic-lens or generic-optics with 'currentMetrics' instead." #-}

instance Lude.AWSRequest GetCurrentMetricData where
  type Rs GetCurrentMetricData = GetCurrentMetricDataResponse
  request = Req.postJSON connectService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetCurrentMetricDataResponse'
            Lude.<$> (x Lude..?> "MetricResults" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "DataSnapshotTime")
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetCurrentMetricData where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetCurrentMetricData where
  toJSON GetCurrentMetricData' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Groupings" Lude..=) Lude.<$> groupings,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("Filters" Lude..= filters),
            Lude.Just ("CurrentMetrics" Lude..= currentMetrics)
          ]
      )

instance Lude.ToPath GetCurrentMetricData where
  toPath GetCurrentMetricData' {..} =
    Lude.mconcat ["/metrics/current/", Lude.toBS instanceId]

instance Lude.ToQuery GetCurrentMetricData where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetCurrentMetricDataResponse' smart constructor.
data GetCurrentMetricDataResponse = GetCurrentMetricDataResponse'
  { metricResults ::
      Lude.Maybe [CurrentMetricResult],
    dataSnapshotTime ::
      Lude.Maybe Lude.Timestamp,
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

-- | Creates a value of 'GetCurrentMetricDataResponse' with the minimum fields required to make a request.
--
-- * 'dataSnapshotTime' - The time at which the metrics were retrieved and cached for pagination.
-- * 'metricResults' - Information about the real-time metrics.
-- * 'nextToken' - If there are additional results, this is the token for the next set of results.
--
-- The token expires after 5 minutes from the time it is created. Subsequent requests that use the token must use the same request parameters as the request that generated the token.
-- * 'responseStatus' - The response status code.
mkGetCurrentMetricDataResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetCurrentMetricDataResponse
mkGetCurrentMetricDataResponse pResponseStatus_ =
  GetCurrentMetricDataResponse'
    { metricResults = Lude.Nothing,
      dataSnapshotTime = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the real-time metrics.
--
-- /Note:/ Consider using 'metricResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmdrsMetricResults :: Lens.Lens' GetCurrentMetricDataResponse (Lude.Maybe [CurrentMetricResult])
gcmdrsMetricResults = Lens.lens (metricResults :: GetCurrentMetricDataResponse -> Lude.Maybe [CurrentMetricResult]) (\s a -> s {metricResults = a} :: GetCurrentMetricDataResponse)
{-# DEPRECATED gcmdrsMetricResults "Use generic-lens or generic-optics with 'metricResults' instead." #-}

-- | The time at which the metrics were retrieved and cached for pagination.
--
-- /Note:/ Consider using 'dataSnapshotTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmdrsDataSnapshotTime :: Lens.Lens' GetCurrentMetricDataResponse (Lude.Maybe Lude.Timestamp)
gcmdrsDataSnapshotTime = Lens.lens (dataSnapshotTime :: GetCurrentMetricDataResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {dataSnapshotTime = a} :: GetCurrentMetricDataResponse)
{-# DEPRECATED gcmdrsDataSnapshotTime "Use generic-lens or generic-optics with 'dataSnapshotTime' instead." #-}

-- | If there are additional results, this is the token for the next set of results.
--
-- The token expires after 5 minutes from the time it is created. Subsequent requests that use the token must use the same request parameters as the request that generated the token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmdrsNextToken :: Lens.Lens' GetCurrentMetricDataResponse (Lude.Maybe Lude.Text)
gcmdrsNextToken = Lens.lens (nextToken :: GetCurrentMetricDataResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetCurrentMetricDataResponse)
{-# DEPRECATED gcmdrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmdrsResponseStatus :: Lens.Lens' GetCurrentMetricDataResponse Lude.Int
gcmdrsResponseStatus = Lens.lens (responseStatus :: GetCurrentMetricDataResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetCurrentMetricDataResponse)
{-# DEPRECATED gcmdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
