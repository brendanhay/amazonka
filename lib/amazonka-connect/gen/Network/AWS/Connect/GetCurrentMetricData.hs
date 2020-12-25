{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    gcmdInstanceId,
    gcmdFilters,
    gcmdCurrentMetrics,
    gcmdGroupings,
    gcmdMaxResults,
    gcmdNextToken,

    -- * Destructuring the response
    GetCurrentMetricDataResponse (..),
    mkGetCurrentMetricDataResponse,

    -- ** Response lenses
    gcmdrrsDataSnapshotTime,
    gcmdrrsMetricResults,
    gcmdrrsNextToken,
    gcmdrrsResponseStatus,
  )
where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetCurrentMetricData' smart constructor.
data GetCurrentMetricData = GetCurrentMetricData'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Types.InstanceId,
    -- | The queues, up to 100, or channels, to use to filter the metrics returned. Metric data is retrieved only for the resources associated with the queues or channels included in the filter. You can include both queue IDs and queue ARNs in the same request. Both @VOICE@ and @CHAT@ channels are supported.
    filters :: Types.Filters,
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
    currentMetrics :: [Types.CurrentMetric],
    -- | The grouping applied to the metrics returned. For example, when grouped by @QUEUE@ , the metrics returned apply to each queue rather than aggregated for all queues. If you group by @CHANNEL@ , you should include a Channels filter. Both @VOICE@ and @CHAT@ channels are supported.
    --
    -- If no @Grouping@ is included in the request, a summary of metrics is returned.
    groupings :: Core.Maybe [Types.Grouping],
    -- | The maximimum number of results to return per page.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
    --
    -- The token expires after 5 minutes from the time it is created. Subsequent requests that use the token must use the same request parameters as the request that generated the token.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCurrentMetricData' value with any optional fields omitted.
mkGetCurrentMetricData ::
  -- | 'instanceId'
  Types.InstanceId ->
  -- | 'filters'
  Types.Filters ->
  GetCurrentMetricData
mkGetCurrentMetricData instanceId filters =
  GetCurrentMetricData'
    { instanceId,
      filters,
      currentMetrics = Core.mempty,
      groupings = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmdInstanceId :: Lens.Lens' GetCurrentMetricData Types.InstanceId
gcmdInstanceId = Lens.field @"instanceId"
{-# DEPRECATED gcmdInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The queues, up to 100, or channels, to use to filter the metrics returned. Metric data is retrieved only for the resources associated with the queues or channels included in the filter. You can include both queue IDs and queue ARNs in the same request. Both @VOICE@ and @CHAT@ channels are supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmdFilters :: Lens.Lens' GetCurrentMetricData Types.Filters
gcmdFilters = Lens.field @"filters"
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
gcmdCurrentMetrics :: Lens.Lens' GetCurrentMetricData [Types.CurrentMetric]
gcmdCurrentMetrics = Lens.field @"currentMetrics"
{-# DEPRECATED gcmdCurrentMetrics "Use generic-lens or generic-optics with 'currentMetrics' instead." #-}

-- | The grouping applied to the metrics returned. For example, when grouped by @QUEUE@ , the metrics returned apply to each queue rather than aggregated for all queues. If you group by @CHANNEL@ , you should include a Channels filter. Both @VOICE@ and @CHAT@ channels are supported.
--
-- If no @Grouping@ is included in the request, a summary of metrics is returned.
--
-- /Note:/ Consider using 'groupings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmdGroupings :: Lens.Lens' GetCurrentMetricData (Core.Maybe [Types.Grouping])
gcmdGroupings = Lens.field @"groupings"
{-# DEPRECATED gcmdGroupings "Use generic-lens or generic-optics with 'groupings' instead." #-}

-- | The maximimum number of results to return per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmdMaxResults :: Lens.Lens' GetCurrentMetricData (Core.Maybe Core.Natural)
gcmdMaxResults = Lens.field @"maxResults"
{-# DEPRECATED gcmdMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- The token expires after 5 minutes from the time it is created. Subsequent requests that use the token must use the same request parameters as the request that generated the token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmdNextToken :: Lens.Lens' GetCurrentMetricData (Core.Maybe Types.NextToken)
gcmdNextToken = Lens.field @"nextToken"
{-# DEPRECATED gcmdNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON GetCurrentMetricData where
  toJSON GetCurrentMetricData {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Filters" Core..= filters),
            Core.Just ("CurrentMetrics" Core..= currentMetrics),
            ("Groupings" Core..=) Core.<$> groupings,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest GetCurrentMetricData where
  type Rs GetCurrentMetricData = GetCurrentMetricDataResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ("/metrics/current/" Core.<> (Core.toText instanceId)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCurrentMetricDataResponse'
            Core.<$> (x Core..:? "DataSnapshotTime")
            Core.<*> (x Core..:? "MetricResults")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetCurrentMetricDataResponse' smart constructor.
data GetCurrentMetricDataResponse = GetCurrentMetricDataResponse'
  { -- | The time at which the metrics were retrieved and cached for pagination.
    dataSnapshotTime :: Core.Maybe Core.NominalDiffTime,
    -- | Information about the real-time metrics.
    metricResults :: Core.Maybe [Types.CurrentMetricResult],
    -- | If there are additional results, this is the token for the next set of results.
    --
    -- The token expires after 5 minutes from the time it is created. Subsequent requests that use the token must use the same request parameters as the request that generated the token.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetCurrentMetricDataResponse' value with any optional fields omitted.
mkGetCurrentMetricDataResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetCurrentMetricDataResponse
mkGetCurrentMetricDataResponse responseStatus =
  GetCurrentMetricDataResponse'
    { dataSnapshotTime = Core.Nothing,
      metricResults = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The time at which the metrics were retrieved and cached for pagination.
--
-- /Note:/ Consider using 'dataSnapshotTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmdrrsDataSnapshotTime :: Lens.Lens' GetCurrentMetricDataResponse (Core.Maybe Core.NominalDiffTime)
gcmdrrsDataSnapshotTime = Lens.field @"dataSnapshotTime"
{-# DEPRECATED gcmdrrsDataSnapshotTime "Use generic-lens or generic-optics with 'dataSnapshotTime' instead." #-}

-- | Information about the real-time metrics.
--
-- /Note:/ Consider using 'metricResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmdrrsMetricResults :: Lens.Lens' GetCurrentMetricDataResponse (Core.Maybe [Types.CurrentMetricResult])
gcmdrrsMetricResults = Lens.field @"metricResults"
{-# DEPRECATED gcmdrrsMetricResults "Use generic-lens or generic-optics with 'metricResults' instead." #-}

-- | If there are additional results, this is the token for the next set of results.
--
-- The token expires after 5 minutes from the time it is created. Subsequent requests that use the token must use the same request parameters as the request that generated the token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmdrrsNextToken :: Lens.Lens' GetCurrentMetricDataResponse (Core.Maybe Types.NextToken)
gcmdrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gcmdrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmdrrsResponseStatus :: Lens.Lens' GetCurrentMetricDataResponse Core.Int
gcmdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gcmdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
