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
-- Module      : Network.AWS.Connect.GetMetricData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets historical metric data from the specified Amazon Connect instance.
--
-- For a description of each historical metric, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/historical-metrics-definitions.html Historical Metrics Definitions>
-- in the /Amazon Connect Administrator Guide/.
--
-- This operation returns paginated results.
module Network.AWS.Connect.GetMetricData
  ( -- * Creating a Request
    GetMetricData (..),
    newGetMetricData,

    -- * Request Lenses
    getMetricData_nextToken,
    getMetricData_groupings,
    getMetricData_maxResults,
    getMetricData_instanceId,
    getMetricData_startTime,
    getMetricData_endTime,
    getMetricData_filters,
    getMetricData_historicalMetrics,

    -- * Destructuring the Response
    GetMetricDataResponse (..),
    newGetMetricDataResponse,

    -- * Response Lenses
    getMetricDataResponse_nextToken,
    getMetricDataResponse_metricResults,
    getMetricDataResponse_httpStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetMetricData' smart constructor.
data GetMetricData = GetMetricData'
  { -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | The grouping applied to the metrics returned. For example, when results
    -- are grouped by queue, the metrics returned are grouped by queue. The
    -- values returned apply to the metrics for each queue rather than
    -- aggregated for all queues.
    --
    -- The only supported grouping is @QUEUE@.
    --
    -- If no grouping is specified, a summary of metrics for all queues is
    -- returned.
    groupings :: Core.Maybe [Grouping],
    -- | The maximum number of results to return per page.
    maxResults :: Core.Maybe Core.Natural,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Core.Text,
    -- | The timestamp, in UNIX Epoch time format, at which to start the
    -- reporting interval for the retrieval of historical metrics data. The
    -- time must be specified using a multiple of 5 minutes, such as 10:05,
    -- 10:10, 10:15.
    --
    -- The start time cannot be earlier than 24 hours before the time of the
    -- request. Historical metrics are available only for 24 hours.
    startTime :: Core.POSIX,
    -- | The timestamp, in UNIX Epoch time format, at which to end the reporting
    -- interval for the retrieval of historical metrics data. The time must be
    -- specified using an interval of 5 minutes, such as 11:00, 11:05, 11:10,
    -- and must be later than the start time timestamp.
    --
    -- The time range between the start and end time must be less than 24
    -- hours.
    endTime :: Core.POSIX,
    -- | The queues, up to 100, or channels, to use to filter the metrics
    -- returned. Metric data is retrieved only for the resources associated
    -- with the queues or channels included in the filter. You can include both
    -- queue IDs and queue ARNs in the same request. VOICE, CHAT, and TASK
    -- channels are supported.
    filters :: Filters,
    -- | The metrics to retrieve. Specify the name, unit, and statistic for each
    -- metric. The following historical metrics are available. For a
    -- description of each metric, see
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/historical-metrics-definitions.html Historical Metrics Definitions>
    -- in the /Amazon Connect Administrator Guide/.
    --
    -- [ABANDON_TIME]
    --     Unit: SECONDS
    --
    --     Statistic: AVG
    --
    -- [AFTER_CONTACT_WORK_TIME]
    --     Unit: SECONDS
    --
    --     Statistic: AVG
    --
    -- [API_CONTACTS_HANDLED]
    --     Unit: COUNT
    --
    --     Statistic: SUM
    --
    -- [CALLBACK_CONTACTS_HANDLED]
    --     Unit: COUNT
    --
    --     Statistic: SUM
    --
    -- [CONTACTS_ABANDONED]
    --     Unit: COUNT
    --
    --     Statistic: SUM
    --
    -- [CONTACTS_AGENT_HUNG_UP_FIRST]
    --     Unit: COUNT
    --
    --     Statistic: SUM
    --
    -- [CONTACTS_CONSULTED]
    --     Unit: COUNT
    --
    --     Statistic: SUM
    --
    -- [CONTACTS_HANDLED]
    --     Unit: COUNT
    --
    --     Statistic: SUM
    --
    -- [CONTACTS_HANDLED_INCOMING]
    --     Unit: COUNT
    --
    --     Statistic: SUM
    --
    -- [CONTACTS_HANDLED_OUTBOUND]
    --     Unit: COUNT
    --
    --     Statistic: SUM
    --
    -- [CONTACTS_HOLD_ABANDONS]
    --     Unit: COUNT
    --
    --     Statistic: SUM
    --
    -- [CONTACTS_MISSED]
    --     Unit: COUNT
    --
    --     Statistic: SUM
    --
    -- [CONTACTS_QUEUED]
    --     Unit: COUNT
    --
    --     Statistic: SUM
    --
    -- [CONTACTS_TRANSFERRED_IN]
    --     Unit: COUNT
    --
    --     Statistic: SUM
    --
    -- [CONTACTS_TRANSFERRED_IN_FROM_QUEUE]
    --     Unit: COUNT
    --
    --     Statistic: SUM
    --
    -- [CONTACTS_TRANSFERRED_OUT]
    --     Unit: COUNT
    --
    --     Statistic: SUM
    --
    -- [CONTACTS_TRANSFERRED_OUT_FROM_QUEUE]
    --     Unit: COUNT
    --
    --     Statistic: SUM
    --
    -- [HANDLE_TIME]
    --     Unit: SECONDS
    --
    --     Statistic: AVG
    --
    -- [HOLD_TIME]
    --     Unit: SECONDS
    --
    --     Statistic: AVG
    --
    -- [INTERACTION_AND_HOLD_TIME]
    --     Unit: SECONDS
    --
    --     Statistic: AVG
    --
    -- [INTERACTION_TIME]
    --     Unit: SECONDS
    --
    --     Statistic: AVG
    --
    -- [OCCUPANCY]
    --     Unit: PERCENT
    --
    --     Statistic: AVG
    --
    -- [QUEUE_ANSWER_TIME]
    --     Unit: SECONDS
    --
    --     Statistic: AVG
    --
    -- [QUEUED_TIME]
    --     Unit: SECONDS
    --
    --     Statistic: MAX
    --
    -- [SERVICE_LEVEL]
    --     Unit: PERCENT
    --
    --     Statistic: AVG
    --
    --     Threshold: Only \"Less than\" comparisons are supported, with the
    --     following service level thresholds: 15, 20, 25, 30, 45, 60, 90, 120,
    --     180, 240, 300, 600
    historicalMetrics :: [HistoricalMetric]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetMetricData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getMetricData_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'groupings', 'getMetricData_groupings' - The grouping applied to the metrics returned. For example, when results
-- are grouped by queue, the metrics returned are grouped by queue. The
-- values returned apply to the metrics for each queue rather than
-- aggregated for all queues.
--
-- The only supported grouping is @QUEUE@.
--
-- If no grouping is specified, a summary of metrics for all queues is
-- returned.
--
-- 'maxResults', 'getMetricData_maxResults' - The maximum number of results to return per page.
--
-- 'instanceId', 'getMetricData_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'startTime', 'getMetricData_startTime' - The timestamp, in UNIX Epoch time format, at which to start the
-- reporting interval for the retrieval of historical metrics data. The
-- time must be specified using a multiple of 5 minutes, such as 10:05,
-- 10:10, 10:15.
--
-- The start time cannot be earlier than 24 hours before the time of the
-- request. Historical metrics are available only for 24 hours.
--
-- 'endTime', 'getMetricData_endTime' - The timestamp, in UNIX Epoch time format, at which to end the reporting
-- interval for the retrieval of historical metrics data. The time must be
-- specified using an interval of 5 minutes, such as 11:00, 11:05, 11:10,
-- and must be later than the start time timestamp.
--
-- The time range between the start and end time must be less than 24
-- hours.
--
-- 'filters', 'getMetricData_filters' - The queues, up to 100, or channels, to use to filter the metrics
-- returned. Metric data is retrieved only for the resources associated
-- with the queues or channels included in the filter. You can include both
-- queue IDs and queue ARNs in the same request. VOICE, CHAT, and TASK
-- channels are supported.
--
-- 'historicalMetrics', 'getMetricData_historicalMetrics' - The metrics to retrieve. Specify the name, unit, and statistic for each
-- metric. The following historical metrics are available. For a
-- description of each metric, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/historical-metrics-definitions.html Historical Metrics Definitions>
-- in the /Amazon Connect Administrator Guide/.
--
-- [ABANDON_TIME]
--     Unit: SECONDS
--
--     Statistic: AVG
--
-- [AFTER_CONTACT_WORK_TIME]
--     Unit: SECONDS
--
--     Statistic: AVG
--
-- [API_CONTACTS_HANDLED]
--     Unit: COUNT
--
--     Statistic: SUM
--
-- [CALLBACK_CONTACTS_HANDLED]
--     Unit: COUNT
--
--     Statistic: SUM
--
-- [CONTACTS_ABANDONED]
--     Unit: COUNT
--
--     Statistic: SUM
--
-- [CONTACTS_AGENT_HUNG_UP_FIRST]
--     Unit: COUNT
--
--     Statistic: SUM
--
-- [CONTACTS_CONSULTED]
--     Unit: COUNT
--
--     Statistic: SUM
--
-- [CONTACTS_HANDLED]
--     Unit: COUNT
--
--     Statistic: SUM
--
-- [CONTACTS_HANDLED_INCOMING]
--     Unit: COUNT
--
--     Statistic: SUM
--
-- [CONTACTS_HANDLED_OUTBOUND]
--     Unit: COUNT
--
--     Statistic: SUM
--
-- [CONTACTS_HOLD_ABANDONS]
--     Unit: COUNT
--
--     Statistic: SUM
--
-- [CONTACTS_MISSED]
--     Unit: COUNT
--
--     Statistic: SUM
--
-- [CONTACTS_QUEUED]
--     Unit: COUNT
--
--     Statistic: SUM
--
-- [CONTACTS_TRANSFERRED_IN]
--     Unit: COUNT
--
--     Statistic: SUM
--
-- [CONTACTS_TRANSFERRED_IN_FROM_QUEUE]
--     Unit: COUNT
--
--     Statistic: SUM
--
-- [CONTACTS_TRANSFERRED_OUT]
--     Unit: COUNT
--
--     Statistic: SUM
--
-- [CONTACTS_TRANSFERRED_OUT_FROM_QUEUE]
--     Unit: COUNT
--
--     Statistic: SUM
--
-- [HANDLE_TIME]
--     Unit: SECONDS
--
--     Statistic: AVG
--
-- [HOLD_TIME]
--     Unit: SECONDS
--
--     Statistic: AVG
--
-- [INTERACTION_AND_HOLD_TIME]
--     Unit: SECONDS
--
--     Statistic: AVG
--
-- [INTERACTION_TIME]
--     Unit: SECONDS
--
--     Statistic: AVG
--
-- [OCCUPANCY]
--     Unit: PERCENT
--
--     Statistic: AVG
--
-- [QUEUE_ANSWER_TIME]
--     Unit: SECONDS
--
--     Statistic: AVG
--
-- [QUEUED_TIME]
--     Unit: SECONDS
--
--     Statistic: MAX
--
-- [SERVICE_LEVEL]
--     Unit: PERCENT
--
--     Statistic: AVG
--
--     Threshold: Only \"Less than\" comparisons are supported, with the
--     following service level thresholds: 15, 20, 25, 30, 45, 60, 90, 120,
--     180, 240, 300, 600
newGetMetricData ::
  -- | 'instanceId'
  Core.Text ->
  -- | 'startTime'
  Core.UTCTime ->
  -- | 'endTime'
  Core.UTCTime ->
  -- | 'filters'
  Filters ->
  GetMetricData
newGetMetricData
  pInstanceId_
  pStartTime_
  pEndTime_
  pFilters_ =
    GetMetricData'
      { nextToken = Core.Nothing,
        groupings = Core.Nothing,
        maxResults = Core.Nothing,
        instanceId = pInstanceId_,
        startTime = Core._Time Lens.# pStartTime_,
        endTime = Core._Time Lens.# pEndTime_,
        filters = pFilters_,
        historicalMetrics = Core.mempty
      }

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
getMetricData_nextToken :: Lens.Lens' GetMetricData (Core.Maybe Core.Text)
getMetricData_nextToken = Lens.lens (\GetMetricData' {nextToken} -> nextToken) (\s@GetMetricData' {} a -> s {nextToken = a} :: GetMetricData)

-- | The grouping applied to the metrics returned. For example, when results
-- are grouped by queue, the metrics returned are grouped by queue. The
-- values returned apply to the metrics for each queue rather than
-- aggregated for all queues.
--
-- The only supported grouping is @QUEUE@.
--
-- If no grouping is specified, a summary of metrics for all queues is
-- returned.
getMetricData_groupings :: Lens.Lens' GetMetricData (Core.Maybe [Grouping])
getMetricData_groupings = Lens.lens (\GetMetricData' {groupings} -> groupings) (\s@GetMetricData' {} a -> s {groupings = a} :: GetMetricData) Core.. Lens.mapping Lens._Coerce

-- | The maximum number of results to return per page.
getMetricData_maxResults :: Lens.Lens' GetMetricData (Core.Maybe Core.Natural)
getMetricData_maxResults = Lens.lens (\GetMetricData' {maxResults} -> maxResults) (\s@GetMetricData' {} a -> s {maxResults = a} :: GetMetricData)

-- | The identifier of the Amazon Connect instance.
getMetricData_instanceId :: Lens.Lens' GetMetricData Core.Text
getMetricData_instanceId = Lens.lens (\GetMetricData' {instanceId} -> instanceId) (\s@GetMetricData' {} a -> s {instanceId = a} :: GetMetricData)

-- | The timestamp, in UNIX Epoch time format, at which to start the
-- reporting interval for the retrieval of historical metrics data. The
-- time must be specified using a multiple of 5 minutes, such as 10:05,
-- 10:10, 10:15.
--
-- The start time cannot be earlier than 24 hours before the time of the
-- request. Historical metrics are available only for 24 hours.
getMetricData_startTime :: Lens.Lens' GetMetricData Core.UTCTime
getMetricData_startTime = Lens.lens (\GetMetricData' {startTime} -> startTime) (\s@GetMetricData' {} a -> s {startTime = a} :: GetMetricData) Core.. Core._Time

-- | The timestamp, in UNIX Epoch time format, at which to end the reporting
-- interval for the retrieval of historical metrics data. The time must be
-- specified using an interval of 5 minutes, such as 11:00, 11:05, 11:10,
-- and must be later than the start time timestamp.
--
-- The time range between the start and end time must be less than 24
-- hours.
getMetricData_endTime :: Lens.Lens' GetMetricData Core.UTCTime
getMetricData_endTime = Lens.lens (\GetMetricData' {endTime} -> endTime) (\s@GetMetricData' {} a -> s {endTime = a} :: GetMetricData) Core.. Core._Time

-- | The queues, up to 100, or channels, to use to filter the metrics
-- returned. Metric data is retrieved only for the resources associated
-- with the queues or channels included in the filter. You can include both
-- queue IDs and queue ARNs in the same request. VOICE, CHAT, and TASK
-- channels are supported.
getMetricData_filters :: Lens.Lens' GetMetricData Filters
getMetricData_filters = Lens.lens (\GetMetricData' {filters} -> filters) (\s@GetMetricData' {} a -> s {filters = a} :: GetMetricData)

-- | The metrics to retrieve. Specify the name, unit, and statistic for each
-- metric. The following historical metrics are available. For a
-- description of each metric, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/historical-metrics-definitions.html Historical Metrics Definitions>
-- in the /Amazon Connect Administrator Guide/.
--
-- [ABANDON_TIME]
--     Unit: SECONDS
--
--     Statistic: AVG
--
-- [AFTER_CONTACT_WORK_TIME]
--     Unit: SECONDS
--
--     Statistic: AVG
--
-- [API_CONTACTS_HANDLED]
--     Unit: COUNT
--
--     Statistic: SUM
--
-- [CALLBACK_CONTACTS_HANDLED]
--     Unit: COUNT
--
--     Statistic: SUM
--
-- [CONTACTS_ABANDONED]
--     Unit: COUNT
--
--     Statistic: SUM
--
-- [CONTACTS_AGENT_HUNG_UP_FIRST]
--     Unit: COUNT
--
--     Statistic: SUM
--
-- [CONTACTS_CONSULTED]
--     Unit: COUNT
--
--     Statistic: SUM
--
-- [CONTACTS_HANDLED]
--     Unit: COUNT
--
--     Statistic: SUM
--
-- [CONTACTS_HANDLED_INCOMING]
--     Unit: COUNT
--
--     Statistic: SUM
--
-- [CONTACTS_HANDLED_OUTBOUND]
--     Unit: COUNT
--
--     Statistic: SUM
--
-- [CONTACTS_HOLD_ABANDONS]
--     Unit: COUNT
--
--     Statistic: SUM
--
-- [CONTACTS_MISSED]
--     Unit: COUNT
--
--     Statistic: SUM
--
-- [CONTACTS_QUEUED]
--     Unit: COUNT
--
--     Statistic: SUM
--
-- [CONTACTS_TRANSFERRED_IN]
--     Unit: COUNT
--
--     Statistic: SUM
--
-- [CONTACTS_TRANSFERRED_IN_FROM_QUEUE]
--     Unit: COUNT
--
--     Statistic: SUM
--
-- [CONTACTS_TRANSFERRED_OUT]
--     Unit: COUNT
--
--     Statistic: SUM
--
-- [CONTACTS_TRANSFERRED_OUT_FROM_QUEUE]
--     Unit: COUNT
--
--     Statistic: SUM
--
-- [HANDLE_TIME]
--     Unit: SECONDS
--
--     Statistic: AVG
--
-- [HOLD_TIME]
--     Unit: SECONDS
--
--     Statistic: AVG
--
-- [INTERACTION_AND_HOLD_TIME]
--     Unit: SECONDS
--
--     Statistic: AVG
--
-- [INTERACTION_TIME]
--     Unit: SECONDS
--
--     Statistic: AVG
--
-- [OCCUPANCY]
--     Unit: PERCENT
--
--     Statistic: AVG
--
-- [QUEUE_ANSWER_TIME]
--     Unit: SECONDS
--
--     Statistic: AVG
--
-- [QUEUED_TIME]
--     Unit: SECONDS
--
--     Statistic: MAX
--
-- [SERVICE_LEVEL]
--     Unit: PERCENT
--
--     Statistic: AVG
--
--     Threshold: Only \"Less than\" comparisons are supported, with the
--     following service level thresholds: 15, 20, 25, 30, 45, 60, 90, 120,
--     180, 240, 300, 600
getMetricData_historicalMetrics :: Lens.Lens' GetMetricData [HistoricalMetric]
getMetricData_historicalMetrics = Lens.lens (\GetMetricData' {historicalMetrics} -> historicalMetrics) (\s@GetMetricData' {} a -> s {historicalMetrics = a} :: GetMetricData) Core.. Lens._Coerce

instance Core.AWSPager GetMetricData where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getMetricDataResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getMetricDataResponse_metricResults
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getMetricData_nextToken
          Lens..~ rs
          Lens.^? getMetricDataResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest GetMetricData where
  type
    AWSResponse GetMetricData =
      GetMetricDataResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMetricDataResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "MetricResults" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetMetricData

instance Core.NFData GetMetricData

instance Core.ToHeaders GetMetricData where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetMetricData where
  toJSON GetMetricData' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("Groupings" Core..=) Core.<$> groupings,
            ("MaxResults" Core..=) Core.<$> maxResults,
            Core.Just ("StartTime" Core..= startTime),
            Core.Just ("EndTime" Core..= endTime),
            Core.Just ("Filters" Core..= filters),
            Core.Just
              ("HistoricalMetrics" Core..= historicalMetrics)
          ]
      )

instance Core.ToPath GetMetricData where
  toPath GetMetricData' {..} =
    Core.mconcat
      ["/metrics/historical/", Core.toBS instanceId]

instance Core.ToQuery GetMetricData where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetMetricDataResponse' smart constructor.
data GetMetricDataResponse = GetMetricDataResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    --
    -- The token expires after 5 minutes from the time it is created.
    -- Subsequent requests that use the token must use the same request
    -- parameters as the request that generated the token.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the historical metrics.
    --
    -- If no grouping is specified, a summary of metric data is returned.
    metricResults :: Core.Maybe [HistoricalMetricResult],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetMetricDataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getMetricDataResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- The token expires after 5 minutes from the time it is created.
-- Subsequent requests that use the token must use the same request
-- parameters as the request that generated the token.
--
-- 'metricResults', 'getMetricDataResponse_metricResults' - Information about the historical metrics.
--
-- If no grouping is specified, a summary of metric data is returned.
--
-- 'httpStatus', 'getMetricDataResponse_httpStatus' - The response's http status code.
newGetMetricDataResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetMetricDataResponse
newGetMetricDataResponse pHttpStatus_ =
  GetMetricDataResponse'
    { nextToken = Core.Nothing,
      metricResults = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are additional results, this is the token for the next set of
-- results.
--
-- The token expires after 5 minutes from the time it is created.
-- Subsequent requests that use the token must use the same request
-- parameters as the request that generated the token.
getMetricDataResponse_nextToken :: Lens.Lens' GetMetricDataResponse (Core.Maybe Core.Text)
getMetricDataResponse_nextToken = Lens.lens (\GetMetricDataResponse' {nextToken} -> nextToken) (\s@GetMetricDataResponse' {} a -> s {nextToken = a} :: GetMetricDataResponse)

-- | Information about the historical metrics.
--
-- If no grouping is specified, a summary of metric data is returned.
getMetricDataResponse_metricResults :: Lens.Lens' GetMetricDataResponse (Core.Maybe [HistoricalMetricResult])
getMetricDataResponse_metricResults = Lens.lens (\GetMetricDataResponse' {metricResults} -> metricResults) (\s@GetMetricDataResponse' {} a -> s {metricResults = a} :: GetMetricDataResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getMetricDataResponse_httpStatus :: Lens.Lens' GetMetricDataResponse Core.Int
getMetricDataResponse_httpStatus = Lens.lens (\GetMetricDataResponse' {httpStatus} -> httpStatus) (\s@GetMetricDataResponse' {} a -> s {httpStatus = a} :: GetMetricDataResponse)

instance Core.NFData GetMetricDataResponse
