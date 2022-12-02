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
-- Module      : Amazonka.Connect.GetMetricData
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.Connect.GetMetricData
  ( -- * Creating a Request
    GetMetricData (..),
    newGetMetricData,

    -- * Request Lenses
    getMetricData_groupings,
    getMetricData_nextToken,
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

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetMetricData' smart constructor.
data GetMetricData = GetMetricData'
  { -- | The grouping applied to the metrics returned. For example, when results
    -- are grouped by queue, the metrics returned are grouped by queue. The
    -- values returned apply to the metrics for each queue rather than
    -- aggregated for all queues.
    --
    -- If no grouping is specified, a summary of metrics for all queues is
    -- returned.
    groupings :: Prelude.Maybe [Grouping],
    -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The timestamp, in UNIX Epoch time format, at which to start the
    -- reporting interval for the retrieval of historical metrics data. The
    -- time must be specified using a multiple of 5 minutes, such as 10:05,
    -- 10:10, 10:15.
    --
    -- The start time cannot be earlier than 24 hours before the time of the
    -- request. Historical metrics are available only for 24 hours.
    startTime :: Data.POSIX,
    -- | The timestamp, in UNIX Epoch time format, at which to end the reporting
    -- interval for the retrieval of historical metrics data. The time must be
    -- specified using an interval of 5 minutes, such as 11:00, 11:05, 11:10,
    -- and must be later than the start time timestamp.
    --
    -- The time range between the start and end time must be less than 24
    -- hours.
    endTime :: Data.POSIX,
    -- | The queues, up to 100, or channels, to use to filter the metrics
    -- returned. Metric data is retrieved only for the resources associated
    -- with the queues or channels included in the filter. You can include both
    -- queue IDs and queue ARNs in the same request. VOICE, CHAT, and TASK
    -- channels are supported.
    --
    -- To filter by @Queues@, enter the queue ID\/ARN, not the name of the
    -- queue.
    filters :: Filters,
    -- | The metrics to retrieve. Specify the name, unit, and statistic for each
    -- metric. The following historical metrics are available. For a
    -- description of each metric, see
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/historical-metrics-definitions.html Historical Metrics Definitions>
    -- in the /Amazon Connect Administrator Guide/.
    --
    -- This API does not support a contacts incoming metric (there\'s no
    -- CONTACTS_INCOMING metric missing from the documented list).
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
    --     You can include up to 20 SERVICE_LEVEL metrics in a request.
    --
    --     Unit: PERCENT
    --
    --     Statistic: AVG
    --
    --     Threshold: For @ThresholdValue@, enter any whole number from 1 to
    --     604800 (inclusive), in seconds. For @Comparison@, you must enter
    --     @LT@ (for \"Less than\").
    historicalMetrics :: [HistoricalMetric]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMetricData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupings', 'getMetricData_groupings' - The grouping applied to the metrics returned. For example, when results
-- are grouped by queue, the metrics returned are grouped by queue. The
-- values returned apply to the metrics for each queue rather than
-- aggregated for all queues.
--
-- If no grouping is specified, a summary of metrics for all queues is
-- returned.
--
-- 'nextToken', 'getMetricData_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'maxResults', 'getMetricData_maxResults' - The maximum number of results to return per page.
--
-- 'instanceId', 'getMetricData_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
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
-- To filter by @Queues@, enter the queue ID\/ARN, not the name of the
-- queue.
--
-- 'historicalMetrics', 'getMetricData_historicalMetrics' - The metrics to retrieve. Specify the name, unit, and statistic for each
-- metric. The following historical metrics are available. For a
-- description of each metric, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/historical-metrics-definitions.html Historical Metrics Definitions>
-- in the /Amazon Connect Administrator Guide/.
--
-- This API does not support a contacts incoming metric (there\'s no
-- CONTACTS_INCOMING metric missing from the documented list).
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
--     You can include up to 20 SERVICE_LEVEL metrics in a request.
--
--     Unit: PERCENT
--
--     Statistic: AVG
--
--     Threshold: For @ThresholdValue@, enter any whole number from 1 to
--     604800 (inclusive), in seconds. For @Comparison@, you must enter
--     @LT@ (for \"Less than\").
newGetMetricData ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'startTime'
  Prelude.UTCTime ->
  -- | 'endTime'
  Prelude.UTCTime ->
  -- | 'filters'
  Filters ->
  GetMetricData
newGetMetricData
  pInstanceId_
  pStartTime_
  pEndTime_
  pFilters_ =
    GetMetricData'
      { groupings = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        maxResults = Prelude.Nothing,
        instanceId = pInstanceId_,
        startTime = Data._Time Lens.# pStartTime_,
        endTime = Data._Time Lens.# pEndTime_,
        filters = pFilters_,
        historicalMetrics = Prelude.mempty
      }

-- | The grouping applied to the metrics returned. For example, when results
-- are grouped by queue, the metrics returned are grouped by queue. The
-- values returned apply to the metrics for each queue rather than
-- aggregated for all queues.
--
-- If no grouping is specified, a summary of metrics for all queues is
-- returned.
getMetricData_groupings :: Lens.Lens' GetMetricData (Prelude.Maybe [Grouping])
getMetricData_groupings = Lens.lens (\GetMetricData' {groupings} -> groupings) (\s@GetMetricData' {} a -> s {groupings = a} :: GetMetricData) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
getMetricData_nextToken :: Lens.Lens' GetMetricData (Prelude.Maybe Prelude.Text)
getMetricData_nextToken = Lens.lens (\GetMetricData' {nextToken} -> nextToken) (\s@GetMetricData' {} a -> s {nextToken = a} :: GetMetricData)

-- | The maximum number of results to return per page.
getMetricData_maxResults :: Lens.Lens' GetMetricData (Prelude.Maybe Prelude.Natural)
getMetricData_maxResults = Lens.lens (\GetMetricData' {maxResults} -> maxResults) (\s@GetMetricData' {} a -> s {maxResults = a} :: GetMetricData)

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
getMetricData_instanceId :: Lens.Lens' GetMetricData Prelude.Text
getMetricData_instanceId = Lens.lens (\GetMetricData' {instanceId} -> instanceId) (\s@GetMetricData' {} a -> s {instanceId = a} :: GetMetricData)

-- | The timestamp, in UNIX Epoch time format, at which to start the
-- reporting interval for the retrieval of historical metrics data. The
-- time must be specified using a multiple of 5 minutes, such as 10:05,
-- 10:10, 10:15.
--
-- The start time cannot be earlier than 24 hours before the time of the
-- request. Historical metrics are available only for 24 hours.
getMetricData_startTime :: Lens.Lens' GetMetricData Prelude.UTCTime
getMetricData_startTime = Lens.lens (\GetMetricData' {startTime} -> startTime) (\s@GetMetricData' {} a -> s {startTime = a} :: GetMetricData) Prelude.. Data._Time

-- | The timestamp, in UNIX Epoch time format, at which to end the reporting
-- interval for the retrieval of historical metrics data. The time must be
-- specified using an interval of 5 minutes, such as 11:00, 11:05, 11:10,
-- and must be later than the start time timestamp.
--
-- The time range between the start and end time must be less than 24
-- hours.
getMetricData_endTime :: Lens.Lens' GetMetricData Prelude.UTCTime
getMetricData_endTime = Lens.lens (\GetMetricData' {endTime} -> endTime) (\s@GetMetricData' {} a -> s {endTime = a} :: GetMetricData) Prelude.. Data._Time

-- | The queues, up to 100, or channels, to use to filter the metrics
-- returned. Metric data is retrieved only for the resources associated
-- with the queues or channels included in the filter. You can include both
-- queue IDs and queue ARNs in the same request. VOICE, CHAT, and TASK
-- channels are supported.
--
-- To filter by @Queues@, enter the queue ID\/ARN, not the name of the
-- queue.
getMetricData_filters :: Lens.Lens' GetMetricData Filters
getMetricData_filters = Lens.lens (\GetMetricData' {filters} -> filters) (\s@GetMetricData' {} a -> s {filters = a} :: GetMetricData)

-- | The metrics to retrieve. Specify the name, unit, and statistic for each
-- metric. The following historical metrics are available. For a
-- description of each metric, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/historical-metrics-definitions.html Historical Metrics Definitions>
-- in the /Amazon Connect Administrator Guide/.
--
-- This API does not support a contacts incoming metric (there\'s no
-- CONTACTS_INCOMING metric missing from the documented list).
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
--     You can include up to 20 SERVICE_LEVEL metrics in a request.
--
--     Unit: PERCENT
--
--     Statistic: AVG
--
--     Threshold: For @ThresholdValue@, enter any whole number from 1 to
--     604800 (inclusive), in seconds. For @Comparison@, you must enter
--     @LT@ (for \"Less than\").
getMetricData_historicalMetrics :: Lens.Lens' GetMetricData [HistoricalMetric]
getMetricData_historicalMetrics = Lens.lens (\GetMetricData' {historicalMetrics} -> historicalMetrics) (\s@GetMetricData' {} a -> s {historicalMetrics = a} :: GetMetricData) Prelude.. Lens.coerced

instance Core.AWSPager GetMetricData where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getMetricDataResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getMetricDataResponse_metricResults
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getMetricData_nextToken
          Lens..~ rs
          Lens.^? getMetricDataResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest GetMetricData where
  type
    AWSResponse GetMetricData =
      GetMetricDataResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMetricDataResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "MetricResults" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetMetricData where
  hashWithSalt _salt GetMetricData' {..} =
    _salt `Prelude.hashWithSalt` groupings
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` historicalMetrics

instance Prelude.NFData GetMetricData where
  rnf GetMetricData' {..} =
    Prelude.rnf groupings
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf historicalMetrics

instance Data.ToHeaders GetMetricData where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetMetricData where
  toJSON GetMetricData' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Groupings" Data..=) Prelude.<$> groupings,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            Prelude.Just ("StartTime" Data..= startTime),
            Prelude.Just ("EndTime" Data..= endTime),
            Prelude.Just ("Filters" Data..= filters),
            Prelude.Just
              ("HistoricalMetrics" Data..= historicalMetrics)
          ]
      )

instance Data.ToPath GetMetricData where
  toPath GetMetricData' {..} =
    Prelude.mconcat
      ["/metrics/historical/", Data.toBS instanceId]

instance Data.ToQuery GetMetricData where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetMetricDataResponse' smart constructor.
data GetMetricDataResponse = GetMetricDataResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    --
    -- The token expires after 5 minutes from the time it is created.
    -- Subsequent requests that use the token must use the same request
    -- parameters as the request that generated the token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the historical metrics.
    --
    -- If no grouping is specified, a summary of metric data is returned.
    metricResults :: Prelude.Maybe [HistoricalMetricResult],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetMetricDataResponse
newGetMetricDataResponse pHttpStatus_ =
  GetMetricDataResponse'
    { nextToken = Prelude.Nothing,
      metricResults = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are additional results, this is the token for the next set of
-- results.
--
-- The token expires after 5 minutes from the time it is created.
-- Subsequent requests that use the token must use the same request
-- parameters as the request that generated the token.
getMetricDataResponse_nextToken :: Lens.Lens' GetMetricDataResponse (Prelude.Maybe Prelude.Text)
getMetricDataResponse_nextToken = Lens.lens (\GetMetricDataResponse' {nextToken} -> nextToken) (\s@GetMetricDataResponse' {} a -> s {nextToken = a} :: GetMetricDataResponse)

-- | Information about the historical metrics.
--
-- If no grouping is specified, a summary of metric data is returned.
getMetricDataResponse_metricResults :: Lens.Lens' GetMetricDataResponse (Prelude.Maybe [HistoricalMetricResult])
getMetricDataResponse_metricResults = Lens.lens (\GetMetricDataResponse' {metricResults} -> metricResults) (\s@GetMetricDataResponse' {} a -> s {metricResults = a} :: GetMetricDataResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getMetricDataResponse_httpStatus :: Lens.Lens' GetMetricDataResponse Prelude.Int
getMetricDataResponse_httpStatus = Lens.lens (\GetMetricDataResponse' {httpStatus} -> httpStatus) (\s@GetMetricDataResponse' {} a -> s {httpStatus = a} :: GetMetricDataResponse)

instance Prelude.NFData GetMetricDataResponse where
  rnf GetMetricDataResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf metricResults
      `Prelude.seq` Prelude.rnf httpStatus
