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
-- Module      : Amazonka.Connect.GetCurrentMetricData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the real-time metric data from the specified Amazon Connect
-- instance.
--
-- For a description of each metric, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html Real-time Metrics Definitions>
-- in the /Amazon Connect Administrator Guide/.
module Amazonka.Connect.GetCurrentMetricData
  ( -- * Creating a Request
    GetCurrentMetricData (..),
    newGetCurrentMetricData,

    -- * Request Lenses
    getCurrentMetricData_groupings,
    getCurrentMetricData_nextToken,
    getCurrentMetricData_maxResults,
    getCurrentMetricData_instanceId,
    getCurrentMetricData_filters,
    getCurrentMetricData_currentMetrics,

    -- * Destructuring the Response
    GetCurrentMetricDataResponse (..),
    newGetCurrentMetricDataResponse,

    -- * Response Lenses
    getCurrentMetricDataResponse_nextToken,
    getCurrentMetricDataResponse_dataSnapshotTime,
    getCurrentMetricDataResponse_metricResults,
    getCurrentMetricDataResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCurrentMetricData' smart constructor.
data GetCurrentMetricData = GetCurrentMetricData'
  { -- | The grouping applied to the metrics returned. For example, when grouped
    -- by @QUEUE@, the metrics returned apply to each queue rather than
    -- aggregated for all queues.
    --
    -- -   If you group by @CHANNEL@, you should include a Channels filter.
    --     VOICE, CHAT, and TASK channels are supported.
    --
    -- -   If you group by @ROUTING_PROFILE@, you must include either a queue
    --     or routing profile filter.
    --
    -- -   If no @Grouping@ is included in the request, a summary of metrics is
    --     returned.
    groupings :: Prelude.Maybe [Grouping],
    -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    --
    -- The token expires after 5 minutes from the time it is created.
    -- Subsequent requests that use the token must use the same request
    -- parameters as the request that generated the token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The queues, up to 100, or channels, to use to filter the metrics
    -- returned. Metric data is retrieved only for the resources associated
    -- with the queues or channels included in the filter. You can include both
    -- queue IDs and queue ARNs in the same request. VOICE, CHAT, and TASK
    -- channels are supported.
    filters :: Filters,
    -- | The metrics to retrieve. Specify the name and unit for each metric. The
    -- following metrics are available. For a description of all the metrics,
    -- see
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html Real-time Metrics Definitions>
    -- in the /Amazon Connect Administrator Guide/.
    --
    -- [AGENTS_AFTER_CONTACT_WORK]
    --     Unit: COUNT
    --
    --     Name in real-time metrics report:
    --     <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#aftercallwork-real-time ACW>
    --
    -- [AGENTS_AVAILABLE]
    --     Unit: COUNT
    --
    --     Name in real-time metrics report:
    --     <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#available-real-time Available>
    --
    -- [AGENTS_ERROR]
    --     Unit: COUNT
    --
    --     Name in real-time metrics report:
    --     <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#error-real-time Error>
    --
    -- [AGENTS_NON_PRODUCTIVE]
    --     Unit: COUNT
    --
    --     Name in real-time metrics report:
    --     <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#non-productive-time-real-time NPT (Non-Productive Time)>
    --
    -- [AGENTS_ON_CALL]
    --     Unit: COUNT
    --
    --     Name in real-time metrics report:
    --     <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#on-call-real-time On contact>
    --
    -- [AGENTS_ON_CONTACT]
    --     Unit: COUNT
    --
    --     Name in real-time metrics report:
    --     <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#on-call-real-time On contact>
    --
    -- [AGENTS_ONLINE]
    --     Unit: COUNT
    --
    --     Name in real-time metrics report:
    --     <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#online-real-time Online>
    --
    -- [AGENTS_STAFFED]
    --     Unit: COUNT
    --
    --     Name in real-time metrics report:
    --     <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#staffed-real-time Staffed>
    --
    -- [CONTACTS_IN_QUEUE]
    --     Unit: COUNT
    --
    --     Name in real-time metrics report:
    --     <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#in-queue-real-time In queue>
    --
    -- [CONTACTS_SCHEDULED]
    --     Unit: COUNT
    --
    --     Name in real-time metrics report:
    --     <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#scheduled-real-time Scheduled>
    --
    -- [OLDEST_CONTACT_AGE]
    --     Unit: SECONDS
    --
    --     When you use groupings, Unit says SECONDS and the Value is returned
    --     in SECONDS.
    --
    --     When you do not use groupings, Unit says SECONDS but the Value is
    --     returned in MILLISECONDS. For example, if you get a response like
    --     this:
    --
    --     @{ \"Metric\": { \"Name\": \"OLDEST_CONTACT_AGE\", \"Unit\": \"SECONDS\" }, \"Value\": 24113.0 @}
    --
    --     The actual OLDEST_CONTACT_AGE is 24 seconds.
    --
    --     Name in real-time metrics report:
    --     <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#oldest-real-time Oldest>
    --
    -- [SLOTS_ACTIVE]
    --     Unit: COUNT
    --
    --     Name in real-time metrics report:
    --     <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#active-real-time Active>
    --
    -- [SLOTS_AVAILABLE]
    --     Unit: COUNT
    --
    --     Name in real-time metrics report:
    --     <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#availability-real-time Availability>
    currentMetrics :: [CurrentMetric]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCurrentMetricData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupings', 'getCurrentMetricData_groupings' - The grouping applied to the metrics returned. For example, when grouped
-- by @QUEUE@, the metrics returned apply to each queue rather than
-- aggregated for all queues.
--
-- -   If you group by @CHANNEL@, you should include a Channels filter.
--     VOICE, CHAT, and TASK channels are supported.
--
-- -   If you group by @ROUTING_PROFILE@, you must include either a queue
--     or routing profile filter.
--
-- -   If no @Grouping@ is included in the request, a summary of metrics is
--     returned.
--
-- 'nextToken', 'getCurrentMetricData_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- The token expires after 5 minutes from the time it is created.
-- Subsequent requests that use the token must use the same request
-- parameters as the request that generated the token.
--
-- 'maxResults', 'getCurrentMetricData_maxResults' - The maximum number of results to return per page.
--
-- 'instanceId', 'getCurrentMetricData_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'filters', 'getCurrentMetricData_filters' - The queues, up to 100, or channels, to use to filter the metrics
-- returned. Metric data is retrieved only for the resources associated
-- with the queues or channels included in the filter. You can include both
-- queue IDs and queue ARNs in the same request. VOICE, CHAT, and TASK
-- channels are supported.
--
-- 'currentMetrics', 'getCurrentMetricData_currentMetrics' - The metrics to retrieve. Specify the name and unit for each metric. The
-- following metrics are available. For a description of all the metrics,
-- see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html Real-time Metrics Definitions>
-- in the /Amazon Connect Administrator Guide/.
--
-- [AGENTS_AFTER_CONTACT_WORK]
--     Unit: COUNT
--
--     Name in real-time metrics report:
--     <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#aftercallwork-real-time ACW>
--
-- [AGENTS_AVAILABLE]
--     Unit: COUNT
--
--     Name in real-time metrics report:
--     <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#available-real-time Available>
--
-- [AGENTS_ERROR]
--     Unit: COUNT
--
--     Name in real-time metrics report:
--     <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#error-real-time Error>
--
-- [AGENTS_NON_PRODUCTIVE]
--     Unit: COUNT
--
--     Name in real-time metrics report:
--     <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#non-productive-time-real-time NPT (Non-Productive Time)>
--
-- [AGENTS_ON_CALL]
--     Unit: COUNT
--
--     Name in real-time metrics report:
--     <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#on-call-real-time On contact>
--
-- [AGENTS_ON_CONTACT]
--     Unit: COUNT
--
--     Name in real-time metrics report:
--     <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#on-call-real-time On contact>
--
-- [AGENTS_ONLINE]
--     Unit: COUNT
--
--     Name in real-time metrics report:
--     <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#online-real-time Online>
--
-- [AGENTS_STAFFED]
--     Unit: COUNT
--
--     Name in real-time metrics report:
--     <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#staffed-real-time Staffed>
--
-- [CONTACTS_IN_QUEUE]
--     Unit: COUNT
--
--     Name in real-time metrics report:
--     <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#in-queue-real-time In queue>
--
-- [CONTACTS_SCHEDULED]
--     Unit: COUNT
--
--     Name in real-time metrics report:
--     <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#scheduled-real-time Scheduled>
--
-- [OLDEST_CONTACT_AGE]
--     Unit: SECONDS
--
--     When you use groupings, Unit says SECONDS and the Value is returned
--     in SECONDS.
--
--     When you do not use groupings, Unit says SECONDS but the Value is
--     returned in MILLISECONDS. For example, if you get a response like
--     this:
--
--     @{ \"Metric\": { \"Name\": \"OLDEST_CONTACT_AGE\", \"Unit\": \"SECONDS\" }, \"Value\": 24113.0 @}
--
--     The actual OLDEST_CONTACT_AGE is 24 seconds.
--
--     Name in real-time metrics report:
--     <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#oldest-real-time Oldest>
--
-- [SLOTS_ACTIVE]
--     Unit: COUNT
--
--     Name in real-time metrics report:
--     <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#active-real-time Active>
--
-- [SLOTS_AVAILABLE]
--     Unit: COUNT
--
--     Name in real-time metrics report:
--     <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#availability-real-time Availability>
newGetCurrentMetricData ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'filters'
  Filters ->
  GetCurrentMetricData
newGetCurrentMetricData pInstanceId_ pFilters_ =
  GetCurrentMetricData'
    { groupings = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      instanceId = pInstanceId_,
      filters = pFilters_,
      currentMetrics = Prelude.mempty
    }

-- | The grouping applied to the metrics returned. For example, when grouped
-- by @QUEUE@, the metrics returned apply to each queue rather than
-- aggregated for all queues.
--
-- -   If you group by @CHANNEL@, you should include a Channels filter.
--     VOICE, CHAT, and TASK channels are supported.
--
-- -   If you group by @ROUTING_PROFILE@, you must include either a queue
--     or routing profile filter.
--
-- -   If no @Grouping@ is included in the request, a summary of metrics is
--     returned.
getCurrentMetricData_groupings :: Lens.Lens' GetCurrentMetricData (Prelude.Maybe [Grouping])
getCurrentMetricData_groupings = Lens.lens (\GetCurrentMetricData' {groupings} -> groupings) (\s@GetCurrentMetricData' {} a -> s {groupings = a} :: GetCurrentMetricData) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- The token expires after 5 minutes from the time it is created.
-- Subsequent requests that use the token must use the same request
-- parameters as the request that generated the token.
getCurrentMetricData_nextToken :: Lens.Lens' GetCurrentMetricData (Prelude.Maybe Prelude.Text)
getCurrentMetricData_nextToken = Lens.lens (\GetCurrentMetricData' {nextToken} -> nextToken) (\s@GetCurrentMetricData' {} a -> s {nextToken = a} :: GetCurrentMetricData)

-- | The maximum number of results to return per page.
getCurrentMetricData_maxResults :: Lens.Lens' GetCurrentMetricData (Prelude.Maybe Prelude.Natural)
getCurrentMetricData_maxResults = Lens.lens (\GetCurrentMetricData' {maxResults} -> maxResults) (\s@GetCurrentMetricData' {} a -> s {maxResults = a} :: GetCurrentMetricData)

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
getCurrentMetricData_instanceId :: Lens.Lens' GetCurrentMetricData Prelude.Text
getCurrentMetricData_instanceId = Lens.lens (\GetCurrentMetricData' {instanceId} -> instanceId) (\s@GetCurrentMetricData' {} a -> s {instanceId = a} :: GetCurrentMetricData)

-- | The queues, up to 100, or channels, to use to filter the metrics
-- returned. Metric data is retrieved only for the resources associated
-- with the queues or channels included in the filter. You can include both
-- queue IDs and queue ARNs in the same request. VOICE, CHAT, and TASK
-- channels are supported.
getCurrentMetricData_filters :: Lens.Lens' GetCurrentMetricData Filters
getCurrentMetricData_filters = Lens.lens (\GetCurrentMetricData' {filters} -> filters) (\s@GetCurrentMetricData' {} a -> s {filters = a} :: GetCurrentMetricData)

-- | The metrics to retrieve. Specify the name and unit for each metric. The
-- following metrics are available. For a description of all the metrics,
-- see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html Real-time Metrics Definitions>
-- in the /Amazon Connect Administrator Guide/.
--
-- [AGENTS_AFTER_CONTACT_WORK]
--     Unit: COUNT
--
--     Name in real-time metrics report:
--     <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#aftercallwork-real-time ACW>
--
-- [AGENTS_AVAILABLE]
--     Unit: COUNT
--
--     Name in real-time metrics report:
--     <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#available-real-time Available>
--
-- [AGENTS_ERROR]
--     Unit: COUNT
--
--     Name in real-time metrics report:
--     <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#error-real-time Error>
--
-- [AGENTS_NON_PRODUCTIVE]
--     Unit: COUNT
--
--     Name in real-time metrics report:
--     <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#non-productive-time-real-time NPT (Non-Productive Time)>
--
-- [AGENTS_ON_CALL]
--     Unit: COUNT
--
--     Name in real-time metrics report:
--     <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#on-call-real-time On contact>
--
-- [AGENTS_ON_CONTACT]
--     Unit: COUNT
--
--     Name in real-time metrics report:
--     <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#on-call-real-time On contact>
--
-- [AGENTS_ONLINE]
--     Unit: COUNT
--
--     Name in real-time metrics report:
--     <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#online-real-time Online>
--
-- [AGENTS_STAFFED]
--     Unit: COUNT
--
--     Name in real-time metrics report:
--     <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#staffed-real-time Staffed>
--
-- [CONTACTS_IN_QUEUE]
--     Unit: COUNT
--
--     Name in real-time metrics report:
--     <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#in-queue-real-time In queue>
--
-- [CONTACTS_SCHEDULED]
--     Unit: COUNT
--
--     Name in real-time metrics report:
--     <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#scheduled-real-time Scheduled>
--
-- [OLDEST_CONTACT_AGE]
--     Unit: SECONDS
--
--     When you use groupings, Unit says SECONDS and the Value is returned
--     in SECONDS.
--
--     When you do not use groupings, Unit says SECONDS but the Value is
--     returned in MILLISECONDS. For example, if you get a response like
--     this:
--
--     @{ \"Metric\": { \"Name\": \"OLDEST_CONTACT_AGE\", \"Unit\": \"SECONDS\" }, \"Value\": 24113.0 @}
--
--     The actual OLDEST_CONTACT_AGE is 24 seconds.
--
--     Name in real-time metrics report:
--     <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#oldest-real-time Oldest>
--
-- [SLOTS_ACTIVE]
--     Unit: COUNT
--
--     Name in real-time metrics report:
--     <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#active-real-time Active>
--
-- [SLOTS_AVAILABLE]
--     Unit: COUNT
--
--     Name in real-time metrics report:
--     <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html#availability-real-time Availability>
getCurrentMetricData_currentMetrics :: Lens.Lens' GetCurrentMetricData [CurrentMetric]
getCurrentMetricData_currentMetrics = Lens.lens (\GetCurrentMetricData' {currentMetrics} -> currentMetrics) (\s@GetCurrentMetricData' {} a -> s {currentMetrics = a} :: GetCurrentMetricData) Prelude.. Lens.coerced

instance Core.AWSRequest GetCurrentMetricData where
  type
    AWSResponse GetCurrentMetricData =
      GetCurrentMetricDataResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCurrentMetricDataResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "DataSnapshotTime")
            Prelude.<*> (x Core..?> "MetricResults" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCurrentMetricData where
  hashWithSalt _salt GetCurrentMetricData' {..} =
    _salt `Prelude.hashWithSalt` groupings
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` currentMetrics

instance Prelude.NFData GetCurrentMetricData where
  rnf GetCurrentMetricData' {..} =
    Prelude.rnf groupings
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf currentMetrics

instance Core.ToHeaders GetCurrentMetricData where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetCurrentMetricData where
  toJSON GetCurrentMetricData' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Groupings" Core..=) Prelude.<$> groupings,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("Filters" Core..= filters),
            Prelude.Just
              ("CurrentMetrics" Core..= currentMetrics)
          ]
      )

instance Core.ToPath GetCurrentMetricData where
  toPath GetCurrentMetricData' {..} =
    Prelude.mconcat
      ["/metrics/current/", Core.toBS instanceId]

instance Core.ToQuery GetCurrentMetricData where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCurrentMetricDataResponse' smart constructor.
data GetCurrentMetricDataResponse = GetCurrentMetricDataResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    --
    -- The token expires after 5 minutes from the time it is created.
    -- Subsequent requests that use the token must use the same request
    -- parameters as the request that generated the token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The time at which the metrics were retrieved and cached for pagination.
    dataSnapshotTime :: Prelude.Maybe Core.POSIX,
    -- | Information about the real-time metrics.
    metricResults :: Prelude.Maybe [CurrentMetricResult],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCurrentMetricDataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getCurrentMetricDataResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- The token expires after 5 minutes from the time it is created.
-- Subsequent requests that use the token must use the same request
-- parameters as the request that generated the token.
--
-- 'dataSnapshotTime', 'getCurrentMetricDataResponse_dataSnapshotTime' - The time at which the metrics were retrieved and cached for pagination.
--
-- 'metricResults', 'getCurrentMetricDataResponse_metricResults' - Information about the real-time metrics.
--
-- 'httpStatus', 'getCurrentMetricDataResponse_httpStatus' - The response's http status code.
newGetCurrentMetricDataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCurrentMetricDataResponse
newGetCurrentMetricDataResponse pHttpStatus_ =
  GetCurrentMetricDataResponse'
    { nextToken =
        Prelude.Nothing,
      dataSnapshotTime = Prelude.Nothing,
      metricResults = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are additional results, this is the token for the next set of
-- results.
--
-- The token expires after 5 minutes from the time it is created.
-- Subsequent requests that use the token must use the same request
-- parameters as the request that generated the token.
getCurrentMetricDataResponse_nextToken :: Lens.Lens' GetCurrentMetricDataResponse (Prelude.Maybe Prelude.Text)
getCurrentMetricDataResponse_nextToken = Lens.lens (\GetCurrentMetricDataResponse' {nextToken} -> nextToken) (\s@GetCurrentMetricDataResponse' {} a -> s {nextToken = a} :: GetCurrentMetricDataResponse)

-- | The time at which the metrics were retrieved and cached for pagination.
getCurrentMetricDataResponse_dataSnapshotTime :: Lens.Lens' GetCurrentMetricDataResponse (Prelude.Maybe Prelude.UTCTime)
getCurrentMetricDataResponse_dataSnapshotTime = Lens.lens (\GetCurrentMetricDataResponse' {dataSnapshotTime} -> dataSnapshotTime) (\s@GetCurrentMetricDataResponse' {} a -> s {dataSnapshotTime = a} :: GetCurrentMetricDataResponse) Prelude.. Lens.mapping Core._Time

-- | Information about the real-time metrics.
getCurrentMetricDataResponse_metricResults :: Lens.Lens' GetCurrentMetricDataResponse (Prelude.Maybe [CurrentMetricResult])
getCurrentMetricDataResponse_metricResults = Lens.lens (\GetCurrentMetricDataResponse' {metricResults} -> metricResults) (\s@GetCurrentMetricDataResponse' {} a -> s {metricResults = a} :: GetCurrentMetricDataResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getCurrentMetricDataResponse_httpStatus :: Lens.Lens' GetCurrentMetricDataResponse Prelude.Int
getCurrentMetricDataResponse_httpStatus = Lens.lens (\GetCurrentMetricDataResponse' {httpStatus} -> httpStatus) (\s@GetCurrentMetricDataResponse' {} a -> s {httpStatus = a} :: GetCurrentMetricDataResponse)

instance Prelude.NFData GetCurrentMetricDataResponse where
  rnf GetCurrentMetricDataResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf dataSnapshotTime
      `Prelude.seq` Prelude.rnf metricResults
      `Prelude.seq` Prelude.rnf httpStatus
