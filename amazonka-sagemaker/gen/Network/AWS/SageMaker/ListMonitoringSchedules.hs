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
-- Module      : Network.AWS.SageMaker.ListMonitoringSchedules
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns list of all monitoring schedules.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListMonitoringSchedules
  ( -- * Creating a Request
    ListMonitoringSchedules (..),
    newListMonitoringSchedules,

    -- * Request Lenses
    listMonitoringSchedules_lastModifiedTimeBefore,
    listMonitoringSchedules_sortOrder,
    listMonitoringSchedules_nextToken,
    listMonitoringSchedules_endpointName,
    listMonitoringSchedules_nameContains,
    listMonitoringSchedules_monitoringJobDefinitionName,
    listMonitoringSchedules_maxResults,
    listMonitoringSchedules_creationTimeBefore,
    listMonitoringSchedules_lastModifiedTimeAfter,
    listMonitoringSchedules_sortBy,
    listMonitoringSchedules_statusEquals,
    listMonitoringSchedules_monitoringTypeEquals,
    listMonitoringSchedules_creationTimeAfter,

    -- * Destructuring the Response
    ListMonitoringSchedulesResponse (..),
    newListMonitoringSchedulesResponse,

    -- * Response Lenses
    listMonitoringSchedulesResponse_nextToken,
    listMonitoringSchedulesResponse_httpStatus,
    listMonitoringSchedulesResponse_monitoringScheduleSummaries,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListMonitoringSchedules' smart constructor.
data ListMonitoringSchedules = ListMonitoringSchedules'
  { -- | A filter that returns only monitoring schedules modified before a
    -- specified time.
    lastModifiedTimeBefore :: Core.Maybe Core.POSIX,
    -- | Whether to sort the results in @Ascending@ or @Descending@ order. The
    -- default is @Descending@.
    sortOrder :: Core.Maybe SortOrder,
    -- | The token returned if the response is truncated. To retrieve the next
    -- set of job executions, use it in the next request.
    nextToken :: Core.Maybe Core.Text,
    -- | Name of a specific endpoint to fetch schedules for.
    endpointName :: Core.Maybe Core.Text,
    -- | Filter for monitoring schedules whose name contains a specified string.
    nameContains :: Core.Maybe Core.Text,
    -- | Gets a list of the monitoring schedules for the specified monitoring job
    -- definition.
    monitoringJobDefinitionName :: Core.Maybe Core.Text,
    -- | The maximum number of jobs to return in the response. The default value
    -- is 10.
    maxResults :: Core.Maybe Core.Natural,
    -- | A filter that returns only monitoring schedules created before a
    -- specified time.
    creationTimeBefore :: Core.Maybe Core.POSIX,
    -- | A filter that returns only monitoring schedules modified after a
    -- specified time.
    lastModifiedTimeAfter :: Core.Maybe Core.POSIX,
    -- | Whether to sort results by @Status@, @CreationTime@, @ScheduledTime@
    -- field. The default is @CreationTime@.
    sortBy :: Core.Maybe MonitoringScheduleSortKey,
    -- | A filter that returns only monitoring schedules modified before a
    -- specified time.
    statusEquals :: Core.Maybe ScheduleStatus,
    -- | A filter that returns only the monitoring schedules for the specified
    -- monitoring type.
    monitoringTypeEquals :: Core.Maybe MonitoringType,
    -- | A filter that returns only monitoring schedules created after a
    -- specified time.
    creationTimeAfter :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListMonitoringSchedules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedTimeBefore', 'listMonitoringSchedules_lastModifiedTimeBefore' - A filter that returns only monitoring schedules modified before a
-- specified time.
--
-- 'sortOrder', 'listMonitoringSchedules_sortOrder' - Whether to sort the results in @Ascending@ or @Descending@ order. The
-- default is @Descending@.
--
-- 'nextToken', 'listMonitoringSchedules_nextToken' - The token returned if the response is truncated. To retrieve the next
-- set of job executions, use it in the next request.
--
-- 'endpointName', 'listMonitoringSchedules_endpointName' - Name of a specific endpoint to fetch schedules for.
--
-- 'nameContains', 'listMonitoringSchedules_nameContains' - Filter for monitoring schedules whose name contains a specified string.
--
-- 'monitoringJobDefinitionName', 'listMonitoringSchedules_monitoringJobDefinitionName' - Gets a list of the monitoring schedules for the specified monitoring job
-- definition.
--
-- 'maxResults', 'listMonitoringSchedules_maxResults' - The maximum number of jobs to return in the response. The default value
-- is 10.
--
-- 'creationTimeBefore', 'listMonitoringSchedules_creationTimeBefore' - A filter that returns only monitoring schedules created before a
-- specified time.
--
-- 'lastModifiedTimeAfter', 'listMonitoringSchedules_lastModifiedTimeAfter' - A filter that returns only monitoring schedules modified after a
-- specified time.
--
-- 'sortBy', 'listMonitoringSchedules_sortBy' - Whether to sort results by @Status@, @CreationTime@, @ScheduledTime@
-- field. The default is @CreationTime@.
--
-- 'statusEquals', 'listMonitoringSchedules_statusEquals' - A filter that returns only monitoring schedules modified before a
-- specified time.
--
-- 'monitoringTypeEquals', 'listMonitoringSchedules_monitoringTypeEquals' - A filter that returns only the monitoring schedules for the specified
-- monitoring type.
--
-- 'creationTimeAfter', 'listMonitoringSchedules_creationTimeAfter' - A filter that returns only monitoring schedules created after a
-- specified time.
newListMonitoringSchedules ::
  ListMonitoringSchedules
newListMonitoringSchedules =
  ListMonitoringSchedules'
    { lastModifiedTimeBefore =
        Core.Nothing,
      sortOrder = Core.Nothing,
      nextToken = Core.Nothing,
      endpointName = Core.Nothing,
      nameContains = Core.Nothing,
      monitoringJobDefinitionName = Core.Nothing,
      maxResults = Core.Nothing,
      creationTimeBefore = Core.Nothing,
      lastModifiedTimeAfter = Core.Nothing,
      sortBy = Core.Nothing,
      statusEquals = Core.Nothing,
      monitoringTypeEquals = Core.Nothing,
      creationTimeAfter = Core.Nothing
    }

-- | A filter that returns only monitoring schedules modified before a
-- specified time.
listMonitoringSchedules_lastModifiedTimeBefore :: Lens.Lens' ListMonitoringSchedules (Core.Maybe Core.UTCTime)
listMonitoringSchedules_lastModifiedTimeBefore = Lens.lens (\ListMonitoringSchedules' {lastModifiedTimeBefore} -> lastModifiedTimeBefore) (\s@ListMonitoringSchedules' {} a -> s {lastModifiedTimeBefore = a} :: ListMonitoringSchedules) Core.. Lens.mapping Core._Time

-- | Whether to sort the results in @Ascending@ or @Descending@ order. The
-- default is @Descending@.
listMonitoringSchedules_sortOrder :: Lens.Lens' ListMonitoringSchedules (Core.Maybe SortOrder)
listMonitoringSchedules_sortOrder = Lens.lens (\ListMonitoringSchedules' {sortOrder} -> sortOrder) (\s@ListMonitoringSchedules' {} a -> s {sortOrder = a} :: ListMonitoringSchedules)

-- | The token returned if the response is truncated. To retrieve the next
-- set of job executions, use it in the next request.
listMonitoringSchedules_nextToken :: Lens.Lens' ListMonitoringSchedules (Core.Maybe Core.Text)
listMonitoringSchedules_nextToken = Lens.lens (\ListMonitoringSchedules' {nextToken} -> nextToken) (\s@ListMonitoringSchedules' {} a -> s {nextToken = a} :: ListMonitoringSchedules)

-- | Name of a specific endpoint to fetch schedules for.
listMonitoringSchedules_endpointName :: Lens.Lens' ListMonitoringSchedules (Core.Maybe Core.Text)
listMonitoringSchedules_endpointName = Lens.lens (\ListMonitoringSchedules' {endpointName} -> endpointName) (\s@ListMonitoringSchedules' {} a -> s {endpointName = a} :: ListMonitoringSchedules)

-- | Filter for monitoring schedules whose name contains a specified string.
listMonitoringSchedules_nameContains :: Lens.Lens' ListMonitoringSchedules (Core.Maybe Core.Text)
listMonitoringSchedules_nameContains = Lens.lens (\ListMonitoringSchedules' {nameContains} -> nameContains) (\s@ListMonitoringSchedules' {} a -> s {nameContains = a} :: ListMonitoringSchedules)

-- | Gets a list of the monitoring schedules for the specified monitoring job
-- definition.
listMonitoringSchedules_monitoringJobDefinitionName :: Lens.Lens' ListMonitoringSchedules (Core.Maybe Core.Text)
listMonitoringSchedules_monitoringJobDefinitionName = Lens.lens (\ListMonitoringSchedules' {monitoringJobDefinitionName} -> monitoringJobDefinitionName) (\s@ListMonitoringSchedules' {} a -> s {monitoringJobDefinitionName = a} :: ListMonitoringSchedules)

-- | The maximum number of jobs to return in the response. The default value
-- is 10.
listMonitoringSchedules_maxResults :: Lens.Lens' ListMonitoringSchedules (Core.Maybe Core.Natural)
listMonitoringSchedules_maxResults = Lens.lens (\ListMonitoringSchedules' {maxResults} -> maxResults) (\s@ListMonitoringSchedules' {} a -> s {maxResults = a} :: ListMonitoringSchedules)

-- | A filter that returns only monitoring schedules created before a
-- specified time.
listMonitoringSchedules_creationTimeBefore :: Lens.Lens' ListMonitoringSchedules (Core.Maybe Core.UTCTime)
listMonitoringSchedules_creationTimeBefore = Lens.lens (\ListMonitoringSchedules' {creationTimeBefore} -> creationTimeBefore) (\s@ListMonitoringSchedules' {} a -> s {creationTimeBefore = a} :: ListMonitoringSchedules) Core.. Lens.mapping Core._Time

-- | A filter that returns only monitoring schedules modified after a
-- specified time.
listMonitoringSchedules_lastModifiedTimeAfter :: Lens.Lens' ListMonitoringSchedules (Core.Maybe Core.UTCTime)
listMonitoringSchedules_lastModifiedTimeAfter = Lens.lens (\ListMonitoringSchedules' {lastModifiedTimeAfter} -> lastModifiedTimeAfter) (\s@ListMonitoringSchedules' {} a -> s {lastModifiedTimeAfter = a} :: ListMonitoringSchedules) Core.. Lens.mapping Core._Time

-- | Whether to sort results by @Status@, @CreationTime@, @ScheduledTime@
-- field. The default is @CreationTime@.
listMonitoringSchedules_sortBy :: Lens.Lens' ListMonitoringSchedules (Core.Maybe MonitoringScheduleSortKey)
listMonitoringSchedules_sortBy = Lens.lens (\ListMonitoringSchedules' {sortBy} -> sortBy) (\s@ListMonitoringSchedules' {} a -> s {sortBy = a} :: ListMonitoringSchedules)

-- | A filter that returns only monitoring schedules modified before a
-- specified time.
listMonitoringSchedules_statusEquals :: Lens.Lens' ListMonitoringSchedules (Core.Maybe ScheduleStatus)
listMonitoringSchedules_statusEquals = Lens.lens (\ListMonitoringSchedules' {statusEquals} -> statusEquals) (\s@ListMonitoringSchedules' {} a -> s {statusEquals = a} :: ListMonitoringSchedules)

-- | A filter that returns only the monitoring schedules for the specified
-- monitoring type.
listMonitoringSchedules_monitoringTypeEquals :: Lens.Lens' ListMonitoringSchedules (Core.Maybe MonitoringType)
listMonitoringSchedules_monitoringTypeEquals = Lens.lens (\ListMonitoringSchedules' {monitoringTypeEquals} -> monitoringTypeEquals) (\s@ListMonitoringSchedules' {} a -> s {monitoringTypeEquals = a} :: ListMonitoringSchedules)

-- | A filter that returns only monitoring schedules created after a
-- specified time.
listMonitoringSchedules_creationTimeAfter :: Lens.Lens' ListMonitoringSchedules (Core.Maybe Core.UTCTime)
listMonitoringSchedules_creationTimeAfter = Lens.lens (\ListMonitoringSchedules' {creationTimeAfter} -> creationTimeAfter) (\s@ListMonitoringSchedules' {} a -> s {creationTimeAfter = a} :: ListMonitoringSchedules) Core.. Lens.mapping Core._Time

instance Core.AWSPager ListMonitoringSchedules where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listMonitoringSchedulesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^. listMonitoringSchedulesResponse_monitoringScheduleSummaries
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listMonitoringSchedules_nextToken
          Lens..~ rs
          Lens.^? listMonitoringSchedulesResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListMonitoringSchedules where
  type
    AWSResponse ListMonitoringSchedules =
      ListMonitoringSchedulesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMonitoringSchedulesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> ( x Core..?> "MonitoringScheduleSummaries"
                         Core..!@ Core.mempty
                     )
      )

instance Core.Hashable ListMonitoringSchedules

instance Core.NFData ListMonitoringSchedules

instance Core.ToHeaders ListMonitoringSchedules where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.ListMonitoringSchedules" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListMonitoringSchedules where
  toJSON ListMonitoringSchedules' {..} =
    Core.object
      ( Core.catMaybes
          [ ("LastModifiedTimeBefore" Core..=)
              Core.<$> lastModifiedTimeBefore,
            ("SortOrder" Core..=) Core.<$> sortOrder,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("EndpointName" Core..=) Core.<$> endpointName,
            ("NameContains" Core..=) Core.<$> nameContains,
            ("MonitoringJobDefinitionName" Core..=)
              Core.<$> monitoringJobDefinitionName,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("CreationTimeBefore" Core..=)
              Core.<$> creationTimeBefore,
            ("LastModifiedTimeAfter" Core..=)
              Core.<$> lastModifiedTimeAfter,
            ("SortBy" Core..=) Core.<$> sortBy,
            ("StatusEquals" Core..=) Core.<$> statusEquals,
            ("MonitoringTypeEquals" Core..=)
              Core.<$> monitoringTypeEquals,
            ("CreationTimeAfter" Core..=)
              Core.<$> creationTimeAfter
          ]
      )

instance Core.ToPath ListMonitoringSchedules where
  toPath = Core.const "/"

instance Core.ToQuery ListMonitoringSchedules where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListMonitoringSchedulesResponse' smart constructor.
data ListMonitoringSchedulesResponse = ListMonitoringSchedulesResponse'
  { -- | If the response is truncated, Amazon SageMaker returns this token. To
    -- retrieve the next set of jobs, use it in the subsequent request.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A JSON array in which each element is a summary for a monitoring
    -- schedule.
    monitoringScheduleSummaries :: [MonitoringScheduleSummary]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListMonitoringSchedulesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listMonitoringSchedulesResponse_nextToken' - If the response is truncated, Amazon SageMaker returns this token. To
-- retrieve the next set of jobs, use it in the subsequent request.
--
-- 'httpStatus', 'listMonitoringSchedulesResponse_httpStatus' - The response's http status code.
--
-- 'monitoringScheduleSummaries', 'listMonitoringSchedulesResponse_monitoringScheduleSummaries' - A JSON array in which each element is a summary for a monitoring
-- schedule.
newListMonitoringSchedulesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListMonitoringSchedulesResponse
newListMonitoringSchedulesResponse pHttpStatus_ =
  ListMonitoringSchedulesResponse'
    { nextToken =
        Core.Nothing,
      httpStatus = pHttpStatus_,
      monitoringScheduleSummaries = Core.mempty
    }

-- | If the response is truncated, Amazon SageMaker returns this token. To
-- retrieve the next set of jobs, use it in the subsequent request.
listMonitoringSchedulesResponse_nextToken :: Lens.Lens' ListMonitoringSchedulesResponse (Core.Maybe Core.Text)
listMonitoringSchedulesResponse_nextToken = Lens.lens (\ListMonitoringSchedulesResponse' {nextToken} -> nextToken) (\s@ListMonitoringSchedulesResponse' {} a -> s {nextToken = a} :: ListMonitoringSchedulesResponse)

-- | The response's http status code.
listMonitoringSchedulesResponse_httpStatus :: Lens.Lens' ListMonitoringSchedulesResponse Core.Int
listMonitoringSchedulesResponse_httpStatus = Lens.lens (\ListMonitoringSchedulesResponse' {httpStatus} -> httpStatus) (\s@ListMonitoringSchedulesResponse' {} a -> s {httpStatus = a} :: ListMonitoringSchedulesResponse)

-- | A JSON array in which each element is a summary for a monitoring
-- schedule.
listMonitoringSchedulesResponse_monitoringScheduleSummaries :: Lens.Lens' ListMonitoringSchedulesResponse [MonitoringScheduleSummary]
listMonitoringSchedulesResponse_monitoringScheduleSummaries = Lens.lens (\ListMonitoringSchedulesResponse' {monitoringScheduleSummaries} -> monitoringScheduleSummaries) (\s@ListMonitoringSchedulesResponse' {} a -> s {monitoringScheduleSummaries = a} :: ListMonitoringSchedulesResponse) Core.. Lens._Coerce

instance Core.NFData ListMonitoringSchedulesResponse
