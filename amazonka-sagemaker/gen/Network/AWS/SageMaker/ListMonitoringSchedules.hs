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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListMonitoringSchedules' smart constructor.
data ListMonitoringSchedules = ListMonitoringSchedules'
  { -- | A filter that returns only monitoring schedules modified before a
    -- specified time.
    lastModifiedTimeBefore :: Prelude.Maybe Core.POSIX,
    -- | Whether to sort the results in @Ascending@ or @Descending@ order. The
    -- default is @Descending@.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | The token returned if the response is truncated. To retrieve the next
    -- set of job executions, use it in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Name of a specific endpoint to fetch schedules for.
    endpointName :: Prelude.Maybe Prelude.Text,
    -- | Filter for monitoring schedules whose name contains a specified string.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | Gets a list of the monitoring schedules for the specified monitoring job
    -- definition.
    monitoringJobDefinitionName :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of jobs to return in the response. The default value
    -- is 10.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A filter that returns only monitoring schedules created before a
    -- specified time.
    creationTimeBefore :: Prelude.Maybe Core.POSIX,
    -- | A filter that returns only monitoring schedules modified after a
    -- specified time.
    lastModifiedTimeAfter :: Prelude.Maybe Core.POSIX,
    -- | Whether to sort results by @Status@, @CreationTime@, @ScheduledTime@
    -- field. The default is @CreationTime@.
    sortBy :: Prelude.Maybe MonitoringScheduleSortKey,
    -- | A filter that returns only monitoring schedules modified before a
    -- specified time.
    statusEquals :: Prelude.Maybe ScheduleStatus,
    -- | A filter that returns only the monitoring schedules for the specified
    -- monitoring type.
    monitoringTypeEquals :: Prelude.Maybe MonitoringType,
    -- | A filter that returns only monitoring schedules created after a
    -- specified time.
    creationTimeAfter :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      endpointName = Prelude.Nothing,
      nameContains = Prelude.Nothing,
      monitoringJobDefinitionName = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      lastModifiedTimeAfter = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      statusEquals = Prelude.Nothing,
      monitoringTypeEquals = Prelude.Nothing,
      creationTimeAfter = Prelude.Nothing
    }

-- | A filter that returns only monitoring schedules modified before a
-- specified time.
listMonitoringSchedules_lastModifiedTimeBefore :: Lens.Lens' ListMonitoringSchedules (Prelude.Maybe Prelude.UTCTime)
listMonitoringSchedules_lastModifiedTimeBefore = Lens.lens (\ListMonitoringSchedules' {lastModifiedTimeBefore} -> lastModifiedTimeBefore) (\s@ListMonitoringSchedules' {} a -> s {lastModifiedTimeBefore = a} :: ListMonitoringSchedules) Prelude.. Lens.mapping Core._Time

-- | Whether to sort the results in @Ascending@ or @Descending@ order. The
-- default is @Descending@.
listMonitoringSchedules_sortOrder :: Lens.Lens' ListMonitoringSchedules (Prelude.Maybe SortOrder)
listMonitoringSchedules_sortOrder = Lens.lens (\ListMonitoringSchedules' {sortOrder} -> sortOrder) (\s@ListMonitoringSchedules' {} a -> s {sortOrder = a} :: ListMonitoringSchedules)

-- | The token returned if the response is truncated. To retrieve the next
-- set of job executions, use it in the next request.
listMonitoringSchedules_nextToken :: Lens.Lens' ListMonitoringSchedules (Prelude.Maybe Prelude.Text)
listMonitoringSchedules_nextToken = Lens.lens (\ListMonitoringSchedules' {nextToken} -> nextToken) (\s@ListMonitoringSchedules' {} a -> s {nextToken = a} :: ListMonitoringSchedules)

-- | Name of a specific endpoint to fetch schedules for.
listMonitoringSchedules_endpointName :: Lens.Lens' ListMonitoringSchedules (Prelude.Maybe Prelude.Text)
listMonitoringSchedules_endpointName = Lens.lens (\ListMonitoringSchedules' {endpointName} -> endpointName) (\s@ListMonitoringSchedules' {} a -> s {endpointName = a} :: ListMonitoringSchedules)

-- | Filter for monitoring schedules whose name contains a specified string.
listMonitoringSchedules_nameContains :: Lens.Lens' ListMonitoringSchedules (Prelude.Maybe Prelude.Text)
listMonitoringSchedules_nameContains = Lens.lens (\ListMonitoringSchedules' {nameContains} -> nameContains) (\s@ListMonitoringSchedules' {} a -> s {nameContains = a} :: ListMonitoringSchedules)

-- | Gets a list of the monitoring schedules for the specified monitoring job
-- definition.
listMonitoringSchedules_monitoringJobDefinitionName :: Lens.Lens' ListMonitoringSchedules (Prelude.Maybe Prelude.Text)
listMonitoringSchedules_monitoringJobDefinitionName = Lens.lens (\ListMonitoringSchedules' {monitoringJobDefinitionName} -> monitoringJobDefinitionName) (\s@ListMonitoringSchedules' {} a -> s {monitoringJobDefinitionName = a} :: ListMonitoringSchedules)

-- | The maximum number of jobs to return in the response. The default value
-- is 10.
listMonitoringSchedules_maxResults :: Lens.Lens' ListMonitoringSchedules (Prelude.Maybe Prelude.Natural)
listMonitoringSchedules_maxResults = Lens.lens (\ListMonitoringSchedules' {maxResults} -> maxResults) (\s@ListMonitoringSchedules' {} a -> s {maxResults = a} :: ListMonitoringSchedules)

-- | A filter that returns only monitoring schedules created before a
-- specified time.
listMonitoringSchedules_creationTimeBefore :: Lens.Lens' ListMonitoringSchedules (Prelude.Maybe Prelude.UTCTime)
listMonitoringSchedules_creationTimeBefore = Lens.lens (\ListMonitoringSchedules' {creationTimeBefore} -> creationTimeBefore) (\s@ListMonitoringSchedules' {} a -> s {creationTimeBefore = a} :: ListMonitoringSchedules) Prelude.. Lens.mapping Core._Time

-- | A filter that returns only monitoring schedules modified after a
-- specified time.
listMonitoringSchedules_lastModifiedTimeAfter :: Lens.Lens' ListMonitoringSchedules (Prelude.Maybe Prelude.UTCTime)
listMonitoringSchedules_lastModifiedTimeAfter = Lens.lens (\ListMonitoringSchedules' {lastModifiedTimeAfter} -> lastModifiedTimeAfter) (\s@ListMonitoringSchedules' {} a -> s {lastModifiedTimeAfter = a} :: ListMonitoringSchedules) Prelude.. Lens.mapping Core._Time

-- | Whether to sort results by @Status@, @CreationTime@, @ScheduledTime@
-- field. The default is @CreationTime@.
listMonitoringSchedules_sortBy :: Lens.Lens' ListMonitoringSchedules (Prelude.Maybe MonitoringScheduleSortKey)
listMonitoringSchedules_sortBy = Lens.lens (\ListMonitoringSchedules' {sortBy} -> sortBy) (\s@ListMonitoringSchedules' {} a -> s {sortBy = a} :: ListMonitoringSchedules)

-- | A filter that returns only monitoring schedules modified before a
-- specified time.
listMonitoringSchedules_statusEquals :: Lens.Lens' ListMonitoringSchedules (Prelude.Maybe ScheduleStatus)
listMonitoringSchedules_statusEquals = Lens.lens (\ListMonitoringSchedules' {statusEquals} -> statusEquals) (\s@ListMonitoringSchedules' {} a -> s {statusEquals = a} :: ListMonitoringSchedules)

-- | A filter that returns only the monitoring schedules for the specified
-- monitoring type.
listMonitoringSchedules_monitoringTypeEquals :: Lens.Lens' ListMonitoringSchedules (Prelude.Maybe MonitoringType)
listMonitoringSchedules_monitoringTypeEquals = Lens.lens (\ListMonitoringSchedules' {monitoringTypeEquals} -> monitoringTypeEquals) (\s@ListMonitoringSchedules' {} a -> s {monitoringTypeEquals = a} :: ListMonitoringSchedules)

-- | A filter that returns only monitoring schedules created after a
-- specified time.
listMonitoringSchedules_creationTimeAfter :: Lens.Lens' ListMonitoringSchedules (Prelude.Maybe Prelude.UTCTime)
listMonitoringSchedules_creationTimeAfter = Lens.lens (\ListMonitoringSchedules' {creationTimeAfter} -> creationTimeAfter) (\s@ListMonitoringSchedules' {} a -> s {creationTimeAfter = a} :: ListMonitoringSchedules) Prelude.. Lens.mapping Core._Time

instance Core.AWSPager ListMonitoringSchedules where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listMonitoringSchedulesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listMonitoringSchedulesResponse_monitoringScheduleSummaries
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listMonitoringSchedules_nextToken
          Lens..~ rs
          Lens.^? listMonitoringSchedulesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListMonitoringSchedules where
  type
    AWSResponse ListMonitoringSchedules =
      ListMonitoringSchedulesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMonitoringSchedulesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..?> "MonitoringScheduleSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListMonitoringSchedules

instance Prelude.NFData ListMonitoringSchedules

instance Core.ToHeaders ListMonitoringSchedules where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.ListMonitoringSchedules" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListMonitoringSchedules where
  toJSON ListMonitoringSchedules' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("LastModifiedTimeBefore" Core..=)
              Prelude.<$> lastModifiedTimeBefore,
            ("SortOrder" Core..=) Prelude.<$> sortOrder,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("EndpointName" Core..=) Prelude.<$> endpointName,
            ("NameContains" Core..=) Prelude.<$> nameContains,
            ("MonitoringJobDefinitionName" Core..=)
              Prelude.<$> monitoringJobDefinitionName,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("CreationTimeBefore" Core..=)
              Prelude.<$> creationTimeBefore,
            ("LastModifiedTimeAfter" Core..=)
              Prelude.<$> lastModifiedTimeAfter,
            ("SortBy" Core..=) Prelude.<$> sortBy,
            ("StatusEquals" Core..=) Prelude.<$> statusEquals,
            ("MonitoringTypeEquals" Core..=)
              Prelude.<$> monitoringTypeEquals,
            ("CreationTimeAfter" Core..=)
              Prelude.<$> creationTimeAfter
          ]
      )

instance Core.ToPath ListMonitoringSchedules where
  toPath = Prelude.const "/"

instance Core.ToQuery ListMonitoringSchedules where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListMonitoringSchedulesResponse' smart constructor.
data ListMonitoringSchedulesResponse = ListMonitoringSchedulesResponse'
  { -- | If the response is truncated, Amazon SageMaker returns this token. To
    -- retrieve the next set of jobs, use it in the subsequent request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A JSON array in which each element is a summary for a monitoring
    -- schedule.
    monitoringScheduleSummaries :: [MonitoringScheduleSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListMonitoringSchedulesResponse
newListMonitoringSchedulesResponse pHttpStatus_ =
  ListMonitoringSchedulesResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      monitoringScheduleSummaries =
        Prelude.mempty
    }

-- | If the response is truncated, Amazon SageMaker returns this token. To
-- retrieve the next set of jobs, use it in the subsequent request.
listMonitoringSchedulesResponse_nextToken :: Lens.Lens' ListMonitoringSchedulesResponse (Prelude.Maybe Prelude.Text)
listMonitoringSchedulesResponse_nextToken = Lens.lens (\ListMonitoringSchedulesResponse' {nextToken} -> nextToken) (\s@ListMonitoringSchedulesResponse' {} a -> s {nextToken = a} :: ListMonitoringSchedulesResponse)

-- | The response's http status code.
listMonitoringSchedulesResponse_httpStatus :: Lens.Lens' ListMonitoringSchedulesResponse Prelude.Int
listMonitoringSchedulesResponse_httpStatus = Lens.lens (\ListMonitoringSchedulesResponse' {httpStatus} -> httpStatus) (\s@ListMonitoringSchedulesResponse' {} a -> s {httpStatus = a} :: ListMonitoringSchedulesResponse)

-- | A JSON array in which each element is a summary for a monitoring
-- schedule.
listMonitoringSchedulesResponse_monitoringScheduleSummaries :: Lens.Lens' ListMonitoringSchedulesResponse [MonitoringScheduleSummary]
listMonitoringSchedulesResponse_monitoringScheduleSummaries = Lens.lens (\ListMonitoringSchedulesResponse' {monitoringScheduleSummaries} -> monitoringScheduleSummaries) (\s@ListMonitoringSchedulesResponse' {} a -> s {monitoringScheduleSummaries = a} :: ListMonitoringSchedulesResponse) Prelude.. Lens._Coerce

instance
  Prelude.NFData
    ListMonitoringSchedulesResponse
