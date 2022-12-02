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
-- Module      : Amazonka.SageMaker.ListMonitoringSchedules
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns list of all monitoring schedules.
--
-- This operation returns paginated results.
module Amazonka.SageMaker.ListMonitoringSchedules
  ( -- * Creating a Request
    ListMonitoringSchedules (..),
    newListMonitoringSchedules,

    -- * Request Lenses
    listMonitoringSchedules_sortOrder,
    listMonitoringSchedules_nextToken,
    listMonitoringSchedules_lastModifiedTimeAfter,
    listMonitoringSchedules_endpointName,
    listMonitoringSchedules_nameContains,
    listMonitoringSchedules_lastModifiedTimeBefore,
    listMonitoringSchedules_creationTimeBefore,
    listMonitoringSchedules_monitoringTypeEquals,
    listMonitoringSchedules_sortBy,
    listMonitoringSchedules_maxResults,
    listMonitoringSchedules_statusEquals,
    listMonitoringSchedules_creationTimeAfter,
    listMonitoringSchedules_monitoringJobDefinitionName,

    -- * Destructuring the Response
    ListMonitoringSchedulesResponse (..),
    newListMonitoringSchedulesResponse,

    -- * Response Lenses
    listMonitoringSchedulesResponse_nextToken,
    listMonitoringSchedulesResponse_httpStatus,
    listMonitoringSchedulesResponse_monitoringScheduleSummaries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListMonitoringSchedules' smart constructor.
data ListMonitoringSchedules = ListMonitoringSchedules'
  { -- | Whether to sort the results in @Ascending@ or @Descending@ order. The
    -- default is @Descending@.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | The token returned if the response is truncated. To retrieve the next
    -- set of job executions, use it in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A filter that returns only monitoring schedules modified after a
    -- specified time.
    lastModifiedTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | Name of a specific endpoint to fetch schedules for.
    endpointName :: Prelude.Maybe Prelude.Text,
    -- | Filter for monitoring schedules whose name contains a specified string.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | A filter that returns only monitoring schedules modified before a
    -- specified time.
    lastModifiedTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | A filter that returns only monitoring schedules created before a
    -- specified time.
    creationTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | A filter that returns only the monitoring schedules for the specified
    -- monitoring type.
    monitoringTypeEquals :: Prelude.Maybe MonitoringType,
    -- | Whether to sort results by @Status@, @CreationTime@, @ScheduledTime@
    -- field. The default is @CreationTime@.
    sortBy :: Prelude.Maybe MonitoringScheduleSortKey,
    -- | The maximum number of jobs to return in the response. The default value
    -- is 10.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A filter that returns only monitoring schedules modified before a
    -- specified time.
    statusEquals :: Prelude.Maybe ScheduleStatus,
    -- | A filter that returns only monitoring schedules created after a
    -- specified time.
    creationTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | Gets a list of the monitoring schedules for the specified monitoring job
    -- definition.
    monitoringJobDefinitionName :: Prelude.Maybe Prelude.Text
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
-- 'sortOrder', 'listMonitoringSchedules_sortOrder' - Whether to sort the results in @Ascending@ or @Descending@ order. The
-- default is @Descending@.
--
-- 'nextToken', 'listMonitoringSchedules_nextToken' - The token returned if the response is truncated. To retrieve the next
-- set of job executions, use it in the next request.
--
-- 'lastModifiedTimeAfter', 'listMonitoringSchedules_lastModifiedTimeAfter' - A filter that returns only monitoring schedules modified after a
-- specified time.
--
-- 'endpointName', 'listMonitoringSchedules_endpointName' - Name of a specific endpoint to fetch schedules for.
--
-- 'nameContains', 'listMonitoringSchedules_nameContains' - Filter for monitoring schedules whose name contains a specified string.
--
-- 'lastModifiedTimeBefore', 'listMonitoringSchedules_lastModifiedTimeBefore' - A filter that returns only monitoring schedules modified before a
-- specified time.
--
-- 'creationTimeBefore', 'listMonitoringSchedules_creationTimeBefore' - A filter that returns only monitoring schedules created before a
-- specified time.
--
-- 'monitoringTypeEquals', 'listMonitoringSchedules_monitoringTypeEquals' - A filter that returns only the monitoring schedules for the specified
-- monitoring type.
--
-- 'sortBy', 'listMonitoringSchedules_sortBy' - Whether to sort results by @Status@, @CreationTime@, @ScheduledTime@
-- field. The default is @CreationTime@.
--
-- 'maxResults', 'listMonitoringSchedules_maxResults' - The maximum number of jobs to return in the response. The default value
-- is 10.
--
-- 'statusEquals', 'listMonitoringSchedules_statusEquals' - A filter that returns only monitoring schedules modified before a
-- specified time.
--
-- 'creationTimeAfter', 'listMonitoringSchedules_creationTimeAfter' - A filter that returns only monitoring schedules created after a
-- specified time.
--
-- 'monitoringJobDefinitionName', 'listMonitoringSchedules_monitoringJobDefinitionName' - Gets a list of the monitoring schedules for the specified monitoring job
-- definition.
newListMonitoringSchedules ::
  ListMonitoringSchedules
newListMonitoringSchedules =
  ListMonitoringSchedules'
    { sortOrder =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      lastModifiedTimeAfter = Prelude.Nothing,
      endpointName = Prelude.Nothing,
      nameContains = Prelude.Nothing,
      lastModifiedTimeBefore = Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      monitoringTypeEquals = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      statusEquals = Prelude.Nothing,
      creationTimeAfter = Prelude.Nothing,
      monitoringJobDefinitionName = Prelude.Nothing
    }

-- | Whether to sort the results in @Ascending@ or @Descending@ order. The
-- default is @Descending@.
listMonitoringSchedules_sortOrder :: Lens.Lens' ListMonitoringSchedules (Prelude.Maybe SortOrder)
listMonitoringSchedules_sortOrder = Lens.lens (\ListMonitoringSchedules' {sortOrder} -> sortOrder) (\s@ListMonitoringSchedules' {} a -> s {sortOrder = a} :: ListMonitoringSchedules)

-- | The token returned if the response is truncated. To retrieve the next
-- set of job executions, use it in the next request.
listMonitoringSchedules_nextToken :: Lens.Lens' ListMonitoringSchedules (Prelude.Maybe Prelude.Text)
listMonitoringSchedules_nextToken = Lens.lens (\ListMonitoringSchedules' {nextToken} -> nextToken) (\s@ListMonitoringSchedules' {} a -> s {nextToken = a} :: ListMonitoringSchedules)

-- | A filter that returns only monitoring schedules modified after a
-- specified time.
listMonitoringSchedules_lastModifiedTimeAfter :: Lens.Lens' ListMonitoringSchedules (Prelude.Maybe Prelude.UTCTime)
listMonitoringSchedules_lastModifiedTimeAfter = Lens.lens (\ListMonitoringSchedules' {lastModifiedTimeAfter} -> lastModifiedTimeAfter) (\s@ListMonitoringSchedules' {} a -> s {lastModifiedTimeAfter = a} :: ListMonitoringSchedules) Prelude.. Lens.mapping Data._Time

-- | Name of a specific endpoint to fetch schedules for.
listMonitoringSchedules_endpointName :: Lens.Lens' ListMonitoringSchedules (Prelude.Maybe Prelude.Text)
listMonitoringSchedules_endpointName = Lens.lens (\ListMonitoringSchedules' {endpointName} -> endpointName) (\s@ListMonitoringSchedules' {} a -> s {endpointName = a} :: ListMonitoringSchedules)

-- | Filter for monitoring schedules whose name contains a specified string.
listMonitoringSchedules_nameContains :: Lens.Lens' ListMonitoringSchedules (Prelude.Maybe Prelude.Text)
listMonitoringSchedules_nameContains = Lens.lens (\ListMonitoringSchedules' {nameContains} -> nameContains) (\s@ListMonitoringSchedules' {} a -> s {nameContains = a} :: ListMonitoringSchedules)

-- | A filter that returns only monitoring schedules modified before a
-- specified time.
listMonitoringSchedules_lastModifiedTimeBefore :: Lens.Lens' ListMonitoringSchedules (Prelude.Maybe Prelude.UTCTime)
listMonitoringSchedules_lastModifiedTimeBefore = Lens.lens (\ListMonitoringSchedules' {lastModifiedTimeBefore} -> lastModifiedTimeBefore) (\s@ListMonitoringSchedules' {} a -> s {lastModifiedTimeBefore = a} :: ListMonitoringSchedules) Prelude.. Lens.mapping Data._Time

-- | A filter that returns only monitoring schedules created before a
-- specified time.
listMonitoringSchedules_creationTimeBefore :: Lens.Lens' ListMonitoringSchedules (Prelude.Maybe Prelude.UTCTime)
listMonitoringSchedules_creationTimeBefore = Lens.lens (\ListMonitoringSchedules' {creationTimeBefore} -> creationTimeBefore) (\s@ListMonitoringSchedules' {} a -> s {creationTimeBefore = a} :: ListMonitoringSchedules) Prelude.. Lens.mapping Data._Time

-- | A filter that returns only the monitoring schedules for the specified
-- monitoring type.
listMonitoringSchedules_monitoringTypeEquals :: Lens.Lens' ListMonitoringSchedules (Prelude.Maybe MonitoringType)
listMonitoringSchedules_monitoringTypeEquals = Lens.lens (\ListMonitoringSchedules' {monitoringTypeEquals} -> monitoringTypeEquals) (\s@ListMonitoringSchedules' {} a -> s {monitoringTypeEquals = a} :: ListMonitoringSchedules)

-- | Whether to sort results by @Status@, @CreationTime@, @ScheduledTime@
-- field. The default is @CreationTime@.
listMonitoringSchedules_sortBy :: Lens.Lens' ListMonitoringSchedules (Prelude.Maybe MonitoringScheduleSortKey)
listMonitoringSchedules_sortBy = Lens.lens (\ListMonitoringSchedules' {sortBy} -> sortBy) (\s@ListMonitoringSchedules' {} a -> s {sortBy = a} :: ListMonitoringSchedules)

-- | The maximum number of jobs to return in the response. The default value
-- is 10.
listMonitoringSchedules_maxResults :: Lens.Lens' ListMonitoringSchedules (Prelude.Maybe Prelude.Natural)
listMonitoringSchedules_maxResults = Lens.lens (\ListMonitoringSchedules' {maxResults} -> maxResults) (\s@ListMonitoringSchedules' {} a -> s {maxResults = a} :: ListMonitoringSchedules)

-- | A filter that returns only monitoring schedules modified before a
-- specified time.
listMonitoringSchedules_statusEquals :: Lens.Lens' ListMonitoringSchedules (Prelude.Maybe ScheduleStatus)
listMonitoringSchedules_statusEquals = Lens.lens (\ListMonitoringSchedules' {statusEquals} -> statusEquals) (\s@ListMonitoringSchedules' {} a -> s {statusEquals = a} :: ListMonitoringSchedules)

-- | A filter that returns only monitoring schedules created after a
-- specified time.
listMonitoringSchedules_creationTimeAfter :: Lens.Lens' ListMonitoringSchedules (Prelude.Maybe Prelude.UTCTime)
listMonitoringSchedules_creationTimeAfter = Lens.lens (\ListMonitoringSchedules' {creationTimeAfter} -> creationTimeAfter) (\s@ListMonitoringSchedules' {} a -> s {creationTimeAfter = a} :: ListMonitoringSchedules) Prelude.. Lens.mapping Data._Time

-- | Gets a list of the monitoring schedules for the specified monitoring job
-- definition.
listMonitoringSchedules_monitoringJobDefinitionName :: Lens.Lens' ListMonitoringSchedules (Prelude.Maybe Prelude.Text)
listMonitoringSchedules_monitoringJobDefinitionName = Lens.lens (\ListMonitoringSchedules' {monitoringJobDefinitionName} -> monitoringJobDefinitionName) (\s@ListMonitoringSchedules' {} a -> s {monitoringJobDefinitionName = a} :: ListMonitoringSchedules)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMonitoringSchedulesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..?> "MonitoringScheduleSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListMonitoringSchedules where
  hashWithSalt _salt ListMonitoringSchedules' {..} =
    _salt `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` lastModifiedTimeAfter
      `Prelude.hashWithSalt` endpointName
      `Prelude.hashWithSalt` nameContains
      `Prelude.hashWithSalt` lastModifiedTimeBefore
      `Prelude.hashWithSalt` creationTimeBefore
      `Prelude.hashWithSalt` monitoringTypeEquals
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` statusEquals
      `Prelude.hashWithSalt` creationTimeAfter
      `Prelude.hashWithSalt` monitoringJobDefinitionName

instance Prelude.NFData ListMonitoringSchedules where
  rnf ListMonitoringSchedules' {..} =
    Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf lastModifiedTimeAfter
      `Prelude.seq` Prelude.rnf endpointName
      `Prelude.seq` Prelude.rnf nameContains
      `Prelude.seq` Prelude.rnf lastModifiedTimeBefore
      `Prelude.seq` Prelude.rnf creationTimeBefore
      `Prelude.seq` Prelude.rnf monitoringTypeEquals
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf statusEquals
      `Prelude.seq` Prelude.rnf creationTimeAfter
      `Prelude.seq` Prelude.rnf monitoringJobDefinitionName

instance Data.ToHeaders ListMonitoringSchedules where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.ListMonitoringSchedules" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListMonitoringSchedules where
  toJSON ListMonitoringSchedules' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SortOrder" Data..=) Prelude.<$> sortOrder,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("LastModifiedTimeAfter" Data..=)
              Prelude.<$> lastModifiedTimeAfter,
            ("EndpointName" Data..=) Prelude.<$> endpointName,
            ("NameContains" Data..=) Prelude.<$> nameContains,
            ("LastModifiedTimeBefore" Data..=)
              Prelude.<$> lastModifiedTimeBefore,
            ("CreationTimeBefore" Data..=)
              Prelude.<$> creationTimeBefore,
            ("MonitoringTypeEquals" Data..=)
              Prelude.<$> monitoringTypeEquals,
            ("SortBy" Data..=) Prelude.<$> sortBy,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("StatusEquals" Data..=) Prelude.<$> statusEquals,
            ("CreationTimeAfter" Data..=)
              Prelude.<$> creationTimeAfter,
            ("MonitoringJobDefinitionName" Data..=)
              Prelude.<$> monitoringJobDefinitionName
          ]
      )

instance Data.ToPath ListMonitoringSchedules where
  toPath = Prelude.const "/"

instance Data.ToQuery ListMonitoringSchedules where
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
listMonitoringSchedulesResponse_monitoringScheduleSummaries = Lens.lens (\ListMonitoringSchedulesResponse' {monitoringScheduleSummaries} -> monitoringScheduleSummaries) (\s@ListMonitoringSchedulesResponse' {} a -> s {monitoringScheduleSummaries = a} :: ListMonitoringSchedulesResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListMonitoringSchedulesResponse
  where
  rnf ListMonitoringSchedulesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf monitoringScheduleSummaries
