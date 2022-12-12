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
    listMonitoringSchedules_creationTimeAfter,
    listMonitoringSchedules_creationTimeBefore,
    listMonitoringSchedules_endpointName,
    listMonitoringSchedules_lastModifiedTimeAfter,
    listMonitoringSchedules_lastModifiedTimeBefore,
    listMonitoringSchedules_maxResults,
    listMonitoringSchedules_monitoringJobDefinitionName,
    listMonitoringSchedules_monitoringTypeEquals,
    listMonitoringSchedules_nameContains,
    listMonitoringSchedules_nextToken,
    listMonitoringSchedules_sortBy,
    listMonitoringSchedules_sortOrder,
    listMonitoringSchedules_statusEquals,

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
  { -- | A filter that returns only monitoring schedules created after a
    -- specified time.
    creationTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | A filter that returns only monitoring schedules created before a
    -- specified time.
    creationTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | Name of a specific endpoint to fetch schedules for.
    endpointName :: Prelude.Maybe Prelude.Text,
    -- | A filter that returns only monitoring schedules modified after a
    -- specified time.
    lastModifiedTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | A filter that returns only monitoring schedules modified before a
    -- specified time.
    lastModifiedTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | The maximum number of jobs to return in the response. The default value
    -- is 10.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Gets a list of the monitoring schedules for the specified monitoring job
    -- definition.
    monitoringJobDefinitionName :: Prelude.Maybe Prelude.Text,
    -- | A filter that returns only the monitoring schedules for the specified
    -- monitoring type.
    monitoringTypeEquals :: Prelude.Maybe MonitoringType,
    -- | Filter for monitoring schedules whose name contains a specified string.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | The token returned if the response is truncated. To retrieve the next
    -- set of job executions, use it in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Whether to sort results by @Status@, @CreationTime@, @ScheduledTime@
    -- field. The default is @CreationTime@.
    sortBy :: Prelude.Maybe MonitoringScheduleSortKey,
    -- | Whether to sort the results in @Ascending@ or @Descending@ order. The
    -- default is @Descending@.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | A filter that returns only monitoring schedules modified before a
    -- specified time.
    statusEquals :: Prelude.Maybe ScheduleStatus
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
-- 'creationTimeAfter', 'listMonitoringSchedules_creationTimeAfter' - A filter that returns only monitoring schedules created after a
-- specified time.
--
-- 'creationTimeBefore', 'listMonitoringSchedules_creationTimeBefore' - A filter that returns only monitoring schedules created before a
-- specified time.
--
-- 'endpointName', 'listMonitoringSchedules_endpointName' - Name of a specific endpoint to fetch schedules for.
--
-- 'lastModifiedTimeAfter', 'listMonitoringSchedules_lastModifiedTimeAfter' - A filter that returns only monitoring schedules modified after a
-- specified time.
--
-- 'lastModifiedTimeBefore', 'listMonitoringSchedules_lastModifiedTimeBefore' - A filter that returns only monitoring schedules modified before a
-- specified time.
--
-- 'maxResults', 'listMonitoringSchedules_maxResults' - The maximum number of jobs to return in the response. The default value
-- is 10.
--
-- 'monitoringJobDefinitionName', 'listMonitoringSchedules_monitoringJobDefinitionName' - Gets a list of the monitoring schedules for the specified monitoring job
-- definition.
--
-- 'monitoringTypeEquals', 'listMonitoringSchedules_monitoringTypeEquals' - A filter that returns only the monitoring schedules for the specified
-- monitoring type.
--
-- 'nameContains', 'listMonitoringSchedules_nameContains' - Filter for monitoring schedules whose name contains a specified string.
--
-- 'nextToken', 'listMonitoringSchedules_nextToken' - The token returned if the response is truncated. To retrieve the next
-- set of job executions, use it in the next request.
--
-- 'sortBy', 'listMonitoringSchedules_sortBy' - Whether to sort results by @Status@, @CreationTime@, @ScheduledTime@
-- field. The default is @CreationTime@.
--
-- 'sortOrder', 'listMonitoringSchedules_sortOrder' - Whether to sort the results in @Ascending@ or @Descending@ order. The
-- default is @Descending@.
--
-- 'statusEquals', 'listMonitoringSchedules_statusEquals' - A filter that returns only monitoring schedules modified before a
-- specified time.
newListMonitoringSchedules ::
  ListMonitoringSchedules
newListMonitoringSchedules =
  ListMonitoringSchedules'
    { creationTimeAfter =
        Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      endpointName = Prelude.Nothing,
      lastModifiedTimeAfter = Prelude.Nothing,
      lastModifiedTimeBefore = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      monitoringJobDefinitionName = Prelude.Nothing,
      monitoringTypeEquals = Prelude.Nothing,
      nameContains = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      statusEquals = Prelude.Nothing
    }

-- | A filter that returns only monitoring schedules created after a
-- specified time.
listMonitoringSchedules_creationTimeAfter :: Lens.Lens' ListMonitoringSchedules (Prelude.Maybe Prelude.UTCTime)
listMonitoringSchedules_creationTimeAfter = Lens.lens (\ListMonitoringSchedules' {creationTimeAfter} -> creationTimeAfter) (\s@ListMonitoringSchedules' {} a -> s {creationTimeAfter = a} :: ListMonitoringSchedules) Prelude.. Lens.mapping Data._Time

-- | A filter that returns only monitoring schedules created before a
-- specified time.
listMonitoringSchedules_creationTimeBefore :: Lens.Lens' ListMonitoringSchedules (Prelude.Maybe Prelude.UTCTime)
listMonitoringSchedules_creationTimeBefore = Lens.lens (\ListMonitoringSchedules' {creationTimeBefore} -> creationTimeBefore) (\s@ListMonitoringSchedules' {} a -> s {creationTimeBefore = a} :: ListMonitoringSchedules) Prelude.. Lens.mapping Data._Time

-- | Name of a specific endpoint to fetch schedules for.
listMonitoringSchedules_endpointName :: Lens.Lens' ListMonitoringSchedules (Prelude.Maybe Prelude.Text)
listMonitoringSchedules_endpointName = Lens.lens (\ListMonitoringSchedules' {endpointName} -> endpointName) (\s@ListMonitoringSchedules' {} a -> s {endpointName = a} :: ListMonitoringSchedules)

-- | A filter that returns only monitoring schedules modified after a
-- specified time.
listMonitoringSchedules_lastModifiedTimeAfter :: Lens.Lens' ListMonitoringSchedules (Prelude.Maybe Prelude.UTCTime)
listMonitoringSchedules_lastModifiedTimeAfter = Lens.lens (\ListMonitoringSchedules' {lastModifiedTimeAfter} -> lastModifiedTimeAfter) (\s@ListMonitoringSchedules' {} a -> s {lastModifiedTimeAfter = a} :: ListMonitoringSchedules) Prelude.. Lens.mapping Data._Time

-- | A filter that returns only monitoring schedules modified before a
-- specified time.
listMonitoringSchedules_lastModifiedTimeBefore :: Lens.Lens' ListMonitoringSchedules (Prelude.Maybe Prelude.UTCTime)
listMonitoringSchedules_lastModifiedTimeBefore = Lens.lens (\ListMonitoringSchedules' {lastModifiedTimeBefore} -> lastModifiedTimeBefore) (\s@ListMonitoringSchedules' {} a -> s {lastModifiedTimeBefore = a} :: ListMonitoringSchedules) Prelude.. Lens.mapping Data._Time

-- | The maximum number of jobs to return in the response. The default value
-- is 10.
listMonitoringSchedules_maxResults :: Lens.Lens' ListMonitoringSchedules (Prelude.Maybe Prelude.Natural)
listMonitoringSchedules_maxResults = Lens.lens (\ListMonitoringSchedules' {maxResults} -> maxResults) (\s@ListMonitoringSchedules' {} a -> s {maxResults = a} :: ListMonitoringSchedules)

-- | Gets a list of the monitoring schedules for the specified monitoring job
-- definition.
listMonitoringSchedules_monitoringJobDefinitionName :: Lens.Lens' ListMonitoringSchedules (Prelude.Maybe Prelude.Text)
listMonitoringSchedules_monitoringJobDefinitionName = Lens.lens (\ListMonitoringSchedules' {monitoringJobDefinitionName} -> monitoringJobDefinitionName) (\s@ListMonitoringSchedules' {} a -> s {monitoringJobDefinitionName = a} :: ListMonitoringSchedules)

-- | A filter that returns only the monitoring schedules for the specified
-- monitoring type.
listMonitoringSchedules_monitoringTypeEquals :: Lens.Lens' ListMonitoringSchedules (Prelude.Maybe MonitoringType)
listMonitoringSchedules_monitoringTypeEquals = Lens.lens (\ListMonitoringSchedules' {monitoringTypeEquals} -> monitoringTypeEquals) (\s@ListMonitoringSchedules' {} a -> s {monitoringTypeEquals = a} :: ListMonitoringSchedules)

-- | Filter for monitoring schedules whose name contains a specified string.
listMonitoringSchedules_nameContains :: Lens.Lens' ListMonitoringSchedules (Prelude.Maybe Prelude.Text)
listMonitoringSchedules_nameContains = Lens.lens (\ListMonitoringSchedules' {nameContains} -> nameContains) (\s@ListMonitoringSchedules' {} a -> s {nameContains = a} :: ListMonitoringSchedules)

-- | The token returned if the response is truncated. To retrieve the next
-- set of job executions, use it in the next request.
listMonitoringSchedules_nextToken :: Lens.Lens' ListMonitoringSchedules (Prelude.Maybe Prelude.Text)
listMonitoringSchedules_nextToken = Lens.lens (\ListMonitoringSchedules' {nextToken} -> nextToken) (\s@ListMonitoringSchedules' {} a -> s {nextToken = a} :: ListMonitoringSchedules)

-- | Whether to sort results by @Status@, @CreationTime@, @ScheduledTime@
-- field. The default is @CreationTime@.
listMonitoringSchedules_sortBy :: Lens.Lens' ListMonitoringSchedules (Prelude.Maybe MonitoringScheduleSortKey)
listMonitoringSchedules_sortBy = Lens.lens (\ListMonitoringSchedules' {sortBy} -> sortBy) (\s@ListMonitoringSchedules' {} a -> s {sortBy = a} :: ListMonitoringSchedules)

-- | Whether to sort the results in @Ascending@ or @Descending@ order. The
-- default is @Descending@.
listMonitoringSchedules_sortOrder :: Lens.Lens' ListMonitoringSchedules (Prelude.Maybe SortOrder)
listMonitoringSchedules_sortOrder = Lens.lens (\ListMonitoringSchedules' {sortOrder} -> sortOrder) (\s@ListMonitoringSchedules' {} a -> s {sortOrder = a} :: ListMonitoringSchedules)

-- | A filter that returns only monitoring schedules modified before a
-- specified time.
listMonitoringSchedules_statusEquals :: Lens.Lens' ListMonitoringSchedules (Prelude.Maybe ScheduleStatus)
listMonitoringSchedules_statusEquals = Lens.lens (\ListMonitoringSchedules' {statusEquals} -> statusEquals) (\s@ListMonitoringSchedules' {} a -> s {statusEquals = a} :: ListMonitoringSchedules)

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
    _salt `Prelude.hashWithSalt` creationTimeAfter
      `Prelude.hashWithSalt` creationTimeBefore
      `Prelude.hashWithSalt` endpointName
      `Prelude.hashWithSalt` lastModifiedTimeAfter
      `Prelude.hashWithSalt` lastModifiedTimeBefore
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` monitoringJobDefinitionName
      `Prelude.hashWithSalt` monitoringTypeEquals
      `Prelude.hashWithSalt` nameContains
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` statusEquals

instance Prelude.NFData ListMonitoringSchedules where
  rnf ListMonitoringSchedules' {..} =
    Prelude.rnf creationTimeAfter
      `Prelude.seq` Prelude.rnf creationTimeBefore
      `Prelude.seq` Prelude.rnf endpointName
      `Prelude.seq` Prelude.rnf lastModifiedTimeAfter
      `Prelude.seq` Prelude.rnf lastModifiedTimeBefore
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf monitoringJobDefinitionName
      `Prelude.seq` Prelude.rnf monitoringTypeEquals
      `Prelude.seq` Prelude.rnf nameContains
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf statusEquals

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
          [ ("CreationTimeAfter" Data..=)
              Prelude.<$> creationTimeAfter,
            ("CreationTimeBefore" Data..=)
              Prelude.<$> creationTimeBefore,
            ("EndpointName" Data..=) Prelude.<$> endpointName,
            ("LastModifiedTimeAfter" Data..=)
              Prelude.<$> lastModifiedTimeAfter,
            ("LastModifiedTimeBefore" Data..=)
              Prelude.<$> lastModifiedTimeBefore,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("MonitoringJobDefinitionName" Data..=)
              Prelude.<$> monitoringJobDefinitionName,
            ("MonitoringTypeEquals" Data..=)
              Prelude.<$> monitoringTypeEquals,
            ("NameContains" Data..=) Prelude.<$> nameContains,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SortBy" Data..=) Prelude.<$> sortBy,
            ("SortOrder" Data..=) Prelude.<$> sortOrder,
            ("StatusEquals" Data..=) Prelude.<$> statusEquals
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
