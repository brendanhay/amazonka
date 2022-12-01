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
-- Module      : Amazonka.SageMaker.ListMonitoringExecutions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns list of all monitoring job executions.
--
-- This operation returns paginated results.
module Amazonka.SageMaker.ListMonitoringExecutions
  ( -- * Creating a Request
    ListMonitoringExecutions (..),
    newListMonitoringExecutions,

    -- * Request Lenses
    listMonitoringExecutions_sortOrder,
    listMonitoringExecutions_nextToken,
    listMonitoringExecutions_lastModifiedTimeAfter,
    listMonitoringExecutions_endpointName,
    listMonitoringExecutions_lastModifiedTimeBefore,
    listMonitoringExecutions_scheduledTimeAfter,
    listMonitoringExecutions_creationTimeBefore,
    listMonitoringExecutions_monitoringTypeEquals,
    listMonitoringExecutions_sortBy,
    listMonitoringExecutions_monitoringScheduleName,
    listMonitoringExecutions_maxResults,
    listMonitoringExecutions_scheduledTimeBefore,
    listMonitoringExecutions_statusEquals,
    listMonitoringExecutions_creationTimeAfter,
    listMonitoringExecutions_monitoringJobDefinitionName,

    -- * Destructuring the Response
    ListMonitoringExecutionsResponse (..),
    newListMonitoringExecutionsResponse,

    -- * Response Lenses
    listMonitoringExecutionsResponse_nextToken,
    listMonitoringExecutionsResponse_httpStatus,
    listMonitoringExecutionsResponse_monitoringExecutionSummaries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListMonitoringExecutions' smart constructor.
data ListMonitoringExecutions = ListMonitoringExecutions'
  { -- | Whether to sort the results in @Ascending@ or @Descending@ order. The
    -- default is @Descending@.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | The token returned if the response is truncated. To retrieve the next
    -- set of job executions, use it in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A filter that returns only jobs modified before a specified time.
    lastModifiedTimeAfter :: Prelude.Maybe Core.POSIX,
    -- | Name of a specific endpoint to fetch jobs for.
    endpointName :: Prelude.Maybe Prelude.Text,
    -- | A filter that returns only jobs modified after a specified time.
    lastModifiedTimeBefore :: Prelude.Maybe Core.POSIX,
    -- | Filter for jobs scheduled after a specified time.
    scheduledTimeAfter :: Prelude.Maybe Core.POSIX,
    -- | A filter that returns only jobs created before a specified time.
    creationTimeBefore :: Prelude.Maybe Core.POSIX,
    -- | A filter that returns only the monitoring job runs of the specified
    -- monitoring type.
    monitoringTypeEquals :: Prelude.Maybe MonitoringType,
    -- | Whether to sort results by @Status@, @CreationTime@, @ScheduledTime@
    -- field. The default is @CreationTime@.
    sortBy :: Prelude.Maybe MonitoringExecutionSortKey,
    -- | Name of a specific schedule to fetch jobs for.
    monitoringScheduleName :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of jobs to return in the response. The default value
    -- is 10.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Filter for jobs scheduled before a specified time.
    scheduledTimeBefore :: Prelude.Maybe Core.POSIX,
    -- | A filter that retrieves only jobs with a specific status.
    statusEquals :: Prelude.Maybe ExecutionStatus,
    -- | A filter that returns only jobs created after a specified time.
    creationTimeAfter :: Prelude.Maybe Core.POSIX,
    -- | Gets a list of the monitoring job runs of the specified monitoring job
    -- definitions.
    monitoringJobDefinitionName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMonitoringExecutions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortOrder', 'listMonitoringExecutions_sortOrder' - Whether to sort the results in @Ascending@ or @Descending@ order. The
-- default is @Descending@.
--
-- 'nextToken', 'listMonitoringExecutions_nextToken' - The token returned if the response is truncated. To retrieve the next
-- set of job executions, use it in the next request.
--
-- 'lastModifiedTimeAfter', 'listMonitoringExecutions_lastModifiedTimeAfter' - A filter that returns only jobs modified before a specified time.
--
-- 'endpointName', 'listMonitoringExecutions_endpointName' - Name of a specific endpoint to fetch jobs for.
--
-- 'lastModifiedTimeBefore', 'listMonitoringExecutions_lastModifiedTimeBefore' - A filter that returns only jobs modified after a specified time.
--
-- 'scheduledTimeAfter', 'listMonitoringExecutions_scheduledTimeAfter' - Filter for jobs scheduled after a specified time.
--
-- 'creationTimeBefore', 'listMonitoringExecutions_creationTimeBefore' - A filter that returns only jobs created before a specified time.
--
-- 'monitoringTypeEquals', 'listMonitoringExecutions_monitoringTypeEquals' - A filter that returns only the monitoring job runs of the specified
-- monitoring type.
--
-- 'sortBy', 'listMonitoringExecutions_sortBy' - Whether to sort results by @Status@, @CreationTime@, @ScheduledTime@
-- field. The default is @CreationTime@.
--
-- 'monitoringScheduleName', 'listMonitoringExecutions_monitoringScheduleName' - Name of a specific schedule to fetch jobs for.
--
-- 'maxResults', 'listMonitoringExecutions_maxResults' - The maximum number of jobs to return in the response. The default value
-- is 10.
--
-- 'scheduledTimeBefore', 'listMonitoringExecutions_scheduledTimeBefore' - Filter for jobs scheduled before a specified time.
--
-- 'statusEquals', 'listMonitoringExecutions_statusEquals' - A filter that retrieves only jobs with a specific status.
--
-- 'creationTimeAfter', 'listMonitoringExecutions_creationTimeAfter' - A filter that returns only jobs created after a specified time.
--
-- 'monitoringJobDefinitionName', 'listMonitoringExecutions_monitoringJobDefinitionName' - Gets a list of the monitoring job runs of the specified monitoring job
-- definitions.
newListMonitoringExecutions ::
  ListMonitoringExecutions
newListMonitoringExecutions =
  ListMonitoringExecutions'
    { sortOrder =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      lastModifiedTimeAfter = Prelude.Nothing,
      endpointName = Prelude.Nothing,
      lastModifiedTimeBefore = Prelude.Nothing,
      scheduledTimeAfter = Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      monitoringTypeEquals = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      monitoringScheduleName = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      scheduledTimeBefore = Prelude.Nothing,
      statusEquals = Prelude.Nothing,
      creationTimeAfter = Prelude.Nothing,
      monitoringJobDefinitionName = Prelude.Nothing
    }

-- | Whether to sort the results in @Ascending@ or @Descending@ order. The
-- default is @Descending@.
listMonitoringExecutions_sortOrder :: Lens.Lens' ListMonitoringExecutions (Prelude.Maybe SortOrder)
listMonitoringExecutions_sortOrder = Lens.lens (\ListMonitoringExecutions' {sortOrder} -> sortOrder) (\s@ListMonitoringExecutions' {} a -> s {sortOrder = a} :: ListMonitoringExecutions)

-- | The token returned if the response is truncated. To retrieve the next
-- set of job executions, use it in the next request.
listMonitoringExecutions_nextToken :: Lens.Lens' ListMonitoringExecutions (Prelude.Maybe Prelude.Text)
listMonitoringExecutions_nextToken = Lens.lens (\ListMonitoringExecutions' {nextToken} -> nextToken) (\s@ListMonitoringExecutions' {} a -> s {nextToken = a} :: ListMonitoringExecutions)

-- | A filter that returns only jobs modified before a specified time.
listMonitoringExecutions_lastModifiedTimeAfter :: Lens.Lens' ListMonitoringExecutions (Prelude.Maybe Prelude.UTCTime)
listMonitoringExecutions_lastModifiedTimeAfter = Lens.lens (\ListMonitoringExecutions' {lastModifiedTimeAfter} -> lastModifiedTimeAfter) (\s@ListMonitoringExecutions' {} a -> s {lastModifiedTimeAfter = a} :: ListMonitoringExecutions) Prelude.. Lens.mapping Core._Time

-- | Name of a specific endpoint to fetch jobs for.
listMonitoringExecutions_endpointName :: Lens.Lens' ListMonitoringExecutions (Prelude.Maybe Prelude.Text)
listMonitoringExecutions_endpointName = Lens.lens (\ListMonitoringExecutions' {endpointName} -> endpointName) (\s@ListMonitoringExecutions' {} a -> s {endpointName = a} :: ListMonitoringExecutions)

-- | A filter that returns only jobs modified after a specified time.
listMonitoringExecutions_lastModifiedTimeBefore :: Lens.Lens' ListMonitoringExecutions (Prelude.Maybe Prelude.UTCTime)
listMonitoringExecutions_lastModifiedTimeBefore = Lens.lens (\ListMonitoringExecutions' {lastModifiedTimeBefore} -> lastModifiedTimeBefore) (\s@ListMonitoringExecutions' {} a -> s {lastModifiedTimeBefore = a} :: ListMonitoringExecutions) Prelude.. Lens.mapping Core._Time

-- | Filter for jobs scheduled after a specified time.
listMonitoringExecutions_scheduledTimeAfter :: Lens.Lens' ListMonitoringExecutions (Prelude.Maybe Prelude.UTCTime)
listMonitoringExecutions_scheduledTimeAfter = Lens.lens (\ListMonitoringExecutions' {scheduledTimeAfter} -> scheduledTimeAfter) (\s@ListMonitoringExecutions' {} a -> s {scheduledTimeAfter = a} :: ListMonitoringExecutions) Prelude.. Lens.mapping Core._Time

-- | A filter that returns only jobs created before a specified time.
listMonitoringExecutions_creationTimeBefore :: Lens.Lens' ListMonitoringExecutions (Prelude.Maybe Prelude.UTCTime)
listMonitoringExecutions_creationTimeBefore = Lens.lens (\ListMonitoringExecutions' {creationTimeBefore} -> creationTimeBefore) (\s@ListMonitoringExecutions' {} a -> s {creationTimeBefore = a} :: ListMonitoringExecutions) Prelude.. Lens.mapping Core._Time

-- | A filter that returns only the monitoring job runs of the specified
-- monitoring type.
listMonitoringExecutions_monitoringTypeEquals :: Lens.Lens' ListMonitoringExecutions (Prelude.Maybe MonitoringType)
listMonitoringExecutions_monitoringTypeEquals = Lens.lens (\ListMonitoringExecutions' {monitoringTypeEquals} -> monitoringTypeEquals) (\s@ListMonitoringExecutions' {} a -> s {monitoringTypeEquals = a} :: ListMonitoringExecutions)

-- | Whether to sort results by @Status@, @CreationTime@, @ScheduledTime@
-- field. The default is @CreationTime@.
listMonitoringExecutions_sortBy :: Lens.Lens' ListMonitoringExecutions (Prelude.Maybe MonitoringExecutionSortKey)
listMonitoringExecutions_sortBy = Lens.lens (\ListMonitoringExecutions' {sortBy} -> sortBy) (\s@ListMonitoringExecutions' {} a -> s {sortBy = a} :: ListMonitoringExecutions)

-- | Name of a specific schedule to fetch jobs for.
listMonitoringExecutions_monitoringScheduleName :: Lens.Lens' ListMonitoringExecutions (Prelude.Maybe Prelude.Text)
listMonitoringExecutions_monitoringScheduleName = Lens.lens (\ListMonitoringExecutions' {monitoringScheduleName} -> monitoringScheduleName) (\s@ListMonitoringExecutions' {} a -> s {monitoringScheduleName = a} :: ListMonitoringExecutions)

-- | The maximum number of jobs to return in the response. The default value
-- is 10.
listMonitoringExecutions_maxResults :: Lens.Lens' ListMonitoringExecutions (Prelude.Maybe Prelude.Natural)
listMonitoringExecutions_maxResults = Lens.lens (\ListMonitoringExecutions' {maxResults} -> maxResults) (\s@ListMonitoringExecutions' {} a -> s {maxResults = a} :: ListMonitoringExecutions)

-- | Filter for jobs scheduled before a specified time.
listMonitoringExecutions_scheduledTimeBefore :: Lens.Lens' ListMonitoringExecutions (Prelude.Maybe Prelude.UTCTime)
listMonitoringExecutions_scheduledTimeBefore = Lens.lens (\ListMonitoringExecutions' {scheduledTimeBefore} -> scheduledTimeBefore) (\s@ListMonitoringExecutions' {} a -> s {scheduledTimeBefore = a} :: ListMonitoringExecutions) Prelude.. Lens.mapping Core._Time

-- | A filter that retrieves only jobs with a specific status.
listMonitoringExecutions_statusEquals :: Lens.Lens' ListMonitoringExecutions (Prelude.Maybe ExecutionStatus)
listMonitoringExecutions_statusEquals = Lens.lens (\ListMonitoringExecutions' {statusEquals} -> statusEquals) (\s@ListMonitoringExecutions' {} a -> s {statusEquals = a} :: ListMonitoringExecutions)

-- | A filter that returns only jobs created after a specified time.
listMonitoringExecutions_creationTimeAfter :: Lens.Lens' ListMonitoringExecutions (Prelude.Maybe Prelude.UTCTime)
listMonitoringExecutions_creationTimeAfter = Lens.lens (\ListMonitoringExecutions' {creationTimeAfter} -> creationTimeAfter) (\s@ListMonitoringExecutions' {} a -> s {creationTimeAfter = a} :: ListMonitoringExecutions) Prelude.. Lens.mapping Core._Time

-- | Gets a list of the monitoring job runs of the specified monitoring job
-- definitions.
listMonitoringExecutions_monitoringJobDefinitionName :: Lens.Lens' ListMonitoringExecutions (Prelude.Maybe Prelude.Text)
listMonitoringExecutions_monitoringJobDefinitionName = Lens.lens (\ListMonitoringExecutions' {monitoringJobDefinitionName} -> monitoringJobDefinitionName) (\s@ListMonitoringExecutions' {} a -> s {monitoringJobDefinitionName = a} :: ListMonitoringExecutions)

instance Core.AWSPager ListMonitoringExecutions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listMonitoringExecutionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listMonitoringExecutionsResponse_monitoringExecutionSummaries
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listMonitoringExecutions_nextToken
          Lens..~ rs
          Lens.^? listMonitoringExecutionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListMonitoringExecutions where
  type
    AWSResponse ListMonitoringExecutions =
      ListMonitoringExecutionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMonitoringExecutionsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..?> "MonitoringExecutionSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListMonitoringExecutions where
  hashWithSalt _salt ListMonitoringExecutions' {..} =
    _salt `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` lastModifiedTimeAfter
      `Prelude.hashWithSalt` endpointName
      `Prelude.hashWithSalt` lastModifiedTimeBefore
      `Prelude.hashWithSalt` scheduledTimeAfter
      `Prelude.hashWithSalt` creationTimeBefore
      `Prelude.hashWithSalt` monitoringTypeEquals
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` monitoringScheduleName
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` scheduledTimeBefore
      `Prelude.hashWithSalt` statusEquals
      `Prelude.hashWithSalt` creationTimeAfter
      `Prelude.hashWithSalt` monitoringJobDefinitionName

instance Prelude.NFData ListMonitoringExecutions where
  rnf ListMonitoringExecutions' {..} =
    Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf lastModifiedTimeAfter
      `Prelude.seq` Prelude.rnf endpointName
      `Prelude.seq` Prelude.rnf lastModifiedTimeBefore
      `Prelude.seq` Prelude.rnf scheduledTimeAfter
      `Prelude.seq` Prelude.rnf creationTimeBefore
      `Prelude.seq` Prelude.rnf monitoringTypeEquals
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf monitoringScheduleName
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf scheduledTimeBefore
      `Prelude.seq` Prelude.rnf statusEquals
      `Prelude.seq` Prelude.rnf creationTimeAfter
      `Prelude.seq` Prelude.rnf monitoringJobDefinitionName

instance Core.ToHeaders ListMonitoringExecutions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.ListMonitoringExecutions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListMonitoringExecutions where
  toJSON ListMonitoringExecutions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SortOrder" Core..=) Prelude.<$> sortOrder,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("LastModifiedTimeAfter" Core..=)
              Prelude.<$> lastModifiedTimeAfter,
            ("EndpointName" Core..=) Prelude.<$> endpointName,
            ("LastModifiedTimeBefore" Core..=)
              Prelude.<$> lastModifiedTimeBefore,
            ("ScheduledTimeAfter" Core..=)
              Prelude.<$> scheduledTimeAfter,
            ("CreationTimeBefore" Core..=)
              Prelude.<$> creationTimeBefore,
            ("MonitoringTypeEquals" Core..=)
              Prelude.<$> monitoringTypeEquals,
            ("SortBy" Core..=) Prelude.<$> sortBy,
            ("MonitoringScheduleName" Core..=)
              Prelude.<$> monitoringScheduleName,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("ScheduledTimeBefore" Core..=)
              Prelude.<$> scheduledTimeBefore,
            ("StatusEquals" Core..=) Prelude.<$> statusEquals,
            ("CreationTimeAfter" Core..=)
              Prelude.<$> creationTimeAfter,
            ("MonitoringJobDefinitionName" Core..=)
              Prelude.<$> monitoringJobDefinitionName
          ]
      )

instance Core.ToPath ListMonitoringExecutions where
  toPath = Prelude.const "/"

instance Core.ToQuery ListMonitoringExecutions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListMonitoringExecutionsResponse' smart constructor.
data ListMonitoringExecutionsResponse = ListMonitoringExecutionsResponse'
  { -- | If the response is truncated, Amazon SageMaker returns this token. To
    -- retrieve the next set of jobs, use it in the subsequent reques
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A JSON array in which each element is a summary for a monitoring
    -- execution.
    monitoringExecutionSummaries :: [MonitoringExecutionSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMonitoringExecutionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listMonitoringExecutionsResponse_nextToken' - If the response is truncated, Amazon SageMaker returns this token. To
-- retrieve the next set of jobs, use it in the subsequent reques
--
-- 'httpStatus', 'listMonitoringExecutionsResponse_httpStatus' - The response's http status code.
--
-- 'monitoringExecutionSummaries', 'listMonitoringExecutionsResponse_monitoringExecutionSummaries' - A JSON array in which each element is a summary for a monitoring
-- execution.
newListMonitoringExecutionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListMonitoringExecutionsResponse
newListMonitoringExecutionsResponse pHttpStatus_ =
  ListMonitoringExecutionsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      monitoringExecutionSummaries =
        Prelude.mempty
    }

-- | If the response is truncated, Amazon SageMaker returns this token. To
-- retrieve the next set of jobs, use it in the subsequent reques
listMonitoringExecutionsResponse_nextToken :: Lens.Lens' ListMonitoringExecutionsResponse (Prelude.Maybe Prelude.Text)
listMonitoringExecutionsResponse_nextToken = Lens.lens (\ListMonitoringExecutionsResponse' {nextToken} -> nextToken) (\s@ListMonitoringExecutionsResponse' {} a -> s {nextToken = a} :: ListMonitoringExecutionsResponse)

-- | The response's http status code.
listMonitoringExecutionsResponse_httpStatus :: Lens.Lens' ListMonitoringExecutionsResponse Prelude.Int
listMonitoringExecutionsResponse_httpStatus = Lens.lens (\ListMonitoringExecutionsResponse' {httpStatus} -> httpStatus) (\s@ListMonitoringExecutionsResponse' {} a -> s {httpStatus = a} :: ListMonitoringExecutionsResponse)

-- | A JSON array in which each element is a summary for a monitoring
-- execution.
listMonitoringExecutionsResponse_monitoringExecutionSummaries :: Lens.Lens' ListMonitoringExecutionsResponse [MonitoringExecutionSummary]
listMonitoringExecutionsResponse_monitoringExecutionSummaries = Lens.lens (\ListMonitoringExecutionsResponse' {monitoringExecutionSummaries} -> monitoringExecutionSummaries) (\s@ListMonitoringExecutionsResponse' {} a -> s {monitoringExecutionSummaries = a} :: ListMonitoringExecutionsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListMonitoringExecutionsResponse
  where
  rnf ListMonitoringExecutionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf monitoringExecutionSummaries
