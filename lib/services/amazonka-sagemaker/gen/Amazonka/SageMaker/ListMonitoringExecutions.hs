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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
    listMonitoringExecutions_creationTimeAfter,
    listMonitoringExecutions_creationTimeBefore,
    listMonitoringExecutions_endpointName,
    listMonitoringExecutions_lastModifiedTimeAfter,
    listMonitoringExecutions_lastModifiedTimeBefore,
    listMonitoringExecutions_maxResults,
    listMonitoringExecutions_monitoringJobDefinitionName,
    listMonitoringExecutions_monitoringScheduleName,
    listMonitoringExecutions_monitoringTypeEquals,
    listMonitoringExecutions_nextToken,
    listMonitoringExecutions_scheduledTimeAfter,
    listMonitoringExecutions_scheduledTimeBefore,
    listMonitoringExecutions_sortBy,
    listMonitoringExecutions_sortOrder,
    listMonitoringExecutions_statusEquals,

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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListMonitoringExecutions' smart constructor.
data ListMonitoringExecutions = ListMonitoringExecutions'
  { -- | A filter that returns only jobs created after a specified time.
    creationTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | A filter that returns only jobs created before a specified time.
    creationTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | Name of a specific endpoint to fetch jobs for.
    endpointName :: Prelude.Maybe Prelude.Text,
    -- | A filter that returns only jobs modified before a specified time.
    lastModifiedTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | A filter that returns only jobs modified after a specified time.
    lastModifiedTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | The maximum number of jobs to return in the response. The default value
    -- is 10.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Gets a list of the monitoring job runs of the specified monitoring job
    -- definitions.
    monitoringJobDefinitionName :: Prelude.Maybe Prelude.Text,
    -- | Name of a specific schedule to fetch jobs for.
    monitoringScheduleName :: Prelude.Maybe Prelude.Text,
    -- | A filter that returns only the monitoring job runs of the specified
    -- monitoring type.
    monitoringTypeEquals :: Prelude.Maybe MonitoringType,
    -- | The token returned if the response is truncated. To retrieve the next
    -- set of job executions, use it in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Filter for jobs scheduled after a specified time.
    scheduledTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | Filter for jobs scheduled before a specified time.
    scheduledTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | Whether to sort results by @Status@, @CreationTime@, @ScheduledTime@
    -- field. The default is @CreationTime@.
    sortBy :: Prelude.Maybe MonitoringExecutionSortKey,
    -- | Whether to sort the results in @Ascending@ or @Descending@ order. The
    -- default is @Descending@.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | A filter that retrieves only jobs with a specific status.
    statusEquals :: Prelude.Maybe ExecutionStatus
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
-- 'creationTimeAfter', 'listMonitoringExecutions_creationTimeAfter' - A filter that returns only jobs created after a specified time.
--
-- 'creationTimeBefore', 'listMonitoringExecutions_creationTimeBefore' - A filter that returns only jobs created before a specified time.
--
-- 'endpointName', 'listMonitoringExecutions_endpointName' - Name of a specific endpoint to fetch jobs for.
--
-- 'lastModifiedTimeAfter', 'listMonitoringExecutions_lastModifiedTimeAfter' - A filter that returns only jobs modified before a specified time.
--
-- 'lastModifiedTimeBefore', 'listMonitoringExecutions_lastModifiedTimeBefore' - A filter that returns only jobs modified after a specified time.
--
-- 'maxResults', 'listMonitoringExecutions_maxResults' - The maximum number of jobs to return in the response. The default value
-- is 10.
--
-- 'monitoringJobDefinitionName', 'listMonitoringExecutions_monitoringJobDefinitionName' - Gets a list of the monitoring job runs of the specified monitoring job
-- definitions.
--
-- 'monitoringScheduleName', 'listMonitoringExecutions_monitoringScheduleName' - Name of a specific schedule to fetch jobs for.
--
-- 'monitoringTypeEquals', 'listMonitoringExecutions_monitoringTypeEquals' - A filter that returns only the monitoring job runs of the specified
-- monitoring type.
--
-- 'nextToken', 'listMonitoringExecutions_nextToken' - The token returned if the response is truncated. To retrieve the next
-- set of job executions, use it in the next request.
--
-- 'scheduledTimeAfter', 'listMonitoringExecutions_scheduledTimeAfter' - Filter for jobs scheduled after a specified time.
--
-- 'scheduledTimeBefore', 'listMonitoringExecutions_scheduledTimeBefore' - Filter for jobs scheduled before a specified time.
--
-- 'sortBy', 'listMonitoringExecutions_sortBy' - Whether to sort results by @Status@, @CreationTime@, @ScheduledTime@
-- field. The default is @CreationTime@.
--
-- 'sortOrder', 'listMonitoringExecutions_sortOrder' - Whether to sort the results in @Ascending@ or @Descending@ order. The
-- default is @Descending@.
--
-- 'statusEquals', 'listMonitoringExecutions_statusEquals' - A filter that retrieves only jobs with a specific status.
newListMonitoringExecutions ::
  ListMonitoringExecutions
newListMonitoringExecutions =
  ListMonitoringExecutions'
    { creationTimeAfter =
        Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      endpointName = Prelude.Nothing,
      lastModifiedTimeAfter = Prelude.Nothing,
      lastModifiedTimeBefore = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      monitoringJobDefinitionName = Prelude.Nothing,
      monitoringScheduleName = Prelude.Nothing,
      monitoringTypeEquals = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      scheduledTimeAfter = Prelude.Nothing,
      scheduledTimeBefore = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      statusEquals = Prelude.Nothing
    }

-- | A filter that returns only jobs created after a specified time.
listMonitoringExecutions_creationTimeAfter :: Lens.Lens' ListMonitoringExecutions (Prelude.Maybe Prelude.UTCTime)
listMonitoringExecutions_creationTimeAfter = Lens.lens (\ListMonitoringExecutions' {creationTimeAfter} -> creationTimeAfter) (\s@ListMonitoringExecutions' {} a -> s {creationTimeAfter = a} :: ListMonitoringExecutions) Prelude.. Lens.mapping Data._Time

-- | A filter that returns only jobs created before a specified time.
listMonitoringExecutions_creationTimeBefore :: Lens.Lens' ListMonitoringExecutions (Prelude.Maybe Prelude.UTCTime)
listMonitoringExecutions_creationTimeBefore = Lens.lens (\ListMonitoringExecutions' {creationTimeBefore} -> creationTimeBefore) (\s@ListMonitoringExecutions' {} a -> s {creationTimeBefore = a} :: ListMonitoringExecutions) Prelude.. Lens.mapping Data._Time

-- | Name of a specific endpoint to fetch jobs for.
listMonitoringExecutions_endpointName :: Lens.Lens' ListMonitoringExecutions (Prelude.Maybe Prelude.Text)
listMonitoringExecutions_endpointName = Lens.lens (\ListMonitoringExecutions' {endpointName} -> endpointName) (\s@ListMonitoringExecutions' {} a -> s {endpointName = a} :: ListMonitoringExecutions)

-- | A filter that returns only jobs modified before a specified time.
listMonitoringExecutions_lastModifiedTimeAfter :: Lens.Lens' ListMonitoringExecutions (Prelude.Maybe Prelude.UTCTime)
listMonitoringExecutions_lastModifiedTimeAfter = Lens.lens (\ListMonitoringExecutions' {lastModifiedTimeAfter} -> lastModifiedTimeAfter) (\s@ListMonitoringExecutions' {} a -> s {lastModifiedTimeAfter = a} :: ListMonitoringExecutions) Prelude.. Lens.mapping Data._Time

-- | A filter that returns only jobs modified after a specified time.
listMonitoringExecutions_lastModifiedTimeBefore :: Lens.Lens' ListMonitoringExecutions (Prelude.Maybe Prelude.UTCTime)
listMonitoringExecutions_lastModifiedTimeBefore = Lens.lens (\ListMonitoringExecutions' {lastModifiedTimeBefore} -> lastModifiedTimeBefore) (\s@ListMonitoringExecutions' {} a -> s {lastModifiedTimeBefore = a} :: ListMonitoringExecutions) Prelude.. Lens.mapping Data._Time

-- | The maximum number of jobs to return in the response. The default value
-- is 10.
listMonitoringExecutions_maxResults :: Lens.Lens' ListMonitoringExecutions (Prelude.Maybe Prelude.Natural)
listMonitoringExecutions_maxResults = Lens.lens (\ListMonitoringExecutions' {maxResults} -> maxResults) (\s@ListMonitoringExecutions' {} a -> s {maxResults = a} :: ListMonitoringExecutions)

-- | Gets a list of the monitoring job runs of the specified monitoring job
-- definitions.
listMonitoringExecutions_monitoringJobDefinitionName :: Lens.Lens' ListMonitoringExecutions (Prelude.Maybe Prelude.Text)
listMonitoringExecutions_monitoringJobDefinitionName = Lens.lens (\ListMonitoringExecutions' {monitoringJobDefinitionName} -> monitoringJobDefinitionName) (\s@ListMonitoringExecutions' {} a -> s {monitoringJobDefinitionName = a} :: ListMonitoringExecutions)

-- | Name of a specific schedule to fetch jobs for.
listMonitoringExecutions_monitoringScheduleName :: Lens.Lens' ListMonitoringExecutions (Prelude.Maybe Prelude.Text)
listMonitoringExecutions_monitoringScheduleName = Lens.lens (\ListMonitoringExecutions' {monitoringScheduleName} -> monitoringScheduleName) (\s@ListMonitoringExecutions' {} a -> s {monitoringScheduleName = a} :: ListMonitoringExecutions)

-- | A filter that returns only the monitoring job runs of the specified
-- monitoring type.
listMonitoringExecutions_monitoringTypeEquals :: Lens.Lens' ListMonitoringExecutions (Prelude.Maybe MonitoringType)
listMonitoringExecutions_monitoringTypeEquals = Lens.lens (\ListMonitoringExecutions' {monitoringTypeEquals} -> monitoringTypeEquals) (\s@ListMonitoringExecutions' {} a -> s {monitoringTypeEquals = a} :: ListMonitoringExecutions)

-- | The token returned if the response is truncated. To retrieve the next
-- set of job executions, use it in the next request.
listMonitoringExecutions_nextToken :: Lens.Lens' ListMonitoringExecutions (Prelude.Maybe Prelude.Text)
listMonitoringExecutions_nextToken = Lens.lens (\ListMonitoringExecutions' {nextToken} -> nextToken) (\s@ListMonitoringExecutions' {} a -> s {nextToken = a} :: ListMonitoringExecutions)

-- | Filter for jobs scheduled after a specified time.
listMonitoringExecutions_scheduledTimeAfter :: Lens.Lens' ListMonitoringExecutions (Prelude.Maybe Prelude.UTCTime)
listMonitoringExecutions_scheduledTimeAfter = Lens.lens (\ListMonitoringExecutions' {scheduledTimeAfter} -> scheduledTimeAfter) (\s@ListMonitoringExecutions' {} a -> s {scheduledTimeAfter = a} :: ListMonitoringExecutions) Prelude.. Lens.mapping Data._Time

-- | Filter for jobs scheduled before a specified time.
listMonitoringExecutions_scheduledTimeBefore :: Lens.Lens' ListMonitoringExecutions (Prelude.Maybe Prelude.UTCTime)
listMonitoringExecutions_scheduledTimeBefore = Lens.lens (\ListMonitoringExecutions' {scheduledTimeBefore} -> scheduledTimeBefore) (\s@ListMonitoringExecutions' {} a -> s {scheduledTimeBefore = a} :: ListMonitoringExecutions) Prelude.. Lens.mapping Data._Time

-- | Whether to sort results by @Status@, @CreationTime@, @ScheduledTime@
-- field. The default is @CreationTime@.
listMonitoringExecutions_sortBy :: Lens.Lens' ListMonitoringExecutions (Prelude.Maybe MonitoringExecutionSortKey)
listMonitoringExecutions_sortBy = Lens.lens (\ListMonitoringExecutions' {sortBy} -> sortBy) (\s@ListMonitoringExecutions' {} a -> s {sortBy = a} :: ListMonitoringExecutions)

-- | Whether to sort the results in @Ascending@ or @Descending@ order. The
-- default is @Descending@.
listMonitoringExecutions_sortOrder :: Lens.Lens' ListMonitoringExecutions (Prelude.Maybe SortOrder)
listMonitoringExecutions_sortOrder = Lens.lens (\ListMonitoringExecutions' {sortOrder} -> sortOrder) (\s@ListMonitoringExecutions' {} a -> s {sortOrder = a} :: ListMonitoringExecutions)

-- | A filter that retrieves only jobs with a specific status.
listMonitoringExecutions_statusEquals :: Lens.Lens' ListMonitoringExecutions (Prelude.Maybe ExecutionStatus)
listMonitoringExecutions_statusEquals = Lens.lens (\ListMonitoringExecutions' {statusEquals} -> statusEquals) (\s@ListMonitoringExecutions' {} a -> s {statusEquals = a} :: ListMonitoringExecutions)

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
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "MonitoringExecutionSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListMonitoringExecutions where
  hashWithSalt _salt ListMonitoringExecutions' {..} =
    _salt
      `Prelude.hashWithSalt` creationTimeAfter
      `Prelude.hashWithSalt` creationTimeBefore
      `Prelude.hashWithSalt` endpointName
      `Prelude.hashWithSalt` lastModifiedTimeAfter
      `Prelude.hashWithSalt` lastModifiedTimeBefore
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` monitoringJobDefinitionName
      `Prelude.hashWithSalt` monitoringScheduleName
      `Prelude.hashWithSalt` monitoringTypeEquals
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` scheduledTimeAfter
      `Prelude.hashWithSalt` scheduledTimeBefore
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` statusEquals

instance Prelude.NFData ListMonitoringExecutions where
  rnf ListMonitoringExecutions' {..} =
    Prelude.rnf creationTimeAfter `Prelude.seq`
      Prelude.rnf creationTimeBefore `Prelude.seq`
        Prelude.rnf endpointName `Prelude.seq`
          Prelude.rnf lastModifiedTimeAfter `Prelude.seq`
            Prelude.rnf lastModifiedTimeBefore `Prelude.seq`
              Prelude.rnf maxResults `Prelude.seq`
                Prelude.rnf monitoringJobDefinitionName `Prelude.seq`
                  Prelude.rnf monitoringScheduleName `Prelude.seq`
                    Prelude.rnf monitoringTypeEquals `Prelude.seq`
                      Prelude.rnf nextToken `Prelude.seq`
                        Prelude.rnf scheduledTimeAfter `Prelude.seq`
                          Prelude.rnf scheduledTimeBefore `Prelude.seq`
                            Prelude.rnf sortBy `Prelude.seq`
                              Prelude.rnf sortOrder `Prelude.seq`
                                Prelude.rnf statusEquals

instance Data.ToHeaders ListMonitoringExecutions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.ListMonitoringExecutions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListMonitoringExecutions where
  toJSON ListMonitoringExecutions' {..} =
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
            ("MonitoringScheduleName" Data..=)
              Prelude.<$> monitoringScheduleName,
            ("MonitoringTypeEquals" Data..=)
              Prelude.<$> monitoringTypeEquals,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("ScheduledTimeAfter" Data..=)
              Prelude.<$> scheduledTimeAfter,
            ("ScheduledTimeBefore" Data..=)
              Prelude.<$> scheduledTimeBefore,
            ("SortBy" Data..=) Prelude.<$> sortBy,
            ("SortOrder" Data..=) Prelude.<$> sortOrder,
            ("StatusEquals" Data..=) Prelude.<$> statusEquals
          ]
      )

instance Data.ToPath ListMonitoringExecutions where
  toPath = Prelude.const "/"

instance Data.ToQuery ListMonitoringExecutions where
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
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf monitoringExecutionSummaries
