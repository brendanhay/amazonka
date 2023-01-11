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
-- Module      : Amazonka.SageMaker.ListProcessingJobs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists processing jobs that satisfy various filters.
--
-- This operation returns paginated results.
module Amazonka.SageMaker.ListProcessingJobs
  ( -- * Creating a Request
    ListProcessingJobs (..),
    newListProcessingJobs,

    -- * Request Lenses
    listProcessingJobs_creationTimeAfter,
    listProcessingJobs_creationTimeBefore,
    listProcessingJobs_lastModifiedTimeAfter,
    listProcessingJobs_lastModifiedTimeBefore,
    listProcessingJobs_maxResults,
    listProcessingJobs_nameContains,
    listProcessingJobs_nextToken,
    listProcessingJobs_sortBy,
    listProcessingJobs_sortOrder,
    listProcessingJobs_statusEquals,

    -- * Destructuring the Response
    ListProcessingJobsResponse (..),
    newListProcessingJobsResponse,

    -- * Response Lenses
    listProcessingJobsResponse_nextToken,
    listProcessingJobsResponse_httpStatus,
    listProcessingJobsResponse_processingJobSummaries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListProcessingJobs' smart constructor.
data ListProcessingJobs = ListProcessingJobs'
  { -- | A filter that returns only processing jobs created after the specified
    -- time.
    creationTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | A filter that returns only processing jobs created after the specified
    -- time.
    creationTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | A filter that returns only processing jobs modified after the specified
    -- time.
    lastModifiedTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | A filter that returns only processing jobs modified before the specified
    -- time.
    lastModifiedTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | The maximum number of processing jobs to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A string in the processing job name. This filter returns only processing
    -- jobs whose name contains the specified string.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | If the result of the previous @ListProcessingJobs@ request was
    -- truncated, the response includes a @NextToken@. To retrieve the next set
    -- of processing jobs, use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The field to sort results by. The default is @CreationTime@.
    sortBy :: Prelude.Maybe SortBy,
    -- | The sort order for results. The default is @Ascending@.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | A filter that retrieves only processing jobs with a specific status.
    statusEquals :: Prelude.Maybe ProcessingJobStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProcessingJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimeAfter', 'listProcessingJobs_creationTimeAfter' - A filter that returns only processing jobs created after the specified
-- time.
--
-- 'creationTimeBefore', 'listProcessingJobs_creationTimeBefore' - A filter that returns only processing jobs created after the specified
-- time.
--
-- 'lastModifiedTimeAfter', 'listProcessingJobs_lastModifiedTimeAfter' - A filter that returns only processing jobs modified after the specified
-- time.
--
-- 'lastModifiedTimeBefore', 'listProcessingJobs_lastModifiedTimeBefore' - A filter that returns only processing jobs modified before the specified
-- time.
--
-- 'maxResults', 'listProcessingJobs_maxResults' - The maximum number of processing jobs to return in the response.
--
-- 'nameContains', 'listProcessingJobs_nameContains' - A string in the processing job name. This filter returns only processing
-- jobs whose name contains the specified string.
--
-- 'nextToken', 'listProcessingJobs_nextToken' - If the result of the previous @ListProcessingJobs@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of processing jobs, use the token in the next request.
--
-- 'sortBy', 'listProcessingJobs_sortBy' - The field to sort results by. The default is @CreationTime@.
--
-- 'sortOrder', 'listProcessingJobs_sortOrder' - The sort order for results. The default is @Ascending@.
--
-- 'statusEquals', 'listProcessingJobs_statusEquals' - A filter that retrieves only processing jobs with a specific status.
newListProcessingJobs ::
  ListProcessingJobs
newListProcessingJobs =
  ListProcessingJobs'
    { creationTimeAfter =
        Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      lastModifiedTimeAfter = Prelude.Nothing,
      lastModifiedTimeBefore = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nameContains = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      statusEquals = Prelude.Nothing
    }

-- | A filter that returns only processing jobs created after the specified
-- time.
listProcessingJobs_creationTimeAfter :: Lens.Lens' ListProcessingJobs (Prelude.Maybe Prelude.UTCTime)
listProcessingJobs_creationTimeAfter = Lens.lens (\ListProcessingJobs' {creationTimeAfter} -> creationTimeAfter) (\s@ListProcessingJobs' {} a -> s {creationTimeAfter = a} :: ListProcessingJobs) Prelude.. Lens.mapping Data._Time

-- | A filter that returns only processing jobs created after the specified
-- time.
listProcessingJobs_creationTimeBefore :: Lens.Lens' ListProcessingJobs (Prelude.Maybe Prelude.UTCTime)
listProcessingJobs_creationTimeBefore = Lens.lens (\ListProcessingJobs' {creationTimeBefore} -> creationTimeBefore) (\s@ListProcessingJobs' {} a -> s {creationTimeBefore = a} :: ListProcessingJobs) Prelude.. Lens.mapping Data._Time

-- | A filter that returns only processing jobs modified after the specified
-- time.
listProcessingJobs_lastModifiedTimeAfter :: Lens.Lens' ListProcessingJobs (Prelude.Maybe Prelude.UTCTime)
listProcessingJobs_lastModifiedTimeAfter = Lens.lens (\ListProcessingJobs' {lastModifiedTimeAfter} -> lastModifiedTimeAfter) (\s@ListProcessingJobs' {} a -> s {lastModifiedTimeAfter = a} :: ListProcessingJobs) Prelude.. Lens.mapping Data._Time

-- | A filter that returns only processing jobs modified before the specified
-- time.
listProcessingJobs_lastModifiedTimeBefore :: Lens.Lens' ListProcessingJobs (Prelude.Maybe Prelude.UTCTime)
listProcessingJobs_lastModifiedTimeBefore = Lens.lens (\ListProcessingJobs' {lastModifiedTimeBefore} -> lastModifiedTimeBefore) (\s@ListProcessingJobs' {} a -> s {lastModifiedTimeBefore = a} :: ListProcessingJobs) Prelude.. Lens.mapping Data._Time

-- | The maximum number of processing jobs to return in the response.
listProcessingJobs_maxResults :: Lens.Lens' ListProcessingJobs (Prelude.Maybe Prelude.Natural)
listProcessingJobs_maxResults = Lens.lens (\ListProcessingJobs' {maxResults} -> maxResults) (\s@ListProcessingJobs' {} a -> s {maxResults = a} :: ListProcessingJobs)

-- | A string in the processing job name. This filter returns only processing
-- jobs whose name contains the specified string.
listProcessingJobs_nameContains :: Lens.Lens' ListProcessingJobs (Prelude.Maybe Prelude.Text)
listProcessingJobs_nameContains = Lens.lens (\ListProcessingJobs' {nameContains} -> nameContains) (\s@ListProcessingJobs' {} a -> s {nameContains = a} :: ListProcessingJobs)

-- | If the result of the previous @ListProcessingJobs@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of processing jobs, use the token in the next request.
listProcessingJobs_nextToken :: Lens.Lens' ListProcessingJobs (Prelude.Maybe Prelude.Text)
listProcessingJobs_nextToken = Lens.lens (\ListProcessingJobs' {nextToken} -> nextToken) (\s@ListProcessingJobs' {} a -> s {nextToken = a} :: ListProcessingJobs)

-- | The field to sort results by. The default is @CreationTime@.
listProcessingJobs_sortBy :: Lens.Lens' ListProcessingJobs (Prelude.Maybe SortBy)
listProcessingJobs_sortBy = Lens.lens (\ListProcessingJobs' {sortBy} -> sortBy) (\s@ListProcessingJobs' {} a -> s {sortBy = a} :: ListProcessingJobs)

-- | The sort order for results. The default is @Ascending@.
listProcessingJobs_sortOrder :: Lens.Lens' ListProcessingJobs (Prelude.Maybe SortOrder)
listProcessingJobs_sortOrder = Lens.lens (\ListProcessingJobs' {sortOrder} -> sortOrder) (\s@ListProcessingJobs' {} a -> s {sortOrder = a} :: ListProcessingJobs)

-- | A filter that retrieves only processing jobs with a specific status.
listProcessingJobs_statusEquals :: Lens.Lens' ListProcessingJobs (Prelude.Maybe ProcessingJobStatus)
listProcessingJobs_statusEquals = Lens.lens (\ListProcessingJobs' {statusEquals} -> statusEquals) (\s@ListProcessingJobs' {} a -> s {statusEquals = a} :: ListProcessingJobs)

instance Core.AWSPager ListProcessingJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listProcessingJobsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listProcessingJobsResponse_processingJobSummaries
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listProcessingJobs_nextToken
          Lens..~ rs
          Lens.^? listProcessingJobsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListProcessingJobs where
  type
    AWSResponse ListProcessingJobs =
      ListProcessingJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProcessingJobsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..?> "ProcessingJobSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListProcessingJobs where
  hashWithSalt _salt ListProcessingJobs' {..} =
    _salt `Prelude.hashWithSalt` creationTimeAfter
      `Prelude.hashWithSalt` creationTimeBefore
      `Prelude.hashWithSalt` lastModifiedTimeAfter
      `Prelude.hashWithSalt` lastModifiedTimeBefore
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nameContains
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` statusEquals

instance Prelude.NFData ListProcessingJobs where
  rnf ListProcessingJobs' {..} =
    Prelude.rnf creationTimeAfter
      `Prelude.seq` Prelude.rnf creationTimeBefore
      `Prelude.seq` Prelude.rnf lastModifiedTimeAfter
      `Prelude.seq` Prelude.rnf lastModifiedTimeBefore
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nameContains
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf statusEquals

instance Data.ToHeaders ListProcessingJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.ListProcessingJobs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListProcessingJobs where
  toJSON ListProcessingJobs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CreationTimeAfter" Data..=)
              Prelude.<$> creationTimeAfter,
            ("CreationTimeBefore" Data..=)
              Prelude.<$> creationTimeBefore,
            ("LastModifiedTimeAfter" Data..=)
              Prelude.<$> lastModifiedTimeAfter,
            ("LastModifiedTimeBefore" Data..=)
              Prelude.<$> lastModifiedTimeBefore,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NameContains" Data..=) Prelude.<$> nameContains,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SortBy" Data..=) Prelude.<$> sortBy,
            ("SortOrder" Data..=) Prelude.<$> sortOrder,
            ("StatusEquals" Data..=) Prelude.<$> statusEquals
          ]
      )

instance Data.ToPath ListProcessingJobs where
  toPath = Prelude.const "/"

instance Data.ToQuery ListProcessingJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListProcessingJobsResponse' smart constructor.
data ListProcessingJobsResponse = ListProcessingJobsResponse'
  { -- | If the response is truncated, Amazon SageMaker returns this token. To
    -- retrieve the next set of processing jobs, use it in the subsequent
    -- request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array of @ProcessingJobSummary@ objects, each listing a processing
    -- job.
    processingJobSummaries :: [ProcessingJobSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProcessingJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listProcessingJobsResponse_nextToken' - If the response is truncated, Amazon SageMaker returns this token. To
-- retrieve the next set of processing jobs, use it in the subsequent
-- request.
--
-- 'httpStatus', 'listProcessingJobsResponse_httpStatus' - The response's http status code.
--
-- 'processingJobSummaries', 'listProcessingJobsResponse_processingJobSummaries' - An array of @ProcessingJobSummary@ objects, each listing a processing
-- job.
newListProcessingJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListProcessingJobsResponse
newListProcessingJobsResponse pHttpStatus_ =
  ListProcessingJobsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      processingJobSummaries = Prelude.mempty
    }

-- | If the response is truncated, Amazon SageMaker returns this token. To
-- retrieve the next set of processing jobs, use it in the subsequent
-- request.
listProcessingJobsResponse_nextToken :: Lens.Lens' ListProcessingJobsResponse (Prelude.Maybe Prelude.Text)
listProcessingJobsResponse_nextToken = Lens.lens (\ListProcessingJobsResponse' {nextToken} -> nextToken) (\s@ListProcessingJobsResponse' {} a -> s {nextToken = a} :: ListProcessingJobsResponse)

-- | The response's http status code.
listProcessingJobsResponse_httpStatus :: Lens.Lens' ListProcessingJobsResponse Prelude.Int
listProcessingJobsResponse_httpStatus = Lens.lens (\ListProcessingJobsResponse' {httpStatus} -> httpStatus) (\s@ListProcessingJobsResponse' {} a -> s {httpStatus = a} :: ListProcessingJobsResponse)

-- | An array of @ProcessingJobSummary@ objects, each listing a processing
-- job.
listProcessingJobsResponse_processingJobSummaries :: Lens.Lens' ListProcessingJobsResponse [ProcessingJobSummary]
listProcessingJobsResponse_processingJobSummaries = Lens.lens (\ListProcessingJobsResponse' {processingJobSummaries} -> processingJobSummaries) (\s@ListProcessingJobsResponse' {} a -> s {processingJobSummaries = a} :: ListProcessingJobsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListProcessingJobsResponse where
  rnf ListProcessingJobsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf processingJobSummaries
