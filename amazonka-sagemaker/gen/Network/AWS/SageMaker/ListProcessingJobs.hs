{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SageMaker.ListProcessingJobs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists processing jobs that satisfy various filters.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListProcessingJobs
  ( -- * Creating a Request
    ListProcessingJobs (..),
    newListProcessingJobs,

    -- * Request Lenses
    listProcessingJobs_lastModifiedTimeBefore,
    listProcessingJobs_sortOrder,
    listProcessingJobs_nextToken,
    listProcessingJobs_nameContains,
    listProcessingJobs_maxResults,
    listProcessingJobs_creationTimeBefore,
    listProcessingJobs_lastModifiedTimeAfter,
    listProcessingJobs_sortBy,
    listProcessingJobs_statusEquals,
    listProcessingJobs_creationTimeAfter,

    -- * Destructuring the Response
    ListProcessingJobsResponse (..),
    newListProcessingJobsResponse,

    -- * Response Lenses
    listProcessingJobsResponse_nextToken,
    listProcessingJobsResponse_httpStatus,
    listProcessingJobsResponse_processingJobSummaries,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListProcessingJobs' smart constructor.
data ListProcessingJobs = ListProcessingJobs'
  { -- | A filter that returns only processing jobs modified before the specified
    -- time.
    lastModifiedTimeBefore :: Prelude.Maybe Prelude.POSIX,
    -- | The sort order for results. The default is @Ascending@.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | If the result of the previous @ListProcessingJobs@ request was
    -- truncated, the response includes a @NextToken@. To retrieve the next set
    -- of processing jobs, use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A string in the processing job name. This filter returns only processing
    -- jobs whose name contains the specified string.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of processing jobs to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A filter that returns only processing jobs created after the specified
    -- time.
    creationTimeBefore :: Prelude.Maybe Prelude.POSIX,
    -- | A filter that returns only processing jobs modified after the specified
    -- time.
    lastModifiedTimeAfter :: Prelude.Maybe Prelude.POSIX,
    -- | The field to sort results by. The default is @CreationTime@.
    sortBy :: Prelude.Maybe SortBy,
    -- | A filter that retrieves only processing jobs with a specific status.
    statusEquals :: Prelude.Maybe ProcessingJobStatus,
    -- | A filter that returns only processing jobs created after the specified
    -- time.
    creationTimeAfter :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListProcessingJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedTimeBefore', 'listProcessingJobs_lastModifiedTimeBefore' - A filter that returns only processing jobs modified before the specified
-- time.
--
-- 'sortOrder', 'listProcessingJobs_sortOrder' - The sort order for results. The default is @Ascending@.
--
-- 'nextToken', 'listProcessingJobs_nextToken' - If the result of the previous @ListProcessingJobs@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of processing jobs, use the token in the next request.
--
-- 'nameContains', 'listProcessingJobs_nameContains' - A string in the processing job name. This filter returns only processing
-- jobs whose name contains the specified string.
--
-- 'maxResults', 'listProcessingJobs_maxResults' - The maximum number of processing jobs to return in the response.
--
-- 'creationTimeBefore', 'listProcessingJobs_creationTimeBefore' - A filter that returns only processing jobs created after the specified
-- time.
--
-- 'lastModifiedTimeAfter', 'listProcessingJobs_lastModifiedTimeAfter' - A filter that returns only processing jobs modified after the specified
-- time.
--
-- 'sortBy', 'listProcessingJobs_sortBy' - The field to sort results by. The default is @CreationTime@.
--
-- 'statusEquals', 'listProcessingJobs_statusEquals' - A filter that retrieves only processing jobs with a specific status.
--
-- 'creationTimeAfter', 'listProcessingJobs_creationTimeAfter' - A filter that returns only processing jobs created after the specified
-- time.
newListProcessingJobs ::
  ListProcessingJobs
newListProcessingJobs =
  ListProcessingJobs'
    { lastModifiedTimeBefore =
        Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      nameContains = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      lastModifiedTimeAfter = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      statusEquals = Prelude.Nothing,
      creationTimeAfter = Prelude.Nothing
    }

-- | A filter that returns only processing jobs modified before the specified
-- time.
listProcessingJobs_lastModifiedTimeBefore :: Lens.Lens' ListProcessingJobs (Prelude.Maybe Prelude.UTCTime)
listProcessingJobs_lastModifiedTimeBefore = Lens.lens (\ListProcessingJobs' {lastModifiedTimeBefore} -> lastModifiedTimeBefore) (\s@ListProcessingJobs' {} a -> s {lastModifiedTimeBefore = a} :: ListProcessingJobs) Prelude.. Lens.mapping Prelude._Time

-- | The sort order for results. The default is @Ascending@.
listProcessingJobs_sortOrder :: Lens.Lens' ListProcessingJobs (Prelude.Maybe SortOrder)
listProcessingJobs_sortOrder = Lens.lens (\ListProcessingJobs' {sortOrder} -> sortOrder) (\s@ListProcessingJobs' {} a -> s {sortOrder = a} :: ListProcessingJobs)

-- | If the result of the previous @ListProcessingJobs@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of processing jobs, use the token in the next request.
listProcessingJobs_nextToken :: Lens.Lens' ListProcessingJobs (Prelude.Maybe Prelude.Text)
listProcessingJobs_nextToken = Lens.lens (\ListProcessingJobs' {nextToken} -> nextToken) (\s@ListProcessingJobs' {} a -> s {nextToken = a} :: ListProcessingJobs)

-- | A string in the processing job name. This filter returns only processing
-- jobs whose name contains the specified string.
listProcessingJobs_nameContains :: Lens.Lens' ListProcessingJobs (Prelude.Maybe Prelude.Text)
listProcessingJobs_nameContains = Lens.lens (\ListProcessingJobs' {nameContains} -> nameContains) (\s@ListProcessingJobs' {} a -> s {nameContains = a} :: ListProcessingJobs)

-- | The maximum number of processing jobs to return in the response.
listProcessingJobs_maxResults :: Lens.Lens' ListProcessingJobs (Prelude.Maybe Prelude.Natural)
listProcessingJobs_maxResults = Lens.lens (\ListProcessingJobs' {maxResults} -> maxResults) (\s@ListProcessingJobs' {} a -> s {maxResults = a} :: ListProcessingJobs)

-- | A filter that returns only processing jobs created after the specified
-- time.
listProcessingJobs_creationTimeBefore :: Lens.Lens' ListProcessingJobs (Prelude.Maybe Prelude.UTCTime)
listProcessingJobs_creationTimeBefore = Lens.lens (\ListProcessingJobs' {creationTimeBefore} -> creationTimeBefore) (\s@ListProcessingJobs' {} a -> s {creationTimeBefore = a} :: ListProcessingJobs) Prelude.. Lens.mapping Prelude._Time

-- | A filter that returns only processing jobs modified after the specified
-- time.
listProcessingJobs_lastModifiedTimeAfter :: Lens.Lens' ListProcessingJobs (Prelude.Maybe Prelude.UTCTime)
listProcessingJobs_lastModifiedTimeAfter = Lens.lens (\ListProcessingJobs' {lastModifiedTimeAfter} -> lastModifiedTimeAfter) (\s@ListProcessingJobs' {} a -> s {lastModifiedTimeAfter = a} :: ListProcessingJobs) Prelude.. Lens.mapping Prelude._Time

-- | The field to sort results by. The default is @CreationTime@.
listProcessingJobs_sortBy :: Lens.Lens' ListProcessingJobs (Prelude.Maybe SortBy)
listProcessingJobs_sortBy = Lens.lens (\ListProcessingJobs' {sortBy} -> sortBy) (\s@ListProcessingJobs' {} a -> s {sortBy = a} :: ListProcessingJobs)

-- | A filter that retrieves only processing jobs with a specific status.
listProcessingJobs_statusEquals :: Lens.Lens' ListProcessingJobs (Prelude.Maybe ProcessingJobStatus)
listProcessingJobs_statusEquals = Lens.lens (\ListProcessingJobs' {statusEquals} -> statusEquals) (\s@ListProcessingJobs' {} a -> s {statusEquals = a} :: ListProcessingJobs)

-- | A filter that returns only processing jobs created after the specified
-- time.
listProcessingJobs_creationTimeAfter :: Lens.Lens' ListProcessingJobs (Prelude.Maybe Prelude.UTCTime)
listProcessingJobs_creationTimeAfter = Lens.lens (\ListProcessingJobs' {creationTimeAfter} -> creationTimeAfter) (\s@ListProcessingJobs' {} a -> s {creationTimeAfter = a} :: ListProcessingJobs) Prelude.. Lens.mapping Prelude._Time

instance Pager.AWSPager ListProcessingJobs where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listProcessingJobsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^. listProcessingJobsResponse_processingJobSummaries
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listProcessingJobs_nextToken
          Lens..~ rs
          Lens.^? listProcessingJobsResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest ListProcessingJobs where
  type
    Rs ListProcessingJobs =
      ListProcessingJobsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProcessingJobsResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Prelude..?> "ProcessingJobSummaries"
                            Prelude..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListProcessingJobs

instance Prelude.NFData ListProcessingJobs

instance Prelude.ToHeaders ListProcessingJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "SageMaker.ListProcessingJobs" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListProcessingJobs where
  toJSON ListProcessingJobs' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("LastModifiedTimeBefore" Prelude..=)
              Prelude.<$> lastModifiedTimeBefore,
            ("SortOrder" Prelude..=) Prelude.<$> sortOrder,
            ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("NameContains" Prelude..=) Prelude.<$> nameContains,
            ("MaxResults" Prelude..=) Prelude.<$> maxResults,
            ("CreationTimeBefore" Prelude..=)
              Prelude.<$> creationTimeBefore,
            ("LastModifiedTimeAfter" Prelude..=)
              Prelude.<$> lastModifiedTimeAfter,
            ("SortBy" Prelude..=) Prelude.<$> sortBy,
            ("StatusEquals" Prelude..=) Prelude.<$> statusEquals,
            ("CreationTimeAfter" Prelude..=)
              Prelude.<$> creationTimeAfter
          ]
      )

instance Prelude.ToPath ListProcessingJobs where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListProcessingJobs where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
listProcessingJobsResponse_processingJobSummaries = Lens.lens (\ListProcessingJobsResponse' {processingJobSummaries} -> processingJobSummaries) (\s@ListProcessingJobsResponse' {} a -> s {processingJobSummaries = a} :: ListProcessingJobsResponse) Prelude.. Prelude._Coerce

instance Prelude.NFData ListProcessingJobsResponse
