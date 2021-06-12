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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListProcessingJobs' smart constructor.
data ListProcessingJobs = ListProcessingJobs'
  { -- | A filter that returns only processing jobs modified before the specified
    -- time.
    lastModifiedTimeBefore :: Core.Maybe Core.POSIX,
    -- | The sort order for results. The default is @Ascending@.
    sortOrder :: Core.Maybe SortOrder,
    -- | If the result of the previous @ListProcessingJobs@ request was
    -- truncated, the response includes a @NextToken@. To retrieve the next set
    -- of processing jobs, use the token in the next request.
    nextToken :: Core.Maybe Core.Text,
    -- | A string in the processing job name. This filter returns only processing
    -- jobs whose name contains the specified string.
    nameContains :: Core.Maybe Core.Text,
    -- | The maximum number of processing jobs to return in the response.
    maxResults :: Core.Maybe Core.Natural,
    -- | A filter that returns only processing jobs created after the specified
    -- time.
    creationTimeBefore :: Core.Maybe Core.POSIX,
    -- | A filter that returns only processing jobs modified after the specified
    -- time.
    lastModifiedTimeAfter :: Core.Maybe Core.POSIX,
    -- | The field to sort results by. The default is @CreationTime@.
    sortBy :: Core.Maybe SortBy,
    -- | A filter that retrieves only processing jobs with a specific status.
    statusEquals :: Core.Maybe ProcessingJobStatus,
    -- | A filter that returns only processing jobs created after the specified
    -- time.
    creationTimeAfter :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      sortOrder = Core.Nothing,
      nextToken = Core.Nothing,
      nameContains = Core.Nothing,
      maxResults = Core.Nothing,
      creationTimeBefore = Core.Nothing,
      lastModifiedTimeAfter = Core.Nothing,
      sortBy = Core.Nothing,
      statusEquals = Core.Nothing,
      creationTimeAfter = Core.Nothing
    }

-- | A filter that returns only processing jobs modified before the specified
-- time.
listProcessingJobs_lastModifiedTimeBefore :: Lens.Lens' ListProcessingJobs (Core.Maybe Core.UTCTime)
listProcessingJobs_lastModifiedTimeBefore = Lens.lens (\ListProcessingJobs' {lastModifiedTimeBefore} -> lastModifiedTimeBefore) (\s@ListProcessingJobs' {} a -> s {lastModifiedTimeBefore = a} :: ListProcessingJobs) Core.. Lens.mapping Core._Time

-- | The sort order for results. The default is @Ascending@.
listProcessingJobs_sortOrder :: Lens.Lens' ListProcessingJobs (Core.Maybe SortOrder)
listProcessingJobs_sortOrder = Lens.lens (\ListProcessingJobs' {sortOrder} -> sortOrder) (\s@ListProcessingJobs' {} a -> s {sortOrder = a} :: ListProcessingJobs)

-- | If the result of the previous @ListProcessingJobs@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of processing jobs, use the token in the next request.
listProcessingJobs_nextToken :: Lens.Lens' ListProcessingJobs (Core.Maybe Core.Text)
listProcessingJobs_nextToken = Lens.lens (\ListProcessingJobs' {nextToken} -> nextToken) (\s@ListProcessingJobs' {} a -> s {nextToken = a} :: ListProcessingJobs)

-- | A string in the processing job name. This filter returns only processing
-- jobs whose name contains the specified string.
listProcessingJobs_nameContains :: Lens.Lens' ListProcessingJobs (Core.Maybe Core.Text)
listProcessingJobs_nameContains = Lens.lens (\ListProcessingJobs' {nameContains} -> nameContains) (\s@ListProcessingJobs' {} a -> s {nameContains = a} :: ListProcessingJobs)

-- | The maximum number of processing jobs to return in the response.
listProcessingJobs_maxResults :: Lens.Lens' ListProcessingJobs (Core.Maybe Core.Natural)
listProcessingJobs_maxResults = Lens.lens (\ListProcessingJobs' {maxResults} -> maxResults) (\s@ListProcessingJobs' {} a -> s {maxResults = a} :: ListProcessingJobs)

-- | A filter that returns only processing jobs created after the specified
-- time.
listProcessingJobs_creationTimeBefore :: Lens.Lens' ListProcessingJobs (Core.Maybe Core.UTCTime)
listProcessingJobs_creationTimeBefore = Lens.lens (\ListProcessingJobs' {creationTimeBefore} -> creationTimeBefore) (\s@ListProcessingJobs' {} a -> s {creationTimeBefore = a} :: ListProcessingJobs) Core.. Lens.mapping Core._Time

-- | A filter that returns only processing jobs modified after the specified
-- time.
listProcessingJobs_lastModifiedTimeAfter :: Lens.Lens' ListProcessingJobs (Core.Maybe Core.UTCTime)
listProcessingJobs_lastModifiedTimeAfter = Lens.lens (\ListProcessingJobs' {lastModifiedTimeAfter} -> lastModifiedTimeAfter) (\s@ListProcessingJobs' {} a -> s {lastModifiedTimeAfter = a} :: ListProcessingJobs) Core.. Lens.mapping Core._Time

-- | The field to sort results by. The default is @CreationTime@.
listProcessingJobs_sortBy :: Lens.Lens' ListProcessingJobs (Core.Maybe SortBy)
listProcessingJobs_sortBy = Lens.lens (\ListProcessingJobs' {sortBy} -> sortBy) (\s@ListProcessingJobs' {} a -> s {sortBy = a} :: ListProcessingJobs)

-- | A filter that retrieves only processing jobs with a specific status.
listProcessingJobs_statusEquals :: Lens.Lens' ListProcessingJobs (Core.Maybe ProcessingJobStatus)
listProcessingJobs_statusEquals = Lens.lens (\ListProcessingJobs' {statusEquals} -> statusEquals) (\s@ListProcessingJobs' {} a -> s {statusEquals = a} :: ListProcessingJobs)

-- | A filter that returns only processing jobs created after the specified
-- time.
listProcessingJobs_creationTimeAfter :: Lens.Lens' ListProcessingJobs (Core.Maybe Core.UTCTime)
listProcessingJobs_creationTimeAfter = Lens.lens (\ListProcessingJobs' {creationTimeAfter} -> creationTimeAfter) (\s@ListProcessingJobs' {} a -> s {creationTimeAfter = a} :: ListProcessingJobs) Core.. Lens.mapping Core._Time

instance Core.AWSPager ListProcessingJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listProcessingJobsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^. listProcessingJobsResponse_processingJobSummaries
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listProcessingJobs_nextToken
          Lens..~ rs
          Lens.^? listProcessingJobsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListProcessingJobs where
  type
    AWSResponse ListProcessingJobs =
      ListProcessingJobsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProcessingJobsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> ( x Core..?> "ProcessingJobSummaries"
                         Core..!@ Core.mempty
                     )
      )

instance Core.Hashable ListProcessingJobs

instance Core.NFData ListProcessingJobs

instance Core.ToHeaders ListProcessingJobs where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.ListProcessingJobs" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListProcessingJobs where
  toJSON ListProcessingJobs' {..} =
    Core.object
      ( Core.catMaybes
          [ ("LastModifiedTimeBefore" Core..=)
              Core.<$> lastModifiedTimeBefore,
            ("SortOrder" Core..=) Core.<$> sortOrder,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("NameContains" Core..=) Core.<$> nameContains,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("CreationTimeBefore" Core..=)
              Core.<$> creationTimeBefore,
            ("LastModifiedTimeAfter" Core..=)
              Core.<$> lastModifiedTimeAfter,
            ("SortBy" Core..=) Core.<$> sortBy,
            ("StatusEquals" Core..=) Core.<$> statusEquals,
            ("CreationTimeAfter" Core..=)
              Core.<$> creationTimeAfter
          ]
      )

instance Core.ToPath ListProcessingJobs where
  toPath = Core.const "/"

instance Core.ToQuery ListProcessingJobs where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListProcessingJobsResponse' smart constructor.
data ListProcessingJobsResponse = ListProcessingJobsResponse'
  { -- | If the response is truncated, Amazon SageMaker returns this token. To
    -- retrieve the next set of processing jobs, use it in the subsequent
    -- request.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | An array of @ProcessingJobSummary@ objects, each listing a processing
    -- job.
    processingJobSummaries :: [ProcessingJobSummary]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListProcessingJobsResponse
newListProcessingJobsResponse pHttpStatus_ =
  ListProcessingJobsResponse'
    { nextToken =
        Core.Nothing,
      httpStatus = pHttpStatus_,
      processingJobSummaries = Core.mempty
    }

-- | If the response is truncated, Amazon SageMaker returns this token. To
-- retrieve the next set of processing jobs, use it in the subsequent
-- request.
listProcessingJobsResponse_nextToken :: Lens.Lens' ListProcessingJobsResponse (Core.Maybe Core.Text)
listProcessingJobsResponse_nextToken = Lens.lens (\ListProcessingJobsResponse' {nextToken} -> nextToken) (\s@ListProcessingJobsResponse' {} a -> s {nextToken = a} :: ListProcessingJobsResponse)

-- | The response's http status code.
listProcessingJobsResponse_httpStatus :: Lens.Lens' ListProcessingJobsResponse Core.Int
listProcessingJobsResponse_httpStatus = Lens.lens (\ListProcessingJobsResponse' {httpStatus} -> httpStatus) (\s@ListProcessingJobsResponse' {} a -> s {httpStatus = a} :: ListProcessingJobsResponse)

-- | An array of @ProcessingJobSummary@ objects, each listing a processing
-- job.
listProcessingJobsResponse_processingJobSummaries :: Lens.Lens' ListProcessingJobsResponse [ProcessingJobSummary]
listProcessingJobsResponse_processingJobSummaries = Lens.lens (\ListProcessingJobsResponse' {processingJobSummaries} -> processingJobSummaries) (\s@ListProcessingJobsResponse' {} a -> s {processingJobSummaries = a} :: ListProcessingJobsResponse) Core.. Lens._Coerce

instance Core.NFData ListProcessingJobsResponse
