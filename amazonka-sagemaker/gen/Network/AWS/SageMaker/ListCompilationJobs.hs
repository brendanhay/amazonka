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
-- Module      : Network.AWS.SageMaker.ListCompilationJobs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists model compilation jobs that satisfy various filters.
--
-- To create a model compilation job, use CreateCompilationJob. To get
-- information about a particular model compilation job you have created,
-- use DescribeCompilationJob.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListCompilationJobs
  ( -- * Creating a Request
    ListCompilationJobs (..),
    newListCompilationJobs,

    -- * Request Lenses
    listCompilationJobs_lastModifiedTimeBefore,
    listCompilationJobs_sortOrder,
    listCompilationJobs_nextToken,
    listCompilationJobs_nameContains,
    listCompilationJobs_maxResults,
    listCompilationJobs_creationTimeBefore,
    listCompilationJobs_lastModifiedTimeAfter,
    listCompilationJobs_sortBy,
    listCompilationJobs_statusEquals,
    listCompilationJobs_creationTimeAfter,

    -- * Destructuring the Response
    ListCompilationJobsResponse (..),
    newListCompilationJobsResponse,

    -- * Response Lenses
    listCompilationJobsResponse_nextToken,
    listCompilationJobsResponse_httpStatus,
    listCompilationJobsResponse_compilationJobSummaries,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListCompilationJobs' smart constructor.
data ListCompilationJobs = ListCompilationJobs'
  { -- | A filter that returns the model compilation jobs that were modified
    -- before a specified time.
    lastModifiedTimeBefore :: Core.Maybe Core.POSIX,
    -- | The sort order for results. The default is @Ascending@.
    sortOrder :: Core.Maybe SortOrder,
    -- | If the result of the previous @ListCompilationJobs@ request was
    -- truncated, the response includes a @NextToken@. To retrieve the next set
    -- of model compilation jobs, use the token in the next request.
    nextToken :: Core.Maybe Core.Text,
    -- | A filter that returns the model compilation jobs whose name contains a
    -- specified string.
    nameContains :: Core.Maybe Core.Text,
    -- | The maximum number of model compilation jobs to return in the response.
    maxResults :: Core.Maybe Core.Natural,
    -- | A filter that returns the model compilation jobs that were created
    -- before a specified time.
    creationTimeBefore :: Core.Maybe Core.POSIX,
    -- | A filter that returns the model compilation jobs that were modified
    -- after a specified time.
    lastModifiedTimeAfter :: Core.Maybe Core.POSIX,
    -- | The field by which to sort results. The default is @CreationTime@.
    sortBy :: Core.Maybe ListCompilationJobsSortBy,
    -- | A filter that retrieves model compilation jobs with a specific
    -- DescribeCompilationJobResponse$CompilationJobStatus status.
    statusEquals :: Core.Maybe CompilationJobStatus,
    -- | A filter that returns the model compilation jobs that were created after
    -- a specified time.
    creationTimeAfter :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListCompilationJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedTimeBefore', 'listCompilationJobs_lastModifiedTimeBefore' - A filter that returns the model compilation jobs that were modified
-- before a specified time.
--
-- 'sortOrder', 'listCompilationJobs_sortOrder' - The sort order for results. The default is @Ascending@.
--
-- 'nextToken', 'listCompilationJobs_nextToken' - If the result of the previous @ListCompilationJobs@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of model compilation jobs, use the token in the next request.
--
-- 'nameContains', 'listCompilationJobs_nameContains' - A filter that returns the model compilation jobs whose name contains a
-- specified string.
--
-- 'maxResults', 'listCompilationJobs_maxResults' - The maximum number of model compilation jobs to return in the response.
--
-- 'creationTimeBefore', 'listCompilationJobs_creationTimeBefore' - A filter that returns the model compilation jobs that were created
-- before a specified time.
--
-- 'lastModifiedTimeAfter', 'listCompilationJobs_lastModifiedTimeAfter' - A filter that returns the model compilation jobs that were modified
-- after a specified time.
--
-- 'sortBy', 'listCompilationJobs_sortBy' - The field by which to sort results. The default is @CreationTime@.
--
-- 'statusEquals', 'listCompilationJobs_statusEquals' - A filter that retrieves model compilation jobs with a specific
-- DescribeCompilationJobResponse$CompilationJobStatus status.
--
-- 'creationTimeAfter', 'listCompilationJobs_creationTimeAfter' - A filter that returns the model compilation jobs that were created after
-- a specified time.
newListCompilationJobs ::
  ListCompilationJobs
newListCompilationJobs =
  ListCompilationJobs'
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

-- | A filter that returns the model compilation jobs that were modified
-- before a specified time.
listCompilationJobs_lastModifiedTimeBefore :: Lens.Lens' ListCompilationJobs (Core.Maybe Core.UTCTime)
listCompilationJobs_lastModifiedTimeBefore = Lens.lens (\ListCompilationJobs' {lastModifiedTimeBefore} -> lastModifiedTimeBefore) (\s@ListCompilationJobs' {} a -> s {lastModifiedTimeBefore = a} :: ListCompilationJobs) Core.. Lens.mapping Core._Time

-- | The sort order for results. The default is @Ascending@.
listCompilationJobs_sortOrder :: Lens.Lens' ListCompilationJobs (Core.Maybe SortOrder)
listCompilationJobs_sortOrder = Lens.lens (\ListCompilationJobs' {sortOrder} -> sortOrder) (\s@ListCompilationJobs' {} a -> s {sortOrder = a} :: ListCompilationJobs)

-- | If the result of the previous @ListCompilationJobs@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of model compilation jobs, use the token in the next request.
listCompilationJobs_nextToken :: Lens.Lens' ListCompilationJobs (Core.Maybe Core.Text)
listCompilationJobs_nextToken = Lens.lens (\ListCompilationJobs' {nextToken} -> nextToken) (\s@ListCompilationJobs' {} a -> s {nextToken = a} :: ListCompilationJobs)

-- | A filter that returns the model compilation jobs whose name contains a
-- specified string.
listCompilationJobs_nameContains :: Lens.Lens' ListCompilationJobs (Core.Maybe Core.Text)
listCompilationJobs_nameContains = Lens.lens (\ListCompilationJobs' {nameContains} -> nameContains) (\s@ListCompilationJobs' {} a -> s {nameContains = a} :: ListCompilationJobs)

-- | The maximum number of model compilation jobs to return in the response.
listCompilationJobs_maxResults :: Lens.Lens' ListCompilationJobs (Core.Maybe Core.Natural)
listCompilationJobs_maxResults = Lens.lens (\ListCompilationJobs' {maxResults} -> maxResults) (\s@ListCompilationJobs' {} a -> s {maxResults = a} :: ListCompilationJobs)

-- | A filter that returns the model compilation jobs that were created
-- before a specified time.
listCompilationJobs_creationTimeBefore :: Lens.Lens' ListCompilationJobs (Core.Maybe Core.UTCTime)
listCompilationJobs_creationTimeBefore = Lens.lens (\ListCompilationJobs' {creationTimeBefore} -> creationTimeBefore) (\s@ListCompilationJobs' {} a -> s {creationTimeBefore = a} :: ListCompilationJobs) Core.. Lens.mapping Core._Time

-- | A filter that returns the model compilation jobs that were modified
-- after a specified time.
listCompilationJobs_lastModifiedTimeAfter :: Lens.Lens' ListCompilationJobs (Core.Maybe Core.UTCTime)
listCompilationJobs_lastModifiedTimeAfter = Lens.lens (\ListCompilationJobs' {lastModifiedTimeAfter} -> lastModifiedTimeAfter) (\s@ListCompilationJobs' {} a -> s {lastModifiedTimeAfter = a} :: ListCompilationJobs) Core.. Lens.mapping Core._Time

-- | The field by which to sort results. The default is @CreationTime@.
listCompilationJobs_sortBy :: Lens.Lens' ListCompilationJobs (Core.Maybe ListCompilationJobsSortBy)
listCompilationJobs_sortBy = Lens.lens (\ListCompilationJobs' {sortBy} -> sortBy) (\s@ListCompilationJobs' {} a -> s {sortBy = a} :: ListCompilationJobs)

-- | A filter that retrieves model compilation jobs with a specific
-- DescribeCompilationJobResponse$CompilationJobStatus status.
listCompilationJobs_statusEquals :: Lens.Lens' ListCompilationJobs (Core.Maybe CompilationJobStatus)
listCompilationJobs_statusEquals = Lens.lens (\ListCompilationJobs' {statusEquals} -> statusEquals) (\s@ListCompilationJobs' {} a -> s {statusEquals = a} :: ListCompilationJobs)

-- | A filter that returns the model compilation jobs that were created after
-- a specified time.
listCompilationJobs_creationTimeAfter :: Lens.Lens' ListCompilationJobs (Core.Maybe Core.UTCTime)
listCompilationJobs_creationTimeAfter = Lens.lens (\ListCompilationJobs' {creationTimeAfter} -> creationTimeAfter) (\s@ListCompilationJobs' {} a -> s {creationTimeAfter = a} :: ListCompilationJobs) Core.. Lens.mapping Core._Time

instance Core.AWSPager ListCompilationJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCompilationJobsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^. listCompilationJobsResponse_compilationJobSummaries
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listCompilationJobs_nextToken
          Lens..~ rs
          Lens.^? listCompilationJobsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListCompilationJobs where
  type
    AWSResponse ListCompilationJobs =
      ListCompilationJobsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCompilationJobsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> ( x Core..?> "CompilationJobSummaries"
                         Core..!@ Core.mempty
                     )
      )

instance Core.Hashable ListCompilationJobs

instance Core.NFData ListCompilationJobs

instance Core.ToHeaders ListCompilationJobs where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.ListCompilationJobs" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListCompilationJobs where
  toJSON ListCompilationJobs' {..} =
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

instance Core.ToPath ListCompilationJobs where
  toPath = Core.const "/"

instance Core.ToQuery ListCompilationJobs where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListCompilationJobsResponse' smart constructor.
data ListCompilationJobsResponse = ListCompilationJobsResponse'
  { -- | If the response is truncated, Amazon SageMaker returns this @NextToken@.
    -- To retrieve the next set of model compilation jobs, use this token in
    -- the next request.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | An array of CompilationJobSummary objects, each describing a model
    -- compilation job.
    compilationJobSummaries :: [CompilationJobSummary]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListCompilationJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCompilationJobsResponse_nextToken' - If the response is truncated, Amazon SageMaker returns this @NextToken@.
-- To retrieve the next set of model compilation jobs, use this token in
-- the next request.
--
-- 'httpStatus', 'listCompilationJobsResponse_httpStatus' - The response's http status code.
--
-- 'compilationJobSummaries', 'listCompilationJobsResponse_compilationJobSummaries' - An array of CompilationJobSummary objects, each describing a model
-- compilation job.
newListCompilationJobsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListCompilationJobsResponse
newListCompilationJobsResponse pHttpStatus_ =
  ListCompilationJobsResponse'
    { nextToken =
        Core.Nothing,
      httpStatus = pHttpStatus_,
      compilationJobSummaries = Core.mempty
    }

-- | If the response is truncated, Amazon SageMaker returns this @NextToken@.
-- To retrieve the next set of model compilation jobs, use this token in
-- the next request.
listCompilationJobsResponse_nextToken :: Lens.Lens' ListCompilationJobsResponse (Core.Maybe Core.Text)
listCompilationJobsResponse_nextToken = Lens.lens (\ListCompilationJobsResponse' {nextToken} -> nextToken) (\s@ListCompilationJobsResponse' {} a -> s {nextToken = a} :: ListCompilationJobsResponse)

-- | The response's http status code.
listCompilationJobsResponse_httpStatus :: Lens.Lens' ListCompilationJobsResponse Core.Int
listCompilationJobsResponse_httpStatus = Lens.lens (\ListCompilationJobsResponse' {httpStatus} -> httpStatus) (\s@ListCompilationJobsResponse' {} a -> s {httpStatus = a} :: ListCompilationJobsResponse)

-- | An array of CompilationJobSummary objects, each describing a model
-- compilation job.
listCompilationJobsResponse_compilationJobSummaries :: Lens.Lens' ListCompilationJobsResponse [CompilationJobSummary]
listCompilationJobsResponse_compilationJobSummaries = Lens.lens (\ListCompilationJobsResponse' {compilationJobSummaries} -> compilationJobSummaries) (\s@ListCompilationJobsResponse' {} a -> s {compilationJobSummaries = a} :: ListCompilationJobsResponse) Core.. Lens._Coerce

instance Core.NFData ListCompilationJobsResponse
