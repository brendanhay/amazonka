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
-- Module      : Network.AWS.SageMaker.ListTransformJobs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists transform jobs.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListTransformJobs
  ( -- * Creating a Request
    ListTransformJobs (..),
    newListTransformJobs,

    -- * Request Lenses
    listTransformJobs_lastModifiedTimeBefore,
    listTransformJobs_sortOrder,
    listTransformJobs_nextToken,
    listTransformJobs_nameContains,
    listTransformJobs_maxResults,
    listTransformJobs_creationTimeBefore,
    listTransformJobs_lastModifiedTimeAfter,
    listTransformJobs_sortBy,
    listTransformJobs_statusEquals,
    listTransformJobs_creationTimeAfter,

    -- * Destructuring the Response
    ListTransformJobsResponse (..),
    newListTransformJobsResponse,

    -- * Response Lenses
    listTransformJobsResponse_nextToken,
    listTransformJobsResponse_httpStatus,
    listTransformJobsResponse_transformJobSummaries,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListTransformJobs' smart constructor.
data ListTransformJobs = ListTransformJobs'
  { -- | A filter that returns only transform jobs modified before the specified
    -- time.
    lastModifiedTimeBefore :: Core.Maybe Core.POSIX,
    -- | The sort order for results. The default is @Descending@.
    sortOrder :: Core.Maybe SortOrder,
    -- | If the result of the previous @ListTransformJobs@ request was truncated,
    -- the response includes a @NextToken@. To retrieve the next set of
    -- transform jobs, use the token in the next request.
    nextToken :: Core.Maybe Core.Text,
    -- | A string in the transform job name. This filter returns only transform
    -- jobs whose name contains the specified string.
    nameContains :: Core.Maybe Core.Text,
    -- | The maximum number of transform jobs to return in the response. The
    -- default value is @10@.
    maxResults :: Core.Maybe Core.Natural,
    -- | A filter that returns only transform jobs created before the specified
    -- time.
    creationTimeBefore :: Core.Maybe Core.POSIX,
    -- | A filter that returns only transform jobs modified after the specified
    -- time.
    lastModifiedTimeAfter :: Core.Maybe Core.POSIX,
    -- | The field to sort results by. The default is @CreationTime@.
    sortBy :: Core.Maybe SortBy,
    -- | A filter that retrieves only transform jobs with a specific status.
    statusEquals :: Core.Maybe TransformJobStatus,
    -- | A filter that returns only transform jobs created after the specified
    -- time.
    creationTimeAfter :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTransformJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedTimeBefore', 'listTransformJobs_lastModifiedTimeBefore' - A filter that returns only transform jobs modified before the specified
-- time.
--
-- 'sortOrder', 'listTransformJobs_sortOrder' - The sort order for results. The default is @Descending@.
--
-- 'nextToken', 'listTransformJobs_nextToken' - If the result of the previous @ListTransformJobs@ request was truncated,
-- the response includes a @NextToken@. To retrieve the next set of
-- transform jobs, use the token in the next request.
--
-- 'nameContains', 'listTransformJobs_nameContains' - A string in the transform job name. This filter returns only transform
-- jobs whose name contains the specified string.
--
-- 'maxResults', 'listTransformJobs_maxResults' - The maximum number of transform jobs to return in the response. The
-- default value is @10@.
--
-- 'creationTimeBefore', 'listTransformJobs_creationTimeBefore' - A filter that returns only transform jobs created before the specified
-- time.
--
-- 'lastModifiedTimeAfter', 'listTransformJobs_lastModifiedTimeAfter' - A filter that returns only transform jobs modified after the specified
-- time.
--
-- 'sortBy', 'listTransformJobs_sortBy' - The field to sort results by. The default is @CreationTime@.
--
-- 'statusEquals', 'listTransformJobs_statusEquals' - A filter that retrieves only transform jobs with a specific status.
--
-- 'creationTimeAfter', 'listTransformJobs_creationTimeAfter' - A filter that returns only transform jobs created after the specified
-- time.
newListTransformJobs ::
  ListTransformJobs
newListTransformJobs =
  ListTransformJobs'
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

-- | A filter that returns only transform jobs modified before the specified
-- time.
listTransformJobs_lastModifiedTimeBefore :: Lens.Lens' ListTransformJobs (Core.Maybe Core.UTCTime)
listTransformJobs_lastModifiedTimeBefore = Lens.lens (\ListTransformJobs' {lastModifiedTimeBefore} -> lastModifiedTimeBefore) (\s@ListTransformJobs' {} a -> s {lastModifiedTimeBefore = a} :: ListTransformJobs) Core.. Lens.mapping Core._Time

-- | The sort order for results. The default is @Descending@.
listTransformJobs_sortOrder :: Lens.Lens' ListTransformJobs (Core.Maybe SortOrder)
listTransformJobs_sortOrder = Lens.lens (\ListTransformJobs' {sortOrder} -> sortOrder) (\s@ListTransformJobs' {} a -> s {sortOrder = a} :: ListTransformJobs)

-- | If the result of the previous @ListTransformJobs@ request was truncated,
-- the response includes a @NextToken@. To retrieve the next set of
-- transform jobs, use the token in the next request.
listTransformJobs_nextToken :: Lens.Lens' ListTransformJobs (Core.Maybe Core.Text)
listTransformJobs_nextToken = Lens.lens (\ListTransformJobs' {nextToken} -> nextToken) (\s@ListTransformJobs' {} a -> s {nextToken = a} :: ListTransformJobs)

-- | A string in the transform job name. This filter returns only transform
-- jobs whose name contains the specified string.
listTransformJobs_nameContains :: Lens.Lens' ListTransformJobs (Core.Maybe Core.Text)
listTransformJobs_nameContains = Lens.lens (\ListTransformJobs' {nameContains} -> nameContains) (\s@ListTransformJobs' {} a -> s {nameContains = a} :: ListTransformJobs)

-- | The maximum number of transform jobs to return in the response. The
-- default value is @10@.
listTransformJobs_maxResults :: Lens.Lens' ListTransformJobs (Core.Maybe Core.Natural)
listTransformJobs_maxResults = Lens.lens (\ListTransformJobs' {maxResults} -> maxResults) (\s@ListTransformJobs' {} a -> s {maxResults = a} :: ListTransformJobs)

-- | A filter that returns only transform jobs created before the specified
-- time.
listTransformJobs_creationTimeBefore :: Lens.Lens' ListTransformJobs (Core.Maybe Core.UTCTime)
listTransformJobs_creationTimeBefore = Lens.lens (\ListTransformJobs' {creationTimeBefore} -> creationTimeBefore) (\s@ListTransformJobs' {} a -> s {creationTimeBefore = a} :: ListTransformJobs) Core.. Lens.mapping Core._Time

-- | A filter that returns only transform jobs modified after the specified
-- time.
listTransformJobs_lastModifiedTimeAfter :: Lens.Lens' ListTransformJobs (Core.Maybe Core.UTCTime)
listTransformJobs_lastModifiedTimeAfter = Lens.lens (\ListTransformJobs' {lastModifiedTimeAfter} -> lastModifiedTimeAfter) (\s@ListTransformJobs' {} a -> s {lastModifiedTimeAfter = a} :: ListTransformJobs) Core.. Lens.mapping Core._Time

-- | The field to sort results by. The default is @CreationTime@.
listTransformJobs_sortBy :: Lens.Lens' ListTransformJobs (Core.Maybe SortBy)
listTransformJobs_sortBy = Lens.lens (\ListTransformJobs' {sortBy} -> sortBy) (\s@ListTransformJobs' {} a -> s {sortBy = a} :: ListTransformJobs)

-- | A filter that retrieves only transform jobs with a specific status.
listTransformJobs_statusEquals :: Lens.Lens' ListTransformJobs (Core.Maybe TransformJobStatus)
listTransformJobs_statusEquals = Lens.lens (\ListTransformJobs' {statusEquals} -> statusEquals) (\s@ListTransformJobs' {} a -> s {statusEquals = a} :: ListTransformJobs)

-- | A filter that returns only transform jobs created after the specified
-- time.
listTransformJobs_creationTimeAfter :: Lens.Lens' ListTransformJobs (Core.Maybe Core.UTCTime)
listTransformJobs_creationTimeAfter = Lens.lens (\ListTransformJobs' {creationTimeAfter} -> creationTimeAfter) (\s@ListTransformJobs' {} a -> s {creationTimeAfter = a} :: ListTransformJobs) Core.. Lens.mapping Core._Time

instance Core.AWSPager ListTransformJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTransformJobsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^. listTransformJobsResponse_transformJobSummaries
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listTransformJobs_nextToken
          Lens..~ rs
          Lens.^? listTransformJobsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListTransformJobs where
  type
    AWSResponse ListTransformJobs =
      ListTransformJobsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTransformJobsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> ( x Core..?> "TransformJobSummaries"
                         Core..!@ Core.mempty
                     )
      )

instance Core.Hashable ListTransformJobs

instance Core.NFData ListTransformJobs

instance Core.ToHeaders ListTransformJobs where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.ListTransformJobs" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListTransformJobs where
  toJSON ListTransformJobs' {..} =
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

instance Core.ToPath ListTransformJobs where
  toPath = Core.const "/"

instance Core.ToQuery ListTransformJobs where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListTransformJobsResponse' smart constructor.
data ListTransformJobsResponse = ListTransformJobsResponse'
  { -- | If the response is truncated, Amazon SageMaker returns this token. To
    -- retrieve the next set of transform jobs, use it in the next request.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | An array of @TransformJobSummary@ objects.
    transformJobSummaries :: [TransformJobSummary]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTransformJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTransformJobsResponse_nextToken' - If the response is truncated, Amazon SageMaker returns this token. To
-- retrieve the next set of transform jobs, use it in the next request.
--
-- 'httpStatus', 'listTransformJobsResponse_httpStatus' - The response's http status code.
--
-- 'transformJobSummaries', 'listTransformJobsResponse_transformJobSummaries' - An array of @TransformJobSummary@ objects.
newListTransformJobsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListTransformJobsResponse
newListTransformJobsResponse pHttpStatus_ =
  ListTransformJobsResponse'
    { nextToken =
        Core.Nothing,
      httpStatus = pHttpStatus_,
      transformJobSummaries = Core.mempty
    }

-- | If the response is truncated, Amazon SageMaker returns this token. To
-- retrieve the next set of transform jobs, use it in the next request.
listTransformJobsResponse_nextToken :: Lens.Lens' ListTransformJobsResponse (Core.Maybe Core.Text)
listTransformJobsResponse_nextToken = Lens.lens (\ListTransformJobsResponse' {nextToken} -> nextToken) (\s@ListTransformJobsResponse' {} a -> s {nextToken = a} :: ListTransformJobsResponse)

-- | The response's http status code.
listTransformJobsResponse_httpStatus :: Lens.Lens' ListTransformJobsResponse Core.Int
listTransformJobsResponse_httpStatus = Lens.lens (\ListTransformJobsResponse' {httpStatus} -> httpStatus) (\s@ListTransformJobsResponse' {} a -> s {httpStatus = a} :: ListTransformJobsResponse)

-- | An array of @TransformJobSummary@ objects.
listTransformJobsResponse_transformJobSummaries :: Lens.Lens' ListTransformJobsResponse [TransformJobSummary]
listTransformJobsResponse_transformJobSummaries = Lens.lens (\ListTransformJobsResponse' {transformJobSummaries} -> transformJobSummaries) (\s@ListTransformJobsResponse' {} a -> s {transformJobSummaries = a} :: ListTransformJobsResponse) Core.. Lens._Coerce

instance Core.NFData ListTransformJobsResponse
