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
    listTransformJobs_nameContains,
    listTransformJobs_lastModifiedTimeBefore,
    listTransformJobs_creationTimeAfter,
    listTransformJobs_nextToken,
    listTransformJobs_sortOrder,
    listTransformJobs_lastModifiedTimeAfter,
    listTransformJobs_creationTimeBefore,
    listTransformJobs_statusEquals,
    listTransformJobs_maxResults,
    listTransformJobs_sortBy,

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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListTransformJobs' smart constructor.
data ListTransformJobs = ListTransformJobs'
  { -- | A string in the transform job name. This filter returns only transform
    -- jobs whose name contains the specified string.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | A filter that returns only transform jobs modified before the specified
    -- time.
    lastModifiedTimeBefore :: Prelude.Maybe Core.POSIX,
    -- | A filter that returns only transform jobs created after the specified
    -- time.
    creationTimeAfter :: Prelude.Maybe Core.POSIX,
    -- | If the result of the previous @ListTransformJobs@ request was truncated,
    -- the response includes a @NextToken@. To retrieve the next set of
    -- transform jobs, use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The sort order for results. The default is @Descending@.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | A filter that returns only transform jobs modified after the specified
    -- time.
    lastModifiedTimeAfter :: Prelude.Maybe Core.POSIX,
    -- | A filter that returns only transform jobs created before the specified
    -- time.
    creationTimeBefore :: Prelude.Maybe Core.POSIX,
    -- | A filter that retrieves only transform jobs with a specific status.
    statusEquals :: Prelude.Maybe TransformJobStatus,
    -- | The maximum number of transform jobs to return in the response. The
    -- default value is @10@.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The field to sort results by. The default is @CreationTime@.
    sortBy :: Prelude.Maybe SortBy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTransformJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nameContains', 'listTransformJobs_nameContains' - A string in the transform job name. This filter returns only transform
-- jobs whose name contains the specified string.
--
-- 'lastModifiedTimeBefore', 'listTransformJobs_lastModifiedTimeBefore' - A filter that returns only transform jobs modified before the specified
-- time.
--
-- 'creationTimeAfter', 'listTransformJobs_creationTimeAfter' - A filter that returns only transform jobs created after the specified
-- time.
--
-- 'nextToken', 'listTransformJobs_nextToken' - If the result of the previous @ListTransformJobs@ request was truncated,
-- the response includes a @NextToken@. To retrieve the next set of
-- transform jobs, use the token in the next request.
--
-- 'sortOrder', 'listTransformJobs_sortOrder' - The sort order for results. The default is @Descending@.
--
-- 'lastModifiedTimeAfter', 'listTransformJobs_lastModifiedTimeAfter' - A filter that returns only transform jobs modified after the specified
-- time.
--
-- 'creationTimeBefore', 'listTransformJobs_creationTimeBefore' - A filter that returns only transform jobs created before the specified
-- time.
--
-- 'statusEquals', 'listTransformJobs_statusEquals' - A filter that retrieves only transform jobs with a specific status.
--
-- 'maxResults', 'listTransformJobs_maxResults' - The maximum number of transform jobs to return in the response. The
-- default value is @10@.
--
-- 'sortBy', 'listTransformJobs_sortBy' - The field to sort results by. The default is @CreationTime@.
newListTransformJobs ::
  ListTransformJobs
newListTransformJobs =
  ListTransformJobs'
    { nameContains = Prelude.Nothing,
      lastModifiedTimeBefore = Prelude.Nothing,
      creationTimeAfter = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      lastModifiedTimeAfter = Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      statusEquals = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      sortBy = Prelude.Nothing
    }

-- | A string in the transform job name. This filter returns only transform
-- jobs whose name contains the specified string.
listTransformJobs_nameContains :: Lens.Lens' ListTransformJobs (Prelude.Maybe Prelude.Text)
listTransformJobs_nameContains = Lens.lens (\ListTransformJobs' {nameContains} -> nameContains) (\s@ListTransformJobs' {} a -> s {nameContains = a} :: ListTransformJobs)

-- | A filter that returns only transform jobs modified before the specified
-- time.
listTransformJobs_lastModifiedTimeBefore :: Lens.Lens' ListTransformJobs (Prelude.Maybe Prelude.UTCTime)
listTransformJobs_lastModifiedTimeBefore = Lens.lens (\ListTransformJobs' {lastModifiedTimeBefore} -> lastModifiedTimeBefore) (\s@ListTransformJobs' {} a -> s {lastModifiedTimeBefore = a} :: ListTransformJobs) Prelude.. Lens.mapping Core._Time

-- | A filter that returns only transform jobs created after the specified
-- time.
listTransformJobs_creationTimeAfter :: Lens.Lens' ListTransformJobs (Prelude.Maybe Prelude.UTCTime)
listTransformJobs_creationTimeAfter = Lens.lens (\ListTransformJobs' {creationTimeAfter} -> creationTimeAfter) (\s@ListTransformJobs' {} a -> s {creationTimeAfter = a} :: ListTransformJobs) Prelude.. Lens.mapping Core._Time

-- | If the result of the previous @ListTransformJobs@ request was truncated,
-- the response includes a @NextToken@. To retrieve the next set of
-- transform jobs, use the token in the next request.
listTransformJobs_nextToken :: Lens.Lens' ListTransformJobs (Prelude.Maybe Prelude.Text)
listTransformJobs_nextToken = Lens.lens (\ListTransformJobs' {nextToken} -> nextToken) (\s@ListTransformJobs' {} a -> s {nextToken = a} :: ListTransformJobs)

-- | The sort order for results. The default is @Descending@.
listTransformJobs_sortOrder :: Lens.Lens' ListTransformJobs (Prelude.Maybe SortOrder)
listTransformJobs_sortOrder = Lens.lens (\ListTransformJobs' {sortOrder} -> sortOrder) (\s@ListTransformJobs' {} a -> s {sortOrder = a} :: ListTransformJobs)

-- | A filter that returns only transform jobs modified after the specified
-- time.
listTransformJobs_lastModifiedTimeAfter :: Lens.Lens' ListTransformJobs (Prelude.Maybe Prelude.UTCTime)
listTransformJobs_lastModifiedTimeAfter = Lens.lens (\ListTransformJobs' {lastModifiedTimeAfter} -> lastModifiedTimeAfter) (\s@ListTransformJobs' {} a -> s {lastModifiedTimeAfter = a} :: ListTransformJobs) Prelude.. Lens.mapping Core._Time

-- | A filter that returns only transform jobs created before the specified
-- time.
listTransformJobs_creationTimeBefore :: Lens.Lens' ListTransformJobs (Prelude.Maybe Prelude.UTCTime)
listTransformJobs_creationTimeBefore = Lens.lens (\ListTransformJobs' {creationTimeBefore} -> creationTimeBefore) (\s@ListTransformJobs' {} a -> s {creationTimeBefore = a} :: ListTransformJobs) Prelude.. Lens.mapping Core._Time

-- | A filter that retrieves only transform jobs with a specific status.
listTransformJobs_statusEquals :: Lens.Lens' ListTransformJobs (Prelude.Maybe TransformJobStatus)
listTransformJobs_statusEquals = Lens.lens (\ListTransformJobs' {statusEquals} -> statusEquals) (\s@ListTransformJobs' {} a -> s {statusEquals = a} :: ListTransformJobs)

-- | The maximum number of transform jobs to return in the response. The
-- default value is @10@.
listTransformJobs_maxResults :: Lens.Lens' ListTransformJobs (Prelude.Maybe Prelude.Natural)
listTransformJobs_maxResults = Lens.lens (\ListTransformJobs' {maxResults} -> maxResults) (\s@ListTransformJobs' {} a -> s {maxResults = a} :: ListTransformJobs)

-- | The field to sort results by. The default is @CreationTime@.
listTransformJobs_sortBy :: Lens.Lens' ListTransformJobs (Prelude.Maybe SortBy)
listTransformJobs_sortBy = Lens.lens (\ListTransformJobs' {sortBy} -> sortBy) (\s@ListTransformJobs' {} a -> s {sortBy = a} :: ListTransformJobs)

instance Core.AWSPager ListTransformJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTransformJobsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listTransformJobsResponse_transformJobSummaries
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listTransformJobs_nextToken
          Lens..~ rs
          Lens.^? listTransformJobsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListTransformJobs where
  type
    AWSResponse ListTransformJobs =
      ListTransformJobsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTransformJobsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..?> "TransformJobSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListTransformJobs

instance Prelude.NFData ListTransformJobs

instance Core.ToHeaders ListTransformJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.ListTransformJobs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListTransformJobs where
  toJSON ListTransformJobs' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NameContains" Core..=) Prelude.<$> nameContains,
            ("LastModifiedTimeBefore" Core..=)
              Prelude.<$> lastModifiedTimeBefore,
            ("CreationTimeAfter" Core..=)
              Prelude.<$> creationTimeAfter,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("SortOrder" Core..=) Prelude.<$> sortOrder,
            ("LastModifiedTimeAfter" Core..=)
              Prelude.<$> lastModifiedTimeAfter,
            ("CreationTimeBefore" Core..=)
              Prelude.<$> creationTimeBefore,
            ("StatusEquals" Core..=) Prelude.<$> statusEquals,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("SortBy" Core..=) Prelude.<$> sortBy
          ]
      )

instance Core.ToPath ListTransformJobs where
  toPath = Prelude.const "/"

instance Core.ToQuery ListTransformJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTransformJobsResponse' smart constructor.
data ListTransformJobsResponse = ListTransformJobsResponse'
  { -- | If the response is truncated, Amazon SageMaker returns this token. To
    -- retrieve the next set of transform jobs, use it in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array of @TransformJobSummary@ objects.
    transformJobSummaries :: [TransformJobSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListTransformJobsResponse
newListTransformJobsResponse pHttpStatus_ =
  ListTransformJobsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      transformJobSummaries = Prelude.mempty
    }

-- | If the response is truncated, Amazon SageMaker returns this token. To
-- retrieve the next set of transform jobs, use it in the next request.
listTransformJobsResponse_nextToken :: Lens.Lens' ListTransformJobsResponse (Prelude.Maybe Prelude.Text)
listTransformJobsResponse_nextToken = Lens.lens (\ListTransformJobsResponse' {nextToken} -> nextToken) (\s@ListTransformJobsResponse' {} a -> s {nextToken = a} :: ListTransformJobsResponse)

-- | The response's http status code.
listTransformJobsResponse_httpStatus :: Lens.Lens' ListTransformJobsResponse Prelude.Int
listTransformJobsResponse_httpStatus = Lens.lens (\ListTransformJobsResponse' {httpStatus} -> httpStatus) (\s@ListTransformJobsResponse' {} a -> s {httpStatus = a} :: ListTransformJobsResponse)

-- | An array of @TransformJobSummary@ objects.
listTransformJobsResponse_transformJobSummaries :: Lens.Lens' ListTransformJobsResponse [TransformJobSummary]
listTransformJobsResponse_transformJobSummaries = Lens.lens (\ListTransformJobsResponse' {transformJobSummaries} -> transformJobSummaries) (\s@ListTransformJobsResponse' {} a -> s {transformJobSummaries = a} :: ListTransformJobsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListTransformJobsResponse
