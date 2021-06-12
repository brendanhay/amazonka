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
-- Module      : Network.AWS.SageMaker.ListHyperParameterTuningJobs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of HyperParameterTuningJobSummary objects that describe the
-- hyperparameter tuning jobs launched in your account.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListHyperParameterTuningJobs
  ( -- * Creating a Request
    ListHyperParameterTuningJobs (..),
    newListHyperParameterTuningJobs,

    -- * Request Lenses
    listHyperParameterTuningJobs_lastModifiedTimeBefore,
    listHyperParameterTuningJobs_sortOrder,
    listHyperParameterTuningJobs_nextToken,
    listHyperParameterTuningJobs_nameContains,
    listHyperParameterTuningJobs_maxResults,
    listHyperParameterTuningJobs_creationTimeBefore,
    listHyperParameterTuningJobs_lastModifiedTimeAfter,
    listHyperParameterTuningJobs_sortBy,
    listHyperParameterTuningJobs_statusEquals,
    listHyperParameterTuningJobs_creationTimeAfter,

    -- * Destructuring the Response
    ListHyperParameterTuningJobsResponse (..),
    newListHyperParameterTuningJobsResponse,

    -- * Response Lenses
    listHyperParameterTuningJobsResponse_nextToken,
    listHyperParameterTuningJobsResponse_httpStatus,
    listHyperParameterTuningJobsResponse_hyperParameterTuningJobSummaries,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListHyperParameterTuningJobs' smart constructor.
data ListHyperParameterTuningJobs = ListHyperParameterTuningJobs'
  { -- | A filter that returns only tuning jobs that were modified before the
    -- specified time.
    lastModifiedTimeBefore :: Core.Maybe Core.POSIX,
    -- | The sort order for results. The default is @Ascending@.
    sortOrder :: Core.Maybe SortOrder,
    -- | If the result of the previous @ListHyperParameterTuningJobs@ request was
    -- truncated, the response includes a @NextToken@. To retrieve the next set
    -- of tuning jobs, use the token in the next request.
    nextToken :: Core.Maybe Core.Text,
    -- | A string in the tuning job name. This filter returns only tuning jobs
    -- whose name contains the specified string.
    nameContains :: Core.Maybe Core.Text,
    -- | The maximum number of tuning jobs to return. The default value is 10.
    maxResults :: Core.Maybe Core.Natural,
    -- | A filter that returns only tuning jobs that were created before the
    -- specified time.
    creationTimeBefore :: Core.Maybe Core.POSIX,
    -- | A filter that returns only tuning jobs that were modified after the
    -- specified time.
    lastModifiedTimeAfter :: Core.Maybe Core.POSIX,
    -- | The field to sort results by. The default is @Name@.
    sortBy :: Core.Maybe HyperParameterTuningJobSortByOptions,
    -- | A filter that returns only tuning jobs with the specified status.
    statusEquals :: Core.Maybe HyperParameterTuningJobStatus,
    -- | A filter that returns only tuning jobs that were created after the
    -- specified time.
    creationTimeAfter :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListHyperParameterTuningJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedTimeBefore', 'listHyperParameterTuningJobs_lastModifiedTimeBefore' - A filter that returns only tuning jobs that were modified before the
-- specified time.
--
-- 'sortOrder', 'listHyperParameterTuningJobs_sortOrder' - The sort order for results. The default is @Ascending@.
--
-- 'nextToken', 'listHyperParameterTuningJobs_nextToken' - If the result of the previous @ListHyperParameterTuningJobs@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of tuning jobs, use the token in the next request.
--
-- 'nameContains', 'listHyperParameterTuningJobs_nameContains' - A string in the tuning job name. This filter returns only tuning jobs
-- whose name contains the specified string.
--
-- 'maxResults', 'listHyperParameterTuningJobs_maxResults' - The maximum number of tuning jobs to return. The default value is 10.
--
-- 'creationTimeBefore', 'listHyperParameterTuningJobs_creationTimeBefore' - A filter that returns only tuning jobs that were created before the
-- specified time.
--
-- 'lastModifiedTimeAfter', 'listHyperParameterTuningJobs_lastModifiedTimeAfter' - A filter that returns only tuning jobs that were modified after the
-- specified time.
--
-- 'sortBy', 'listHyperParameterTuningJobs_sortBy' - The field to sort results by. The default is @Name@.
--
-- 'statusEquals', 'listHyperParameterTuningJobs_statusEquals' - A filter that returns only tuning jobs with the specified status.
--
-- 'creationTimeAfter', 'listHyperParameterTuningJobs_creationTimeAfter' - A filter that returns only tuning jobs that were created after the
-- specified time.
newListHyperParameterTuningJobs ::
  ListHyperParameterTuningJobs
newListHyperParameterTuningJobs =
  ListHyperParameterTuningJobs'
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

-- | A filter that returns only tuning jobs that were modified before the
-- specified time.
listHyperParameterTuningJobs_lastModifiedTimeBefore :: Lens.Lens' ListHyperParameterTuningJobs (Core.Maybe Core.UTCTime)
listHyperParameterTuningJobs_lastModifiedTimeBefore = Lens.lens (\ListHyperParameterTuningJobs' {lastModifiedTimeBefore} -> lastModifiedTimeBefore) (\s@ListHyperParameterTuningJobs' {} a -> s {lastModifiedTimeBefore = a} :: ListHyperParameterTuningJobs) Core.. Lens.mapping Core._Time

-- | The sort order for results. The default is @Ascending@.
listHyperParameterTuningJobs_sortOrder :: Lens.Lens' ListHyperParameterTuningJobs (Core.Maybe SortOrder)
listHyperParameterTuningJobs_sortOrder = Lens.lens (\ListHyperParameterTuningJobs' {sortOrder} -> sortOrder) (\s@ListHyperParameterTuningJobs' {} a -> s {sortOrder = a} :: ListHyperParameterTuningJobs)

-- | If the result of the previous @ListHyperParameterTuningJobs@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of tuning jobs, use the token in the next request.
listHyperParameterTuningJobs_nextToken :: Lens.Lens' ListHyperParameterTuningJobs (Core.Maybe Core.Text)
listHyperParameterTuningJobs_nextToken = Lens.lens (\ListHyperParameterTuningJobs' {nextToken} -> nextToken) (\s@ListHyperParameterTuningJobs' {} a -> s {nextToken = a} :: ListHyperParameterTuningJobs)

-- | A string in the tuning job name. This filter returns only tuning jobs
-- whose name contains the specified string.
listHyperParameterTuningJobs_nameContains :: Lens.Lens' ListHyperParameterTuningJobs (Core.Maybe Core.Text)
listHyperParameterTuningJobs_nameContains = Lens.lens (\ListHyperParameterTuningJobs' {nameContains} -> nameContains) (\s@ListHyperParameterTuningJobs' {} a -> s {nameContains = a} :: ListHyperParameterTuningJobs)

-- | The maximum number of tuning jobs to return. The default value is 10.
listHyperParameterTuningJobs_maxResults :: Lens.Lens' ListHyperParameterTuningJobs (Core.Maybe Core.Natural)
listHyperParameterTuningJobs_maxResults = Lens.lens (\ListHyperParameterTuningJobs' {maxResults} -> maxResults) (\s@ListHyperParameterTuningJobs' {} a -> s {maxResults = a} :: ListHyperParameterTuningJobs)

-- | A filter that returns only tuning jobs that were created before the
-- specified time.
listHyperParameterTuningJobs_creationTimeBefore :: Lens.Lens' ListHyperParameterTuningJobs (Core.Maybe Core.UTCTime)
listHyperParameterTuningJobs_creationTimeBefore = Lens.lens (\ListHyperParameterTuningJobs' {creationTimeBefore} -> creationTimeBefore) (\s@ListHyperParameterTuningJobs' {} a -> s {creationTimeBefore = a} :: ListHyperParameterTuningJobs) Core.. Lens.mapping Core._Time

-- | A filter that returns only tuning jobs that were modified after the
-- specified time.
listHyperParameterTuningJobs_lastModifiedTimeAfter :: Lens.Lens' ListHyperParameterTuningJobs (Core.Maybe Core.UTCTime)
listHyperParameterTuningJobs_lastModifiedTimeAfter = Lens.lens (\ListHyperParameterTuningJobs' {lastModifiedTimeAfter} -> lastModifiedTimeAfter) (\s@ListHyperParameterTuningJobs' {} a -> s {lastModifiedTimeAfter = a} :: ListHyperParameterTuningJobs) Core.. Lens.mapping Core._Time

-- | The field to sort results by. The default is @Name@.
listHyperParameterTuningJobs_sortBy :: Lens.Lens' ListHyperParameterTuningJobs (Core.Maybe HyperParameterTuningJobSortByOptions)
listHyperParameterTuningJobs_sortBy = Lens.lens (\ListHyperParameterTuningJobs' {sortBy} -> sortBy) (\s@ListHyperParameterTuningJobs' {} a -> s {sortBy = a} :: ListHyperParameterTuningJobs)

-- | A filter that returns only tuning jobs with the specified status.
listHyperParameterTuningJobs_statusEquals :: Lens.Lens' ListHyperParameterTuningJobs (Core.Maybe HyperParameterTuningJobStatus)
listHyperParameterTuningJobs_statusEquals = Lens.lens (\ListHyperParameterTuningJobs' {statusEquals} -> statusEquals) (\s@ListHyperParameterTuningJobs' {} a -> s {statusEquals = a} :: ListHyperParameterTuningJobs)

-- | A filter that returns only tuning jobs that were created after the
-- specified time.
listHyperParameterTuningJobs_creationTimeAfter :: Lens.Lens' ListHyperParameterTuningJobs (Core.Maybe Core.UTCTime)
listHyperParameterTuningJobs_creationTimeAfter = Lens.lens (\ListHyperParameterTuningJobs' {creationTimeAfter} -> creationTimeAfter) (\s@ListHyperParameterTuningJobs' {} a -> s {creationTimeAfter = a} :: ListHyperParameterTuningJobs) Core.. Lens.mapping Core._Time

instance Core.AWSPager ListHyperParameterTuningJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listHyperParameterTuningJobsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^. listHyperParameterTuningJobsResponse_hyperParameterTuningJobSummaries
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listHyperParameterTuningJobs_nextToken
          Lens..~ rs
          Lens.^? listHyperParameterTuningJobsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListHyperParameterTuningJobs where
  type
    AWSResponse ListHyperParameterTuningJobs =
      ListHyperParameterTuningJobsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListHyperParameterTuningJobsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> ( x Core..?> "HyperParameterTuningJobSummaries"
                         Core..!@ Core.mempty
                     )
      )

instance Core.Hashable ListHyperParameterTuningJobs

instance Core.NFData ListHyperParameterTuningJobs

instance Core.ToHeaders ListHyperParameterTuningJobs where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.ListHyperParameterTuningJobs" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListHyperParameterTuningJobs where
  toJSON ListHyperParameterTuningJobs' {..} =
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

instance Core.ToPath ListHyperParameterTuningJobs where
  toPath = Core.const "/"

instance Core.ToQuery ListHyperParameterTuningJobs where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListHyperParameterTuningJobsResponse' smart constructor.
data ListHyperParameterTuningJobsResponse = ListHyperParameterTuningJobsResponse'
  { -- | If the result of this @ListHyperParameterTuningJobs@ request was
    -- truncated, the response includes a @NextToken@. To retrieve the next set
    -- of tuning jobs, use the token in the next request.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A list of HyperParameterTuningJobSummary objects that describe the
    -- tuning jobs that the @ListHyperParameterTuningJobs@ request returned.
    hyperParameterTuningJobSummaries :: [HyperParameterTuningJobSummary]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListHyperParameterTuningJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listHyperParameterTuningJobsResponse_nextToken' - If the result of this @ListHyperParameterTuningJobs@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of tuning jobs, use the token in the next request.
--
-- 'httpStatus', 'listHyperParameterTuningJobsResponse_httpStatus' - The response's http status code.
--
-- 'hyperParameterTuningJobSummaries', 'listHyperParameterTuningJobsResponse_hyperParameterTuningJobSummaries' - A list of HyperParameterTuningJobSummary objects that describe the
-- tuning jobs that the @ListHyperParameterTuningJobs@ request returned.
newListHyperParameterTuningJobsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListHyperParameterTuningJobsResponse
newListHyperParameterTuningJobsResponse pHttpStatus_ =
  ListHyperParameterTuningJobsResponse'
    { nextToken =
        Core.Nothing,
      httpStatus = pHttpStatus_,
      hyperParameterTuningJobSummaries =
        Core.mempty
    }

-- | If the result of this @ListHyperParameterTuningJobs@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of tuning jobs, use the token in the next request.
listHyperParameterTuningJobsResponse_nextToken :: Lens.Lens' ListHyperParameterTuningJobsResponse (Core.Maybe Core.Text)
listHyperParameterTuningJobsResponse_nextToken = Lens.lens (\ListHyperParameterTuningJobsResponse' {nextToken} -> nextToken) (\s@ListHyperParameterTuningJobsResponse' {} a -> s {nextToken = a} :: ListHyperParameterTuningJobsResponse)

-- | The response's http status code.
listHyperParameterTuningJobsResponse_httpStatus :: Lens.Lens' ListHyperParameterTuningJobsResponse Core.Int
listHyperParameterTuningJobsResponse_httpStatus = Lens.lens (\ListHyperParameterTuningJobsResponse' {httpStatus} -> httpStatus) (\s@ListHyperParameterTuningJobsResponse' {} a -> s {httpStatus = a} :: ListHyperParameterTuningJobsResponse)

-- | A list of HyperParameterTuningJobSummary objects that describe the
-- tuning jobs that the @ListHyperParameterTuningJobs@ request returned.
listHyperParameterTuningJobsResponse_hyperParameterTuningJobSummaries :: Lens.Lens' ListHyperParameterTuningJobsResponse [HyperParameterTuningJobSummary]
listHyperParameterTuningJobsResponse_hyperParameterTuningJobSummaries = Lens.lens (\ListHyperParameterTuningJobsResponse' {hyperParameterTuningJobSummaries} -> hyperParameterTuningJobSummaries) (\s@ListHyperParameterTuningJobsResponse' {} a -> s {hyperParameterTuningJobSummaries = a} :: ListHyperParameterTuningJobsResponse) Core.. Lens._Coerce

instance
  Core.NFData
    ListHyperParameterTuningJobsResponse
