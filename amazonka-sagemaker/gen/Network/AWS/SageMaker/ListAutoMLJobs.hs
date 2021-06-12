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
-- Module      : Network.AWS.SageMaker.ListAutoMLJobs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Request a list of jobs.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListAutoMLJobs
  ( -- * Creating a Request
    ListAutoMLJobs (..),
    newListAutoMLJobs,

    -- * Request Lenses
    listAutoMLJobs_lastModifiedTimeBefore,
    listAutoMLJobs_sortOrder,
    listAutoMLJobs_nextToken,
    listAutoMLJobs_nameContains,
    listAutoMLJobs_maxResults,
    listAutoMLJobs_creationTimeBefore,
    listAutoMLJobs_lastModifiedTimeAfter,
    listAutoMLJobs_sortBy,
    listAutoMLJobs_statusEquals,
    listAutoMLJobs_creationTimeAfter,

    -- * Destructuring the Response
    ListAutoMLJobsResponse (..),
    newListAutoMLJobsResponse,

    -- * Response Lenses
    listAutoMLJobsResponse_nextToken,
    listAutoMLJobsResponse_httpStatus,
    listAutoMLJobsResponse_autoMLJobSummaries,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListAutoMLJobs' smart constructor.
data ListAutoMLJobs = ListAutoMLJobs'
  { -- | Request a list of jobs, using a filter for time.
    lastModifiedTimeBefore :: Core.Maybe Core.POSIX,
    -- | The sort order for the results. The default is Descending.
    sortOrder :: Core.Maybe AutoMLSortOrder,
    -- | If the previous response was truncated, you receive this token. Use it
    -- in your next request to receive the next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | Request a list of jobs, using a search filter for name.
    nameContains :: Core.Maybe Core.Text,
    -- | Request a list of jobs up to a specified limit.
    maxResults :: Core.Maybe Core.Natural,
    -- | Request a list of jobs, using a filter for time.
    creationTimeBefore :: Core.Maybe Core.POSIX,
    -- | Request a list of jobs, using a filter for time.
    lastModifiedTimeAfter :: Core.Maybe Core.POSIX,
    -- | The parameter by which to sort the results. The default is
    -- AutoMLJobName.
    sortBy :: Core.Maybe AutoMLSortBy,
    -- | Request a list of jobs, using a filter for status.
    statusEquals :: Core.Maybe AutoMLJobStatus,
    -- | Request a list of jobs, using a filter for time.
    creationTimeAfter :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAutoMLJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedTimeBefore', 'listAutoMLJobs_lastModifiedTimeBefore' - Request a list of jobs, using a filter for time.
--
-- 'sortOrder', 'listAutoMLJobs_sortOrder' - The sort order for the results. The default is Descending.
--
-- 'nextToken', 'listAutoMLJobs_nextToken' - If the previous response was truncated, you receive this token. Use it
-- in your next request to receive the next set of results.
--
-- 'nameContains', 'listAutoMLJobs_nameContains' - Request a list of jobs, using a search filter for name.
--
-- 'maxResults', 'listAutoMLJobs_maxResults' - Request a list of jobs up to a specified limit.
--
-- 'creationTimeBefore', 'listAutoMLJobs_creationTimeBefore' - Request a list of jobs, using a filter for time.
--
-- 'lastModifiedTimeAfter', 'listAutoMLJobs_lastModifiedTimeAfter' - Request a list of jobs, using a filter for time.
--
-- 'sortBy', 'listAutoMLJobs_sortBy' - The parameter by which to sort the results. The default is
-- AutoMLJobName.
--
-- 'statusEquals', 'listAutoMLJobs_statusEquals' - Request a list of jobs, using a filter for status.
--
-- 'creationTimeAfter', 'listAutoMLJobs_creationTimeAfter' - Request a list of jobs, using a filter for time.
newListAutoMLJobs ::
  ListAutoMLJobs
newListAutoMLJobs =
  ListAutoMLJobs'
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

-- | Request a list of jobs, using a filter for time.
listAutoMLJobs_lastModifiedTimeBefore :: Lens.Lens' ListAutoMLJobs (Core.Maybe Core.UTCTime)
listAutoMLJobs_lastModifiedTimeBefore = Lens.lens (\ListAutoMLJobs' {lastModifiedTimeBefore} -> lastModifiedTimeBefore) (\s@ListAutoMLJobs' {} a -> s {lastModifiedTimeBefore = a} :: ListAutoMLJobs) Core.. Lens.mapping Core._Time

-- | The sort order for the results. The default is Descending.
listAutoMLJobs_sortOrder :: Lens.Lens' ListAutoMLJobs (Core.Maybe AutoMLSortOrder)
listAutoMLJobs_sortOrder = Lens.lens (\ListAutoMLJobs' {sortOrder} -> sortOrder) (\s@ListAutoMLJobs' {} a -> s {sortOrder = a} :: ListAutoMLJobs)

-- | If the previous response was truncated, you receive this token. Use it
-- in your next request to receive the next set of results.
listAutoMLJobs_nextToken :: Lens.Lens' ListAutoMLJobs (Core.Maybe Core.Text)
listAutoMLJobs_nextToken = Lens.lens (\ListAutoMLJobs' {nextToken} -> nextToken) (\s@ListAutoMLJobs' {} a -> s {nextToken = a} :: ListAutoMLJobs)

-- | Request a list of jobs, using a search filter for name.
listAutoMLJobs_nameContains :: Lens.Lens' ListAutoMLJobs (Core.Maybe Core.Text)
listAutoMLJobs_nameContains = Lens.lens (\ListAutoMLJobs' {nameContains} -> nameContains) (\s@ListAutoMLJobs' {} a -> s {nameContains = a} :: ListAutoMLJobs)

-- | Request a list of jobs up to a specified limit.
listAutoMLJobs_maxResults :: Lens.Lens' ListAutoMLJobs (Core.Maybe Core.Natural)
listAutoMLJobs_maxResults = Lens.lens (\ListAutoMLJobs' {maxResults} -> maxResults) (\s@ListAutoMLJobs' {} a -> s {maxResults = a} :: ListAutoMLJobs)

-- | Request a list of jobs, using a filter for time.
listAutoMLJobs_creationTimeBefore :: Lens.Lens' ListAutoMLJobs (Core.Maybe Core.UTCTime)
listAutoMLJobs_creationTimeBefore = Lens.lens (\ListAutoMLJobs' {creationTimeBefore} -> creationTimeBefore) (\s@ListAutoMLJobs' {} a -> s {creationTimeBefore = a} :: ListAutoMLJobs) Core.. Lens.mapping Core._Time

-- | Request a list of jobs, using a filter for time.
listAutoMLJobs_lastModifiedTimeAfter :: Lens.Lens' ListAutoMLJobs (Core.Maybe Core.UTCTime)
listAutoMLJobs_lastModifiedTimeAfter = Lens.lens (\ListAutoMLJobs' {lastModifiedTimeAfter} -> lastModifiedTimeAfter) (\s@ListAutoMLJobs' {} a -> s {lastModifiedTimeAfter = a} :: ListAutoMLJobs) Core.. Lens.mapping Core._Time

-- | The parameter by which to sort the results. The default is
-- AutoMLJobName.
listAutoMLJobs_sortBy :: Lens.Lens' ListAutoMLJobs (Core.Maybe AutoMLSortBy)
listAutoMLJobs_sortBy = Lens.lens (\ListAutoMLJobs' {sortBy} -> sortBy) (\s@ListAutoMLJobs' {} a -> s {sortBy = a} :: ListAutoMLJobs)

-- | Request a list of jobs, using a filter for status.
listAutoMLJobs_statusEquals :: Lens.Lens' ListAutoMLJobs (Core.Maybe AutoMLJobStatus)
listAutoMLJobs_statusEquals = Lens.lens (\ListAutoMLJobs' {statusEquals} -> statusEquals) (\s@ListAutoMLJobs' {} a -> s {statusEquals = a} :: ListAutoMLJobs)

-- | Request a list of jobs, using a filter for time.
listAutoMLJobs_creationTimeAfter :: Lens.Lens' ListAutoMLJobs (Core.Maybe Core.UTCTime)
listAutoMLJobs_creationTimeAfter = Lens.lens (\ListAutoMLJobs' {creationTimeAfter} -> creationTimeAfter) (\s@ListAutoMLJobs' {} a -> s {creationTimeAfter = a} :: ListAutoMLJobs) Core.. Lens.mapping Core._Time

instance Core.AWSPager ListAutoMLJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAutoMLJobsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^. listAutoMLJobsResponse_autoMLJobSummaries
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listAutoMLJobs_nextToken
          Lens..~ rs
          Lens.^? listAutoMLJobsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListAutoMLJobs where
  type
    AWSResponse ListAutoMLJobs =
      ListAutoMLJobsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAutoMLJobsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> ( x Core..?> "AutoMLJobSummaries"
                         Core..!@ Core.mempty
                     )
      )

instance Core.Hashable ListAutoMLJobs

instance Core.NFData ListAutoMLJobs

instance Core.ToHeaders ListAutoMLJobs where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.ListAutoMLJobs" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListAutoMLJobs where
  toJSON ListAutoMLJobs' {..} =
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

instance Core.ToPath ListAutoMLJobs where
  toPath = Core.const "/"

instance Core.ToQuery ListAutoMLJobs where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListAutoMLJobsResponse' smart constructor.
data ListAutoMLJobsResponse = ListAutoMLJobsResponse'
  { -- | If the previous response was truncated, you receive this token. Use it
    -- in your next request to receive the next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | Returns a summary list of jobs.
    autoMLJobSummaries :: [AutoMLJobSummary]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAutoMLJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAutoMLJobsResponse_nextToken' - If the previous response was truncated, you receive this token. Use it
-- in your next request to receive the next set of results.
--
-- 'httpStatus', 'listAutoMLJobsResponse_httpStatus' - The response's http status code.
--
-- 'autoMLJobSummaries', 'listAutoMLJobsResponse_autoMLJobSummaries' - Returns a summary list of jobs.
newListAutoMLJobsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListAutoMLJobsResponse
newListAutoMLJobsResponse pHttpStatus_ =
  ListAutoMLJobsResponse'
    { nextToken = Core.Nothing,
      httpStatus = pHttpStatus_,
      autoMLJobSummaries = Core.mempty
    }

-- | If the previous response was truncated, you receive this token. Use it
-- in your next request to receive the next set of results.
listAutoMLJobsResponse_nextToken :: Lens.Lens' ListAutoMLJobsResponse (Core.Maybe Core.Text)
listAutoMLJobsResponse_nextToken = Lens.lens (\ListAutoMLJobsResponse' {nextToken} -> nextToken) (\s@ListAutoMLJobsResponse' {} a -> s {nextToken = a} :: ListAutoMLJobsResponse)

-- | The response's http status code.
listAutoMLJobsResponse_httpStatus :: Lens.Lens' ListAutoMLJobsResponse Core.Int
listAutoMLJobsResponse_httpStatus = Lens.lens (\ListAutoMLJobsResponse' {httpStatus} -> httpStatus) (\s@ListAutoMLJobsResponse' {} a -> s {httpStatus = a} :: ListAutoMLJobsResponse)

-- | Returns a summary list of jobs.
listAutoMLJobsResponse_autoMLJobSummaries :: Lens.Lens' ListAutoMLJobsResponse [AutoMLJobSummary]
listAutoMLJobsResponse_autoMLJobSummaries = Lens.lens (\ListAutoMLJobsResponse' {autoMLJobSummaries} -> autoMLJobSummaries) (\s@ListAutoMLJobsResponse' {} a -> s {autoMLJobSummaries = a} :: ListAutoMLJobsResponse) Core.. Lens._Coerce

instance Core.NFData ListAutoMLJobsResponse
