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
-- Module      : Network.AWS.SageMaker.ListEdgePackagingJobs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of edge packaging jobs.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListEdgePackagingJobs
  ( -- * Creating a Request
    ListEdgePackagingJobs (..),
    newListEdgePackagingJobs,

    -- * Request Lenses
    listEdgePackagingJobs_lastModifiedTimeBefore,
    listEdgePackagingJobs_sortOrder,
    listEdgePackagingJobs_nextToken,
    listEdgePackagingJobs_nameContains,
    listEdgePackagingJobs_maxResults,
    listEdgePackagingJobs_modelNameContains,
    listEdgePackagingJobs_creationTimeBefore,
    listEdgePackagingJobs_lastModifiedTimeAfter,
    listEdgePackagingJobs_sortBy,
    listEdgePackagingJobs_statusEquals,
    listEdgePackagingJobs_creationTimeAfter,

    -- * Destructuring the Response
    ListEdgePackagingJobsResponse (..),
    newListEdgePackagingJobsResponse,

    -- * Response Lenses
    listEdgePackagingJobsResponse_nextToken,
    listEdgePackagingJobsResponse_httpStatus,
    listEdgePackagingJobsResponse_edgePackagingJobSummaries,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListEdgePackagingJobs' smart constructor.
data ListEdgePackagingJobs = ListEdgePackagingJobs'
  { -- | Select jobs where the job was updated before specified time.
    lastModifiedTimeBefore :: Core.Maybe Core.POSIX,
    -- | What direction to sort by.
    sortOrder :: Core.Maybe SortOrder,
    -- | The response from the last list when returning a list large enough to
    -- need tokening.
    nextToken :: Core.Maybe Core.Text,
    -- | Filter for jobs containing this name in their packaging job name.
    nameContains :: Core.Maybe Core.Text,
    -- | Maximum number of results to select.
    maxResults :: Core.Maybe Core.Int,
    -- | Filter for jobs where the model name contains this string.
    modelNameContains :: Core.Maybe Core.Text,
    -- | Select jobs where the job was created before specified time.
    creationTimeBefore :: Core.Maybe Core.POSIX,
    -- | Select jobs where the job was updated after specified time.
    lastModifiedTimeAfter :: Core.Maybe Core.POSIX,
    -- | Use to specify what column to sort by.
    sortBy :: Core.Maybe ListEdgePackagingJobsSortBy,
    -- | The job status to filter for.
    statusEquals :: Core.Maybe EdgePackagingJobStatus,
    -- | Select jobs where the job was created after specified time.
    creationTimeAfter :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListEdgePackagingJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedTimeBefore', 'listEdgePackagingJobs_lastModifiedTimeBefore' - Select jobs where the job was updated before specified time.
--
-- 'sortOrder', 'listEdgePackagingJobs_sortOrder' - What direction to sort by.
--
-- 'nextToken', 'listEdgePackagingJobs_nextToken' - The response from the last list when returning a list large enough to
-- need tokening.
--
-- 'nameContains', 'listEdgePackagingJobs_nameContains' - Filter for jobs containing this name in their packaging job name.
--
-- 'maxResults', 'listEdgePackagingJobs_maxResults' - Maximum number of results to select.
--
-- 'modelNameContains', 'listEdgePackagingJobs_modelNameContains' - Filter for jobs where the model name contains this string.
--
-- 'creationTimeBefore', 'listEdgePackagingJobs_creationTimeBefore' - Select jobs where the job was created before specified time.
--
-- 'lastModifiedTimeAfter', 'listEdgePackagingJobs_lastModifiedTimeAfter' - Select jobs where the job was updated after specified time.
--
-- 'sortBy', 'listEdgePackagingJobs_sortBy' - Use to specify what column to sort by.
--
-- 'statusEquals', 'listEdgePackagingJobs_statusEquals' - The job status to filter for.
--
-- 'creationTimeAfter', 'listEdgePackagingJobs_creationTimeAfter' - Select jobs where the job was created after specified time.
newListEdgePackagingJobs ::
  ListEdgePackagingJobs
newListEdgePackagingJobs =
  ListEdgePackagingJobs'
    { lastModifiedTimeBefore =
        Core.Nothing,
      sortOrder = Core.Nothing,
      nextToken = Core.Nothing,
      nameContains = Core.Nothing,
      maxResults = Core.Nothing,
      modelNameContains = Core.Nothing,
      creationTimeBefore = Core.Nothing,
      lastModifiedTimeAfter = Core.Nothing,
      sortBy = Core.Nothing,
      statusEquals = Core.Nothing,
      creationTimeAfter = Core.Nothing
    }

-- | Select jobs where the job was updated before specified time.
listEdgePackagingJobs_lastModifiedTimeBefore :: Lens.Lens' ListEdgePackagingJobs (Core.Maybe Core.UTCTime)
listEdgePackagingJobs_lastModifiedTimeBefore = Lens.lens (\ListEdgePackagingJobs' {lastModifiedTimeBefore} -> lastModifiedTimeBefore) (\s@ListEdgePackagingJobs' {} a -> s {lastModifiedTimeBefore = a} :: ListEdgePackagingJobs) Core.. Lens.mapping Core._Time

-- | What direction to sort by.
listEdgePackagingJobs_sortOrder :: Lens.Lens' ListEdgePackagingJobs (Core.Maybe SortOrder)
listEdgePackagingJobs_sortOrder = Lens.lens (\ListEdgePackagingJobs' {sortOrder} -> sortOrder) (\s@ListEdgePackagingJobs' {} a -> s {sortOrder = a} :: ListEdgePackagingJobs)

-- | The response from the last list when returning a list large enough to
-- need tokening.
listEdgePackagingJobs_nextToken :: Lens.Lens' ListEdgePackagingJobs (Core.Maybe Core.Text)
listEdgePackagingJobs_nextToken = Lens.lens (\ListEdgePackagingJobs' {nextToken} -> nextToken) (\s@ListEdgePackagingJobs' {} a -> s {nextToken = a} :: ListEdgePackagingJobs)

-- | Filter for jobs containing this name in their packaging job name.
listEdgePackagingJobs_nameContains :: Lens.Lens' ListEdgePackagingJobs (Core.Maybe Core.Text)
listEdgePackagingJobs_nameContains = Lens.lens (\ListEdgePackagingJobs' {nameContains} -> nameContains) (\s@ListEdgePackagingJobs' {} a -> s {nameContains = a} :: ListEdgePackagingJobs)

-- | Maximum number of results to select.
listEdgePackagingJobs_maxResults :: Lens.Lens' ListEdgePackagingJobs (Core.Maybe Core.Int)
listEdgePackagingJobs_maxResults = Lens.lens (\ListEdgePackagingJobs' {maxResults} -> maxResults) (\s@ListEdgePackagingJobs' {} a -> s {maxResults = a} :: ListEdgePackagingJobs)

-- | Filter for jobs where the model name contains this string.
listEdgePackagingJobs_modelNameContains :: Lens.Lens' ListEdgePackagingJobs (Core.Maybe Core.Text)
listEdgePackagingJobs_modelNameContains = Lens.lens (\ListEdgePackagingJobs' {modelNameContains} -> modelNameContains) (\s@ListEdgePackagingJobs' {} a -> s {modelNameContains = a} :: ListEdgePackagingJobs)

-- | Select jobs where the job was created before specified time.
listEdgePackagingJobs_creationTimeBefore :: Lens.Lens' ListEdgePackagingJobs (Core.Maybe Core.UTCTime)
listEdgePackagingJobs_creationTimeBefore = Lens.lens (\ListEdgePackagingJobs' {creationTimeBefore} -> creationTimeBefore) (\s@ListEdgePackagingJobs' {} a -> s {creationTimeBefore = a} :: ListEdgePackagingJobs) Core.. Lens.mapping Core._Time

-- | Select jobs where the job was updated after specified time.
listEdgePackagingJobs_lastModifiedTimeAfter :: Lens.Lens' ListEdgePackagingJobs (Core.Maybe Core.UTCTime)
listEdgePackagingJobs_lastModifiedTimeAfter = Lens.lens (\ListEdgePackagingJobs' {lastModifiedTimeAfter} -> lastModifiedTimeAfter) (\s@ListEdgePackagingJobs' {} a -> s {lastModifiedTimeAfter = a} :: ListEdgePackagingJobs) Core.. Lens.mapping Core._Time

-- | Use to specify what column to sort by.
listEdgePackagingJobs_sortBy :: Lens.Lens' ListEdgePackagingJobs (Core.Maybe ListEdgePackagingJobsSortBy)
listEdgePackagingJobs_sortBy = Lens.lens (\ListEdgePackagingJobs' {sortBy} -> sortBy) (\s@ListEdgePackagingJobs' {} a -> s {sortBy = a} :: ListEdgePackagingJobs)

-- | The job status to filter for.
listEdgePackagingJobs_statusEquals :: Lens.Lens' ListEdgePackagingJobs (Core.Maybe EdgePackagingJobStatus)
listEdgePackagingJobs_statusEquals = Lens.lens (\ListEdgePackagingJobs' {statusEquals} -> statusEquals) (\s@ListEdgePackagingJobs' {} a -> s {statusEquals = a} :: ListEdgePackagingJobs)

-- | Select jobs where the job was created after specified time.
listEdgePackagingJobs_creationTimeAfter :: Lens.Lens' ListEdgePackagingJobs (Core.Maybe Core.UTCTime)
listEdgePackagingJobs_creationTimeAfter = Lens.lens (\ListEdgePackagingJobs' {creationTimeAfter} -> creationTimeAfter) (\s@ListEdgePackagingJobs' {} a -> s {creationTimeAfter = a} :: ListEdgePackagingJobs) Core.. Lens.mapping Core._Time

instance Core.AWSPager ListEdgePackagingJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listEdgePackagingJobsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^. listEdgePackagingJobsResponse_edgePackagingJobSummaries
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listEdgePackagingJobs_nextToken
          Lens..~ rs
          Lens.^? listEdgePackagingJobsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListEdgePackagingJobs where
  type
    AWSResponse ListEdgePackagingJobs =
      ListEdgePackagingJobsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEdgePackagingJobsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> ( x Core..?> "EdgePackagingJobSummaries"
                         Core..!@ Core.mempty
                     )
      )

instance Core.Hashable ListEdgePackagingJobs

instance Core.NFData ListEdgePackagingJobs

instance Core.ToHeaders ListEdgePackagingJobs where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.ListEdgePackagingJobs" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListEdgePackagingJobs where
  toJSON ListEdgePackagingJobs' {..} =
    Core.object
      ( Core.catMaybes
          [ ("LastModifiedTimeBefore" Core..=)
              Core.<$> lastModifiedTimeBefore,
            ("SortOrder" Core..=) Core.<$> sortOrder,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("NameContains" Core..=) Core.<$> nameContains,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("ModelNameContains" Core..=)
              Core.<$> modelNameContains,
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

instance Core.ToPath ListEdgePackagingJobs where
  toPath = Core.const "/"

instance Core.ToQuery ListEdgePackagingJobs where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListEdgePackagingJobsResponse' smart constructor.
data ListEdgePackagingJobsResponse = ListEdgePackagingJobsResponse'
  { -- | Token to use when calling the next page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | Summaries of edge packaging jobs.
    edgePackagingJobSummaries :: [EdgePackagingJobSummary]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListEdgePackagingJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEdgePackagingJobsResponse_nextToken' - Token to use when calling the next page of results.
--
-- 'httpStatus', 'listEdgePackagingJobsResponse_httpStatus' - The response's http status code.
--
-- 'edgePackagingJobSummaries', 'listEdgePackagingJobsResponse_edgePackagingJobSummaries' - Summaries of edge packaging jobs.
newListEdgePackagingJobsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListEdgePackagingJobsResponse
newListEdgePackagingJobsResponse pHttpStatus_ =
  ListEdgePackagingJobsResponse'
    { nextToken =
        Core.Nothing,
      httpStatus = pHttpStatus_,
      edgePackagingJobSummaries = Core.mempty
    }

-- | Token to use when calling the next page of results.
listEdgePackagingJobsResponse_nextToken :: Lens.Lens' ListEdgePackagingJobsResponse (Core.Maybe Core.Text)
listEdgePackagingJobsResponse_nextToken = Lens.lens (\ListEdgePackagingJobsResponse' {nextToken} -> nextToken) (\s@ListEdgePackagingJobsResponse' {} a -> s {nextToken = a} :: ListEdgePackagingJobsResponse)

-- | The response's http status code.
listEdgePackagingJobsResponse_httpStatus :: Lens.Lens' ListEdgePackagingJobsResponse Core.Int
listEdgePackagingJobsResponse_httpStatus = Lens.lens (\ListEdgePackagingJobsResponse' {httpStatus} -> httpStatus) (\s@ListEdgePackagingJobsResponse' {} a -> s {httpStatus = a} :: ListEdgePackagingJobsResponse)

-- | Summaries of edge packaging jobs.
listEdgePackagingJobsResponse_edgePackagingJobSummaries :: Lens.Lens' ListEdgePackagingJobsResponse [EdgePackagingJobSummary]
listEdgePackagingJobsResponse_edgePackagingJobSummaries = Lens.lens (\ListEdgePackagingJobsResponse' {edgePackagingJobSummaries} -> edgePackagingJobSummaries) (\s@ListEdgePackagingJobsResponse' {} a -> s {edgePackagingJobSummaries = a} :: ListEdgePackagingJobsResponse) Core.. Lens._Coerce

instance Core.NFData ListEdgePackagingJobsResponse
