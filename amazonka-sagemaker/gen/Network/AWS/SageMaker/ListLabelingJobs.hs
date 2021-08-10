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
-- Module      : Network.AWS.SageMaker.ListLabelingJobs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of labeling jobs.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListLabelingJobs
  ( -- * Creating a Request
    ListLabelingJobs (..),
    newListLabelingJobs,

    -- * Request Lenses
    listLabelingJobs_lastModifiedTimeBefore,
    listLabelingJobs_sortOrder,
    listLabelingJobs_nextToken,
    listLabelingJobs_nameContains,
    listLabelingJobs_maxResults,
    listLabelingJobs_creationTimeBefore,
    listLabelingJobs_lastModifiedTimeAfter,
    listLabelingJobs_sortBy,
    listLabelingJobs_statusEquals,
    listLabelingJobs_creationTimeAfter,

    -- * Destructuring the Response
    ListLabelingJobsResponse (..),
    newListLabelingJobsResponse,

    -- * Response Lenses
    listLabelingJobsResponse_labelingJobSummaryList,
    listLabelingJobsResponse_nextToken,
    listLabelingJobsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListLabelingJobs' smart constructor.
data ListLabelingJobs = ListLabelingJobs'
  { -- | A filter that returns only labeling jobs modified before the specified
    -- time (timestamp).
    lastModifiedTimeBefore :: Prelude.Maybe Core.POSIX,
    -- | The sort order for results. The default is @Ascending@.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | If the result of the previous @ListLabelingJobs@ request was truncated,
    -- the response includes a @NextToken@. To retrieve the next set of
    -- labeling jobs, use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A string in the labeling job name. This filter returns only labeling
    -- jobs whose name contains the specified string.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of labeling jobs to return in each page of the
    -- response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A filter that returns only labeling jobs created before the specified
    -- time (timestamp).
    creationTimeBefore :: Prelude.Maybe Core.POSIX,
    -- | A filter that returns only labeling jobs modified after the specified
    -- time (timestamp).
    lastModifiedTimeAfter :: Prelude.Maybe Core.POSIX,
    -- | The field to sort results by. The default is @CreationTime@.
    sortBy :: Prelude.Maybe SortBy,
    -- | A filter that retrieves only labeling jobs with a specific status.
    statusEquals :: Prelude.Maybe LabelingJobStatus,
    -- | A filter that returns only labeling jobs created after the specified
    -- time (timestamp).
    creationTimeAfter :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLabelingJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedTimeBefore', 'listLabelingJobs_lastModifiedTimeBefore' - A filter that returns only labeling jobs modified before the specified
-- time (timestamp).
--
-- 'sortOrder', 'listLabelingJobs_sortOrder' - The sort order for results. The default is @Ascending@.
--
-- 'nextToken', 'listLabelingJobs_nextToken' - If the result of the previous @ListLabelingJobs@ request was truncated,
-- the response includes a @NextToken@. To retrieve the next set of
-- labeling jobs, use the token in the next request.
--
-- 'nameContains', 'listLabelingJobs_nameContains' - A string in the labeling job name. This filter returns only labeling
-- jobs whose name contains the specified string.
--
-- 'maxResults', 'listLabelingJobs_maxResults' - The maximum number of labeling jobs to return in each page of the
-- response.
--
-- 'creationTimeBefore', 'listLabelingJobs_creationTimeBefore' - A filter that returns only labeling jobs created before the specified
-- time (timestamp).
--
-- 'lastModifiedTimeAfter', 'listLabelingJobs_lastModifiedTimeAfter' - A filter that returns only labeling jobs modified after the specified
-- time (timestamp).
--
-- 'sortBy', 'listLabelingJobs_sortBy' - The field to sort results by. The default is @CreationTime@.
--
-- 'statusEquals', 'listLabelingJobs_statusEquals' - A filter that retrieves only labeling jobs with a specific status.
--
-- 'creationTimeAfter', 'listLabelingJobs_creationTimeAfter' - A filter that returns only labeling jobs created after the specified
-- time (timestamp).
newListLabelingJobs ::
  ListLabelingJobs
newListLabelingJobs =
  ListLabelingJobs'
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

-- | A filter that returns only labeling jobs modified before the specified
-- time (timestamp).
listLabelingJobs_lastModifiedTimeBefore :: Lens.Lens' ListLabelingJobs (Prelude.Maybe Prelude.UTCTime)
listLabelingJobs_lastModifiedTimeBefore = Lens.lens (\ListLabelingJobs' {lastModifiedTimeBefore} -> lastModifiedTimeBefore) (\s@ListLabelingJobs' {} a -> s {lastModifiedTimeBefore = a} :: ListLabelingJobs) Prelude.. Lens.mapping Core._Time

-- | The sort order for results. The default is @Ascending@.
listLabelingJobs_sortOrder :: Lens.Lens' ListLabelingJobs (Prelude.Maybe SortOrder)
listLabelingJobs_sortOrder = Lens.lens (\ListLabelingJobs' {sortOrder} -> sortOrder) (\s@ListLabelingJobs' {} a -> s {sortOrder = a} :: ListLabelingJobs)

-- | If the result of the previous @ListLabelingJobs@ request was truncated,
-- the response includes a @NextToken@. To retrieve the next set of
-- labeling jobs, use the token in the next request.
listLabelingJobs_nextToken :: Lens.Lens' ListLabelingJobs (Prelude.Maybe Prelude.Text)
listLabelingJobs_nextToken = Lens.lens (\ListLabelingJobs' {nextToken} -> nextToken) (\s@ListLabelingJobs' {} a -> s {nextToken = a} :: ListLabelingJobs)

-- | A string in the labeling job name. This filter returns only labeling
-- jobs whose name contains the specified string.
listLabelingJobs_nameContains :: Lens.Lens' ListLabelingJobs (Prelude.Maybe Prelude.Text)
listLabelingJobs_nameContains = Lens.lens (\ListLabelingJobs' {nameContains} -> nameContains) (\s@ListLabelingJobs' {} a -> s {nameContains = a} :: ListLabelingJobs)

-- | The maximum number of labeling jobs to return in each page of the
-- response.
listLabelingJobs_maxResults :: Lens.Lens' ListLabelingJobs (Prelude.Maybe Prelude.Natural)
listLabelingJobs_maxResults = Lens.lens (\ListLabelingJobs' {maxResults} -> maxResults) (\s@ListLabelingJobs' {} a -> s {maxResults = a} :: ListLabelingJobs)

-- | A filter that returns only labeling jobs created before the specified
-- time (timestamp).
listLabelingJobs_creationTimeBefore :: Lens.Lens' ListLabelingJobs (Prelude.Maybe Prelude.UTCTime)
listLabelingJobs_creationTimeBefore = Lens.lens (\ListLabelingJobs' {creationTimeBefore} -> creationTimeBefore) (\s@ListLabelingJobs' {} a -> s {creationTimeBefore = a} :: ListLabelingJobs) Prelude.. Lens.mapping Core._Time

-- | A filter that returns only labeling jobs modified after the specified
-- time (timestamp).
listLabelingJobs_lastModifiedTimeAfter :: Lens.Lens' ListLabelingJobs (Prelude.Maybe Prelude.UTCTime)
listLabelingJobs_lastModifiedTimeAfter = Lens.lens (\ListLabelingJobs' {lastModifiedTimeAfter} -> lastModifiedTimeAfter) (\s@ListLabelingJobs' {} a -> s {lastModifiedTimeAfter = a} :: ListLabelingJobs) Prelude.. Lens.mapping Core._Time

-- | The field to sort results by. The default is @CreationTime@.
listLabelingJobs_sortBy :: Lens.Lens' ListLabelingJobs (Prelude.Maybe SortBy)
listLabelingJobs_sortBy = Lens.lens (\ListLabelingJobs' {sortBy} -> sortBy) (\s@ListLabelingJobs' {} a -> s {sortBy = a} :: ListLabelingJobs)

-- | A filter that retrieves only labeling jobs with a specific status.
listLabelingJobs_statusEquals :: Lens.Lens' ListLabelingJobs (Prelude.Maybe LabelingJobStatus)
listLabelingJobs_statusEquals = Lens.lens (\ListLabelingJobs' {statusEquals} -> statusEquals) (\s@ListLabelingJobs' {} a -> s {statusEquals = a} :: ListLabelingJobs)

-- | A filter that returns only labeling jobs created after the specified
-- time (timestamp).
listLabelingJobs_creationTimeAfter :: Lens.Lens' ListLabelingJobs (Prelude.Maybe Prelude.UTCTime)
listLabelingJobs_creationTimeAfter = Lens.lens (\ListLabelingJobs' {creationTimeAfter} -> creationTimeAfter) (\s@ListLabelingJobs' {} a -> s {creationTimeAfter = a} :: ListLabelingJobs) Prelude.. Lens.mapping Core._Time

instance Core.AWSPager ListLabelingJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listLabelingJobsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listLabelingJobsResponse_labelingJobSummaryList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listLabelingJobs_nextToken
          Lens..~ rs
          Lens.^? listLabelingJobsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListLabelingJobs where
  type
    AWSResponse ListLabelingJobs =
      ListLabelingJobsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLabelingJobsResponse'
            Prelude.<$> ( x Core..?> "LabelingJobSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListLabelingJobs

instance Prelude.NFData ListLabelingJobs

instance Core.ToHeaders ListLabelingJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.ListLabelingJobs" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListLabelingJobs where
  toJSON ListLabelingJobs' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("LastModifiedTimeBefore" Core..=)
              Prelude.<$> lastModifiedTimeBefore,
            ("SortOrder" Core..=) Prelude.<$> sortOrder,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("NameContains" Core..=) Prelude.<$> nameContains,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("CreationTimeBefore" Core..=)
              Prelude.<$> creationTimeBefore,
            ("LastModifiedTimeAfter" Core..=)
              Prelude.<$> lastModifiedTimeAfter,
            ("SortBy" Core..=) Prelude.<$> sortBy,
            ("StatusEquals" Core..=) Prelude.<$> statusEquals,
            ("CreationTimeAfter" Core..=)
              Prelude.<$> creationTimeAfter
          ]
      )

instance Core.ToPath ListLabelingJobs where
  toPath = Prelude.const "/"

instance Core.ToQuery ListLabelingJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListLabelingJobsResponse' smart constructor.
data ListLabelingJobsResponse = ListLabelingJobsResponse'
  { -- | An array of @LabelingJobSummary@ objects, each describing a labeling
    -- job.
    labelingJobSummaryList :: Prelude.Maybe [LabelingJobSummary],
    -- | If the response is truncated, Amazon SageMaker returns this token. To
    -- retrieve the next set of labeling jobs, use it in the subsequent
    -- request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLabelingJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'labelingJobSummaryList', 'listLabelingJobsResponse_labelingJobSummaryList' - An array of @LabelingJobSummary@ objects, each describing a labeling
-- job.
--
-- 'nextToken', 'listLabelingJobsResponse_nextToken' - If the response is truncated, Amazon SageMaker returns this token. To
-- retrieve the next set of labeling jobs, use it in the subsequent
-- request.
--
-- 'httpStatus', 'listLabelingJobsResponse_httpStatus' - The response's http status code.
newListLabelingJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListLabelingJobsResponse
newListLabelingJobsResponse pHttpStatus_ =
  ListLabelingJobsResponse'
    { labelingJobSummaryList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of @LabelingJobSummary@ objects, each describing a labeling
-- job.
listLabelingJobsResponse_labelingJobSummaryList :: Lens.Lens' ListLabelingJobsResponse (Prelude.Maybe [LabelingJobSummary])
listLabelingJobsResponse_labelingJobSummaryList = Lens.lens (\ListLabelingJobsResponse' {labelingJobSummaryList} -> labelingJobSummaryList) (\s@ListLabelingJobsResponse' {} a -> s {labelingJobSummaryList = a} :: ListLabelingJobsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | If the response is truncated, Amazon SageMaker returns this token. To
-- retrieve the next set of labeling jobs, use it in the subsequent
-- request.
listLabelingJobsResponse_nextToken :: Lens.Lens' ListLabelingJobsResponse (Prelude.Maybe Prelude.Text)
listLabelingJobsResponse_nextToken = Lens.lens (\ListLabelingJobsResponse' {nextToken} -> nextToken) (\s@ListLabelingJobsResponse' {} a -> s {nextToken = a} :: ListLabelingJobsResponse)

-- | The response's http status code.
listLabelingJobsResponse_httpStatus :: Lens.Lens' ListLabelingJobsResponse Prelude.Int
listLabelingJobsResponse_httpStatus = Lens.lens (\ListLabelingJobsResponse' {httpStatus} -> httpStatus) (\s@ListLabelingJobsResponse' {} a -> s {httpStatus = a} :: ListLabelingJobsResponse)

instance Prelude.NFData ListLabelingJobsResponse
