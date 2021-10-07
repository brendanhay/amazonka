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
-- Module      : Network.AWS.SageMaker.ListLabelingJobsForWorkteam
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of labeling jobs assigned to a specified work team.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListLabelingJobsForWorkteam
  ( -- * Creating a Request
    ListLabelingJobsForWorkteam (..),
    newListLabelingJobsForWorkteam,

    -- * Request Lenses
    listLabelingJobsForWorkteam_nextToken,
    listLabelingJobsForWorkteam_sortOrder,
    listLabelingJobsForWorkteam_maxResults,
    listLabelingJobsForWorkteam_creationTimeBefore,
    listLabelingJobsForWorkteam_jobReferenceCodeContains,
    listLabelingJobsForWorkteam_sortBy,
    listLabelingJobsForWorkteam_creationTimeAfter,
    listLabelingJobsForWorkteam_workteamArn,

    -- * Destructuring the Response
    ListLabelingJobsForWorkteamResponse (..),
    newListLabelingJobsForWorkteamResponse,

    -- * Response Lenses
    listLabelingJobsForWorkteamResponse_nextToken,
    listLabelingJobsForWorkteamResponse_httpStatus,
    listLabelingJobsForWorkteamResponse_labelingJobSummaryList,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListLabelingJobsForWorkteam' smart constructor.
data ListLabelingJobsForWorkteam = ListLabelingJobsForWorkteam'
  { -- | If the result of the previous @ListLabelingJobsForWorkteam@ request was
    -- truncated, the response includes a @NextToken@. To retrieve the next set
    -- of labeling jobs, use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The sort order for results. The default is @Ascending@.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | The maximum number of labeling jobs to return in each page of the
    -- response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A filter that returns only labeling jobs created before the specified
    -- time (timestamp).
    creationTimeBefore :: Prelude.Maybe Core.POSIX,
    -- | A filter the limits jobs to only the ones whose job reference code
    -- contains the specified string.
    jobReferenceCodeContains :: Prelude.Maybe Prelude.Text,
    -- | The field to sort results by. The default is @CreationTime@.
    sortBy :: Prelude.Maybe ListLabelingJobsForWorkteamSortByOptions,
    -- | A filter that returns only labeling jobs created after the specified
    -- time (timestamp).
    creationTimeAfter :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the work team for which you want to
    -- see labeling jobs for.
    workteamArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLabelingJobsForWorkteam' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listLabelingJobsForWorkteam_nextToken' - If the result of the previous @ListLabelingJobsForWorkteam@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of labeling jobs, use the token in the next request.
--
-- 'sortOrder', 'listLabelingJobsForWorkteam_sortOrder' - The sort order for results. The default is @Ascending@.
--
-- 'maxResults', 'listLabelingJobsForWorkteam_maxResults' - The maximum number of labeling jobs to return in each page of the
-- response.
--
-- 'creationTimeBefore', 'listLabelingJobsForWorkteam_creationTimeBefore' - A filter that returns only labeling jobs created before the specified
-- time (timestamp).
--
-- 'jobReferenceCodeContains', 'listLabelingJobsForWorkteam_jobReferenceCodeContains' - A filter the limits jobs to only the ones whose job reference code
-- contains the specified string.
--
-- 'sortBy', 'listLabelingJobsForWorkteam_sortBy' - The field to sort results by. The default is @CreationTime@.
--
-- 'creationTimeAfter', 'listLabelingJobsForWorkteam_creationTimeAfter' - A filter that returns only labeling jobs created after the specified
-- time (timestamp).
--
-- 'workteamArn', 'listLabelingJobsForWorkteam_workteamArn' - The Amazon Resource Name (ARN) of the work team for which you want to
-- see labeling jobs for.
newListLabelingJobsForWorkteam ::
  -- | 'workteamArn'
  Prelude.Text ->
  ListLabelingJobsForWorkteam
newListLabelingJobsForWorkteam pWorkteamArn_ =
  ListLabelingJobsForWorkteam'
    { nextToken =
        Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      jobReferenceCodeContains = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      creationTimeAfter = Prelude.Nothing,
      workteamArn = pWorkteamArn_
    }

-- | If the result of the previous @ListLabelingJobsForWorkteam@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of labeling jobs, use the token in the next request.
listLabelingJobsForWorkteam_nextToken :: Lens.Lens' ListLabelingJobsForWorkteam (Prelude.Maybe Prelude.Text)
listLabelingJobsForWorkteam_nextToken = Lens.lens (\ListLabelingJobsForWorkteam' {nextToken} -> nextToken) (\s@ListLabelingJobsForWorkteam' {} a -> s {nextToken = a} :: ListLabelingJobsForWorkteam)

-- | The sort order for results. The default is @Ascending@.
listLabelingJobsForWorkteam_sortOrder :: Lens.Lens' ListLabelingJobsForWorkteam (Prelude.Maybe SortOrder)
listLabelingJobsForWorkteam_sortOrder = Lens.lens (\ListLabelingJobsForWorkteam' {sortOrder} -> sortOrder) (\s@ListLabelingJobsForWorkteam' {} a -> s {sortOrder = a} :: ListLabelingJobsForWorkteam)

-- | The maximum number of labeling jobs to return in each page of the
-- response.
listLabelingJobsForWorkteam_maxResults :: Lens.Lens' ListLabelingJobsForWorkteam (Prelude.Maybe Prelude.Natural)
listLabelingJobsForWorkteam_maxResults = Lens.lens (\ListLabelingJobsForWorkteam' {maxResults} -> maxResults) (\s@ListLabelingJobsForWorkteam' {} a -> s {maxResults = a} :: ListLabelingJobsForWorkteam)

-- | A filter that returns only labeling jobs created before the specified
-- time (timestamp).
listLabelingJobsForWorkteam_creationTimeBefore :: Lens.Lens' ListLabelingJobsForWorkteam (Prelude.Maybe Prelude.UTCTime)
listLabelingJobsForWorkteam_creationTimeBefore = Lens.lens (\ListLabelingJobsForWorkteam' {creationTimeBefore} -> creationTimeBefore) (\s@ListLabelingJobsForWorkteam' {} a -> s {creationTimeBefore = a} :: ListLabelingJobsForWorkteam) Prelude.. Lens.mapping Core._Time

-- | A filter the limits jobs to only the ones whose job reference code
-- contains the specified string.
listLabelingJobsForWorkteam_jobReferenceCodeContains :: Lens.Lens' ListLabelingJobsForWorkteam (Prelude.Maybe Prelude.Text)
listLabelingJobsForWorkteam_jobReferenceCodeContains = Lens.lens (\ListLabelingJobsForWorkteam' {jobReferenceCodeContains} -> jobReferenceCodeContains) (\s@ListLabelingJobsForWorkteam' {} a -> s {jobReferenceCodeContains = a} :: ListLabelingJobsForWorkteam)

-- | The field to sort results by. The default is @CreationTime@.
listLabelingJobsForWorkteam_sortBy :: Lens.Lens' ListLabelingJobsForWorkteam (Prelude.Maybe ListLabelingJobsForWorkteamSortByOptions)
listLabelingJobsForWorkteam_sortBy = Lens.lens (\ListLabelingJobsForWorkteam' {sortBy} -> sortBy) (\s@ListLabelingJobsForWorkteam' {} a -> s {sortBy = a} :: ListLabelingJobsForWorkteam)

-- | A filter that returns only labeling jobs created after the specified
-- time (timestamp).
listLabelingJobsForWorkteam_creationTimeAfter :: Lens.Lens' ListLabelingJobsForWorkteam (Prelude.Maybe Prelude.UTCTime)
listLabelingJobsForWorkteam_creationTimeAfter = Lens.lens (\ListLabelingJobsForWorkteam' {creationTimeAfter} -> creationTimeAfter) (\s@ListLabelingJobsForWorkteam' {} a -> s {creationTimeAfter = a} :: ListLabelingJobsForWorkteam) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the work team for which you want to
-- see labeling jobs for.
listLabelingJobsForWorkteam_workteamArn :: Lens.Lens' ListLabelingJobsForWorkteam Prelude.Text
listLabelingJobsForWorkteam_workteamArn = Lens.lens (\ListLabelingJobsForWorkteam' {workteamArn} -> workteamArn) (\s@ListLabelingJobsForWorkteam' {} a -> s {workteamArn = a} :: ListLabelingJobsForWorkteam)

instance Core.AWSPager ListLabelingJobsForWorkteam where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listLabelingJobsForWorkteamResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listLabelingJobsForWorkteamResponse_labelingJobSummaryList
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listLabelingJobsForWorkteam_nextToken
          Lens..~ rs
          Lens.^? listLabelingJobsForWorkteamResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListLabelingJobsForWorkteam where
  type
    AWSResponse ListLabelingJobsForWorkteam =
      ListLabelingJobsForWorkteamResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLabelingJobsForWorkteamResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..?> "LabelingJobSummaryList"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListLabelingJobsForWorkteam

instance Prelude.NFData ListLabelingJobsForWorkteam

instance Core.ToHeaders ListLabelingJobsForWorkteam where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.ListLabelingJobsForWorkteam" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListLabelingJobsForWorkteam where
  toJSON ListLabelingJobsForWorkteam' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("SortOrder" Core..=) Prelude.<$> sortOrder,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("CreationTimeBefore" Core..=)
              Prelude.<$> creationTimeBefore,
            ("JobReferenceCodeContains" Core..=)
              Prelude.<$> jobReferenceCodeContains,
            ("SortBy" Core..=) Prelude.<$> sortBy,
            ("CreationTimeAfter" Core..=)
              Prelude.<$> creationTimeAfter,
            Prelude.Just ("WorkteamArn" Core..= workteamArn)
          ]
      )

instance Core.ToPath ListLabelingJobsForWorkteam where
  toPath = Prelude.const "/"

instance Core.ToQuery ListLabelingJobsForWorkteam where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListLabelingJobsForWorkteamResponse' smart constructor.
data ListLabelingJobsForWorkteamResponse = ListLabelingJobsForWorkteamResponse'
  { -- | If the response is truncated, Amazon SageMaker returns this token. To
    -- retrieve the next set of labeling jobs, use it in the subsequent
    -- request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array of @LabelingJobSummary@ objects, each describing a labeling
    -- job.
    labelingJobSummaryList :: [LabelingJobForWorkteamSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLabelingJobsForWorkteamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listLabelingJobsForWorkteamResponse_nextToken' - If the response is truncated, Amazon SageMaker returns this token. To
-- retrieve the next set of labeling jobs, use it in the subsequent
-- request.
--
-- 'httpStatus', 'listLabelingJobsForWorkteamResponse_httpStatus' - The response's http status code.
--
-- 'labelingJobSummaryList', 'listLabelingJobsForWorkteamResponse_labelingJobSummaryList' - An array of @LabelingJobSummary@ objects, each describing a labeling
-- job.
newListLabelingJobsForWorkteamResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListLabelingJobsForWorkteamResponse
newListLabelingJobsForWorkteamResponse pHttpStatus_ =
  ListLabelingJobsForWorkteamResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      labelingJobSummaryList =
        Prelude.mempty
    }

-- | If the response is truncated, Amazon SageMaker returns this token. To
-- retrieve the next set of labeling jobs, use it in the subsequent
-- request.
listLabelingJobsForWorkteamResponse_nextToken :: Lens.Lens' ListLabelingJobsForWorkteamResponse (Prelude.Maybe Prelude.Text)
listLabelingJobsForWorkteamResponse_nextToken = Lens.lens (\ListLabelingJobsForWorkteamResponse' {nextToken} -> nextToken) (\s@ListLabelingJobsForWorkteamResponse' {} a -> s {nextToken = a} :: ListLabelingJobsForWorkteamResponse)

-- | The response's http status code.
listLabelingJobsForWorkteamResponse_httpStatus :: Lens.Lens' ListLabelingJobsForWorkteamResponse Prelude.Int
listLabelingJobsForWorkteamResponse_httpStatus = Lens.lens (\ListLabelingJobsForWorkteamResponse' {httpStatus} -> httpStatus) (\s@ListLabelingJobsForWorkteamResponse' {} a -> s {httpStatus = a} :: ListLabelingJobsForWorkteamResponse)

-- | An array of @LabelingJobSummary@ objects, each describing a labeling
-- job.
listLabelingJobsForWorkteamResponse_labelingJobSummaryList :: Lens.Lens' ListLabelingJobsForWorkteamResponse [LabelingJobForWorkteamSummary]
listLabelingJobsForWorkteamResponse_labelingJobSummaryList = Lens.lens (\ListLabelingJobsForWorkteamResponse' {labelingJobSummaryList} -> labelingJobSummaryList) (\s@ListLabelingJobsForWorkteamResponse' {} a -> s {labelingJobSummaryList = a} :: ListLabelingJobsForWorkteamResponse) Prelude.. Lens._Coerce

instance
  Prelude.NFData
    ListLabelingJobsForWorkteamResponse
