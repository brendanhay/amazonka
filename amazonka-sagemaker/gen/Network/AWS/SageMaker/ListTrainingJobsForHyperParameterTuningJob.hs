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
-- Module      : Network.AWS.SageMaker.ListTrainingJobsForHyperParameterTuningJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of TrainingJobSummary objects that describe the training
-- jobs that a hyperparameter tuning job launched.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListTrainingJobsForHyperParameterTuningJob
  ( -- * Creating a Request
    ListTrainingJobsForHyperParameterTuningJob (..),
    newListTrainingJobsForHyperParameterTuningJob,

    -- * Request Lenses
    listTrainingJobsForHyperParameterTuningJob_sortOrder,
    listTrainingJobsForHyperParameterTuningJob_nextToken,
    listTrainingJobsForHyperParameterTuningJob_maxResults,
    listTrainingJobsForHyperParameterTuningJob_sortBy,
    listTrainingJobsForHyperParameterTuningJob_statusEquals,
    listTrainingJobsForHyperParameterTuningJob_hyperParameterTuningJobName,

    -- * Destructuring the Response
    ListTrainingJobsForHyperParameterTuningJobResponse (..),
    newListTrainingJobsForHyperParameterTuningJobResponse,

    -- * Response Lenses
    listTrainingJobsForHyperParameterTuningJobResponse_nextToken,
    listTrainingJobsForHyperParameterTuningJobResponse_httpStatus,
    listTrainingJobsForHyperParameterTuningJobResponse_trainingJobSummaries,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListTrainingJobsForHyperParameterTuningJob' smart constructor.
data ListTrainingJobsForHyperParameterTuningJob = ListTrainingJobsForHyperParameterTuningJob'
  { -- | The sort order for results. The default is @Ascending@.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | If the result of the previous
    -- @ListTrainingJobsForHyperParameterTuningJob@ request was truncated, the
    -- response includes a @NextToken@. To retrieve the next set of training
    -- jobs, use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of training jobs to return. The default value is 10.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The field to sort results by. The default is @Name@.
    --
    -- If the value of this field is @FinalObjectiveMetricValue@, any training
    -- jobs that did not return an objective metric are not listed.
    sortBy :: Prelude.Maybe TrainingJobSortByOptions,
    -- | A filter that returns only training jobs with the specified status.
    statusEquals :: Prelude.Maybe TrainingJobStatus,
    -- | The name of the tuning job whose training jobs you want to list.
    hyperParameterTuningJobName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTrainingJobsForHyperParameterTuningJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortOrder', 'listTrainingJobsForHyperParameterTuningJob_sortOrder' - The sort order for results. The default is @Ascending@.
--
-- 'nextToken', 'listTrainingJobsForHyperParameterTuningJob_nextToken' - If the result of the previous
-- @ListTrainingJobsForHyperParameterTuningJob@ request was truncated, the
-- response includes a @NextToken@. To retrieve the next set of training
-- jobs, use the token in the next request.
--
-- 'maxResults', 'listTrainingJobsForHyperParameterTuningJob_maxResults' - The maximum number of training jobs to return. The default value is 10.
--
-- 'sortBy', 'listTrainingJobsForHyperParameterTuningJob_sortBy' - The field to sort results by. The default is @Name@.
--
-- If the value of this field is @FinalObjectiveMetricValue@, any training
-- jobs that did not return an objective metric are not listed.
--
-- 'statusEquals', 'listTrainingJobsForHyperParameterTuningJob_statusEquals' - A filter that returns only training jobs with the specified status.
--
-- 'hyperParameterTuningJobName', 'listTrainingJobsForHyperParameterTuningJob_hyperParameterTuningJobName' - The name of the tuning job whose training jobs you want to list.
newListTrainingJobsForHyperParameterTuningJob ::
  -- | 'hyperParameterTuningJobName'
  Prelude.Text ->
  ListTrainingJobsForHyperParameterTuningJob
newListTrainingJobsForHyperParameterTuningJob
  pHyperParameterTuningJobName_ =
    ListTrainingJobsForHyperParameterTuningJob'
      { sortOrder =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        maxResults = Prelude.Nothing,
        sortBy = Prelude.Nothing,
        statusEquals = Prelude.Nothing,
        hyperParameterTuningJobName =
          pHyperParameterTuningJobName_
      }

-- | The sort order for results. The default is @Ascending@.
listTrainingJobsForHyperParameterTuningJob_sortOrder :: Lens.Lens' ListTrainingJobsForHyperParameterTuningJob (Prelude.Maybe SortOrder)
listTrainingJobsForHyperParameterTuningJob_sortOrder = Lens.lens (\ListTrainingJobsForHyperParameterTuningJob' {sortOrder} -> sortOrder) (\s@ListTrainingJobsForHyperParameterTuningJob' {} a -> s {sortOrder = a} :: ListTrainingJobsForHyperParameterTuningJob)

-- | If the result of the previous
-- @ListTrainingJobsForHyperParameterTuningJob@ request was truncated, the
-- response includes a @NextToken@. To retrieve the next set of training
-- jobs, use the token in the next request.
listTrainingJobsForHyperParameterTuningJob_nextToken :: Lens.Lens' ListTrainingJobsForHyperParameterTuningJob (Prelude.Maybe Prelude.Text)
listTrainingJobsForHyperParameterTuningJob_nextToken = Lens.lens (\ListTrainingJobsForHyperParameterTuningJob' {nextToken} -> nextToken) (\s@ListTrainingJobsForHyperParameterTuningJob' {} a -> s {nextToken = a} :: ListTrainingJobsForHyperParameterTuningJob)

-- | The maximum number of training jobs to return. The default value is 10.
listTrainingJobsForHyperParameterTuningJob_maxResults :: Lens.Lens' ListTrainingJobsForHyperParameterTuningJob (Prelude.Maybe Prelude.Natural)
listTrainingJobsForHyperParameterTuningJob_maxResults = Lens.lens (\ListTrainingJobsForHyperParameterTuningJob' {maxResults} -> maxResults) (\s@ListTrainingJobsForHyperParameterTuningJob' {} a -> s {maxResults = a} :: ListTrainingJobsForHyperParameterTuningJob)

-- | The field to sort results by. The default is @Name@.
--
-- If the value of this field is @FinalObjectiveMetricValue@, any training
-- jobs that did not return an objective metric are not listed.
listTrainingJobsForHyperParameterTuningJob_sortBy :: Lens.Lens' ListTrainingJobsForHyperParameterTuningJob (Prelude.Maybe TrainingJobSortByOptions)
listTrainingJobsForHyperParameterTuningJob_sortBy = Lens.lens (\ListTrainingJobsForHyperParameterTuningJob' {sortBy} -> sortBy) (\s@ListTrainingJobsForHyperParameterTuningJob' {} a -> s {sortBy = a} :: ListTrainingJobsForHyperParameterTuningJob)

-- | A filter that returns only training jobs with the specified status.
listTrainingJobsForHyperParameterTuningJob_statusEquals :: Lens.Lens' ListTrainingJobsForHyperParameterTuningJob (Prelude.Maybe TrainingJobStatus)
listTrainingJobsForHyperParameterTuningJob_statusEquals = Lens.lens (\ListTrainingJobsForHyperParameterTuningJob' {statusEquals} -> statusEquals) (\s@ListTrainingJobsForHyperParameterTuningJob' {} a -> s {statusEquals = a} :: ListTrainingJobsForHyperParameterTuningJob)

-- | The name of the tuning job whose training jobs you want to list.
listTrainingJobsForHyperParameterTuningJob_hyperParameterTuningJobName :: Lens.Lens' ListTrainingJobsForHyperParameterTuningJob Prelude.Text
listTrainingJobsForHyperParameterTuningJob_hyperParameterTuningJobName = Lens.lens (\ListTrainingJobsForHyperParameterTuningJob' {hyperParameterTuningJobName} -> hyperParameterTuningJobName) (\s@ListTrainingJobsForHyperParameterTuningJob' {} a -> s {hyperParameterTuningJobName = a} :: ListTrainingJobsForHyperParameterTuningJob)

instance
  Core.AWSPager
    ListTrainingJobsForHyperParameterTuningJob
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTrainingJobsForHyperParameterTuningJobResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listTrainingJobsForHyperParameterTuningJobResponse_trainingJobSummaries
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listTrainingJobsForHyperParameterTuningJob_nextToken
          Lens..~ rs
            Lens.^? listTrainingJobsForHyperParameterTuningJobResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListTrainingJobsForHyperParameterTuningJob
  where
  type
    AWSResponse
      ListTrainingJobsForHyperParameterTuningJob =
      ListTrainingJobsForHyperParameterTuningJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTrainingJobsForHyperParameterTuningJobResponse'
            Prelude.<$> (x Core..?> "NextToken")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
              Prelude.<*> ( x Core..?> "TrainingJobSummaries"
                              Core..!@ Prelude.mempty
                          )
      )

instance
  Prelude.Hashable
    ListTrainingJobsForHyperParameterTuningJob

instance
  Prelude.NFData
    ListTrainingJobsForHyperParameterTuningJob

instance
  Core.ToHeaders
    ListTrainingJobsForHyperParameterTuningJob
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.ListTrainingJobsForHyperParameterTuningJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    ListTrainingJobsForHyperParameterTuningJob
  where
  toJSON
    ListTrainingJobsForHyperParameterTuningJob' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("SortOrder" Core..=) Prelude.<$> sortOrder,
              ("NextToken" Core..=) Prelude.<$> nextToken,
              ("MaxResults" Core..=) Prelude.<$> maxResults,
              ("SortBy" Core..=) Prelude.<$> sortBy,
              ("StatusEquals" Core..=) Prelude.<$> statusEquals,
              Prelude.Just
                ( "HyperParameterTuningJobName"
                    Core..= hyperParameterTuningJobName
                )
            ]
        )

instance
  Core.ToPath
    ListTrainingJobsForHyperParameterTuningJob
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    ListTrainingJobsForHyperParameterTuningJob
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTrainingJobsForHyperParameterTuningJobResponse' smart constructor.
data ListTrainingJobsForHyperParameterTuningJobResponse = ListTrainingJobsForHyperParameterTuningJobResponse'
  { -- | If the result of this @ListTrainingJobsForHyperParameterTuningJob@
    -- request was truncated, the response includes a @NextToken@. To retrieve
    -- the next set of training jobs, use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of TrainingJobSummary objects that describe the training jobs
    -- that the @ListTrainingJobsForHyperParameterTuningJob@ request returned.
    trainingJobSummaries :: [HyperParameterTrainingJobSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTrainingJobsForHyperParameterTuningJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTrainingJobsForHyperParameterTuningJobResponse_nextToken' - If the result of this @ListTrainingJobsForHyperParameterTuningJob@
-- request was truncated, the response includes a @NextToken@. To retrieve
-- the next set of training jobs, use the token in the next request.
--
-- 'httpStatus', 'listTrainingJobsForHyperParameterTuningJobResponse_httpStatus' - The response's http status code.
--
-- 'trainingJobSummaries', 'listTrainingJobsForHyperParameterTuningJobResponse_trainingJobSummaries' - A list of TrainingJobSummary objects that describe the training jobs
-- that the @ListTrainingJobsForHyperParameterTuningJob@ request returned.
newListTrainingJobsForHyperParameterTuningJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTrainingJobsForHyperParameterTuningJobResponse
newListTrainingJobsForHyperParameterTuningJobResponse
  pHttpStatus_ =
    ListTrainingJobsForHyperParameterTuningJobResponse'
      { nextToken =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_,
        trainingJobSummaries =
          Prelude.mempty
      }

-- | If the result of this @ListTrainingJobsForHyperParameterTuningJob@
-- request was truncated, the response includes a @NextToken@. To retrieve
-- the next set of training jobs, use the token in the next request.
listTrainingJobsForHyperParameterTuningJobResponse_nextToken :: Lens.Lens' ListTrainingJobsForHyperParameterTuningJobResponse (Prelude.Maybe Prelude.Text)
listTrainingJobsForHyperParameterTuningJobResponse_nextToken = Lens.lens (\ListTrainingJobsForHyperParameterTuningJobResponse' {nextToken} -> nextToken) (\s@ListTrainingJobsForHyperParameterTuningJobResponse' {} a -> s {nextToken = a} :: ListTrainingJobsForHyperParameterTuningJobResponse)

-- | The response's http status code.
listTrainingJobsForHyperParameterTuningJobResponse_httpStatus :: Lens.Lens' ListTrainingJobsForHyperParameterTuningJobResponse Prelude.Int
listTrainingJobsForHyperParameterTuningJobResponse_httpStatus = Lens.lens (\ListTrainingJobsForHyperParameterTuningJobResponse' {httpStatus} -> httpStatus) (\s@ListTrainingJobsForHyperParameterTuningJobResponse' {} a -> s {httpStatus = a} :: ListTrainingJobsForHyperParameterTuningJobResponse)

-- | A list of TrainingJobSummary objects that describe the training jobs
-- that the @ListTrainingJobsForHyperParameterTuningJob@ request returned.
listTrainingJobsForHyperParameterTuningJobResponse_trainingJobSummaries :: Lens.Lens' ListTrainingJobsForHyperParameterTuningJobResponse [HyperParameterTrainingJobSummary]
listTrainingJobsForHyperParameterTuningJobResponse_trainingJobSummaries = Lens.lens (\ListTrainingJobsForHyperParameterTuningJobResponse' {trainingJobSummaries} -> trainingJobSummaries) (\s@ListTrainingJobsForHyperParameterTuningJobResponse' {} a -> s {trainingJobSummaries = a} :: ListTrainingJobsForHyperParameterTuningJobResponse) Prelude.. Lens._Coerce

instance
  Prelude.NFData
    ListTrainingJobsForHyperParameterTuningJobResponse
