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
-- Module      : Amazonka.SageMaker.ListTrainingJobsForHyperParameterTuningJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of TrainingJobSummary objects that describe the training
-- jobs that a hyperparameter tuning job launched.
--
-- This operation returns paginated results.
module Amazonka.SageMaker.ListTrainingJobsForHyperParameterTuningJob
  ( -- * Creating a Request
    ListTrainingJobsForHyperParameterTuningJob (..),
    newListTrainingJobsForHyperParameterTuningJob,

    -- * Request Lenses
    listTrainingJobsForHyperParameterTuningJob_maxResults,
    listTrainingJobsForHyperParameterTuningJob_nextToken,
    listTrainingJobsForHyperParameterTuningJob_sortBy,
    listTrainingJobsForHyperParameterTuningJob_sortOrder,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListTrainingJobsForHyperParameterTuningJob' smart constructor.
data ListTrainingJobsForHyperParameterTuningJob = ListTrainingJobsForHyperParameterTuningJob'
  { -- | The maximum number of training jobs to return. The default value is 10.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the result of the previous
    -- @ListTrainingJobsForHyperParameterTuningJob@ request was truncated, the
    -- response includes a @NextToken@. To retrieve the next set of training
    -- jobs, use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The field to sort results by. The default is @Name@.
    --
    -- If the value of this field is @FinalObjectiveMetricValue@, any training
    -- jobs that did not return an objective metric are not listed.
    sortBy :: Prelude.Maybe TrainingJobSortByOptions,
    -- | The sort order for results. The default is @Ascending@.
    sortOrder :: Prelude.Maybe SortOrder,
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
-- 'maxResults', 'listTrainingJobsForHyperParameterTuningJob_maxResults' - The maximum number of training jobs to return. The default value is 10.
--
-- 'nextToken', 'listTrainingJobsForHyperParameterTuningJob_nextToken' - If the result of the previous
-- @ListTrainingJobsForHyperParameterTuningJob@ request was truncated, the
-- response includes a @NextToken@. To retrieve the next set of training
-- jobs, use the token in the next request.
--
-- 'sortBy', 'listTrainingJobsForHyperParameterTuningJob_sortBy' - The field to sort results by. The default is @Name@.
--
-- If the value of this field is @FinalObjectiveMetricValue@, any training
-- jobs that did not return an objective metric are not listed.
--
-- 'sortOrder', 'listTrainingJobsForHyperParameterTuningJob_sortOrder' - The sort order for results. The default is @Ascending@.
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
      { maxResults =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        sortBy = Prelude.Nothing,
        sortOrder = Prelude.Nothing,
        statusEquals = Prelude.Nothing,
        hyperParameterTuningJobName =
          pHyperParameterTuningJobName_
      }

-- | The maximum number of training jobs to return. The default value is 10.
listTrainingJobsForHyperParameterTuningJob_maxResults :: Lens.Lens' ListTrainingJobsForHyperParameterTuningJob (Prelude.Maybe Prelude.Natural)
listTrainingJobsForHyperParameterTuningJob_maxResults = Lens.lens (\ListTrainingJobsForHyperParameterTuningJob' {maxResults} -> maxResults) (\s@ListTrainingJobsForHyperParameterTuningJob' {} a -> s {maxResults = a} :: ListTrainingJobsForHyperParameterTuningJob)

-- | If the result of the previous
-- @ListTrainingJobsForHyperParameterTuningJob@ request was truncated, the
-- response includes a @NextToken@. To retrieve the next set of training
-- jobs, use the token in the next request.
listTrainingJobsForHyperParameterTuningJob_nextToken :: Lens.Lens' ListTrainingJobsForHyperParameterTuningJob (Prelude.Maybe Prelude.Text)
listTrainingJobsForHyperParameterTuningJob_nextToken = Lens.lens (\ListTrainingJobsForHyperParameterTuningJob' {nextToken} -> nextToken) (\s@ListTrainingJobsForHyperParameterTuningJob' {} a -> s {nextToken = a} :: ListTrainingJobsForHyperParameterTuningJob)

-- | The field to sort results by. The default is @Name@.
--
-- If the value of this field is @FinalObjectiveMetricValue@, any training
-- jobs that did not return an objective metric are not listed.
listTrainingJobsForHyperParameterTuningJob_sortBy :: Lens.Lens' ListTrainingJobsForHyperParameterTuningJob (Prelude.Maybe TrainingJobSortByOptions)
listTrainingJobsForHyperParameterTuningJob_sortBy = Lens.lens (\ListTrainingJobsForHyperParameterTuningJob' {sortBy} -> sortBy) (\s@ListTrainingJobsForHyperParameterTuningJob' {} a -> s {sortBy = a} :: ListTrainingJobsForHyperParameterTuningJob)

-- | The sort order for results. The default is @Ascending@.
listTrainingJobsForHyperParameterTuningJob_sortOrder :: Lens.Lens' ListTrainingJobsForHyperParameterTuningJob (Prelude.Maybe SortOrder)
listTrainingJobsForHyperParameterTuningJob_sortOrder = Lens.lens (\ListTrainingJobsForHyperParameterTuningJob' {sortOrder} -> sortOrder) (\s@ListTrainingJobsForHyperParameterTuningJob' {} a -> s {sortOrder = a} :: ListTrainingJobsForHyperParameterTuningJob)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTrainingJobsForHyperParameterTuningJobResponse'
            Prelude.<$> (x Data..?> "NextToken")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
              Prelude.<*> ( x Data..?> "TrainingJobSummaries"
                              Core..!@ Prelude.mempty
                          )
      )

instance
  Prelude.Hashable
    ListTrainingJobsForHyperParameterTuningJob
  where
  hashWithSalt
    _salt
    ListTrainingJobsForHyperParameterTuningJob' {..} =
      _salt `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` sortBy
        `Prelude.hashWithSalt` sortOrder
        `Prelude.hashWithSalt` statusEquals
        `Prelude.hashWithSalt` hyperParameterTuningJobName

instance
  Prelude.NFData
    ListTrainingJobsForHyperParameterTuningJob
  where
  rnf ListTrainingJobsForHyperParameterTuningJob' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf statusEquals
      `Prelude.seq` Prelude.rnf hyperParameterTuningJobName

instance
  Data.ToHeaders
    ListTrainingJobsForHyperParameterTuningJob
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.ListTrainingJobsForHyperParameterTuningJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    ListTrainingJobsForHyperParameterTuningJob
  where
  toJSON
    ListTrainingJobsForHyperParameterTuningJob' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("MaxResults" Data..=) Prelude.<$> maxResults,
              ("NextToken" Data..=) Prelude.<$> nextToken,
              ("SortBy" Data..=) Prelude.<$> sortBy,
              ("SortOrder" Data..=) Prelude.<$> sortOrder,
              ("StatusEquals" Data..=) Prelude.<$> statusEquals,
              Prelude.Just
                ( "HyperParameterTuningJobName"
                    Data..= hyperParameterTuningJobName
                )
            ]
        )

instance
  Data.ToPath
    ListTrainingJobsForHyperParameterTuningJob
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
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
listTrainingJobsForHyperParameterTuningJobResponse_trainingJobSummaries = Lens.lens (\ListTrainingJobsForHyperParameterTuningJobResponse' {trainingJobSummaries} -> trainingJobSummaries) (\s@ListTrainingJobsForHyperParameterTuningJobResponse' {} a -> s {trainingJobSummaries = a} :: ListTrainingJobsForHyperParameterTuningJobResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListTrainingJobsForHyperParameterTuningJobResponse
  where
  rnf
    ListTrainingJobsForHyperParameterTuningJobResponse' {..} =
      Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf httpStatus
        `Prelude.seq` Prelude.rnf trainingJobSummaries
