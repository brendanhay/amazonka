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
-- Module      : Amazonka.SageMaker.ListInferenceRecommendationsJobSteps
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the subtasks for an Inference Recommender job.
--
-- The supported subtasks are benchmarks, which evaluate the performance of
-- your model on different instance types.
--
-- This operation returns paginated results.
module Amazonka.SageMaker.ListInferenceRecommendationsJobSteps
  ( -- * Creating a Request
    ListInferenceRecommendationsJobSteps (..),
    newListInferenceRecommendationsJobSteps,

    -- * Request Lenses
    listInferenceRecommendationsJobSteps_maxResults,
    listInferenceRecommendationsJobSteps_nextToken,
    listInferenceRecommendationsJobSteps_status,
    listInferenceRecommendationsJobSteps_stepType,
    listInferenceRecommendationsJobSteps_jobName,

    -- * Destructuring the Response
    ListInferenceRecommendationsJobStepsResponse (..),
    newListInferenceRecommendationsJobStepsResponse,

    -- * Response Lenses
    listInferenceRecommendationsJobStepsResponse_nextToken,
    listInferenceRecommendationsJobStepsResponse_steps,
    listInferenceRecommendationsJobStepsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListInferenceRecommendationsJobSteps' smart constructor.
data ListInferenceRecommendationsJobSteps = ListInferenceRecommendationsJobSteps'
  { -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token that you can specify to return more results from the list.
    -- Specify this field if you have a token that was returned from a previous
    -- request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A filter to return benchmarks of a specified status. If this field is
    -- left empty, then all benchmarks are returned.
    status :: Prelude.Maybe RecommendationJobStatus,
    -- | A filter to return details about the specified type of subtask.
    --
    -- @BENCHMARK@: Evaluate the performance of your model on different
    -- instance types.
    stepType :: Prelude.Maybe RecommendationStepType,
    -- | The name for the Inference Recommender job.
    jobName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListInferenceRecommendationsJobSteps' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listInferenceRecommendationsJobSteps_maxResults' - The maximum number of results to return.
--
-- 'nextToken', 'listInferenceRecommendationsJobSteps_nextToken' - A token that you can specify to return more results from the list.
-- Specify this field if you have a token that was returned from a previous
-- request.
--
-- 'status', 'listInferenceRecommendationsJobSteps_status' - A filter to return benchmarks of a specified status. If this field is
-- left empty, then all benchmarks are returned.
--
-- 'stepType', 'listInferenceRecommendationsJobSteps_stepType' - A filter to return details about the specified type of subtask.
--
-- @BENCHMARK@: Evaluate the performance of your model on different
-- instance types.
--
-- 'jobName', 'listInferenceRecommendationsJobSteps_jobName' - The name for the Inference Recommender job.
newListInferenceRecommendationsJobSteps ::
  -- | 'jobName'
  Prelude.Text ->
  ListInferenceRecommendationsJobSteps
newListInferenceRecommendationsJobSteps pJobName_ =
  ListInferenceRecommendationsJobSteps'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      status = Prelude.Nothing,
      stepType = Prelude.Nothing,
      jobName = pJobName_
    }

-- | The maximum number of results to return.
listInferenceRecommendationsJobSteps_maxResults :: Lens.Lens' ListInferenceRecommendationsJobSteps (Prelude.Maybe Prelude.Natural)
listInferenceRecommendationsJobSteps_maxResults = Lens.lens (\ListInferenceRecommendationsJobSteps' {maxResults} -> maxResults) (\s@ListInferenceRecommendationsJobSteps' {} a -> s {maxResults = a} :: ListInferenceRecommendationsJobSteps)

-- | A token that you can specify to return more results from the list.
-- Specify this field if you have a token that was returned from a previous
-- request.
listInferenceRecommendationsJobSteps_nextToken :: Lens.Lens' ListInferenceRecommendationsJobSteps (Prelude.Maybe Prelude.Text)
listInferenceRecommendationsJobSteps_nextToken = Lens.lens (\ListInferenceRecommendationsJobSteps' {nextToken} -> nextToken) (\s@ListInferenceRecommendationsJobSteps' {} a -> s {nextToken = a} :: ListInferenceRecommendationsJobSteps)

-- | A filter to return benchmarks of a specified status. If this field is
-- left empty, then all benchmarks are returned.
listInferenceRecommendationsJobSteps_status :: Lens.Lens' ListInferenceRecommendationsJobSteps (Prelude.Maybe RecommendationJobStatus)
listInferenceRecommendationsJobSteps_status = Lens.lens (\ListInferenceRecommendationsJobSteps' {status} -> status) (\s@ListInferenceRecommendationsJobSteps' {} a -> s {status = a} :: ListInferenceRecommendationsJobSteps)

-- | A filter to return details about the specified type of subtask.
--
-- @BENCHMARK@: Evaluate the performance of your model on different
-- instance types.
listInferenceRecommendationsJobSteps_stepType :: Lens.Lens' ListInferenceRecommendationsJobSteps (Prelude.Maybe RecommendationStepType)
listInferenceRecommendationsJobSteps_stepType = Lens.lens (\ListInferenceRecommendationsJobSteps' {stepType} -> stepType) (\s@ListInferenceRecommendationsJobSteps' {} a -> s {stepType = a} :: ListInferenceRecommendationsJobSteps)

-- | The name for the Inference Recommender job.
listInferenceRecommendationsJobSteps_jobName :: Lens.Lens' ListInferenceRecommendationsJobSteps Prelude.Text
listInferenceRecommendationsJobSteps_jobName = Lens.lens (\ListInferenceRecommendationsJobSteps' {jobName} -> jobName) (\s@ListInferenceRecommendationsJobSteps' {} a -> s {jobName = a} :: ListInferenceRecommendationsJobSteps)

instance
  Core.AWSPager
    ListInferenceRecommendationsJobSteps
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listInferenceRecommendationsJobStepsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listInferenceRecommendationsJobStepsResponse_steps
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listInferenceRecommendationsJobSteps_nextToken
          Lens..~ rs
            Lens.^? listInferenceRecommendationsJobStepsResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListInferenceRecommendationsJobSteps
  where
  type
    AWSResponse ListInferenceRecommendationsJobSteps =
      ListInferenceRecommendationsJobStepsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListInferenceRecommendationsJobStepsResponse'
            Prelude.<$> (x Data..?> "NextToken")
              Prelude.<*> (x Data..?> "Steps" Core..!@ Prelude.mempty)
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListInferenceRecommendationsJobSteps
  where
  hashWithSalt
    _salt
    ListInferenceRecommendationsJobSteps' {..} =
      _salt `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` stepType
        `Prelude.hashWithSalt` jobName

instance
  Prelude.NFData
    ListInferenceRecommendationsJobSteps
  where
  rnf ListInferenceRecommendationsJobSteps' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf stepType
      `Prelude.seq` Prelude.rnf jobName

instance
  Data.ToHeaders
    ListInferenceRecommendationsJobSteps
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.ListInferenceRecommendationsJobSteps" ::
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
    ListInferenceRecommendationsJobSteps
  where
  toJSON ListInferenceRecommendationsJobSteps' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("Status" Data..=) Prelude.<$> status,
            ("StepType" Data..=) Prelude.<$> stepType,
            Prelude.Just ("JobName" Data..= jobName)
          ]
      )

instance
  Data.ToPath
    ListInferenceRecommendationsJobSteps
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ListInferenceRecommendationsJobSteps
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListInferenceRecommendationsJobStepsResponse' smart constructor.
data ListInferenceRecommendationsJobStepsResponse = ListInferenceRecommendationsJobStepsResponse'
  { -- | A token that you can specify in your next request to return more results
    -- from the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of all subtask details in Inference Recommender.
    steps :: Prelude.Maybe [InferenceRecommendationsJobStep],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListInferenceRecommendationsJobStepsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listInferenceRecommendationsJobStepsResponse_nextToken' - A token that you can specify in your next request to return more results
-- from the list.
--
-- 'steps', 'listInferenceRecommendationsJobStepsResponse_steps' - A list of all subtask details in Inference Recommender.
--
-- 'httpStatus', 'listInferenceRecommendationsJobStepsResponse_httpStatus' - The response's http status code.
newListInferenceRecommendationsJobStepsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListInferenceRecommendationsJobStepsResponse
newListInferenceRecommendationsJobStepsResponse
  pHttpStatus_ =
    ListInferenceRecommendationsJobStepsResponse'
      { nextToken =
          Prelude.Nothing,
        steps = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A token that you can specify in your next request to return more results
-- from the list.
listInferenceRecommendationsJobStepsResponse_nextToken :: Lens.Lens' ListInferenceRecommendationsJobStepsResponse (Prelude.Maybe Prelude.Text)
listInferenceRecommendationsJobStepsResponse_nextToken = Lens.lens (\ListInferenceRecommendationsJobStepsResponse' {nextToken} -> nextToken) (\s@ListInferenceRecommendationsJobStepsResponse' {} a -> s {nextToken = a} :: ListInferenceRecommendationsJobStepsResponse)

-- | A list of all subtask details in Inference Recommender.
listInferenceRecommendationsJobStepsResponse_steps :: Lens.Lens' ListInferenceRecommendationsJobStepsResponse (Prelude.Maybe [InferenceRecommendationsJobStep])
listInferenceRecommendationsJobStepsResponse_steps = Lens.lens (\ListInferenceRecommendationsJobStepsResponse' {steps} -> steps) (\s@ListInferenceRecommendationsJobStepsResponse' {} a -> s {steps = a} :: ListInferenceRecommendationsJobStepsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listInferenceRecommendationsJobStepsResponse_httpStatus :: Lens.Lens' ListInferenceRecommendationsJobStepsResponse Prelude.Int
listInferenceRecommendationsJobStepsResponse_httpStatus = Lens.lens (\ListInferenceRecommendationsJobStepsResponse' {httpStatus} -> httpStatus) (\s@ListInferenceRecommendationsJobStepsResponse' {} a -> s {httpStatus = a} :: ListInferenceRecommendationsJobStepsResponse)

instance
  Prelude.NFData
    ListInferenceRecommendationsJobStepsResponse
  where
  rnf ListInferenceRecommendationsJobStepsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf steps
      `Prelude.seq` Prelude.rnf httpStatus
