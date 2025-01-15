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
-- Module      : Amazonka.SageMaker.ListPipelineExecutionSteps
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of @PipeLineExecutionStep@ objects.
--
-- This operation returns paginated results.
module Amazonka.SageMaker.ListPipelineExecutionSteps
  ( -- * Creating a Request
    ListPipelineExecutionSteps (..),
    newListPipelineExecutionSteps,

    -- * Request Lenses
    listPipelineExecutionSteps_maxResults,
    listPipelineExecutionSteps_nextToken,
    listPipelineExecutionSteps_pipelineExecutionArn,
    listPipelineExecutionSteps_sortOrder,

    -- * Destructuring the Response
    ListPipelineExecutionStepsResponse (..),
    newListPipelineExecutionStepsResponse,

    -- * Response Lenses
    listPipelineExecutionStepsResponse_nextToken,
    listPipelineExecutionStepsResponse_pipelineExecutionSteps,
    listPipelineExecutionStepsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListPipelineExecutionSteps' smart constructor.
data ListPipelineExecutionSteps = ListPipelineExecutionSteps'
  { -- | The maximum number of pipeline execution steps to return in the
    -- response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the result of the previous @ListPipelineExecutionSteps@ request was
    -- truncated, the response includes a @NextToken@. To retrieve the next set
    -- of pipeline execution steps, use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the pipeline execution.
    pipelineExecutionArn :: Prelude.Maybe Prelude.Text,
    -- | The field by which to sort results. The default is @CreatedTime@.
    sortOrder :: Prelude.Maybe SortOrder
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPipelineExecutionSteps' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listPipelineExecutionSteps_maxResults' - The maximum number of pipeline execution steps to return in the
-- response.
--
-- 'nextToken', 'listPipelineExecutionSteps_nextToken' - If the result of the previous @ListPipelineExecutionSteps@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of pipeline execution steps, use the token in the next request.
--
-- 'pipelineExecutionArn', 'listPipelineExecutionSteps_pipelineExecutionArn' - The Amazon Resource Name (ARN) of the pipeline execution.
--
-- 'sortOrder', 'listPipelineExecutionSteps_sortOrder' - The field by which to sort results. The default is @CreatedTime@.
newListPipelineExecutionSteps ::
  ListPipelineExecutionSteps
newListPipelineExecutionSteps =
  ListPipelineExecutionSteps'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      pipelineExecutionArn = Prelude.Nothing,
      sortOrder = Prelude.Nothing
    }

-- | The maximum number of pipeline execution steps to return in the
-- response.
listPipelineExecutionSteps_maxResults :: Lens.Lens' ListPipelineExecutionSteps (Prelude.Maybe Prelude.Natural)
listPipelineExecutionSteps_maxResults = Lens.lens (\ListPipelineExecutionSteps' {maxResults} -> maxResults) (\s@ListPipelineExecutionSteps' {} a -> s {maxResults = a} :: ListPipelineExecutionSteps)

-- | If the result of the previous @ListPipelineExecutionSteps@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of pipeline execution steps, use the token in the next request.
listPipelineExecutionSteps_nextToken :: Lens.Lens' ListPipelineExecutionSteps (Prelude.Maybe Prelude.Text)
listPipelineExecutionSteps_nextToken = Lens.lens (\ListPipelineExecutionSteps' {nextToken} -> nextToken) (\s@ListPipelineExecutionSteps' {} a -> s {nextToken = a} :: ListPipelineExecutionSteps)

-- | The Amazon Resource Name (ARN) of the pipeline execution.
listPipelineExecutionSteps_pipelineExecutionArn :: Lens.Lens' ListPipelineExecutionSteps (Prelude.Maybe Prelude.Text)
listPipelineExecutionSteps_pipelineExecutionArn = Lens.lens (\ListPipelineExecutionSteps' {pipelineExecutionArn} -> pipelineExecutionArn) (\s@ListPipelineExecutionSteps' {} a -> s {pipelineExecutionArn = a} :: ListPipelineExecutionSteps)

-- | The field by which to sort results. The default is @CreatedTime@.
listPipelineExecutionSteps_sortOrder :: Lens.Lens' ListPipelineExecutionSteps (Prelude.Maybe SortOrder)
listPipelineExecutionSteps_sortOrder = Lens.lens (\ListPipelineExecutionSteps' {sortOrder} -> sortOrder) (\s@ListPipelineExecutionSteps' {} a -> s {sortOrder = a} :: ListPipelineExecutionSteps)

instance Core.AWSPager ListPipelineExecutionSteps where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPipelineExecutionStepsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listPipelineExecutionStepsResponse_pipelineExecutionSteps
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listPipelineExecutionSteps_nextToken
              Lens..~ rs
              Lens.^? listPipelineExecutionStepsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListPipelineExecutionSteps where
  type
    AWSResponse ListPipelineExecutionSteps =
      ListPipelineExecutionStepsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPipelineExecutionStepsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "PipelineExecutionSteps"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPipelineExecutionSteps where
  hashWithSalt _salt ListPipelineExecutionSteps' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` pipelineExecutionArn
      `Prelude.hashWithSalt` sortOrder

instance Prelude.NFData ListPipelineExecutionSteps where
  rnf ListPipelineExecutionSteps' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf pipelineExecutionArn `Prelude.seq`
          Prelude.rnf sortOrder

instance Data.ToHeaders ListPipelineExecutionSteps where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.ListPipelineExecutionSteps" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListPipelineExecutionSteps where
  toJSON ListPipelineExecutionSteps' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("PipelineExecutionArn" Data..=)
              Prelude.<$> pipelineExecutionArn,
            ("SortOrder" Data..=) Prelude.<$> sortOrder
          ]
      )

instance Data.ToPath ListPipelineExecutionSteps where
  toPath = Prelude.const "/"

instance Data.ToQuery ListPipelineExecutionSteps where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListPipelineExecutionStepsResponse' smart constructor.
data ListPipelineExecutionStepsResponse = ListPipelineExecutionStepsResponse'
  { -- | If the result of the previous @ListPipelineExecutionSteps@ request was
    -- truncated, the response includes a @NextToken@. To retrieve the next set
    -- of pipeline execution steps, use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of @PipeLineExecutionStep@ objects. Each @PipeLineExecutionStep@
    -- consists of StepName, StartTime, EndTime, StepStatus, and Metadata.
    -- Metadata is an object with properties for each job that contains
    -- relevant information about the job created by the step.
    pipelineExecutionSteps :: Prelude.Maybe [PipelineExecutionStep],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPipelineExecutionStepsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPipelineExecutionStepsResponse_nextToken' - If the result of the previous @ListPipelineExecutionSteps@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of pipeline execution steps, use the token in the next request.
--
-- 'pipelineExecutionSteps', 'listPipelineExecutionStepsResponse_pipelineExecutionSteps' - A list of @PipeLineExecutionStep@ objects. Each @PipeLineExecutionStep@
-- consists of StepName, StartTime, EndTime, StepStatus, and Metadata.
-- Metadata is an object with properties for each job that contains
-- relevant information about the job created by the step.
--
-- 'httpStatus', 'listPipelineExecutionStepsResponse_httpStatus' - The response's http status code.
newListPipelineExecutionStepsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPipelineExecutionStepsResponse
newListPipelineExecutionStepsResponse pHttpStatus_ =
  ListPipelineExecutionStepsResponse'
    { nextToken =
        Prelude.Nothing,
      pipelineExecutionSteps =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the result of the previous @ListPipelineExecutionSteps@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of pipeline execution steps, use the token in the next request.
listPipelineExecutionStepsResponse_nextToken :: Lens.Lens' ListPipelineExecutionStepsResponse (Prelude.Maybe Prelude.Text)
listPipelineExecutionStepsResponse_nextToken = Lens.lens (\ListPipelineExecutionStepsResponse' {nextToken} -> nextToken) (\s@ListPipelineExecutionStepsResponse' {} a -> s {nextToken = a} :: ListPipelineExecutionStepsResponse)

-- | A list of @PipeLineExecutionStep@ objects. Each @PipeLineExecutionStep@
-- consists of StepName, StartTime, EndTime, StepStatus, and Metadata.
-- Metadata is an object with properties for each job that contains
-- relevant information about the job created by the step.
listPipelineExecutionStepsResponse_pipelineExecutionSteps :: Lens.Lens' ListPipelineExecutionStepsResponse (Prelude.Maybe [PipelineExecutionStep])
listPipelineExecutionStepsResponse_pipelineExecutionSteps = Lens.lens (\ListPipelineExecutionStepsResponse' {pipelineExecutionSteps} -> pipelineExecutionSteps) (\s@ListPipelineExecutionStepsResponse' {} a -> s {pipelineExecutionSteps = a} :: ListPipelineExecutionStepsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listPipelineExecutionStepsResponse_httpStatus :: Lens.Lens' ListPipelineExecutionStepsResponse Prelude.Int
listPipelineExecutionStepsResponse_httpStatus = Lens.lens (\ListPipelineExecutionStepsResponse' {httpStatus} -> httpStatus) (\s@ListPipelineExecutionStepsResponse' {} a -> s {httpStatus = a} :: ListPipelineExecutionStepsResponse)

instance
  Prelude.NFData
    ListPipelineExecutionStepsResponse
  where
  rnf ListPipelineExecutionStepsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf pipelineExecutionSteps `Prelude.seq`
        Prelude.rnf httpStatus
