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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
    listPipelineExecutionSteps_pipelineExecutionArn,
    listPipelineExecutionSteps_nextToken,
    listPipelineExecutionSteps_sortOrder,
    listPipelineExecutionSteps_maxResults,

    -- * Destructuring the Response
    ListPipelineExecutionStepsResponse (..),
    newListPipelineExecutionStepsResponse,

    -- * Response Lenses
    listPipelineExecutionStepsResponse_pipelineExecutionSteps,
    listPipelineExecutionStepsResponse_nextToken,
    listPipelineExecutionStepsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListPipelineExecutionSteps' smart constructor.
data ListPipelineExecutionSteps = ListPipelineExecutionSteps'
  { -- | The Amazon Resource Name (ARN) of the pipeline execution.
    pipelineExecutionArn :: Prelude.Maybe Prelude.Text,
    -- | If the result of the previous @ListPipelineExecutionSteps@ request was
    -- truncated, the response includes a @NextToken@. To retrieve the next set
    -- of pipeline execution steps, use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The field by which to sort results. The default is @CreatedTime@.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | The maximum number of pipeline execution steps to return in the
    -- response.
    maxResults :: Prelude.Maybe Prelude.Natural
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
-- 'pipelineExecutionArn', 'listPipelineExecutionSteps_pipelineExecutionArn' - The Amazon Resource Name (ARN) of the pipeline execution.
--
-- 'nextToken', 'listPipelineExecutionSteps_nextToken' - If the result of the previous @ListPipelineExecutionSteps@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of pipeline execution steps, use the token in the next request.
--
-- 'sortOrder', 'listPipelineExecutionSteps_sortOrder' - The field by which to sort results. The default is @CreatedTime@.
--
-- 'maxResults', 'listPipelineExecutionSteps_maxResults' - The maximum number of pipeline execution steps to return in the
-- response.
newListPipelineExecutionSteps ::
  ListPipelineExecutionSteps
newListPipelineExecutionSteps =
  ListPipelineExecutionSteps'
    { pipelineExecutionArn =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the pipeline execution.
listPipelineExecutionSteps_pipelineExecutionArn :: Lens.Lens' ListPipelineExecutionSteps (Prelude.Maybe Prelude.Text)
listPipelineExecutionSteps_pipelineExecutionArn = Lens.lens (\ListPipelineExecutionSteps' {pipelineExecutionArn} -> pipelineExecutionArn) (\s@ListPipelineExecutionSteps' {} a -> s {pipelineExecutionArn = a} :: ListPipelineExecutionSteps)

-- | If the result of the previous @ListPipelineExecutionSteps@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of pipeline execution steps, use the token in the next request.
listPipelineExecutionSteps_nextToken :: Lens.Lens' ListPipelineExecutionSteps (Prelude.Maybe Prelude.Text)
listPipelineExecutionSteps_nextToken = Lens.lens (\ListPipelineExecutionSteps' {nextToken} -> nextToken) (\s@ListPipelineExecutionSteps' {} a -> s {nextToken = a} :: ListPipelineExecutionSteps)

-- | The field by which to sort results. The default is @CreatedTime@.
listPipelineExecutionSteps_sortOrder :: Lens.Lens' ListPipelineExecutionSteps (Prelude.Maybe SortOrder)
listPipelineExecutionSteps_sortOrder = Lens.lens (\ListPipelineExecutionSteps' {sortOrder} -> sortOrder) (\s@ListPipelineExecutionSteps' {} a -> s {sortOrder = a} :: ListPipelineExecutionSteps)

-- | The maximum number of pipeline execution steps to return in the
-- response.
listPipelineExecutionSteps_maxResults :: Lens.Lens' ListPipelineExecutionSteps (Prelude.Maybe Prelude.Natural)
listPipelineExecutionSteps_maxResults = Lens.lens (\ListPipelineExecutionSteps' {maxResults} -> maxResults) (\s@ListPipelineExecutionSteps' {} a -> s {maxResults = a} :: ListPipelineExecutionSteps)

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPipelineExecutionStepsResponse'
            Prelude.<$> ( x Core..?> "PipelineExecutionSteps"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPipelineExecutionSteps

instance Prelude.NFData ListPipelineExecutionSteps

instance Core.ToHeaders ListPipelineExecutionSteps where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.ListPipelineExecutionSteps" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListPipelineExecutionSteps where
  toJSON ListPipelineExecutionSteps' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PipelineExecutionArn" Core..=)
              Prelude.<$> pipelineExecutionArn,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("SortOrder" Core..=) Prelude.<$> sortOrder,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListPipelineExecutionSteps where
  toPath = Prelude.const "/"

instance Core.ToQuery ListPipelineExecutionSteps where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListPipelineExecutionStepsResponse' smart constructor.
data ListPipelineExecutionStepsResponse = ListPipelineExecutionStepsResponse'
  { -- | A list of @PipeLineExecutionStep@ objects. Each @PipeLineExecutionStep@
    -- consists of StepName, StartTime, EndTime, StepStatus, and Metadata.
    -- Metadata is an object with properties for each job that contains
    -- relevant information about the job created by the step.
    pipelineExecutionSteps :: Prelude.Maybe [PipelineExecutionStep],
    -- | If the result of the previous @ListPipelineExecutionSteps@ request was
    -- truncated, the response includes a @NextToken@. To retrieve the next set
    -- of pipeline execution steps, use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'pipelineExecutionSteps', 'listPipelineExecutionStepsResponse_pipelineExecutionSteps' - A list of @PipeLineExecutionStep@ objects. Each @PipeLineExecutionStep@
-- consists of StepName, StartTime, EndTime, StepStatus, and Metadata.
-- Metadata is an object with properties for each job that contains
-- relevant information about the job created by the step.
--
-- 'nextToken', 'listPipelineExecutionStepsResponse_nextToken' - If the result of the previous @ListPipelineExecutionSteps@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of pipeline execution steps, use the token in the next request.
--
-- 'httpStatus', 'listPipelineExecutionStepsResponse_httpStatus' - The response's http status code.
newListPipelineExecutionStepsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPipelineExecutionStepsResponse
newListPipelineExecutionStepsResponse pHttpStatus_ =
  ListPipelineExecutionStepsResponse'
    { pipelineExecutionSteps =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of @PipeLineExecutionStep@ objects. Each @PipeLineExecutionStep@
-- consists of StepName, StartTime, EndTime, StepStatus, and Metadata.
-- Metadata is an object with properties for each job that contains
-- relevant information about the job created by the step.
listPipelineExecutionStepsResponse_pipelineExecutionSteps :: Lens.Lens' ListPipelineExecutionStepsResponse (Prelude.Maybe [PipelineExecutionStep])
listPipelineExecutionStepsResponse_pipelineExecutionSteps = Lens.lens (\ListPipelineExecutionStepsResponse' {pipelineExecutionSteps} -> pipelineExecutionSteps) (\s@ListPipelineExecutionStepsResponse' {} a -> s {pipelineExecutionSteps = a} :: ListPipelineExecutionStepsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If the result of the previous @ListPipelineExecutionSteps@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of pipeline execution steps, use the token in the next request.
listPipelineExecutionStepsResponse_nextToken :: Lens.Lens' ListPipelineExecutionStepsResponse (Prelude.Maybe Prelude.Text)
listPipelineExecutionStepsResponse_nextToken = Lens.lens (\ListPipelineExecutionStepsResponse' {nextToken} -> nextToken) (\s@ListPipelineExecutionStepsResponse' {} a -> s {nextToken = a} :: ListPipelineExecutionStepsResponse)

-- | The response's http status code.
listPipelineExecutionStepsResponse_httpStatus :: Lens.Lens' ListPipelineExecutionStepsResponse Prelude.Int
listPipelineExecutionStepsResponse_httpStatus = Lens.lens (\ListPipelineExecutionStepsResponse' {httpStatus} -> httpStatus) (\s@ListPipelineExecutionStepsResponse' {} a -> s {httpStatus = a} :: ListPipelineExecutionStepsResponse)

instance
  Prelude.NFData
    ListPipelineExecutionStepsResponse
