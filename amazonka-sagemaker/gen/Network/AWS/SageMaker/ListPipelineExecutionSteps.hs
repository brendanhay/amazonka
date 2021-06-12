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
-- Module      : Network.AWS.SageMaker.ListPipelineExecutionSteps
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of @PipeLineExecutionStep@ objects.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListPipelineExecutionSteps
  ( -- * Creating a Request
    ListPipelineExecutionSteps (..),
    newListPipelineExecutionSteps,

    -- * Request Lenses
    listPipelineExecutionSteps_sortOrder,
    listPipelineExecutionSteps_nextToken,
    listPipelineExecutionSteps_maxResults,
    listPipelineExecutionSteps_pipelineExecutionArn,

    -- * Destructuring the Response
    ListPipelineExecutionStepsResponse (..),
    newListPipelineExecutionStepsResponse,

    -- * Response Lenses
    listPipelineExecutionStepsResponse_nextToken,
    listPipelineExecutionStepsResponse_pipelineExecutionSteps,
    listPipelineExecutionStepsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListPipelineExecutionSteps' smart constructor.
data ListPipelineExecutionSteps = ListPipelineExecutionSteps'
  { -- | The field by which to sort results. The default is @CreatedTime@.
    sortOrder :: Core.Maybe SortOrder,
    -- | If the result of the previous @ListPipelineExecutionSteps@ request was
    -- truncated, the response includes a @NextToken@. To retrieve the next set
    -- of pipeline execution steps, use the token in the next request.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of pipeline execution steps to return in the
    -- response.
    maxResults :: Core.Maybe Core.Natural,
    -- | The Amazon Resource Name (ARN) of the pipeline execution.
    pipelineExecutionArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListPipelineExecutionSteps' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortOrder', 'listPipelineExecutionSteps_sortOrder' - The field by which to sort results. The default is @CreatedTime@.
--
-- 'nextToken', 'listPipelineExecutionSteps_nextToken' - If the result of the previous @ListPipelineExecutionSteps@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of pipeline execution steps, use the token in the next request.
--
-- 'maxResults', 'listPipelineExecutionSteps_maxResults' - The maximum number of pipeline execution steps to return in the
-- response.
--
-- 'pipelineExecutionArn', 'listPipelineExecutionSteps_pipelineExecutionArn' - The Amazon Resource Name (ARN) of the pipeline execution.
newListPipelineExecutionSteps ::
  ListPipelineExecutionSteps
newListPipelineExecutionSteps =
  ListPipelineExecutionSteps'
    { sortOrder =
        Core.Nothing,
      nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      pipelineExecutionArn = Core.Nothing
    }

-- | The field by which to sort results. The default is @CreatedTime@.
listPipelineExecutionSteps_sortOrder :: Lens.Lens' ListPipelineExecutionSteps (Core.Maybe SortOrder)
listPipelineExecutionSteps_sortOrder = Lens.lens (\ListPipelineExecutionSteps' {sortOrder} -> sortOrder) (\s@ListPipelineExecutionSteps' {} a -> s {sortOrder = a} :: ListPipelineExecutionSteps)

-- | If the result of the previous @ListPipelineExecutionSteps@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of pipeline execution steps, use the token in the next request.
listPipelineExecutionSteps_nextToken :: Lens.Lens' ListPipelineExecutionSteps (Core.Maybe Core.Text)
listPipelineExecutionSteps_nextToken = Lens.lens (\ListPipelineExecutionSteps' {nextToken} -> nextToken) (\s@ListPipelineExecutionSteps' {} a -> s {nextToken = a} :: ListPipelineExecutionSteps)

-- | The maximum number of pipeline execution steps to return in the
-- response.
listPipelineExecutionSteps_maxResults :: Lens.Lens' ListPipelineExecutionSteps (Core.Maybe Core.Natural)
listPipelineExecutionSteps_maxResults = Lens.lens (\ListPipelineExecutionSteps' {maxResults} -> maxResults) (\s@ListPipelineExecutionSteps' {} a -> s {maxResults = a} :: ListPipelineExecutionSteps)

-- | The Amazon Resource Name (ARN) of the pipeline execution.
listPipelineExecutionSteps_pipelineExecutionArn :: Lens.Lens' ListPipelineExecutionSteps (Core.Maybe Core.Text)
listPipelineExecutionSteps_pipelineExecutionArn = Lens.lens (\ListPipelineExecutionSteps' {pipelineExecutionArn} -> pipelineExecutionArn) (\s@ListPipelineExecutionSteps' {} a -> s {pipelineExecutionArn = a} :: ListPipelineExecutionSteps)

instance Core.AWSPager ListPipelineExecutionSteps where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPipelineExecutionStepsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listPipelineExecutionStepsResponse_pipelineExecutionSteps
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listPipelineExecutionSteps_nextToken
          Lens..~ rs
          Lens.^? listPipelineExecutionStepsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListPipelineExecutionSteps where
  type
    AWSResponse ListPipelineExecutionSteps =
      ListPipelineExecutionStepsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPipelineExecutionStepsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "PipelineExecutionSteps"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListPipelineExecutionSteps

instance Core.NFData ListPipelineExecutionSteps

instance Core.ToHeaders ListPipelineExecutionSteps where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.ListPipelineExecutionSteps" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListPipelineExecutionSteps where
  toJSON ListPipelineExecutionSteps' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SortOrder" Core..=) Core.<$> sortOrder,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("PipelineExecutionArn" Core..=)
              Core.<$> pipelineExecutionArn
          ]
      )

instance Core.ToPath ListPipelineExecutionSteps where
  toPath = Core.const "/"

instance Core.ToQuery ListPipelineExecutionSteps where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListPipelineExecutionStepsResponse' smart constructor.
data ListPipelineExecutionStepsResponse = ListPipelineExecutionStepsResponse'
  { -- | If the result of the previous @ListPipelineExecutionSteps@ request was
    -- truncated, the response includes a @NextToken@. To retrieve the next set
    -- of pipeline execution steps, use the token in the next request.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of @PipeLineExecutionStep@ objects. Each @PipeLineExecutionStep@
    -- consists of StepName, StartTime, EndTime, StepStatus, and Metadata.
    -- Metadata is an object with properties for each job that contains
    -- relevant information about the job created by the step.
    pipelineExecutionSteps :: Core.Maybe [PipelineExecutionStep],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListPipelineExecutionStepsResponse
newListPipelineExecutionStepsResponse pHttpStatus_ =
  ListPipelineExecutionStepsResponse'
    { nextToken =
        Core.Nothing,
      pipelineExecutionSteps = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the result of the previous @ListPipelineExecutionSteps@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of pipeline execution steps, use the token in the next request.
listPipelineExecutionStepsResponse_nextToken :: Lens.Lens' ListPipelineExecutionStepsResponse (Core.Maybe Core.Text)
listPipelineExecutionStepsResponse_nextToken = Lens.lens (\ListPipelineExecutionStepsResponse' {nextToken} -> nextToken) (\s@ListPipelineExecutionStepsResponse' {} a -> s {nextToken = a} :: ListPipelineExecutionStepsResponse)

-- | A list of @PipeLineExecutionStep@ objects. Each @PipeLineExecutionStep@
-- consists of StepName, StartTime, EndTime, StepStatus, and Metadata.
-- Metadata is an object with properties for each job that contains
-- relevant information about the job created by the step.
listPipelineExecutionStepsResponse_pipelineExecutionSteps :: Lens.Lens' ListPipelineExecutionStepsResponse (Core.Maybe [PipelineExecutionStep])
listPipelineExecutionStepsResponse_pipelineExecutionSteps = Lens.lens (\ListPipelineExecutionStepsResponse' {pipelineExecutionSteps} -> pipelineExecutionSteps) (\s@ListPipelineExecutionStepsResponse' {} a -> s {pipelineExecutionSteps = a} :: ListPipelineExecutionStepsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listPipelineExecutionStepsResponse_httpStatus :: Lens.Lens' ListPipelineExecutionStepsResponse Core.Int
listPipelineExecutionStepsResponse_httpStatus = Lens.lens (\ListPipelineExecutionStepsResponse' {httpStatus} -> httpStatus) (\s@ListPipelineExecutionStepsResponse' {} a -> s {httpStatus = a} :: ListPipelineExecutionStepsResponse)

instance
  Core.NFData
    ListPipelineExecutionStepsResponse
