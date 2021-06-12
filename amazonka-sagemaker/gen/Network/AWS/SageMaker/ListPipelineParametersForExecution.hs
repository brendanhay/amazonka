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
-- Module      : Network.AWS.SageMaker.ListPipelineParametersForExecution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of parameters for a pipeline execution.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListPipelineParametersForExecution
  ( -- * Creating a Request
    ListPipelineParametersForExecution (..),
    newListPipelineParametersForExecution,

    -- * Request Lenses
    listPipelineParametersForExecution_nextToken,
    listPipelineParametersForExecution_maxResults,
    listPipelineParametersForExecution_pipelineExecutionArn,

    -- * Destructuring the Response
    ListPipelineParametersForExecutionResponse (..),
    newListPipelineParametersForExecutionResponse,

    -- * Response Lenses
    listPipelineParametersForExecutionResponse_nextToken,
    listPipelineParametersForExecutionResponse_pipelineParameters,
    listPipelineParametersForExecutionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListPipelineParametersForExecution' smart constructor.
data ListPipelineParametersForExecution = ListPipelineParametersForExecution'
  { -- | If the result of the previous @ListPipelineParametersForExecution@
    -- request was truncated, the response includes a @NextToken@. To retrieve
    -- the next set of parameters, use the token in the next request.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of parameters to return in the response.
    maxResults :: Core.Maybe Core.Natural,
    -- | The Amazon Resource Name (ARN) of the pipeline execution.
    pipelineExecutionArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListPipelineParametersForExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPipelineParametersForExecution_nextToken' - If the result of the previous @ListPipelineParametersForExecution@
-- request was truncated, the response includes a @NextToken@. To retrieve
-- the next set of parameters, use the token in the next request.
--
-- 'maxResults', 'listPipelineParametersForExecution_maxResults' - The maximum number of parameters to return in the response.
--
-- 'pipelineExecutionArn', 'listPipelineParametersForExecution_pipelineExecutionArn' - The Amazon Resource Name (ARN) of the pipeline execution.
newListPipelineParametersForExecution ::
  -- | 'pipelineExecutionArn'
  Core.Text ->
  ListPipelineParametersForExecution
newListPipelineParametersForExecution
  pPipelineExecutionArn_ =
    ListPipelineParametersForExecution'
      { nextToken =
          Core.Nothing,
        maxResults = Core.Nothing,
        pipelineExecutionArn =
          pPipelineExecutionArn_
      }

-- | If the result of the previous @ListPipelineParametersForExecution@
-- request was truncated, the response includes a @NextToken@. To retrieve
-- the next set of parameters, use the token in the next request.
listPipelineParametersForExecution_nextToken :: Lens.Lens' ListPipelineParametersForExecution (Core.Maybe Core.Text)
listPipelineParametersForExecution_nextToken = Lens.lens (\ListPipelineParametersForExecution' {nextToken} -> nextToken) (\s@ListPipelineParametersForExecution' {} a -> s {nextToken = a} :: ListPipelineParametersForExecution)

-- | The maximum number of parameters to return in the response.
listPipelineParametersForExecution_maxResults :: Lens.Lens' ListPipelineParametersForExecution (Core.Maybe Core.Natural)
listPipelineParametersForExecution_maxResults = Lens.lens (\ListPipelineParametersForExecution' {maxResults} -> maxResults) (\s@ListPipelineParametersForExecution' {} a -> s {maxResults = a} :: ListPipelineParametersForExecution)

-- | The Amazon Resource Name (ARN) of the pipeline execution.
listPipelineParametersForExecution_pipelineExecutionArn :: Lens.Lens' ListPipelineParametersForExecution Core.Text
listPipelineParametersForExecution_pipelineExecutionArn = Lens.lens (\ListPipelineParametersForExecution' {pipelineExecutionArn} -> pipelineExecutionArn) (\s@ListPipelineParametersForExecution' {} a -> s {pipelineExecutionArn = a} :: ListPipelineParametersForExecution)

instance
  Core.AWSPager
    ListPipelineParametersForExecution
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPipelineParametersForExecutionResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listPipelineParametersForExecutionResponse_pipelineParameters
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listPipelineParametersForExecution_nextToken
          Lens..~ rs
          Lens.^? listPipelineParametersForExecutionResponse_nextToken
            Core.. Lens._Just

instance
  Core.AWSRequest
    ListPipelineParametersForExecution
  where
  type
    AWSResponse ListPipelineParametersForExecution =
      ListPipelineParametersForExecutionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPipelineParametersForExecutionResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "PipelineParameters"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    ListPipelineParametersForExecution

instance
  Core.NFData
    ListPipelineParametersForExecution

instance
  Core.ToHeaders
    ListPipelineParametersForExecution
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.ListPipelineParametersForExecution" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    ListPipelineParametersForExecution
  where
  toJSON ListPipelineParametersForExecution' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            Core.Just
              ( "PipelineExecutionArn"
                  Core..= pipelineExecutionArn
              )
          ]
      )

instance
  Core.ToPath
    ListPipelineParametersForExecution
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    ListPipelineParametersForExecution
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListPipelineParametersForExecutionResponse' smart constructor.
data ListPipelineParametersForExecutionResponse = ListPipelineParametersForExecutionResponse'
  { -- | If the result of the previous @ListPipelineParametersForExecution@
    -- request was truncated, the response includes a @NextToken@. To retrieve
    -- the next set of parameters, use the token in the next request.
    nextToken :: Core.Maybe Core.Text,
    -- | Contains a list of pipeline parameters. This list can be empty.
    pipelineParameters :: Core.Maybe [Parameter],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListPipelineParametersForExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPipelineParametersForExecutionResponse_nextToken' - If the result of the previous @ListPipelineParametersForExecution@
-- request was truncated, the response includes a @NextToken@. To retrieve
-- the next set of parameters, use the token in the next request.
--
-- 'pipelineParameters', 'listPipelineParametersForExecutionResponse_pipelineParameters' - Contains a list of pipeline parameters. This list can be empty.
--
-- 'httpStatus', 'listPipelineParametersForExecutionResponse_httpStatus' - The response's http status code.
newListPipelineParametersForExecutionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListPipelineParametersForExecutionResponse
newListPipelineParametersForExecutionResponse
  pHttpStatus_ =
    ListPipelineParametersForExecutionResponse'
      { nextToken =
          Core.Nothing,
        pipelineParameters =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | If the result of the previous @ListPipelineParametersForExecution@
-- request was truncated, the response includes a @NextToken@. To retrieve
-- the next set of parameters, use the token in the next request.
listPipelineParametersForExecutionResponse_nextToken :: Lens.Lens' ListPipelineParametersForExecutionResponse (Core.Maybe Core.Text)
listPipelineParametersForExecutionResponse_nextToken = Lens.lens (\ListPipelineParametersForExecutionResponse' {nextToken} -> nextToken) (\s@ListPipelineParametersForExecutionResponse' {} a -> s {nextToken = a} :: ListPipelineParametersForExecutionResponse)

-- | Contains a list of pipeline parameters. This list can be empty.
listPipelineParametersForExecutionResponse_pipelineParameters :: Lens.Lens' ListPipelineParametersForExecutionResponse (Core.Maybe [Parameter])
listPipelineParametersForExecutionResponse_pipelineParameters = Lens.lens (\ListPipelineParametersForExecutionResponse' {pipelineParameters} -> pipelineParameters) (\s@ListPipelineParametersForExecutionResponse' {} a -> s {pipelineParameters = a} :: ListPipelineParametersForExecutionResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listPipelineParametersForExecutionResponse_httpStatus :: Lens.Lens' ListPipelineParametersForExecutionResponse Core.Int
listPipelineParametersForExecutionResponse_httpStatus = Lens.lens (\ListPipelineParametersForExecutionResponse' {httpStatus} -> httpStatus) (\s@ListPipelineParametersForExecutionResponse' {} a -> s {httpStatus = a} :: ListPipelineParametersForExecutionResponse)

instance
  Core.NFData
    ListPipelineParametersForExecutionResponse
