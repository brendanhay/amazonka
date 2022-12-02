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
-- Module      : Amazonka.SageMaker.ListPipelineParametersForExecution
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of parameters for a pipeline execution.
--
-- This operation returns paginated results.
module Amazonka.SageMaker.ListPipelineParametersForExecution
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListPipelineParametersForExecution' smart constructor.
data ListPipelineParametersForExecution = ListPipelineParametersForExecution'
  { -- | If the result of the previous @ListPipelineParametersForExecution@
    -- request was truncated, the response includes a @NextToken@. To retrieve
    -- the next set of parameters, use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of parameters to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of the pipeline execution.
    pipelineExecutionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  ListPipelineParametersForExecution
newListPipelineParametersForExecution
  pPipelineExecutionArn_ =
    ListPipelineParametersForExecution'
      { nextToken =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        pipelineExecutionArn =
          pPipelineExecutionArn_
      }

-- | If the result of the previous @ListPipelineParametersForExecution@
-- request was truncated, the response includes a @NextToken@. To retrieve
-- the next set of parameters, use the token in the next request.
listPipelineParametersForExecution_nextToken :: Lens.Lens' ListPipelineParametersForExecution (Prelude.Maybe Prelude.Text)
listPipelineParametersForExecution_nextToken = Lens.lens (\ListPipelineParametersForExecution' {nextToken} -> nextToken) (\s@ListPipelineParametersForExecution' {} a -> s {nextToken = a} :: ListPipelineParametersForExecution)

-- | The maximum number of parameters to return in the response.
listPipelineParametersForExecution_maxResults :: Lens.Lens' ListPipelineParametersForExecution (Prelude.Maybe Prelude.Natural)
listPipelineParametersForExecution_maxResults = Lens.lens (\ListPipelineParametersForExecution' {maxResults} -> maxResults) (\s@ListPipelineParametersForExecution' {} a -> s {maxResults = a} :: ListPipelineParametersForExecution)

-- | The Amazon Resource Name (ARN) of the pipeline execution.
listPipelineParametersForExecution_pipelineExecutionArn :: Lens.Lens' ListPipelineParametersForExecution Prelude.Text
listPipelineParametersForExecution_pipelineExecutionArn = Lens.lens (\ListPipelineParametersForExecution' {pipelineExecutionArn} -> pipelineExecutionArn) (\s@ListPipelineParametersForExecution' {} a -> s {pipelineExecutionArn = a} :: ListPipelineParametersForExecution)

instance
  Core.AWSPager
    ListPipelineParametersForExecution
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPipelineParametersForExecutionResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listPipelineParametersForExecutionResponse_pipelineParameters
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listPipelineParametersForExecution_nextToken
          Lens..~ rs
          Lens.^? listPipelineParametersForExecutionResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListPipelineParametersForExecution
  where
  type
    AWSResponse ListPipelineParametersForExecution =
      ListPipelineParametersForExecutionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPipelineParametersForExecutionResponse'
            Prelude.<$> (x Data..?> "NextToken")
              Prelude.<*> ( x Data..?> "PipelineParameters"
                              Core..!@ Prelude.mempty
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListPipelineParametersForExecution
  where
  hashWithSalt
    _salt
    ListPipelineParametersForExecution' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` pipelineExecutionArn

instance
  Prelude.NFData
    ListPipelineParametersForExecution
  where
  rnf ListPipelineParametersForExecution' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf pipelineExecutionArn

instance
  Data.ToHeaders
    ListPipelineParametersForExecution
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.ListPipelineParametersForExecution" ::
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
    ListPipelineParametersForExecution
  where
  toJSON ListPipelineParametersForExecution' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            Prelude.Just
              ( "PipelineExecutionArn"
                  Data..= pipelineExecutionArn
              )
          ]
      )

instance
  Data.ToPath
    ListPipelineParametersForExecution
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ListPipelineParametersForExecution
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListPipelineParametersForExecutionResponse' smart constructor.
data ListPipelineParametersForExecutionResponse = ListPipelineParametersForExecutionResponse'
  { -- | If the result of the previous @ListPipelineParametersForExecution@
    -- request was truncated, the response includes a @NextToken@. To retrieve
    -- the next set of parameters, use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Contains a list of pipeline parameters. This list can be empty.
    pipelineParameters :: Prelude.Maybe [Parameter],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListPipelineParametersForExecutionResponse
newListPipelineParametersForExecutionResponse
  pHttpStatus_ =
    ListPipelineParametersForExecutionResponse'
      { nextToken =
          Prelude.Nothing,
        pipelineParameters =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | If the result of the previous @ListPipelineParametersForExecution@
-- request was truncated, the response includes a @NextToken@. To retrieve
-- the next set of parameters, use the token in the next request.
listPipelineParametersForExecutionResponse_nextToken :: Lens.Lens' ListPipelineParametersForExecutionResponse (Prelude.Maybe Prelude.Text)
listPipelineParametersForExecutionResponse_nextToken = Lens.lens (\ListPipelineParametersForExecutionResponse' {nextToken} -> nextToken) (\s@ListPipelineParametersForExecutionResponse' {} a -> s {nextToken = a} :: ListPipelineParametersForExecutionResponse)

-- | Contains a list of pipeline parameters. This list can be empty.
listPipelineParametersForExecutionResponse_pipelineParameters :: Lens.Lens' ListPipelineParametersForExecutionResponse (Prelude.Maybe [Parameter])
listPipelineParametersForExecutionResponse_pipelineParameters = Lens.lens (\ListPipelineParametersForExecutionResponse' {pipelineParameters} -> pipelineParameters) (\s@ListPipelineParametersForExecutionResponse' {} a -> s {pipelineParameters = a} :: ListPipelineParametersForExecutionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listPipelineParametersForExecutionResponse_httpStatus :: Lens.Lens' ListPipelineParametersForExecutionResponse Prelude.Int
listPipelineParametersForExecutionResponse_httpStatus = Lens.lens (\ListPipelineParametersForExecutionResponse' {httpStatus} -> httpStatus) (\s@ListPipelineParametersForExecutionResponse' {} a -> s {httpStatus = a} :: ListPipelineParametersForExecutionResponse)

instance
  Prelude.NFData
    ListPipelineParametersForExecutionResponse
  where
  rnf ListPipelineParametersForExecutionResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf pipelineParameters
      `Prelude.seq` Prelude.rnf httpStatus
