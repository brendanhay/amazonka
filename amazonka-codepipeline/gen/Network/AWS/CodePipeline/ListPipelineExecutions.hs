{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CodePipeline.ListPipelineExecutions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a summary of the most recent executions for a pipeline.
--
-- This operation returns paginated results.
module Network.AWS.CodePipeline.ListPipelineExecutions
  ( -- * Creating a Request
    ListPipelineExecutions (..),
    newListPipelineExecutions,

    -- * Request Lenses
    listPipelineExecutions_nextToken,
    listPipelineExecutions_maxResults,
    listPipelineExecutions_pipelineName,

    -- * Destructuring the Response
    ListPipelineExecutionsResponse (..),
    newListPipelineExecutionsResponse,

    -- * Response Lenses
    listPipelineExecutionsResponse_nextToken,
    listPipelineExecutionsResponse_pipelineExecutionSummaries,
    listPipelineExecutionsResponse_httpStatus,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @ListPipelineExecutions@ action.
--
-- /See:/ 'newListPipelineExecutions' smart constructor.
data ListPipelineExecutions = ListPipelineExecutions'
  { -- | The token that was returned from the previous @ListPipelineExecutions@
    -- call, which can be used to return the next set of pipeline executions in
    -- the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in a single call. To retrieve
    -- the remaining results, make another call with the returned nextToken
    -- value. Pipeline history is limited to the most recent 12 months, based
    -- on pipeline execution start times. Default value is 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name of the pipeline for which you want to get execution summary
    -- information.
    pipelineName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListPipelineExecutions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPipelineExecutions_nextToken' - The token that was returned from the previous @ListPipelineExecutions@
-- call, which can be used to return the next set of pipeline executions in
-- the list.
--
-- 'maxResults', 'listPipelineExecutions_maxResults' - The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another call with the returned nextToken
-- value. Pipeline history is limited to the most recent 12 months, based
-- on pipeline execution start times. Default value is 100.
--
-- 'pipelineName', 'listPipelineExecutions_pipelineName' - The name of the pipeline for which you want to get execution summary
-- information.
newListPipelineExecutions ::
  -- | 'pipelineName'
  Prelude.Text ->
  ListPipelineExecutions
newListPipelineExecutions pPipelineName_ =
  ListPipelineExecutions'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      pipelineName = pPipelineName_
    }

-- | The token that was returned from the previous @ListPipelineExecutions@
-- call, which can be used to return the next set of pipeline executions in
-- the list.
listPipelineExecutions_nextToken :: Lens.Lens' ListPipelineExecutions (Prelude.Maybe Prelude.Text)
listPipelineExecutions_nextToken = Lens.lens (\ListPipelineExecutions' {nextToken} -> nextToken) (\s@ListPipelineExecutions' {} a -> s {nextToken = a} :: ListPipelineExecutions)

-- | The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another call with the returned nextToken
-- value. Pipeline history is limited to the most recent 12 months, based
-- on pipeline execution start times. Default value is 100.
listPipelineExecutions_maxResults :: Lens.Lens' ListPipelineExecutions (Prelude.Maybe Prelude.Natural)
listPipelineExecutions_maxResults = Lens.lens (\ListPipelineExecutions' {maxResults} -> maxResults) (\s@ListPipelineExecutions' {} a -> s {maxResults = a} :: ListPipelineExecutions)

-- | The name of the pipeline for which you want to get execution summary
-- information.
listPipelineExecutions_pipelineName :: Lens.Lens' ListPipelineExecutions Prelude.Text
listPipelineExecutions_pipelineName = Lens.lens (\ListPipelineExecutions' {pipelineName} -> pipelineName) (\s@ListPipelineExecutions' {} a -> s {pipelineName = a} :: ListPipelineExecutions)

instance Pager.AWSPager ListPipelineExecutions where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listPipelineExecutionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listPipelineExecutionsResponse_pipelineExecutionSummaries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listPipelineExecutions_nextToken
          Lens..~ rs
          Lens.^? listPipelineExecutionsResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest ListPipelineExecutions where
  type
    Rs ListPipelineExecutions =
      ListPipelineExecutionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPipelineExecutionsResponse'
            Prelude.<$> (x Prelude..?> "nextToken")
            Prelude.<*> ( x Prelude..?> "pipelineExecutionSummaries"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPipelineExecutions

instance Prelude.NFData ListPipelineExecutions

instance Prelude.ToHeaders ListPipelineExecutions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodePipeline_20150709.ListPipelineExecutions" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListPipelineExecutions where
  toJSON ListPipelineExecutions' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("nextToken" Prelude..=) Prelude.<$> nextToken,
            ("maxResults" Prelude..=) Prelude.<$> maxResults,
            Prelude.Just
              ("pipelineName" Prelude..= pipelineName)
          ]
      )

instance Prelude.ToPath ListPipelineExecutions where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListPipelineExecutions where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @ListPipelineExecutions@ action.
--
-- /See:/ 'newListPipelineExecutionsResponse' smart constructor.
data ListPipelineExecutionsResponse = ListPipelineExecutionsResponse'
  { -- | A token that can be used in the next @ListPipelineExecutions@ call. To
    -- view all items in the list, continue to call this operation with each
    -- subsequent token until no more nextToken values are returned.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of executions in the history of a pipeline.
    pipelineExecutionSummaries :: Prelude.Maybe [PipelineExecutionSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListPipelineExecutionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPipelineExecutionsResponse_nextToken' - A token that can be used in the next @ListPipelineExecutions@ call. To
-- view all items in the list, continue to call this operation with each
-- subsequent token until no more nextToken values are returned.
--
-- 'pipelineExecutionSummaries', 'listPipelineExecutionsResponse_pipelineExecutionSummaries' - A list of executions in the history of a pipeline.
--
-- 'httpStatus', 'listPipelineExecutionsResponse_httpStatus' - The response's http status code.
newListPipelineExecutionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPipelineExecutionsResponse
newListPipelineExecutionsResponse pHttpStatus_ =
  ListPipelineExecutionsResponse'
    { nextToken =
        Prelude.Nothing,
      pipelineExecutionSummaries =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that can be used in the next @ListPipelineExecutions@ call. To
-- view all items in the list, continue to call this operation with each
-- subsequent token until no more nextToken values are returned.
listPipelineExecutionsResponse_nextToken :: Lens.Lens' ListPipelineExecutionsResponse (Prelude.Maybe Prelude.Text)
listPipelineExecutionsResponse_nextToken = Lens.lens (\ListPipelineExecutionsResponse' {nextToken} -> nextToken) (\s@ListPipelineExecutionsResponse' {} a -> s {nextToken = a} :: ListPipelineExecutionsResponse)

-- | A list of executions in the history of a pipeline.
listPipelineExecutionsResponse_pipelineExecutionSummaries :: Lens.Lens' ListPipelineExecutionsResponse (Prelude.Maybe [PipelineExecutionSummary])
listPipelineExecutionsResponse_pipelineExecutionSummaries = Lens.lens (\ListPipelineExecutionsResponse' {pipelineExecutionSummaries} -> pipelineExecutionSummaries) (\s@ListPipelineExecutionsResponse' {} a -> s {pipelineExecutionSummaries = a} :: ListPipelineExecutionsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listPipelineExecutionsResponse_httpStatus :: Lens.Lens' ListPipelineExecutionsResponse Prelude.Int
listPipelineExecutionsResponse_httpStatus = Lens.lens (\ListPipelineExecutionsResponse' {httpStatus} -> httpStatus) (\s@ListPipelineExecutionsResponse' {} a -> s {httpStatus = a} :: ListPipelineExecutionsResponse)

instance
  Prelude.NFData
    ListPipelineExecutionsResponse
