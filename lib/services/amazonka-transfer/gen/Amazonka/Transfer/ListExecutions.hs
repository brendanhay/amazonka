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
-- Module      : Amazonka.Transfer.ListExecutions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all executions for the specified workflow.
--
-- This operation returns paginated results.
module Amazonka.Transfer.ListExecutions
  ( -- * Creating a Request
    ListExecutions (..),
    newListExecutions,

    -- * Request Lenses
    listExecutions_maxResults,
    listExecutions_nextToken,
    listExecutions_workflowId,

    -- * Destructuring the Response
    ListExecutionsResponse (..),
    newListExecutionsResponse,

    -- * Response Lenses
    listExecutionsResponse_nextToken,
    listExecutionsResponse_httpStatus,
    listExecutionsResponse_workflowId,
    listExecutionsResponse_executions,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transfer.Types

-- | /See:/ 'newListExecutions' smart constructor.
data ListExecutions = ListExecutions'
  { -- | Specifies the maximum number of executions to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | @ListExecutions@ returns the @NextToken@ parameter in the output. You
    -- can then pass the @NextToken@ parameter in a subsequent command to
    -- continue listing additional executions.
    --
    -- This is useful for pagination, for instance. If you have 100 executions
    -- for a workflow, you might only want to list first 10. If so, call the
    -- API by specifying the @max-results@:
    --
    -- @aws transfer list-executions --max-results 10@
    --
    -- This returns details for the first 10 executions, as well as the pointer
    -- (@NextToken@) to the eleventh execution. You can now call the API again,
    -- supplying the @NextToken@ value you received:
    --
    -- @aws transfer list-executions --max-results 10 --next-token $somePointerReturnedFromPreviousListResult@
    --
    -- This call returns the next 10 executions, the 11th through the 20th. You
    -- can then repeat the call until the details for all 100 executions have
    -- been returned.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the workflow.
    workflowId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListExecutions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listExecutions_maxResults' - Specifies the maximum number of executions to return.
--
-- 'nextToken', 'listExecutions_nextToken' - @ListExecutions@ returns the @NextToken@ parameter in the output. You
-- can then pass the @NextToken@ parameter in a subsequent command to
-- continue listing additional executions.
--
-- This is useful for pagination, for instance. If you have 100 executions
-- for a workflow, you might only want to list first 10. If so, call the
-- API by specifying the @max-results@:
--
-- @aws transfer list-executions --max-results 10@
--
-- This returns details for the first 10 executions, as well as the pointer
-- (@NextToken@) to the eleventh execution. You can now call the API again,
-- supplying the @NextToken@ value you received:
--
-- @aws transfer list-executions --max-results 10 --next-token $somePointerReturnedFromPreviousListResult@
--
-- This call returns the next 10 executions, the 11th through the 20th. You
-- can then repeat the call until the details for all 100 executions have
-- been returned.
--
-- 'workflowId', 'listExecutions_workflowId' - A unique identifier for the workflow.
newListExecutions ::
  -- | 'workflowId'
  Prelude.Text ->
  ListExecutions
newListExecutions pWorkflowId_ =
  ListExecutions'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      workflowId = pWorkflowId_
    }

-- | Specifies the maximum number of executions to return.
listExecutions_maxResults :: Lens.Lens' ListExecutions (Prelude.Maybe Prelude.Natural)
listExecutions_maxResults = Lens.lens (\ListExecutions' {maxResults} -> maxResults) (\s@ListExecutions' {} a -> s {maxResults = a} :: ListExecutions)

-- | @ListExecutions@ returns the @NextToken@ parameter in the output. You
-- can then pass the @NextToken@ parameter in a subsequent command to
-- continue listing additional executions.
--
-- This is useful for pagination, for instance. If you have 100 executions
-- for a workflow, you might only want to list first 10. If so, call the
-- API by specifying the @max-results@:
--
-- @aws transfer list-executions --max-results 10@
--
-- This returns details for the first 10 executions, as well as the pointer
-- (@NextToken@) to the eleventh execution. You can now call the API again,
-- supplying the @NextToken@ value you received:
--
-- @aws transfer list-executions --max-results 10 --next-token $somePointerReturnedFromPreviousListResult@
--
-- This call returns the next 10 executions, the 11th through the 20th. You
-- can then repeat the call until the details for all 100 executions have
-- been returned.
listExecutions_nextToken :: Lens.Lens' ListExecutions (Prelude.Maybe Prelude.Text)
listExecutions_nextToken = Lens.lens (\ListExecutions' {nextToken} -> nextToken) (\s@ListExecutions' {} a -> s {nextToken = a} :: ListExecutions)

-- | A unique identifier for the workflow.
listExecutions_workflowId :: Lens.Lens' ListExecutions Prelude.Text
listExecutions_workflowId = Lens.lens (\ListExecutions' {workflowId} -> workflowId) (\s@ListExecutions' {} a -> s {workflowId = a} :: ListExecutions)

instance Core.AWSPager ListExecutions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listExecutionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        (rs Lens.^. listExecutionsResponse_executions) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listExecutions_nextToken
          Lens..~ rs
          Lens.^? listExecutionsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListExecutions where
  type
    AWSResponse ListExecutions =
      ListExecutionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListExecutionsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "WorkflowId")
            Prelude.<*> (x Data..?> "Executions" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListExecutions where
  hashWithSalt _salt ListExecutions' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` workflowId

instance Prelude.NFData ListExecutions where
  rnf ListExecutions' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf workflowId

instance Data.ToHeaders ListExecutions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "TransferService.ListExecutions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListExecutions where
  toJSON ListExecutions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("WorkflowId" Data..= workflowId)
          ]
      )

instance Data.ToPath ListExecutions where
  toPath = Prelude.const "/"

instance Data.ToQuery ListExecutions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListExecutionsResponse' smart constructor.
data ListExecutionsResponse = ListExecutionsResponse'
  { -- | @ListExecutions@ returns the @NextToken@ parameter in the output. You
    -- can then pass the @NextToken@ parameter in a subsequent command to
    -- continue listing additional executions.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A unique identifier for the workflow.
    workflowId :: Prelude.Text,
    -- | Returns the details for each execution.
    --
    -- -   __NextToken__: returned from a call to several APIs, you can use
    --     pass it to a subsequent command to continue listing additional
    --     executions.
    --
    -- -   __StartTime__: timestamp indicating when the execution began.
    --
    -- -   __Executions__: details of the execution, including the execution
    --     ID, initial file location, and Service metadata.
    --
    -- -   __Status__: one of the following values: @IN_PROGRESS@, @COMPLETED@,
    --     @EXCEPTION@, @HANDLING_EXEPTION@.
    executions :: [ListedExecution]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListExecutionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listExecutionsResponse_nextToken' - @ListExecutions@ returns the @NextToken@ parameter in the output. You
-- can then pass the @NextToken@ parameter in a subsequent command to
-- continue listing additional executions.
--
-- 'httpStatus', 'listExecutionsResponse_httpStatus' - The response's http status code.
--
-- 'workflowId', 'listExecutionsResponse_workflowId' - A unique identifier for the workflow.
--
-- 'executions', 'listExecutionsResponse_executions' - Returns the details for each execution.
--
-- -   __NextToken__: returned from a call to several APIs, you can use
--     pass it to a subsequent command to continue listing additional
--     executions.
--
-- -   __StartTime__: timestamp indicating when the execution began.
--
-- -   __Executions__: details of the execution, including the execution
--     ID, initial file location, and Service metadata.
--
-- -   __Status__: one of the following values: @IN_PROGRESS@, @COMPLETED@,
--     @EXCEPTION@, @HANDLING_EXEPTION@.
newListExecutionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'workflowId'
  Prelude.Text ->
  ListExecutionsResponse
newListExecutionsResponse pHttpStatus_ pWorkflowId_ =
  ListExecutionsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      workflowId = pWorkflowId_,
      executions = Prelude.mempty
    }

-- | @ListExecutions@ returns the @NextToken@ parameter in the output. You
-- can then pass the @NextToken@ parameter in a subsequent command to
-- continue listing additional executions.
listExecutionsResponse_nextToken :: Lens.Lens' ListExecutionsResponse (Prelude.Maybe Prelude.Text)
listExecutionsResponse_nextToken = Lens.lens (\ListExecutionsResponse' {nextToken} -> nextToken) (\s@ListExecutionsResponse' {} a -> s {nextToken = a} :: ListExecutionsResponse)

-- | The response's http status code.
listExecutionsResponse_httpStatus :: Lens.Lens' ListExecutionsResponse Prelude.Int
listExecutionsResponse_httpStatus = Lens.lens (\ListExecutionsResponse' {httpStatus} -> httpStatus) (\s@ListExecutionsResponse' {} a -> s {httpStatus = a} :: ListExecutionsResponse)

-- | A unique identifier for the workflow.
listExecutionsResponse_workflowId :: Lens.Lens' ListExecutionsResponse Prelude.Text
listExecutionsResponse_workflowId = Lens.lens (\ListExecutionsResponse' {workflowId} -> workflowId) (\s@ListExecutionsResponse' {} a -> s {workflowId = a} :: ListExecutionsResponse)

-- | Returns the details for each execution.
--
-- -   __NextToken__: returned from a call to several APIs, you can use
--     pass it to a subsequent command to continue listing additional
--     executions.
--
-- -   __StartTime__: timestamp indicating when the execution began.
--
-- -   __Executions__: details of the execution, including the execution
--     ID, initial file location, and Service metadata.
--
-- -   __Status__: one of the following values: @IN_PROGRESS@, @COMPLETED@,
--     @EXCEPTION@, @HANDLING_EXEPTION@.
listExecutionsResponse_executions :: Lens.Lens' ListExecutionsResponse [ListedExecution]
listExecutionsResponse_executions = Lens.lens (\ListExecutionsResponse' {executions} -> executions) (\s@ListExecutionsResponse' {} a -> s {executions = a} :: ListExecutionsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListExecutionsResponse where
  rnf ListExecutionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf workflowId
      `Prelude.seq` Prelude.rnf executions
