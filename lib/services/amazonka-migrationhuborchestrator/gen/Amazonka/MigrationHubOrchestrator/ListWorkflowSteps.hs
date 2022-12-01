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
-- Module      : Amazonka.MigrationHubOrchestrator.ListWorkflowSteps
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the steps in a workflow.
--
-- This operation returns paginated results.
module Amazonka.MigrationHubOrchestrator.ListWorkflowSteps
  ( -- * Creating a Request
    ListWorkflowSteps (..),
    newListWorkflowSteps,

    -- * Request Lenses
    listWorkflowSteps_nextToken,
    listWorkflowSteps_maxResults,
    listWorkflowSteps_workflowId,
    listWorkflowSteps_stepGroupId,

    -- * Destructuring the Response
    ListWorkflowStepsResponse (..),
    newListWorkflowStepsResponse,

    -- * Response Lenses
    listWorkflowStepsResponse_nextToken,
    listWorkflowStepsResponse_httpStatus,
    listWorkflowStepsResponse_workflowStepsSummary,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MigrationHubOrchestrator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListWorkflowSteps' smart constructor.
data ListWorkflowSteps = ListWorkflowSteps'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results that can be returned.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the migration workflow.
    workflowId :: Prelude.Text,
    -- | The ID of the step group.
    stepGroupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWorkflowSteps' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listWorkflowSteps_nextToken' - The pagination token.
--
-- 'maxResults', 'listWorkflowSteps_maxResults' - The maximum number of results that can be returned.
--
-- 'workflowId', 'listWorkflowSteps_workflowId' - The ID of the migration workflow.
--
-- 'stepGroupId', 'listWorkflowSteps_stepGroupId' - The ID of the step group.
newListWorkflowSteps ::
  -- | 'workflowId'
  Prelude.Text ->
  -- | 'stepGroupId'
  Prelude.Text ->
  ListWorkflowSteps
newListWorkflowSteps pWorkflowId_ pStepGroupId_ =
  ListWorkflowSteps'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      workflowId = pWorkflowId_,
      stepGroupId = pStepGroupId_
    }

-- | The pagination token.
listWorkflowSteps_nextToken :: Lens.Lens' ListWorkflowSteps (Prelude.Maybe Prelude.Text)
listWorkflowSteps_nextToken = Lens.lens (\ListWorkflowSteps' {nextToken} -> nextToken) (\s@ListWorkflowSteps' {} a -> s {nextToken = a} :: ListWorkflowSteps)

-- | The maximum number of results that can be returned.
listWorkflowSteps_maxResults :: Lens.Lens' ListWorkflowSteps (Prelude.Maybe Prelude.Natural)
listWorkflowSteps_maxResults = Lens.lens (\ListWorkflowSteps' {maxResults} -> maxResults) (\s@ListWorkflowSteps' {} a -> s {maxResults = a} :: ListWorkflowSteps)

-- | The ID of the migration workflow.
listWorkflowSteps_workflowId :: Lens.Lens' ListWorkflowSteps Prelude.Text
listWorkflowSteps_workflowId = Lens.lens (\ListWorkflowSteps' {workflowId} -> workflowId) (\s@ListWorkflowSteps' {} a -> s {workflowId = a} :: ListWorkflowSteps)

-- | The ID of the step group.
listWorkflowSteps_stepGroupId :: Lens.Lens' ListWorkflowSteps Prelude.Text
listWorkflowSteps_stepGroupId = Lens.lens (\ListWorkflowSteps' {stepGroupId} -> stepGroupId) (\s@ListWorkflowSteps' {} a -> s {stepGroupId = a} :: ListWorkflowSteps)

instance Core.AWSPager ListWorkflowSteps where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listWorkflowStepsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listWorkflowStepsResponse_workflowStepsSummary
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listWorkflowSteps_nextToken
          Lens..~ rs
          Lens.^? listWorkflowStepsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListWorkflowSteps where
  type
    AWSResponse ListWorkflowSteps =
      ListWorkflowStepsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListWorkflowStepsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..?> "workflowStepsSummary"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListWorkflowSteps where
  hashWithSalt _salt ListWorkflowSteps' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` workflowId
      `Prelude.hashWithSalt` stepGroupId

instance Prelude.NFData ListWorkflowSteps where
  rnf ListWorkflowSteps' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf workflowId
      `Prelude.seq` Prelude.rnf stepGroupId

instance Core.ToHeaders ListWorkflowSteps where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListWorkflowSteps where
  toPath ListWorkflowSteps' {..} =
    Prelude.mconcat
      [ "/workflow/",
        Core.toBS workflowId,
        "/workflowstepgroups/",
        Core.toBS stepGroupId,
        "/workflowsteps"
      ]

instance Core.ToQuery ListWorkflowSteps where
  toQuery ListWorkflowSteps' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListWorkflowStepsResponse' smart constructor.
data ListWorkflowStepsResponse = ListWorkflowStepsResponse'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The summary of steps in a migration workflow.
    workflowStepsSummary :: [WorkflowStepSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWorkflowStepsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listWorkflowStepsResponse_nextToken' - The pagination token.
--
-- 'httpStatus', 'listWorkflowStepsResponse_httpStatus' - The response's http status code.
--
-- 'workflowStepsSummary', 'listWorkflowStepsResponse_workflowStepsSummary' - The summary of steps in a migration workflow.
newListWorkflowStepsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListWorkflowStepsResponse
newListWorkflowStepsResponse pHttpStatus_ =
  ListWorkflowStepsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      workflowStepsSummary = Prelude.mempty
    }

-- | The pagination token.
listWorkflowStepsResponse_nextToken :: Lens.Lens' ListWorkflowStepsResponse (Prelude.Maybe Prelude.Text)
listWorkflowStepsResponse_nextToken = Lens.lens (\ListWorkflowStepsResponse' {nextToken} -> nextToken) (\s@ListWorkflowStepsResponse' {} a -> s {nextToken = a} :: ListWorkflowStepsResponse)

-- | The response's http status code.
listWorkflowStepsResponse_httpStatus :: Lens.Lens' ListWorkflowStepsResponse Prelude.Int
listWorkflowStepsResponse_httpStatus = Lens.lens (\ListWorkflowStepsResponse' {httpStatus} -> httpStatus) (\s@ListWorkflowStepsResponse' {} a -> s {httpStatus = a} :: ListWorkflowStepsResponse)

-- | The summary of steps in a migration workflow.
listWorkflowStepsResponse_workflowStepsSummary :: Lens.Lens' ListWorkflowStepsResponse [WorkflowStepSummary]
listWorkflowStepsResponse_workflowStepsSummary = Lens.lens (\ListWorkflowStepsResponse' {workflowStepsSummary} -> workflowStepsSummary) (\s@ListWorkflowStepsResponse' {} a -> s {workflowStepsSummary = a} :: ListWorkflowStepsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListWorkflowStepsResponse where
  rnf ListWorkflowStepsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf workflowStepsSummary
