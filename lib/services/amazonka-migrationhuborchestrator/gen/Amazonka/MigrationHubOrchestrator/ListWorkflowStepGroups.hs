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
-- Module      : Amazonka.MigrationHubOrchestrator.ListWorkflowStepGroups
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the step groups in a migration workflow.
--
-- This operation returns paginated results.
module Amazonka.MigrationHubOrchestrator.ListWorkflowStepGroups
  ( -- * Creating a Request
    ListWorkflowStepGroups (..),
    newListWorkflowStepGroups,

    -- * Request Lenses
    listWorkflowStepGroups_nextToken,
    listWorkflowStepGroups_maxResults,
    listWorkflowStepGroups_workflowId,

    -- * Destructuring the Response
    ListWorkflowStepGroupsResponse (..),
    newListWorkflowStepGroupsResponse,

    -- * Response Lenses
    listWorkflowStepGroupsResponse_nextToken,
    listWorkflowStepGroupsResponse_httpStatus,
    listWorkflowStepGroupsResponse_workflowStepGroupsSummary,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MigrationHubOrchestrator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListWorkflowStepGroups' smart constructor.
data ListWorkflowStepGroups = ListWorkflowStepGroups'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results that can be returned.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the migration workflow.
    workflowId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWorkflowStepGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listWorkflowStepGroups_nextToken' - The pagination token.
--
-- 'maxResults', 'listWorkflowStepGroups_maxResults' - The maximum number of results that can be returned.
--
-- 'workflowId', 'listWorkflowStepGroups_workflowId' - The ID of the migration workflow.
newListWorkflowStepGroups ::
  -- | 'workflowId'
  Prelude.Text ->
  ListWorkflowStepGroups
newListWorkflowStepGroups pWorkflowId_ =
  ListWorkflowStepGroups'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      workflowId = pWorkflowId_
    }

-- | The pagination token.
listWorkflowStepGroups_nextToken :: Lens.Lens' ListWorkflowStepGroups (Prelude.Maybe Prelude.Text)
listWorkflowStepGroups_nextToken = Lens.lens (\ListWorkflowStepGroups' {nextToken} -> nextToken) (\s@ListWorkflowStepGroups' {} a -> s {nextToken = a} :: ListWorkflowStepGroups)

-- | The maximum number of results that can be returned.
listWorkflowStepGroups_maxResults :: Lens.Lens' ListWorkflowStepGroups (Prelude.Maybe Prelude.Natural)
listWorkflowStepGroups_maxResults = Lens.lens (\ListWorkflowStepGroups' {maxResults} -> maxResults) (\s@ListWorkflowStepGroups' {} a -> s {maxResults = a} :: ListWorkflowStepGroups)

-- | The ID of the migration workflow.
listWorkflowStepGroups_workflowId :: Lens.Lens' ListWorkflowStepGroups Prelude.Text
listWorkflowStepGroups_workflowId = Lens.lens (\ListWorkflowStepGroups' {workflowId} -> workflowId) (\s@ListWorkflowStepGroups' {} a -> s {workflowId = a} :: ListWorkflowStepGroups)

instance Core.AWSPager ListWorkflowStepGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listWorkflowStepGroupsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listWorkflowStepGroupsResponse_workflowStepGroupsSummary
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listWorkflowStepGroups_nextToken
          Lens..~ rs
          Lens.^? listWorkflowStepGroupsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListWorkflowStepGroups where
  type
    AWSResponse ListWorkflowStepGroups =
      ListWorkflowStepGroupsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListWorkflowStepGroupsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..?> "workflowStepGroupsSummary"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListWorkflowStepGroups where
  hashWithSalt _salt ListWorkflowStepGroups' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` workflowId

instance Prelude.NFData ListWorkflowStepGroups where
  rnf ListWorkflowStepGroups' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf workflowId

instance Core.ToHeaders ListWorkflowStepGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListWorkflowStepGroups where
  toPath = Prelude.const "/workflowstepgroups"

instance Core.ToQuery ListWorkflowStepGroups where
  toQuery ListWorkflowStepGroups' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults,
        "workflowId" Core.=: workflowId
      ]

-- | /See:/ 'newListWorkflowStepGroupsResponse' smart constructor.
data ListWorkflowStepGroupsResponse = ListWorkflowStepGroupsResponse'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The summary of step groups in a migration workflow.
    workflowStepGroupsSummary :: [WorkflowStepGroupSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWorkflowStepGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listWorkflowStepGroupsResponse_nextToken' - The pagination token.
--
-- 'httpStatus', 'listWorkflowStepGroupsResponse_httpStatus' - The response's http status code.
--
-- 'workflowStepGroupsSummary', 'listWorkflowStepGroupsResponse_workflowStepGroupsSummary' - The summary of step groups in a migration workflow.
newListWorkflowStepGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListWorkflowStepGroupsResponse
newListWorkflowStepGroupsResponse pHttpStatus_ =
  ListWorkflowStepGroupsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      workflowStepGroupsSummary = Prelude.mempty
    }

-- | The pagination token.
listWorkflowStepGroupsResponse_nextToken :: Lens.Lens' ListWorkflowStepGroupsResponse (Prelude.Maybe Prelude.Text)
listWorkflowStepGroupsResponse_nextToken = Lens.lens (\ListWorkflowStepGroupsResponse' {nextToken} -> nextToken) (\s@ListWorkflowStepGroupsResponse' {} a -> s {nextToken = a} :: ListWorkflowStepGroupsResponse)

-- | The response's http status code.
listWorkflowStepGroupsResponse_httpStatus :: Lens.Lens' ListWorkflowStepGroupsResponse Prelude.Int
listWorkflowStepGroupsResponse_httpStatus = Lens.lens (\ListWorkflowStepGroupsResponse' {httpStatus} -> httpStatus) (\s@ListWorkflowStepGroupsResponse' {} a -> s {httpStatus = a} :: ListWorkflowStepGroupsResponse)

-- | The summary of step groups in a migration workflow.
listWorkflowStepGroupsResponse_workflowStepGroupsSummary :: Lens.Lens' ListWorkflowStepGroupsResponse [WorkflowStepGroupSummary]
listWorkflowStepGroupsResponse_workflowStepGroupsSummary = Lens.lens (\ListWorkflowStepGroupsResponse' {workflowStepGroupsSummary} -> workflowStepGroupsSummary) (\s@ListWorkflowStepGroupsResponse' {} a -> s {workflowStepGroupsSummary = a} :: ListWorkflowStepGroupsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListWorkflowStepGroupsResponse
  where
  rnf ListWorkflowStepGroupsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf workflowStepGroupsSummary
