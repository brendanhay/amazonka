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
-- Module      : Network.AWS.Glue.ListWorkflows
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists names of workflows created in the account.
module Network.AWS.Glue.ListWorkflows
  ( -- * Creating a Request
    ListWorkflows (..),
    newListWorkflows,

    -- * Request Lenses
    listWorkflows_nextToken,
    listWorkflows_maxResults,

    -- * Destructuring the Response
    ListWorkflowsResponse (..),
    newListWorkflowsResponse,

    -- * Response Lenses
    listWorkflowsResponse_nextToken,
    listWorkflowsResponse_workflows,
    listWorkflowsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListWorkflows' smart constructor.
data ListWorkflows = ListWorkflows'
  { -- | A continuation token, if this is a continuation request.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum size of a list to return.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListWorkflows' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listWorkflows_nextToken' - A continuation token, if this is a continuation request.
--
-- 'maxResults', 'listWorkflows_maxResults' - The maximum size of a list to return.
newListWorkflows ::
  ListWorkflows
newListWorkflows =
  ListWorkflows'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | A continuation token, if this is a continuation request.
listWorkflows_nextToken :: Lens.Lens' ListWorkflows (Core.Maybe Core.Text)
listWorkflows_nextToken = Lens.lens (\ListWorkflows' {nextToken} -> nextToken) (\s@ListWorkflows' {} a -> s {nextToken = a} :: ListWorkflows)

-- | The maximum size of a list to return.
listWorkflows_maxResults :: Lens.Lens' ListWorkflows (Core.Maybe Core.Natural)
listWorkflows_maxResults = Lens.lens (\ListWorkflows' {maxResults} -> maxResults) (\s@ListWorkflows' {} a -> s {maxResults = a} :: ListWorkflows)

instance Core.AWSRequest ListWorkflows where
  type
    AWSResponse ListWorkflows =
      ListWorkflowsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListWorkflowsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Workflows")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListWorkflows

instance Core.NFData ListWorkflows

instance Core.ToHeaders ListWorkflows where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.ListWorkflows" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListWorkflows where
  toJSON ListWorkflows' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults
          ]
      )

instance Core.ToPath ListWorkflows where
  toPath = Core.const "/"

instance Core.ToQuery ListWorkflows where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListWorkflowsResponse' smart constructor.
data ListWorkflowsResponse = ListWorkflowsResponse'
  { -- | A continuation token, if not all workflow names have been returned.
    nextToken :: Core.Maybe Core.Text,
    -- | List of names of workflows in the account.
    workflows :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListWorkflowsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listWorkflowsResponse_nextToken' - A continuation token, if not all workflow names have been returned.
--
-- 'workflows', 'listWorkflowsResponse_workflows' - List of names of workflows in the account.
--
-- 'httpStatus', 'listWorkflowsResponse_httpStatus' - The response's http status code.
newListWorkflowsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListWorkflowsResponse
newListWorkflowsResponse pHttpStatus_ =
  ListWorkflowsResponse'
    { nextToken = Core.Nothing,
      workflows = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A continuation token, if not all workflow names have been returned.
listWorkflowsResponse_nextToken :: Lens.Lens' ListWorkflowsResponse (Core.Maybe Core.Text)
listWorkflowsResponse_nextToken = Lens.lens (\ListWorkflowsResponse' {nextToken} -> nextToken) (\s@ListWorkflowsResponse' {} a -> s {nextToken = a} :: ListWorkflowsResponse)

-- | List of names of workflows in the account.
listWorkflowsResponse_workflows :: Lens.Lens' ListWorkflowsResponse (Core.Maybe (Core.NonEmpty Core.Text))
listWorkflowsResponse_workflows = Lens.lens (\ListWorkflowsResponse' {workflows} -> workflows) (\s@ListWorkflowsResponse' {} a -> s {workflows = a} :: ListWorkflowsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listWorkflowsResponse_httpStatus :: Lens.Lens' ListWorkflowsResponse Core.Int
listWorkflowsResponse_httpStatus = Lens.lens (\ListWorkflowsResponse' {httpStatus} -> httpStatus) (\s@ListWorkflowsResponse' {} a -> s {httpStatus = a} :: ListWorkflowsResponse)

instance Core.NFData ListWorkflowsResponse
