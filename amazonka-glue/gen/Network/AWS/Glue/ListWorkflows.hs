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

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListWorkflows' smart constructor.
data ListWorkflows = ListWorkflows'
  { -- | A continuation token, if this is a continuation request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum size of a list to return.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | A continuation token, if this is a continuation request.
listWorkflows_nextToken :: Lens.Lens' ListWorkflows (Prelude.Maybe Prelude.Text)
listWorkflows_nextToken = Lens.lens (\ListWorkflows' {nextToken} -> nextToken) (\s@ListWorkflows' {} a -> s {nextToken = a} :: ListWorkflows)

-- | The maximum size of a list to return.
listWorkflows_maxResults :: Lens.Lens' ListWorkflows (Prelude.Maybe Prelude.Natural)
listWorkflows_maxResults = Lens.lens (\ListWorkflows' {maxResults} -> maxResults) (\s@ListWorkflows' {} a -> s {maxResults = a} :: ListWorkflows)

instance Prelude.AWSRequest ListWorkflows where
  type Rs ListWorkflows = ListWorkflowsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListWorkflowsResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> (x Prelude..?> "Workflows")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListWorkflows

instance Prelude.NFData ListWorkflows

instance Prelude.ToHeaders ListWorkflows where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("AWSGlue.ListWorkflows" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListWorkflows where
  toJSON ListWorkflows' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("MaxResults" Prelude..=) Prelude.<$> maxResults
          ]
      )

instance Prelude.ToPath ListWorkflows where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListWorkflows where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListWorkflowsResponse' smart constructor.
data ListWorkflowsResponse = ListWorkflowsResponse'
  { -- | A continuation token, if not all workflow names have been returned.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | List of names of workflows in the account.
    workflows :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  ListWorkflowsResponse
newListWorkflowsResponse pHttpStatus_ =
  ListWorkflowsResponse'
    { nextToken = Prelude.Nothing,
      workflows = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A continuation token, if not all workflow names have been returned.
listWorkflowsResponse_nextToken :: Lens.Lens' ListWorkflowsResponse (Prelude.Maybe Prelude.Text)
listWorkflowsResponse_nextToken = Lens.lens (\ListWorkflowsResponse' {nextToken} -> nextToken) (\s@ListWorkflowsResponse' {} a -> s {nextToken = a} :: ListWorkflowsResponse)

-- | List of names of workflows in the account.
listWorkflowsResponse_workflows :: Lens.Lens' ListWorkflowsResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
listWorkflowsResponse_workflows = Lens.lens (\ListWorkflowsResponse' {workflows} -> workflows) (\s@ListWorkflowsResponse' {} a -> s {workflows = a} :: ListWorkflowsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listWorkflowsResponse_httpStatus :: Lens.Lens' ListWorkflowsResponse Prelude.Int
listWorkflowsResponse_httpStatus = Lens.lens (\ListWorkflowsResponse' {httpStatus} -> httpStatus) (\s@ListWorkflowsResponse' {} a -> s {httpStatus = a} :: ListWorkflowsResponse)

instance Prelude.NFData ListWorkflowsResponse
