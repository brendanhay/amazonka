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
-- Module      : Amazonka.Grafana.ListWorkspaces
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of Amazon Managed Grafana workspaces in the account, with
-- some information about each workspace. For more complete information
-- about one workspace, use
-- <https://docs.aws.amazon.com/AAMG/latest/APIReference/API_DescribeWorkspace.html DescribeWorkspace>.
--
-- This operation returns paginated results.
module Amazonka.Grafana.ListWorkspaces
  ( -- * Creating a Request
    ListWorkspaces (..),
    newListWorkspaces,

    -- * Request Lenses
    listWorkspaces_maxResults,
    listWorkspaces_nextToken,

    -- * Destructuring the Response
    ListWorkspacesResponse (..),
    newListWorkspacesResponse,

    -- * Response Lenses
    listWorkspacesResponse_nextToken,
    listWorkspacesResponse_httpStatus,
    listWorkspacesResponse_workspaces,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Grafana.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListWorkspaces' smart constructor.
data ListWorkspaces = ListWorkspaces'
  { -- | The maximum number of workspaces to include in the results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of workspaces to return. (You receive this
    -- token from a previous @ListWorkspaces@ operation.)
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWorkspaces' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listWorkspaces_maxResults' - The maximum number of workspaces to include in the results.
--
-- 'nextToken', 'listWorkspaces_nextToken' - The token for the next set of workspaces to return. (You receive this
-- token from a previous @ListWorkspaces@ operation.)
newListWorkspaces ::
  ListWorkspaces
newListWorkspaces =
  ListWorkspaces'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of workspaces to include in the results.
listWorkspaces_maxResults :: Lens.Lens' ListWorkspaces (Prelude.Maybe Prelude.Natural)
listWorkspaces_maxResults = Lens.lens (\ListWorkspaces' {maxResults} -> maxResults) (\s@ListWorkspaces' {} a -> s {maxResults = a} :: ListWorkspaces)

-- | The token for the next set of workspaces to return. (You receive this
-- token from a previous @ListWorkspaces@ operation.)
listWorkspaces_nextToken :: Lens.Lens' ListWorkspaces (Prelude.Maybe Prelude.Text)
listWorkspaces_nextToken = Lens.lens (\ListWorkspaces' {nextToken} -> nextToken) (\s@ListWorkspaces' {} a -> s {nextToken = a} :: ListWorkspaces)

instance Core.AWSPager ListWorkspaces where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listWorkspacesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        (rs Lens.^. listWorkspacesResponse_workspaces) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listWorkspaces_nextToken
          Lens..~ rs
          Lens.^? listWorkspacesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListWorkspaces where
  type
    AWSResponse ListWorkspaces =
      ListWorkspacesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListWorkspacesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "workspaces" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListWorkspaces where
  hashWithSalt _salt ListWorkspaces' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListWorkspaces where
  rnf ListWorkspaces' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListWorkspaces where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListWorkspaces where
  toPath = Prelude.const "/workspaces"

instance Data.ToQuery ListWorkspaces where
  toQuery ListWorkspaces' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListWorkspacesResponse' smart constructor.
data ListWorkspacesResponse = ListWorkspacesResponse'
  { -- | The token to use when requesting the next set of workspaces.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array of structures that contain some information about the
    -- workspaces in the account.
    workspaces :: [WorkspaceSummary]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWorkspacesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listWorkspacesResponse_nextToken' - The token to use when requesting the next set of workspaces.
--
-- 'httpStatus', 'listWorkspacesResponse_httpStatus' - The response's http status code.
--
-- 'workspaces', 'listWorkspacesResponse_workspaces' - An array of structures that contain some information about the
-- workspaces in the account.
newListWorkspacesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListWorkspacesResponse
newListWorkspacesResponse pHttpStatus_ =
  ListWorkspacesResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      workspaces = Prelude.mempty
    }

-- | The token to use when requesting the next set of workspaces.
listWorkspacesResponse_nextToken :: Lens.Lens' ListWorkspacesResponse (Prelude.Maybe Prelude.Text)
listWorkspacesResponse_nextToken = Lens.lens (\ListWorkspacesResponse' {nextToken} -> nextToken) (\s@ListWorkspacesResponse' {} a -> s {nextToken = a} :: ListWorkspacesResponse)

-- | The response's http status code.
listWorkspacesResponse_httpStatus :: Lens.Lens' ListWorkspacesResponse Prelude.Int
listWorkspacesResponse_httpStatus = Lens.lens (\ListWorkspacesResponse' {httpStatus} -> httpStatus) (\s@ListWorkspacesResponse' {} a -> s {httpStatus = a} :: ListWorkspacesResponse)

-- | An array of structures that contain some information about the
-- workspaces in the account.
listWorkspacesResponse_workspaces :: Lens.Lens' ListWorkspacesResponse [WorkspaceSummary]
listWorkspacesResponse_workspaces = Lens.lens (\ListWorkspacesResponse' {workspaces} -> workspaces) (\s@ListWorkspacesResponse' {} a -> s {workspaces = a} :: ListWorkspacesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListWorkspacesResponse where
  rnf ListWorkspacesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf workspaces
