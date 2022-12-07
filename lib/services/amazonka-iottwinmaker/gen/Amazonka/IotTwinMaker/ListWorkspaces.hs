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
-- Module      : Amazonka.IotTwinMaker.ListWorkspaces
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about workspaces in the current account.
module Amazonka.IotTwinMaker.ListWorkspaces
  ( -- * Creating a Request
    ListWorkspaces (..),
    newListWorkspaces,

    -- * Request Lenses
    listWorkspaces_nextToken,
    listWorkspaces_maxResults,

    -- * Destructuring the Response
    ListWorkspacesResponse (..),
    newListWorkspacesResponse,

    -- * Response Lenses
    listWorkspacesResponse_nextToken,
    listWorkspacesResponse_workspaceSummaries,
    listWorkspacesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListWorkspaces' smart constructor.
data ListWorkspaces = ListWorkspaces'
  { -- | The string that specifies the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return at one time. The default is 25.
    --
    -- Valid Range: Minimum value of 1. Maximum value of 250.
    maxResults :: Prelude.Maybe Prelude.Natural
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
-- 'nextToken', 'listWorkspaces_nextToken' - The string that specifies the next page of results.
--
-- 'maxResults', 'listWorkspaces_maxResults' - The maximum number of results to return at one time. The default is 25.
--
-- Valid Range: Minimum value of 1. Maximum value of 250.
newListWorkspaces ::
  ListWorkspaces
newListWorkspaces =
  ListWorkspaces'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The string that specifies the next page of results.
listWorkspaces_nextToken :: Lens.Lens' ListWorkspaces (Prelude.Maybe Prelude.Text)
listWorkspaces_nextToken = Lens.lens (\ListWorkspaces' {nextToken} -> nextToken) (\s@ListWorkspaces' {} a -> s {nextToken = a} :: ListWorkspaces)

-- | The maximum number of results to return at one time. The default is 25.
--
-- Valid Range: Minimum value of 1. Maximum value of 250.
listWorkspaces_maxResults :: Lens.Lens' ListWorkspaces (Prelude.Maybe Prelude.Natural)
listWorkspaces_maxResults = Lens.lens (\ListWorkspaces' {maxResults} -> maxResults) (\s@ListWorkspaces' {} a -> s {maxResults = a} :: ListWorkspaces)

instance Core.AWSRequest ListWorkspaces where
  type
    AWSResponse ListWorkspaces =
      ListWorkspacesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListWorkspacesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x Data..?> "workspaceSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListWorkspaces where
  hashWithSalt _salt ListWorkspaces' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListWorkspaces where
  rnf ListWorkspaces' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

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

instance Data.ToJSON ListWorkspaces where
  toJSON ListWorkspaces' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nextToken" Data..=) Prelude.<$> nextToken,
            ("maxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath ListWorkspaces where
  toPath = Prelude.const "/workspaces-list"

instance Data.ToQuery ListWorkspaces where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListWorkspacesResponse' smart constructor.
data ListWorkspacesResponse = ListWorkspacesResponse'
  { -- | The string that specifies the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of objects that contain information about the workspaces.
    workspaceSummaries :: Prelude.Maybe [WorkspaceSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWorkspacesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listWorkspacesResponse_nextToken' - The string that specifies the next page of results.
--
-- 'workspaceSummaries', 'listWorkspacesResponse_workspaceSummaries' - A list of objects that contain information about the workspaces.
--
-- 'httpStatus', 'listWorkspacesResponse_httpStatus' - The response's http status code.
newListWorkspacesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListWorkspacesResponse
newListWorkspacesResponse pHttpStatus_ =
  ListWorkspacesResponse'
    { nextToken =
        Prelude.Nothing,
      workspaceSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The string that specifies the next page of results.
listWorkspacesResponse_nextToken :: Lens.Lens' ListWorkspacesResponse (Prelude.Maybe Prelude.Text)
listWorkspacesResponse_nextToken = Lens.lens (\ListWorkspacesResponse' {nextToken} -> nextToken) (\s@ListWorkspacesResponse' {} a -> s {nextToken = a} :: ListWorkspacesResponse)

-- | A list of objects that contain information about the workspaces.
listWorkspacesResponse_workspaceSummaries :: Lens.Lens' ListWorkspacesResponse (Prelude.Maybe [WorkspaceSummary])
listWorkspacesResponse_workspaceSummaries = Lens.lens (\ListWorkspacesResponse' {workspaceSummaries} -> workspaceSummaries) (\s@ListWorkspacesResponse' {} a -> s {workspaceSummaries = a} :: ListWorkspacesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listWorkspacesResponse_httpStatus :: Lens.Lens' ListWorkspacesResponse Prelude.Int
listWorkspacesResponse_httpStatus = Lens.lens (\ListWorkspacesResponse' {httpStatus} -> httpStatus) (\s@ListWorkspacesResponse' {} a -> s {httpStatus = a} :: ListWorkspacesResponse)

instance Prelude.NFData ListWorkspacesResponse where
  rnf ListWorkspacesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf workspaceSummaries
      `Prelude.seq` Prelude.rnf httpStatus
