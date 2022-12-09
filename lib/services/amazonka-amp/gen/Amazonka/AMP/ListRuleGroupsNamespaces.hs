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
-- Module      : Amazonka.AMP.ListRuleGroupsNamespaces
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists rule groups namespaces.
--
-- This operation returns paginated results.
module Amazonka.AMP.ListRuleGroupsNamespaces
  ( -- * Creating a Request
    ListRuleGroupsNamespaces (..),
    newListRuleGroupsNamespaces,

    -- * Request Lenses
    listRuleGroupsNamespaces_maxResults,
    listRuleGroupsNamespaces_name,
    listRuleGroupsNamespaces_nextToken,
    listRuleGroupsNamespaces_workspaceId,

    -- * Destructuring the Response
    ListRuleGroupsNamespacesResponse (..),
    newListRuleGroupsNamespacesResponse,

    -- * Response Lenses
    listRuleGroupsNamespacesResponse_nextToken,
    listRuleGroupsNamespacesResponse_httpStatus,
    listRuleGroupsNamespacesResponse_ruleGroupsNamespaces,
  )
where

import Amazonka.AMP.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a ListRuleGroupsNamespaces operation.
--
-- /See:/ 'newListRuleGroupsNamespaces' smart constructor.
data ListRuleGroupsNamespaces = ListRuleGroupsNamespaces'
  { -- | Maximum results to return in response (default=100, maximum=1000).
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Optional filter for rule groups namespace name. Only the rule groups
    -- namespace that begin with this value will be returned.
    name :: Prelude.Maybe Prelude.Text,
    -- | Pagination token to request the next page in a paginated list. This
    -- token is obtained from the output of the previous
    -- ListRuleGroupsNamespaces request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the workspace.
    workspaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRuleGroupsNamespaces' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listRuleGroupsNamespaces_maxResults' - Maximum results to return in response (default=100, maximum=1000).
--
-- 'name', 'listRuleGroupsNamespaces_name' - Optional filter for rule groups namespace name. Only the rule groups
-- namespace that begin with this value will be returned.
--
-- 'nextToken', 'listRuleGroupsNamespaces_nextToken' - Pagination token to request the next page in a paginated list. This
-- token is obtained from the output of the previous
-- ListRuleGroupsNamespaces request.
--
-- 'workspaceId', 'listRuleGroupsNamespaces_workspaceId' - The ID of the workspace.
newListRuleGroupsNamespaces ::
  -- | 'workspaceId'
  Prelude.Text ->
  ListRuleGroupsNamespaces
newListRuleGroupsNamespaces pWorkspaceId_ =
  ListRuleGroupsNamespaces'
    { maxResults =
        Prelude.Nothing,
      name = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      workspaceId = pWorkspaceId_
    }

-- | Maximum results to return in response (default=100, maximum=1000).
listRuleGroupsNamespaces_maxResults :: Lens.Lens' ListRuleGroupsNamespaces (Prelude.Maybe Prelude.Natural)
listRuleGroupsNamespaces_maxResults = Lens.lens (\ListRuleGroupsNamespaces' {maxResults} -> maxResults) (\s@ListRuleGroupsNamespaces' {} a -> s {maxResults = a} :: ListRuleGroupsNamespaces)

-- | Optional filter for rule groups namespace name. Only the rule groups
-- namespace that begin with this value will be returned.
listRuleGroupsNamespaces_name :: Lens.Lens' ListRuleGroupsNamespaces (Prelude.Maybe Prelude.Text)
listRuleGroupsNamespaces_name = Lens.lens (\ListRuleGroupsNamespaces' {name} -> name) (\s@ListRuleGroupsNamespaces' {} a -> s {name = a} :: ListRuleGroupsNamespaces)

-- | Pagination token to request the next page in a paginated list. This
-- token is obtained from the output of the previous
-- ListRuleGroupsNamespaces request.
listRuleGroupsNamespaces_nextToken :: Lens.Lens' ListRuleGroupsNamespaces (Prelude.Maybe Prelude.Text)
listRuleGroupsNamespaces_nextToken = Lens.lens (\ListRuleGroupsNamespaces' {nextToken} -> nextToken) (\s@ListRuleGroupsNamespaces' {} a -> s {nextToken = a} :: ListRuleGroupsNamespaces)

-- | The ID of the workspace.
listRuleGroupsNamespaces_workspaceId :: Lens.Lens' ListRuleGroupsNamespaces Prelude.Text
listRuleGroupsNamespaces_workspaceId = Lens.lens (\ListRuleGroupsNamespaces' {workspaceId} -> workspaceId) (\s@ListRuleGroupsNamespaces' {} a -> s {workspaceId = a} :: ListRuleGroupsNamespaces)

instance Core.AWSPager ListRuleGroupsNamespaces where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRuleGroupsNamespacesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listRuleGroupsNamespacesResponse_ruleGroupsNamespaces
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listRuleGroupsNamespaces_nextToken
          Lens..~ rs
          Lens.^? listRuleGroupsNamespacesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListRuleGroupsNamespaces where
  type
    AWSResponse ListRuleGroupsNamespaces =
      ListRuleGroupsNamespacesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRuleGroupsNamespacesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..?> "ruleGroupsNamespaces"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListRuleGroupsNamespaces where
  hashWithSalt _salt ListRuleGroupsNamespaces' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` workspaceId

instance Prelude.NFData ListRuleGroupsNamespaces where
  rnf ListRuleGroupsNamespaces' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf workspaceId

instance Data.ToHeaders ListRuleGroupsNamespaces where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListRuleGroupsNamespaces where
  toPath ListRuleGroupsNamespaces' {..} =
    Prelude.mconcat
      [ "/workspaces/",
        Data.toBS workspaceId,
        "/rulegroupsnamespaces"
      ]

instance Data.ToQuery ListRuleGroupsNamespaces where
  toQuery ListRuleGroupsNamespaces' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "name" Data.=: name,
        "nextToken" Data.=: nextToken
      ]

-- | Represents the output of a ListRuleGroupsNamespaces operation.
--
-- /See:/ 'newListRuleGroupsNamespacesResponse' smart constructor.
data ListRuleGroupsNamespacesResponse = ListRuleGroupsNamespacesResponse'
  { -- | Pagination token to use when requesting the next page in this list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The list of the selected rule groups namespaces.
    ruleGroupsNamespaces :: [RuleGroupsNamespaceSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRuleGroupsNamespacesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRuleGroupsNamespacesResponse_nextToken' - Pagination token to use when requesting the next page in this list.
--
-- 'httpStatus', 'listRuleGroupsNamespacesResponse_httpStatus' - The response's http status code.
--
-- 'ruleGroupsNamespaces', 'listRuleGroupsNamespacesResponse_ruleGroupsNamespaces' - The list of the selected rule groups namespaces.
newListRuleGroupsNamespacesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRuleGroupsNamespacesResponse
newListRuleGroupsNamespacesResponse pHttpStatus_ =
  ListRuleGroupsNamespacesResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      ruleGroupsNamespaces = Prelude.mempty
    }

-- | Pagination token to use when requesting the next page in this list.
listRuleGroupsNamespacesResponse_nextToken :: Lens.Lens' ListRuleGroupsNamespacesResponse (Prelude.Maybe Prelude.Text)
listRuleGroupsNamespacesResponse_nextToken = Lens.lens (\ListRuleGroupsNamespacesResponse' {nextToken} -> nextToken) (\s@ListRuleGroupsNamespacesResponse' {} a -> s {nextToken = a} :: ListRuleGroupsNamespacesResponse)

-- | The response's http status code.
listRuleGroupsNamespacesResponse_httpStatus :: Lens.Lens' ListRuleGroupsNamespacesResponse Prelude.Int
listRuleGroupsNamespacesResponse_httpStatus = Lens.lens (\ListRuleGroupsNamespacesResponse' {httpStatus} -> httpStatus) (\s@ListRuleGroupsNamespacesResponse' {} a -> s {httpStatus = a} :: ListRuleGroupsNamespacesResponse)

-- | The list of the selected rule groups namespaces.
listRuleGroupsNamespacesResponse_ruleGroupsNamespaces :: Lens.Lens' ListRuleGroupsNamespacesResponse [RuleGroupsNamespaceSummary]
listRuleGroupsNamespacesResponse_ruleGroupsNamespaces = Lens.lens (\ListRuleGroupsNamespacesResponse' {ruleGroupsNamespaces} -> ruleGroupsNamespaces) (\s@ListRuleGroupsNamespacesResponse' {} a -> s {ruleGroupsNamespaces = a} :: ListRuleGroupsNamespacesResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListRuleGroupsNamespacesResponse
  where
  rnf ListRuleGroupsNamespacesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf ruleGroupsNamespaces
