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
-- Module      : Amazonka.Omics.ListWorkflows
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of workflows.
--
-- This operation returns paginated results.
module Amazonka.Omics.ListWorkflows
  ( -- * Creating a Request
    ListWorkflows (..),
    newListWorkflows,

    -- * Request Lenses
    listWorkflows_maxResults,
    listWorkflows_name,
    listWorkflows_startingToken,
    listWorkflows_type,

    -- * Destructuring the Response
    ListWorkflowsResponse (..),
    newListWorkflowsResponse,

    -- * Response Lenses
    listWorkflowsResponse_items,
    listWorkflowsResponse_nextToken,
    listWorkflowsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListWorkflows' smart constructor.
data ListWorkflows = ListWorkflows'
  { -- | The maximum number of workflows to return in one page of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The workflows\' name.
    name :: Prelude.Maybe Prelude.Text,
    -- | Specify the pagination token from a previous request to retrieve the
    -- next page of results.
    startingToken :: Prelude.Maybe Prelude.Text,
    -- | The workflows\' type.
    type' :: Prelude.Maybe WorkflowType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWorkflows' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listWorkflows_maxResults' - The maximum number of workflows to return in one page of results.
--
-- 'name', 'listWorkflows_name' - The workflows\' name.
--
-- 'startingToken', 'listWorkflows_startingToken' - Specify the pagination token from a previous request to retrieve the
-- next page of results.
--
-- 'type'', 'listWorkflows_type' - The workflows\' type.
newListWorkflows ::
  ListWorkflows
newListWorkflows =
  ListWorkflows'
    { maxResults = Prelude.Nothing,
      name = Prelude.Nothing,
      startingToken = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The maximum number of workflows to return in one page of results.
listWorkflows_maxResults :: Lens.Lens' ListWorkflows (Prelude.Maybe Prelude.Natural)
listWorkflows_maxResults = Lens.lens (\ListWorkflows' {maxResults} -> maxResults) (\s@ListWorkflows' {} a -> s {maxResults = a} :: ListWorkflows)

-- | The workflows\' name.
listWorkflows_name :: Lens.Lens' ListWorkflows (Prelude.Maybe Prelude.Text)
listWorkflows_name = Lens.lens (\ListWorkflows' {name} -> name) (\s@ListWorkflows' {} a -> s {name = a} :: ListWorkflows)

-- | Specify the pagination token from a previous request to retrieve the
-- next page of results.
listWorkflows_startingToken :: Lens.Lens' ListWorkflows (Prelude.Maybe Prelude.Text)
listWorkflows_startingToken = Lens.lens (\ListWorkflows' {startingToken} -> startingToken) (\s@ListWorkflows' {} a -> s {startingToken = a} :: ListWorkflows)

-- | The workflows\' type.
listWorkflows_type :: Lens.Lens' ListWorkflows (Prelude.Maybe WorkflowType)
listWorkflows_type = Lens.lens (\ListWorkflows' {type'} -> type') (\s@ListWorkflows' {} a -> s {type' = a} :: ListWorkflows)

instance Core.AWSPager ListWorkflows where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listWorkflowsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listWorkflowsResponse_items Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listWorkflows_startingToken
          Lens..~ rs
          Lens.^? listWorkflowsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListWorkflows where
  type
    AWSResponse ListWorkflows =
      ListWorkflowsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListWorkflowsResponse'
            Prelude.<$> (x Data..?> "items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListWorkflows where
  hashWithSalt _salt ListWorkflows' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` startingToken
      `Prelude.hashWithSalt` type'

instance Prelude.NFData ListWorkflows where
  rnf ListWorkflows' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf startingToken
      `Prelude.seq` Prelude.rnf type'

instance Data.ToHeaders ListWorkflows where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListWorkflows where
  toPath = Prelude.const "/workflow"

instance Data.ToQuery ListWorkflows where
  toQuery ListWorkflows' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "name" Data.=: name,
        "startingToken" Data.=: startingToken,
        "type" Data.=: type'
      ]

-- | /See:/ 'newListWorkflowsResponse' smart constructor.
data ListWorkflowsResponse = ListWorkflowsResponse'
  { -- | The workflows\' items.
    items :: Prelude.Maybe [WorkflowListItem],
    -- | A pagination token that\'s included if more results are available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWorkflowsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'listWorkflowsResponse_items' - The workflows\' items.
--
-- 'nextToken', 'listWorkflowsResponse_nextToken' - A pagination token that\'s included if more results are available.
--
-- 'httpStatus', 'listWorkflowsResponse_httpStatus' - The response's http status code.
newListWorkflowsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListWorkflowsResponse
newListWorkflowsResponse pHttpStatus_ =
  ListWorkflowsResponse'
    { items = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The workflows\' items.
listWorkflowsResponse_items :: Lens.Lens' ListWorkflowsResponse (Prelude.Maybe [WorkflowListItem])
listWorkflowsResponse_items = Lens.lens (\ListWorkflowsResponse' {items} -> items) (\s@ListWorkflowsResponse' {} a -> s {items = a} :: ListWorkflowsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A pagination token that\'s included if more results are available.
listWorkflowsResponse_nextToken :: Lens.Lens' ListWorkflowsResponse (Prelude.Maybe Prelude.Text)
listWorkflowsResponse_nextToken = Lens.lens (\ListWorkflowsResponse' {nextToken} -> nextToken) (\s@ListWorkflowsResponse' {} a -> s {nextToken = a} :: ListWorkflowsResponse)

-- | The response's http status code.
listWorkflowsResponse_httpStatus :: Lens.Lens' ListWorkflowsResponse Prelude.Int
listWorkflowsResponse_httpStatus = Lens.lens (\ListWorkflowsResponse' {httpStatus} -> httpStatus) (\s@ListWorkflowsResponse' {} a -> s {httpStatus = a} :: ListWorkflowsResponse)

instance Prelude.NFData ListWorkflowsResponse where
  rnf ListWorkflowsResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
