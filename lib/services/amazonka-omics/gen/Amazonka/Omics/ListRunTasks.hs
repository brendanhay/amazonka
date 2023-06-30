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
-- Module      : Amazonka.Omics.ListRunTasks
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of tasks for a run.
--
-- This operation returns paginated results.
module Amazonka.Omics.ListRunTasks
  ( -- * Creating a Request
    ListRunTasks (..),
    newListRunTasks,

    -- * Request Lenses
    listRunTasks_maxResults,
    listRunTasks_startingToken,
    listRunTasks_status,
    listRunTasks_id,

    -- * Destructuring the Response
    ListRunTasksResponse (..),
    newListRunTasksResponse,

    -- * Response Lenses
    listRunTasksResponse_items,
    listRunTasksResponse_nextToken,
    listRunTasksResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListRunTasks' smart constructor.
data ListRunTasks = ListRunTasks'
  { -- | The maximum number of run tasks to return in one page of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Specify the pagination token from a previous request to retrieve the
    -- next page of results.
    startingToken :: Prelude.Maybe Prelude.Text,
    -- | Filter the list by status.
    status :: Prelude.Maybe TaskStatus,
    -- | The run\'s ID.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRunTasks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listRunTasks_maxResults' - The maximum number of run tasks to return in one page of results.
--
-- 'startingToken', 'listRunTasks_startingToken' - Specify the pagination token from a previous request to retrieve the
-- next page of results.
--
-- 'status', 'listRunTasks_status' - Filter the list by status.
--
-- 'id', 'listRunTasks_id' - The run\'s ID.
newListRunTasks ::
  -- | 'id'
  Prelude.Text ->
  ListRunTasks
newListRunTasks pId_ =
  ListRunTasks'
    { maxResults = Prelude.Nothing,
      startingToken = Prelude.Nothing,
      status = Prelude.Nothing,
      id = pId_
    }

-- | The maximum number of run tasks to return in one page of results.
listRunTasks_maxResults :: Lens.Lens' ListRunTasks (Prelude.Maybe Prelude.Natural)
listRunTasks_maxResults = Lens.lens (\ListRunTasks' {maxResults} -> maxResults) (\s@ListRunTasks' {} a -> s {maxResults = a} :: ListRunTasks)

-- | Specify the pagination token from a previous request to retrieve the
-- next page of results.
listRunTasks_startingToken :: Lens.Lens' ListRunTasks (Prelude.Maybe Prelude.Text)
listRunTasks_startingToken = Lens.lens (\ListRunTasks' {startingToken} -> startingToken) (\s@ListRunTasks' {} a -> s {startingToken = a} :: ListRunTasks)

-- | Filter the list by status.
listRunTasks_status :: Lens.Lens' ListRunTasks (Prelude.Maybe TaskStatus)
listRunTasks_status = Lens.lens (\ListRunTasks' {status} -> status) (\s@ListRunTasks' {} a -> s {status = a} :: ListRunTasks)

-- | The run\'s ID.
listRunTasks_id :: Lens.Lens' ListRunTasks Prelude.Text
listRunTasks_id = Lens.lens (\ListRunTasks' {id} -> id) (\s@ListRunTasks' {} a -> s {id = a} :: ListRunTasks)

instance Core.AWSPager ListRunTasks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRunTasksResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listRunTasksResponse_items
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listRunTasks_startingToken
          Lens..~ rs
          Lens.^? listRunTasksResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListRunTasks where
  type AWSResponse ListRunTasks = ListRunTasksResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRunTasksResponse'
            Prelude.<$> (x Data..?> "items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRunTasks where
  hashWithSalt _salt ListRunTasks' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` startingToken
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` id

instance Prelude.NFData ListRunTasks where
  rnf ListRunTasks' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf startingToken
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders ListRunTasks where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListRunTasks where
  toPath ListRunTasks' {..} =
    Prelude.mconcat ["/run/", Data.toBS id, "/task"]

instance Data.ToQuery ListRunTasks where
  toQuery ListRunTasks' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "startingToken" Data.=: startingToken,
        "status" Data.=: status
      ]

-- | /See:/ 'newListRunTasksResponse' smart constructor.
data ListRunTasksResponse = ListRunTasksResponse'
  { -- | A list of tasks.
    items :: Prelude.Maybe [TaskListItem],
    -- | A pagination token that\'s included if more results are available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRunTasksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'listRunTasksResponse_items' - A list of tasks.
--
-- 'nextToken', 'listRunTasksResponse_nextToken' - A pagination token that\'s included if more results are available.
--
-- 'httpStatus', 'listRunTasksResponse_httpStatus' - The response's http status code.
newListRunTasksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRunTasksResponse
newListRunTasksResponse pHttpStatus_ =
  ListRunTasksResponse'
    { items = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of tasks.
listRunTasksResponse_items :: Lens.Lens' ListRunTasksResponse (Prelude.Maybe [TaskListItem])
listRunTasksResponse_items = Lens.lens (\ListRunTasksResponse' {items} -> items) (\s@ListRunTasksResponse' {} a -> s {items = a} :: ListRunTasksResponse) Prelude.. Lens.mapping Lens.coerced

-- | A pagination token that\'s included if more results are available.
listRunTasksResponse_nextToken :: Lens.Lens' ListRunTasksResponse (Prelude.Maybe Prelude.Text)
listRunTasksResponse_nextToken = Lens.lens (\ListRunTasksResponse' {nextToken} -> nextToken) (\s@ListRunTasksResponse' {} a -> s {nextToken = a} :: ListRunTasksResponse)

-- | The response's http status code.
listRunTasksResponse_httpStatus :: Lens.Lens' ListRunTasksResponse Prelude.Int
listRunTasksResponse_httpStatus = Lens.lens (\ListRunTasksResponse' {httpStatus} -> httpStatus) (\s@ListRunTasksResponse' {} a -> s {httpStatus = a} :: ListRunTasksResponse)

instance Prelude.NFData ListRunTasksResponse where
  rnf ListRunTasksResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
