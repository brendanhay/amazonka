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
-- Module      : Amazonka.DataSync.ListTasks
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the DataSync tasks you created.
--
-- This operation returns paginated results.
module Amazonka.DataSync.ListTasks
  ( -- * Creating a Request
    ListTasks (..),
    newListTasks,

    -- * Request Lenses
    listTasks_filters,
    listTasks_maxResults,
    listTasks_nextToken,

    -- * Destructuring the Response
    ListTasksResponse (..),
    newListTasksResponse,

    -- * Response Lenses
    listTasksResponse_nextToken,
    listTasksResponse_tasks,
    listTasksResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | ListTasksRequest
--
-- /See:/ 'newListTasks' smart constructor.
data ListTasks = ListTasks'
  { -- | You can use API filters to narrow down the list of resources returned by
    -- @ListTasks@. For example, to retrieve all tasks on a specific source
    -- location, you can use @ListTasks@ with filter name @LocationId@ and
    -- @Operator Equals@ with the ARN for the location.
    filters :: Prelude.Maybe [TaskFilter],
    -- | The maximum number of tasks to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | An opaque string that indicates the position at which to begin the next
    -- list of tasks.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTasks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listTasks_filters' - You can use API filters to narrow down the list of resources returned by
-- @ListTasks@. For example, to retrieve all tasks on a specific source
-- location, you can use @ListTasks@ with filter name @LocationId@ and
-- @Operator Equals@ with the ARN for the location.
--
-- 'maxResults', 'listTasks_maxResults' - The maximum number of tasks to return.
--
-- 'nextToken', 'listTasks_nextToken' - An opaque string that indicates the position at which to begin the next
-- list of tasks.
newListTasks ::
  ListTasks
newListTasks =
  ListTasks'
    { filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | You can use API filters to narrow down the list of resources returned by
-- @ListTasks@. For example, to retrieve all tasks on a specific source
-- location, you can use @ListTasks@ with filter name @LocationId@ and
-- @Operator Equals@ with the ARN for the location.
listTasks_filters :: Lens.Lens' ListTasks (Prelude.Maybe [TaskFilter])
listTasks_filters = Lens.lens (\ListTasks' {filters} -> filters) (\s@ListTasks' {} a -> s {filters = a} :: ListTasks) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of tasks to return.
listTasks_maxResults :: Lens.Lens' ListTasks (Prelude.Maybe Prelude.Natural)
listTasks_maxResults = Lens.lens (\ListTasks' {maxResults} -> maxResults) (\s@ListTasks' {} a -> s {maxResults = a} :: ListTasks)

-- | An opaque string that indicates the position at which to begin the next
-- list of tasks.
listTasks_nextToken :: Lens.Lens' ListTasks (Prelude.Maybe Prelude.Text)
listTasks_nextToken = Lens.lens (\ListTasks' {nextToken} -> nextToken) (\s@ListTasks' {} a -> s {nextToken = a} :: ListTasks)

instance Core.AWSPager ListTasks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTasksResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listTasksResponse_tasks
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listTasks_nextToken
          Lens..~ rs
          Lens.^? listTasksResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListTasks where
  type AWSResponse ListTasks = ListTasksResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTasksResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Tasks" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTasks where
  hashWithSalt _salt ListTasks' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListTasks where
  rnf ListTasks' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListTasks where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("FmrsService.ListTasks" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListTasks where
  toJSON ListTasks' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListTasks where
  toPath = Prelude.const "/"

instance Data.ToQuery ListTasks where
  toQuery = Prelude.const Prelude.mempty

-- | ListTasksResponse
--
-- /See:/ 'newListTasksResponse' smart constructor.
data ListTasksResponse = ListTasksResponse'
  { -- | An opaque string that indicates the position at which to begin returning
    -- the next list of tasks.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of all the tasks that are returned.
    tasks :: Prelude.Maybe [TaskListEntry],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTasksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTasksResponse_nextToken' - An opaque string that indicates the position at which to begin returning
-- the next list of tasks.
--
-- 'tasks', 'listTasksResponse_tasks' - A list of all the tasks that are returned.
--
-- 'httpStatus', 'listTasksResponse_httpStatus' - The response's http status code.
newListTasksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTasksResponse
newListTasksResponse pHttpStatus_ =
  ListTasksResponse'
    { nextToken = Prelude.Nothing,
      tasks = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An opaque string that indicates the position at which to begin returning
-- the next list of tasks.
listTasksResponse_nextToken :: Lens.Lens' ListTasksResponse (Prelude.Maybe Prelude.Text)
listTasksResponse_nextToken = Lens.lens (\ListTasksResponse' {nextToken} -> nextToken) (\s@ListTasksResponse' {} a -> s {nextToken = a} :: ListTasksResponse)

-- | A list of all the tasks that are returned.
listTasksResponse_tasks :: Lens.Lens' ListTasksResponse (Prelude.Maybe [TaskListEntry])
listTasksResponse_tasks = Lens.lens (\ListTasksResponse' {tasks} -> tasks) (\s@ListTasksResponse' {} a -> s {tasks = a} :: ListTasksResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTasksResponse_httpStatus :: Lens.Lens' ListTasksResponse Prelude.Int
listTasksResponse_httpStatus = Lens.lens (\ListTasksResponse' {httpStatus} -> httpStatus) (\s@ListTasksResponse' {} a -> s {httpStatus = a} :: ListTasksResponse)

instance Prelude.NFData ListTasksResponse where
  rnf ListTasksResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf tasks
      `Prelude.seq` Prelude.rnf httpStatus
