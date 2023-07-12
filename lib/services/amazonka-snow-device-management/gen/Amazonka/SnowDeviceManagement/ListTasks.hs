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
-- Module      : Amazonka.SnowDeviceManagement.ListTasks
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of tasks that can be filtered by state.
--
-- This operation returns paginated results.
module Amazonka.SnowDeviceManagement.ListTasks
  ( -- * Creating a Request
    ListTasks (..),
    newListTasks,

    -- * Request Lenses
    listTasks_maxResults,
    listTasks_nextToken,
    listTasks_state,

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
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SnowDeviceManagement.Types

-- | /See:/ 'newListTasks' smart constructor.
data ListTasks = ListTasks'
  { -- | The maximum number of tasks per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A pagination token to continue to the next page of tasks.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A structure used to filter the list of tasks.
    state :: Prelude.Maybe TaskState
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
-- 'maxResults', 'listTasks_maxResults' - The maximum number of tasks per page.
--
-- 'nextToken', 'listTasks_nextToken' - A pagination token to continue to the next page of tasks.
--
-- 'state', 'listTasks_state' - A structure used to filter the list of tasks.
newListTasks ::
  ListTasks
newListTasks =
  ListTasks'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      state = Prelude.Nothing
    }

-- | The maximum number of tasks per page.
listTasks_maxResults :: Lens.Lens' ListTasks (Prelude.Maybe Prelude.Natural)
listTasks_maxResults = Lens.lens (\ListTasks' {maxResults} -> maxResults) (\s@ListTasks' {} a -> s {maxResults = a} :: ListTasks)

-- | A pagination token to continue to the next page of tasks.
listTasks_nextToken :: Lens.Lens' ListTasks (Prelude.Maybe Prelude.Text)
listTasks_nextToken = Lens.lens (\ListTasks' {nextToken} -> nextToken) (\s@ListTasks' {} a -> s {nextToken = a} :: ListTasks)

-- | A structure used to filter the list of tasks.
listTasks_state :: Lens.Lens' ListTasks (Prelude.Maybe TaskState)
listTasks_state = Lens.lens (\ListTasks' {state} -> state) (\s@ListTasks' {} a -> s {state = a} :: ListTasks)

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
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTasksResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "tasks" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTasks where
  hashWithSalt _salt ListTasks' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` state

instance Prelude.NFData ListTasks where
  rnf ListTasks' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf state

instance Data.ToHeaders ListTasks where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListTasks where
  toPath = Prelude.const "/tasks"

instance Data.ToQuery ListTasks where
  toQuery ListTasks' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "state" Data.=: state
      ]

-- | /See:/ 'newListTasksResponse' smart constructor.
data ListTasksResponse = ListTasksResponse'
  { -- | A pagination token to continue to the next page of tasks.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of task structures containing details about each task.
    tasks :: Prelude.Maybe [TaskSummary],
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
-- 'nextToken', 'listTasksResponse_nextToken' - A pagination token to continue to the next page of tasks.
--
-- 'tasks', 'listTasksResponse_tasks' - A list of task structures containing details about each task.
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

-- | A pagination token to continue to the next page of tasks.
listTasksResponse_nextToken :: Lens.Lens' ListTasksResponse (Prelude.Maybe Prelude.Text)
listTasksResponse_nextToken = Lens.lens (\ListTasksResponse' {nextToken} -> nextToken) (\s@ListTasksResponse' {} a -> s {nextToken = a} :: ListTasksResponse)

-- | A list of task structures containing details about each task.
listTasksResponse_tasks :: Lens.Lens' ListTasksResponse (Prelude.Maybe [TaskSummary])
listTasksResponse_tasks = Lens.lens (\ListTasksResponse' {tasks} -> tasks) (\s@ListTasksResponse' {} a -> s {tasks = a} :: ListTasksResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTasksResponse_httpStatus :: Lens.Lens' ListTasksResponse Prelude.Int
listTasksResponse_httpStatus = Lens.lens (\ListTasksResponse' {httpStatus} -> httpStatus) (\s@ListTasksResponse' {} a -> s {httpStatus = a} :: ListTasksResponse)

instance Prelude.NFData ListTasksResponse where
  rnf ListTasksResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf tasks
      `Prelude.seq` Prelude.rnf httpStatus
