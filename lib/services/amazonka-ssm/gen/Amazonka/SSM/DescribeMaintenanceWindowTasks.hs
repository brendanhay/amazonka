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
-- Module      : Amazonka.SSM.DescribeMaintenanceWindowTasks
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tasks in a maintenance window.
--
-- For maintenance window tasks without a specified target, you can\'t
-- supply values for @--max-errors@ and @--max-concurrency@. Instead, the
-- system inserts a placeholder value of @1@, which may be reported in the
-- response to this command. These values don\'t affect the running of your
-- task and can be ignored.
--
-- This operation returns paginated results.
module Amazonka.SSM.DescribeMaintenanceWindowTasks
  ( -- * Creating a Request
    DescribeMaintenanceWindowTasks (..),
    newDescribeMaintenanceWindowTasks,

    -- * Request Lenses
    describeMaintenanceWindowTasks_filters,
    describeMaintenanceWindowTasks_maxResults,
    describeMaintenanceWindowTasks_nextToken,
    describeMaintenanceWindowTasks_windowId,

    -- * Destructuring the Response
    DescribeMaintenanceWindowTasksResponse (..),
    newDescribeMaintenanceWindowTasksResponse,

    -- * Response Lenses
    describeMaintenanceWindowTasksResponse_nextToken,
    describeMaintenanceWindowTasksResponse_tasks,
    describeMaintenanceWindowTasksResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newDescribeMaintenanceWindowTasks' smart constructor.
data DescribeMaintenanceWindowTasks = DescribeMaintenanceWindowTasks'
  { -- | Optional filters used to narrow down the scope of the returned tasks.
    -- The supported filter keys are @WindowTaskId@, @TaskArn@, @Priority@, and
    -- @TaskType@.
    filters :: Prelude.Maybe [MaintenanceWindowFilter],
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the maintenance window whose tasks should be retrieved.
    windowId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeMaintenanceWindowTasks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'describeMaintenanceWindowTasks_filters' - Optional filters used to narrow down the scope of the returned tasks.
-- The supported filter keys are @WindowTaskId@, @TaskArn@, @Priority@, and
-- @TaskType@.
--
-- 'maxResults', 'describeMaintenanceWindowTasks_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'nextToken', 'describeMaintenanceWindowTasks_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'windowId', 'describeMaintenanceWindowTasks_windowId' - The ID of the maintenance window whose tasks should be retrieved.
newDescribeMaintenanceWindowTasks ::
  -- | 'windowId'
  Prelude.Text ->
  DescribeMaintenanceWindowTasks
newDescribeMaintenanceWindowTasks pWindowId_ =
  DescribeMaintenanceWindowTasks'
    { filters =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      windowId = pWindowId_
    }

-- | Optional filters used to narrow down the scope of the returned tasks.
-- The supported filter keys are @WindowTaskId@, @TaskArn@, @Priority@, and
-- @TaskType@.
describeMaintenanceWindowTasks_filters :: Lens.Lens' DescribeMaintenanceWindowTasks (Prelude.Maybe [MaintenanceWindowFilter])
describeMaintenanceWindowTasks_filters = Lens.lens (\DescribeMaintenanceWindowTasks' {filters} -> filters) (\s@DescribeMaintenanceWindowTasks' {} a -> s {filters = a} :: DescribeMaintenanceWindowTasks) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
describeMaintenanceWindowTasks_maxResults :: Lens.Lens' DescribeMaintenanceWindowTasks (Prelude.Maybe Prelude.Natural)
describeMaintenanceWindowTasks_maxResults = Lens.lens (\DescribeMaintenanceWindowTasks' {maxResults} -> maxResults) (\s@DescribeMaintenanceWindowTasks' {} a -> s {maxResults = a} :: DescribeMaintenanceWindowTasks)

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeMaintenanceWindowTasks_nextToken :: Lens.Lens' DescribeMaintenanceWindowTasks (Prelude.Maybe Prelude.Text)
describeMaintenanceWindowTasks_nextToken = Lens.lens (\DescribeMaintenanceWindowTasks' {nextToken} -> nextToken) (\s@DescribeMaintenanceWindowTasks' {} a -> s {nextToken = a} :: DescribeMaintenanceWindowTasks)

-- | The ID of the maintenance window whose tasks should be retrieved.
describeMaintenanceWindowTasks_windowId :: Lens.Lens' DescribeMaintenanceWindowTasks Prelude.Text
describeMaintenanceWindowTasks_windowId = Lens.lens (\DescribeMaintenanceWindowTasks' {windowId} -> windowId) (\s@DescribeMaintenanceWindowTasks' {} a -> s {windowId = a} :: DescribeMaintenanceWindowTasks)

instance Core.AWSPager DescribeMaintenanceWindowTasks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeMaintenanceWindowTasksResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeMaintenanceWindowTasksResponse_tasks
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeMaintenanceWindowTasks_nextToken
          Lens..~ rs
          Lens.^? describeMaintenanceWindowTasksResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeMaintenanceWindowTasks
  where
  type
    AWSResponse DescribeMaintenanceWindowTasks =
      DescribeMaintenanceWindowTasksResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMaintenanceWindowTasksResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Tasks" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeMaintenanceWindowTasks
  where
  hashWithSalt
    _salt
    DescribeMaintenanceWindowTasks' {..} =
      _salt
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` windowId

instance
  Prelude.NFData
    DescribeMaintenanceWindowTasks
  where
  rnf DescribeMaintenanceWindowTasks' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf windowId

instance
  Data.ToHeaders
    DescribeMaintenanceWindowTasks
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.DescribeMaintenanceWindowTasks" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeMaintenanceWindowTasks where
  toJSON DescribeMaintenanceWindowTasks' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("WindowId" Data..= windowId)
          ]
      )

instance Data.ToPath DescribeMaintenanceWindowTasks where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeMaintenanceWindowTasks where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeMaintenanceWindowTasksResponse' smart constructor.
data DescribeMaintenanceWindowTasksResponse = DescribeMaintenanceWindowTasksResponse'
  { -- | The token to use when requesting the next set of items. If there are no
    -- additional items to return, the string is empty.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the tasks in the maintenance window.
    tasks :: Prelude.Maybe [MaintenanceWindowTask],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeMaintenanceWindowTasksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeMaintenanceWindowTasksResponse_nextToken' - The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
--
-- 'tasks', 'describeMaintenanceWindowTasksResponse_tasks' - Information about the tasks in the maintenance window.
--
-- 'httpStatus', 'describeMaintenanceWindowTasksResponse_httpStatus' - The response's http status code.
newDescribeMaintenanceWindowTasksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeMaintenanceWindowTasksResponse
newDescribeMaintenanceWindowTasksResponse
  pHttpStatus_ =
    DescribeMaintenanceWindowTasksResponse'
      { nextToken =
          Prelude.Nothing,
        tasks = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
describeMaintenanceWindowTasksResponse_nextToken :: Lens.Lens' DescribeMaintenanceWindowTasksResponse (Prelude.Maybe Prelude.Text)
describeMaintenanceWindowTasksResponse_nextToken = Lens.lens (\DescribeMaintenanceWindowTasksResponse' {nextToken} -> nextToken) (\s@DescribeMaintenanceWindowTasksResponse' {} a -> s {nextToken = a} :: DescribeMaintenanceWindowTasksResponse)

-- | Information about the tasks in the maintenance window.
describeMaintenanceWindowTasksResponse_tasks :: Lens.Lens' DescribeMaintenanceWindowTasksResponse (Prelude.Maybe [MaintenanceWindowTask])
describeMaintenanceWindowTasksResponse_tasks = Lens.lens (\DescribeMaintenanceWindowTasksResponse' {tasks} -> tasks) (\s@DescribeMaintenanceWindowTasksResponse' {} a -> s {tasks = a} :: DescribeMaintenanceWindowTasksResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeMaintenanceWindowTasksResponse_httpStatus :: Lens.Lens' DescribeMaintenanceWindowTasksResponse Prelude.Int
describeMaintenanceWindowTasksResponse_httpStatus = Lens.lens (\DescribeMaintenanceWindowTasksResponse' {httpStatus} -> httpStatus) (\s@DescribeMaintenanceWindowTasksResponse' {} a -> s {httpStatus = a} :: DescribeMaintenanceWindowTasksResponse)

instance
  Prelude.NFData
    DescribeMaintenanceWindowTasksResponse
  where
  rnf DescribeMaintenanceWindowTasksResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf tasks
      `Prelude.seq` Prelude.rnf httpStatus
