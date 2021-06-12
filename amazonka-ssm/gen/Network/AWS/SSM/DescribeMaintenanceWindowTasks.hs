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
-- Module      : Network.AWS.SSM.DescribeMaintenanceWindowTasks
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tasks in a maintenance window.
--
-- For maintenance window tasks without a specified target, you cannot
-- supply values for @--max-errors@ and @--max-concurrency@. Instead, the
-- system inserts a placeholder value of @1@, which may be reported in the
-- response to this command. These values do not affect the running of your
-- task and can be ignored.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeMaintenanceWindowTasks
  ( -- * Creating a Request
    DescribeMaintenanceWindowTasks (..),
    newDescribeMaintenanceWindowTasks,

    -- * Request Lenses
    describeMaintenanceWindowTasks_nextToken,
    describeMaintenanceWindowTasks_maxResults,
    describeMaintenanceWindowTasks_filters,
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newDescribeMaintenanceWindowTasks' smart constructor.
data DescribeMaintenanceWindowTasks = DescribeMaintenanceWindowTasks'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | Optional filters used to narrow down the scope of the returned tasks.
    -- The supported filter keys are WindowTaskId, TaskArn, Priority, and
    -- TaskType.
    filters :: Core.Maybe [MaintenanceWindowFilter],
    -- | The ID of the maintenance window whose tasks should be retrieved.
    windowId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeMaintenanceWindowTasks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeMaintenanceWindowTasks_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'maxResults', 'describeMaintenanceWindowTasks_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'filters', 'describeMaintenanceWindowTasks_filters' - Optional filters used to narrow down the scope of the returned tasks.
-- The supported filter keys are WindowTaskId, TaskArn, Priority, and
-- TaskType.
--
-- 'windowId', 'describeMaintenanceWindowTasks_windowId' - The ID of the maintenance window whose tasks should be retrieved.
newDescribeMaintenanceWindowTasks ::
  -- | 'windowId'
  Core.Text ->
  DescribeMaintenanceWindowTasks
newDescribeMaintenanceWindowTasks pWindowId_ =
  DescribeMaintenanceWindowTasks'
    { nextToken =
        Core.Nothing,
      maxResults = Core.Nothing,
      filters = Core.Nothing,
      windowId = pWindowId_
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeMaintenanceWindowTasks_nextToken :: Lens.Lens' DescribeMaintenanceWindowTasks (Core.Maybe Core.Text)
describeMaintenanceWindowTasks_nextToken = Lens.lens (\DescribeMaintenanceWindowTasks' {nextToken} -> nextToken) (\s@DescribeMaintenanceWindowTasks' {} a -> s {nextToken = a} :: DescribeMaintenanceWindowTasks)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
describeMaintenanceWindowTasks_maxResults :: Lens.Lens' DescribeMaintenanceWindowTasks (Core.Maybe Core.Natural)
describeMaintenanceWindowTasks_maxResults = Lens.lens (\DescribeMaintenanceWindowTasks' {maxResults} -> maxResults) (\s@DescribeMaintenanceWindowTasks' {} a -> s {maxResults = a} :: DescribeMaintenanceWindowTasks)

-- | Optional filters used to narrow down the scope of the returned tasks.
-- The supported filter keys are WindowTaskId, TaskArn, Priority, and
-- TaskType.
describeMaintenanceWindowTasks_filters :: Lens.Lens' DescribeMaintenanceWindowTasks (Core.Maybe [MaintenanceWindowFilter])
describeMaintenanceWindowTasks_filters = Lens.lens (\DescribeMaintenanceWindowTasks' {filters} -> filters) (\s@DescribeMaintenanceWindowTasks' {} a -> s {filters = a} :: DescribeMaintenanceWindowTasks) Core.. Lens.mapping Lens._Coerce

-- | The ID of the maintenance window whose tasks should be retrieved.
describeMaintenanceWindowTasks_windowId :: Lens.Lens' DescribeMaintenanceWindowTasks Core.Text
describeMaintenanceWindowTasks_windowId = Lens.lens (\DescribeMaintenanceWindowTasks' {windowId} -> windowId) (\s@DescribeMaintenanceWindowTasks' {} a -> s {windowId = a} :: DescribeMaintenanceWindowTasks)

instance Core.AWSPager DescribeMaintenanceWindowTasks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeMaintenanceWindowTasksResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeMaintenanceWindowTasksResponse_tasks
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeMaintenanceWindowTasks_nextToken
          Lens..~ rs
          Lens.^? describeMaintenanceWindowTasksResponse_nextToken
            Core.. Lens._Just

instance
  Core.AWSRequest
    DescribeMaintenanceWindowTasks
  where
  type
    AWSResponse DescribeMaintenanceWindowTasks =
      DescribeMaintenanceWindowTasksResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMaintenanceWindowTasksResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Tasks" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeMaintenanceWindowTasks

instance Core.NFData DescribeMaintenanceWindowTasks

instance
  Core.ToHeaders
    DescribeMaintenanceWindowTasks
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.DescribeMaintenanceWindowTasks" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeMaintenanceWindowTasks where
  toJSON DescribeMaintenanceWindowTasks' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("Filters" Core..=) Core.<$> filters,
            Core.Just ("WindowId" Core..= windowId)
          ]
      )

instance Core.ToPath DescribeMaintenanceWindowTasks where
  toPath = Core.const "/"

instance Core.ToQuery DescribeMaintenanceWindowTasks where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeMaintenanceWindowTasksResponse' smart constructor.
data DescribeMaintenanceWindowTasksResponse = DescribeMaintenanceWindowTasksResponse'
  { -- | The token to use when requesting the next set of items. If there are no
    -- additional items to return, the string is empty.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the tasks in the maintenance window.
    tasks :: Core.Maybe [MaintenanceWindowTask],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeMaintenanceWindowTasksResponse
newDescribeMaintenanceWindowTasksResponse
  pHttpStatus_ =
    DescribeMaintenanceWindowTasksResponse'
      { nextToken =
          Core.Nothing,
        tasks = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
describeMaintenanceWindowTasksResponse_nextToken :: Lens.Lens' DescribeMaintenanceWindowTasksResponse (Core.Maybe Core.Text)
describeMaintenanceWindowTasksResponse_nextToken = Lens.lens (\DescribeMaintenanceWindowTasksResponse' {nextToken} -> nextToken) (\s@DescribeMaintenanceWindowTasksResponse' {} a -> s {nextToken = a} :: DescribeMaintenanceWindowTasksResponse)

-- | Information about the tasks in the maintenance window.
describeMaintenanceWindowTasksResponse_tasks :: Lens.Lens' DescribeMaintenanceWindowTasksResponse (Core.Maybe [MaintenanceWindowTask])
describeMaintenanceWindowTasksResponse_tasks = Lens.lens (\DescribeMaintenanceWindowTasksResponse' {tasks} -> tasks) (\s@DescribeMaintenanceWindowTasksResponse' {} a -> s {tasks = a} :: DescribeMaintenanceWindowTasksResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeMaintenanceWindowTasksResponse_httpStatus :: Lens.Lens' DescribeMaintenanceWindowTasksResponse Core.Int
describeMaintenanceWindowTasksResponse_httpStatus = Lens.lens (\DescribeMaintenanceWindowTasksResponse' {httpStatus} -> httpStatus) (\s@DescribeMaintenanceWindowTasksResponse' {} a -> s {httpStatus = a} :: DescribeMaintenanceWindowTasksResponse)

instance
  Core.NFData
    DescribeMaintenanceWindowTasksResponse
