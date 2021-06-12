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
-- Module      : Network.AWS.SSM.DescribeMaintenanceWindowExecutionTasks
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For a given maintenance window execution, lists the tasks that were run.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeMaintenanceWindowExecutionTasks
  ( -- * Creating a Request
    DescribeMaintenanceWindowExecutionTasks (..),
    newDescribeMaintenanceWindowExecutionTasks,

    -- * Request Lenses
    describeMaintenanceWindowExecutionTasks_nextToken,
    describeMaintenanceWindowExecutionTasks_maxResults,
    describeMaintenanceWindowExecutionTasks_filters,
    describeMaintenanceWindowExecutionTasks_windowExecutionId,

    -- * Destructuring the Response
    DescribeMaintenanceWindowExecutionTasksResponse (..),
    newDescribeMaintenanceWindowExecutionTasksResponse,

    -- * Response Lenses
    describeMaintenanceWindowExecutionTasksResponse_nextToken,
    describeMaintenanceWindowExecutionTasksResponse_windowExecutionTaskIdentities,
    describeMaintenanceWindowExecutionTasksResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newDescribeMaintenanceWindowExecutionTasks' smart constructor.
data DescribeMaintenanceWindowExecutionTasks = DescribeMaintenanceWindowExecutionTasks'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | Optional filters used to scope down the returned tasks. The supported
    -- filter key is STATUS with the corresponding values PENDING, IN_PROGRESS,
    -- SUCCESS, FAILED, TIMED_OUT, CANCELLING, and CANCELLED.
    filters :: Core.Maybe [MaintenanceWindowFilter],
    -- | The ID of the maintenance window execution whose task executions should
    -- be retrieved.
    windowExecutionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeMaintenanceWindowExecutionTasks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeMaintenanceWindowExecutionTasks_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'maxResults', 'describeMaintenanceWindowExecutionTasks_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'filters', 'describeMaintenanceWindowExecutionTasks_filters' - Optional filters used to scope down the returned tasks. The supported
-- filter key is STATUS with the corresponding values PENDING, IN_PROGRESS,
-- SUCCESS, FAILED, TIMED_OUT, CANCELLING, and CANCELLED.
--
-- 'windowExecutionId', 'describeMaintenanceWindowExecutionTasks_windowExecutionId' - The ID of the maintenance window execution whose task executions should
-- be retrieved.
newDescribeMaintenanceWindowExecutionTasks ::
  -- | 'windowExecutionId'
  Core.Text ->
  DescribeMaintenanceWindowExecutionTasks
newDescribeMaintenanceWindowExecutionTasks
  pWindowExecutionId_ =
    DescribeMaintenanceWindowExecutionTasks'
      { nextToken =
          Core.Nothing,
        maxResults = Core.Nothing,
        filters = Core.Nothing,
        windowExecutionId =
          pWindowExecutionId_
      }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeMaintenanceWindowExecutionTasks_nextToken :: Lens.Lens' DescribeMaintenanceWindowExecutionTasks (Core.Maybe Core.Text)
describeMaintenanceWindowExecutionTasks_nextToken = Lens.lens (\DescribeMaintenanceWindowExecutionTasks' {nextToken} -> nextToken) (\s@DescribeMaintenanceWindowExecutionTasks' {} a -> s {nextToken = a} :: DescribeMaintenanceWindowExecutionTasks)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
describeMaintenanceWindowExecutionTasks_maxResults :: Lens.Lens' DescribeMaintenanceWindowExecutionTasks (Core.Maybe Core.Natural)
describeMaintenanceWindowExecutionTasks_maxResults = Lens.lens (\DescribeMaintenanceWindowExecutionTasks' {maxResults} -> maxResults) (\s@DescribeMaintenanceWindowExecutionTasks' {} a -> s {maxResults = a} :: DescribeMaintenanceWindowExecutionTasks)

-- | Optional filters used to scope down the returned tasks. The supported
-- filter key is STATUS with the corresponding values PENDING, IN_PROGRESS,
-- SUCCESS, FAILED, TIMED_OUT, CANCELLING, and CANCELLED.
describeMaintenanceWindowExecutionTasks_filters :: Lens.Lens' DescribeMaintenanceWindowExecutionTasks (Core.Maybe [MaintenanceWindowFilter])
describeMaintenanceWindowExecutionTasks_filters = Lens.lens (\DescribeMaintenanceWindowExecutionTasks' {filters} -> filters) (\s@DescribeMaintenanceWindowExecutionTasks' {} a -> s {filters = a} :: DescribeMaintenanceWindowExecutionTasks) Core.. Lens.mapping Lens._Coerce

-- | The ID of the maintenance window execution whose task executions should
-- be retrieved.
describeMaintenanceWindowExecutionTasks_windowExecutionId :: Lens.Lens' DescribeMaintenanceWindowExecutionTasks Core.Text
describeMaintenanceWindowExecutionTasks_windowExecutionId = Lens.lens (\DescribeMaintenanceWindowExecutionTasks' {windowExecutionId} -> windowExecutionId) (\s@DescribeMaintenanceWindowExecutionTasks' {} a -> s {windowExecutionId = a} :: DescribeMaintenanceWindowExecutionTasks)

instance
  Core.AWSPager
    DescribeMaintenanceWindowExecutionTasks
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeMaintenanceWindowExecutionTasksResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeMaintenanceWindowExecutionTasksResponse_windowExecutionTaskIdentities
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeMaintenanceWindowExecutionTasks_nextToken
          Lens..~ rs
            Lens.^? describeMaintenanceWindowExecutionTasksResponse_nextToken
              Core.. Lens._Just

instance
  Core.AWSRequest
    DescribeMaintenanceWindowExecutionTasks
  where
  type
    AWSResponse
      DescribeMaintenanceWindowExecutionTasks =
      DescribeMaintenanceWindowExecutionTasksResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMaintenanceWindowExecutionTasksResponse'
            Core.<$> (x Core..?> "NextToken")
              Core.<*> ( x Core..?> "WindowExecutionTaskIdentities"
                           Core..!@ Core.mempty
                       )
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeMaintenanceWindowExecutionTasks

instance
  Core.NFData
    DescribeMaintenanceWindowExecutionTasks

instance
  Core.ToHeaders
    DescribeMaintenanceWindowExecutionTasks
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.DescribeMaintenanceWindowExecutionTasks" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    DescribeMaintenanceWindowExecutionTasks
  where
  toJSON DescribeMaintenanceWindowExecutionTasks' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("Filters" Core..=) Core.<$> filters,
            Core.Just
              ("WindowExecutionId" Core..= windowExecutionId)
          ]
      )

instance
  Core.ToPath
    DescribeMaintenanceWindowExecutionTasks
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeMaintenanceWindowExecutionTasks
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeMaintenanceWindowExecutionTasksResponse' smart constructor.
data DescribeMaintenanceWindowExecutionTasksResponse = DescribeMaintenanceWindowExecutionTasksResponse'
  { -- | The token to use when requesting the next set of items. If there are no
    -- additional items to return, the string is empty.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the task executions.
    windowExecutionTaskIdentities :: Core.Maybe [MaintenanceWindowExecutionTaskIdentity],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeMaintenanceWindowExecutionTasksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeMaintenanceWindowExecutionTasksResponse_nextToken' - The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
--
-- 'windowExecutionTaskIdentities', 'describeMaintenanceWindowExecutionTasksResponse_windowExecutionTaskIdentities' - Information about the task executions.
--
-- 'httpStatus', 'describeMaintenanceWindowExecutionTasksResponse_httpStatus' - The response's http status code.
newDescribeMaintenanceWindowExecutionTasksResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeMaintenanceWindowExecutionTasksResponse
newDescribeMaintenanceWindowExecutionTasksResponse
  pHttpStatus_ =
    DescribeMaintenanceWindowExecutionTasksResponse'
      { nextToken =
          Core.Nothing,
        windowExecutionTaskIdentities =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
describeMaintenanceWindowExecutionTasksResponse_nextToken :: Lens.Lens' DescribeMaintenanceWindowExecutionTasksResponse (Core.Maybe Core.Text)
describeMaintenanceWindowExecutionTasksResponse_nextToken = Lens.lens (\DescribeMaintenanceWindowExecutionTasksResponse' {nextToken} -> nextToken) (\s@DescribeMaintenanceWindowExecutionTasksResponse' {} a -> s {nextToken = a} :: DescribeMaintenanceWindowExecutionTasksResponse)

-- | Information about the task executions.
describeMaintenanceWindowExecutionTasksResponse_windowExecutionTaskIdentities :: Lens.Lens' DescribeMaintenanceWindowExecutionTasksResponse (Core.Maybe [MaintenanceWindowExecutionTaskIdentity])
describeMaintenanceWindowExecutionTasksResponse_windowExecutionTaskIdentities = Lens.lens (\DescribeMaintenanceWindowExecutionTasksResponse' {windowExecutionTaskIdentities} -> windowExecutionTaskIdentities) (\s@DescribeMaintenanceWindowExecutionTasksResponse' {} a -> s {windowExecutionTaskIdentities = a} :: DescribeMaintenanceWindowExecutionTasksResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeMaintenanceWindowExecutionTasksResponse_httpStatus :: Lens.Lens' DescribeMaintenanceWindowExecutionTasksResponse Core.Int
describeMaintenanceWindowExecutionTasksResponse_httpStatus = Lens.lens (\DescribeMaintenanceWindowExecutionTasksResponse' {httpStatus} -> httpStatus) (\s@DescribeMaintenanceWindowExecutionTasksResponse' {} a -> s {httpStatus = a} :: DescribeMaintenanceWindowExecutionTasksResponse)

instance
  Core.NFData
    DescribeMaintenanceWindowExecutionTasksResponse
