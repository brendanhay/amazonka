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
-- Module      : Network.AWS.SSM.DescribeMaintenanceWindowExecutionTaskInvocations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the individual task executions (one per target) for a
-- particular task run as part of a maintenance window execution.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeMaintenanceWindowExecutionTaskInvocations
  ( -- * Creating a Request
    DescribeMaintenanceWindowExecutionTaskInvocations (..),
    newDescribeMaintenanceWindowExecutionTaskInvocations,

    -- * Request Lenses
    describeMaintenanceWindowExecutionTaskInvocations_nextToken,
    describeMaintenanceWindowExecutionTaskInvocations_maxResults,
    describeMaintenanceWindowExecutionTaskInvocations_filters,
    describeMaintenanceWindowExecutionTaskInvocations_windowExecutionId,
    describeMaintenanceWindowExecutionTaskInvocations_taskId,

    -- * Destructuring the Response
    DescribeMaintenanceWindowExecutionTaskInvocationsResponse (..),
    newDescribeMaintenanceWindowExecutionTaskInvocationsResponse,

    -- * Response Lenses
    describeMaintenanceWindowExecutionTaskInvocationsResponse_nextToken,
    describeMaintenanceWindowExecutionTaskInvocationsResponse_windowExecutionTaskInvocationIdentities,
    describeMaintenanceWindowExecutionTaskInvocationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newDescribeMaintenanceWindowExecutionTaskInvocations' smart constructor.
data DescribeMaintenanceWindowExecutionTaskInvocations = DescribeMaintenanceWindowExecutionTaskInvocations'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | Optional filters used to scope down the returned task invocations. The
    -- supported filter key is STATUS with the corresponding values PENDING,
    -- IN_PROGRESS, SUCCESS, FAILED, TIMED_OUT, CANCELLING, and CANCELLED.
    filters :: Core.Maybe [MaintenanceWindowFilter],
    -- | The ID of the maintenance window execution the task is part of.
    windowExecutionId :: Core.Text,
    -- | The ID of the specific task in the maintenance window task that should
    -- be retrieved.
    taskId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeMaintenanceWindowExecutionTaskInvocations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeMaintenanceWindowExecutionTaskInvocations_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'maxResults', 'describeMaintenanceWindowExecutionTaskInvocations_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'filters', 'describeMaintenanceWindowExecutionTaskInvocations_filters' - Optional filters used to scope down the returned task invocations. The
-- supported filter key is STATUS with the corresponding values PENDING,
-- IN_PROGRESS, SUCCESS, FAILED, TIMED_OUT, CANCELLING, and CANCELLED.
--
-- 'windowExecutionId', 'describeMaintenanceWindowExecutionTaskInvocations_windowExecutionId' - The ID of the maintenance window execution the task is part of.
--
-- 'taskId', 'describeMaintenanceWindowExecutionTaskInvocations_taskId' - The ID of the specific task in the maintenance window task that should
-- be retrieved.
newDescribeMaintenanceWindowExecutionTaskInvocations ::
  -- | 'windowExecutionId'
  Core.Text ->
  -- | 'taskId'
  Core.Text ->
  DescribeMaintenanceWindowExecutionTaskInvocations
newDescribeMaintenanceWindowExecutionTaskInvocations
  pWindowExecutionId_
  pTaskId_ =
    DescribeMaintenanceWindowExecutionTaskInvocations'
      { nextToken =
          Core.Nothing,
        maxResults =
          Core.Nothing,
        filters = Core.Nothing,
        windowExecutionId =
          pWindowExecutionId_,
        taskId = pTaskId_
      }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeMaintenanceWindowExecutionTaskInvocations_nextToken :: Lens.Lens' DescribeMaintenanceWindowExecutionTaskInvocations (Core.Maybe Core.Text)
describeMaintenanceWindowExecutionTaskInvocations_nextToken = Lens.lens (\DescribeMaintenanceWindowExecutionTaskInvocations' {nextToken} -> nextToken) (\s@DescribeMaintenanceWindowExecutionTaskInvocations' {} a -> s {nextToken = a} :: DescribeMaintenanceWindowExecutionTaskInvocations)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
describeMaintenanceWindowExecutionTaskInvocations_maxResults :: Lens.Lens' DescribeMaintenanceWindowExecutionTaskInvocations (Core.Maybe Core.Natural)
describeMaintenanceWindowExecutionTaskInvocations_maxResults = Lens.lens (\DescribeMaintenanceWindowExecutionTaskInvocations' {maxResults} -> maxResults) (\s@DescribeMaintenanceWindowExecutionTaskInvocations' {} a -> s {maxResults = a} :: DescribeMaintenanceWindowExecutionTaskInvocations)

-- | Optional filters used to scope down the returned task invocations. The
-- supported filter key is STATUS with the corresponding values PENDING,
-- IN_PROGRESS, SUCCESS, FAILED, TIMED_OUT, CANCELLING, and CANCELLED.
describeMaintenanceWindowExecutionTaskInvocations_filters :: Lens.Lens' DescribeMaintenanceWindowExecutionTaskInvocations (Core.Maybe [MaintenanceWindowFilter])
describeMaintenanceWindowExecutionTaskInvocations_filters = Lens.lens (\DescribeMaintenanceWindowExecutionTaskInvocations' {filters} -> filters) (\s@DescribeMaintenanceWindowExecutionTaskInvocations' {} a -> s {filters = a} :: DescribeMaintenanceWindowExecutionTaskInvocations) Core.. Lens.mapping Lens._Coerce

-- | The ID of the maintenance window execution the task is part of.
describeMaintenanceWindowExecutionTaskInvocations_windowExecutionId :: Lens.Lens' DescribeMaintenanceWindowExecutionTaskInvocations Core.Text
describeMaintenanceWindowExecutionTaskInvocations_windowExecutionId = Lens.lens (\DescribeMaintenanceWindowExecutionTaskInvocations' {windowExecutionId} -> windowExecutionId) (\s@DescribeMaintenanceWindowExecutionTaskInvocations' {} a -> s {windowExecutionId = a} :: DescribeMaintenanceWindowExecutionTaskInvocations)

-- | The ID of the specific task in the maintenance window task that should
-- be retrieved.
describeMaintenanceWindowExecutionTaskInvocations_taskId :: Lens.Lens' DescribeMaintenanceWindowExecutionTaskInvocations Core.Text
describeMaintenanceWindowExecutionTaskInvocations_taskId = Lens.lens (\DescribeMaintenanceWindowExecutionTaskInvocations' {taskId} -> taskId) (\s@DescribeMaintenanceWindowExecutionTaskInvocations' {} a -> s {taskId = a} :: DescribeMaintenanceWindowExecutionTaskInvocations)

instance
  Core.AWSPager
    DescribeMaintenanceWindowExecutionTaskInvocations
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeMaintenanceWindowExecutionTaskInvocationsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeMaintenanceWindowExecutionTaskInvocationsResponse_windowExecutionTaskInvocationIdentities
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeMaintenanceWindowExecutionTaskInvocations_nextToken
          Lens..~ rs
            Lens.^? describeMaintenanceWindowExecutionTaskInvocationsResponse_nextToken
              Core.. Lens._Just

instance
  Core.AWSRequest
    DescribeMaintenanceWindowExecutionTaskInvocations
  where
  type
    AWSResponse
      DescribeMaintenanceWindowExecutionTaskInvocations =
      DescribeMaintenanceWindowExecutionTaskInvocationsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMaintenanceWindowExecutionTaskInvocationsResponse'
            Core.<$> (x Core..?> "NextToken")
              Core.<*> ( x Core..?> "WindowExecutionTaskInvocationIdentities"
                           Core..!@ Core.mempty
                       )
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeMaintenanceWindowExecutionTaskInvocations

instance
  Core.NFData
    DescribeMaintenanceWindowExecutionTaskInvocations

instance
  Core.ToHeaders
    DescribeMaintenanceWindowExecutionTaskInvocations
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.DescribeMaintenanceWindowExecutionTaskInvocations" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    DescribeMaintenanceWindowExecutionTaskInvocations
  where
  toJSON
    DescribeMaintenanceWindowExecutionTaskInvocations' {..} =
      Core.object
        ( Core.catMaybes
            [ ("NextToken" Core..=) Core.<$> nextToken,
              ("MaxResults" Core..=) Core.<$> maxResults,
              ("Filters" Core..=) Core.<$> filters,
              Core.Just
                ("WindowExecutionId" Core..= windowExecutionId),
              Core.Just ("TaskId" Core..= taskId)
            ]
        )

instance
  Core.ToPath
    DescribeMaintenanceWindowExecutionTaskInvocations
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeMaintenanceWindowExecutionTaskInvocations
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeMaintenanceWindowExecutionTaskInvocationsResponse' smart constructor.
data DescribeMaintenanceWindowExecutionTaskInvocationsResponse = DescribeMaintenanceWindowExecutionTaskInvocationsResponse'
  { -- | The token to use when requesting the next set of items. If there are no
    -- additional items to return, the string is empty.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the task invocation results per invocation.
    windowExecutionTaskInvocationIdentities :: Core.Maybe [MaintenanceWindowExecutionTaskInvocationIdentity],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeMaintenanceWindowExecutionTaskInvocationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeMaintenanceWindowExecutionTaskInvocationsResponse_nextToken' - The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
--
-- 'windowExecutionTaskInvocationIdentities', 'describeMaintenanceWindowExecutionTaskInvocationsResponse_windowExecutionTaskInvocationIdentities' - Information about the task invocation results per invocation.
--
-- 'httpStatus', 'describeMaintenanceWindowExecutionTaskInvocationsResponse_httpStatus' - The response's http status code.
newDescribeMaintenanceWindowExecutionTaskInvocationsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeMaintenanceWindowExecutionTaskInvocationsResponse
newDescribeMaintenanceWindowExecutionTaskInvocationsResponse
  pHttpStatus_ =
    DescribeMaintenanceWindowExecutionTaskInvocationsResponse'
      { nextToken =
          Core.Nothing,
        windowExecutionTaskInvocationIdentities =
          Core.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
describeMaintenanceWindowExecutionTaskInvocationsResponse_nextToken :: Lens.Lens' DescribeMaintenanceWindowExecutionTaskInvocationsResponse (Core.Maybe Core.Text)
describeMaintenanceWindowExecutionTaskInvocationsResponse_nextToken = Lens.lens (\DescribeMaintenanceWindowExecutionTaskInvocationsResponse' {nextToken} -> nextToken) (\s@DescribeMaintenanceWindowExecutionTaskInvocationsResponse' {} a -> s {nextToken = a} :: DescribeMaintenanceWindowExecutionTaskInvocationsResponse)

-- | Information about the task invocation results per invocation.
describeMaintenanceWindowExecutionTaskInvocationsResponse_windowExecutionTaskInvocationIdentities :: Lens.Lens' DescribeMaintenanceWindowExecutionTaskInvocationsResponse (Core.Maybe [MaintenanceWindowExecutionTaskInvocationIdentity])
describeMaintenanceWindowExecutionTaskInvocationsResponse_windowExecutionTaskInvocationIdentities = Lens.lens (\DescribeMaintenanceWindowExecutionTaskInvocationsResponse' {windowExecutionTaskInvocationIdentities} -> windowExecutionTaskInvocationIdentities) (\s@DescribeMaintenanceWindowExecutionTaskInvocationsResponse' {} a -> s {windowExecutionTaskInvocationIdentities = a} :: DescribeMaintenanceWindowExecutionTaskInvocationsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeMaintenanceWindowExecutionTaskInvocationsResponse_httpStatus :: Lens.Lens' DescribeMaintenanceWindowExecutionTaskInvocationsResponse Core.Int
describeMaintenanceWindowExecutionTaskInvocationsResponse_httpStatus = Lens.lens (\DescribeMaintenanceWindowExecutionTaskInvocationsResponse' {httpStatus} -> httpStatus) (\s@DescribeMaintenanceWindowExecutionTaskInvocationsResponse' {} a -> s {httpStatus = a} :: DescribeMaintenanceWindowExecutionTaskInvocationsResponse)

instance
  Core.NFData
    DescribeMaintenanceWindowExecutionTaskInvocationsResponse
