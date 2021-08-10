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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newDescribeMaintenanceWindowExecutionTasks' smart constructor.
data DescribeMaintenanceWindowExecutionTasks = DescribeMaintenanceWindowExecutionTasks'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Optional filters used to scope down the returned tasks. The supported
    -- filter key is STATUS with the corresponding values PENDING, IN_PROGRESS,
    -- SUCCESS, FAILED, TIMED_OUT, CANCELLING, and CANCELLED.
    filters :: Prelude.Maybe [MaintenanceWindowFilter],
    -- | The ID of the maintenance window execution whose task executions should
    -- be retrieved.
    windowExecutionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DescribeMaintenanceWindowExecutionTasks
newDescribeMaintenanceWindowExecutionTasks
  pWindowExecutionId_ =
    DescribeMaintenanceWindowExecutionTasks'
      { nextToken =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        filters = Prelude.Nothing,
        windowExecutionId =
          pWindowExecutionId_
      }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeMaintenanceWindowExecutionTasks_nextToken :: Lens.Lens' DescribeMaintenanceWindowExecutionTasks (Prelude.Maybe Prelude.Text)
describeMaintenanceWindowExecutionTasks_nextToken = Lens.lens (\DescribeMaintenanceWindowExecutionTasks' {nextToken} -> nextToken) (\s@DescribeMaintenanceWindowExecutionTasks' {} a -> s {nextToken = a} :: DescribeMaintenanceWindowExecutionTasks)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
describeMaintenanceWindowExecutionTasks_maxResults :: Lens.Lens' DescribeMaintenanceWindowExecutionTasks (Prelude.Maybe Prelude.Natural)
describeMaintenanceWindowExecutionTasks_maxResults = Lens.lens (\DescribeMaintenanceWindowExecutionTasks' {maxResults} -> maxResults) (\s@DescribeMaintenanceWindowExecutionTasks' {} a -> s {maxResults = a} :: DescribeMaintenanceWindowExecutionTasks)

-- | Optional filters used to scope down the returned tasks. The supported
-- filter key is STATUS with the corresponding values PENDING, IN_PROGRESS,
-- SUCCESS, FAILED, TIMED_OUT, CANCELLING, and CANCELLED.
describeMaintenanceWindowExecutionTasks_filters :: Lens.Lens' DescribeMaintenanceWindowExecutionTasks (Prelude.Maybe [MaintenanceWindowFilter])
describeMaintenanceWindowExecutionTasks_filters = Lens.lens (\DescribeMaintenanceWindowExecutionTasks' {filters} -> filters) (\s@DescribeMaintenanceWindowExecutionTasks' {} a -> s {filters = a} :: DescribeMaintenanceWindowExecutionTasks) Prelude.. Lens.mapping Lens._Coerce

-- | The ID of the maintenance window execution whose task executions should
-- be retrieved.
describeMaintenanceWindowExecutionTasks_windowExecutionId :: Lens.Lens' DescribeMaintenanceWindowExecutionTasks Prelude.Text
describeMaintenanceWindowExecutionTasks_windowExecutionId = Lens.lens (\DescribeMaintenanceWindowExecutionTasks' {windowExecutionId} -> windowExecutionId) (\s@DescribeMaintenanceWindowExecutionTasks' {} a -> s {windowExecutionId = a} :: DescribeMaintenanceWindowExecutionTasks)

instance
  Core.AWSPager
    DescribeMaintenanceWindowExecutionTasks
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeMaintenanceWindowExecutionTasksResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeMaintenanceWindowExecutionTasksResponse_windowExecutionTaskIdentities
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeMaintenanceWindowExecutionTasks_nextToken
          Lens..~ rs
            Lens.^? describeMaintenanceWindowExecutionTasksResponse_nextToken
              Prelude.. Lens._Just

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
            Prelude.<$> (x Core..?> "NextToken")
              Prelude.<*> ( x Core..?> "WindowExecutionTaskIdentities"
                              Core..!@ Prelude.mempty
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeMaintenanceWindowExecutionTasks

instance
  Prelude.NFData
    DescribeMaintenanceWindowExecutionTasks

instance
  Core.ToHeaders
    DescribeMaintenanceWindowExecutionTasks
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.DescribeMaintenanceWindowExecutionTasks" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    DescribeMaintenanceWindowExecutionTasks
  where
  toJSON DescribeMaintenanceWindowExecutionTasks' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("Filters" Core..=) Prelude.<$> filters,
            Prelude.Just
              ("WindowExecutionId" Core..= windowExecutionId)
          ]
      )

instance
  Core.ToPath
    DescribeMaintenanceWindowExecutionTasks
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DescribeMaintenanceWindowExecutionTasks
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeMaintenanceWindowExecutionTasksResponse' smart constructor.
data DescribeMaintenanceWindowExecutionTasksResponse = DescribeMaintenanceWindowExecutionTasksResponse'
  { -- | The token to use when requesting the next set of items. If there are no
    -- additional items to return, the string is empty.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the task executions.
    windowExecutionTaskIdentities :: Prelude.Maybe [MaintenanceWindowExecutionTaskIdentity],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeMaintenanceWindowExecutionTasksResponse
newDescribeMaintenanceWindowExecutionTasksResponse
  pHttpStatus_ =
    DescribeMaintenanceWindowExecutionTasksResponse'
      { nextToken =
          Prelude.Nothing,
        windowExecutionTaskIdentities =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
describeMaintenanceWindowExecutionTasksResponse_nextToken :: Lens.Lens' DescribeMaintenanceWindowExecutionTasksResponse (Prelude.Maybe Prelude.Text)
describeMaintenanceWindowExecutionTasksResponse_nextToken = Lens.lens (\DescribeMaintenanceWindowExecutionTasksResponse' {nextToken} -> nextToken) (\s@DescribeMaintenanceWindowExecutionTasksResponse' {} a -> s {nextToken = a} :: DescribeMaintenanceWindowExecutionTasksResponse)

-- | Information about the task executions.
describeMaintenanceWindowExecutionTasksResponse_windowExecutionTaskIdentities :: Lens.Lens' DescribeMaintenanceWindowExecutionTasksResponse (Prelude.Maybe [MaintenanceWindowExecutionTaskIdentity])
describeMaintenanceWindowExecutionTasksResponse_windowExecutionTaskIdentities = Lens.lens (\DescribeMaintenanceWindowExecutionTasksResponse' {windowExecutionTaskIdentities} -> windowExecutionTaskIdentities) (\s@DescribeMaintenanceWindowExecutionTasksResponse' {} a -> s {windowExecutionTaskIdentities = a} :: DescribeMaintenanceWindowExecutionTasksResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeMaintenanceWindowExecutionTasksResponse_httpStatus :: Lens.Lens' DescribeMaintenanceWindowExecutionTasksResponse Prelude.Int
describeMaintenanceWindowExecutionTasksResponse_httpStatus = Lens.lens (\DescribeMaintenanceWindowExecutionTasksResponse' {httpStatus} -> httpStatus) (\s@DescribeMaintenanceWindowExecutionTasksResponse' {} a -> s {httpStatus = a} :: DescribeMaintenanceWindowExecutionTasksResponse)

instance
  Prelude.NFData
    DescribeMaintenanceWindowExecutionTasksResponse
