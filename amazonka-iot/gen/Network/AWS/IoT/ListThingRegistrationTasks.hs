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
-- Module      : Network.AWS.IoT.ListThingRegistrationTasks
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List bulk thing provisioning tasks.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListThingRegistrationTasks
  ( -- * Creating a Request
    ListThingRegistrationTasks (..),
    newListThingRegistrationTasks,

    -- * Request Lenses
    listThingRegistrationTasks_nextToken,
    listThingRegistrationTasks_status,
    listThingRegistrationTasks_maxResults,

    -- * Destructuring the Response
    ListThingRegistrationTasksResponse (..),
    newListThingRegistrationTasksResponse,

    -- * Response Lenses
    listThingRegistrationTasksResponse_nextToken,
    listThingRegistrationTasksResponse_taskIds,
    listThingRegistrationTasksResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListThingRegistrationTasks' smart constructor.
data ListThingRegistrationTasks = ListThingRegistrationTasks'
  { -- | To retrieve the next set of results, the @nextToken@ value from a
    -- previous response; otherwise __null__ to receive the first set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | The status of the bulk thing provisioning task.
    status :: Core.Maybe TaskStatus,
    -- | The maximum number of results to return at one time.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListThingRegistrationTasks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listThingRegistrationTasks_nextToken' - To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
--
-- 'status', 'listThingRegistrationTasks_status' - The status of the bulk thing provisioning task.
--
-- 'maxResults', 'listThingRegistrationTasks_maxResults' - The maximum number of results to return at one time.
newListThingRegistrationTasks ::
  ListThingRegistrationTasks
newListThingRegistrationTasks =
  ListThingRegistrationTasks'
    { nextToken =
        Core.Nothing,
      status = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
listThingRegistrationTasks_nextToken :: Lens.Lens' ListThingRegistrationTasks (Core.Maybe Core.Text)
listThingRegistrationTasks_nextToken = Lens.lens (\ListThingRegistrationTasks' {nextToken} -> nextToken) (\s@ListThingRegistrationTasks' {} a -> s {nextToken = a} :: ListThingRegistrationTasks)

-- | The status of the bulk thing provisioning task.
listThingRegistrationTasks_status :: Lens.Lens' ListThingRegistrationTasks (Core.Maybe TaskStatus)
listThingRegistrationTasks_status = Lens.lens (\ListThingRegistrationTasks' {status} -> status) (\s@ListThingRegistrationTasks' {} a -> s {status = a} :: ListThingRegistrationTasks)

-- | The maximum number of results to return at one time.
listThingRegistrationTasks_maxResults :: Lens.Lens' ListThingRegistrationTasks (Core.Maybe Core.Natural)
listThingRegistrationTasks_maxResults = Lens.lens (\ListThingRegistrationTasks' {maxResults} -> maxResults) (\s@ListThingRegistrationTasks' {} a -> s {maxResults = a} :: ListThingRegistrationTasks)

instance Core.AWSPager ListThingRegistrationTasks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listThingRegistrationTasksResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listThingRegistrationTasksResponse_taskIds
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listThingRegistrationTasks_nextToken
          Lens..~ rs
          Lens.^? listThingRegistrationTasksResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListThingRegistrationTasks where
  type
    AWSResponse ListThingRegistrationTasks =
      ListThingRegistrationTasksResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListThingRegistrationTasksResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "taskIds" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListThingRegistrationTasks

instance Core.NFData ListThingRegistrationTasks

instance Core.ToHeaders ListThingRegistrationTasks where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListThingRegistrationTasks where
  toPath = Core.const "/thing-registration-tasks"

instance Core.ToQuery ListThingRegistrationTasks where
  toQuery ListThingRegistrationTasks' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "status" Core.=: status,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListThingRegistrationTasksResponse' smart constructor.
data ListThingRegistrationTasksResponse = ListThingRegistrationTasksResponse'
  { -- | The token to use to get the next set of results, or __null__ if there
    -- are no additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of bulk thing provisioning task IDs.
    taskIds :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListThingRegistrationTasksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listThingRegistrationTasksResponse_nextToken' - The token to use to get the next set of results, or __null__ if there
-- are no additional results.
--
-- 'taskIds', 'listThingRegistrationTasksResponse_taskIds' - A list of bulk thing provisioning task IDs.
--
-- 'httpStatus', 'listThingRegistrationTasksResponse_httpStatus' - The response's http status code.
newListThingRegistrationTasksResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListThingRegistrationTasksResponse
newListThingRegistrationTasksResponse pHttpStatus_ =
  ListThingRegistrationTasksResponse'
    { nextToken =
        Core.Nothing,
      taskIds = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to get the next set of results, or __null__ if there
-- are no additional results.
listThingRegistrationTasksResponse_nextToken :: Lens.Lens' ListThingRegistrationTasksResponse (Core.Maybe Core.Text)
listThingRegistrationTasksResponse_nextToken = Lens.lens (\ListThingRegistrationTasksResponse' {nextToken} -> nextToken) (\s@ListThingRegistrationTasksResponse' {} a -> s {nextToken = a} :: ListThingRegistrationTasksResponse)

-- | A list of bulk thing provisioning task IDs.
listThingRegistrationTasksResponse_taskIds :: Lens.Lens' ListThingRegistrationTasksResponse (Core.Maybe [Core.Text])
listThingRegistrationTasksResponse_taskIds = Lens.lens (\ListThingRegistrationTasksResponse' {taskIds} -> taskIds) (\s@ListThingRegistrationTasksResponse' {} a -> s {taskIds = a} :: ListThingRegistrationTasksResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listThingRegistrationTasksResponse_httpStatus :: Lens.Lens' ListThingRegistrationTasksResponse Core.Int
listThingRegistrationTasksResponse_httpStatus = Lens.lens (\ListThingRegistrationTasksResponse' {httpStatus} -> httpStatus) (\s@ListThingRegistrationTasksResponse' {} a -> s {httpStatus = a} :: ListThingRegistrationTasksResponse)

instance
  Core.NFData
    ListThingRegistrationTasksResponse
