{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListThingRegistrationTasks' smart constructor.
data ListThingRegistrationTasks = ListThingRegistrationTasks'
  { -- | To retrieve the next set of results, the @nextToken@ value from a
    -- previous response; otherwise __null__ to receive the first set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The status of the bulk thing provisioning task.
    status :: Prelude.Maybe TaskStatus,
    -- | The maximum number of results to return at one time.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      status = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
listThingRegistrationTasks_nextToken :: Lens.Lens' ListThingRegistrationTasks (Prelude.Maybe Prelude.Text)
listThingRegistrationTasks_nextToken = Lens.lens (\ListThingRegistrationTasks' {nextToken} -> nextToken) (\s@ListThingRegistrationTasks' {} a -> s {nextToken = a} :: ListThingRegistrationTasks)

-- | The status of the bulk thing provisioning task.
listThingRegistrationTasks_status :: Lens.Lens' ListThingRegistrationTasks (Prelude.Maybe TaskStatus)
listThingRegistrationTasks_status = Lens.lens (\ListThingRegistrationTasks' {status} -> status) (\s@ListThingRegistrationTasks' {} a -> s {status = a} :: ListThingRegistrationTasks)

-- | The maximum number of results to return at one time.
listThingRegistrationTasks_maxResults :: Lens.Lens' ListThingRegistrationTasks (Prelude.Maybe Prelude.Natural)
listThingRegistrationTasks_maxResults = Lens.lens (\ListThingRegistrationTasks' {maxResults} -> maxResults) (\s@ListThingRegistrationTasks' {} a -> s {maxResults = a} :: ListThingRegistrationTasks)

instance Pager.AWSPager ListThingRegistrationTasks where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listThingRegistrationTasksResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listThingRegistrationTasksResponse_taskIds
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listThingRegistrationTasks_nextToken
          Lens..~ rs
          Lens.^? listThingRegistrationTasksResponse_nextToken
            Prelude.. Lens._Just

instance
  Prelude.AWSRequest
    ListThingRegistrationTasks
  where
  type
    Rs ListThingRegistrationTasks =
      ListThingRegistrationTasksResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListThingRegistrationTasksResponse'
            Prelude.<$> (x Prelude..?> "nextToken")
            Prelude.<*> (x Prelude..?> "taskIds" Prelude..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListThingRegistrationTasks

instance Prelude.NFData ListThingRegistrationTasks

instance Prelude.ToHeaders ListThingRegistrationTasks where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ListThingRegistrationTasks where
  toPath = Prelude.const "/thing-registration-tasks"

instance Prelude.ToQuery ListThingRegistrationTasks where
  toQuery ListThingRegistrationTasks' {..} =
    Prelude.mconcat
      [ "nextToken" Prelude.=: nextToken,
        "status" Prelude.=: status,
        "maxResults" Prelude.=: maxResults
      ]

-- | /See:/ 'newListThingRegistrationTasksResponse' smart constructor.
data ListThingRegistrationTasksResponse = ListThingRegistrationTasksResponse'
  { -- | The token to use to get the next set of results, or __null__ if there
    -- are no additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of bulk thing provisioning task IDs.
    taskIds :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  ListThingRegistrationTasksResponse
newListThingRegistrationTasksResponse pHttpStatus_ =
  ListThingRegistrationTasksResponse'
    { nextToken =
        Prelude.Nothing,
      taskIds = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to get the next set of results, or __null__ if there
-- are no additional results.
listThingRegistrationTasksResponse_nextToken :: Lens.Lens' ListThingRegistrationTasksResponse (Prelude.Maybe Prelude.Text)
listThingRegistrationTasksResponse_nextToken = Lens.lens (\ListThingRegistrationTasksResponse' {nextToken} -> nextToken) (\s@ListThingRegistrationTasksResponse' {} a -> s {nextToken = a} :: ListThingRegistrationTasksResponse)

-- | A list of bulk thing provisioning task IDs.
listThingRegistrationTasksResponse_taskIds :: Lens.Lens' ListThingRegistrationTasksResponse (Prelude.Maybe [Prelude.Text])
listThingRegistrationTasksResponse_taskIds = Lens.lens (\ListThingRegistrationTasksResponse' {taskIds} -> taskIds) (\s@ListThingRegistrationTasksResponse' {} a -> s {taskIds = a} :: ListThingRegistrationTasksResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listThingRegistrationTasksResponse_httpStatus :: Lens.Lens' ListThingRegistrationTasksResponse Prelude.Int
listThingRegistrationTasksResponse_httpStatus = Lens.lens (\ListThingRegistrationTasksResponse' {httpStatus} -> httpStatus) (\s@ListThingRegistrationTasksResponse' {} a -> s {httpStatus = a} :: ListThingRegistrationTasksResponse)

instance
  Prelude.NFData
    ListThingRegistrationTasksResponse
