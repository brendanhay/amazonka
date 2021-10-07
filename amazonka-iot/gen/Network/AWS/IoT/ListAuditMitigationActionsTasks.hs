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
-- Module      : Network.AWS.IoT.ListAuditMitigationActionsTasks
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of audit mitigation action tasks that match the specified
-- filters.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions ListAuditMitigationActionsTasks>
-- action.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListAuditMitigationActionsTasks
  ( -- * Creating a Request
    ListAuditMitigationActionsTasks (..),
    newListAuditMitigationActionsTasks,

    -- * Request Lenses
    listAuditMitigationActionsTasks_nextToken,
    listAuditMitigationActionsTasks_maxResults,
    listAuditMitigationActionsTasks_findingId,
    listAuditMitigationActionsTasks_auditTaskId,
    listAuditMitigationActionsTasks_taskStatus,
    listAuditMitigationActionsTasks_startTime,
    listAuditMitigationActionsTasks_endTime,

    -- * Destructuring the Response
    ListAuditMitigationActionsTasksResponse (..),
    newListAuditMitigationActionsTasksResponse,

    -- * Response Lenses
    listAuditMitigationActionsTasksResponse_nextToken,
    listAuditMitigationActionsTasksResponse_tasks,
    listAuditMitigationActionsTasksResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListAuditMitigationActionsTasks' smart constructor.
data ListAuditMitigationActionsTasks = ListAuditMitigationActionsTasks'
  { -- | The token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return at one time. The default is 25.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Specify this filter to limit results to tasks that were applied to a
    -- specific audit finding.
    findingId :: Prelude.Maybe Prelude.Text,
    -- | Specify this filter to limit results to tasks that were applied to
    -- results for a specific audit.
    auditTaskId :: Prelude.Maybe Prelude.Text,
    -- | Specify this filter to limit results to tasks that are in a specific
    -- state.
    taskStatus :: Prelude.Maybe AuditMitigationActionsTaskStatus,
    -- | Specify this filter to limit results to tasks that began on or after a
    -- specific date and time.
    startTime :: Core.POSIX,
    -- | Specify this filter to limit results to tasks that were completed or
    -- canceled on or before a specific date and time.
    endTime :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAuditMitigationActionsTasks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAuditMitigationActionsTasks_nextToken' - The token for the next set of results.
--
-- 'maxResults', 'listAuditMitigationActionsTasks_maxResults' - The maximum number of results to return at one time. The default is 25.
--
-- 'findingId', 'listAuditMitigationActionsTasks_findingId' - Specify this filter to limit results to tasks that were applied to a
-- specific audit finding.
--
-- 'auditTaskId', 'listAuditMitigationActionsTasks_auditTaskId' - Specify this filter to limit results to tasks that were applied to
-- results for a specific audit.
--
-- 'taskStatus', 'listAuditMitigationActionsTasks_taskStatus' - Specify this filter to limit results to tasks that are in a specific
-- state.
--
-- 'startTime', 'listAuditMitigationActionsTasks_startTime' - Specify this filter to limit results to tasks that began on or after a
-- specific date and time.
--
-- 'endTime', 'listAuditMitigationActionsTasks_endTime' - Specify this filter to limit results to tasks that were completed or
-- canceled on or before a specific date and time.
newListAuditMitigationActionsTasks ::
  -- | 'startTime'
  Prelude.UTCTime ->
  -- | 'endTime'
  Prelude.UTCTime ->
  ListAuditMitigationActionsTasks
newListAuditMitigationActionsTasks
  pStartTime_
  pEndTime_ =
    ListAuditMitigationActionsTasks'
      { nextToken =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        findingId = Prelude.Nothing,
        auditTaskId = Prelude.Nothing,
        taskStatus = Prelude.Nothing,
        startTime = Core._Time Lens.# pStartTime_,
        endTime = Core._Time Lens.# pEndTime_
      }

-- | The token for the next set of results.
listAuditMitigationActionsTasks_nextToken :: Lens.Lens' ListAuditMitigationActionsTasks (Prelude.Maybe Prelude.Text)
listAuditMitigationActionsTasks_nextToken = Lens.lens (\ListAuditMitigationActionsTasks' {nextToken} -> nextToken) (\s@ListAuditMitigationActionsTasks' {} a -> s {nextToken = a} :: ListAuditMitigationActionsTasks)

-- | The maximum number of results to return at one time. The default is 25.
listAuditMitigationActionsTasks_maxResults :: Lens.Lens' ListAuditMitigationActionsTasks (Prelude.Maybe Prelude.Natural)
listAuditMitigationActionsTasks_maxResults = Lens.lens (\ListAuditMitigationActionsTasks' {maxResults} -> maxResults) (\s@ListAuditMitigationActionsTasks' {} a -> s {maxResults = a} :: ListAuditMitigationActionsTasks)

-- | Specify this filter to limit results to tasks that were applied to a
-- specific audit finding.
listAuditMitigationActionsTasks_findingId :: Lens.Lens' ListAuditMitigationActionsTasks (Prelude.Maybe Prelude.Text)
listAuditMitigationActionsTasks_findingId = Lens.lens (\ListAuditMitigationActionsTasks' {findingId} -> findingId) (\s@ListAuditMitigationActionsTasks' {} a -> s {findingId = a} :: ListAuditMitigationActionsTasks)

-- | Specify this filter to limit results to tasks that were applied to
-- results for a specific audit.
listAuditMitigationActionsTasks_auditTaskId :: Lens.Lens' ListAuditMitigationActionsTasks (Prelude.Maybe Prelude.Text)
listAuditMitigationActionsTasks_auditTaskId = Lens.lens (\ListAuditMitigationActionsTasks' {auditTaskId} -> auditTaskId) (\s@ListAuditMitigationActionsTasks' {} a -> s {auditTaskId = a} :: ListAuditMitigationActionsTasks)

-- | Specify this filter to limit results to tasks that are in a specific
-- state.
listAuditMitigationActionsTasks_taskStatus :: Lens.Lens' ListAuditMitigationActionsTasks (Prelude.Maybe AuditMitigationActionsTaskStatus)
listAuditMitigationActionsTasks_taskStatus = Lens.lens (\ListAuditMitigationActionsTasks' {taskStatus} -> taskStatus) (\s@ListAuditMitigationActionsTasks' {} a -> s {taskStatus = a} :: ListAuditMitigationActionsTasks)

-- | Specify this filter to limit results to tasks that began on or after a
-- specific date and time.
listAuditMitigationActionsTasks_startTime :: Lens.Lens' ListAuditMitigationActionsTasks Prelude.UTCTime
listAuditMitigationActionsTasks_startTime = Lens.lens (\ListAuditMitigationActionsTasks' {startTime} -> startTime) (\s@ListAuditMitigationActionsTasks' {} a -> s {startTime = a} :: ListAuditMitigationActionsTasks) Prelude.. Core._Time

-- | Specify this filter to limit results to tasks that were completed or
-- canceled on or before a specific date and time.
listAuditMitigationActionsTasks_endTime :: Lens.Lens' ListAuditMitigationActionsTasks Prelude.UTCTime
listAuditMitigationActionsTasks_endTime = Lens.lens (\ListAuditMitigationActionsTasks' {endTime} -> endTime) (\s@ListAuditMitigationActionsTasks' {} a -> s {endTime = a} :: ListAuditMitigationActionsTasks) Prelude.. Core._Time

instance
  Core.AWSPager
    ListAuditMitigationActionsTasks
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAuditMitigationActionsTasksResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAuditMitigationActionsTasksResponse_tasks
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listAuditMitigationActionsTasks_nextToken
          Lens..~ rs
          Lens.^? listAuditMitigationActionsTasksResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListAuditMitigationActionsTasks
  where
  type
    AWSResponse ListAuditMitigationActionsTasks =
      ListAuditMitigationActionsTasksResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAuditMitigationActionsTasksResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "tasks" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListAuditMitigationActionsTasks

instance
  Prelude.NFData
    ListAuditMitigationActionsTasks

instance
  Core.ToHeaders
    ListAuditMitigationActionsTasks
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListAuditMitigationActionsTasks where
  toPath =
    Prelude.const "/audit/mitigationactions/tasks"

instance Core.ToQuery ListAuditMitigationActionsTasks where
  toQuery ListAuditMitigationActionsTasks' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults,
        "findingId" Core.=: findingId,
        "auditTaskId" Core.=: auditTaskId,
        "taskStatus" Core.=: taskStatus,
        "startTime" Core.=: startTime,
        "endTime" Core.=: endTime
      ]

-- | /See:/ 'newListAuditMitigationActionsTasksResponse' smart constructor.
data ListAuditMitigationActionsTasksResponse = ListAuditMitigationActionsTasksResponse'
  { -- | The token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The collection of audit mitigation tasks that matched the filter
    -- criteria.
    tasks :: Prelude.Maybe [AuditMitigationActionsTaskMetadata],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAuditMitigationActionsTasksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAuditMitigationActionsTasksResponse_nextToken' - The token for the next set of results.
--
-- 'tasks', 'listAuditMitigationActionsTasksResponse_tasks' - The collection of audit mitigation tasks that matched the filter
-- criteria.
--
-- 'httpStatus', 'listAuditMitigationActionsTasksResponse_httpStatus' - The response's http status code.
newListAuditMitigationActionsTasksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAuditMitigationActionsTasksResponse
newListAuditMitigationActionsTasksResponse
  pHttpStatus_ =
    ListAuditMitigationActionsTasksResponse'
      { nextToken =
          Prelude.Nothing,
        tasks = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token for the next set of results.
listAuditMitigationActionsTasksResponse_nextToken :: Lens.Lens' ListAuditMitigationActionsTasksResponse (Prelude.Maybe Prelude.Text)
listAuditMitigationActionsTasksResponse_nextToken = Lens.lens (\ListAuditMitigationActionsTasksResponse' {nextToken} -> nextToken) (\s@ListAuditMitigationActionsTasksResponse' {} a -> s {nextToken = a} :: ListAuditMitigationActionsTasksResponse)

-- | The collection of audit mitigation tasks that matched the filter
-- criteria.
listAuditMitigationActionsTasksResponse_tasks :: Lens.Lens' ListAuditMitigationActionsTasksResponse (Prelude.Maybe [AuditMitigationActionsTaskMetadata])
listAuditMitigationActionsTasksResponse_tasks = Lens.lens (\ListAuditMitigationActionsTasksResponse' {tasks} -> tasks) (\s@ListAuditMitigationActionsTasksResponse' {} a -> s {tasks = a} :: ListAuditMitigationActionsTasksResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listAuditMitigationActionsTasksResponse_httpStatus :: Lens.Lens' ListAuditMitigationActionsTasksResponse Prelude.Int
listAuditMitigationActionsTasksResponse_httpStatus = Lens.lens (\ListAuditMitigationActionsTasksResponse' {httpStatus} -> httpStatus) (\s@ListAuditMitigationActionsTasksResponse' {} a -> s {httpStatus = a} :: ListAuditMitigationActionsTasksResponse)

instance
  Prelude.NFData
    ListAuditMitigationActionsTasksResponse
