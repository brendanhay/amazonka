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
-- Module      : Network.AWS.IoT.ListAuditTasks
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the Device Defender audits that have been performed during a given
-- time period.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListAuditTasks
  ( -- * Creating a Request
    ListAuditTasks (..),
    newListAuditTasks,

    -- * Request Lenses
    listAuditTasks_nextToken,
    listAuditTasks_maxResults,
    listAuditTasks_taskStatus,
    listAuditTasks_taskType,
    listAuditTasks_startTime,
    listAuditTasks_endTime,

    -- * Destructuring the Response
    ListAuditTasksResponse (..),
    newListAuditTasksResponse,

    -- * Response Lenses
    listAuditTasksResponse_nextToken,
    listAuditTasksResponse_tasks,
    listAuditTasksResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListAuditTasks' smart constructor.
data ListAuditTasks = ListAuditTasks'
  { -- | The token for the next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return at one time. The default is 25.
    maxResults :: Core.Maybe Core.Natural,
    -- | A filter to limit the output to audits with the specified completion
    -- status: can be one of \"IN_PROGRESS\", \"COMPLETED\", \"FAILED\", or
    -- \"CANCELED\".
    taskStatus :: Core.Maybe AuditTaskStatus,
    -- | A filter to limit the output to the specified type of audit: can be one
    -- of \"ON_DEMAND_AUDIT_TASK\" or \"SCHEDULED__AUDIT_TASK\".
    taskType :: Core.Maybe AuditTaskType,
    -- | The beginning of the time period. Audit information is retained for a
    -- limited time (90 days). Requesting a start time prior to what is
    -- retained results in an \"InvalidRequestException\".
    startTime :: Core.POSIX,
    -- | The end of the time period.
    endTime :: Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAuditTasks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAuditTasks_nextToken' - The token for the next set of results.
--
-- 'maxResults', 'listAuditTasks_maxResults' - The maximum number of results to return at one time. The default is 25.
--
-- 'taskStatus', 'listAuditTasks_taskStatus' - A filter to limit the output to audits with the specified completion
-- status: can be one of \"IN_PROGRESS\", \"COMPLETED\", \"FAILED\", or
-- \"CANCELED\".
--
-- 'taskType', 'listAuditTasks_taskType' - A filter to limit the output to the specified type of audit: can be one
-- of \"ON_DEMAND_AUDIT_TASK\" or \"SCHEDULED__AUDIT_TASK\".
--
-- 'startTime', 'listAuditTasks_startTime' - The beginning of the time period. Audit information is retained for a
-- limited time (90 days). Requesting a start time prior to what is
-- retained results in an \"InvalidRequestException\".
--
-- 'endTime', 'listAuditTasks_endTime' - The end of the time period.
newListAuditTasks ::
  -- | 'startTime'
  Core.UTCTime ->
  -- | 'endTime'
  Core.UTCTime ->
  ListAuditTasks
newListAuditTasks pStartTime_ pEndTime_ =
  ListAuditTasks'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      taskStatus = Core.Nothing,
      taskType = Core.Nothing,
      startTime = Core._Time Lens.# pStartTime_,
      endTime = Core._Time Lens.# pEndTime_
    }

-- | The token for the next set of results.
listAuditTasks_nextToken :: Lens.Lens' ListAuditTasks (Core.Maybe Core.Text)
listAuditTasks_nextToken = Lens.lens (\ListAuditTasks' {nextToken} -> nextToken) (\s@ListAuditTasks' {} a -> s {nextToken = a} :: ListAuditTasks)

-- | The maximum number of results to return at one time. The default is 25.
listAuditTasks_maxResults :: Lens.Lens' ListAuditTasks (Core.Maybe Core.Natural)
listAuditTasks_maxResults = Lens.lens (\ListAuditTasks' {maxResults} -> maxResults) (\s@ListAuditTasks' {} a -> s {maxResults = a} :: ListAuditTasks)

-- | A filter to limit the output to audits with the specified completion
-- status: can be one of \"IN_PROGRESS\", \"COMPLETED\", \"FAILED\", or
-- \"CANCELED\".
listAuditTasks_taskStatus :: Lens.Lens' ListAuditTasks (Core.Maybe AuditTaskStatus)
listAuditTasks_taskStatus = Lens.lens (\ListAuditTasks' {taskStatus} -> taskStatus) (\s@ListAuditTasks' {} a -> s {taskStatus = a} :: ListAuditTasks)

-- | A filter to limit the output to the specified type of audit: can be one
-- of \"ON_DEMAND_AUDIT_TASK\" or \"SCHEDULED__AUDIT_TASK\".
listAuditTasks_taskType :: Lens.Lens' ListAuditTasks (Core.Maybe AuditTaskType)
listAuditTasks_taskType = Lens.lens (\ListAuditTasks' {taskType} -> taskType) (\s@ListAuditTasks' {} a -> s {taskType = a} :: ListAuditTasks)

-- | The beginning of the time period. Audit information is retained for a
-- limited time (90 days). Requesting a start time prior to what is
-- retained results in an \"InvalidRequestException\".
listAuditTasks_startTime :: Lens.Lens' ListAuditTasks Core.UTCTime
listAuditTasks_startTime = Lens.lens (\ListAuditTasks' {startTime} -> startTime) (\s@ListAuditTasks' {} a -> s {startTime = a} :: ListAuditTasks) Core.. Core._Time

-- | The end of the time period.
listAuditTasks_endTime :: Lens.Lens' ListAuditTasks Core.UTCTime
listAuditTasks_endTime = Lens.lens (\ListAuditTasks' {endTime} -> endTime) (\s@ListAuditTasks' {} a -> s {endTime = a} :: ListAuditTasks) Core.. Core._Time

instance Core.AWSPager ListAuditTasks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAuditTasksResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listAuditTasksResponse_tasks Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listAuditTasks_nextToken
          Lens..~ rs
          Lens.^? listAuditTasksResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListAuditTasks where
  type
    AWSResponse ListAuditTasks =
      ListAuditTasksResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAuditTasksResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "tasks" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListAuditTasks

instance Core.NFData ListAuditTasks

instance Core.ToHeaders ListAuditTasks where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListAuditTasks where
  toPath = Core.const "/audit/tasks"

instance Core.ToQuery ListAuditTasks where
  toQuery ListAuditTasks' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults,
        "taskStatus" Core.=: taskStatus,
        "taskType" Core.=: taskType,
        "startTime" Core.=: startTime,
        "endTime" Core.=: endTime
      ]

-- | /See:/ 'newListAuditTasksResponse' smart constructor.
data ListAuditTasksResponse = ListAuditTasksResponse'
  { -- | A token that can be used to retrieve the next set of results, or @null@
    -- if there are no additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | The audits that were performed during the specified time period.
    tasks :: Core.Maybe [AuditTaskMetadata],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAuditTasksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAuditTasksResponse_nextToken' - A token that can be used to retrieve the next set of results, or @null@
-- if there are no additional results.
--
-- 'tasks', 'listAuditTasksResponse_tasks' - The audits that were performed during the specified time period.
--
-- 'httpStatus', 'listAuditTasksResponse_httpStatus' - The response's http status code.
newListAuditTasksResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListAuditTasksResponse
newListAuditTasksResponse pHttpStatus_ =
  ListAuditTasksResponse'
    { nextToken = Core.Nothing,
      tasks = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that can be used to retrieve the next set of results, or @null@
-- if there are no additional results.
listAuditTasksResponse_nextToken :: Lens.Lens' ListAuditTasksResponse (Core.Maybe Core.Text)
listAuditTasksResponse_nextToken = Lens.lens (\ListAuditTasksResponse' {nextToken} -> nextToken) (\s@ListAuditTasksResponse' {} a -> s {nextToken = a} :: ListAuditTasksResponse)

-- | The audits that were performed during the specified time period.
listAuditTasksResponse_tasks :: Lens.Lens' ListAuditTasksResponse (Core.Maybe [AuditTaskMetadata])
listAuditTasksResponse_tasks = Lens.lens (\ListAuditTasksResponse' {tasks} -> tasks) (\s@ListAuditTasksResponse' {} a -> s {tasks = a} :: ListAuditTasksResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listAuditTasksResponse_httpStatus :: Lens.Lens' ListAuditTasksResponse Core.Int
listAuditTasksResponse_httpStatus = Lens.lens (\ListAuditTasksResponse' {httpStatus} -> httpStatus) (\s@ListAuditTasksResponse' {} a -> s {httpStatus = a} :: ListAuditTasksResponse)

instance Core.NFData ListAuditTasksResponse
