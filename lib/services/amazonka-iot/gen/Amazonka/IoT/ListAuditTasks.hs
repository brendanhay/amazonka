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
-- Module      : Amazonka.IoT.ListAuditTasks
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the Device Defender audits that have been performed during a given
-- time period.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions ListAuditTasks>
-- action.
--
-- This operation returns paginated results.
module Amazonka.IoT.ListAuditTasks
  ( -- * Creating a Request
    ListAuditTasks (..),
    newListAuditTasks,

    -- * Request Lenses
    listAuditTasks_maxResults,
    listAuditTasks_nextToken,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAuditTasks' smart constructor.
data ListAuditTasks = ListAuditTasks'
  { -- | The maximum number of results to return at one time. The default is 25.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A filter to limit the output to audits with the specified completion
    -- status: can be one of \"IN_PROGRESS\", \"COMPLETED\", \"FAILED\", or
    -- \"CANCELED\".
    taskStatus :: Prelude.Maybe AuditTaskStatus,
    -- | A filter to limit the output to the specified type of audit: can be one
    -- of \"ON_DEMAND_AUDIT_TASK\" or \"SCHEDULED__AUDIT_TASK\".
    taskType :: Prelude.Maybe AuditTaskType,
    -- | The beginning of the time period. Audit information is retained for a
    -- limited time (90 days). Requesting a start time prior to what is
    -- retained results in an \"InvalidRequestException\".
    startTime :: Data.POSIX,
    -- | The end of the time period.
    endTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAuditTasks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listAuditTasks_maxResults' - The maximum number of results to return at one time. The default is 25.
--
-- 'nextToken', 'listAuditTasks_nextToken' - The token for the next set of results.
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
  Prelude.UTCTime ->
  -- | 'endTime'
  Prelude.UTCTime ->
  ListAuditTasks
newListAuditTasks pStartTime_ pEndTime_ =
  ListAuditTasks'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      taskStatus = Prelude.Nothing,
      taskType = Prelude.Nothing,
      startTime = Data._Time Lens.# pStartTime_,
      endTime = Data._Time Lens.# pEndTime_
    }

-- | The maximum number of results to return at one time. The default is 25.
listAuditTasks_maxResults :: Lens.Lens' ListAuditTasks (Prelude.Maybe Prelude.Natural)
listAuditTasks_maxResults = Lens.lens (\ListAuditTasks' {maxResults} -> maxResults) (\s@ListAuditTasks' {} a -> s {maxResults = a} :: ListAuditTasks)

-- | The token for the next set of results.
listAuditTasks_nextToken :: Lens.Lens' ListAuditTasks (Prelude.Maybe Prelude.Text)
listAuditTasks_nextToken = Lens.lens (\ListAuditTasks' {nextToken} -> nextToken) (\s@ListAuditTasks' {} a -> s {nextToken = a} :: ListAuditTasks)

-- | A filter to limit the output to audits with the specified completion
-- status: can be one of \"IN_PROGRESS\", \"COMPLETED\", \"FAILED\", or
-- \"CANCELED\".
listAuditTasks_taskStatus :: Lens.Lens' ListAuditTasks (Prelude.Maybe AuditTaskStatus)
listAuditTasks_taskStatus = Lens.lens (\ListAuditTasks' {taskStatus} -> taskStatus) (\s@ListAuditTasks' {} a -> s {taskStatus = a} :: ListAuditTasks)

-- | A filter to limit the output to the specified type of audit: can be one
-- of \"ON_DEMAND_AUDIT_TASK\" or \"SCHEDULED__AUDIT_TASK\".
listAuditTasks_taskType :: Lens.Lens' ListAuditTasks (Prelude.Maybe AuditTaskType)
listAuditTasks_taskType = Lens.lens (\ListAuditTasks' {taskType} -> taskType) (\s@ListAuditTasks' {} a -> s {taskType = a} :: ListAuditTasks)

-- | The beginning of the time period. Audit information is retained for a
-- limited time (90 days). Requesting a start time prior to what is
-- retained results in an \"InvalidRequestException\".
listAuditTasks_startTime :: Lens.Lens' ListAuditTasks Prelude.UTCTime
listAuditTasks_startTime = Lens.lens (\ListAuditTasks' {startTime} -> startTime) (\s@ListAuditTasks' {} a -> s {startTime = a} :: ListAuditTasks) Prelude.. Data._Time

-- | The end of the time period.
listAuditTasks_endTime :: Lens.Lens' ListAuditTasks Prelude.UTCTime
listAuditTasks_endTime = Lens.lens (\ListAuditTasks' {endTime} -> endTime) (\s@ListAuditTasks' {} a -> s {endTime = a} :: ListAuditTasks) Prelude.. Data._Time

instance Core.AWSPager ListAuditTasks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAuditTasksResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAuditTasksResponse_tasks
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listAuditTasks_nextToken
          Lens..~ rs
          Lens.^? listAuditTasksResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListAuditTasks where
  type
    AWSResponse ListAuditTasks =
      ListAuditTasksResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAuditTasksResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "tasks" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAuditTasks where
  hashWithSalt _salt ListAuditTasks' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` taskStatus
      `Prelude.hashWithSalt` taskType
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` endTime

instance Prelude.NFData ListAuditTasks where
  rnf ListAuditTasks' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf taskStatus
      `Prelude.seq` Prelude.rnf taskType
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf endTime

instance Data.ToHeaders ListAuditTasks where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListAuditTasks where
  toPath = Prelude.const "/audit/tasks"

instance Data.ToQuery ListAuditTasks where
  toQuery ListAuditTasks' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "taskStatus" Data.=: taskStatus,
        "taskType" Data.=: taskType,
        "startTime" Data.=: startTime,
        "endTime" Data.=: endTime
      ]

-- | /See:/ 'newListAuditTasksResponse' smart constructor.
data ListAuditTasksResponse = ListAuditTasksResponse'
  { -- | A token that can be used to retrieve the next set of results, or @null@
    -- if there are no additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The audits that were performed during the specified time period.
    tasks :: Prelude.Maybe [AuditTaskMetadata],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListAuditTasksResponse
newListAuditTasksResponse pHttpStatus_ =
  ListAuditTasksResponse'
    { nextToken =
        Prelude.Nothing,
      tasks = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that can be used to retrieve the next set of results, or @null@
-- if there are no additional results.
listAuditTasksResponse_nextToken :: Lens.Lens' ListAuditTasksResponse (Prelude.Maybe Prelude.Text)
listAuditTasksResponse_nextToken = Lens.lens (\ListAuditTasksResponse' {nextToken} -> nextToken) (\s@ListAuditTasksResponse' {} a -> s {nextToken = a} :: ListAuditTasksResponse)

-- | The audits that were performed during the specified time period.
listAuditTasksResponse_tasks :: Lens.Lens' ListAuditTasksResponse (Prelude.Maybe [AuditTaskMetadata])
listAuditTasksResponse_tasks = Lens.lens (\ListAuditTasksResponse' {tasks} -> tasks) (\s@ListAuditTasksResponse' {} a -> s {tasks = a} :: ListAuditTasksResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listAuditTasksResponse_httpStatus :: Lens.Lens' ListAuditTasksResponse Prelude.Int
listAuditTasksResponse_httpStatus = Lens.lens (\ListAuditTasksResponse' {httpStatus} -> httpStatus) (\s@ListAuditTasksResponse' {} a -> s {httpStatus = a} :: ListAuditTasksResponse)

instance Prelude.NFData ListAuditTasksResponse where
  rnf ListAuditTasksResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf tasks
      `Prelude.seq` Prelude.rnf httpStatus
