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
-- Module      : Network.AWS.Glue.GetMLTaskRun
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details for a specific task run on a machine learning transform.
-- Machine learning task runs are asynchronous tasks that AWS Glue runs on
-- your behalf as part of various machine learning workflows. You can check
-- the stats of any task run by calling @GetMLTaskRun@ with the @TaskRunID@
-- and its parent transform\'s @TransformID@.
module Network.AWS.Glue.GetMLTaskRun
  ( -- * Creating a Request
    GetMLTaskRun (..),
    newGetMLTaskRun,

    -- * Request Lenses
    getMLTaskRun_transformId,
    getMLTaskRun_taskRunId,

    -- * Destructuring the Response
    GetMLTaskRunResponse (..),
    newGetMLTaskRunResponse,

    -- * Response Lenses
    getMLTaskRunResponse_executionTime,
    getMLTaskRunResponse_status,
    getMLTaskRunResponse_transformId,
    getMLTaskRunResponse_taskRunId,
    getMLTaskRunResponse_errorString,
    getMLTaskRunResponse_lastModifiedOn,
    getMLTaskRunResponse_logGroupName,
    getMLTaskRunResponse_completedOn,
    getMLTaskRunResponse_properties,
    getMLTaskRunResponse_startedOn,
    getMLTaskRunResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetMLTaskRun' smart constructor.
data GetMLTaskRun = GetMLTaskRun'
  { -- | The unique identifier of the machine learning transform.
    transformId :: Core.Text,
    -- | The unique identifier of the task run.
    taskRunId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetMLTaskRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transformId', 'getMLTaskRun_transformId' - The unique identifier of the machine learning transform.
--
-- 'taskRunId', 'getMLTaskRun_taskRunId' - The unique identifier of the task run.
newGetMLTaskRun ::
  -- | 'transformId'
  Core.Text ->
  -- | 'taskRunId'
  Core.Text ->
  GetMLTaskRun
newGetMLTaskRun pTransformId_ pTaskRunId_ =
  GetMLTaskRun'
    { transformId = pTransformId_,
      taskRunId = pTaskRunId_
    }

-- | The unique identifier of the machine learning transform.
getMLTaskRun_transformId :: Lens.Lens' GetMLTaskRun Core.Text
getMLTaskRun_transformId = Lens.lens (\GetMLTaskRun' {transformId} -> transformId) (\s@GetMLTaskRun' {} a -> s {transformId = a} :: GetMLTaskRun)

-- | The unique identifier of the task run.
getMLTaskRun_taskRunId :: Lens.Lens' GetMLTaskRun Core.Text
getMLTaskRun_taskRunId = Lens.lens (\GetMLTaskRun' {taskRunId} -> taskRunId) (\s@GetMLTaskRun' {} a -> s {taskRunId = a} :: GetMLTaskRun)

instance Core.AWSRequest GetMLTaskRun where
  type AWSResponse GetMLTaskRun = GetMLTaskRunResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMLTaskRunResponse'
            Core.<$> (x Core..?> "ExecutionTime")
            Core.<*> (x Core..?> "Status")
            Core.<*> (x Core..?> "TransformId")
            Core.<*> (x Core..?> "TaskRunId")
            Core.<*> (x Core..?> "ErrorString")
            Core.<*> (x Core..?> "LastModifiedOn")
            Core.<*> (x Core..?> "LogGroupName")
            Core.<*> (x Core..?> "CompletedOn")
            Core.<*> (x Core..?> "Properties")
            Core.<*> (x Core..?> "StartedOn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetMLTaskRun

instance Core.NFData GetMLTaskRun

instance Core.ToHeaders GetMLTaskRun where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.GetMLTaskRun" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetMLTaskRun where
  toJSON GetMLTaskRun' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TransformId" Core..= transformId),
            Core.Just ("TaskRunId" Core..= taskRunId)
          ]
      )

instance Core.ToPath GetMLTaskRun where
  toPath = Core.const "/"

instance Core.ToQuery GetMLTaskRun where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetMLTaskRunResponse' smart constructor.
data GetMLTaskRunResponse = GetMLTaskRunResponse'
  { -- | The amount of time (in seconds) that the task run consumed resources.
    executionTime :: Core.Maybe Core.Int,
    -- | The status for this task run.
    status :: Core.Maybe TaskStatusType,
    -- | The unique identifier of the task run.
    transformId :: Core.Maybe Core.Text,
    -- | The unique run identifier associated with this run.
    taskRunId :: Core.Maybe Core.Text,
    -- | The error strings that are associated with the task run.
    errorString :: Core.Maybe Core.Text,
    -- | The date and time when this task run was last modified.
    lastModifiedOn :: Core.Maybe Core.POSIX,
    -- | The names of the log groups that are associated with the task run.
    logGroupName :: Core.Maybe Core.Text,
    -- | The date and time when this task run was completed.
    completedOn :: Core.Maybe Core.POSIX,
    -- | The list of properties that are associated with the task run.
    properties :: Core.Maybe TaskRunProperties,
    -- | The date and time when this task run started.
    startedOn :: Core.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetMLTaskRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'executionTime', 'getMLTaskRunResponse_executionTime' - The amount of time (in seconds) that the task run consumed resources.
--
-- 'status', 'getMLTaskRunResponse_status' - The status for this task run.
--
-- 'transformId', 'getMLTaskRunResponse_transformId' - The unique identifier of the task run.
--
-- 'taskRunId', 'getMLTaskRunResponse_taskRunId' - The unique run identifier associated with this run.
--
-- 'errorString', 'getMLTaskRunResponse_errorString' - The error strings that are associated with the task run.
--
-- 'lastModifiedOn', 'getMLTaskRunResponse_lastModifiedOn' - The date and time when this task run was last modified.
--
-- 'logGroupName', 'getMLTaskRunResponse_logGroupName' - The names of the log groups that are associated with the task run.
--
-- 'completedOn', 'getMLTaskRunResponse_completedOn' - The date and time when this task run was completed.
--
-- 'properties', 'getMLTaskRunResponse_properties' - The list of properties that are associated with the task run.
--
-- 'startedOn', 'getMLTaskRunResponse_startedOn' - The date and time when this task run started.
--
-- 'httpStatus', 'getMLTaskRunResponse_httpStatus' - The response's http status code.
newGetMLTaskRunResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetMLTaskRunResponse
newGetMLTaskRunResponse pHttpStatus_ =
  GetMLTaskRunResponse'
    { executionTime = Core.Nothing,
      status = Core.Nothing,
      transformId = Core.Nothing,
      taskRunId = Core.Nothing,
      errorString = Core.Nothing,
      lastModifiedOn = Core.Nothing,
      logGroupName = Core.Nothing,
      completedOn = Core.Nothing,
      properties = Core.Nothing,
      startedOn = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The amount of time (in seconds) that the task run consumed resources.
getMLTaskRunResponse_executionTime :: Lens.Lens' GetMLTaskRunResponse (Core.Maybe Core.Int)
getMLTaskRunResponse_executionTime = Lens.lens (\GetMLTaskRunResponse' {executionTime} -> executionTime) (\s@GetMLTaskRunResponse' {} a -> s {executionTime = a} :: GetMLTaskRunResponse)

-- | The status for this task run.
getMLTaskRunResponse_status :: Lens.Lens' GetMLTaskRunResponse (Core.Maybe TaskStatusType)
getMLTaskRunResponse_status = Lens.lens (\GetMLTaskRunResponse' {status} -> status) (\s@GetMLTaskRunResponse' {} a -> s {status = a} :: GetMLTaskRunResponse)

-- | The unique identifier of the task run.
getMLTaskRunResponse_transformId :: Lens.Lens' GetMLTaskRunResponse (Core.Maybe Core.Text)
getMLTaskRunResponse_transformId = Lens.lens (\GetMLTaskRunResponse' {transformId} -> transformId) (\s@GetMLTaskRunResponse' {} a -> s {transformId = a} :: GetMLTaskRunResponse)

-- | The unique run identifier associated with this run.
getMLTaskRunResponse_taskRunId :: Lens.Lens' GetMLTaskRunResponse (Core.Maybe Core.Text)
getMLTaskRunResponse_taskRunId = Lens.lens (\GetMLTaskRunResponse' {taskRunId} -> taskRunId) (\s@GetMLTaskRunResponse' {} a -> s {taskRunId = a} :: GetMLTaskRunResponse)

-- | The error strings that are associated with the task run.
getMLTaskRunResponse_errorString :: Lens.Lens' GetMLTaskRunResponse (Core.Maybe Core.Text)
getMLTaskRunResponse_errorString = Lens.lens (\GetMLTaskRunResponse' {errorString} -> errorString) (\s@GetMLTaskRunResponse' {} a -> s {errorString = a} :: GetMLTaskRunResponse)

-- | The date and time when this task run was last modified.
getMLTaskRunResponse_lastModifiedOn :: Lens.Lens' GetMLTaskRunResponse (Core.Maybe Core.UTCTime)
getMLTaskRunResponse_lastModifiedOn = Lens.lens (\GetMLTaskRunResponse' {lastModifiedOn} -> lastModifiedOn) (\s@GetMLTaskRunResponse' {} a -> s {lastModifiedOn = a} :: GetMLTaskRunResponse) Core.. Lens.mapping Core._Time

-- | The names of the log groups that are associated with the task run.
getMLTaskRunResponse_logGroupName :: Lens.Lens' GetMLTaskRunResponse (Core.Maybe Core.Text)
getMLTaskRunResponse_logGroupName = Lens.lens (\GetMLTaskRunResponse' {logGroupName} -> logGroupName) (\s@GetMLTaskRunResponse' {} a -> s {logGroupName = a} :: GetMLTaskRunResponse)

-- | The date and time when this task run was completed.
getMLTaskRunResponse_completedOn :: Lens.Lens' GetMLTaskRunResponse (Core.Maybe Core.UTCTime)
getMLTaskRunResponse_completedOn = Lens.lens (\GetMLTaskRunResponse' {completedOn} -> completedOn) (\s@GetMLTaskRunResponse' {} a -> s {completedOn = a} :: GetMLTaskRunResponse) Core.. Lens.mapping Core._Time

-- | The list of properties that are associated with the task run.
getMLTaskRunResponse_properties :: Lens.Lens' GetMLTaskRunResponse (Core.Maybe TaskRunProperties)
getMLTaskRunResponse_properties = Lens.lens (\GetMLTaskRunResponse' {properties} -> properties) (\s@GetMLTaskRunResponse' {} a -> s {properties = a} :: GetMLTaskRunResponse)

-- | The date and time when this task run started.
getMLTaskRunResponse_startedOn :: Lens.Lens' GetMLTaskRunResponse (Core.Maybe Core.UTCTime)
getMLTaskRunResponse_startedOn = Lens.lens (\GetMLTaskRunResponse' {startedOn} -> startedOn) (\s@GetMLTaskRunResponse' {} a -> s {startedOn = a} :: GetMLTaskRunResponse) Core.. Lens.mapping Core._Time

-- | The response's http status code.
getMLTaskRunResponse_httpStatus :: Lens.Lens' GetMLTaskRunResponse Core.Int
getMLTaskRunResponse_httpStatus = Lens.lens (\GetMLTaskRunResponse' {httpStatus} -> httpStatus) (\s@GetMLTaskRunResponse' {} a -> s {httpStatus = a} :: GetMLTaskRunResponse)

instance Core.NFData GetMLTaskRunResponse
