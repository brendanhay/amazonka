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

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetMLTaskRun' smart constructor.
data GetMLTaskRun = GetMLTaskRun'
  { -- | The unique identifier of the machine learning transform.
    transformId :: Prelude.Text,
    -- | The unique identifier of the task run.
    taskRunId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'taskRunId'
  Prelude.Text ->
  GetMLTaskRun
newGetMLTaskRun pTransformId_ pTaskRunId_ =
  GetMLTaskRun'
    { transformId = pTransformId_,
      taskRunId = pTaskRunId_
    }

-- | The unique identifier of the machine learning transform.
getMLTaskRun_transformId :: Lens.Lens' GetMLTaskRun Prelude.Text
getMLTaskRun_transformId = Lens.lens (\GetMLTaskRun' {transformId} -> transformId) (\s@GetMLTaskRun' {} a -> s {transformId = a} :: GetMLTaskRun)

-- | The unique identifier of the task run.
getMLTaskRun_taskRunId :: Lens.Lens' GetMLTaskRun Prelude.Text
getMLTaskRun_taskRunId = Lens.lens (\GetMLTaskRun' {taskRunId} -> taskRunId) (\s@GetMLTaskRun' {} a -> s {taskRunId = a} :: GetMLTaskRun)

instance Prelude.AWSRequest GetMLTaskRun where
  type Rs GetMLTaskRun = GetMLTaskRunResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMLTaskRunResponse'
            Prelude.<$> (x Prelude..?> "ExecutionTime")
            Prelude.<*> (x Prelude..?> "Status")
            Prelude.<*> (x Prelude..?> "TransformId")
            Prelude.<*> (x Prelude..?> "TaskRunId")
            Prelude.<*> (x Prelude..?> "ErrorString")
            Prelude.<*> (x Prelude..?> "LastModifiedOn")
            Prelude.<*> (x Prelude..?> "LogGroupName")
            Prelude.<*> (x Prelude..?> "CompletedOn")
            Prelude.<*> (x Prelude..?> "Properties")
            Prelude.<*> (x Prelude..?> "StartedOn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetMLTaskRun

instance Prelude.NFData GetMLTaskRun

instance Prelude.ToHeaders GetMLTaskRun where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("AWSGlue.GetMLTaskRun" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetMLTaskRun where
  toJSON GetMLTaskRun' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("TransformId" Prelude..= transformId),
            Prelude.Just ("TaskRunId" Prelude..= taskRunId)
          ]
      )

instance Prelude.ToPath GetMLTaskRun where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetMLTaskRun where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetMLTaskRunResponse' smart constructor.
data GetMLTaskRunResponse = GetMLTaskRunResponse'
  { -- | The amount of time (in seconds) that the task run consumed resources.
    executionTime :: Prelude.Maybe Prelude.Int,
    -- | The status for this task run.
    status :: Prelude.Maybe TaskStatusType,
    -- | The unique identifier of the task run.
    transformId :: Prelude.Maybe Prelude.Text,
    -- | The unique run identifier associated with this run.
    taskRunId :: Prelude.Maybe Prelude.Text,
    -- | The error strings that are associated with the task run.
    errorString :: Prelude.Maybe Prelude.Text,
    -- | The date and time when this task run was last modified.
    lastModifiedOn :: Prelude.Maybe Prelude.POSIX,
    -- | The names of the log groups that are associated with the task run.
    logGroupName :: Prelude.Maybe Prelude.Text,
    -- | The date and time when this task run was completed.
    completedOn :: Prelude.Maybe Prelude.POSIX,
    -- | The list of properties that are associated with the task run.
    properties :: Prelude.Maybe TaskRunProperties,
    -- | The date and time when this task run started.
    startedOn :: Prelude.Maybe Prelude.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  GetMLTaskRunResponse
newGetMLTaskRunResponse pHttpStatus_ =
  GetMLTaskRunResponse'
    { executionTime =
        Prelude.Nothing,
      status = Prelude.Nothing,
      transformId = Prelude.Nothing,
      taskRunId = Prelude.Nothing,
      errorString = Prelude.Nothing,
      lastModifiedOn = Prelude.Nothing,
      logGroupName = Prelude.Nothing,
      completedOn = Prelude.Nothing,
      properties = Prelude.Nothing,
      startedOn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The amount of time (in seconds) that the task run consumed resources.
getMLTaskRunResponse_executionTime :: Lens.Lens' GetMLTaskRunResponse (Prelude.Maybe Prelude.Int)
getMLTaskRunResponse_executionTime = Lens.lens (\GetMLTaskRunResponse' {executionTime} -> executionTime) (\s@GetMLTaskRunResponse' {} a -> s {executionTime = a} :: GetMLTaskRunResponse)

-- | The status for this task run.
getMLTaskRunResponse_status :: Lens.Lens' GetMLTaskRunResponse (Prelude.Maybe TaskStatusType)
getMLTaskRunResponse_status = Lens.lens (\GetMLTaskRunResponse' {status} -> status) (\s@GetMLTaskRunResponse' {} a -> s {status = a} :: GetMLTaskRunResponse)

-- | The unique identifier of the task run.
getMLTaskRunResponse_transformId :: Lens.Lens' GetMLTaskRunResponse (Prelude.Maybe Prelude.Text)
getMLTaskRunResponse_transformId = Lens.lens (\GetMLTaskRunResponse' {transformId} -> transformId) (\s@GetMLTaskRunResponse' {} a -> s {transformId = a} :: GetMLTaskRunResponse)

-- | The unique run identifier associated with this run.
getMLTaskRunResponse_taskRunId :: Lens.Lens' GetMLTaskRunResponse (Prelude.Maybe Prelude.Text)
getMLTaskRunResponse_taskRunId = Lens.lens (\GetMLTaskRunResponse' {taskRunId} -> taskRunId) (\s@GetMLTaskRunResponse' {} a -> s {taskRunId = a} :: GetMLTaskRunResponse)

-- | The error strings that are associated with the task run.
getMLTaskRunResponse_errorString :: Lens.Lens' GetMLTaskRunResponse (Prelude.Maybe Prelude.Text)
getMLTaskRunResponse_errorString = Lens.lens (\GetMLTaskRunResponse' {errorString} -> errorString) (\s@GetMLTaskRunResponse' {} a -> s {errorString = a} :: GetMLTaskRunResponse)

-- | The date and time when this task run was last modified.
getMLTaskRunResponse_lastModifiedOn :: Lens.Lens' GetMLTaskRunResponse (Prelude.Maybe Prelude.UTCTime)
getMLTaskRunResponse_lastModifiedOn = Lens.lens (\GetMLTaskRunResponse' {lastModifiedOn} -> lastModifiedOn) (\s@GetMLTaskRunResponse' {} a -> s {lastModifiedOn = a} :: GetMLTaskRunResponse) Prelude.. Lens.mapping Prelude._Time

-- | The names of the log groups that are associated with the task run.
getMLTaskRunResponse_logGroupName :: Lens.Lens' GetMLTaskRunResponse (Prelude.Maybe Prelude.Text)
getMLTaskRunResponse_logGroupName = Lens.lens (\GetMLTaskRunResponse' {logGroupName} -> logGroupName) (\s@GetMLTaskRunResponse' {} a -> s {logGroupName = a} :: GetMLTaskRunResponse)

-- | The date and time when this task run was completed.
getMLTaskRunResponse_completedOn :: Lens.Lens' GetMLTaskRunResponse (Prelude.Maybe Prelude.UTCTime)
getMLTaskRunResponse_completedOn = Lens.lens (\GetMLTaskRunResponse' {completedOn} -> completedOn) (\s@GetMLTaskRunResponse' {} a -> s {completedOn = a} :: GetMLTaskRunResponse) Prelude.. Lens.mapping Prelude._Time

-- | The list of properties that are associated with the task run.
getMLTaskRunResponse_properties :: Lens.Lens' GetMLTaskRunResponse (Prelude.Maybe TaskRunProperties)
getMLTaskRunResponse_properties = Lens.lens (\GetMLTaskRunResponse' {properties} -> properties) (\s@GetMLTaskRunResponse' {} a -> s {properties = a} :: GetMLTaskRunResponse)

-- | The date and time when this task run started.
getMLTaskRunResponse_startedOn :: Lens.Lens' GetMLTaskRunResponse (Prelude.Maybe Prelude.UTCTime)
getMLTaskRunResponse_startedOn = Lens.lens (\GetMLTaskRunResponse' {startedOn} -> startedOn) (\s@GetMLTaskRunResponse' {} a -> s {startedOn = a} :: GetMLTaskRunResponse) Prelude.. Lens.mapping Prelude._Time

-- | The response's http status code.
getMLTaskRunResponse_httpStatus :: Lens.Lens' GetMLTaskRunResponse Prelude.Int
getMLTaskRunResponse_httpStatus = Lens.lens (\GetMLTaskRunResponse' {httpStatus} -> httpStatus) (\s@GetMLTaskRunResponse' {} a -> s {httpStatus = a} :: GetMLTaskRunResponse)

instance Prelude.NFData GetMLTaskRunResponse
