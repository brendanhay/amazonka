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
-- Module      : Amazonka.Glue.GetMLTaskRun
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details for a specific task run on a machine learning transform.
-- Machine learning task runs are asynchronous tasks that Glue runs on your
-- behalf as part of various machine learning workflows. You can check the
-- stats of any task run by calling @GetMLTaskRun@ with the @TaskRunID@ and
-- its parent transform\'s @TransformID@.
module Amazonka.Glue.GetMLTaskRun
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
    getMLTaskRunResponse_lastModifiedOn,
    getMLTaskRunResponse_startedOn,
    getMLTaskRunResponse_properties,
    getMLTaskRunResponse_executionTime,
    getMLTaskRunResponse_status,
    getMLTaskRunResponse_transformId,
    getMLTaskRunResponse_completedOn,
    getMLTaskRunResponse_taskRunId,
    getMLTaskRunResponse_errorString,
    getMLTaskRunResponse_logGroupName,
    getMLTaskRunResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetMLTaskRun' smart constructor.
data GetMLTaskRun = GetMLTaskRun'
  { -- | The unique identifier of the machine learning transform.
    transformId :: Prelude.Text,
    -- | The unique identifier of the task run.
    taskRunId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest GetMLTaskRun where
  type AWSResponse GetMLTaskRun = GetMLTaskRunResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMLTaskRunResponse'
            Prelude.<$> (x Data..?> "LastModifiedOn")
            Prelude.<*> (x Data..?> "StartedOn")
            Prelude.<*> (x Data..?> "Properties")
            Prelude.<*> (x Data..?> "ExecutionTime")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "TransformId")
            Prelude.<*> (x Data..?> "CompletedOn")
            Prelude.<*> (x Data..?> "TaskRunId")
            Prelude.<*> (x Data..?> "ErrorString")
            Prelude.<*> (x Data..?> "LogGroupName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetMLTaskRun where
  hashWithSalt _salt GetMLTaskRun' {..} =
    _salt `Prelude.hashWithSalt` transformId
      `Prelude.hashWithSalt` taskRunId

instance Prelude.NFData GetMLTaskRun where
  rnf GetMLTaskRun' {..} =
    Prelude.rnf transformId
      `Prelude.seq` Prelude.rnf taskRunId

instance Data.ToHeaders GetMLTaskRun where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.GetMLTaskRun" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetMLTaskRun where
  toJSON GetMLTaskRun' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("TransformId" Data..= transformId),
            Prelude.Just ("TaskRunId" Data..= taskRunId)
          ]
      )

instance Data.ToPath GetMLTaskRun where
  toPath = Prelude.const "/"

instance Data.ToQuery GetMLTaskRun where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetMLTaskRunResponse' smart constructor.
data GetMLTaskRunResponse = GetMLTaskRunResponse'
  { -- | The date and time when this task run was last modified.
    lastModifiedOn :: Prelude.Maybe Data.POSIX,
    -- | The date and time when this task run started.
    startedOn :: Prelude.Maybe Data.POSIX,
    -- | The list of properties that are associated with the task run.
    properties :: Prelude.Maybe TaskRunProperties,
    -- | The amount of time (in seconds) that the task run consumed resources.
    executionTime :: Prelude.Maybe Prelude.Int,
    -- | The status for this task run.
    status :: Prelude.Maybe TaskStatusType,
    -- | The unique identifier of the task run.
    transformId :: Prelude.Maybe Prelude.Text,
    -- | The date and time when this task run was completed.
    completedOn :: Prelude.Maybe Data.POSIX,
    -- | The unique run identifier associated with this run.
    taskRunId :: Prelude.Maybe Prelude.Text,
    -- | The error strings that are associated with the task run.
    errorString :: Prelude.Maybe Prelude.Text,
    -- | The names of the log groups that are associated with the task run.
    logGroupName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMLTaskRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedOn', 'getMLTaskRunResponse_lastModifiedOn' - The date and time when this task run was last modified.
--
-- 'startedOn', 'getMLTaskRunResponse_startedOn' - The date and time when this task run started.
--
-- 'properties', 'getMLTaskRunResponse_properties' - The list of properties that are associated with the task run.
--
-- 'executionTime', 'getMLTaskRunResponse_executionTime' - The amount of time (in seconds) that the task run consumed resources.
--
-- 'status', 'getMLTaskRunResponse_status' - The status for this task run.
--
-- 'transformId', 'getMLTaskRunResponse_transformId' - The unique identifier of the task run.
--
-- 'completedOn', 'getMLTaskRunResponse_completedOn' - The date and time when this task run was completed.
--
-- 'taskRunId', 'getMLTaskRunResponse_taskRunId' - The unique run identifier associated with this run.
--
-- 'errorString', 'getMLTaskRunResponse_errorString' - The error strings that are associated with the task run.
--
-- 'logGroupName', 'getMLTaskRunResponse_logGroupName' - The names of the log groups that are associated with the task run.
--
-- 'httpStatus', 'getMLTaskRunResponse_httpStatus' - The response's http status code.
newGetMLTaskRunResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetMLTaskRunResponse
newGetMLTaskRunResponse pHttpStatus_ =
  GetMLTaskRunResponse'
    { lastModifiedOn =
        Prelude.Nothing,
      startedOn = Prelude.Nothing,
      properties = Prelude.Nothing,
      executionTime = Prelude.Nothing,
      status = Prelude.Nothing,
      transformId = Prelude.Nothing,
      completedOn = Prelude.Nothing,
      taskRunId = Prelude.Nothing,
      errorString = Prelude.Nothing,
      logGroupName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date and time when this task run was last modified.
getMLTaskRunResponse_lastModifiedOn :: Lens.Lens' GetMLTaskRunResponse (Prelude.Maybe Prelude.UTCTime)
getMLTaskRunResponse_lastModifiedOn = Lens.lens (\GetMLTaskRunResponse' {lastModifiedOn} -> lastModifiedOn) (\s@GetMLTaskRunResponse' {} a -> s {lastModifiedOn = a} :: GetMLTaskRunResponse) Prelude.. Lens.mapping Data._Time

-- | The date and time when this task run started.
getMLTaskRunResponse_startedOn :: Lens.Lens' GetMLTaskRunResponse (Prelude.Maybe Prelude.UTCTime)
getMLTaskRunResponse_startedOn = Lens.lens (\GetMLTaskRunResponse' {startedOn} -> startedOn) (\s@GetMLTaskRunResponse' {} a -> s {startedOn = a} :: GetMLTaskRunResponse) Prelude.. Lens.mapping Data._Time

-- | The list of properties that are associated with the task run.
getMLTaskRunResponse_properties :: Lens.Lens' GetMLTaskRunResponse (Prelude.Maybe TaskRunProperties)
getMLTaskRunResponse_properties = Lens.lens (\GetMLTaskRunResponse' {properties} -> properties) (\s@GetMLTaskRunResponse' {} a -> s {properties = a} :: GetMLTaskRunResponse)

-- | The amount of time (in seconds) that the task run consumed resources.
getMLTaskRunResponse_executionTime :: Lens.Lens' GetMLTaskRunResponse (Prelude.Maybe Prelude.Int)
getMLTaskRunResponse_executionTime = Lens.lens (\GetMLTaskRunResponse' {executionTime} -> executionTime) (\s@GetMLTaskRunResponse' {} a -> s {executionTime = a} :: GetMLTaskRunResponse)

-- | The status for this task run.
getMLTaskRunResponse_status :: Lens.Lens' GetMLTaskRunResponse (Prelude.Maybe TaskStatusType)
getMLTaskRunResponse_status = Lens.lens (\GetMLTaskRunResponse' {status} -> status) (\s@GetMLTaskRunResponse' {} a -> s {status = a} :: GetMLTaskRunResponse)

-- | The unique identifier of the task run.
getMLTaskRunResponse_transformId :: Lens.Lens' GetMLTaskRunResponse (Prelude.Maybe Prelude.Text)
getMLTaskRunResponse_transformId = Lens.lens (\GetMLTaskRunResponse' {transformId} -> transformId) (\s@GetMLTaskRunResponse' {} a -> s {transformId = a} :: GetMLTaskRunResponse)

-- | The date and time when this task run was completed.
getMLTaskRunResponse_completedOn :: Lens.Lens' GetMLTaskRunResponse (Prelude.Maybe Prelude.UTCTime)
getMLTaskRunResponse_completedOn = Lens.lens (\GetMLTaskRunResponse' {completedOn} -> completedOn) (\s@GetMLTaskRunResponse' {} a -> s {completedOn = a} :: GetMLTaskRunResponse) Prelude.. Lens.mapping Data._Time

-- | The unique run identifier associated with this run.
getMLTaskRunResponse_taskRunId :: Lens.Lens' GetMLTaskRunResponse (Prelude.Maybe Prelude.Text)
getMLTaskRunResponse_taskRunId = Lens.lens (\GetMLTaskRunResponse' {taskRunId} -> taskRunId) (\s@GetMLTaskRunResponse' {} a -> s {taskRunId = a} :: GetMLTaskRunResponse)

-- | The error strings that are associated with the task run.
getMLTaskRunResponse_errorString :: Lens.Lens' GetMLTaskRunResponse (Prelude.Maybe Prelude.Text)
getMLTaskRunResponse_errorString = Lens.lens (\GetMLTaskRunResponse' {errorString} -> errorString) (\s@GetMLTaskRunResponse' {} a -> s {errorString = a} :: GetMLTaskRunResponse)

-- | The names of the log groups that are associated with the task run.
getMLTaskRunResponse_logGroupName :: Lens.Lens' GetMLTaskRunResponse (Prelude.Maybe Prelude.Text)
getMLTaskRunResponse_logGroupName = Lens.lens (\GetMLTaskRunResponse' {logGroupName} -> logGroupName) (\s@GetMLTaskRunResponse' {} a -> s {logGroupName = a} :: GetMLTaskRunResponse)

-- | The response's http status code.
getMLTaskRunResponse_httpStatus :: Lens.Lens' GetMLTaskRunResponse Prelude.Int
getMLTaskRunResponse_httpStatus = Lens.lens (\GetMLTaskRunResponse' {httpStatus} -> httpStatus) (\s@GetMLTaskRunResponse' {} a -> s {httpStatus = a} :: GetMLTaskRunResponse)

instance Prelude.NFData GetMLTaskRunResponse where
  rnf GetMLTaskRunResponse' {..} =
    Prelude.rnf lastModifiedOn
      `Prelude.seq` Prelude.rnf startedOn
      `Prelude.seq` Prelude.rnf properties
      `Prelude.seq` Prelude.rnf executionTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf transformId
      `Prelude.seq` Prelude.rnf completedOn
      `Prelude.seq` Prelude.rnf taskRunId
      `Prelude.seq` Prelude.rnf errorString
      `Prelude.seq` Prelude.rnf logGroupName
      `Prelude.seq` Prelude.rnf httpStatus
