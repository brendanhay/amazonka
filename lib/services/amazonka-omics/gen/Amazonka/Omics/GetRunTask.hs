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
-- Module      : Amazonka.Omics.GetRunTask
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a workflow run task.
module Amazonka.Omics.GetRunTask
  ( -- * Creating a Request
    GetRunTask (..),
    newGetRunTask,

    -- * Request Lenses
    getRunTask_id,
    getRunTask_taskId,

    -- * Destructuring the Response
    GetRunTaskResponse (..),
    newGetRunTaskResponse,

    -- * Response Lenses
    getRunTaskResponse_cpus,
    getRunTaskResponse_creationTime,
    getRunTaskResponse_logStream,
    getRunTaskResponse_memory,
    getRunTaskResponse_name,
    getRunTaskResponse_startTime,
    getRunTaskResponse_status,
    getRunTaskResponse_statusMessage,
    getRunTaskResponse_stopTime,
    getRunTaskResponse_taskId,
    getRunTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetRunTask' smart constructor.
data GetRunTask = GetRunTask'
  { -- | The task\'s ID.
    id :: Prelude.Text,
    -- | The task\'s ID.
    taskId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRunTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getRunTask_id' - The task\'s ID.
--
-- 'taskId', 'getRunTask_taskId' - The task\'s ID.
newGetRunTask ::
  -- | 'id'
  Prelude.Text ->
  -- | 'taskId'
  Prelude.Text ->
  GetRunTask
newGetRunTask pId_ pTaskId_ =
  GetRunTask' {id = pId_, taskId = pTaskId_}

-- | The task\'s ID.
getRunTask_id :: Lens.Lens' GetRunTask Prelude.Text
getRunTask_id = Lens.lens (\GetRunTask' {id} -> id) (\s@GetRunTask' {} a -> s {id = a} :: GetRunTask)

-- | The task\'s ID.
getRunTask_taskId :: Lens.Lens' GetRunTask Prelude.Text
getRunTask_taskId = Lens.lens (\GetRunTask' {taskId} -> taskId) (\s@GetRunTask' {} a -> s {taskId = a} :: GetRunTask)

instance Core.AWSRequest GetRunTask where
  type AWSResponse GetRunTask = GetRunTaskResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRunTaskResponse'
            Prelude.<$> (x Data..?> "cpus")
            Prelude.<*> (x Data..?> "creationTime")
            Prelude.<*> (x Data..?> "logStream")
            Prelude.<*> (x Data..?> "memory")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "startTime")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "statusMessage")
            Prelude.<*> (x Data..?> "stopTime")
            Prelude.<*> (x Data..?> "taskId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRunTask where
  hashWithSalt _salt GetRunTask' {..} =
    _salt `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` taskId

instance Prelude.NFData GetRunTask where
  rnf GetRunTask' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf taskId

instance Data.ToHeaders GetRunTask where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetRunTask where
  toPath GetRunTask' {..} =
    Prelude.mconcat
      ["/run/", Data.toBS id, "/task/", Data.toBS taskId]

instance Data.ToQuery GetRunTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRunTaskResponse' smart constructor.
data GetRunTaskResponse = GetRunTaskResponse'
  { -- | The task\'s CPU usage.
    cpus :: Prelude.Maybe Prelude.Natural,
    -- | When the task was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The task\'s log stream.
    logStream :: Prelude.Maybe Prelude.Text,
    -- | The task\'s memory setting.
    memory :: Prelude.Maybe Prelude.Natural,
    -- | The task\'s name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The task\'s start time.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The task\'s status.
    status :: Prelude.Maybe TaskStatus,
    -- | The task\'s status message.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The task\'s stop time.
    stopTime :: Prelude.Maybe Data.POSIX,
    -- | The task\'s ID.
    taskId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRunTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cpus', 'getRunTaskResponse_cpus' - The task\'s CPU usage.
--
-- 'creationTime', 'getRunTaskResponse_creationTime' - When the task was created.
--
-- 'logStream', 'getRunTaskResponse_logStream' - The task\'s log stream.
--
-- 'memory', 'getRunTaskResponse_memory' - The task\'s memory setting.
--
-- 'name', 'getRunTaskResponse_name' - The task\'s name.
--
-- 'startTime', 'getRunTaskResponse_startTime' - The task\'s start time.
--
-- 'status', 'getRunTaskResponse_status' - The task\'s status.
--
-- 'statusMessage', 'getRunTaskResponse_statusMessage' - The task\'s status message.
--
-- 'stopTime', 'getRunTaskResponse_stopTime' - The task\'s stop time.
--
-- 'taskId', 'getRunTaskResponse_taskId' - The task\'s ID.
--
-- 'httpStatus', 'getRunTaskResponse_httpStatus' - The response's http status code.
newGetRunTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRunTaskResponse
newGetRunTaskResponse pHttpStatus_ =
  GetRunTaskResponse'
    { cpus = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      logStream = Prelude.Nothing,
      memory = Prelude.Nothing,
      name = Prelude.Nothing,
      startTime = Prelude.Nothing,
      status = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      stopTime = Prelude.Nothing,
      taskId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The task\'s CPU usage.
getRunTaskResponse_cpus :: Lens.Lens' GetRunTaskResponse (Prelude.Maybe Prelude.Natural)
getRunTaskResponse_cpus = Lens.lens (\GetRunTaskResponse' {cpus} -> cpus) (\s@GetRunTaskResponse' {} a -> s {cpus = a} :: GetRunTaskResponse)

-- | When the task was created.
getRunTaskResponse_creationTime :: Lens.Lens' GetRunTaskResponse (Prelude.Maybe Prelude.UTCTime)
getRunTaskResponse_creationTime = Lens.lens (\GetRunTaskResponse' {creationTime} -> creationTime) (\s@GetRunTaskResponse' {} a -> s {creationTime = a} :: GetRunTaskResponse) Prelude.. Lens.mapping Data._Time

-- | The task\'s log stream.
getRunTaskResponse_logStream :: Lens.Lens' GetRunTaskResponse (Prelude.Maybe Prelude.Text)
getRunTaskResponse_logStream = Lens.lens (\GetRunTaskResponse' {logStream} -> logStream) (\s@GetRunTaskResponse' {} a -> s {logStream = a} :: GetRunTaskResponse)

-- | The task\'s memory setting.
getRunTaskResponse_memory :: Lens.Lens' GetRunTaskResponse (Prelude.Maybe Prelude.Natural)
getRunTaskResponse_memory = Lens.lens (\GetRunTaskResponse' {memory} -> memory) (\s@GetRunTaskResponse' {} a -> s {memory = a} :: GetRunTaskResponse)

-- | The task\'s name.
getRunTaskResponse_name :: Lens.Lens' GetRunTaskResponse (Prelude.Maybe Prelude.Text)
getRunTaskResponse_name = Lens.lens (\GetRunTaskResponse' {name} -> name) (\s@GetRunTaskResponse' {} a -> s {name = a} :: GetRunTaskResponse)

-- | The task\'s start time.
getRunTaskResponse_startTime :: Lens.Lens' GetRunTaskResponse (Prelude.Maybe Prelude.UTCTime)
getRunTaskResponse_startTime = Lens.lens (\GetRunTaskResponse' {startTime} -> startTime) (\s@GetRunTaskResponse' {} a -> s {startTime = a} :: GetRunTaskResponse) Prelude.. Lens.mapping Data._Time

-- | The task\'s status.
getRunTaskResponse_status :: Lens.Lens' GetRunTaskResponse (Prelude.Maybe TaskStatus)
getRunTaskResponse_status = Lens.lens (\GetRunTaskResponse' {status} -> status) (\s@GetRunTaskResponse' {} a -> s {status = a} :: GetRunTaskResponse)

-- | The task\'s status message.
getRunTaskResponse_statusMessage :: Lens.Lens' GetRunTaskResponse (Prelude.Maybe Prelude.Text)
getRunTaskResponse_statusMessage = Lens.lens (\GetRunTaskResponse' {statusMessage} -> statusMessage) (\s@GetRunTaskResponse' {} a -> s {statusMessage = a} :: GetRunTaskResponse)

-- | The task\'s stop time.
getRunTaskResponse_stopTime :: Lens.Lens' GetRunTaskResponse (Prelude.Maybe Prelude.UTCTime)
getRunTaskResponse_stopTime = Lens.lens (\GetRunTaskResponse' {stopTime} -> stopTime) (\s@GetRunTaskResponse' {} a -> s {stopTime = a} :: GetRunTaskResponse) Prelude.. Lens.mapping Data._Time

-- | The task\'s ID.
getRunTaskResponse_taskId :: Lens.Lens' GetRunTaskResponse (Prelude.Maybe Prelude.Text)
getRunTaskResponse_taskId = Lens.lens (\GetRunTaskResponse' {taskId} -> taskId) (\s@GetRunTaskResponse' {} a -> s {taskId = a} :: GetRunTaskResponse)

-- | The response's http status code.
getRunTaskResponse_httpStatus :: Lens.Lens' GetRunTaskResponse Prelude.Int
getRunTaskResponse_httpStatus = Lens.lens (\GetRunTaskResponse' {httpStatus} -> httpStatus) (\s@GetRunTaskResponse' {} a -> s {httpStatus = a} :: GetRunTaskResponse)

instance Prelude.NFData GetRunTaskResponse where
  rnf GetRunTaskResponse' {..} =
    Prelude.rnf cpus
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf logStream
      `Prelude.seq` Prelude.rnf memory
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf stopTime
      `Prelude.seq` Prelude.rnf taskId
      `Prelude.seq` Prelude.rnf httpStatus
