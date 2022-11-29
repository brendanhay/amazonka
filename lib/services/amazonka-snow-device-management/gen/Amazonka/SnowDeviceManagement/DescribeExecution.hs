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
-- Module      : Amazonka.SnowDeviceManagement.DescribeExecution
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Checks the status of a remote task running on one or more target
-- devices.
module Amazonka.SnowDeviceManagement.DescribeExecution
  ( -- * Creating a Request
    DescribeExecution (..),
    newDescribeExecution,

    -- * Request Lenses
    describeExecution_managedDeviceId,
    describeExecution_taskId,

    -- * Destructuring the Response
    DescribeExecutionResponse (..),
    newDescribeExecutionResponse,

    -- * Response Lenses
    describeExecutionResponse_lastUpdatedAt,
    describeExecutionResponse_taskId,
    describeExecutionResponse_state,
    describeExecutionResponse_executionId,
    describeExecutionResponse_startedAt,
    describeExecutionResponse_managedDeviceId,
    describeExecutionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SnowDeviceManagement.Types

-- | /See:/ 'newDescribeExecution' smart constructor.
data DescribeExecution = DescribeExecution'
  { -- | The ID of the managed device.
    managedDeviceId :: Prelude.Text,
    -- | The ID of the task that the action is describing.
    taskId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'managedDeviceId', 'describeExecution_managedDeviceId' - The ID of the managed device.
--
-- 'taskId', 'describeExecution_taskId' - The ID of the task that the action is describing.
newDescribeExecution ::
  -- | 'managedDeviceId'
  Prelude.Text ->
  -- | 'taskId'
  Prelude.Text ->
  DescribeExecution
newDescribeExecution pManagedDeviceId_ pTaskId_ =
  DescribeExecution'
    { managedDeviceId =
        pManagedDeviceId_,
      taskId = pTaskId_
    }

-- | The ID of the managed device.
describeExecution_managedDeviceId :: Lens.Lens' DescribeExecution Prelude.Text
describeExecution_managedDeviceId = Lens.lens (\DescribeExecution' {managedDeviceId} -> managedDeviceId) (\s@DescribeExecution' {} a -> s {managedDeviceId = a} :: DescribeExecution)

-- | The ID of the task that the action is describing.
describeExecution_taskId :: Lens.Lens' DescribeExecution Prelude.Text
describeExecution_taskId = Lens.lens (\DescribeExecution' {taskId} -> taskId) (\s@DescribeExecution' {} a -> s {taskId = a} :: DescribeExecution)

instance Core.AWSRequest DescribeExecution where
  type
    AWSResponse DescribeExecution =
      DescribeExecutionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeExecutionResponse'
            Prelude.<$> (x Core..?> "lastUpdatedAt")
            Prelude.<*> (x Core..?> "taskId")
            Prelude.<*> (x Core..?> "state")
            Prelude.<*> (x Core..?> "executionId")
            Prelude.<*> (x Core..?> "startedAt")
            Prelude.<*> (x Core..?> "managedDeviceId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeExecution where
  hashWithSalt _salt DescribeExecution' {..} =
    _salt `Prelude.hashWithSalt` managedDeviceId
      `Prelude.hashWithSalt` taskId

instance Prelude.NFData DescribeExecution where
  rnf DescribeExecution' {..} =
    Prelude.rnf managedDeviceId
      `Prelude.seq` Prelude.rnf taskId

instance Core.ToHeaders DescribeExecution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeExecution where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath DescribeExecution where
  toPath DescribeExecution' {..} =
    Prelude.mconcat
      [ "/task/",
        Core.toBS taskId,
        "/execution/",
        Core.toBS managedDeviceId
      ]

instance Core.ToQuery DescribeExecution where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeExecutionResponse' smart constructor.
data DescribeExecutionResponse = DescribeExecutionResponse'
  { -- | When the status of the execution was last updated.
    lastUpdatedAt :: Prelude.Maybe Core.POSIX,
    -- | The ID of the task being executed on the device.
    taskId :: Prelude.Maybe Prelude.Text,
    -- | The current state of the execution.
    state :: Prelude.Maybe ExecutionState,
    -- | The ID of the execution.
    executionId :: Prelude.Maybe Prelude.Text,
    -- | When the execution began.
    startedAt :: Prelude.Maybe Core.POSIX,
    -- | The ID of the managed device that the task is being executed on.
    managedDeviceId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastUpdatedAt', 'describeExecutionResponse_lastUpdatedAt' - When the status of the execution was last updated.
--
-- 'taskId', 'describeExecutionResponse_taskId' - The ID of the task being executed on the device.
--
-- 'state', 'describeExecutionResponse_state' - The current state of the execution.
--
-- 'executionId', 'describeExecutionResponse_executionId' - The ID of the execution.
--
-- 'startedAt', 'describeExecutionResponse_startedAt' - When the execution began.
--
-- 'managedDeviceId', 'describeExecutionResponse_managedDeviceId' - The ID of the managed device that the task is being executed on.
--
-- 'httpStatus', 'describeExecutionResponse_httpStatus' - The response's http status code.
newDescribeExecutionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeExecutionResponse
newDescribeExecutionResponse pHttpStatus_ =
  DescribeExecutionResponse'
    { lastUpdatedAt =
        Prelude.Nothing,
      taskId = Prelude.Nothing,
      state = Prelude.Nothing,
      executionId = Prelude.Nothing,
      startedAt = Prelude.Nothing,
      managedDeviceId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | When the status of the execution was last updated.
describeExecutionResponse_lastUpdatedAt :: Lens.Lens' DescribeExecutionResponse (Prelude.Maybe Prelude.UTCTime)
describeExecutionResponse_lastUpdatedAt = Lens.lens (\DescribeExecutionResponse' {lastUpdatedAt} -> lastUpdatedAt) (\s@DescribeExecutionResponse' {} a -> s {lastUpdatedAt = a} :: DescribeExecutionResponse) Prelude.. Lens.mapping Core._Time

-- | The ID of the task being executed on the device.
describeExecutionResponse_taskId :: Lens.Lens' DescribeExecutionResponse (Prelude.Maybe Prelude.Text)
describeExecutionResponse_taskId = Lens.lens (\DescribeExecutionResponse' {taskId} -> taskId) (\s@DescribeExecutionResponse' {} a -> s {taskId = a} :: DescribeExecutionResponse)

-- | The current state of the execution.
describeExecutionResponse_state :: Lens.Lens' DescribeExecutionResponse (Prelude.Maybe ExecutionState)
describeExecutionResponse_state = Lens.lens (\DescribeExecutionResponse' {state} -> state) (\s@DescribeExecutionResponse' {} a -> s {state = a} :: DescribeExecutionResponse)

-- | The ID of the execution.
describeExecutionResponse_executionId :: Lens.Lens' DescribeExecutionResponse (Prelude.Maybe Prelude.Text)
describeExecutionResponse_executionId = Lens.lens (\DescribeExecutionResponse' {executionId} -> executionId) (\s@DescribeExecutionResponse' {} a -> s {executionId = a} :: DescribeExecutionResponse)

-- | When the execution began.
describeExecutionResponse_startedAt :: Lens.Lens' DescribeExecutionResponse (Prelude.Maybe Prelude.UTCTime)
describeExecutionResponse_startedAt = Lens.lens (\DescribeExecutionResponse' {startedAt} -> startedAt) (\s@DescribeExecutionResponse' {} a -> s {startedAt = a} :: DescribeExecutionResponse) Prelude.. Lens.mapping Core._Time

-- | The ID of the managed device that the task is being executed on.
describeExecutionResponse_managedDeviceId :: Lens.Lens' DescribeExecutionResponse (Prelude.Maybe Prelude.Text)
describeExecutionResponse_managedDeviceId = Lens.lens (\DescribeExecutionResponse' {managedDeviceId} -> managedDeviceId) (\s@DescribeExecutionResponse' {} a -> s {managedDeviceId = a} :: DescribeExecutionResponse)

-- | The response's http status code.
describeExecutionResponse_httpStatus :: Lens.Lens' DescribeExecutionResponse Prelude.Int
describeExecutionResponse_httpStatus = Lens.lens (\DescribeExecutionResponse' {httpStatus} -> httpStatus) (\s@DescribeExecutionResponse' {} a -> s {httpStatus = a} :: DescribeExecutionResponse)

instance Prelude.NFData DescribeExecutionResponse where
  rnf DescribeExecutionResponse' {..} =
    Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf taskId
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf executionId
      `Prelude.seq` Prelude.rnf startedAt
      `Prelude.seq` Prelude.rnf managedDeviceId
      `Prelude.seq` Prelude.rnf httpStatus
