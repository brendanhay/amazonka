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
-- Module      : Amazonka.SnowDeviceManagement.CancelTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends a cancel request for a specified task. You can cancel a task only
-- if it\'s still in a @QUEUED@ state. Tasks that are already running
-- can\'t be cancelled.
--
-- A task might still run if it\'s processed from the queue before the
-- @CancelTask@ operation changes the task\'s state.
module Amazonka.SnowDeviceManagement.CancelTask
  ( -- * Creating a Request
    CancelTask (..),
    newCancelTask,

    -- * Request Lenses
    cancelTask_taskId,

    -- * Destructuring the Response
    CancelTaskResponse (..),
    newCancelTaskResponse,

    -- * Response Lenses
    cancelTaskResponse_taskId,
    cancelTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SnowDeviceManagement.Types

-- | /See:/ 'newCancelTask' smart constructor.
data CancelTask = CancelTask'
  { -- | The ID of the task that you are attempting to cancel. You can retrieve a
    -- task ID by using the @ListTasks@ operation.
    taskId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskId', 'cancelTask_taskId' - The ID of the task that you are attempting to cancel. You can retrieve a
-- task ID by using the @ListTasks@ operation.
newCancelTask ::
  -- | 'taskId'
  Prelude.Text ->
  CancelTask
newCancelTask pTaskId_ =
  CancelTask' {taskId = pTaskId_}

-- | The ID of the task that you are attempting to cancel. You can retrieve a
-- task ID by using the @ListTasks@ operation.
cancelTask_taskId :: Lens.Lens' CancelTask Prelude.Text
cancelTask_taskId = Lens.lens (\CancelTask' {taskId} -> taskId) (\s@CancelTask' {} a -> s {taskId = a} :: CancelTask)

instance Core.AWSRequest CancelTask where
  type AWSResponse CancelTask = CancelTaskResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelTaskResponse'
            Prelude.<$> (x Data..?> "taskId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelTask where
  hashWithSalt _salt CancelTask' {..} =
    _salt `Prelude.hashWithSalt` taskId

instance Prelude.NFData CancelTask where
  rnf CancelTask' {..} = Prelude.rnf taskId

instance Data.ToHeaders CancelTask where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CancelTask where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath CancelTask where
  toPath CancelTask' {..} =
    Prelude.mconcat
      ["/task/", Data.toBS taskId, "/cancel"]

instance Data.ToQuery CancelTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelTaskResponse' smart constructor.
data CancelTaskResponse = CancelTaskResponse'
  { -- | The ID of the task that you are attempting to cancel.
    taskId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskId', 'cancelTaskResponse_taskId' - The ID of the task that you are attempting to cancel.
--
-- 'httpStatus', 'cancelTaskResponse_httpStatus' - The response's http status code.
newCancelTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelTaskResponse
newCancelTaskResponse pHttpStatus_ =
  CancelTaskResponse'
    { taskId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the task that you are attempting to cancel.
cancelTaskResponse_taskId :: Lens.Lens' CancelTaskResponse (Prelude.Maybe Prelude.Text)
cancelTaskResponse_taskId = Lens.lens (\CancelTaskResponse' {taskId} -> taskId) (\s@CancelTaskResponse' {} a -> s {taskId = a} :: CancelTaskResponse)

-- | The response's http status code.
cancelTaskResponse_httpStatus :: Lens.Lens' CancelTaskResponse Prelude.Int
cancelTaskResponse_httpStatus = Lens.lens (\CancelTaskResponse' {httpStatus} -> httpStatus) (\s@CancelTaskResponse' {} a -> s {httpStatus = a} :: CancelTaskResponse)

instance Prelude.NFData CancelTaskResponse where
  rnf CancelTaskResponse' {..} =
    Prelude.rnf taskId `Prelude.seq`
      Prelude.rnf httpStatus
