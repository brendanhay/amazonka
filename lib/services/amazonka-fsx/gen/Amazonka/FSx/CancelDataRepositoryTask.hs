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
-- Module      : Amazonka.FSx.CancelDataRepositoryTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels an existing Amazon FSx for Lustre data repository task if that
-- task is in either the @PENDING@ or @EXECUTING@ state. When you cancel a
-- task, Amazon FSx does the following.
--
-- -   Any files that FSx has already exported are not reverted.
--
-- -   FSx continues to export any files that are \"in-flight\" when the
--     cancel operation is received.
--
-- -   FSx does not export any files that have not yet been exported.
module Amazonka.FSx.CancelDataRepositoryTask
  ( -- * Creating a Request
    CancelDataRepositoryTask (..),
    newCancelDataRepositoryTask,

    -- * Request Lenses
    cancelDataRepositoryTask_taskId,

    -- * Destructuring the Response
    CancelDataRepositoryTaskResponse (..),
    newCancelDataRepositoryTaskResponse,

    -- * Response Lenses
    cancelDataRepositoryTaskResponse_lifecycle,
    cancelDataRepositoryTaskResponse_taskId,
    cancelDataRepositoryTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Cancels a data repository task.
--
-- /See:/ 'newCancelDataRepositoryTask' smart constructor.
data CancelDataRepositoryTask = CancelDataRepositoryTask'
  { -- | Specifies the data repository task to cancel.
    taskId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelDataRepositoryTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskId', 'cancelDataRepositoryTask_taskId' - Specifies the data repository task to cancel.
newCancelDataRepositoryTask ::
  -- | 'taskId'
  Prelude.Text ->
  CancelDataRepositoryTask
newCancelDataRepositoryTask pTaskId_ =
  CancelDataRepositoryTask' {taskId = pTaskId_}

-- | Specifies the data repository task to cancel.
cancelDataRepositoryTask_taskId :: Lens.Lens' CancelDataRepositoryTask Prelude.Text
cancelDataRepositoryTask_taskId = Lens.lens (\CancelDataRepositoryTask' {taskId} -> taskId) (\s@CancelDataRepositoryTask' {} a -> s {taskId = a} :: CancelDataRepositoryTask)

instance Core.AWSRequest CancelDataRepositoryTask where
  type
    AWSResponse CancelDataRepositoryTask =
      CancelDataRepositoryTaskResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelDataRepositoryTaskResponse'
            Prelude.<$> (x Data..?> "Lifecycle")
            Prelude.<*> (x Data..?> "TaskId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelDataRepositoryTask where
  hashWithSalt _salt CancelDataRepositoryTask' {..} =
    _salt `Prelude.hashWithSalt` taskId

instance Prelude.NFData CancelDataRepositoryTask where
  rnf CancelDataRepositoryTask' {..} =
    Prelude.rnf taskId

instance Data.ToHeaders CancelDataRepositoryTask where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSSimbaAPIService_v20180301.CancelDataRepositoryTask" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CancelDataRepositoryTask where
  toJSON CancelDataRepositoryTask' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("TaskId" Data..= taskId)]
      )

instance Data.ToPath CancelDataRepositoryTask where
  toPath = Prelude.const "/"

instance Data.ToQuery CancelDataRepositoryTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelDataRepositoryTaskResponse' smart constructor.
data CancelDataRepositoryTaskResponse = CancelDataRepositoryTaskResponse'
  { -- | The lifecycle status of the data repository task, as follows:
    --
    -- -   @PENDING@ - Amazon FSx has not started the task.
    --
    -- -   @EXECUTING@ - Amazon FSx is processing the task.
    --
    -- -   @FAILED@ - Amazon FSx was not able to complete the task. For
    --     example, there may be files the task failed to process. The
    --     DataRepositoryTaskFailureDetails property provides more information
    --     about task failures.
    --
    -- -   @SUCCEEDED@ - FSx completed the task successfully.
    --
    -- -   @CANCELED@ - Amazon FSx canceled the task and it did not complete.
    --
    -- -   @CANCELING@ - FSx is in process of canceling the task.
    lifecycle :: Prelude.Maybe DataRepositoryTaskLifecycle,
    -- | The ID of the task being canceled.
    taskId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelDataRepositoryTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lifecycle', 'cancelDataRepositoryTaskResponse_lifecycle' - The lifecycle status of the data repository task, as follows:
--
-- -   @PENDING@ - Amazon FSx has not started the task.
--
-- -   @EXECUTING@ - Amazon FSx is processing the task.
--
-- -   @FAILED@ - Amazon FSx was not able to complete the task. For
--     example, there may be files the task failed to process. The
--     DataRepositoryTaskFailureDetails property provides more information
--     about task failures.
--
-- -   @SUCCEEDED@ - FSx completed the task successfully.
--
-- -   @CANCELED@ - Amazon FSx canceled the task and it did not complete.
--
-- -   @CANCELING@ - FSx is in process of canceling the task.
--
-- 'taskId', 'cancelDataRepositoryTaskResponse_taskId' - The ID of the task being canceled.
--
-- 'httpStatus', 'cancelDataRepositoryTaskResponse_httpStatus' - The response's http status code.
newCancelDataRepositoryTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelDataRepositoryTaskResponse
newCancelDataRepositoryTaskResponse pHttpStatus_ =
  CancelDataRepositoryTaskResponse'
    { lifecycle =
        Prelude.Nothing,
      taskId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The lifecycle status of the data repository task, as follows:
--
-- -   @PENDING@ - Amazon FSx has not started the task.
--
-- -   @EXECUTING@ - Amazon FSx is processing the task.
--
-- -   @FAILED@ - Amazon FSx was not able to complete the task. For
--     example, there may be files the task failed to process. The
--     DataRepositoryTaskFailureDetails property provides more information
--     about task failures.
--
-- -   @SUCCEEDED@ - FSx completed the task successfully.
--
-- -   @CANCELED@ - Amazon FSx canceled the task and it did not complete.
--
-- -   @CANCELING@ - FSx is in process of canceling the task.
cancelDataRepositoryTaskResponse_lifecycle :: Lens.Lens' CancelDataRepositoryTaskResponse (Prelude.Maybe DataRepositoryTaskLifecycle)
cancelDataRepositoryTaskResponse_lifecycle = Lens.lens (\CancelDataRepositoryTaskResponse' {lifecycle} -> lifecycle) (\s@CancelDataRepositoryTaskResponse' {} a -> s {lifecycle = a} :: CancelDataRepositoryTaskResponse)

-- | The ID of the task being canceled.
cancelDataRepositoryTaskResponse_taskId :: Lens.Lens' CancelDataRepositoryTaskResponse (Prelude.Maybe Prelude.Text)
cancelDataRepositoryTaskResponse_taskId = Lens.lens (\CancelDataRepositoryTaskResponse' {taskId} -> taskId) (\s@CancelDataRepositoryTaskResponse' {} a -> s {taskId = a} :: CancelDataRepositoryTaskResponse)

-- | The response's http status code.
cancelDataRepositoryTaskResponse_httpStatus :: Lens.Lens' CancelDataRepositoryTaskResponse Prelude.Int
cancelDataRepositoryTaskResponse_httpStatus = Lens.lens (\CancelDataRepositoryTaskResponse' {httpStatus} -> httpStatus) (\s@CancelDataRepositoryTaskResponse' {} a -> s {httpStatus = a} :: CancelDataRepositoryTaskResponse)

instance
  Prelude.NFData
    CancelDataRepositoryTaskResponse
  where
  rnf CancelDataRepositoryTaskResponse' {..} =
    Prelude.rnf lifecycle
      `Prelude.seq` Prelude.rnf taskId
      `Prelude.seq` Prelude.rnf httpStatus
