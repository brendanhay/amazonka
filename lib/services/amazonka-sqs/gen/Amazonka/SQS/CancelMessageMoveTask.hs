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
-- Module      : Amazonka.SQS.CancelMessageMoveTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a specified message movement task.
--
-- -   A message movement can only be cancelled when the current status is
--     RUNNING.
--
-- -   Cancelling a message movement task does not revert the messages that
--     have already been moved. It can only stop the messages that have not
--     been moved yet.
module Amazonka.SQS.CancelMessageMoveTask
  ( -- * Creating a Request
    CancelMessageMoveTask (..),
    newCancelMessageMoveTask,

    -- * Request Lenses
    cancelMessageMoveTask_taskHandle,

    -- * Destructuring the Response
    CancelMessageMoveTaskResponse (..),
    newCancelMessageMoveTaskResponse,

    -- * Response Lenses
    cancelMessageMoveTaskResponse_approximateNumberOfMessagesMoved,
    cancelMessageMoveTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SQS.Types

-- | /See:/ 'newCancelMessageMoveTask' smart constructor.
data CancelMessageMoveTask = CancelMessageMoveTask'
  { -- | An identifier associated with a message movement task.
    taskHandle :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelMessageMoveTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskHandle', 'cancelMessageMoveTask_taskHandle' - An identifier associated with a message movement task.
newCancelMessageMoveTask ::
  -- | 'taskHandle'
  Prelude.Text ->
  CancelMessageMoveTask
newCancelMessageMoveTask pTaskHandle_ =
  CancelMessageMoveTask' {taskHandle = pTaskHandle_}

-- | An identifier associated with a message movement task.
cancelMessageMoveTask_taskHandle :: Lens.Lens' CancelMessageMoveTask Prelude.Text
cancelMessageMoveTask_taskHandle = Lens.lens (\CancelMessageMoveTask' {taskHandle} -> taskHandle) (\s@CancelMessageMoveTask' {} a -> s {taskHandle = a} :: CancelMessageMoveTask)

instance Core.AWSRequest CancelMessageMoveTask where
  type
    AWSResponse CancelMessageMoveTask =
      CancelMessageMoveTaskResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CancelMessageMoveTaskResult"
      ( \s h x ->
          CancelMessageMoveTaskResponse'
            Prelude.<$> (x Data..@? "ApproximateNumberOfMessagesMoved")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelMessageMoveTask where
  hashWithSalt _salt CancelMessageMoveTask' {..} =
    _salt `Prelude.hashWithSalt` taskHandle

instance Prelude.NFData CancelMessageMoveTask where
  rnf CancelMessageMoveTask' {..} =
    Prelude.rnf taskHandle

instance Data.ToHeaders CancelMessageMoveTask where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CancelMessageMoveTask where
  toPath = Prelude.const "/"

instance Data.ToQuery CancelMessageMoveTask where
  toQuery CancelMessageMoveTask' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CancelMessageMoveTask" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-11-05" :: Prelude.ByteString),
        "TaskHandle" Data.=: taskHandle
      ]

-- | /See:/ 'newCancelMessageMoveTaskResponse' smart constructor.
data CancelMessageMoveTaskResponse = CancelMessageMoveTaskResponse'
  { -- | The approximate number of messages already moved to the destination
    -- queue.
    approximateNumberOfMessagesMoved :: Prelude.Maybe Prelude.Integer,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelMessageMoveTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'approximateNumberOfMessagesMoved', 'cancelMessageMoveTaskResponse_approximateNumberOfMessagesMoved' - The approximate number of messages already moved to the destination
-- queue.
--
-- 'httpStatus', 'cancelMessageMoveTaskResponse_httpStatus' - The response's http status code.
newCancelMessageMoveTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelMessageMoveTaskResponse
newCancelMessageMoveTaskResponse pHttpStatus_ =
  CancelMessageMoveTaskResponse'
    { approximateNumberOfMessagesMoved =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The approximate number of messages already moved to the destination
-- queue.
cancelMessageMoveTaskResponse_approximateNumberOfMessagesMoved :: Lens.Lens' CancelMessageMoveTaskResponse (Prelude.Maybe Prelude.Integer)
cancelMessageMoveTaskResponse_approximateNumberOfMessagesMoved = Lens.lens (\CancelMessageMoveTaskResponse' {approximateNumberOfMessagesMoved} -> approximateNumberOfMessagesMoved) (\s@CancelMessageMoveTaskResponse' {} a -> s {approximateNumberOfMessagesMoved = a} :: CancelMessageMoveTaskResponse)

-- | The response's http status code.
cancelMessageMoveTaskResponse_httpStatus :: Lens.Lens' CancelMessageMoveTaskResponse Prelude.Int
cancelMessageMoveTaskResponse_httpStatus = Lens.lens (\CancelMessageMoveTaskResponse' {httpStatus} -> httpStatus) (\s@CancelMessageMoveTaskResponse' {} a -> s {httpStatus = a} :: CancelMessageMoveTaskResponse)

instance Prelude.NFData CancelMessageMoveTaskResponse where
  rnf CancelMessageMoveTaskResponse' {..} =
    Prelude.rnf approximateNumberOfMessagesMoved
      `Prelude.seq` Prelude.rnf httpStatus
