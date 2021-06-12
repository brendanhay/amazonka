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
-- Module      : Network.AWS.EC2.CancelImportTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels an in-process import virtual machine or import snapshot task.
module Network.AWS.EC2.CancelImportTask
  ( -- * Creating a Request
    CancelImportTask (..),
    newCancelImportTask,

    -- * Request Lenses
    cancelImportTask_dryRun,
    cancelImportTask_importTaskId,
    cancelImportTask_cancelReason,

    -- * Destructuring the Response
    CancelImportTaskResponse (..),
    newCancelImportTaskResponse,

    -- * Response Lenses
    cancelImportTaskResponse_importTaskId,
    cancelImportTaskResponse_state,
    cancelImportTaskResponse_previousState,
    cancelImportTaskResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCancelImportTask' smart constructor.
data CancelImportTask = CancelImportTask'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the import image or import snapshot task to be canceled.
    importTaskId :: Core.Maybe Core.Text,
    -- | The reason for canceling the task.
    cancelReason :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CancelImportTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'cancelImportTask_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'importTaskId', 'cancelImportTask_importTaskId' - The ID of the import image or import snapshot task to be canceled.
--
-- 'cancelReason', 'cancelImportTask_cancelReason' - The reason for canceling the task.
newCancelImportTask ::
  CancelImportTask
newCancelImportTask =
  CancelImportTask'
    { dryRun = Core.Nothing,
      importTaskId = Core.Nothing,
      cancelReason = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
cancelImportTask_dryRun :: Lens.Lens' CancelImportTask (Core.Maybe Core.Bool)
cancelImportTask_dryRun = Lens.lens (\CancelImportTask' {dryRun} -> dryRun) (\s@CancelImportTask' {} a -> s {dryRun = a} :: CancelImportTask)

-- | The ID of the import image or import snapshot task to be canceled.
cancelImportTask_importTaskId :: Lens.Lens' CancelImportTask (Core.Maybe Core.Text)
cancelImportTask_importTaskId = Lens.lens (\CancelImportTask' {importTaskId} -> importTaskId) (\s@CancelImportTask' {} a -> s {importTaskId = a} :: CancelImportTask)

-- | The reason for canceling the task.
cancelImportTask_cancelReason :: Lens.Lens' CancelImportTask (Core.Maybe Core.Text)
cancelImportTask_cancelReason = Lens.lens (\CancelImportTask' {cancelReason} -> cancelReason) (\s@CancelImportTask' {} a -> s {cancelReason = a} :: CancelImportTask)

instance Core.AWSRequest CancelImportTask where
  type
    AWSResponse CancelImportTask =
      CancelImportTaskResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CancelImportTaskResponse'
            Core.<$> (x Core..@? "importTaskId")
            Core.<*> (x Core..@? "state")
            Core.<*> (x Core..@? "previousState")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CancelImportTask

instance Core.NFData CancelImportTask

instance Core.ToHeaders CancelImportTask where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CancelImportTask where
  toPath = Core.const "/"

instance Core.ToQuery CancelImportTask where
  toQuery CancelImportTask' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("CancelImportTask" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "ImportTaskId" Core.=: importTaskId,
        "CancelReason" Core.=: cancelReason
      ]

-- | /See:/ 'newCancelImportTaskResponse' smart constructor.
data CancelImportTaskResponse = CancelImportTaskResponse'
  { -- | The ID of the task being canceled.
    importTaskId :: Core.Maybe Core.Text,
    -- | The current state of the task being canceled.
    state :: Core.Maybe Core.Text,
    -- | The current state of the task being canceled.
    previousState :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CancelImportTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'importTaskId', 'cancelImportTaskResponse_importTaskId' - The ID of the task being canceled.
--
-- 'state', 'cancelImportTaskResponse_state' - The current state of the task being canceled.
--
-- 'previousState', 'cancelImportTaskResponse_previousState' - The current state of the task being canceled.
--
-- 'httpStatus', 'cancelImportTaskResponse_httpStatus' - The response's http status code.
newCancelImportTaskResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CancelImportTaskResponse
newCancelImportTaskResponse pHttpStatus_ =
  CancelImportTaskResponse'
    { importTaskId =
        Core.Nothing,
      state = Core.Nothing,
      previousState = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the task being canceled.
cancelImportTaskResponse_importTaskId :: Lens.Lens' CancelImportTaskResponse (Core.Maybe Core.Text)
cancelImportTaskResponse_importTaskId = Lens.lens (\CancelImportTaskResponse' {importTaskId} -> importTaskId) (\s@CancelImportTaskResponse' {} a -> s {importTaskId = a} :: CancelImportTaskResponse)

-- | The current state of the task being canceled.
cancelImportTaskResponse_state :: Lens.Lens' CancelImportTaskResponse (Core.Maybe Core.Text)
cancelImportTaskResponse_state = Lens.lens (\CancelImportTaskResponse' {state} -> state) (\s@CancelImportTaskResponse' {} a -> s {state = a} :: CancelImportTaskResponse)

-- | The current state of the task being canceled.
cancelImportTaskResponse_previousState :: Lens.Lens' CancelImportTaskResponse (Core.Maybe Core.Text)
cancelImportTaskResponse_previousState = Lens.lens (\CancelImportTaskResponse' {previousState} -> previousState) (\s@CancelImportTaskResponse' {} a -> s {previousState = a} :: CancelImportTaskResponse)

-- | The response's http status code.
cancelImportTaskResponse_httpStatus :: Lens.Lens' CancelImportTaskResponse Core.Int
cancelImportTaskResponse_httpStatus = Lens.lens (\CancelImportTaskResponse' {httpStatus} -> httpStatus) (\s@CancelImportTaskResponse' {} a -> s {httpStatus = a} :: CancelImportTaskResponse)

instance Core.NFData CancelImportTaskResponse
