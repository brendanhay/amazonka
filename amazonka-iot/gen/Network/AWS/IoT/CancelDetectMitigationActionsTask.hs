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
-- Module      : Network.AWS.IoT.CancelDetectMitigationActionsTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a Device Defender ML Detect mitigation action.
module Network.AWS.IoT.CancelDetectMitigationActionsTask
  ( -- * Creating a Request
    CancelDetectMitigationActionsTask (..),
    newCancelDetectMitigationActionsTask,

    -- * Request Lenses
    cancelDetectMitigationActionsTask_taskId,

    -- * Destructuring the Response
    CancelDetectMitigationActionsTaskResponse (..),
    newCancelDetectMitigationActionsTaskResponse,

    -- * Response Lenses
    cancelDetectMitigationActionsTaskResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCancelDetectMitigationActionsTask' smart constructor.
data CancelDetectMitigationActionsTask = CancelDetectMitigationActionsTask'
  { -- | The unique identifier of the task.
    taskId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CancelDetectMitigationActionsTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskId', 'cancelDetectMitigationActionsTask_taskId' - The unique identifier of the task.
newCancelDetectMitigationActionsTask ::
  -- | 'taskId'
  Core.Text ->
  CancelDetectMitigationActionsTask
newCancelDetectMitigationActionsTask pTaskId_ =
  CancelDetectMitigationActionsTask'
    { taskId =
        pTaskId_
    }

-- | The unique identifier of the task.
cancelDetectMitigationActionsTask_taskId :: Lens.Lens' CancelDetectMitigationActionsTask Core.Text
cancelDetectMitigationActionsTask_taskId = Lens.lens (\CancelDetectMitigationActionsTask' {taskId} -> taskId) (\s@CancelDetectMitigationActionsTask' {} a -> s {taskId = a} :: CancelDetectMitigationActionsTask)

instance
  Core.AWSRequest
    CancelDetectMitigationActionsTask
  where
  type
    AWSResponse CancelDetectMitigationActionsTask =
      CancelDetectMitigationActionsTaskResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          CancelDetectMitigationActionsTaskResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    CancelDetectMitigationActionsTask

instance
  Core.NFData
    CancelDetectMitigationActionsTask

instance
  Core.ToHeaders
    CancelDetectMitigationActionsTask
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToJSON
    CancelDetectMitigationActionsTask
  where
  toJSON = Core.const (Core.Object Core.mempty)

instance
  Core.ToPath
    CancelDetectMitigationActionsTask
  where
  toPath CancelDetectMitigationActionsTask' {..} =
    Core.mconcat
      [ "/detect/mitigationactions/tasks/",
        Core.toBS taskId,
        "/cancel"
      ]

instance
  Core.ToQuery
    CancelDetectMitigationActionsTask
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCancelDetectMitigationActionsTaskResponse' smart constructor.
data CancelDetectMitigationActionsTaskResponse = CancelDetectMitigationActionsTaskResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CancelDetectMitigationActionsTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'cancelDetectMitigationActionsTaskResponse_httpStatus' - The response's http status code.
newCancelDetectMitigationActionsTaskResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CancelDetectMitigationActionsTaskResponse
newCancelDetectMitigationActionsTaskResponse
  pHttpStatus_ =
    CancelDetectMitigationActionsTaskResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
cancelDetectMitigationActionsTaskResponse_httpStatus :: Lens.Lens' CancelDetectMitigationActionsTaskResponse Core.Int
cancelDetectMitigationActionsTaskResponse_httpStatus = Lens.lens (\CancelDetectMitigationActionsTaskResponse' {httpStatus} -> httpStatus) (\s@CancelDetectMitigationActionsTaskResponse' {} a -> s {httpStatus = a} :: CancelDetectMitigationActionsTaskResponse)

instance
  Core.NFData
    CancelDetectMitigationActionsTaskResponse
