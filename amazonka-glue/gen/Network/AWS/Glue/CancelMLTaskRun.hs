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
-- Module      : Network.AWS.Glue.CancelMLTaskRun
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels (stops) a task run. Machine learning task runs are asynchronous
-- tasks that AWS Glue runs on your behalf as part of various machine
-- learning workflows. You can cancel a machine learning task run at any
-- time by calling @CancelMLTaskRun@ with a task run\'s parent transform\'s
-- @TransformID@ and the task run\'s @TaskRunId@.
module Network.AWS.Glue.CancelMLTaskRun
  ( -- * Creating a Request
    CancelMLTaskRun (..),
    newCancelMLTaskRun,

    -- * Request Lenses
    cancelMLTaskRun_transformId,
    cancelMLTaskRun_taskRunId,

    -- * Destructuring the Response
    CancelMLTaskRunResponse (..),
    newCancelMLTaskRunResponse,

    -- * Response Lenses
    cancelMLTaskRunResponse_status,
    cancelMLTaskRunResponse_transformId,
    cancelMLTaskRunResponse_taskRunId,
    cancelMLTaskRunResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCancelMLTaskRun' smart constructor.
data CancelMLTaskRun = CancelMLTaskRun'
  { -- | The unique identifier of the machine learning transform.
    transformId :: Core.Text,
    -- | A unique identifier for the task run.
    taskRunId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CancelMLTaskRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transformId', 'cancelMLTaskRun_transformId' - The unique identifier of the machine learning transform.
--
-- 'taskRunId', 'cancelMLTaskRun_taskRunId' - A unique identifier for the task run.
newCancelMLTaskRun ::
  -- | 'transformId'
  Core.Text ->
  -- | 'taskRunId'
  Core.Text ->
  CancelMLTaskRun
newCancelMLTaskRun pTransformId_ pTaskRunId_ =
  CancelMLTaskRun'
    { transformId = pTransformId_,
      taskRunId = pTaskRunId_
    }

-- | The unique identifier of the machine learning transform.
cancelMLTaskRun_transformId :: Lens.Lens' CancelMLTaskRun Core.Text
cancelMLTaskRun_transformId = Lens.lens (\CancelMLTaskRun' {transformId} -> transformId) (\s@CancelMLTaskRun' {} a -> s {transformId = a} :: CancelMLTaskRun)

-- | A unique identifier for the task run.
cancelMLTaskRun_taskRunId :: Lens.Lens' CancelMLTaskRun Core.Text
cancelMLTaskRun_taskRunId = Lens.lens (\CancelMLTaskRun' {taskRunId} -> taskRunId) (\s@CancelMLTaskRun' {} a -> s {taskRunId = a} :: CancelMLTaskRun)

instance Core.AWSRequest CancelMLTaskRun where
  type
    AWSResponse CancelMLTaskRun =
      CancelMLTaskRunResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelMLTaskRunResponse'
            Core.<$> (x Core..?> "Status")
            Core.<*> (x Core..?> "TransformId")
            Core.<*> (x Core..?> "TaskRunId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CancelMLTaskRun

instance Core.NFData CancelMLTaskRun

instance Core.ToHeaders CancelMLTaskRun where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.CancelMLTaskRun" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CancelMLTaskRun where
  toJSON CancelMLTaskRun' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TransformId" Core..= transformId),
            Core.Just ("TaskRunId" Core..= taskRunId)
          ]
      )

instance Core.ToPath CancelMLTaskRun where
  toPath = Core.const "/"

instance Core.ToQuery CancelMLTaskRun where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCancelMLTaskRunResponse' smart constructor.
data CancelMLTaskRunResponse = CancelMLTaskRunResponse'
  { -- | The status for this run.
    status :: Core.Maybe TaskStatusType,
    -- | The unique identifier of the machine learning transform.
    transformId :: Core.Maybe Core.Text,
    -- | The unique identifier for the task run.
    taskRunId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CancelMLTaskRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'cancelMLTaskRunResponse_status' - The status for this run.
--
-- 'transformId', 'cancelMLTaskRunResponse_transformId' - The unique identifier of the machine learning transform.
--
-- 'taskRunId', 'cancelMLTaskRunResponse_taskRunId' - The unique identifier for the task run.
--
-- 'httpStatus', 'cancelMLTaskRunResponse_httpStatus' - The response's http status code.
newCancelMLTaskRunResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CancelMLTaskRunResponse
newCancelMLTaskRunResponse pHttpStatus_ =
  CancelMLTaskRunResponse'
    { status = Core.Nothing,
      transformId = Core.Nothing,
      taskRunId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status for this run.
cancelMLTaskRunResponse_status :: Lens.Lens' CancelMLTaskRunResponse (Core.Maybe TaskStatusType)
cancelMLTaskRunResponse_status = Lens.lens (\CancelMLTaskRunResponse' {status} -> status) (\s@CancelMLTaskRunResponse' {} a -> s {status = a} :: CancelMLTaskRunResponse)

-- | The unique identifier of the machine learning transform.
cancelMLTaskRunResponse_transformId :: Lens.Lens' CancelMLTaskRunResponse (Core.Maybe Core.Text)
cancelMLTaskRunResponse_transformId = Lens.lens (\CancelMLTaskRunResponse' {transformId} -> transformId) (\s@CancelMLTaskRunResponse' {} a -> s {transformId = a} :: CancelMLTaskRunResponse)

-- | The unique identifier for the task run.
cancelMLTaskRunResponse_taskRunId :: Lens.Lens' CancelMLTaskRunResponse (Core.Maybe Core.Text)
cancelMLTaskRunResponse_taskRunId = Lens.lens (\CancelMLTaskRunResponse' {taskRunId} -> taskRunId) (\s@CancelMLTaskRunResponse' {} a -> s {taskRunId = a} :: CancelMLTaskRunResponse)

-- | The response's http status code.
cancelMLTaskRunResponse_httpStatus :: Lens.Lens' CancelMLTaskRunResponse Core.Int
cancelMLTaskRunResponse_httpStatus = Lens.lens (\CancelMLTaskRunResponse' {httpStatus} -> httpStatus) (\s@CancelMLTaskRunResponse' {} a -> s {httpStatus = a} :: CancelMLTaskRunResponse)

instance Core.NFData CancelMLTaskRunResponse
