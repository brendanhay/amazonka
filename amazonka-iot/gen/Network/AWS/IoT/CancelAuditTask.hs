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
-- Module      : Network.AWS.IoT.CancelAuditTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels an audit that is in progress. The audit can be either scheduled
-- or on demand. If the audit isn\'t in progress, an
-- \"InvalidRequestException\" occurs.
module Network.AWS.IoT.CancelAuditTask
  ( -- * Creating a Request
    CancelAuditTask (..),
    newCancelAuditTask,

    -- * Request Lenses
    cancelAuditTask_taskId,

    -- * Destructuring the Response
    CancelAuditTaskResponse (..),
    newCancelAuditTaskResponse,

    -- * Response Lenses
    cancelAuditTaskResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCancelAuditTask' smart constructor.
data CancelAuditTask = CancelAuditTask'
  { -- | The ID of the audit you want to cancel. You can only cancel an audit
    -- that is \"IN_PROGRESS\".
    taskId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelAuditTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskId', 'cancelAuditTask_taskId' - The ID of the audit you want to cancel. You can only cancel an audit
-- that is \"IN_PROGRESS\".
newCancelAuditTask ::
  -- | 'taskId'
  Prelude.Text ->
  CancelAuditTask
newCancelAuditTask pTaskId_ =
  CancelAuditTask' {taskId = pTaskId_}

-- | The ID of the audit you want to cancel. You can only cancel an audit
-- that is \"IN_PROGRESS\".
cancelAuditTask_taskId :: Lens.Lens' CancelAuditTask Prelude.Text
cancelAuditTask_taskId = Lens.lens (\CancelAuditTask' {taskId} -> taskId) (\s@CancelAuditTask' {} a -> s {taskId = a} :: CancelAuditTask)

instance Core.AWSRequest CancelAuditTask where
  type
    AWSResponse CancelAuditTask =
      CancelAuditTaskResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          CancelAuditTaskResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelAuditTask

instance Prelude.NFData CancelAuditTask

instance Core.ToHeaders CancelAuditTask where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON CancelAuditTask where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath CancelAuditTask where
  toPath CancelAuditTask' {..} =
    Prelude.mconcat
      ["/audit/tasks/", Core.toBS taskId, "/cancel"]

instance Core.ToQuery CancelAuditTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelAuditTaskResponse' smart constructor.
data CancelAuditTaskResponse = CancelAuditTaskResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelAuditTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'cancelAuditTaskResponse_httpStatus' - The response's http status code.
newCancelAuditTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelAuditTaskResponse
newCancelAuditTaskResponse pHttpStatus_ =
  CancelAuditTaskResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
cancelAuditTaskResponse_httpStatus :: Lens.Lens' CancelAuditTaskResponse Prelude.Int
cancelAuditTaskResponse_httpStatus = Lens.lens (\CancelAuditTaskResponse' {httpStatus} -> httpStatus) (\s@CancelAuditTaskResponse' {} a -> s {httpStatus = a} :: CancelAuditTaskResponse)

instance Prelude.NFData CancelAuditTaskResponse
