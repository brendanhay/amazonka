{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoT.CancelAuditMitigationActionsTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a mitigation action task that is in progress. If the task is not
-- in progress, an InvalidRequestException occurs.
module Network.AWS.IoT.CancelAuditMitigationActionsTask
  ( -- * Creating a Request
    CancelAuditMitigationActionsTask (..),
    newCancelAuditMitigationActionsTask,

    -- * Request Lenses
    cancelAuditMitigationActionsTask_taskId,

    -- * Destructuring the Response
    CancelAuditMitigationActionsTaskResponse (..),
    newCancelAuditMitigationActionsTaskResponse,

    -- * Response Lenses
    cancelAuditMitigationActionsTaskResponse_httpStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCancelAuditMitigationActionsTask' smart constructor.
data CancelAuditMitigationActionsTask = CancelAuditMitigationActionsTask'
  { -- | The unique identifier for the task that you want to cancel.
    taskId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CancelAuditMitigationActionsTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskId', 'cancelAuditMitigationActionsTask_taskId' - The unique identifier for the task that you want to cancel.
newCancelAuditMitigationActionsTask ::
  -- | 'taskId'
  Prelude.Text ->
  CancelAuditMitigationActionsTask
newCancelAuditMitigationActionsTask pTaskId_ =
  CancelAuditMitigationActionsTask'
    { taskId =
        pTaskId_
    }

-- | The unique identifier for the task that you want to cancel.
cancelAuditMitigationActionsTask_taskId :: Lens.Lens' CancelAuditMitigationActionsTask Prelude.Text
cancelAuditMitigationActionsTask_taskId = Lens.lens (\CancelAuditMitigationActionsTask' {taskId} -> taskId) (\s@CancelAuditMitigationActionsTask' {} a -> s {taskId = a} :: CancelAuditMitigationActionsTask)

instance
  Prelude.AWSRequest
    CancelAuditMitigationActionsTask
  where
  type
    Rs CancelAuditMitigationActionsTask =
      CancelAuditMitigationActionsTaskResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          CancelAuditMitigationActionsTaskResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CancelAuditMitigationActionsTask

instance
  Prelude.NFData
    CancelAuditMitigationActionsTask

instance
  Prelude.ToHeaders
    CancelAuditMitigationActionsTask
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToJSON
    CancelAuditMitigationActionsTask
  where
  toJSON =
    Prelude.const (Prelude.Object Prelude.mempty)

instance
  Prelude.ToPath
    CancelAuditMitigationActionsTask
  where
  toPath CancelAuditMitigationActionsTask' {..} =
    Prelude.mconcat
      [ "/audit/mitigationactions/tasks/",
        Prelude.toBS taskId,
        "/cancel"
      ]

instance
  Prelude.ToQuery
    CancelAuditMitigationActionsTask
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelAuditMitigationActionsTaskResponse' smart constructor.
data CancelAuditMitigationActionsTaskResponse = CancelAuditMitigationActionsTaskResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CancelAuditMitigationActionsTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'cancelAuditMitigationActionsTaskResponse_httpStatus' - The response's http status code.
newCancelAuditMitigationActionsTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelAuditMitigationActionsTaskResponse
newCancelAuditMitigationActionsTaskResponse
  pHttpStatus_ =
    CancelAuditMitigationActionsTaskResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
cancelAuditMitigationActionsTaskResponse_httpStatus :: Lens.Lens' CancelAuditMitigationActionsTaskResponse Prelude.Int
cancelAuditMitigationActionsTaskResponse_httpStatus = Lens.lens (\CancelAuditMitigationActionsTaskResponse' {httpStatus} -> httpStatus) (\s@CancelAuditMitigationActionsTaskResponse' {} a -> s {httpStatus = a} :: CancelAuditMitigationActionsTaskResponse)

instance
  Prelude.NFData
    CancelAuditMitigationActionsTaskResponse
