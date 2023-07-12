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
-- Module      : Amazonka.IoT.CancelAuditMitigationActionsTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a mitigation action task that is in progress. If the task is not
-- in progress, an InvalidRequestException occurs.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions CancelAuditMitigationActionsTask>
-- action.
module Amazonka.IoT.CancelAuditMitigationActionsTask
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCancelAuditMitigationActionsTask' smart constructor.
data CancelAuditMitigationActionsTask = CancelAuditMitigationActionsTask'
  { -- | The unique identifier for the task that you want to cancel.
    taskId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Core.AWSRequest
    CancelAuditMitigationActionsTask
  where
  type
    AWSResponse CancelAuditMitigationActionsTask =
      CancelAuditMitigationActionsTaskResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CancelAuditMitigationActionsTaskResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CancelAuditMitigationActionsTask
  where
  hashWithSalt
    _salt
    CancelAuditMitigationActionsTask' {..} =
      _salt `Prelude.hashWithSalt` taskId

instance
  Prelude.NFData
    CancelAuditMitigationActionsTask
  where
  rnf CancelAuditMitigationActionsTask' {..} =
    Prelude.rnf taskId

instance
  Data.ToHeaders
    CancelAuditMitigationActionsTask
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CancelAuditMitigationActionsTask where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath CancelAuditMitigationActionsTask where
  toPath CancelAuditMitigationActionsTask' {..} =
    Prelude.mconcat
      [ "/audit/mitigationactions/tasks/",
        Data.toBS taskId,
        "/cancel"
      ]

instance
  Data.ToQuery
    CancelAuditMitigationActionsTask
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelAuditMitigationActionsTaskResponse' smart constructor.
data CancelAuditMitigationActionsTaskResponse = CancelAuditMitigationActionsTaskResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf CancelAuditMitigationActionsTaskResponse' {..} =
    Prelude.rnf httpStatus
