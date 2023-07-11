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
-- Module      : Amazonka.IoT.CancelDetectMitigationActionsTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a Device Defender ML Detect mitigation action.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions CancelDetectMitigationActionsTask>
-- action.
module Amazonka.IoT.CancelDetectMitigationActionsTask
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCancelDetectMitigationActionsTask' smart constructor.
data CancelDetectMitigationActionsTask = CancelDetectMitigationActionsTask'
  { -- | The unique identifier of the task.
    taskId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  CancelDetectMitigationActionsTask
newCancelDetectMitigationActionsTask pTaskId_ =
  CancelDetectMitigationActionsTask'
    { taskId =
        pTaskId_
    }

-- | The unique identifier of the task.
cancelDetectMitigationActionsTask_taskId :: Lens.Lens' CancelDetectMitigationActionsTask Prelude.Text
cancelDetectMitigationActionsTask_taskId = Lens.lens (\CancelDetectMitigationActionsTask' {taskId} -> taskId) (\s@CancelDetectMitigationActionsTask' {} a -> s {taskId = a} :: CancelDetectMitigationActionsTask)

instance
  Core.AWSRequest
    CancelDetectMitigationActionsTask
  where
  type
    AWSResponse CancelDetectMitigationActionsTask =
      CancelDetectMitigationActionsTaskResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CancelDetectMitigationActionsTaskResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CancelDetectMitigationActionsTask
  where
  hashWithSalt
    _salt
    CancelDetectMitigationActionsTask' {..} =
      _salt `Prelude.hashWithSalt` taskId

instance
  Prelude.NFData
    CancelDetectMitigationActionsTask
  where
  rnf CancelDetectMitigationActionsTask' {..} =
    Prelude.rnf taskId

instance
  Data.ToHeaders
    CancelDetectMitigationActionsTask
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToJSON
    CancelDetectMitigationActionsTask
  where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance
  Data.ToPath
    CancelDetectMitigationActionsTask
  where
  toPath CancelDetectMitigationActionsTask' {..} =
    Prelude.mconcat
      [ "/detect/mitigationactions/tasks/",
        Data.toBS taskId,
        "/cancel"
      ]

instance
  Data.ToQuery
    CancelDetectMitigationActionsTask
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelDetectMitigationActionsTaskResponse' smart constructor.
data CancelDetectMitigationActionsTaskResponse = CancelDetectMitigationActionsTaskResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CancelDetectMitigationActionsTaskResponse
newCancelDetectMitigationActionsTaskResponse
  pHttpStatus_ =
    CancelDetectMitigationActionsTaskResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
cancelDetectMitigationActionsTaskResponse_httpStatus :: Lens.Lens' CancelDetectMitigationActionsTaskResponse Prelude.Int
cancelDetectMitigationActionsTaskResponse_httpStatus = Lens.lens (\CancelDetectMitigationActionsTaskResponse' {httpStatus} -> httpStatus) (\s@CancelDetectMitigationActionsTaskResponse' {} a -> s {httpStatus = a} :: CancelDetectMitigationActionsTaskResponse)

instance
  Prelude.NFData
    CancelDetectMitigationActionsTaskResponse
  where
  rnf CancelDetectMitigationActionsTaskResponse' {..} =
    Prelude.rnf httpStatus
