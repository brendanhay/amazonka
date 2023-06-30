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
-- Module      : Amazonka.Glue.CancelMLTaskRun
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels (stops) a task run. Machine learning task runs are asynchronous
-- tasks that Glue runs on your behalf as part of various machine learning
-- workflows. You can cancel a machine learning task run at any time by
-- calling @CancelMLTaskRun@ with a task run\'s parent transform\'s
-- @TransformID@ and the task run\'s @TaskRunId@.
module Amazonka.Glue.CancelMLTaskRun
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
    cancelMLTaskRunResponse_taskRunId,
    cancelMLTaskRunResponse_transformId,
    cancelMLTaskRunResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCancelMLTaskRun' smart constructor.
data CancelMLTaskRun = CancelMLTaskRun'
  { -- | The unique identifier of the machine learning transform.
    transformId :: Prelude.Text,
    -- | A unique identifier for the task run.
    taskRunId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'taskRunId'
  Prelude.Text ->
  CancelMLTaskRun
newCancelMLTaskRun pTransformId_ pTaskRunId_ =
  CancelMLTaskRun'
    { transformId = pTransformId_,
      taskRunId = pTaskRunId_
    }

-- | The unique identifier of the machine learning transform.
cancelMLTaskRun_transformId :: Lens.Lens' CancelMLTaskRun Prelude.Text
cancelMLTaskRun_transformId = Lens.lens (\CancelMLTaskRun' {transformId} -> transformId) (\s@CancelMLTaskRun' {} a -> s {transformId = a} :: CancelMLTaskRun)

-- | A unique identifier for the task run.
cancelMLTaskRun_taskRunId :: Lens.Lens' CancelMLTaskRun Prelude.Text
cancelMLTaskRun_taskRunId = Lens.lens (\CancelMLTaskRun' {taskRunId} -> taskRunId) (\s@CancelMLTaskRun' {} a -> s {taskRunId = a} :: CancelMLTaskRun)

instance Core.AWSRequest CancelMLTaskRun where
  type
    AWSResponse CancelMLTaskRun =
      CancelMLTaskRunResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelMLTaskRunResponse'
            Prelude.<$> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "TaskRunId")
            Prelude.<*> (x Data..?> "TransformId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelMLTaskRun where
  hashWithSalt _salt CancelMLTaskRun' {..} =
    _salt
      `Prelude.hashWithSalt` transformId
      `Prelude.hashWithSalt` taskRunId

instance Prelude.NFData CancelMLTaskRun where
  rnf CancelMLTaskRun' {..} =
    Prelude.rnf transformId
      `Prelude.seq` Prelude.rnf taskRunId

instance Data.ToHeaders CancelMLTaskRun where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.CancelMLTaskRun" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CancelMLTaskRun where
  toJSON CancelMLTaskRun' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("TransformId" Data..= transformId),
            Prelude.Just ("TaskRunId" Data..= taskRunId)
          ]
      )

instance Data.ToPath CancelMLTaskRun where
  toPath = Prelude.const "/"

instance Data.ToQuery CancelMLTaskRun where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelMLTaskRunResponse' smart constructor.
data CancelMLTaskRunResponse = CancelMLTaskRunResponse'
  { -- | The status for this run.
    status :: Prelude.Maybe TaskStatusType,
    -- | The unique identifier for the task run.
    taskRunId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the machine learning transform.
    transformId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'taskRunId', 'cancelMLTaskRunResponse_taskRunId' - The unique identifier for the task run.
--
-- 'transformId', 'cancelMLTaskRunResponse_transformId' - The unique identifier of the machine learning transform.
--
-- 'httpStatus', 'cancelMLTaskRunResponse_httpStatus' - The response's http status code.
newCancelMLTaskRunResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelMLTaskRunResponse
newCancelMLTaskRunResponse pHttpStatus_ =
  CancelMLTaskRunResponse'
    { status = Prelude.Nothing,
      taskRunId = Prelude.Nothing,
      transformId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status for this run.
cancelMLTaskRunResponse_status :: Lens.Lens' CancelMLTaskRunResponse (Prelude.Maybe TaskStatusType)
cancelMLTaskRunResponse_status = Lens.lens (\CancelMLTaskRunResponse' {status} -> status) (\s@CancelMLTaskRunResponse' {} a -> s {status = a} :: CancelMLTaskRunResponse)

-- | The unique identifier for the task run.
cancelMLTaskRunResponse_taskRunId :: Lens.Lens' CancelMLTaskRunResponse (Prelude.Maybe Prelude.Text)
cancelMLTaskRunResponse_taskRunId = Lens.lens (\CancelMLTaskRunResponse' {taskRunId} -> taskRunId) (\s@CancelMLTaskRunResponse' {} a -> s {taskRunId = a} :: CancelMLTaskRunResponse)

-- | The unique identifier of the machine learning transform.
cancelMLTaskRunResponse_transformId :: Lens.Lens' CancelMLTaskRunResponse (Prelude.Maybe Prelude.Text)
cancelMLTaskRunResponse_transformId = Lens.lens (\CancelMLTaskRunResponse' {transformId} -> transformId) (\s@CancelMLTaskRunResponse' {} a -> s {transformId = a} :: CancelMLTaskRunResponse)

-- | The response's http status code.
cancelMLTaskRunResponse_httpStatus :: Lens.Lens' CancelMLTaskRunResponse Prelude.Int
cancelMLTaskRunResponse_httpStatus = Lens.lens (\CancelMLTaskRunResponse' {httpStatus} -> httpStatus) (\s@CancelMLTaskRunResponse' {} a -> s {httpStatus = a} :: CancelMLTaskRunResponse)

instance Prelude.NFData CancelMLTaskRunResponse where
  rnf CancelMLTaskRunResponse' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf taskRunId
      `Prelude.seq` Prelude.rnf transformId
      `Prelude.seq` Prelude.rnf httpStatus
