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
-- Module      : Amazonka.IoT.DescribeDetectMitigationActionsTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a Device Defender ML Detect mitigation action.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions DescribeDetectMitigationActionsTask>
-- action.
module Amazonka.IoT.DescribeDetectMitigationActionsTask
  ( -- * Creating a Request
    DescribeDetectMitigationActionsTask (..),
    newDescribeDetectMitigationActionsTask,

    -- * Request Lenses
    describeDetectMitigationActionsTask_taskId,

    -- * Destructuring the Response
    DescribeDetectMitigationActionsTaskResponse (..),
    newDescribeDetectMitigationActionsTaskResponse,

    -- * Response Lenses
    describeDetectMitigationActionsTaskResponse_taskSummary,
    describeDetectMitigationActionsTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDetectMitigationActionsTask' smart constructor.
data DescribeDetectMitigationActionsTask = DescribeDetectMitigationActionsTask'
  { -- | The unique identifier of the task.
    taskId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDetectMitigationActionsTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskId', 'describeDetectMitigationActionsTask_taskId' - The unique identifier of the task.
newDescribeDetectMitigationActionsTask ::
  -- | 'taskId'
  Prelude.Text ->
  DescribeDetectMitigationActionsTask
newDescribeDetectMitigationActionsTask pTaskId_ =
  DescribeDetectMitigationActionsTask'
    { taskId =
        pTaskId_
    }

-- | The unique identifier of the task.
describeDetectMitigationActionsTask_taskId :: Lens.Lens' DescribeDetectMitigationActionsTask Prelude.Text
describeDetectMitigationActionsTask_taskId = Lens.lens (\DescribeDetectMitigationActionsTask' {taskId} -> taskId) (\s@DescribeDetectMitigationActionsTask' {} a -> s {taskId = a} :: DescribeDetectMitigationActionsTask)

instance
  Core.AWSRequest
    DescribeDetectMitigationActionsTask
  where
  type
    AWSResponse DescribeDetectMitigationActionsTask =
      DescribeDetectMitigationActionsTaskResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDetectMitigationActionsTaskResponse'
            Prelude.<$> (x Data..?> "taskSummary")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeDetectMitigationActionsTask
  where
  hashWithSalt
    _salt
    DescribeDetectMitigationActionsTask' {..} =
      _salt `Prelude.hashWithSalt` taskId

instance
  Prelude.NFData
    DescribeDetectMitigationActionsTask
  where
  rnf DescribeDetectMitigationActionsTask' {..} =
    Prelude.rnf taskId

instance
  Data.ToHeaders
    DescribeDetectMitigationActionsTask
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DescribeDetectMitigationActionsTask
  where
  toPath DescribeDetectMitigationActionsTask' {..} =
    Prelude.mconcat
      [ "/detect/mitigationactions/tasks/",
        Data.toBS taskId
      ]

instance
  Data.ToQuery
    DescribeDetectMitigationActionsTask
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDetectMitigationActionsTaskResponse' smart constructor.
data DescribeDetectMitigationActionsTaskResponse = DescribeDetectMitigationActionsTaskResponse'
  { -- | The description of a task.
    taskSummary :: Prelude.Maybe DetectMitigationActionsTaskSummary,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDetectMitigationActionsTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskSummary', 'describeDetectMitigationActionsTaskResponse_taskSummary' - The description of a task.
--
-- 'httpStatus', 'describeDetectMitigationActionsTaskResponse_httpStatus' - The response's http status code.
newDescribeDetectMitigationActionsTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDetectMitigationActionsTaskResponse
newDescribeDetectMitigationActionsTaskResponse
  pHttpStatus_ =
    DescribeDetectMitigationActionsTaskResponse'
      { taskSummary =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The description of a task.
describeDetectMitigationActionsTaskResponse_taskSummary :: Lens.Lens' DescribeDetectMitigationActionsTaskResponse (Prelude.Maybe DetectMitigationActionsTaskSummary)
describeDetectMitigationActionsTaskResponse_taskSummary = Lens.lens (\DescribeDetectMitigationActionsTaskResponse' {taskSummary} -> taskSummary) (\s@DescribeDetectMitigationActionsTaskResponse' {} a -> s {taskSummary = a} :: DescribeDetectMitigationActionsTaskResponse)

-- | The response's http status code.
describeDetectMitigationActionsTaskResponse_httpStatus :: Lens.Lens' DescribeDetectMitigationActionsTaskResponse Prelude.Int
describeDetectMitigationActionsTaskResponse_httpStatus = Lens.lens (\DescribeDetectMitigationActionsTaskResponse' {httpStatus} -> httpStatus) (\s@DescribeDetectMitigationActionsTaskResponse' {} a -> s {httpStatus = a} :: DescribeDetectMitigationActionsTaskResponse)

instance
  Prelude.NFData
    DescribeDetectMitigationActionsTaskResponse
  where
  rnf DescribeDetectMitigationActionsTaskResponse' {..} =
    Prelude.rnf taskSummary `Prelude.seq`
      Prelude.rnf httpStatus
