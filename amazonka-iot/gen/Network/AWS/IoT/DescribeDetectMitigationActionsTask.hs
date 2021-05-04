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
-- Module      : Network.AWS.IoT.DescribeDetectMitigationActionsTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a Device Defender ML Detect mitigation action.
module Network.AWS.IoT.DescribeDetectMitigationActionsTask
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

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeDetectMitigationActionsTask' smart constructor.
data DescribeDetectMitigationActionsTask = DescribeDetectMitigationActionsTask'
  { -- | The unique identifier of the task.
    taskId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.AWSRequest
    DescribeDetectMitigationActionsTask
  where
  type
    Rs DescribeDetectMitigationActionsTask =
      DescribeDetectMitigationActionsTaskResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDetectMitigationActionsTaskResponse'
            Prelude.<$> (x Prelude..?> "taskSummary")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeDetectMitigationActionsTask

instance
  Prelude.NFData
    DescribeDetectMitigationActionsTask

instance
  Prelude.ToHeaders
    DescribeDetectMitigationActionsTask
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    DescribeDetectMitigationActionsTask
  where
  toPath DescribeDetectMitigationActionsTask' {..} =
    Prelude.mconcat
      [ "/detect/mitigationactions/tasks/",
        Prelude.toBS taskId
      ]

instance
  Prelude.ToQuery
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
