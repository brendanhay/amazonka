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

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeDetectMitigationActionsTask' smart constructor.
data DescribeDetectMitigationActionsTask = DescribeDetectMitigationActionsTask'
  { -- | The unique identifier of the task.
    taskId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DescribeDetectMitigationActionsTask
newDescribeDetectMitigationActionsTask pTaskId_ =
  DescribeDetectMitigationActionsTask'
    { taskId =
        pTaskId_
    }

-- | The unique identifier of the task.
describeDetectMitigationActionsTask_taskId :: Lens.Lens' DescribeDetectMitigationActionsTask Core.Text
describeDetectMitigationActionsTask_taskId = Lens.lens (\DescribeDetectMitigationActionsTask' {taskId} -> taskId) (\s@DescribeDetectMitigationActionsTask' {} a -> s {taskId = a} :: DescribeDetectMitigationActionsTask)

instance
  Core.AWSRequest
    DescribeDetectMitigationActionsTask
  where
  type
    AWSResponse DescribeDetectMitigationActionsTask =
      DescribeDetectMitigationActionsTaskResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDetectMitigationActionsTaskResponse'
            Core.<$> (x Core..?> "taskSummary")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeDetectMitigationActionsTask

instance
  Core.NFData
    DescribeDetectMitigationActionsTask

instance
  Core.ToHeaders
    DescribeDetectMitigationActionsTask
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    DescribeDetectMitigationActionsTask
  where
  toPath DescribeDetectMitigationActionsTask' {..} =
    Core.mconcat
      [ "/detect/mitigationactions/tasks/",
        Core.toBS taskId
      ]

instance
  Core.ToQuery
    DescribeDetectMitigationActionsTask
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeDetectMitigationActionsTaskResponse' smart constructor.
data DescribeDetectMitigationActionsTaskResponse = DescribeDetectMitigationActionsTaskResponse'
  { -- | The description of a task.
    taskSummary :: Core.Maybe DetectMitigationActionsTaskSummary,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeDetectMitigationActionsTaskResponse
newDescribeDetectMitigationActionsTaskResponse
  pHttpStatus_ =
    DescribeDetectMitigationActionsTaskResponse'
      { taskSummary =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The description of a task.
describeDetectMitigationActionsTaskResponse_taskSummary :: Lens.Lens' DescribeDetectMitigationActionsTaskResponse (Core.Maybe DetectMitigationActionsTaskSummary)
describeDetectMitigationActionsTaskResponse_taskSummary = Lens.lens (\DescribeDetectMitigationActionsTaskResponse' {taskSummary} -> taskSummary) (\s@DescribeDetectMitigationActionsTaskResponse' {} a -> s {taskSummary = a} :: DescribeDetectMitigationActionsTaskResponse)

-- | The response's http status code.
describeDetectMitigationActionsTaskResponse_httpStatus :: Lens.Lens' DescribeDetectMitigationActionsTaskResponse Core.Int
describeDetectMitigationActionsTaskResponse_httpStatus = Lens.lens (\DescribeDetectMitigationActionsTaskResponse' {httpStatus} -> httpStatus) (\s@DescribeDetectMitigationActionsTaskResponse' {} a -> s {httpStatus = a} :: DescribeDetectMitigationActionsTaskResponse)

instance
  Core.NFData
    DescribeDetectMitigationActionsTaskResponse
