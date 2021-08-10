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
-- Module      : Network.AWS.IoT.StartDetectMitigationActionsTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a Device Defender ML Detect mitigation actions task.
module Network.AWS.IoT.StartDetectMitigationActionsTask
  ( -- * Creating a Request
    StartDetectMitigationActionsTask (..),
    newStartDetectMitigationActionsTask,

    -- * Request Lenses
    startDetectMitigationActionsTask_includeSuppressedAlerts,
    startDetectMitigationActionsTask_includeOnlyActiveViolations,
    startDetectMitigationActionsTask_violationEventOccurrenceRange,
    startDetectMitigationActionsTask_taskId,
    startDetectMitigationActionsTask_target,
    startDetectMitigationActionsTask_actions,
    startDetectMitigationActionsTask_clientRequestToken,

    -- * Destructuring the Response
    StartDetectMitigationActionsTaskResponse (..),
    newStartDetectMitigationActionsTaskResponse,

    -- * Response Lenses
    startDetectMitigationActionsTaskResponse_taskId,
    startDetectMitigationActionsTaskResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartDetectMitigationActionsTask' smart constructor.
data StartDetectMitigationActionsTask = StartDetectMitigationActionsTask'
  { -- | Specifies to include suppressed alerts.
    includeSuppressedAlerts :: Prelude.Maybe Prelude.Bool,
    -- | Specifies to list only active violations.
    includeOnlyActiveViolations :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the time period of which violation events occurred between.
    violationEventOccurrenceRange :: Prelude.Maybe ViolationEventOccurrenceRange,
    -- | The unique identifier of the task.
    taskId :: Prelude.Text,
    -- | Specifies the ML Detect findings to which the mitigation actions are
    -- applied.
    target :: DetectMitigationActionsTaskTarget,
    -- | The actions to be performed when a device has unexpected behavior.
    actions :: Prelude.NonEmpty Prelude.Text,
    -- | Each mitigation action task must have a unique client request token. If
    -- you try to create a new task with the same token as a task that already
    -- exists, an exception occurs. If you omit this value, AWS SDKs will
    -- automatically generate a unique client request.
    clientRequestToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartDetectMitigationActionsTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includeSuppressedAlerts', 'startDetectMitigationActionsTask_includeSuppressedAlerts' - Specifies to include suppressed alerts.
--
-- 'includeOnlyActiveViolations', 'startDetectMitigationActionsTask_includeOnlyActiveViolations' - Specifies to list only active violations.
--
-- 'violationEventOccurrenceRange', 'startDetectMitigationActionsTask_violationEventOccurrenceRange' - Specifies the time period of which violation events occurred between.
--
-- 'taskId', 'startDetectMitigationActionsTask_taskId' - The unique identifier of the task.
--
-- 'target', 'startDetectMitigationActionsTask_target' - Specifies the ML Detect findings to which the mitigation actions are
-- applied.
--
-- 'actions', 'startDetectMitigationActionsTask_actions' - The actions to be performed when a device has unexpected behavior.
--
-- 'clientRequestToken', 'startDetectMitigationActionsTask_clientRequestToken' - Each mitigation action task must have a unique client request token. If
-- you try to create a new task with the same token as a task that already
-- exists, an exception occurs. If you omit this value, AWS SDKs will
-- automatically generate a unique client request.
newStartDetectMitigationActionsTask ::
  -- | 'taskId'
  Prelude.Text ->
  -- | 'target'
  DetectMitigationActionsTaskTarget ->
  -- | 'actions'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'clientRequestToken'
  Prelude.Text ->
  StartDetectMitigationActionsTask
newStartDetectMitigationActionsTask
  pTaskId_
  pTarget_
  pActions_
  pClientRequestToken_ =
    StartDetectMitigationActionsTask'
      { includeSuppressedAlerts =
          Prelude.Nothing,
        includeOnlyActiveViolations =
          Prelude.Nothing,
        violationEventOccurrenceRange =
          Prelude.Nothing,
        taskId = pTaskId_,
        target = pTarget_,
        actions = Lens._Coerce Lens.# pActions_,
        clientRequestToken = pClientRequestToken_
      }

-- | Specifies to include suppressed alerts.
startDetectMitigationActionsTask_includeSuppressedAlerts :: Lens.Lens' StartDetectMitigationActionsTask (Prelude.Maybe Prelude.Bool)
startDetectMitigationActionsTask_includeSuppressedAlerts = Lens.lens (\StartDetectMitigationActionsTask' {includeSuppressedAlerts} -> includeSuppressedAlerts) (\s@StartDetectMitigationActionsTask' {} a -> s {includeSuppressedAlerts = a} :: StartDetectMitigationActionsTask)

-- | Specifies to list only active violations.
startDetectMitigationActionsTask_includeOnlyActiveViolations :: Lens.Lens' StartDetectMitigationActionsTask (Prelude.Maybe Prelude.Bool)
startDetectMitigationActionsTask_includeOnlyActiveViolations = Lens.lens (\StartDetectMitigationActionsTask' {includeOnlyActiveViolations} -> includeOnlyActiveViolations) (\s@StartDetectMitigationActionsTask' {} a -> s {includeOnlyActiveViolations = a} :: StartDetectMitigationActionsTask)

-- | Specifies the time period of which violation events occurred between.
startDetectMitigationActionsTask_violationEventOccurrenceRange :: Lens.Lens' StartDetectMitigationActionsTask (Prelude.Maybe ViolationEventOccurrenceRange)
startDetectMitigationActionsTask_violationEventOccurrenceRange = Lens.lens (\StartDetectMitigationActionsTask' {violationEventOccurrenceRange} -> violationEventOccurrenceRange) (\s@StartDetectMitigationActionsTask' {} a -> s {violationEventOccurrenceRange = a} :: StartDetectMitigationActionsTask)

-- | The unique identifier of the task.
startDetectMitigationActionsTask_taskId :: Lens.Lens' StartDetectMitigationActionsTask Prelude.Text
startDetectMitigationActionsTask_taskId = Lens.lens (\StartDetectMitigationActionsTask' {taskId} -> taskId) (\s@StartDetectMitigationActionsTask' {} a -> s {taskId = a} :: StartDetectMitigationActionsTask)

-- | Specifies the ML Detect findings to which the mitigation actions are
-- applied.
startDetectMitigationActionsTask_target :: Lens.Lens' StartDetectMitigationActionsTask DetectMitigationActionsTaskTarget
startDetectMitigationActionsTask_target = Lens.lens (\StartDetectMitigationActionsTask' {target} -> target) (\s@StartDetectMitigationActionsTask' {} a -> s {target = a} :: StartDetectMitigationActionsTask)

-- | The actions to be performed when a device has unexpected behavior.
startDetectMitigationActionsTask_actions :: Lens.Lens' StartDetectMitigationActionsTask (Prelude.NonEmpty Prelude.Text)
startDetectMitigationActionsTask_actions = Lens.lens (\StartDetectMitigationActionsTask' {actions} -> actions) (\s@StartDetectMitigationActionsTask' {} a -> s {actions = a} :: StartDetectMitigationActionsTask) Prelude.. Lens._Coerce

-- | Each mitigation action task must have a unique client request token. If
-- you try to create a new task with the same token as a task that already
-- exists, an exception occurs. If you omit this value, AWS SDKs will
-- automatically generate a unique client request.
startDetectMitigationActionsTask_clientRequestToken :: Lens.Lens' StartDetectMitigationActionsTask Prelude.Text
startDetectMitigationActionsTask_clientRequestToken = Lens.lens (\StartDetectMitigationActionsTask' {clientRequestToken} -> clientRequestToken) (\s@StartDetectMitigationActionsTask' {} a -> s {clientRequestToken = a} :: StartDetectMitigationActionsTask)

instance
  Core.AWSRequest
    StartDetectMitigationActionsTask
  where
  type
    AWSResponse StartDetectMitigationActionsTask =
      StartDetectMitigationActionsTaskResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartDetectMitigationActionsTaskResponse'
            Prelude.<$> (x Core..?> "taskId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    StartDetectMitigationActionsTask

instance
  Prelude.NFData
    StartDetectMitigationActionsTask

instance
  Core.ToHeaders
    StartDetectMitigationActionsTask
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON StartDetectMitigationActionsTask where
  toJSON StartDetectMitigationActionsTask' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("includeSuppressedAlerts" Core..=)
              Prelude.<$> includeSuppressedAlerts,
            ("includeOnlyActiveViolations" Core..=)
              Prelude.<$> includeOnlyActiveViolations,
            ("violationEventOccurrenceRange" Core..=)
              Prelude.<$> violationEventOccurrenceRange,
            Prelude.Just ("target" Core..= target),
            Prelude.Just ("actions" Core..= actions),
            Prelude.Just
              ("clientRequestToken" Core..= clientRequestToken)
          ]
      )

instance Core.ToPath StartDetectMitigationActionsTask where
  toPath StartDetectMitigationActionsTask' {..} =
    Prelude.mconcat
      [ "/detect/mitigationactions/tasks/",
        Core.toBS taskId
      ]

instance
  Core.ToQuery
    StartDetectMitigationActionsTask
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartDetectMitigationActionsTaskResponse' smart constructor.
data StartDetectMitigationActionsTaskResponse = StartDetectMitigationActionsTaskResponse'
  { -- | The unique identifier of the task.
    taskId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartDetectMitigationActionsTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskId', 'startDetectMitigationActionsTaskResponse_taskId' - The unique identifier of the task.
--
-- 'httpStatus', 'startDetectMitigationActionsTaskResponse_httpStatus' - The response's http status code.
newStartDetectMitigationActionsTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartDetectMitigationActionsTaskResponse
newStartDetectMitigationActionsTaskResponse
  pHttpStatus_ =
    StartDetectMitigationActionsTaskResponse'
      { taskId =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The unique identifier of the task.
startDetectMitigationActionsTaskResponse_taskId :: Lens.Lens' StartDetectMitigationActionsTaskResponse (Prelude.Maybe Prelude.Text)
startDetectMitigationActionsTaskResponse_taskId = Lens.lens (\StartDetectMitigationActionsTaskResponse' {taskId} -> taskId) (\s@StartDetectMitigationActionsTaskResponse' {} a -> s {taskId = a} :: StartDetectMitigationActionsTaskResponse)

-- | The response's http status code.
startDetectMitigationActionsTaskResponse_httpStatus :: Lens.Lens' StartDetectMitigationActionsTaskResponse Prelude.Int
startDetectMitigationActionsTaskResponse_httpStatus = Lens.lens (\StartDetectMitigationActionsTaskResponse' {httpStatus} -> httpStatus) (\s@StartDetectMitigationActionsTaskResponse' {} a -> s {httpStatus = a} :: StartDetectMitigationActionsTaskResponse)

instance
  Prelude.NFData
    StartDetectMitigationActionsTaskResponse
