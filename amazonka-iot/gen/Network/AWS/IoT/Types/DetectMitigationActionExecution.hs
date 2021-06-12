{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.DetectMitigationActionExecution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.DetectMitigationActionExecution where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.DetectMitigationActionExecutionStatus
import qualified Network.AWS.Lens as Lens

-- | Describes which mitigation actions should be executed.
--
-- /See:/ 'newDetectMitigationActionExecution' smart constructor.
data DetectMitigationActionExecution = DetectMitigationActionExecution'
  { -- | The unique identifier of the violation.
    violationId :: Core.Maybe Core.Text,
    -- | The status of a mitigation action.
    status :: Core.Maybe DetectMitigationActionExecutionStatus,
    -- | The friendly name that uniquely identifies the mitigation action.
    actionName :: Core.Maybe Core.Text,
    -- | The date a mitigation action was started.
    executionStartDate :: Core.Maybe Core.POSIX,
    -- | The message of a mitigation action.
    message :: Core.Maybe Core.Text,
    -- | The name of the thing.
    thingName :: Core.Maybe Core.Text,
    -- | The unique identifier of the task.
    taskId :: Core.Maybe Core.Text,
    -- | The date a mitigation action ended.
    executionEndDate :: Core.Maybe Core.POSIX,
    -- | The error code of a mitigation action.
    errorCode :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DetectMitigationActionExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'violationId', 'detectMitigationActionExecution_violationId' - The unique identifier of the violation.
--
-- 'status', 'detectMitigationActionExecution_status' - The status of a mitigation action.
--
-- 'actionName', 'detectMitigationActionExecution_actionName' - The friendly name that uniquely identifies the mitigation action.
--
-- 'executionStartDate', 'detectMitigationActionExecution_executionStartDate' - The date a mitigation action was started.
--
-- 'message', 'detectMitigationActionExecution_message' - The message of a mitigation action.
--
-- 'thingName', 'detectMitigationActionExecution_thingName' - The name of the thing.
--
-- 'taskId', 'detectMitigationActionExecution_taskId' - The unique identifier of the task.
--
-- 'executionEndDate', 'detectMitigationActionExecution_executionEndDate' - The date a mitigation action ended.
--
-- 'errorCode', 'detectMitigationActionExecution_errorCode' - The error code of a mitigation action.
newDetectMitigationActionExecution ::
  DetectMitigationActionExecution
newDetectMitigationActionExecution =
  DetectMitigationActionExecution'
    { violationId =
        Core.Nothing,
      status = Core.Nothing,
      actionName = Core.Nothing,
      executionStartDate = Core.Nothing,
      message = Core.Nothing,
      thingName = Core.Nothing,
      taskId = Core.Nothing,
      executionEndDate = Core.Nothing,
      errorCode = Core.Nothing
    }

-- | The unique identifier of the violation.
detectMitigationActionExecution_violationId :: Lens.Lens' DetectMitigationActionExecution (Core.Maybe Core.Text)
detectMitigationActionExecution_violationId = Lens.lens (\DetectMitigationActionExecution' {violationId} -> violationId) (\s@DetectMitigationActionExecution' {} a -> s {violationId = a} :: DetectMitigationActionExecution)

-- | The status of a mitigation action.
detectMitigationActionExecution_status :: Lens.Lens' DetectMitigationActionExecution (Core.Maybe DetectMitigationActionExecutionStatus)
detectMitigationActionExecution_status = Lens.lens (\DetectMitigationActionExecution' {status} -> status) (\s@DetectMitigationActionExecution' {} a -> s {status = a} :: DetectMitigationActionExecution)

-- | The friendly name that uniquely identifies the mitigation action.
detectMitigationActionExecution_actionName :: Lens.Lens' DetectMitigationActionExecution (Core.Maybe Core.Text)
detectMitigationActionExecution_actionName = Lens.lens (\DetectMitigationActionExecution' {actionName} -> actionName) (\s@DetectMitigationActionExecution' {} a -> s {actionName = a} :: DetectMitigationActionExecution)

-- | The date a mitigation action was started.
detectMitigationActionExecution_executionStartDate :: Lens.Lens' DetectMitigationActionExecution (Core.Maybe Core.UTCTime)
detectMitigationActionExecution_executionStartDate = Lens.lens (\DetectMitigationActionExecution' {executionStartDate} -> executionStartDate) (\s@DetectMitigationActionExecution' {} a -> s {executionStartDate = a} :: DetectMitigationActionExecution) Core.. Lens.mapping Core._Time

-- | The message of a mitigation action.
detectMitigationActionExecution_message :: Lens.Lens' DetectMitigationActionExecution (Core.Maybe Core.Text)
detectMitigationActionExecution_message = Lens.lens (\DetectMitigationActionExecution' {message} -> message) (\s@DetectMitigationActionExecution' {} a -> s {message = a} :: DetectMitigationActionExecution)

-- | The name of the thing.
detectMitigationActionExecution_thingName :: Lens.Lens' DetectMitigationActionExecution (Core.Maybe Core.Text)
detectMitigationActionExecution_thingName = Lens.lens (\DetectMitigationActionExecution' {thingName} -> thingName) (\s@DetectMitigationActionExecution' {} a -> s {thingName = a} :: DetectMitigationActionExecution)

-- | The unique identifier of the task.
detectMitigationActionExecution_taskId :: Lens.Lens' DetectMitigationActionExecution (Core.Maybe Core.Text)
detectMitigationActionExecution_taskId = Lens.lens (\DetectMitigationActionExecution' {taskId} -> taskId) (\s@DetectMitigationActionExecution' {} a -> s {taskId = a} :: DetectMitigationActionExecution)

-- | The date a mitigation action ended.
detectMitigationActionExecution_executionEndDate :: Lens.Lens' DetectMitigationActionExecution (Core.Maybe Core.UTCTime)
detectMitigationActionExecution_executionEndDate = Lens.lens (\DetectMitigationActionExecution' {executionEndDate} -> executionEndDate) (\s@DetectMitigationActionExecution' {} a -> s {executionEndDate = a} :: DetectMitigationActionExecution) Core.. Lens.mapping Core._Time

-- | The error code of a mitigation action.
detectMitigationActionExecution_errorCode :: Lens.Lens' DetectMitigationActionExecution (Core.Maybe Core.Text)
detectMitigationActionExecution_errorCode = Lens.lens (\DetectMitigationActionExecution' {errorCode} -> errorCode) (\s@DetectMitigationActionExecution' {} a -> s {errorCode = a} :: DetectMitigationActionExecution)

instance
  Core.FromJSON
    DetectMitigationActionExecution
  where
  parseJSON =
    Core.withObject
      "DetectMitigationActionExecution"
      ( \x ->
          DetectMitigationActionExecution'
            Core.<$> (x Core..:? "violationId")
            Core.<*> (x Core..:? "status")
            Core.<*> (x Core..:? "actionName")
            Core.<*> (x Core..:? "executionStartDate")
            Core.<*> (x Core..:? "message")
            Core.<*> (x Core..:? "thingName")
            Core.<*> (x Core..:? "taskId")
            Core.<*> (x Core..:? "executionEndDate")
            Core.<*> (x Core..:? "errorCode")
      )

instance
  Core.Hashable
    DetectMitigationActionExecution

instance Core.NFData DetectMitigationActionExecution
