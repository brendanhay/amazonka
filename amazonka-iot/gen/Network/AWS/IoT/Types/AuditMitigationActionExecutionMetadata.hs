{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoT.Types.AuditMitigationActionExecutionMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuditMitigationActionExecutionMetadata where

import Network.AWS.IoT.Types.AuditMitigationActionsExecutionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Returned by ListAuditMitigationActionsTask, this object contains
-- information that describes a mitigation action that has been started.
--
-- /See:/ 'newAuditMitigationActionExecutionMetadata' smart constructor.
data AuditMitigationActionExecutionMetadata = AuditMitigationActionExecutionMetadata'
  { -- | The current status of the task being executed.
    status :: Prelude.Maybe AuditMitigationActionsExecutionStatus,
    -- | The friendly name of the mitigation action being applied by the task.
    actionName :: Prelude.Maybe Prelude.Text,
    -- | If an error occurred, a message that describes the error.
    message :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the mitigation action being applied by the
    -- task.
    actionId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the findings to which the task and associated
    -- mitigation action are applied.
    findingId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the task that applies the mitigation action.
    taskId :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the task was started.
    startTime :: Prelude.Maybe Prelude.POSIX,
    -- | The date and time when the task was completed or canceled. Blank if the
    -- task is still running.
    endTime :: Prelude.Maybe Prelude.POSIX,
    -- | If an error occurred, the code that indicates which type of error
    -- occurred.
    errorCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AuditMitigationActionExecutionMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'auditMitigationActionExecutionMetadata_status' - The current status of the task being executed.
--
-- 'actionName', 'auditMitigationActionExecutionMetadata_actionName' - The friendly name of the mitigation action being applied by the task.
--
-- 'message', 'auditMitigationActionExecutionMetadata_message' - If an error occurred, a message that describes the error.
--
-- 'actionId', 'auditMitigationActionExecutionMetadata_actionId' - The unique identifier for the mitigation action being applied by the
-- task.
--
-- 'findingId', 'auditMitigationActionExecutionMetadata_findingId' - The unique identifier for the findings to which the task and associated
-- mitigation action are applied.
--
-- 'taskId', 'auditMitigationActionExecutionMetadata_taskId' - The unique identifier for the task that applies the mitigation action.
--
-- 'startTime', 'auditMitigationActionExecutionMetadata_startTime' - The date and time when the task was started.
--
-- 'endTime', 'auditMitigationActionExecutionMetadata_endTime' - The date and time when the task was completed or canceled. Blank if the
-- task is still running.
--
-- 'errorCode', 'auditMitigationActionExecutionMetadata_errorCode' - If an error occurred, the code that indicates which type of error
-- occurred.
newAuditMitigationActionExecutionMetadata ::
  AuditMitigationActionExecutionMetadata
newAuditMitigationActionExecutionMetadata =
  AuditMitigationActionExecutionMetadata'
    { status =
        Prelude.Nothing,
      actionName = Prelude.Nothing,
      message = Prelude.Nothing,
      actionId = Prelude.Nothing,
      findingId = Prelude.Nothing,
      taskId = Prelude.Nothing,
      startTime = Prelude.Nothing,
      endTime = Prelude.Nothing,
      errorCode = Prelude.Nothing
    }

-- | The current status of the task being executed.
auditMitigationActionExecutionMetadata_status :: Lens.Lens' AuditMitigationActionExecutionMetadata (Prelude.Maybe AuditMitigationActionsExecutionStatus)
auditMitigationActionExecutionMetadata_status = Lens.lens (\AuditMitigationActionExecutionMetadata' {status} -> status) (\s@AuditMitigationActionExecutionMetadata' {} a -> s {status = a} :: AuditMitigationActionExecutionMetadata)

-- | The friendly name of the mitigation action being applied by the task.
auditMitigationActionExecutionMetadata_actionName :: Lens.Lens' AuditMitigationActionExecutionMetadata (Prelude.Maybe Prelude.Text)
auditMitigationActionExecutionMetadata_actionName = Lens.lens (\AuditMitigationActionExecutionMetadata' {actionName} -> actionName) (\s@AuditMitigationActionExecutionMetadata' {} a -> s {actionName = a} :: AuditMitigationActionExecutionMetadata)

-- | If an error occurred, a message that describes the error.
auditMitigationActionExecutionMetadata_message :: Lens.Lens' AuditMitigationActionExecutionMetadata (Prelude.Maybe Prelude.Text)
auditMitigationActionExecutionMetadata_message = Lens.lens (\AuditMitigationActionExecutionMetadata' {message} -> message) (\s@AuditMitigationActionExecutionMetadata' {} a -> s {message = a} :: AuditMitigationActionExecutionMetadata)

-- | The unique identifier for the mitigation action being applied by the
-- task.
auditMitigationActionExecutionMetadata_actionId :: Lens.Lens' AuditMitigationActionExecutionMetadata (Prelude.Maybe Prelude.Text)
auditMitigationActionExecutionMetadata_actionId = Lens.lens (\AuditMitigationActionExecutionMetadata' {actionId} -> actionId) (\s@AuditMitigationActionExecutionMetadata' {} a -> s {actionId = a} :: AuditMitigationActionExecutionMetadata)

-- | The unique identifier for the findings to which the task and associated
-- mitigation action are applied.
auditMitigationActionExecutionMetadata_findingId :: Lens.Lens' AuditMitigationActionExecutionMetadata (Prelude.Maybe Prelude.Text)
auditMitigationActionExecutionMetadata_findingId = Lens.lens (\AuditMitigationActionExecutionMetadata' {findingId} -> findingId) (\s@AuditMitigationActionExecutionMetadata' {} a -> s {findingId = a} :: AuditMitigationActionExecutionMetadata)

-- | The unique identifier for the task that applies the mitigation action.
auditMitigationActionExecutionMetadata_taskId :: Lens.Lens' AuditMitigationActionExecutionMetadata (Prelude.Maybe Prelude.Text)
auditMitigationActionExecutionMetadata_taskId = Lens.lens (\AuditMitigationActionExecutionMetadata' {taskId} -> taskId) (\s@AuditMitigationActionExecutionMetadata' {} a -> s {taskId = a} :: AuditMitigationActionExecutionMetadata)

-- | The date and time when the task was started.
auditMitigationActionExecutionMetadata_startTime :: Lens.Lens' AuditMitigationActionExecutionMetadata (Prelude.Maybe Prelude.UTCTime)
auditMitigationActionExecutionMetadata_startTime = Lens.lens (\AuditMitigationActionExecutionMetadata' {startTime} -> startTime) (\s@AuditMitigationActionExecutionMetadata' {} a -> s {startTime = a} :: AuditMitigationActionExecutionMetadata) Prelude.. Lens.mapping Prelude._Time

-- | The date and time when the task was completed or canceled. Blank if the
-- task is still running.
auditMitigationActionExecutionMetadata_endTime :: Lens.Lens' AuditMitigationActionExecutionMetadata (Prelude.Maybe Prelude.UTCTime)
auditMitigationActionExecutionMetadata_endTime = Lens.lens (\AuditMitigationActionExecutionMetadata' {endTime} -> endTime) (\s@AuditMitigationActionExecutionMetadata' {} a -> s {endTime = a} :: AuditMitigationActionExecutionMetadata) Prelude.. Lens.mapping Prelude._Time

-- | If an error occurred, the code that indicates which type of error
-- occurred.
auditMitigationActionExecutionMetadata_errorCode :: Lens.Lens' AuditMitigationActionExecutionMetadata (Prelude.Maybe Prelude.Text)
auditMitigationActionExecutionMetadata_errorCode = Lens.lens (\AuditMitigationActionExecutionMetadata' {errorCode} -> errorCode) (\s@AuditMitigationActionExecutionMetadata' {} a -> s {errorCode = a} :: AuditMitigationActionExecutionMetadata)

instance
  Prelude.FromJSON
    AuditMitigationActionExecutionMetadata
  where
  parseJSON =
    Prelude.withObject
      "AuditMitigationActionExecutionMetadata"
      ( \x ->
          AuditMitigationActionExecutionMetadata'
            Prelude.<$> (x Prelude..:? "status")
            Prelude.<*> (x Prelude..:? "actionName")
            Prelude.<*> (x Prelude..:? "message")
            Prelude.<*> (x Prelude..:? "actionId")
            Prelude.<*> (x Prelude..:? "findingId")
            Prelude.<*> (x Prelude..:? "taskId")
            Prelude.<*> (x Prelude..:? "startTime")
            Prelude.<*> (x Prelude..:? "endTime")
            Prelude.<*> (x Prelude..:? "errorCode")
      )

instance
  Prelude.Hashable
    AuditMitigationActionExecutionMetadata

instance
  Prelude.NFData
    AuditMitigationActionExecutionMetadata
