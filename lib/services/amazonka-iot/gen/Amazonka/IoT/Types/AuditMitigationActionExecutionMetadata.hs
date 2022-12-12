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
-- Module      : Amazonka.IoT.Types.AuditMitigationActionExecutionMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.AuditMitigationActionExecutionMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.AuditMitigationActionsExecutionStatus
import qualified Amazonka.Prelude as Prelude

-- | Returned by ListAuditMitigationActionsTask, this object contains
-- information that describes a mitigation action that has been started.
--
-- /See:/ 'newAuditMitigationActionExecutionMetadata' smart constructor.
data AuditMitigationActionExecutionMetadata = AuditMitigationActionExecutionMetadata'
  { -- | The unique identifier for the mitigation action being applied by the
    -- task.
    actionId :: Prelude.Maybe Prelude.Text,
    -- | The friendly name of the mitigation action being applied by the task.
    actionName :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the task was completed or canceled. Blank if the
    -- task is still running.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | If an error occurred, the code that indicates which type of error
    -- occurred.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the findings to which the task and associated
    -- mitigation action are applied.
    findingId :: Prelude.Maybe Prelude.Text,
    -- | If an error occurred, a message that describes the error.
    message :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the task was started.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The current status of the task being executed.
    status :: Prelude.Maybe AuditMitigationActionsExecutionStatus,
    -- | The unique identifier for the task that applies the mitigation action.
    taskId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AuditMitigationActionExecutionMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionId', 'auditMitigationActionExecutionMetadata_actionId' - The unique identifier for the mitigation action being applied by the
-- task.
--
-- 'actionName', 'auditMitigationActionExecutionMetadata_actionName' - The friendly name of the mitigation action being applied by the task.
--
-- 'endTime', 'auditMitigationActionExecutionMetadata_endTime' - The date and time when the task was completed or canceled. Blank if the
-- task is still running.
--
-- 'errorCode', 'auditMitigationActionExecutionMetadata_errorCode' - If an error occurred, the code that indicates which type of error
-- occurred.
--
-- 'findingId', 'auditMitigationActionExecutionMetadata_findingId' - The unique identifier for the findings to which the task and associated
-- mitigation action are applied.
--
-- 'message', 'auditMitigationActionExecutionMetadata_message' - If an error occurred, a message that describes the error.
--
-- 'startTime', 'auditMitigationActionExecutionMetadata_startTime' - The date and time when the task was started.
--
-- 'status', 'auditMitigationActionExecutionMetadata_status' - The current status of the task being executed.
--
-- 'taskId', 'auditMitigationActionExecutionMetadata_taskId' - The unique identifier for the task that applies the mitigation action.
newAuditMitigationActionExecutionMetadata ::
  AuditMitigationActionExecutionMetadata
newAuditMitigationActionExecutionMetadata =
  AuditMitigationActionExecutionMetadata'
    { actionId =
        Prelude.Nothing,
      actionName = Prelude.Nothing,
      endTime = Prelude.Nothing,
      errorCode = Prelude.Nothing,
      findingId = Prelude.Nothing,
      message = Prelude.Nothing,
      startTime = Prelude.Nothing,
      status = Prelude.Nothing,
      taskId = Prelude.Nothing
    }

-- | The unique identifier for the mitigation action being applied by the
-- task.
auditMitigationActionExecutionMetadata_actionId :: Lens.Lens' AuditMitigationActionExecutionMetadata (Prelude.Maybe Prelude.Text)
auditMitigationActionExecutionMetadata_actionId = Lens.lens (\AuditMitigationActionExecutionMetadata' {actionId} -> actionId) (\s@AuditMitigationActionExecutionMetadata' {} a -> s {actionId = a} :: AuditMitigationActionExecutionMetadata)

-- | The friendly name of the mitigation action being applied by the task.
auditMitigationActionExecutionMetadata_actionName :: Lens.Lens' AuditMitigationActionExecutionMetadata (Prelude.Maybe Prelude.Text)
auditMitigationActionExecutionMetadata_actionName = Lens.lens (\AuditMitigationActionExecutionMetadata' {actionName} -> actionName) (\s@AuditMitigationActionExecutionMetadata' {} a -> s {actionName = a} :: AuditMitigationActionExecutionMetadata)

-- | The date and time when the task was completed or canceled. Blank if the
-- task is still running.
auditMitigationActionExecutionMetadata_endTime :: Lens.Lens' AuditMitigationActionExecutionMetadata (Prelude.Maybe Prelude.UTCTime)
auditMitigationActionExecutionMetadata_endTime = Lens.lens (\AuditMitigationActionExecutionMetadata' {endTime} -> endTime) (\s@AuditMitigationActionExecutionMetadata' {} a -> s {endTime = a} :: AuditMitigationActionExecutionMetadata) Prelude.. Lens.mapping Data._Time

-- | If an error occurred, the code that indicates which type of error
-- occurred.
auditMitigationActionExecutionMetadata_errorCode :: Lens.Lens' AuditMitigationActionExecutionMetadata (Prelude.Maybe Prelude.Text)
auditMitigationActionExecutionMetadata_errorCode = Lens.lens (\AuditMitigationActionExecutionMetadata' {errorCode} -> errorCode) (\s@AuditMitigationActionExecutionMetadata' {} a -> s {errorCode = a} :: AuditMitigationActionExecutionMetadata)

-- | The unique identifier for the findings to which the task and associated
-- mitigation action are applied.
auditMitigationActionExecutionMetadata_findingId :: Lens.Lens' AuditMitigationActionExecutionMetadata (Prelude.Maybe Prelude.Text)
auditMitigationActionExecutionMetadata_findingId = Lens.lens (\AuditMitigationActionExecutionMetadata' {findingId} -> findingId) (\s@AuditMitigationActionExecutionMetadata' {} a -> s {findingId = a} :: AuditMitigationActionExecutionMetadata)

-- | If an error occurred, a message that describes the error.
auditMitigationActionExecutionMetadata_message :: Lens.Lens' AuditMitigationActionExecutionMetadata (Prelude.Maybe Prelude.Text)
auditMitigationActionExecutionMetadata_message = Lens.lens (\AuditMitigationActionExecutionMetadata' {message} -> message) (\s@AuditMitigationActionExecutionMetadata' {} a -> s {message = a} :: AuditMitigationActionExecutionMetadata)

-- | The date and time when the task was started.
auditMitigationActionExecutionMetadata_startTime :: Lens.Lens' AuditMitigationActionExecutionMetadata (Prelude.Maybe Prelude.UTCTime)
auditMitigationActionExecutionMetadata_startTime = Lens.lens (\AuditMitigationActionExecutionMetadata' {startTime} -> startTime) (\s@AuditMitigationActionExecutionMetadata' {} a -> s {startTime = a} :: AuditMitigationActionExecutionMetadata) Prelude.. Lens.mapping Data._Time

-- | The current status of the task being executed.
auditMitigationActionExecutionMetadata_status :: Lens.Lens' AuditMitigationActionExecutionMetadata (Prelude.Maybe AuditMitigationActionsExecutionStatus)
auditMitigationActionExecutionMetadata_status = Lens.lens (\AuditMitigationActionExecutionMetadata' {status} -> status) (\s@AuditMitigationActionExecutionMetadata' {} a -> s {status = a} :: AuditMitigationActionExecutionMetadata)

-- | The unique identifier for the task that applies the mitigation action.
auditMitigationActionExecutionMetadata_taskId :: Lens.Lens' AuditMitigationActionExecutionMetadata (Prelude.Maybe Prelude.Text)
auditMitigationActionExecutionMetadata_taskId = Lens.lens (\AuditMitigationActionExecutionMetadata' {taskId} -> taskId) (\s@AuditMitigationActionExecutionMetadata' {} a -> s {taskId = a} :: AuditMitigationActionExecutionMetadata)

instance
  Data.FromJSON
    AuditMitigationActionExecutionMetadata
  where
  parseJSON =
    Data.withObject
      "AuditMitigationActionExecutionMetadata"
      ( \x ->
          AuditMitigationActionExecutionMetadata'
            Prelude.<$> (x Data..:? "actionId")
            Prelude.<*> (x Data..:? "actionName")
            Prelude.<*> (x Data..:? "endTime")
            Prelude.<*> (x Data..:? "errorCode")
            Prelude.<*> (x Data..:? "findingId")
            Prelude.<*> (x Data..:? "message")
            Prelude.<*> (x Data..:? "startTime")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "taskId")
      )

instance
  Prelude.Hashable
    AuditMitigationActionExecutionMetadata
  where
  hashWithSalt
    _salt
    AuditMitigationActionExecutionMetadata' {..} =
      _salt `Prelude.hashWithSalt` actionId
        `Prelude.hashWithSalt` actionName
        `Prelude.hashWithSalt` endTime
        `Prelude.hashWithSalt` errorCode
        `Prelude.hashWithSalt` findingId
        `Prelude.hashWithSalt` message
        `Prelude.hashWithSalt` startTime
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` taskId

instance
  Prelude.NFData
    AuditMitigationActionExecutionMetadata
  where
  rnf AuditMitigationActionExecutionMetadata' {..} =
    Prelude.rnf actionId
      `Prelude.seq` Prelude.rnf actionName
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf findingId
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf taskId
