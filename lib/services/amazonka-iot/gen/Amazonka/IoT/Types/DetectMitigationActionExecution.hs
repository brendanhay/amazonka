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
-- Module      : Amazonka.IoT.Types.DetectMitigationActionExecution
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.DetectMitigationActionExecution where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT.Types.DetectMitigationActionExecutionStatus
import qualified Amazonka.Prelude as Prelude

-- | Describes which mitigation actions should be executed.
--
-- /See:/ 'newDetectMitigationActionExecution' smart constructor.
data DetectMitigationActionExecution = DetectMitigationActionExecution'
  { -- | The message of a mitigation action.
    message :: Prelude.Maybe Prelude.Text,
    -- | The friendly name that uniquely identifies the mitigation action.
    actionName :: Prelude.Maybe Prelude.Text,
    -- | The name of the thing.
    thingName :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the violation.
    violationId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the task.
    taskId :: Prelude.Maybe Prelude.Text,
    -- | The date a mitigation action was started.
    executionStartDate :: Prelude.Maybe Core.POSIX,
    -- | The status of a mitigation action.
    status :: Prelude.Maybe DetectMitigationActionExecutionStatus,
    -- | The date a mitigation action ended.
    executionEndDate :: Prelude.Maybe Core.POSIX,
    -- | The error code of a mitigation action.
    errorCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectMitigationActionExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'detectMitigationActionExecution_message' - The message of a mitigation action.
--
-- 'actionName', 'detectMitigationActionExecution_actionName' - The friendly name that uniquely identifies the mitigation action.
--
-- 'thingName', 'detectMitigationActionExecution_thingName' - The name of the thing.
--
-- 'violationId', 'detectMitigationActionExecution_violationId' - The unique identifier of the violation.
--
-- 'taskId', 'detectMitigationActionExecution_taskId' - The unique identifier of the task.
--
-- 'executionStartDate', 'detectMitigationActionExecution_executionStartDate' - The date a mitigation action was started.
--
-- 'status', 'detectMitigationActionExecution_status' - The status of a mitigation action.
--
-- 'executionEndDate', 'detectMitigationActionExecution_executionEndDate' - The date a mitigation action ended.
--
-- 'errorCode', 'detectMitigationActionExecution_errorCode' - The error code of a mitigation action.
newDetectMitigationActionExecution ::
  DetectMitigationActionExecution
newDetectMitigationActionExecution =
  DetectMitigationActionExecution'
    { message =
        Prelude.Nothing,
      actionName = Prelude.Nothing,
      thingName = Prelude.Nothing,
      violationId = Prelude.Nothing,
      taskId = Prelude.Nothing,
      executionStartDate = Prelude.Nothing,
      status = Prelude.Nothing,
      executionEndDate = Prelude.Nothing,
      errorCode = Prelude.Nothing
    }

-- | The message of a mitigation action.
detectMitigationActionExecution_message :: Lens.Lens' DetectMitigationActionExecution (Prelude.Maybe Prelude.Text)
detectMitigationActionExecution_message = Lens.lens (\DetectMitigationActionExecution' {message} -> message) (\s@DetectMitigationActionExecution' {} a -> s {message = a} :: DetectMitigationActionExecution)

-- | The friendly name that uniquely identifies the mitigation action.
detectMitigationActionExecution_actionName :: Lens.Lens' DetectMitigationActionExecution (Prelude.Maybe Prelude.Text)
detectMitigationActionExecution_actionName = Lens.lens (\DetectMitigationActionExecution' {actionName} -> actionName) (\s@DetectMitigationActionExecution' {} a -> s {actionName = a} :: DetectMitigationActionExecution)

-- | The name of the thing.
detectMitigationActionExecution_thingName :: Lens.Lens' DetectMitigationActionExecution (Prelude.Maybe Prelude.Text)
detectMitigationActionExecution_thingName = Lens.lens (\DetectMitigationActionExecution' {thingName} -> thingName) (\s@DetectMitigationActionExecution' {} a -> s {thingName = a} :: DetectMitigationActionExecution)

-- | The unique identifier of the violation.
detectMitigationActionExecution_violationId :: Lens.Lens' DetectMitigationActionExecution (Prelude.Maybe Prelude.Text)
detectMitigationActionExecution_violationId = Lens.lens (\DetectMitigationActionExecution' {violationId} -> violationId) (\s@DetectMitigationActionExecution' {} a -> s {violationId = a} :: DetectMitigationActionExecution)

-- | The unique identifier of the task.
detectMitigationActionExecution_taskId :: Lens.Lens' DetectMitigationActionExecution (Prelude.Maybe Prelude.Text)
detectMitigationActionExecution_taskId = Lens.lens (\DetectMitigationActionExecution' {taskId} -> taskId) (\s@DetectMitigationActionExecution' {} a -> s {taskId = a} :: DetectMitigationActionExecution)

-- | The date a mitigation action was started.
detectMitigationActionExecution_executionStartDate :: Lens.Lens' DetectMitigationActionExecution (Prelude.Maybe Prelude.UTCTime)
detectMitigationActionExecution_executionStartDate = Lens.lens (\DetectMitigationActionExecution' {executionStartDate} -> executionStartDate) (\s@DetectMitigationActionExecution' {} a -> s {executionStartDate = a} :: DetectMitigationActionExecution) Prelude.. Lens.mapping Core._Time

-- | The status of a mitigation action.
detectMitigationActionExecution_status :: Lens.Lens' DetectMitigationActionExecution (Prelude.Maybe DetectMitigationActionExecutionStatus)
detectMitigationActionExecution_status = Lens.lens (\DetectMitigationActionExecution' {status} -> status) (\s@DetectMitigationActionExecution' {} a -> s {status = a} :: DetectMitigationActionExecution)

-- | The date a mitigation action ended.
detectMitigationActionExecution_executionEndDate :: Lens.Lens' DetectMitigationActionExecution (Prelude.Maybe Prelude.UTCTime)
detectMitigationActionExecution_executionEndDate = Lens.lens (\DetectMitigationActionExecution' {executionEndDate} -> executionEndDate) (\s@DetectMitigationActionExecution' {} a -> s {executionEndDate = a} :: DetectMitigationActionExecution) Prelude.. Lens.mapping Core._Time

-- | The error code of a mitigation action.
detectMitigationActionExecution_errorCode :: Lens.Lens' DetectMitigationActionExecution (Prelude.Maybe Prelude.Text)
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
            Prelude.<$> (x Core..:? "message")
            Prelude.<*> (x Core..:? "actionName")
            Prelude.<*> (x Core..:? "thingName")
            Prelude.<*> (x Core..:? "violationId")
            Prelude.<*> (x Core..:? "taskId")
            Prelude.<*> (x Core..:? "executionStartDate")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "executionEndDate")
            Prelude.<*> (x Core..:? "errorCode")
      )

instance
  Prelude.Hashable
    DetectMitigationActionExecution
  where
  hashWithSalt
    _salt
    DetectMitigationActionExecution' {..} =
      _salt `Prelude.hashWithSalt` message
        `Prelude.hashWithSalt` actionName
        `Prelude.hashWithSalt` thingName
        `Prelude.hashWithSalt` violationId
        `Prelude.hashWithSalt` taskId
        `Prelude.hashWithSalt` executionStartDate
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` executionEndDate
        `Prelude.hashWithSalt` errorCode

instance
  Prelude.NFData
    DetectMitigationActionExecution
  where
  rnf DetectMitigationActionExecution' {..} =
    Prelude.rnf message
      `Prelude.seq` Prelude.rnf actionName
      `Prelude.seq` Prelude.rnf thingName
      `Prelude.seq` Prelude.rnf violationId
      `Prelude.seq` Prelude.rnf taskId
      `Prelude.seq` Prelude.rnf executionStartDate
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf executionEndDate
      `Prelude.seq` Prelude.rnf errorCode
