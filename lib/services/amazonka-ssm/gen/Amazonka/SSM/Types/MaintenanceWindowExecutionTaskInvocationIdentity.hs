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
-- Module      : Amazonka.SSM.Types.MaintenanceWindowExecutionTaskInvocationIdentity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.MaintenanceWindowExecutionTaskInvocationIdentity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.MaintenanceWindowExecutionStatus
import Amazonka.SSM.Types.MaintenanceWindowTaskType

-- | Describes the information about a task invocation for a particular
-- target as part of a task execution performed as part of a maintenance
-- window execution.
--
-- /See:/ 'newMaintenanceWindowExecutionTaskInvocationIdentity' smart constructor.
data MaintenanceWindowExecutionTaskInvocationIdentity = MaintenanceWindowExecutionTaskInvocationIdentity'
  { -- | The time the invocation finished.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The ID of the action performed in the service that actually handled the
    -- task invocation. If the task type is @RUN_COMMAND@, this value is the
    -- command ID.
    executionId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the task invocation.
    invocationId :: Prelude.Maybe Prelude.Text,
    -- | User-provided value that was specified when the target was registered
    -- with the maintenance window. This was also included in any Amazon
    -- CloudWatch Events events raised during the task invocation.
    ownerInformation :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The parameters that were provided for the invocation when it was run.
    parameters :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The time the invocation started.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The status of the task invocation.
    status :: Prelude.Maybe MaintenanceWindowExecutionStatus,
    -- | The details explaining the status of the task invocation. Not available
    -- for all status values.
    statusDetails :: Prelude.Maybe Prelude.Text,
    -- | The ID of the specific task execution in the maintenance window
    -- execution.
    taskExecutionId :: Prelude.Maybe Prelude.Text,
    -- | The task type.
    taskType :: Prelude.Maybe MaintenanceWindowTaskType,
    -- | The ID of the maintenance window execution that ran the task.
    windowExecutionId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the target definition in this maintenance window the
    -- invocation was performed for.
    windowTargetId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MaintenanceWindowExecutionTaskInvocationIdentity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endTime', 'maintenanceWindowExecutionTaskInvocationIdentity_endTime' - The time the invocation finished.
--
-- 'executionId', 'maintenanceWindowExecutionTaskInvocationIdentity_executionId' - The ID of the action performed in the service that actually handled the
-- task invocation. If the task type is @RUN_COMMAND@, this value is the
-- command ID.
--
-- 'invocationId', 'maintenanceWindowExecutionTaskInvocationIdentity_invocationId' - The ID of the task invocation.
--
-- 'ownerInformation', 'maintenanceWindowExecutionTaskInvocationIdentity_ownerInformation' - User-provided value that was specified when the target was registered
-- with the maintenance window. This was also included in any Amazon
-- CloudWatch Events events raised during the task invocation.
--
-- 'parameters', 'maintenanceWindowExecutionTaskInvocationIdentity_parameters' - The parameters that were provided for the invocation when it was run.
--
-- 'startTime', 'maintenanceWindowExecutionTaskInvocationIdentity_startTime' - The time the invocation started.
--
-- 'status', 'maintenanceWindowExecutionTaskInvocationIdentity_status' - The status of the task invocation.
--
-- 'statusDetails', 'maintenanceWindowExecutionTaskInvocationIdentity_statusDetails' - The details explaining the status of the task invocation. Not available
-- for all status values.
--
-- 'taskExecutionId', 'maintenanceWindowExecutionTaskInvocationIdentity_taskExecutionId' - The ID of the specific task execution in the maintenance window
-- execution.
--
-- 'taskType', 'maintenanceWindowExecutionTaskInvocationIdentity_taskType' - The task type.
--
-- 'windowExecutionId', 'maintenanceWindowExecutionTaskInvocationIdentity_windowExecutionId' - The ID of the maintenance window execution that ran the task.
--
-- 'windowTargetId', 'maintenanceWindowExecutionTaskInvocationIdentity_windowTargetId' - The ID of the target definition in this maintenance window the
-- invocation was performed for.
newMaintenanceWindowExecutionTaskInvocationIdentity ::
  MaintenanceWindowExecutionTaskInvocationIdentity
newMaintenanceWindowExecutionTaskInvocationIdentity =
  MaintenanceWindowExecutionTaskInvocationIdentity'
    { endTime =
        Prelude.Nothing,
      executionId =
        Prelude.Nothing,
      invocationId =
        Prelude.Nothing,
      ownerInformation =
        Prelude.Nothing,
      parameters =
        Prelude.Nothing,
      startTime =
        Prelude.Nothing,
      status = Prelude.Nothing,
      statusDetails =
        Prelude.Nothing,
      taskExecutionId =
        Prelude.Nothing,
      taskType =
        Prelude.Nothing,
      windowExecutionId =
        Prelude.Nothing,
      windowTargetId =
        Prelude.Nothing
    }

-- | The time the invocation finished.
maintenanceWindowExecutionTaskInvocationIdentity_endTime :: Lens.Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Prelude.Maybe Prelude.UTCTime)
maintenanceWindowExecutionTaskInvocationIdentity_endTime = Lens.lens (\MaintenanceWindowExecutionTaskInvocationIdentity' {endTime} -> endTime) (\s@MaintenanceWindowExecutionTaskInvocationIdentity' {} a -> s {endTime = a} :: MaintenanceWindowExecutionTaskInvocationIdentity) Prelude.. Lens.mapping Data._Time

-- | The ID of the action performed in the service that actually handled the
-- task invocation. If the task type is @RUN_COMMAND@, this value is the
-- command ID.
maintenanceWindowExecutionTaskInvocationIdentity_executionId :: Lens.Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Prelude.Maybe Prelude.Text)
maintenanceWindowExecutionTaskInvocationIdentity_executionId = Lens.lens (\MaintenanceWindowExecutionTaskInvocationIdentity' {executionId} -> executionId) (\s@MaintenanceWindowExecutionTaskInvocationIdentity' {} a -> s {executionId = a} :: MaintenanceWindowExecutionTaskInvocationIdentity)

-- | The ID of the task invocation.
maintenanceWindowExecutionTaskInvocationIdentity_invocationId :: Lens.Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Prelude.Maybe Prelude.Text)
maintenanceWindowExecutionTaskInvocationIdentity_invocationId = Lens.lens (\MaintenanceWindowExecutionTaskInvocationIdentity' {invocationId} -> invocationId) (\s@MaintenanceWindowExecutionTaskInvocationIdentity' {} a -> s {invocationId = a} :: MaintenanceWindowExecutionTaskInvocationIdentity)

-- | User-provided value that was specified when the target was registered
-- with the maintenance window. This was also included in any Amazon
-- CloudWatch Events events raised during the task invocation.
maintenanceWindowExecutionTaskInvocationIdentity_ownerInformation :: Lens.Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Prelude.Maybe Prelude.Text)
maintenanceWindowExecutionTaskInvocationIdentity_ownerInformation = Lens.lens (\MaintenanceWindowExecutionTaskInvocationIdentity' {ownerInformation} -> ownerInformation) (\s@MaintenanceWindowExecutionTaskInvocationIdentity' {} a -> s {ownerInformation = a} :: MaintenanceWindowExecutionTaskInvocationIdentity) Prelude.. Lens.mapping Data._Sensitive

-- | The parameters that were provided for the invocation when it was run.
maintenanceWindowExecutionTaskInvocationIdentity_parameters :: Lens.Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Prelude.Maybe Prelude.Text)
maintenanceWindowExecutionTaskInvocationIdentity_parameters = Lens.lens (\MaintenanceWindowExecutionTaskInvocationIdentity' {parameters} -> parameters) (\s@MaintenanceWindowExecutionTaskInvocationIdentity' {} a -> s {parameters = a} :: MaintenanceWindowExecutionTaskInvocationIdentity) Prelude.. Lens.mapping Data._Sensitive

-- | The time the invocation started.
maintenanceWindowExecutionTaskInvocationIdentity_startTime :: Lens.Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Prelude.Maybe Prelude.UTCTime)
maintenanceWindowExecutionTaskInvocationIdentity_startTime = Lens.lens (\MaintenanceWindowExecutionTaskInvocationIdentity' {startTime} -> startTime) (\s@MaintenanceWindowExecutionTaskInvocationIdentity' {} a -> s {startTime = a} :: MaintenanceWindowExecutionTaskInvocationIdentity) Prelude.. Lens.mapping Data._Time

-- | The status of the task invocation.
maintenanceWindowExecutionTaskInvocationIdentity_status :: Lens.Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Prelude.Maybe MaintenanceWindowExecutionStatus)
maintenanceWindowExecutionTaskInvocationIdentity_status = Lens.lens (\MaintenanceWindowExecutionTaskInvocationIdentity' {status} -> status) (\s@MaintenanceWindowExecutionTaskInvocationIdentity' {} a -> s {status = a} :: MaintenanceWindowExecutionTaskInvocationIdentity)

-- | The details explaining the status of the task invocation. Not available
-- for all status values.
maintenanceWindowExecutionTaskInvocationIdentity_statusDetails :: Lens.Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Prelude.Maybe Prelude.Text)
maintenanceWindowExecutionTaskInvocationIdentity_statusDetails = Lens.lens (\MaintenanceWindowExecutionTaskInvocationIdentity' {statusDetails} -> statusDetails) (\s@MaintenanceWindowExecutionTaskInvocationIdentity' {} a -> s {statusDetails = a} :: MaintenanceWindowExecutionTaskInvocationIdentity)

-- | The ID of the specific task execution in the maintenance window
-- execution.
maintenanceWindowExecutionTaskInvocationIdentity_taskExecutionId :: Lens.Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Prelude.Maybe Prelude.Text)
maintenanceWindowExecutionTaskInvocationIdentity_taskExecutionId = Lens.lens (\MaintenanceWindowExecutionTaskInvocationIdentity' {taskExecutionId} -> taskExecutionId) (\s@MaintenanceWindowExecutionTaskInvocationIdentity' {} a -> s {taskExecutionId = a} :: MaintenanceWindowExecutionTaskInvocationIdentity)

-- | The task type.
maintenanceWindowExecutionTaskInvocationIdentity_taskType :: Lens.Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Prelude.Maybe MaintenanceWindowTaskType)
maintenanceWindowExecutionTaskInvocationIdentity_taskType = Lens.lens (\MaintenanceWindowExecutionTaskInvocationIdentity' {taskType} -> taskType) (\s@MaintenanceWindowExecutionTaskInvocationIdentity' {} a -> s {taskType = a} :: MaintenanceWindowExecutionTaskInvocationIdentity)

-- | The ID of the maintenance window execution that ran the task.
maintenanceWindowExecutionTaskInvocationIdentity_windowExecutionId :: Lens.Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Prelude.Maybe Prelude.Text)
maintenanceWindowExecutionTaskInvocationIdentity_windowExecutionId = Lens.lens (\MaintenanceWindowExecutionTaskInvocationIdentity' {windowExecutionId} -> windowExecutionId) (\s@MaintenanceWindowExecutionTaskInvocationIdentity' {} a -> s {windowExecutionId = a} :: MaintenanceWindowExecutionTaskInvocationIdentity)

-- | The ID of the target definition in this maintenance window the
-- invocation was performed for.
maintenanceWindowExecutionTaskInvocationIdentity_windowTargetId :: Lens.Lens' MaintenanceWindowExecutionTaskInvocationIdentity (Prelude.Maybe Prelude.Text)
maintenanceWindowExecutionTaskInvocationIdentity_windowTargetId = Lens.lens (\MaintenanceWindowExecutionTaskInvocationIdentity' {windowTargetId} -> windowTargetId) (\s@MaintenanceWindowExecutionTaskInvocationIdentity' {} a -> s {windowTargetId = a} :: MaintenanceWindowExecutionTaskInvocationIdentity)

instance
  Data.FromJSON
    MaintenanceWindowExecutionTaskInvocationIdentity
  where
  parseJSON =
    Data.withObject
      "MaintenanceWindowExecutionTaskInvocationIdentity"
      ( \x ->
          MaintenanceWindowExecutionTaskInvocationIdentity'
            Prelude.<$> (x Data..:? "EndTime")
            Prelude.<*> (x Data..:? "ExecutionId")
            Prelude.<*> (x Data..:? "InvocationId")
            Prelude.<*> (x Data..:? "OwnerInformation")
            Prelude.<*> (x Data..:? "Parameters")
            Prelude.<*> (x Data..:? "StartTime")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "StatusDetails")
            Prelude.<*> (x Data..:? "TaskExecutionId")
            Prelude.<*> (x Data..:? "TaskType")
            Prelude.<*> (x Data..:? "WindowExecutionId")
            Prelude.<*> (x Data..:? "WindowTargetId")
      )

instance
  Prelude.Hashable
    MaintenanceWindowExecutionTaskInvocationIdentity
  where
  hashWithSalt
    _salt
    MaintenanceWindowExecutionTaskInvocationIdentity' {..} =
      _salt
        `Prelude.hashWithSalt` endTime
        `Prelude.hashWithSalt` executionId
        `Prelude.hashWithSalt` invocationId
        `Prelude.hashWithSalt` ownerInformation
        `Prelude.hashWithSalt` parameters
        `Prelude.hashWithSalt` startTime
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` statusDetails
        `Prelude.hashWithSalt` taskExecutionId
        `Prelude.hashWithSalt` taskType
        `Prelude.hashWithSalt` windowExecutionId
        `Prelude.hashWithSalt` windowTargetId

instance
  Prelude.NFData
    MaintenanceWindowExecutionTaskInvocationIdentity
  where
  rnf
    MaintenanceWindowExecutionTaskInvocationIdentity' {..} =
      Prelude.rnf endTime
        `Prelude.seq` Prelude.rnf executionId
        `Prelude.seq` Prelude.rnf invocationId
        `Prelude.seq` Prelude.rnf ownerInformation
        `Prelude.seq` Prelude.rnf parameters
        `Prelude.seq` Prelude.rnf startTime
        `Prelude.seq` Prelude.rnf status
        `Prelude.seq` Prelude.rnf statusDetails
        `Prelude.seq` Prelude.rnf taskExecutionId
        `Prelude.seq` Prelude.rnf taskType
        `Prelude.seq` Prelude.rnf windowExecutionId
        `Prelude.seq` Prelude.rnf windowTargetId
