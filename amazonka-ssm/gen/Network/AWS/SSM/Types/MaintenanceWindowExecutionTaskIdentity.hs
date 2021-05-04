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
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowExecutionTaskIdentity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.MaintenanceWindowExecutionTaskIdentity where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSM.Types.MaintenanceWindowExecutionStatus
import Network.AWS.SSM.Types.MaintenanceWindowTaskType

-- | Information about a task execution performed as part of a maintenance
-- window execution.
--
-- /See:/ 'newMaintenanceWindowExecutionTaskIdentity' smart constructor.
data MaintenanceWindowExecutionTaskIdentity = MaintenanceWindowExecutionTaskIdentity'
  { -- | The status of the task execution.
    status :: Prelude.Maybe MaintenanceWindowExecutionStatus,
    -- | The details explaining the status of the task execution. Only available
    -- for certain status values.
    statusDetails :: Prelude.Maybe Prelude.Text,
    -- | The time the task execution started.
    startTime :: Prelude.Maybe Prelude.POSIX,
    -- | The time the task execution finished.
    endTime :: Prelude.Maybe Prelude.POSIX,
    -- | The ID of the maintenance window execution that ran the task.
    windowExecutionId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the task that ran.
    taskArn :: Prelude.Maybe Prelude.Text,
    -- | The type of task that ran.
    taskType :: Prelude.Maybe MaintenanceWindowTaskType,
    -- | The ID of the specific task execution in the maintenance window
    -- execution.
    taskExecutionId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MaintenanceWindowExecutionTaskIdentity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'maintenanceWindowExecutionTaskIdentity_status' - The status of the task execution.
--
-- 'statusDetails', 'maintenanceWindowExecutionTaskIdentity_statusDetails' - The details explaining the status of the task execution. Only available
-- for certain status values.
--
-- 'startTime', 'maintenanceWindowExecutionTaskIdentity_startTime' - The time the task execution started.
--
-- 'endTime', 'maintenanceWindowExecutionTaskIdentity_endTime' - The time the task execution finished.
--
-- 'windowExecutionId', 'maintenanceWindowExecutionTaskIdentity_windowExecutionId' - The ID of the maintenance window execution that ran the task.
--
-- 'taskArn', 'maintenanceWindowExecutionTaskIdentity_taskArn' - The ARN of the task that ran.
--
-- 'taskType', 'maintenanceWindowExecutionTaskIdentity_taskType' - The type of task that ran.
--
-- 'taskExecutionId', 'maintenanceWindowExecutionTaskIdentity_taskExecutionId' - The ID of the specific task execution in the maintenance window
-- execution.
newMaintenanceWindowExecutionTaskIdentity ::
  MaintenanceWindowExecutionTaskIdentity
newMaintenanceWindowExecutionTaskIdentity =
  MaintenanceWindowExecutionTaskIdentity'
    { status =
        Prelude.Nothing,
      statusDetails = Prelude.Nothing,
      startTime = Prelude.Nothing,
      endTime = Prelude.Nothing,
      windowExecutionId = Prelude.Nothing,
      taskArn = Prelude.Nothing,
      taskType = Prelude.Nothing,
      taskExecutionId = Prelude.Nothing
    }

-- | The status of the task execution.
maintenanceWindowExecutionTaskIdentity_status :: Lens.Lens' MaintenanceWindowExecutionTaskIdentity (Prelude.Maybe MaintenanceWindowExecutionStatus)
maintenanceWindowExecutionTaskIdentity_status = Lens.lens (\MaintenanceWindowExecutionTaskIdentity' {status} -> status) (\s@MaintenanceWindowExecutionTaskIdentity' {} a -> s {status = a} :: MaintenanceWindowExecutionTaskIdentity)

-- | The details explaining the status of the task execution. Only available
-- for certain status values.
maintenanceWindowExecutionTaskIdentity_statusDetails :: Lens.Lens' MaintenanceWindowExecutionTaskIdentity (Prelude.Maybe Prelude.Text)
maintenanceWindowExecutionTaskIdentity_statusDetails = Lens.lens (\MaintenanceWindowExecutionTaskIdentity' {statusDetails} -> statusDetails) (\s@MaintenanceWindowExecutionTaskIdentity' {} a -> s {statusDetails = a} :: MaintenanceWindowExecutionTaskIdentity)

-- | The time the task execution started.
maintenanceWindowExecutionTaskIdentity_startTime :: Lens.Lens' MaintenanceWindowExecutionTaskIdentity (Prelude.Maybe Prelude.UTCTime)
maintenanceWindowExecutionTaskIdentity_startTime = Lens.lens (\MaintenanceWindowExecutionTaskIdentity' {startTime} -> startTime) (\s@MaintenanceWindowExecutionTaskIdentity' {} a -> s {startTime = a} :: MaintenanceWindowExecutionTaskIdentity) Prelude.. Lens.mapping Prelude._Time

-- | The time the task execution finished.
maintenanceWindowExecutionTaskIdentity_endTime :: Lens.Lens' MaintenanceWindowExecutionTaskIdentity (Prelude.Maybe Prelude.UTCTime)
maintenanceWindowExecutionTaskIdentity_endTime = Lens.lens (\MaintenanceWindowExecutionTaskIdentity' {endTime} -> endTime) (\s@MaintenanceWindowExecutionTaskIdentity' {} a -> s {endTime = a} :: MaintenanceWindowExecutionTaskIdentity) Prelude.. Lens.mapping Prelude._Time

-- | The ID of the maintenance window execution that ran the task.
maintenanceWindowExecutionTaskIdentity_windowExecutionId :: Lens.Lens' MaintenanceWindowExecutionTaskIdentity (Prelude.Maybe Prelude.Text)
maintenanceWindowExecutionTaskIdentity_windowExecutionId = Lens.lens (\MaintenanceWindowExecutionTaskIdentity' {windowExecutionId} -> windowExecutionId) (\s@MaintenanceWindowExecutionTaskIdentity' {} a -> s {windowExecutionId = a} :: MaintenanceWindowExecutionTaskIdentity)

-- | The ARN of the task that ran.
maintenanceWindowExecutionTaskIdentity_taskArn :: Lens.Lens' MaintenanceWindowExecutionTaskIdentity (Prelude.Maybe Prelude.Text)
maintenanceWindowExecutionTaskIdentity_taskArn = Lens.lens (\MaintenanceWindowExecutionTaskIdentity' {taskArn} -> taskArn) (\s@MaintenanceWindowExecutionTaskIdentity' {} a -> s {taskArn = a} :: MaintenanceWindowExecutionTaskIdentity)

-- | The type of task that ran.
maintenanceWindowExecutionTaskIdentity_taskType :: Lens.Lens' MaintenanceWindowExecutionTaskIdentity (Prelude.Maybe MaintenanceWindowTaskType)
maintenanceWindowExecutionTaskIdentity_taskType = Lens.lens (\MaintenanceWindowExecutionTaskIdentity' {taskType} -> taskType) (\s@MaintenanceWindowExecutionTaskIdentity' {} a -> s {taskType = a} :: MaintenanceWindowExecutionTaskIdentity)

-- | The ID of the specific task execution in the maintenance window
-- execution.
maintenanceWindowExecutionTaskIdentity_taskExecutionId :: Lens.Lens' MaintenanceWindowExecutionTaskIdentity (Prelude.Maybe Prelude.Text)
maintenanceWindowExecutionTaskIdentity_taskExecutionId = Lens.lens (\MaintenanceWindowExecutionTaskIdentity' {taskExecutionId} -> taskExecutionId) (\s@MaintenanceWindowExecutionTaskIdentity' {} a -> s {taskExecutionId = a} :: MaintenanceWindowExecutionTaskIdentity)

instance
  Prelude.FromJSON
    MaintenanceWindowExecutionTaskIdentity
  where
  parseJSON =
    Prelude.withObject
      "MaintenanceWindowExecutionTaskIdentity"
      ( \x ->
          MaintenanceWindowExecutionTaskIdentity'
            Prelude.<$> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "StatusDetails")
            Prelude.<*> (x Prelude..:? "StartTime")
            Prelude.<*> (x Prelude..:? "EndTime")
            Prelude.<*> (x Prelude..:? "WindowExecutionId")
            Prelude.<*> (x Prelude..:? "TaskArn")
            Prelude.<*> (x Prelude..:? "TaskType")
            Prelude.<*> (x Prelude..:? "TaskExecutionId")
      )

instance
  Prelude.Hashable
    MaintenanceWindowExecutionTaskIdentity

instance
  Prelude.NFData
    MaintenanceWindowExecutionTaskIdentity
