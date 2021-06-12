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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SSM.Types.MaintenanceWindowExecutionStatus
import Network.AWS.SSM.Types.MaintenanceWindowTaskType

-- | Information about a task execution performed as part of a maintenance
-- window execution.
--
-- /See:/ 'newMaintenanceWindowExecutionTaskIdentity' smart constructor.
data MaintenanceWindowExecutionTaskIdentity = MaintenanceWindowExecutionTaskIdentity'
  { -- | The status of the task execution.
    status :: Core.Maybe MaintenanceWindowExecutionStatus,
    -- | The details explaining the status of the task execution. Only available
    -- for certain status values.
    statusDetails :: Core.Maybe Core.Text,
    -- | The time the task execution started.
    startTime :: Core.Maybe Core.POSIX,
    -- | The time the task execution finished.
    endTime :: Core.Maybe Core.POSIX,
    -- | The ID of the maintenance window execution that ran the task.
    windowExecutionId :: Core.Maybe Core.Text,
    -- | The ARN of the task that ran.
    taskArn :: Core.Maybe Core.Text,
    -- | The type of task that ran.
    taskType :: Core.Maybe MaintenanceWindowTaskType,
    -- | The ID of the specific task execution in the maintenance window
    -- execution.
    taskExecutionId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      statusDetails = Core.Nothing,
      startTime = Core.Nothing,
      endTime = Core.Nothing,
      windowExecutionId = Core.Nothing,
      taskArn = Core.Nothing,
      taskType = Core.Nothing,
      taskExecutionId = Core.Nothing
    }

-- | The status of the task execution.
maintenanceWindowExecutionTaskIdentity_status :: Lens.Lens' MaintenanceWindowExecutionTaskIdentity (Core.Maybe MaintenanceWindowExecutionStatus)
maintenanceWindowExecutionTaskIdentity_status = Lens.lens (\MaintenanceWindowExecutionTaskIdentity' {status} -> status) (\s@MaintenanceWindowExecutionTaskIdentity' {} a -> s {status = a} :: MaintenanceWindowExecutionTaskIdentity)

-- | The details explaining the status of the task execution. Only available
-- for certain status values.
maintenanceWindowExecutionTaskIdentity_statusDetails :: Lens.Lens' MaintenanceWindowExecutionTaskIdentity (Core.Maybe Core.Text)
maintenanceWindowExecutionTaskIdentity_statusDetails = Lens.lens (\MaintenanceWindowExecutionTaskIdentity' {statusDetails} -> statusDetails) (\s@MaintenanceWindowExecutionTaskIdentity' {} a -> s {statusDetails = a} :: MaintenanceWindowExecutionTaskIdentity)

-- | The time the task execution started.
maintenanceWindowExecutionTaskIdentity_startTime :: Lens.Lens' MaintenanceWindowExecutionTaskIdentity (Core.Maybe Core.UTCTime)
maintenanceWindowExecutionTaskIdentity_startTime = Lens.lens (\MaintenanceWindowExecutionTaskIdentity' {startTime} -> startTime) (\s@MaintenanceWindowExecutionTaskIdentity' {} a -> s {startTime = a} :: MaintenanceWindowExecutionTaskIdentity) Core.. Lens.mapping Core._Time

-- | The time the task execution finished.
maintenanceWindowExecutionTaskIdentity_endTime :: Lens.Lens' MaintenanceWindowExecutionTaskIdentity (Core.Maybe Core.UTCTime)
maintenanceWindowExecutionTaskIdentity_endTime = Lens.lens (\MaintenanceWindowExecutionTaskIdentity' {endTime} -> endTime) (\s@MaintenanceWindowExecutionTaskIdentity' {} a -> s {endTime = a} :: MaintenanceWindowExecutionTaskIdentity) Core.. Lens.mapping Core._Time

-- | The ID of the maintenance window execution that ran the task.
maintenanceWindowExecutionTaskIdentity_windowExecutionId :: Lens.Lens' MaintenanceWindowExecutionTaskIdentity (Core.Maybe Core.Text)
maintenanceWindowExecutionTaskIdentity_windowExecutionId = Lens.lens (\MaintenanceWindowExecutionTaskIdentity' {windowExecutionId} -> windowExecutionId) (\s@MaintenanceWindowExecutionTaskIdentity' {} a -> s {windowExecutionId = a} :: MaintenanceWindowExecutionTaskIdentity)

-- | The ARN of the task that ran.
maintenanceWindowExecutionTaskIdentity_taskArn :: Lens.Lens' MaintenanceWindowExecutionTaskIdentity (Core.Maybe Core.Text)
maintenanceWindowExecutionTaskIdentity_taskArn = Lens.lens (\MaintenanceWindowExecutionTaskIdentity' {taskArn} -> taskArn) (\s@MaintenanceWindowExecutionTaskIdentity' {} a -> s {taskArn = a} :: MaintenanceWindowExecutionTaskIdentity)

-- | The type of task that ran.
maintenanceWindowExecutionTaskIdentity_taskType :: Lens.Lens' MaintenanceWindowExecutionTaskIdentity (Core.Maybe MaintenanceWindowTaskType)
maintenanceWindowExecutionTaskIdentity_taskType = Lens.lens (\MaintenanceWindowExecutionTaskIdentity' {taskType} -> taskType) (\s@MaintenanceWindowExecutionTaskIdentity' {} a -> s {taskType = a} :: MaintenanceWindowExecutionTaskIdentity)

-- | The ID of the specific task execution in the maintenance window
-- execution.
maintenanceWindowExecutionTaskIdentity_taskExecutionId :: Lens.Lens' MaintenanceWindowExecutionTaskIdentity (Core.Maybe Core.Text)
maintenanceWindowExecutionTaskIdentity_taskExecutionId = Lens.lens (\MaintenanceWindowExecutionTaskIdentity' {taskExecutionId} -> taskExecutionId) (\s@MaintenanceWindowExecutionTaskIdentity' {} a -> s {taskExecutionId = a} :: MaintenanceWindowExecutionTaskIdentity)

instance
  Core.FromJSON
    MaintenanceWindowExecutionTaskIdentity
  where
  parseJSON =
    Core.withObject
      "MaintenanceWindowExecutionTaskIdentity"
      ( \x ->
          MaintenanceWindowExecutionTaskIdentity'
            Core.<$> (x Core..:? "Status")
            Core.<*> (x Core..:? "StatusDetails")
            Core.<*> (x Core..:? "StartTime")
            Core.<*> (x Core..:? "EndTime")
            Core.<*> (x Core..:? "WindowExecutionId")
            Core.<*> (x Core..:? "TaskArn")
            Core.<*> (x Core..:? "TaskType")
            Core.<*> (x Core..:? "TaskExecutionId")
      )

instance
  Core.Hashable
    MaintenanceWindowExecutionTaskIdentity

instance
  Core.NFData
    MaintenanceWindowExecutionTaskIdentity
