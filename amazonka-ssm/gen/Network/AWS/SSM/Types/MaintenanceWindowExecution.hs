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
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowExecution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.MaintenanceWindowExecution where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SSM.Types.MaintenanceWindowExecutionStatus

-- | Describes the information about an execution of a maintenance window.
--
-- /See:/ 'newMaintenanceWindowExecution' smart constructor.
data MaintenanceWindowExecution = MaintenanceWindowExecution'
  { -- | The status of the execution.
    status :: Core.Maybe MaintenanceWindowExecutionStatus,
    -- | The details explaining the Status. Only available for certain status
    -- values.
    statusDetails :: Core.Maybe Core.Text,
    -- | The time the execution started.
    startTime :: Core.Maybe Core.POSIX,
    -- | The time the execution finished.
    endTime :: Core.Maybe Core.POSIX,
    -- | The ID of the maintenance window.
    windowId :: Core.Maybe Core.Text,
    -- | The ID of the maintenance window execution.
    windowExecutionId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MaintenanceWindowExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'maintenanceWindowExecution_status' - The status of the execution.
--
-- 'statusDetails', 'maintenanceWindowExecution_statusDetails' - The details explaining the Status. Only available for certain status
-- values.
--
-- 'startTime', 'maintenanceWindowExecution_startTime' - The time the execution started.
--
-- 'endTime', 'maintenanceWindowExecution_endTime' - The time the execution finished.
--
-- 'windowId', 'maintenanceWindowExecution_windowId' - The ID of the maintenance window.
--
-- 'windowExecutionId', 'maintenanceWindowExecution_windowExecutionId' - The ID of the maintenance window execution.
newMaintenanceWindowExecution ::
  MaintenanceWindowExecution
newMaintenanceWindowExecution =
  MaintenanceWindowExecution'
    { status = Core.Nothing,
      statusDetails = Core.Nothing,
      startTime = Core.Nothing,
      endTime = Core.Nothing,
      windowId = Core.Nothing,
      windowExecutionId = Core.Nothing
    }

-- | The status of the execution.
maintenanceWindowExecution_status :: Lens.Lens' MaintenanceWindowExecution (Core.Maybe MaintenanceWindowExecutionStatus)
maintenanceWindowExecution_status = Lens.lens (\MaintenanceWindowExecution' {status} -> status) (\s@MaintenanceWindowExecution' {} a -> s {status = a} :: MaintenanceWindowExecution)

-- | The details explaining the Status. Only available for certain status
-- values.
maintenanceWindowExecution_statusDetails :: Lens.Lens' MaintenanceWindowExecution (Core.Maybe Core.Text)
maintenanceWindowExecution_statusDetails = Lens.lens (\MaintenanceWindowExecution' {statusDetails} -> statusDetails) (\s@MaintenanceWindowExecution' {} a -> s {statusDetails = a} :: MaintenanceWindowExecution)

-- | The time the execution started.
maintenanceWindowExecution_startTime :: Lens.Lens' MaintenanceWindowExecution (Core.Maybe Core.UTCTime)
maintenanceWindowExecution_startTime = Lens.lens (\MaintenanceWindowExecution' {startTime} -> startTime) (\s@MaintenanceWindowExecution' {} a -> s {startTime = a} :: MaintenanceWindowExecution) Core.. Lens.mapping Core._Time

-- | The time the execution finished.
maintenanceWindowExecution_endTime :: Lens.Lens' MaintenanceWindowExecution (Core.Maybe Core.UTCTime)
maintenanceWindowExecution_endTime = Lens.lens (\MaintenanceWindowExecution' {endTime} -> endTime) (\s@MaintenanceWindowExecution' {} a -> s {endTime = a} :: MaintenanceWindowExecution) Core.. Lens.mapping Core._Time

-- | The ID of the maintenance window.
maintenanceWindowExecution_windowId :: Lens.Lens' MaintenanceWindowExecution (Core.Maybe Core.Text)
maintenanceWindowExecution_windowId = Lens.lens (\MaintenanceWindowExecution' {windowId} -> windowId) (\s@MaintenanceWindowExecution' {} a -> s {windowId = a} :: MaintenanceWindowExecution)

-- | The ID of the maintenance window execution.
maintenanceWindowExecution_windowExecutionId :: Lens.Lens' MaintenanceWindowExecution (Core.Maybe Core.Text)
maintenanceWindowExecution_windowExecutionId = Lens.lens (\MaintenanceWindowExecution' {windowExecutionId} -> windowExecutionId) (\s@MaintenanceWindowExecution' {} a -> s {windowExecutionId = a} :: MaintenanceWindowExecution)

instance Core.FromJSON MaintenanceWindowExecution where
  parseJSON =
    Core.withObject
      "MaintenanceWindowExecution"
      ( \x ->
          MaintenanceWindowExecution'
            Core.<$> (x Core..:? "Status")
            Core.<*> (x Core..:? "StatusDetails")
            Core.<*> (x Core..:? "StartTime")
            Core.<*> (x Core..:? "EndTime")
            Core.<*> (x Core..:? "WindowId")
            Core.<*> (x Core..:? "WindowExecutionId")
      )

instance Core.Hashable MaintenanceWindowExecution

instance Core.NFData MaintenanceWindowExecution
