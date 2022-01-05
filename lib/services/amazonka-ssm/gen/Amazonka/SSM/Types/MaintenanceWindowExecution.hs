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
-- Module      : Amazonka.SSM.Types.MaintenanceWindowExecution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.MaintenanceWindowExecution where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.MaintenanceWindowExecutionStatus

-- | Describes the information about an execution of a maintenance window.
--
-- /See:/ 'newMaintenanceWindowExecution' smart constructor.
data MaintenanceWindowExecution = MaintenanceWindowExecution'
  { -- | The status of the execution.
    status :: Prelude.Maybe MaintenanceWindowExecutionStatus,
    -- | The time the execution started.
    startTime :: Prelude.Maybe Core.POSIX,
    -- | The ID of the maintenance window execution.
    windowExecutionId :: Prelude.Maybe Prelude.Text,
    -- | The details explaining the status. Not available for all status values.
    statusDetails :: Prelude.Maybe Prelude.Text,
    -- | The time the execution finished.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | The ID of the maintenance window.
    windowId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'startTime', 'maintenanceWindowExecution_startTime' - The time the execution started.
--
-- 'windowExecutionId', 'maintenanceWindowExecution_windowExecutionId' - The ID of the maintenance window execution.
--
-- 'statusDetails', 'maintenanceWindowExecution_statusDetails' - The details explaining the status. Not available for all status values.
--
-- 'endTime', 'maintenanceWindowExecution_endTime' - The time the execution finished.
--
-- 'windowId', 'maintenanceWindowExecution_windowId' - The ID of the maintenance window.
newMaintenanceWindowExecution ::
  MaintenanceWindowExecution
newMaintenanceWindowExecution =
  MaintenanceWindowExecution'
    { status =
        Prelude.Nothing,
      startTime = Prelude.Nothing,
      windowExecutionId = Prelude.Nothing,
      statusDetails = Prelude.Nothing,
      endTime = Prelude.Nothing,
      windowId = Prelude.Nothing
    }

-- | The status of the execution.
maintenanceWindowExecution_status :: Lens.Lens' MaintenanceWindowExecution (Prelude.Maybe MaintenanceWindowExecutionStatus)
maintenanceWindowExecution_status = Lens.lens (\MaintenanceWindowExecution' {status} -> status) (\s@MaintenanceWindowExecution' {} a -> s {status = a} :: MaintenanceWindowExecution)

-- | The time the execution started.
maintenanceWindowExecution_startTime :: Lens.Lens' MaintenanceWindowExecution (Prelude.Maybe Prelude.UTCTime)
maintenanceWindowExecution_startTime = Lens.lens (\MaintenanceWindowExecution' {startTime} -> startTime) (\s@MaintenanceWindowExecution' {} a -> s {startTime = a} :: MaintenanceWindowExecution) Prelude.. Lens.mapping Core._Time

-- | The ID of the maintenance window execution.
maintenanceWindowExecution_windowExecutionId :: Lens.Lens' MaintenanceWindowExecution (Prelude.Maybe Prelude.Text)
maintenanceWindowExecution_windowExecutionId = Lens.lens (\MaintenanceWindowExecution' {windowExecutionId} -> windowExecutionId) (\s@MaintenanceWindowExecution' {} a -> s {windowExecutionId = a} :: MaintenanceWindowExecution)

-- | The details explaining the status. Not available for all status values.
maintenanceWindowExecution_statusDetails :: Lens.Lens' MaintenanceWindowExecution (Prelude.Maybe Prelude.Text)
maintenanceWindowExecution_statusDetails = Lens.lens (\MaintenanceWindowExecution' {statusDetails} -> statusDetails) (\s@MaintenanceWindowExecution' {} a -> s {statusDetails = a} :: MaintenanceWindowExecution)

-- | The time the execution finished.
maintenanceWindowExecution_endTime :: Lens.Lens' MaintenanceWindowExecution (Prelude.Maybe Prelude.UTCTime)
maintenanceWindowExecution_endTime = Lens.lens (\MaintenanceWindowExecution' {endTime} -> endTime) (\s@MaintenanceWindowExecution' {} a -> s {endTime = a} :: MaintenanceWindowExecution) Prelude.. Lens.mapping Core._Time

-- | The ID of the maintenance window.
maintenanceWindowExecution_windowId :: Lens.Lens' MaintenanceWindowExecution (Prelude.Maybe Prelude.Text)
maintenanceWindowExecution_windowId = Lens.lens (\MaintenanceWindowExecution' {windowId} -> windowId) (\s@MaintenanceWindowExecution' {} a -> s {windowId = a} :: MaintenanceWindowExecution)

instance Core.FromJSON MaintenanceWindowExecution where
  parseJSON =
    Core.withObject
      "MaintenanceWindowExecution"
      ( \x ->
          MaintenanceWindowExecution'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "StartTime")
            Prelude.<*> (x Core..:? "WindowExecutionId")
            Prelude.<*> (x Core..:? "StatusDetails")
            Prelude.<*> (x Core..:? "EndTime")
            Prelude.<*> (x Core..:? "WindowId")
      )

instance Prelude.Hashable MaintenanceWindowExecution where
  hashWithSalt _salt MaintenanceWindowExecution' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` windowExecutionId
      `Prelude.hashWithSalt` statusDetails
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` windowId

instance Prelude.NFData MaintenanceWindowExecution where
  rnf MaintenanceWindowExecution' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf windowExecutionId
      `Prelude.seq` Prelude.rnf statusDetails
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf windowId
