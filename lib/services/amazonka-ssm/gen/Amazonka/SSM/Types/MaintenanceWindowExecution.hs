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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.MaintenanceWindowExecution where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.MaintenanceWindowExecutionStatus

-- | Describes the information about an execution of a maintenance window.
--
-- /See:/ 'newMaintenanceWindowExecution' smart constructor.
data MaintenanceWindowExecution = MaintenanceWindowExecution'
  { -- | The time the execution finished.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The time the execution started.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The status of the execution.
    status :: Prelude.Maybe MaintenanceWindowExecutionStatus,
    -- | The details explaining the status. Not available for all status values.
    statusDetails :: Prelude.Maybe Prelude.Text,
    -- | The ID of the maintenance window execution.
    windowExecutionId :: Prelude.Maybe Prelude.Text,
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
-- 'endTime', 'maintenanceWindowExecution_endTime' - The time the execution finished.
--
-- 'startTime', 'maintenanceWindowExecution_startTime' - The time the execution started.
--
-- 'status', 'maintenanceWindowExecution_status' - The status of the execution.
--
-- 'statusDetails', 'maintenanceWindowExecution_statusDetails' - The details explaining the status. Not available for all status values.
--
-- 'windowExecutionId', 'maintenanceWindowExecution_windowExecutionId' - The ID of the maintenance window execution.
--
-- 'windowId', 'maintenanceWindowExecution_windowId' - The ID of the maintenance window.
newMaintenanceWindowExecution ::
  MaintenanceWindowExecution
newMaintenanceWindowExecution =
  MaintenanceWindowExecution'
    { endTime =
        Prelude.Nothing,
      startTime = Prelude.Nothing,
      status = Prelude.Nothing,
      statusDetails = Prelude.Nothing,
      windowExecutionId = Prelude.Nothing,
      windowId = Prelude.Nothing
    }

-- | The time the execution finished.
maintenanceWindowExecution_endTime :: Lens.Lens' MaintenanceWindowExecution (Prelude.Maybe Prelude.UTCTime)
maintenanceWindowExecution_endTime = Lens.lens (\MaintenanceWindowExecution' {endTime} -> endTime) (\s@MaintenanceWindowExecution' {} a -> s {endTime = a} :: MaintenanceWindowExecution) Prelude.. Lens.mapping Data._Time

-- | The time the execution started.
maintenanceWindowExecution_startTime :: Lens.Lens' MaintenanceWindowExecution (Prelude.Maybe Prelude.UTCTime)
maintenanceWindowExecution_startTime = Lens.lens (\MaintenanceWindowExecution' {startTime} -> startTime) (\s@MaintenanceWindowExecution' {} a -> s {startTime = a} :: MaintenanceWindowExecution) Prelude.. Lens.mapping Data._Time

-- | The status of the execution.
maintenanceWindowExecution_status :: Lens.Lens' MaintenanceWindowExecution (Prelude.Maybe MaintenanceWindowExecutionStatus)
maintenanceWindowExecution_status = Lens.lens (\MaintenanceWindowExecution' {status} -> status) (\s@MaintenanceWindowExecution' {} a -> s {status = a} :: MaintenanceWindowExecution)

-- | The details explaining the status. Not available for all status values.
maintenanceWindowExecution_statusDetails :: Lens.Lens' MaintenanceWindowExecution (Prelude.Maybe Prelude.Text)
maintenanceWindowExecution_statusDetails = Lens.lens (\MaintenanceWindowExecution' {statusDetails} -> statusDetails) (\s@MaintenanceWindowExecution' {} a -> s {statusDetails = a} :: MaintenanceWindowExecution)

-- | The ID of the maintenance window execution.
maintenanceWindowExecution_windowExecutionId :: Lens.Lens' MaintenanceWindowExecution (Prelude.Maybe Prelude.Text)
maintenanceWindowExecution_windowExecutionId = Lens.lens (\MaintenanceWindowExecution' {windowExecutionId} -> windowExecutionId) (\s@MaintenanceWindowExecution' {} a -> s {windowExecutionId = a} :: MaintenanceWindowExecution)

-- | The ID of the maintenance window.
maintenanceWindowExecution_windowId :: Lens.Lens' MaintenanceWindowExecution (Prelude.Maybe Prelude.Text)
maintenanceWindowExecution_windowId = Lens.lens (\MaintenanceWindowExecution' {windowId} -> windowId) (\s@MaintenanceWindowExecution' {} a -> s {windowId = a} :: MaintenanceWindowExecution)

instance Data.FromJSON MaintenanceWindowExecution where
  parseJSON =
    Data.withObject
      "MaintenanceWindowExecution"
      ( \x ->
          MaintenanceWindowExecution'
            Prelude.<$> (x Data..:? "EndTime")
            Prelude.<*> (x Data..:? "StartTime")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "StatusDetails")
            Prelude.<*> (x Data..:? "WindowExecutionId")
            Prelude.<*> (x Data..:? "WindowId")
      )

instance Prelude.Hashable MaintenanceWindowExecution where
  hashWithSalt _salt MaintenanceWindowExecution' {..} =
    _salt `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusDetails
      `Prelude.hashWithSalt` windowExecutionId
      `Prelude.hashWithSalt` windowId

instance Prelude.NFData MaintenanceWindowExecution where
  rnf MaintenanceWindowExecution' {..} =
    Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusDetails
      `Prelude.seq` Prelude.rnf windowExecutionId
      `Prelude.seq` Prelude.rnf windowId
