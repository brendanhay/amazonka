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
-- Module      : Amazonka.SSM.Types.MaintenanceWindowIdentity
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.MaintenanceWindowIdentity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the maintenance window.
--
-- /See:/ 'newMaintenanceWindowIdentity' smart constructor.
data MaintenanceWindowIdentity = MaintenanceWindowIdentity'
  { -- | The schedule of the maintenance window in the form of a cron or rate
    -- expression.
    schedule :: Prelude.Maybe Prelude.Text,
    -- | The number of hours before the end of the maintenance window that Amazon
    -- Web Services Systems Manager stops scheduling new tasks for execution.
    cutoff :: Prelude.Maybe Prelude.Natural,
    -- | The name of the maintenance window.
    name :: Prelude.Maybe Prelude.Text,
    -- | The date and time, in ISO-8601 Extended format, for when the maintenance
    -- window is scheduled to become inactive.
    endDate :: Prelude.Maybe Prelude.Text,
    -- | The ID of the maintenance window.
    windowId :: Prelude.Maybe Prelude.Text,
    -- | A description of the maintenance window.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Indicates whether the maintenance window is enabled.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The duration of the maintenance window in hours.
    duration :: Prelude.Maybe Prelude.Natural,
    -- | The time zone that the scheduled maintenance window executions are based
    -- on, in Internet Assigned Numbers Authority (IANA) format.
    scheduleTimezone :: Prelude.Maybe Prelude.Text,
    -- | The number of days to wait to run a maintenance window after the
    -- scheduled cron expression date and time.
    scheduleOffset :: Prelude.Maybe Prelude.Natural,
    -- | The date and time, in ISO-8601 Extended format, for when the maintenance
    -- window is scheduled to become active.
    startDate :: Prelude.Maybe Prelude.Text,
    -- | The next time the maintenance window will actually run, taking into
    -- account any specified times for the maintenance window to become active
    -- or inactive.
    nextExecutionTime :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MaintenanceWindowIdentity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schedule', 'maintenanceWindowIdentity_schedule' - The schedule of the maintenance window in the form of a cron or rate
-- expression.
--
-- 'cutoff', 'maintenanceWindowIdentity_cutoff' - The number of hours before the end of the maintenance window that Amazon
-- Web Services Systems Manager stops scheduling new tasks for execution.
--
-- 'name', 'maintenanceWindowIdentity_name' - The name of the maintenance window.
--
-- 'endDate', 'maintenanceWindowIdentity_endDate' - The date and time, in ISO-8601 Extended format, for when the maintenance
-- window is scheduled to become inactive.
--
-- 'windowId', 'maintenanceWindowIdentity_windowId' - The ID of the maintenance window.
--
-- 'description', 'maintenanceWindowIdentity_description' - A description of the maintenance window.
--
-- 'enabled', 'maintenanceWindowIdentity_enabled' - Indicates whether the maintenance window is enabled.
--
-- 'duration', 'maintenanceWindowIdentity_duration' - The duration of the maintenance window in hours.
--
-- 'scheduleTimezone', 'maintenanceWindowIdentity_scheduleTimezone' - The time zone that the scheduled maintenance window executions are based
-- on, in Internet Assigned Numbers Authority (IANA) format.
--
-- 'scheduleOffset', 'maintenanceWindowIdentity_scheduleOffset' - The number of days to wait to run a maintenance window after the
-- scheduled cron expression date and time.
--
-- 'startDate', 'maintenanceWindowIdentity_startDate' - The date and time, in ISO-8601 Extended format, for when the maintenance
-- window is scheduled to become active.
--
-- 'nextExecutionTime', 'maintenanceWindowIdentity_nextExecutionTime' - The next time the maintenance window will actually run, taking into
-- account any specified times for the maintenance window to become active
-- or inactive.
newMaintenanceWindowIdentity ::
  MaintenanceWindowIdentity
newMaintenanceWindowIdentity =
  MaintenanceWindowIdentity'
    { schedule =
        Prelude.Nothing,
      cutoff = Prelude.Nothing,
      name = Prelude.Nothing,
      endDate = Prelude.Nothing,
      windowId = Prelude.Nothing,
      description = Prelude.Nothing,
      enabled = Prelude.Nothing,
      duration = Prelude.Nothing,
      scheduleTimezone = Prelude.Nothing,
      scheduleOffset = Prelude.Nothing,
      startDate = Prelude.Nothing,
      nextExecutionTime = Prelude.Nothing
    }

-- | The schedule of the maintenance window in the form of a cron or rate
-- expression.
maintenanceWindowIdentity_schedule :: Lens.Lens' MaintenanceWindowIdentity (Prelude.Maybe Prelude.Text)
maintenanceWindowIdentity_schedule = Lens.lens (\MaintenanceWindowIdentity' {schedule} -> schedule) (\s@MaintenanceWindowIdentity' {} a -> s {schedule = a} :: MaintenanceWindowIdentity)

-- | The number of hours before the end of the maintenance window that Amazon
-- Web Services Systems Manager stops scheduling new tasks for execution.
maintenanceWindowIdentity_cutoff :: Lens.Lens' MaintenanceWindowIdentity (Prelude.Maybe Prelude.Natural)
maintenanceWindowIdentity_cutoff = Lens.lens (\MaintenanceWindowIdentity' {cutoff} -> cutoff) (\s@MaintenanceWindowIdentity' {} a -> s {cutoff = a} :: MaintenanceWindowIdentity)

-- | The name of the maintenance window.
maintenanceWindowIdentity_name :: Lens.Lens' MaintenanceWindowIdentity (Prelude.Maybe Prelude.Text)
maintenanceWindowIdentity_name = Lens.lens (\MaintenanceWindowIdentity' {name} -> name) (\s@MaintenanceWindowIdentity' {} a -> s {name = a} :: MaintenanceWindowIdentity)

-- | The date and time, in ISO-8601 Extended format, for when the maintenance
-- window is scheduled to become inactive.
maintenanceWindowIdentity_endDate :: Lens.Lens' MaintenanceWindowIdentity (Prelude.Maybe Prelude.Text)
maintenanceWindowIdentity_endDate = Lens.lens (\MaintenanceWindowIdentity' {endDate} -> endDate) (\s@MaintenanceWindowIdentity' {} a -> s {endDate = a} :: MaintenanceWindowIdentity)

-- | The ID of the maintenance window.
maintenanceWindowIdentity_windowId :: Lens.Lens' MaintenanceWindowIdentity (Prelude.Maybe Prelude.Text)
maintenanceWindowIdentity_windowId = Lens.lens (\MaintenanceWindowIdentity' {windowId} -> windowId) (\s@MaintenanceWindowIdentity' {} a -> s {windowId = a} :: MaintenanceWindowIdentity)

-- | A description of the maintenance window.
maintenanceWindowIdentity_description :: Lens.Lens' MaintenanceWindowIdentity (Prelude.Maybe Prelude.Text)
maintenanceWindowIdentity_description = Lens.lens (\MaintenanceWindowIdentity' {description} -> description) (\s@MaintenanceWindowIdentity' {} a -> s {description = a} :: MaintenanceWindowIdentity) Prelude.. Lens.mapping Data._Sensitive

-- | Indicates whether the maintenance window is enabled.
maintenanceWindowIdentity_enabled :: Lens.Lens' MaintenanceWindowIdentity (Prelude.Maybe Prelude.Bool)
maintenanceWindowIdentity_enabled = Lens.lens (\MaintenanceWindowIdentity' {enabled} -> enabled) (\s@MaintenanceWindowIdentity' {} a -> s {enabled = a} :: MaintenanceWindowIdentity)

-- | The duration of the maintenance window in hours.
maintenanceWindowIdentity_duration :: Lens.Lens' MaintenanceWindowIdentity (Prelude.Maybe Prelude.Natural)
maintenanceWindowIdentity_duration = Lens.lens (\MaintenanceWindowIdentity' {duration} -> duration) (\s@MaintenanceWindowIdentity' {} a -> s {duration = a} :: MaintenanceWindowIdentity)

-- | The time zone that the scheduled maintenance window executions are based
-- on, in Internet Assigned Numbers Authority (IANA) format.
maintenanceWindowIdentity_scheduleTimezone :: Lens.Lens' MaintenanceWindowIdentity (Prelude.Maybe Prelude.Text)
maintenanceWindowIdentity_scheduleTimezone = Lens.lens (\MaintenanceWindowIdentity' {scheduleTimezone} -> scheduleTimezone) (\s@MaintenanceWindowIdentity' {} a -> s {scheduleTimezone = a} :: MaintenanceWindowIdentity)

-- | The number of days to wait to run a maintenance window after the
-- scheduled cron expression date and time.
maintenanceWindowIdentity_scheduleOffset :: Lens.Lens' MaintenanceWindowIdentity (Prelude.Maybe Prelude.Natural)
maintenanceWindowIdentity_scheduleOffset = Lens.lens (\MaintenanceWindowIdentity' {scheduleOffset} -> scheduleOffset) (\s@MaintenanceWindowIdentity' {} a -> s {scheduleOffset = a} :: MaintenanceWindowIdentity)

-- | The date and time, in ISO-8601 Extended format, for when the maintenance
-- window is scheduled to become active.
maintenanceWindowIdentity_startDate :: Lens.Lens' MaintenanceWindowIdentity (Prelude.Maybe Prelude.Text)
maintenanceWindowIdentity_startDate = Lens.lens (\MaintenanceWindowIdentity' {startDate} -> startDate) (\s@MaintenanceWindowIdentity' {} a -> s {startDate = a} :: MaintenanceWindowIdentity)

-- | The next time the maintenance window will actually run, taking into
-- account any specified times for the maintenance window to become active
-- or inactive.
maintenanceWindowIdentity_nextExecutionTime :: Lens.Lens' MaintenanceWindowIdentity (Prelude.Maybe Prelude.Text)
maintenanceWindowIdentity_nextExecutionTime = Lens.lens (\MaintenanceWindowIdentity' {nextExecutionTime} -> nextExecutionTime) (\s@MaintenanceWindowIdentity' {} a -> s {nextExecutionTime = a} :: MaintenanceWindowIdentity)

instance Data.FromJSON MaintenanceWindowIdentity where
  parseJSON =
    Data.withObject
      "MaintenanceWindowIdentity"
      ( \x ->
          MaintenanceWindowIdentity'
            Prelude.<$> (x Data..:? "Schedule")
            Prelude.<*> (x Data..:? "Cutoff")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "EndDate")
            Prelude.<*> (x Data..:? "WindowId")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Enabled")
            Prelude.<*> (x Data..:? "Duration")
            Prelude.<*> (x Data..:? "ScheduleTimezone")
            Prelude.<*> (x Data..:? "ScheduleOffset")
            Prelude.<*> (x Data..:? "StartDate")
            Prelude.<*> (x Data..:? "NextExecutionTime")
      )

instance Prelude.Hashable MaintenanceWindowIdentity where
  hashWithSalt _salt MaintenanceWindowIdentity' {..} =
    _salt `Prelude.hashWithSalt` schedule
      `Prelude.hashWithSalt` cutoff
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` endDate
      `Prelude.hashWithSalt` windowId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` duration
      `Prelude.hashWithSalt` scheduleTimezone
      `Prelude.hashWithSalt` scheduleOffset
      `Prelude.hashWithSalt` startDate
      `Prelude.hashWithSalt` nextExecutionTime

instance Prelude.NFData MaintenanceWindowIdentity where
  rnf MaintenanceWindowIdentity' {..} =
    Prelude.rnf schedule
      `Prelude.seq` Prelude.rnf cutoff
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf endDate
      `Prelude.seq` Prelude.rnf windowId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf duration
      `Prelude.seq` Prelude.rnf scheduleTimezone
      `Prelude.seq` Prelude.rnf scheduleOffset
      `Prelude.seq` Prelude.rnf startDate
      `Prelude.seq` Prelude.rnf nextExecutionTime
