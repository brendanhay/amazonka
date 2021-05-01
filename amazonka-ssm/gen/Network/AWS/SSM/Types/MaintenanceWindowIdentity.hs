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
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowIdentity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.MaintenanceWindowIdentity where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the maintenance window.
--
-- /See:/ 'newMaintenanceWindowIdentity' smart constructor.
data MaintenanceWindowIdentity = MaintenanceWindowIdentity'
  { -- | The date and time, in ISO-8601 Extended format, for when the maintenance
    -- window is scheduled to become active.
    startDate :: Prelude.Maybe Prelude.Text,
    -- | The duration of the maintenance window in hours.
    duration :: Prelude.Maybe Prelude.Natural,
    -- | The number of days to wait to run a maintenance window after the
    -- scheduled CRON expression date and time.
    scheduleOffset :: Prelude.Maybe Prelude.Natural,
    -- | Indicates whether the maintenance window is enabled.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The number of hours before the end of the maintenance window that
    -- Systems Manager stops scheduling new tasks for execution.
    cutoff :: Prelude.Maybe Prelude.Natural,
    -- | The name of the maintenance window.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of the maintenance window.
    windowId :: Prelude.Maybe Prelude.Text,
    -- | A description of the maintenance window.
    description :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The time zone that the scheduled maintenance window executions are based
    -- on, in Internet Assigned Numbers Authority (IANA) format.
    scheduleTimezone :: Prelude.Maybe Prelude.Text,
    -- | The date and time, in ISO-8601 Extended format, for when the maintenance
    -- window is scheduled to become inactive.
    endDate :: Prelude.Maybe Prelude.Text,
    -- | The next time the maintenance window will actually run, taking into
    -- account any specified times for the maintenance window to become active
    -- or inactive.
    nextExecutionTime :: Prelude.Maybe Prelude.Text,
    -- | The schedule of the maintenance window in the form of a cron or rate
    -- expression.
    schedule :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MaintenanceWindowIdentity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startDate', 'maintenanceWindowIdentity_startDate' - The date and time, in ISO-8601 Extended format, for when the maintenance
-- window is scheduled to become active.
--
-- 'duration', 'maintenanceWindowIdentity_duration' - The duration of the maintenance window in hours.
--
-- 'scheduleOffset', 'maintenanceWindowIdentity_scheduleOffset' - The number of days to wait to run a maintenance window after the
-- scheduled CRON expression date and time.
--
-- 'enabled', 'maintenanceWindowIdentity_enabled' - Indicates whether the maintenance window is enabled.
--
-- 'cutoff', 'maintenanceWindowIdentity_cutoff' - The number of hours before the end of the maintenance window that
-- Systems Manager stops scheduling new tasks for execution.
--
-- 'name', 'maintenanceWindowIdentity_name' - The name of the maintenance window.
--
-- 'windowId', 'maintenanceWindowIdentity_windowId' - The ID of the maintenance window.
--
-- 'description', 'maintenanceWindowIdentity_description' - A description of the maintenance window.
--
-- 'scheduleTimezone', 'maintenanceWindowIdentity_scheduleTimezone' - The time zone that the scheduled maintenance window executions are based
-- on, in Internet Assigned Numbers Authority (IANA) format.
--
-- 'endDate', 'maintenanceWindowIdentity_endDate' - The date and time, in ISO-8601 Extended format, for when the maintenance
-- window is scheduled to become inactive.
--
-- 'nextExecutionTime', 'maintenanceWindowIdentity_nextExecutionTime' - The next time the maintenance window will actually run, taking into
-- account any specified times for the maintenance window to become active
-- or inactive.
--
-- 'schedule', 'maintenanceWindowIdentity_schedule' - The schedule of the maintenance window in the form of a cron or rate
-- expression.
newMaintenanceWindowIdentity ::
  MaintenanceWindowIdentity
newMaintenanceWindowIdentity =
  MaintenanceWindowIdentity'
    { startDate =
        Prelude.Nothing,
      duration = Prelude.Nothing,
      scheduleOffset = Prelude.Nothing,
      enabled = Prelude.Nothing,
      cutoff = Prelude.Nothing,
      name = Prelude.Nothing,
      windowId = Prelude.Nothing,
      description = Prelude.Nothing,
      scheduleTimezone = Prelude.Nothing,
      endDate = Prelude.Nothing,
      nextExecutionTime = Prelude.Nothing,
      schedule = Prelude.Nothing
    }

-- | The date and time, in ISO-8601 Extended format, for when the maintenance
-- window is scheduled to become active.
maintenanceWindowIdentity_startDate :: Lens.Lens' MaintenanceWindowIdentity (Prelude.Maybe Prelude.Text)
maintenanceWindowIdentity_startDate = Lens.lens (\MaintenanceWindowIdentity' {startDate} -> startDate) (\s@MaintenanceWindowIdentity' {} a -> s {startDate = a} :: MaintenanceWindowIdentity)

-- | The duration of the maintenance window in hours.
maintenanceWindowIdentity_duration :: Lens.Lens' MaintenanceWindowIdentity (Prelude.Maybe Prelude.Natural)
maintenanceWindowIdentity_duration = Lens.lens (\MaintenanceWindowIdentity' {duration} -> duration) (\s@MaintenanceWindowIdentity' {} a -> s {duration = a} :: MaintenanceWindowIdentity)

-- | The number of days to wait to run a maintenance window after the
-- scheduled CRON expression date and time.
maintenanceWindowIdentity_scheduleOffset :: Lens.Lens' MaintenanceWindowIdentity (Prelude.Maybe Prelude.Natural)
maintenanceWindowIdentity_scheduleOffset = Lens.lens (\MaintenanceWindowIdentity' {scheduleOffset} -> scheduleOffset) (\s@MaintenanceWindowIdentity' {} a -> s {scheduleOffset = a} :: MaintenanceWindowIdentity)

-- | Indicates whether the maintenance window is enabled.
maintenanceWindowIdentity_enabled :: Lens.Lens' MaintenanceWindowIdentity (Prelude.Maybe Prelude.Bool)
maintenanceWindowIdentity_enabled = Lens.lens (\MaintenanceWindowIdentity' {enabled} -> enabled) (\s@MaintenanceWindowIdentity' {} a -> s {enabled = a} :: MaintenanceWindowIdentity)

-- | The number of hours before the end of the maintenance window that
-- Systems Manager stops scheduling new tasks for execution.
maintenanceWindowIdentity_cutoff :: Lens.Lens' MaintenanceWindowIdentity (Prelude.Maybe Prelude.Natural)
maintenanceWindowIdentity_cutoff = Lens.lens (\MaintenanceWindowIdentity' {cutoff} -> cutoff) (\s@MaintenanceWindowIdentity' {} a -> s {cutoff = a} :: MaintenanceWindowIdentity)

-- | The name of the maintenance window.
maintenanceWindowIdentity_name :: Lens.Lens' MaintenanceWindowIdentity (Prelude.Maybe Prelude.Text)
maintenanceWindowIdentity_name = Lens.lens (\MaintenanceWindowIdentity' {name} -> name) (\s@MaintenanceWindowIdentity' {} a -> s {name = a} :: MaintenanceWindowIdentity)

-- | The ID of the maintenance window.
maintenanceWindowIdentity_windowId :: Lens.Lens' MaintenanceWindowIdentity (Prelude.Maybe Prelude.Text)
maintenanceWindowIdentity_windowId = Lens.lens (\MaintenanceWindowIdentity' {windowId} -> windowId) (\s@MaintenanceWindowIdentity' {} a -> s {windowId = a} :: MaintenanceWindowIdentity)

-- | A description of the maintenance window.
maintenanceWindowIdentity_description :: Lens.Lens' MaintenanceWindowIdentity (Prelude.Maybe Prelude.Text)
maintenanceWindowIdentity_description = Lens.lens (\MaintenanceWindowIdentity' {description} -> description) (\s@MaintenanceWindowIdentity' {} a -> s {description = a} :: MaintenanceWindowIdentity) Prelude.. Lens.mapping Prelude._Sensitive

-- | The time zone that the scheduled maintenance window executions are based
-- on, in Internet Assigned Numbers Authority (IANA) format.
maintenanceWindowIdentity_scheduleTimezone :: Lens.Lens' MaintenanceWindowIdentity (Prelude.Maybe Prelude.Text)
maintenanceWindowIdentity_scheduleTimezone = Lens.lens (\MaintenanceWindowIdentity' {scheduleTimezone} -> scheduleTimezone) (\s@MaintenanceWindowIdentity' {} a -> s {scheduleTimezone = a} :: MaintenanceWindowIdentity)

-- | The date and time, in ISO-8601 Extended format, for when the maintenance
-- window is scheduled to become inactive.
maintenanceWindowIdentity_endDate :: Lens.Lens' MaintenanceWindowIdentity (Prelude.Maybe Prelude.Text)
maintenanceWindowIdentity_endDate = Lens.lens (\MaintenanceWindowIdentity' {endDate} -> endDate) (\s@MaintenanceWindowIdentity' {} a -> s {endDate = a} :: MaintenanceWindowIdentity)

-- | The next time the maintenance window will actually run, taking into
-- account any specified times for the maintenance window to become active
-- or inactive.
maintenanceWindowIdentity_nextExecutionTime :: Lens.Lens' MaintenanceWindowIdentity (Prelude.Maybe Prelude.Text)
maintenanceWindowIdentity_nextExecutionTime = Lens.lens (\MaintenanceWindowIdentity' {nextExecutionTime} -> nextExecutionTime) (\s@MaintenanceWindowIdentity' {} a -> s {nextExecutionTime = a} :: MaintenanceWindowIdentity)

-- | The schedule of the maintenance window in the form of a cron or rate
-- expression.
maintenanceWindowIdentity_schedule :: Lens.Lens' MaintenanceWindowIdentity (Prelude.Maybe Prelude.Text)
maintenanceWindowIdentity_schedule = Lens.lens (\MaintenanceWindowIdentity' {schedule} -> schedule) (\s@MaintenanceWindowIdentity' {} a -> s {schedule = a} :: MaintenanceWindowIdentity)

instance Prelude.FromJSON MaintenanceWindowIdentity where
  parseJSON =
    Prelude.withObject
      "MaintenanceWindowIdentity"
      ( \x ->
          MaintenanceWindowIdentity'
            Prelude.<$> (x Prelude..:? "StartDate")
            Prelude.<*> (x Prelude..:? "Duration")
            Prelude.<*> (x Prelude..:? "ScheduleOffset")
            Prelude.<*> (x Prelude..:? "Enabled")
            Prelude.<*> (x Prelude..:? "Cutoff")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "WindowId")
            Prelude.<*> (x Prelude..:? "Description")
            Prelude.<*> (x Prelude..:? "ScheduleTimezone")
            Prelude.<*> (x Prelude..:? "EndDate")
            Prelude.<*> (x Prelude..:? "NextExecutionTime")
            Prelude.<*> (x Prelude..:? "Schedule")
      )

instance Prelude.Hashable MaintenanceWindowIdentity

instance Prelude.NFData MaintenanceWindowIdentity
