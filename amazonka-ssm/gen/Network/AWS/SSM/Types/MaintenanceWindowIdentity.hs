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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about the maintenance window.
--
-- /See:/ 'newMaintenanceWindowIdentity' smart constructor.
data MaintenanceWindowIdentity = MaintenanceWindowIdentity'
  { -- | The date and time, in ISO-8601 Extended format, for when the maintenance
    -- window is scheduled to become active.
    startDate :: Core.Maybe Core.Text,
    -- | The duration of the maintenance window in hours.
    duration :: Core.Maybe Core.Natural,
    -- | The number of days to wait to run a maintenance window after the
    -- scheduled CRON expression date and time.
    scheduleOffset :: Core.Maybe Core.Natural,
    -- | Indicates whether the maintenance window is enabled.
    enabled :: Core.Maybe Core.Bool,
    -- | The number of hours before the end of the maintenance window that
    -- Systems Manager stops scheduling new tasks for execution.
    cutoff :: Core.Maybe Core.Natural,
    -- | The name of the maintenance window.
    name :: Core.Maybe Core.Text,
    -- | The ID of the maintenance window.
    windowId :: Core.Maybe Core.Text,
    -- | A description of the maintenance window.
    description :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The time zone that the scheduled maintenance window executions are based
    -- on, in Internet Assigned Numbers Authority (IANA) format.
    scheduleTimezone :: Core.Maybe Core.Text,
    -- | The date and time, in ISO-8601 Extended format, for when the maintenance
    -- window is scheduled to become inactive.
    endDate :: Core.Maybe Core.Text,
    -- | The next time the maintenance window will actually run, taking into
    -- account any specified times for the maintenance window to become active
    -- or inactive.
    nextExecutionTime :: Core.Maybe Core.Text,
    -- | The schedule of the maintenance window in the form of a cron or rate
    -- expression.
    schedule :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
        Core.Nothing,
      duration = Core.Nothing,
      scheduleOffset = Core.Nothing,
      enabled = Core.Nothing,
      cutoff = Core.Nothing,
      name = Core.Nothing,
      windowId = Core.Nothing,
      description = Core.Nothing,
      scheduleTimezone = Core.Nothing,
      endDate = Core.Nothing,
      nextExecutionTime = Core.Nothing,
      schedule = Core.Nothing
    }

-- | The date and time, in ISO-8601 Extended format, for when the maintenance
-- window is scheduled to become active.
maintenanceWindowIdentity_startDate :: Lens.Lens' MaintenanceWindowIdentity (Core.Maybe Core.Text)
maintenanceWindowIdentity_startDate = Lens.lens (\MaintenanceWindowIdentity' {startDate} -> startDate) (\s@MaintenanceWindowIdentity' {} a -> s {startDate = a} :: MaintenanceWindowIdentity)

-- | The duration of the maintenance window in hours.
maintenanceWindowIdentity_duration :: Lens.Lens' MaintenanceWindowIdentity (Core.Maybe Core.Natural)
maintenanceWindowIdentity_duration = Lens.lens (\MaintenanceWindowIdentity' {duration} -> duration) (\s@MaintenanceWindowIdentity' {} a -> s {duration = a} :: MaintenanceWindowIdentity)

-- | The number of days to wait to run a maintenance window after the
-- scheduled CRON expression date and time.
maintenanceWindowIdentity_scheduleOffset :: Lens.Lens' MaintenanceWindowIdentity (Core.Maybe Core.Natural)
maintenanceWindowIdentity_scheduleOffset = Lens.lens (\MaintenanceWindowIdentity' {scheduleOffset} -> scheduleOffset) (\s@MaintenanceWindowIdentity' {} a -> s {scheduleOffset = a} :: MaintenanceWindowIdentity)

-- | Indicates whether the maintenance window is enabled.
maintenanceWindowIdentity_enabled :: Lens.Lens' MaintenanceWindowIdentity (Core.Maybe Core.Bool)
maintenanceWindowIdentity_enabled = Lens.lens (\MaintenanceWindowIdentity' {enabled} -> enabled) (\s@MaintenanceWindowIdentity' {} a -> s {enabled = a} :: MaintenanceWindowIdentity)

-- | The number of hours before the end of the maintenance window that
-- Systems Manager stops scheduling new tasks for execution.
maintenanceWindowIdentity_cutoff :: Lens.Lens' MaintenanceWindowIdentity (Core.Maybe Core.Natural)
maintenanceWindowIdentity_cutoff = Lens.lens (\MaintenanceWindowIdentity' {cutoff} -> cutoff) (\s@MaintenanceWindowIdentity' {} a -> s {cutoff = a} :: MaintenanceWindowIdentity)

-- | The name of the maintenance window.
maintenanceWindowIdentity_name :: Lens.Lens' MaintenanceWindowIdentity (Core.Maybe Core.Text)
maintenanceWindowIdentity_name = Lens.lens (\MaintenanceWindowIdentity' {name} -> name) (\s@MaintenanceWindowIdentity' {} a -> s {name = a} :: MaintenanceWindowIdentity)

-- | The ID of the maintenance window.
maintenanceWindowIdentity_windowId :: Lens.Lens' MaintenanceWindowIdentity (Core.Maybe Core.Text)
maintenanceWindowIdentity_windowId = Lens.lens (\MaintenanceWindowIdentity' {windowId} -> windowId) (\s@MaintenanceWindowIdentity' {} a -> s {windowId = a} :: MaintenanceWindowIdentity)

-- | A description of the maintenance window.
maintenanceWindowIdentity_description :: Lens.Lens' MaintenanceWindowIdentity (Core.Maybe Core.Text)
maintenanceWindowIdentity_description = Lens.lens (\MaintenanceWindowIdentity' {description} -> description) (\s@MaintenanceWindowIdentity' {} a -> s {description = a} :: MaintenanceWindowIdentity) Core.. Lens.mapping Core._Sensitive

-- | The time zone that the scheduled maintenance window executions are based
-- on, in Internet Assigned Numbers Authority (IANA) format.
maintenanceWindowIdentity_scheduleTimezone :: Lens.Lens' MaintenanceWindowIdentity (Core.Maybe Core.Text)
maintenanceWindowIdentity_scheduleTimezone = Lens.lens (\MaintenanceWindowIdentity' {scheduleTimezone} -> scheduleTimezone) (\s@MaintenanceWindowIdentity' {} a -> s {scheduleTimezone = a} :: MaintenanceWindowIdentity)

-- | The date and time, in ISO-8601 Extended format, for when the maintenance
-- window is scheduled to become inactive.
maintenanceWindowIdentity_endDate :: Lens.Lens' MaintenanceWindowIdentity (Core.Maybe Core.Text)
maintenanceWindowIdentity_endDate = Lens.lens (\MaintenanceWindowIdentity' {endDate} -> endDate) (\s@MaintenanceWindowIdentity' {} a -> s {endDate = a} :: MaintenanceWindowIdentity)

-- | The next time the maintenance window will actually run, taking into
-- account any specified times for the maintenance window to become active
-- or inactive.
maintenanceWindowIdentity_nextExecutionTime :: Lens.Lens' MaintenanceWindowIdentity (Core.Maybe Core.Text)
maintenanceWindowIdentity_nextExecutionTime = Lens.lens (\MaintenanceWindowIdentity' {nextExecutionTime} -> nextExecutionTime) (\s@MaintenanceWindowIdentity' {} a -> s {nextExecutionTime = a} :: MaintenanceWindowIdentity)

-- | The schedule of the maintenance window in the form of a cron or rate
-- expression.
maintenanceWindowIdentity_schedule :: Lens.Lens' MaintenanceWindowIdentity (Core.Maybe Core.Text)
maintenanceWindowIdentity_schedule = Lens.lens (\MaintenanceWindowIdentity' {schedule} -> schedule) (\s@MaintenanceWindowIdentity' {} a -> s {schedule = a} :: MaintenanceWindowIdentity)

instance Core.FromJSON MaintenanceWindowIdentity where
  parseJSON =
    Core.withObject
      "MaintenanceWindowIdentity"
      ( \x ->
          MaintenanceWindowIdentity'
            Core.<$> (x Core..:? "StartDate")
            Core.<*> (x Core..:? "Duration")
            Core.<*> (x Core..:? "ScheduleOffset")
            Core.<*> (x Core..:? "Enabled")
            Core.<*> (x Core..:? "Cutoff")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "WindowId")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "ScheduleTimezone")
            Core.<*> (x Core..:? "EndDate")
            Core.<*> (x Core..:? "NextExecutionTime")
            Core.<*> (x Core..:? "Schedule")
      )

instance Core.Hashable MaintenanceWindowIdentity

instance Core.NFData MaintenanceWindowIdentity
