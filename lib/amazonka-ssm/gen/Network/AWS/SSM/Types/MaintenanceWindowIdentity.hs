{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowIdentity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.MaintenanceWindowIdentity
  ( MaintenanceWindowIdentity (..),

    -- * Smart constructor
    mkMaintenanceWindowIdentity,

    -- * Lenses
    mwiCutoff,
    mwiDescription,
    mwiDuration,
    mwiEnabled,
    mwiEndDate,
    mwiName,
    mwiNextExecutionTime,
    mwiSchedule,
    mwiScheduleOffset,
    mwiScheduleTimezone,
    mwiStartDate,
    mwiWindowId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.Description as Types
import qualified Network.AWS.SSM.Types.EndDate as Types
import qualified Network.AWS.SSM.Types.MaintenanceWindowSchedule as Types
import qualified Network.AWS.SSM.Types.Name as Types
import qualified Network.AWS.SSM.Types.NextExecutionTime as Types
import qualified Network.AWS.SSM.Types.ScheduleTimezone as Types
import qualified Network.AWS.SSM.Types.StartDate as Types
import qualified Network.AWS.SSM.Types.WindowId as Types

-- | Information about the maintenance window.
--
-- /See:/ 'mkMaintenanceWindowIdentity' smart constructor.
data MaintenanceWindowIdentity = MaintenanceWindowIdentity'
  { -- | The number of hours before the end of the maintenance window that Systems Manager stops scheduling new tasks for execution.
    cutoff :: Core.Maybe Core.Natural,
    -- | A description of the maintenance window.
    description :: Core.Maybe Types.Description,
    -- | The duration of the maintenance window in hours.
    duration :: Core.Maybe Core.Natural,
    -- | Indicates whether the maintenance window is enabled.
    enabled :: Core.Maybe Core.Bool,
    -- | The date and time, in ISO-8601 Extended format, for when the maintenance window is scheduled to become inactive.
    endDate :: Core.Maybe Types.EndDate,
    -- | The name of the maintenance window.
    name :: Core.Maybe Types.Name,
    -- | The next time the maintenance window will actually run, taking into account any specified times for the maintenance window to become active or inactive.
    nextExecutionTime :: Core.Maybe Types.NextExecutionTime,
    -- | The schedule of the maintenance window in the form of a cron or rate expression.
    schedule :: Core.Maybe Types.MaintenanceWindowSchedule,
    -- | The number of days to wait to run a maintenance window after the scheduled CRON expression date and time.
    scheduleOffset :: Core.Maybe Core.Natural,
    -- | The time zone that the scheduled maintenance window executions are based on, in Internet Assigned Numbers Authority (IANA) format.
    scheduleTimezone :: Core.Maybe Types.ScheduleTimezone,
    -- | The date and time, in ISO-8601 Extended format, for when the maintenance window is scheduled to become active.
    startDate :: Core.Maybe Types.StartDate,
    -- | The ID of the maintenance window.
    windowId :: Core.Maybe Types.WindowId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MaintenanceWindowIdentity' value with any optional fields omitted.
mkMaintenanceWindowIdentity ::
  MaintenanceWindowIdentity
mkMaintenanceWindowIdentity =
  MaintenanceWindowIdentity'
    { cutoff = Core.Nothing,
      description = Core.Nothing,
      duration = Core.Nothing,
      enabled = Core.Nothing,
      endDate = Core.Nothing,
      name = Core.Nothing,
      nextExecutionTime = Core.Nothing,
      schedule = Core.Nothing,
      scheduleOffset = Core.Nothing,
      scheduleTimezone = Core.Nothing,
      startDate = Core.Nothing,
      windowId = Core.Nothing
    }

-- | The number of hours before the end of the maintenance window that Systems Manager stops scheduling new tasks for execution.
--
-- /Note:/ Consider using 'cutoff' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwiCutoff :: Lens.Lens' MaintenanceWindowIdentity (Core.Maybe Core.Natural)
mwiCutoff = Lens.field @"cutoff"
{-# DEPRECATED mwiCutoff "Use generic-lens or generic-optics with 'cutoff' instead." #-}

-- | A description of the maintenance window.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwiDescription :: Lens.Lens' MaintenanceWindowIdentity (Core.Maybe Types.Description)
mwiDescription = Lens.field @"description"
{-# DEPRECATED mwiDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The duration of the maintenance window in hours.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwiDuration :: Lens.Lens' MaintenanceWindowIdentity (Core.Maybe Core.Natural)
mwiDuration = Lens.field @"duration"
{-# DEPRECATED mwiDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

-- | Indicates whether the maintenance window is enabled.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwiEnabled :: Lens.Lens' MaintenanceWindowIdentity (Core.Maybe Core.Bool)
mwiEnabled = Lens.field @"enabled"
{-# DEPRECATED mwiEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The date and time, in ISO-8601 Extended format, for when the maintenance window is scheduled to become inactive.
--
-- /Note:/ Consider using 'endDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwiEndDate :: Lens.Lens' MaintenanceWindowIdentity (Core.Maybe Types.EndDate)
mwiEndDate = Lens.field @"endDate"
{-# DEPRECATED mwiEndDate "Use generic-lens or generic-optics with 'endDate' instead." #-}

-- | The name of the maintenance window.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwiName :: Lens.Lens' MaintenanceWindowIdentity (Core.Maybe Types.Name)
mwiName = Lens.field @"name"
{-# DEPRECATED mwiName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The next time the maintenance window will actually run, taking into account any specified times for the maintenance window to become active or inactive.
--
-- /Note:/ Consider using 'nextExecutionTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwiNextExecutionTime :: Lens.Lens' MaintenanceWindowIdentity (Core.Maybe Types.NextExecutionTime)
mwiNextExecutionTime = Lens.field @"nextExecutionTime"
{-# DEPRECATED mwiNextExecutionTime "Use generic-lens or generic-optics with 'nextExecutionTime' instead." #-}

-- | The schedule of the maintenance window in the form of a cron or rate expression.
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwiSchedule :: Lens.Lens' MaintenanceWindowIdentity (Core.Maybe Types.MaintenanceWindowSchedule)
mwiSchedule = Lens.field @"schedule"
{-# DEPRECATED mwiSchedule "Use generic-lens or generic-optics with 'schedule' instead." #-}

-- | The number of days to wait to run a maintenance window after the scheduled CRON expression date and time.
--
-- /Note:/ Consider using 'scheduleOffset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwiScheduleOffset :: Lens.Lens' MaintenanceWindowIdentity (Core.Maybe Core.Natural)
mwiScheduleOffset = Lens.field @"scheduleOffset"
{-# DEPRECATED mwiScheduleOffset "Use generic-lens or generic-optics with 'scheduleOffset' instead." #-}

-- | The time zone that the scheduled maintenance window executions are based on, in Internet Assigned Numbers Authority (IANA) format.
--
-- /Note:/ Consider using 'scheduleTimezone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwiScheduleTimezone :: Lens.Lens' MaintenanceWindowIdentity (Core.Maybe Types.ScheduleTimezone)
mwiScheduleTimezone = Lens.field @"scheduleTimezone"
{-# DEPRECATED mwiScheduleTimezone "Use generic-lens or generic-optics with 'scheduleTimezone' instead." #-}

-- | The date and time, in ISO-8601 Extended format, for when the maintenance window is scheduled to become active.
--
-- /Note:/ Consider using 'startDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwiStartDate :: Lens.Lens' MaintenanceWindowIdentity (Core.Maybe Types.StartDate)
mwiStartDate = Lens.field @"startDate"
{-# DEPRECATED mwiStartDate "Use generic-lens or generic-optics with 'startDate' instead." #-}

-- | The ID of the maintenance window.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwiWindowId :: Lens.Lens' MaintenanceWindowIdentity (Core.Maybe Types.WindowId)
mwiWindowId = Lens.field @"windowId"
{-# DEPRECATED mwiWindowId "Use generic-lens or generic-optics with 'windowId' instead." #-}

instance Core.FromJSON MaintenanceWindowIdentity where
  parseJSON =
    Core.withObject "MaintenanceWindowIdentity" Core.$
      \x ->
        MaintenanceWindowIdentity'
          Core.<$> (x Core..:? "Cutoff")
          Core.<*> (x Core..:? "Description")
          Core.<*> (x Core..:? "Duration")
          Core.<*> (x Core..:? "Enabled")
          Core.<*> (x Core..:? "EndDate")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "NextExecutionTime")
          Core.<*> (x Core..:? "Schedule")
          Core.<*> (x Core..:? "ScheduleOffset")
          Core.<*> (x Core..:? "ScheduleTimezone")
          Core.<*> (x Core..:? "StartDate")
          Core.<*> (x Core..:? "WindowId")
