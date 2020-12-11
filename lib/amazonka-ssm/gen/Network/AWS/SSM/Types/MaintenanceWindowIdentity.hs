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
    mwiEnabled,
    mwiSchedule,
    mwiNextExecutionTime,
    mwiScheduleOffset,
    mwiEndDate,
    mwiScheduleTimezone,
    mwiStartDate,
    mwiName,
    mwiCutoff,
    mwiDescription,
    mwiDuration,
    mwiWindowId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the maintenance window.
--
-- /See:/ 'mkMaintenanceWindowIdentity' smart constructor.
data MaintenanceWindowIdentity = MaintenanceWindowIdentity'
  { enabled ::
      Lude.Maybe Lude.Bool,
    schedule :: Lude.Maybe Lude.Text,
    nextExecutionTime ::
      Lude.Maybe Lude.Text,
    scheduleOffset ::
      Lude.Maybe Lude.Natural,
    endDate :: Lude.Maybe Lude.Text,
    scheduleTimezone ::
      Lude.Maybe Lude.Text,
    startDate :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    cutoff :: Lude.Maybe Lude.Natural,
    description ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    duration :: Lude.Maybe Lude.Natural,
    windowId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MaintenanceWindowIdentity' with the minimum fields required to make a request.
--
-- * 'cutoff' - The number of hours before the end of the maintenance window that Systems Manager stops scheduling new tasks for execution.
-- * 'description' - A description of the maintenance window.
-- * 'duration' - The duration of the maintenance window in hours.
-- * 'enabled' - Indicates whether the maintenance window is enabled.
-- * 'endDate' - The date and time, in ISO-8601 Extended format, for when the maintenance window is scheduled to become inactive.
-- * 'name' - The name of the maintenance window.
-- * 'nextExecutionTime' - The next time the maintenance window will actually run, taking into account any specified times for the maintenance window to become active or inactive.
-- * 'schedule' - The schedule of the maintenance window in the form of a cron or rate expression.
-- * 'scheduleOffset' - The number of days to wait to run a maintenance window after the scheduled CRON expression date and time.
-- * 'scheduleTimezone' - The time zone that the scheduled maintenance window executions are based on, in Internet Assigned Numbers Authority (IANA) format.
-- * 'startDate' - The date and time, in ISO-8601 Extended format, for when the maintenance window is scheduled to become active.
-- * 'windowId' - The ID of the maintenance window.
mkMaintenanceWindowIdentity ::
  MaintenanceWindowIdentity
mkMaintenanceWindowIdentity =
  MaintenanceWindowIdentity'
    { enabled = Lude.Nothing,
      schedule = Lude.Nothing,
      nextExecutionTime = Lude.Nothing,
      scheduleOffset = Lude.Nothing,
      endDate = Lude.Nothing,
      scheduleTimezone = Lude.Nothing,
      startDate = Lude.Nothing,
      name = Lude.Nothing,
      cutoff = Lude.Nothing,
      description = Lude.Nothing,
      duration = Lude.Nothing,
      windowId = Lude.Nothing
    }

-- | Indicates whether the maintenance window is enabled.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwiEnabled :: Lens.Lens' MaintenanceWindowIdentity (Lude.Maybe Lude.Bool)
mwiEnabled = Lens.lens (enabled :: MaintenanceWindowIdentity -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: MaintenanceWindowIdentity)
{-# DEPRECATED mwiEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The schedule of the maintenance window in the form of a cron or rate expression.
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwiSchedule :: Lens.Lens' MaintenanceWindowIdentity (Lude.Maybe Lude.Text)
mwiSchedule = Lens.lens (schedule :: MaintenanceWindowIdentity -> Lude.Maybe Lude.Text) (\s a -> s {schedule = a} :: MaintenanceWindowIdentity)
{-# DEPRECATED mwiSchedule "Use generic-lens or generic-optics with 'schedule' instead." #-}

-- | The next time the maintenance window will actually run, taking into account any specified times for the maintenance window to become active or inactive.
--
-- /Note:/ Consider using 'nextExecutionTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwiNextExecutionTime :: Lens.Lens' MaintenanceWindowIdentity (Lude.Maybe Lude.Text)
mwiNextExecutionTime = Lens.lens (nextExecutionTime :: MaintenanceWindowIdentity -> Lude.Maybe Lude.Text) (\s a -> s {nextExecutionTime = a} :: MaintenanceWindowIdentity)
{-# DEPRECATED mwiNextExecutionTime "Use generic-lens or generic-optics with 'nextExecutionTime' instead." #-}

-- | The number of days to wait to run a maintenance window after the scheduled CRON expression date and time.
--
-- /Note:/ Consider using 'scheduleOffset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwiScheduleOffset :: Lens.Lens' MaintenanceWindowIdentity (Lude.Maybe Lude.Natural)
mwiScheduleOffset = Lens.lens (scheduleOffset :: MaintenanceWindowIdentity -> Lude.Maybe Lude.Natural) (\s a -> s {scheduleOffset = a} :: MaintenanceWindowIdentity)
{-# DEPRECATED mwiScheduleOffset "Use generic-lens or generic-optics with 'scheduleOffset' instead." #-}

-- | The date and time, in ISO-8601 Extended format, for when the maintenance window is scheduled to become inactive.
--
-- /Note:/ Consider using 'endDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwiEndDate :: Lens.Lens' MaintenanceWindowIdentity (Lude.Maybe Lude.Text)
mwiEndDate = Lens.lens (endDate :: MaintenanceWindowIdentity -> Lude.Maybe Lude.Text) (\s a -> s {endDate = a} :: MaintenanceWindowIdentity)
{-# DEPRECATED mwiEndDate "Use generic-lens or generic-optics with 'endDate' instead." #-}

-- | The time zone that the scheduled maintenance window executions are based on, in Internet Assigned Numbers Authority (IANA) format.
--
-- /Note:/ Consider using 'scheduleTimezone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwiScheduleTimezone :: Lens.Lens' MaintenanceWindowIdentity (Lude.Maybe Lude.Text)
mwiScheduleTimezone = Lens.lens (scheduleTimezone :: MaintenanceWindowIdentity -> Lude.Maybe Lude.Text) (\s a -> s {scheduleTimezone = a} :: MaintenanceWindowIdentity)
{-# DEPRECATED mwiScheduleTimezone "Use generic-lens or generic-optics with 'scheduleTimezone' instead." #-}

-- | The date and time, in ISO-8601 Extended format, for when the maintenance window is scheduled to become active.
--
-- /Note:/ Consider using 'startDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwiStartDate :: Lens.Lens' MaintenanceWindowIdentity (Lude.Maybe Lude.Text)
mwiStartDate = Lens.lens (startDate :: MaintenanceWindowIdentity -> Lude.Maybe Lude.Text) (\s a -> s {startDate = a} :: MaintenanceWindowIdentity)
{-# DEPRECATED mwiStartDate "Use generic-lens or generic-optics with 'startDate' instead." #-}

-- | The name of the maintenance window.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwiName :: Lens.Lens' MaintenanceWindowIdentity (Lude.Maybe Lude.Text)
mwiName = Lens.lens (name :: MaintenanceWindowIdentity -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: MaintenanceWindowIdentity)
{-# DEPRECATED mwiName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The number of hours before the end of the maintenance window that Systems Manager stops scheduling new tasks for execution.
--
-- /Note:/ Consider using 'cutoff' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwiCutoff :: Lens.Lens' MaintenanceWindowIdentity (Lude.Maybe Lude.Natural)
mwiCutoff = Lens.lens (cutoff :: MaintenanceWindowIdentity -> Lude.Maybe Lude.Natural) (\s a -> s {cutoff = a} :: MaintenanceWindowIdentity)
{-# DEPRECATED mwiCutoff "Use generic-lens or generic-optics with 'cutoff' instead." #-}

-- | A description of the maintenance window.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwiDescription :: Lens.Lens' MaintenanceWindowIdentity (Lude.Maybe (Lude.Sensitive Lude.Text))
mwiDescription = Lens.lens (description :: MaintenanceWindowIdentity -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {description = a} :: MaintenanceWindowIdentity)
{-# DEPRECATED mwiDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The duration of the maintenance window in hours.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwiDuration :: Lens.Lens' MaintenanceWindowIdentity (Lude.Maybe Lude.Natural)
mwiDuration = Lens.lens (duration :: MaintenanceWindowIdentity -> Lude.Maybe Lude.Natural) (\s a -> s {duration = a} :: MaintenanceWindowIdentity)
{-# DEPRECATED mwiDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

-- | The ID of the maintenance window.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwiWindowId :: Lens.Lens' MaintenanceWindowIdentity (Lude.Maybe Lude.Text)
mwiWindowId = Lens.lens (windowId :: MaintenanceWindowIdentity -> Lude.Maybe Lude.Text) (\s a -> s {windowId = a} :: MaintenanceWindowIdentity)
{-# DEPRECATED mwiWindowId "Use generic-lens or generic-optics with 'windowId' instead." #-}

instance Lude.FromJSON MaintenanceWindowIdentity where
  parseJSON =
    Lude.withObject
      "MaintenanceWindowIdentity"
      ( \x ->
          MaintenanceWindowIdentity'
            Lude.<$> (x Lude..:? "Enabled")
            Lude.<*> (x Lude..:? "Schedule")
            Lude.<*> (x Lude..:? "NextExecutionTime")
            Lude.<*> (x Lude..:? "ScheduleOffset")
            Lude.<*> (x Lude..:? "EndDate")
            Lude.<*> (x Lude..:? "ScheduleTimezone")
            Lude.<*> (x Lude..:? "StartDate")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Cutoff")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "Duration")
            Lude.<*> (x Lude..:? "WindowId")
      )
