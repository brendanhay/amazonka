{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.UpdateMaintenanceWindow
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing maintenance window. Only specified parameters are modified.
module Network.AWS.SSM.UpdateMaintenanceWindow
  ( -- * Creating a request
    UpdateMaintenanceWindow (..),
    mkUpdateMaintenanceWindow,

    -- ** Request lenses
    umwReplace,
    umwEnabled,
    umwSchedule,
    umwScheduleOffset,
    umwEndDate,
    umwScheduleTimezone,
    umwStartDate,
    umwName,
    umwCutoff,
    umwAllowUnassociatedTargets,
    umwDescription,
    umwDuration,
    umwWindowId,

    -- * Destructuring the response
    UpdateMaintenanceWindowResponse (..),
    mkUpdateMaintenanceWindowResponse,

    -- ** Response lenses
    umwrsEnabled,
    umwrsSchedule,
    umwrsScheduleOffset,
    umwrsEndDate,
    umwrsScheduleTimezone,
    umwrsStartDate,
    umwrsName,
    umwrsCutoff,
    umwrsAllowUnassociatedTargets,
    umwrsDescription,
    umwrsDuration,
    umwrsWindowId,
    umwrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkUpdateMaintenanceWindow' smart constructor.
data UpdateMaintenanceWindow = UpdateMaintenanceWindow'
  { -- | If True, then all fields that are required by the CreateMaintenanceWindow action are also required for this API request. Optional fields that are not specified are set to null.
    replace :: Lude.Maybe Lude.Bool,
    -- | Whether the maintenance window is enabled.
    enabled :: Lude.Maybe Lude.Bool,
    -- | The schedule of the maintenance window in the form of a cron or rate expression.
    schedule :: Lude.Maybe Lude.Text,
    -- | The number of days to wait after the date and time specified by a CRON expression before running the maintenance window.
    --
    -- For example, the following cron expression schedules a maintenance window to run the third Tuesday of every month at 11:30 PM.
    -- @cron(0 30 23 ? * TUE#3 *)@
    -- If the schedule offset is @2@ , the maintenance window won't run until two days later.
    scheduleOffset :: Lude.Maybe Lude.Natural,
    -- | The date and time, in ISO-8601 Extended format, for when you want the maintenance window to become inactive. EndDate allows you to set a date and time in the future when the maintenance window will no longer run.
    endDate :: Lude.Maybe Lude.Text,
    -- | The time zone that the scheduled maintenance window executions are based on, in Internet Assigned Numbers Authority (IANA) format. For example: "America/Los_Angeles", "etc/UTC", or "Asia/Seoul". For more information, see the <https://www.iana.org/time-zones Time Zone Database> on the IANA website.
    scheduleTimezone :: Lude.Maybe Lude.Text,
    -- | The time zone that the scheduled maintenance window executions are based on, in Internet Assigned Numbers Authority (IANA) format. For example: "America/Los_Angeles", "etc/UTC", or "Asia/Seoul". For more information, see the <https://www.iana.org/time-zones Time Zone Database> on the IANA website.
    startDate :: Lude.Maybe Lude.Text,
    -- | The name of the maintenance window.
    name :: Lude.Maybe Lude.Text,
    -- | The number of hours before the end of the maintenance window that Systems Manager stops scheduling new tasks for execution.
    cutoff :: Lude.Maybe Lude.Natural,
    -- | Whether targets must be registered with the maintenance window before tasks can be defined for those targets.
    allowUnassociatedTargets :: Lude.Maybe Lude.Bool,
    -- | An optional description for the update request.
    description :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The duration of the maintenance window in hours.
    duration :: Lude.Maybe Lude.Natural,
    -- | The ID of the maintenance window to update.
    windowId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateMaintenanceWindow' with the minimum fields required to make a request.
--
-- * 'replace' - If True, then all fields that are required by the CreateMaintenanceWindow action are also required for this API request. Optional fields that are not specified are set to null.
-- * 'enabled' - Whether the maintenance window is enabled.
-- * 'schedule' - The schedule of the maintenance window in the form of a cron or rate expression.
-- * 'scheduleOffset' - The number of days to wait after the date and time specified by a CRON expression before running the maintenance window.
--
-- For example, the following cron expression schedules a maintenance window to run the third Tuesday of every month at 11:30 PM.
-- @cron(0 30 23 ? * TUE#3 *)@
-- If the schedule offset is @2@ , the maintenance window won't run until two days later.
-- * 'endDate' - The date and time, in ISO-8601 Extended format, for when you want the maintenance window to become inactive. EndDate allows you to set a date and time in the future when the maintenance window will no longer run.
-- * 'scheduleTimezone' - The time zone that the scheduled maintenance window executions are based on, in Internet Assigned Numbers Authority (IANA) format. For example: "America/Los_Angeles", "etc/UTC", or "Asia/Seoul". For more information, see the <https://www.iana.org/time-zones Time Zone Database> on the IANA website.
-- * 'startDate' - The time zone that the scheduled maintenance window executions are based on, in Internet Assigned Numbers Authority (IANA) format. For example: "America/Los_Angeles", "etc/UTC", or "Asia/Seoul". For more information, see the <https://www.iana.org/time-zones Time Zone Database> on the IANA website.
-- * 'name' - The name of the maintenance window.
-- * 'cutoff' - The number of hours before the end of the maintenance window that Systems Manager stops scheduling new tasks for execution.
-- * 'allowUnassociatedTargets' - Whether targets must be registered with the maintenance window before tasks can be defined for those targets.
-- * 'description' - An optional description for the update request.
-- * 'duration' - The duration of the maintenance window in hours.
-- * 'windowId' - The ID of the maintenance window to update.
mkUpdateMaintenanceWindow ::
  -- | 'windowId'
  Lude.Text ->
  UpdateMaintenanceWindow
mkUpdateMaintenanceWindow pWindowId_ =
  UpdateMaintenanceWindow'
    { replace = Lude.Nothing,
      enabled = Lude.Nothing,
      schedule = Lude.Nothing,
      scheduleOffset = Lude.Nothing,
      endDate = Lude.Nothing,
      scheduleTimezone = Lude.Nothing,
      startDate = Lude.Nothing,
      name = Lude.Nothing,
      cutoff = Lude.Nothing,
      allowUnassociatedTargets = Lude.Nothing,
      description = Lude.Nothing,
      duration = Lude.Nothing,
      windowId = pWindowId_
    }

-- | If True, then all fields that are required by the CreateMaintenanceWindow action are also required for this API request. Optional fields that are not specified are set to null.
--
-- /Note:/ Consider using 'replace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwReplace :: Lens.Lens' UpdateMaintenanceWindow (Lude.Maybe Lude.Bool)
umwReplace = Lens.lens (replace :: UpdateMaintenanceWindow -> Lude.Maybe Lude.Bool) (\s a -> s {replace = a} :: UpdateMaintenanceWindow)
{-# DEPRECATED umwReplace "Use generic-lens or generic-optics with 'replace' instead." #-}

-- | Whether the maintenance window is enabled.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwEnabled :: Lens.Lens' UpdateMaintenanceWindow (Lude.Maybe Lude.Bool)
umwEnabled = Lens.lens (enabled :: UpdateMaintenanceWindow -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: UpdateMaintenanceWindow)
{-# DEPRECATED umwEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The schedule of the maintenance window in the form of a cron or rate expression.
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwSchedule :: Lens.Lens' UpdateMaintenanceWindow (Lude.Maybe Lude.Text)
umwSchedule = Lens.lens (schedule :: UpdateMaintenanceWindow -> Lude.Maybe Lude.Text) (\s a -> s {schedule = a} :: UpdateMaintenanceWindow)
{-# DEPRECATED umwSchedule "Use generic-lens or generic-optics with 'schedule' instead." #-}

-- | The number of days to wait after the date and time specified by a CRON expression before running the maintenance window.
--
-- For example, the following cron expression schedules a maintenance window to run the third Tuesday of every month at 11:30 PM.
-- @cron(0 30 23 ? * TUE#3 *)@
-- If the schedule offset is @2@ , the maintenance window won't run until two days later.
--
-- /Note:/ Consider using 'scheduleOffset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwScheduleOffset :: Lens.Lens' UpdateMaintenanceWindow (Lude.Maybe Lude.Natural)
umwScheduleOffset = Lens.lens (scheduleOffset :: UpdateMaintenanceWindow -> Lude.Maybe Lude.Natural) (\s a -> s {scheduleOffset = a} :: UpdateMaintenanceWindow)
{-# DEPRECATED umwScheduleOffset "Use generic-lens or generic-optics with 'scheduleOffset' instead." #-}

-- | The date and time, in ISO-8601 Extended format, for when you want the maintenance window to become inactive. EndDate allows you to set a date and time in the future when the maintenance window will no longer run.
--
-- /Note:/ Consider using 'endDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwEndDate :: Lens.Lens' UpdateMaintenanceWindow (Lude.Maybe Lude.Text)
umwEndDate = Lens.lens (endDate :: UpdateMaintenanceWindow -> Lude.Maybe Lude.Text) (\s a -> s {endDate = a} :: UpdateMaintenanceWindow)
{-# DEPRECATED umwEndDate "Use generic-lens or generic-optics with 'endDate' instead." #-}

-- | The time zone that the scheduled maintenance window executions are based on, in Internet Assigned Numbers Authority (IANA) format. For example: "America/Los_Angeles", "etc/UTC", or "Asia/Seoul". For more information, see the <https://www.iana.org/time-zones Time Zone Database> on the IANA website.
--
-- /Note:/ Consider using 'scheduleTimezone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwScheduleTimezone :: Lens.Lens' UpdateMaintenanceWindow (Lude.Maybe Lude.Text)
umwScheduleTimezone = Lens.lens (scheduleTimezone :: UpdateMaintenanceWindow -> Lude.Maybe Lude.Text) (\s a -> s {scheduleTimezone = a} :: UpdateMaintenanceWindow)
{-# DEPRECATED umwScheduleTimezone "Use generic-lens or generic-optics with 'scheduleTimezone' instead." #-}

-- | The time zone that the scheduled maintenance window executions are based on, in Internet Assigned Numbers Authority (IANA) format. For example: "America/Los_Angeles", "etc/UTC", or "Asia/Seoul". For more information, see the <https://www.iana.org/time-zones Time Zone Database> on the IANA website.
--
-- /Note:/ Consider using 'startDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwStartDate :: Lens.Lens' UpdateMaintenanceWindow (Lude.Maybe Lude.Text)
umwStartDate = Lens.lens (startDate :: UpdateMaintenanceWindow -> Lude.Maybe Lude.Text) (\s a -> s {startDate = a} :: UpdateMaintenanceWindow)
{-# DEPRECATED umwStartDate "Use generic-lens or generic-optics with 'startDate' instead." #-}

-- | The name of the maintenance window.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwName :: Lens.Lens' UpdateMaintenanceWindow (Lude.Maybe Lude.Text)
umwName = Lens.lens (name :: UpdateMaintenanceWindow -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateMaintenanceWindow)
{-# DEPRECATED umwName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The number of hours before the end of the maintenance window that Systems Manager stops scheduling new tasks for execution.
--
-- /Note:/ Consider using 'cutoff' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwCutoff :: Lens.Lens' UpdateMaintenanceWindow (Lude.Maybe Lude.Natural)
umwCutoff = Lens.lens (cutoff :: UpdateMaintenanceWindow -> Lude.Maybe Lude.Natural) (\s a -> s {cutoff = a} :: UpdateMaintenanceWindow)
{-# DEPRECATED umwCutoff "Use generic-lens or generic-optics with 'cutoff' instead." #-}

-- | Whether targets must be registered with the maintenance window before tasks can be defined for those targets.
--
-- /Note:/ Consider using 'allowUnassociatedTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwAllowUnassociatedTargets :: Lens.Lens' UpdateMaintenanceWindow (Lude.Maybe Lude.Bool)
umwAllowUnassociatedTargets = Lens.lens (allowUnassociatedTargets :: UpdateMaintenanceWindow -> Lude.Maybe Lude.Bool) (\s a -> s {allowUnassociatedTargets = a} :: UpdateMaintenanceWindow)
{-# DEPRECATED umwAllowUnassociatedTargets "Use generic-lens or generic-optics with 'allowUnassociatedTargets' instead." #-}

-- | An optional description for the update request.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwDescription :: Lens.Lens' UpdateMaintenanceWindow (Lude.Maybe (Lude.Sensitive Lude.Text))
umwDescription = Lens.lens (description :: UpdateMaintenanceWindow -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {description = a} :: UpdateMaintenanceWindow)
{-# DEPRECATED umwDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The duration of the maintenance window in hours.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwDuration :: Lens.Lens' UpdateMaintenanceWindow (Lude.Maybe Lude.Natural)
umwDuration = Lens.lens (duration :: UpdateMaintenanceWindow -> Lude.Maybe Lude.Natural) (\s a -> s {duration = a} :: UpdateMaintenanceWindow)
{-# DEPRECATED umwDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

-- | The ID of the maintenance window to update.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwWindowId :: Lens.Lens' UpdateMaintenanceWindow Lude.Text
umwWindowId = Lens.lens (windowId :: UpdateMaintenanceWindow -> Lude.Text) (\s a -> s {windowId = a} :: UpdateMaintenanceWindow)
{-# DEPRECATED umwWindowId "Use generic-lens or generic-optics with 'windowId' instead." #-}

instance Lude.AWSRequest UpdateMaintenanceWindow where
  type Rs UpdateMaintenanceWindow = UpdateMaintenanceWindowResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateMaintenanceWindowResponse'
            Lude.<$> (x Lude..?> "Enabled")
            Lude.<*> (x Lude..?> "Schedule")
            Lude.<*> (x Lude..?> "ScheduleOffset")
            Lude.<*> (x Lude..?> "EndDate")
            Lude.<*> (x Lude..?> "ScheduleTimezone")
            Lude.<*> (x Lude..?> "StartDate")
            Lude.<*> (x Lude..?> "Name")
            Lude.<*> (x Lude..?> "Cutoff")
            Lude.<*> (x Lude..?> "AllowUnassociatedTargets")
            Lude.<*> (x Lude..?> "Description")
            Lude.<*> (x Lude..?> "Duration")
            Lude.<*> (x Lude..?> "WindowId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateMaintenanceWindow where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.UpdateMaintenanceWindow" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateMaintenanceWindow where
  toJSON UpdateMaintenanceWindow' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Replace" Lude..=) Lude.<$> replace,
            ("Enabled" Lude..=) Lude.<$> enabled,
            ("Schedule" Lude..=) Lude.<$> schedule,
            ("ScheduleOffset" Lude..=) Lude.<$> scheduleOffset,
            ("EndDate" Lude..=) Lude.<$> endDate,
            ("ScheduleTimezone" Lude..=) Lude.<$> scheduleTimezone,
            ("StartDate" Lude..=) Lude.<$> startDate,
            ("Name" Lude..=) Lude.<$> name,
            ("Cutoff" Lude..=) Lude.<$> cutoff,
            ("AllowUnassociatedTargets" Lude..=)
              Lude.<$> allowUnassociatedTargets,
            ("Description" Lude..=) Lude.<$> description,
            ("Duration" Lude..=) Lude.<$> duration,
            Lude.Just ("WindowId" Lude..= windowId)
          ]
      )

instance Lude.ToPath UpdateMaintenanceWindow where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateMaintenanceWindow where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateMaintenanceWindowResponse' smart constructor.
data UpdateMaintenanceWindowResponse = UpdateMaintenanceWindowResponse'
  { -- | Whether the maintenance window is enabled.
    enabled :: Lude.Maybe Lude.Bool,
    -- | The schedule of the maintenance window in the form of a cron or rate expression.
    schedule :: Lude.Maybe Lude.Text,
    -- | The number of days to wait to run a maintenance window after the scheduled CRON expression date and time.
    scheduleOffset :: Lude.Maybe Lude.Natural,
    -- | The date and time, in ISO-8601 Extended format, for when the maintenance window is scheduled to become inactive. The maintenance window will not run after this specified time.
    endDate :: Lude.Maybe Lude.Text,
    -- | The time zone that the scheduled maintenance window executions are based on, in Internet Assigned Numbers Authority (IANA) format. For example: "America/Los_Angeles", "etc/UTC", or "Asia/Seoul". For more information, see the <https://www.iana.org/time-zones Time Zone Database> on the IANA website.
    scheduleTimezone :: Lude.Maybe Lude.Text,
    -- | The date and time, in ISO-8601 Extended format, for when the maintenance window is scheduled to become active. The maintenance window will not run before this specified time.
    startDate :: Lude.Maybe Lude.Text,
    -- | The name of the maintenance window.
    name :: Lude.Maybe Lude.Text,
    -- | The number of hours before the end of the maintenance window that Systems Manager stops scheduling new tasks for execution.
    cutoff :: Lude.Maybe Lude.Natural,
    -- | Whether targets must be registered with the maintenance window before tasks can be defined for those targets.
    allowUnassociatedTargets :: Lude.Maybe Lude.Bool,
    -- | An optional description of the update.
    description :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The duration of the maintenance window in hours.
    duration :: Lude.Maybe Lude.Natural,
    -- | The ID of the created maintenance window.
    windowId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateMaintenanceWindowResponse' with the minimum fields required to make a request.
--
-- * 'enabled' - Whether the maintenance window is enabled.
-- * 'schedule' - The schedule of the maintenance window in the form of a cron or rate expression.
-- * 'scheduleOffset' - The number of days to wait to run a maintenance window after the scheduled CRON expression date and time.
-- * 'endDate' - The date and time, in ISO-8601 Extended format, for when the maintenance window is scheduled to become inactive. The maintenance window will not run after this specified time.
-- * 'scheduleTimezone' - The time zone that the scheduled maintenance window executions are based on, in Internet Assigned Numbers Authority (IANA) format. For example: "America/Los_Angeles", "etc/UTC", or "Asia/Seoul". For more information, see the <https://www.iana.org/time-zones Time Zone Database> on the IANA website.
-- * 'startDate' - The date and time, in ISO-8601 Extended format, for when the maintenance window is scheduled to become active. The maintenance window will not run before this specified time.
-- * 'name' - The name of the maintenance window.
-- * 'cutoff' - The number of hours before the end of the maintenance window that Systems Manager stops scheduling new tasks for execution.
-- * 'allowUnassociatedTargets' - Whether targets must be registered with the maintenance window before tasks can be defined for those targets.
-- * 'description' - An optional description of the update.
-- * 'duration' - The duration of the maintenance window in hours.
-- * 'windowId' - The ID of the created maintenance window.
-- * 'responseStatus' - The response status code.
mkUpdateMaintenanceWindowResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateMaintenanceWindowResponse
mkUpdateMaintenanceWindowResponse pResponseStatus_ =
  UpdateMaintenanceWindowResponse'
    { enabled = Lude.Nothing,
      schedule = Lude.Nothing,
      scheduleOffset = Lude.Nothing,
      endDate = Lude.Nothing,
      scheduleTimezone = Lude.Nothing,
      startDate = Lude.Nothing,
      name = Lude.Nothing,
      cutoff = Lude.Nothing,
      allowUnassociatedTargets = Lude.Nothing,
      description = Lude.Nothing,
      duration = Lude.Nothing,
      windowId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Whether the maintenance window is enabled.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwrsEnabled :: Lens.Lens' UpdateMaintenanceWindowResponse (Lude.Maybe Lude.Bool)
umwrsEnabled = Lens.lens (enabled :: UpdateMaintenanceWindowResponse -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: UpdateMaintenanceWindowResponse)
{-# DEPRECATED umwrsEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The schedule of the maintenance window in the form of a cron or rate expression.
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwrsSchedule :: Lens.Lens' UpdateMaintenanceWindowResponse (Lude.Maybe Lude.Text)
umwrsSchedule = Lens.lens (schedule :: UpdateMaintenanceWindowResponse -> Lude.Maybe Lude.Text) (\s a -> s {schedule = a} :: UpdateMaintenanceWindowResponse)
{-# DEPRECATED umwrsSchedule "Use generic-lens or generic-optics with 'schedule' instead." #-}

-- | The number of days to wait to run a maintenance window after the scheduled CRON expression date and time.
--
-- /Note:/ Consider using 'scheduleOffset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwrsScheduleOffset :: Lens.Lens' UpdateMaintenanceWindowResponse (Lude.Maybe Lude.Natural)
umwrsScheduleOffset = Lens.lens (scheduleOffset :: UpdateMaintenanceWindowResponse -> Lude.Maybe Lude.Natural) (\s a -> s {scheduleOffset = a} :: UpdateMaintenanceWindowResponse)
{-# DEPRECATED umwrsScheduleOffset "Use generic-lens or generic-optics with 'scheduleOffset' instead." #-}

-- | The date and time, in ISO-8601 Extended format, for when the maintenance window is scheduled to become inactive. The maintenance window will not run after this specified time.
--
-- /Note:/ Consider using 'endDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwrsEndDate :: Lens.Lens' UpdateMaintenanceWindowResponse (Lude.Maybe Lude.Text)
umwrsEndDate = Lens.lens (endDate :: UpdateMaintenanceWindowResponse -> Lude.Maybe Lude.Text) (\s a -> s {endDate = a} :: UpdateMaintenanceWindowResponse)
{-# DEPRECATED umwrsEndDate "Use generic-lens or generic-optics with 'endDate' instead." #-}

-- | The time zone that the scheduled maintenance window executions are based on, in Internet Assigned Numbers Authority (IANA) format. For example: "America/Los_Angeles", "etc/UTC", or "Asia/Seoul". For more information, see the <https://www.iana.org/time-zones Time Zone Database> on the IANA website.
--
-- /Note:/ Consider using 'scheduleTimezone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwrsScheduleTimezone :: Lens.Lens' UpdateMaintenanceWindowResponse (Lude.Maybe Lude.Text)
umwrsScheduleTimezone = Lens.lens (scheduleTimezone :: UpdateMaintenanceWindowResponse -> Lude.Maybe Lude.Text) (\s a -> s {scheduleTimezone = a} :: UpdateMaintenanceWindowResponse)
{-# DEPRECATED umwrsScheduleTimezone "Use generic-lens or generic-optics with 'scheduleTimezone' instead." #-}

-- | The date and time, in ISO-8601 Extended format, for when the maintenance window is scheduled to become active. The maintenance window will not run before this specified time.
--
-- /Note:/ Consider using 'startDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwrsStartDate :: Lens.Lens' UpdateMaintenanceWindowResponse (Lude.Maybe Lude.Text)
umwrsStartDate = Lens.lens (startDate :: UpdateMaintenanceWindowResponse -> Lude.Maybe Lude.Text) (\s a -> s {startDate = a} :: UpdateMaintenanceWindowResponse)
{-# DEPRECATED umwrsStartDate "Use generic-lens or generic-optics with 'startDate' instead." #-}

-- | The name of the maintenance window.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwrsName :: Lens.Lens' UpdateMaintenanceWindowResponse (Lude.Maybe Lude.Text)
umwrsName = Lens.lens (name :: UpdateMaintenanceWindowResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateMaintenanceWindowResponse)
{-# DEPRECATED umwrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The number of hours before the end of the maintenance window that Systems Manager stops scheduling new tasks for execution.
--
-- /Note:/ Consider using 'cutoff' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwrsCutoff :: Lens.Lens' UpdateMaintenanceWindowResponse (Lude.Maybe Lude.Natural)
umwrsCutoff = Lens.lens (cutoff :: UpdateMaintenanceWindowResponse -> Lude.Maybe Lude.Natural) (\s a -> s {cutoff = a} :: UpdateMaintenanceWindowResponse)
{-# DEPRECATED umwrsCutoff "Use generic-lens or generic-optics with 'cutoff' instead." #-}

-- | Whether targets must be registered with the maintenance window before tasks can be defined for those targets.
--
-- /Note:/ Consider using 'allowUnassociatedTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwrsAllowUnassociatedTargets :: Lens.Lens' UpdateMaintenanceWindowResponse (Lude.Maybe Lude.Bool)
umwrsAllowUnassociatedTargets = Lens.lens (allowUnassociatedTargets :: UpdateMaintenanceWindowResponse -> Lude.Maybe Lude.Bool) (\s a -> s {allowUnassociatedTargets = a} :: UpdateMaintenanceWindowResponse)
{-# DEPRECATED umwrsAllowUnassociatedTargets "Use generic-lens or generic-optics with 'allowUnassociatedTargets' instead." #-}

-- | An optional description of the update.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwrsDescription :: Lens.Lens' UpdateMaintenanceWindowResponse (Lude.Maybe (Lude.Sensitive Lude.Text))
umwrsDescription = Lens.lens (description :: UpdateMaintenanceWindowResponse -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {description = a} :: UpdateMaintenanceWindowResponse)
{-# DEPRECATED umwrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The duration of the maintenance window in hours.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwrsDuration :: Lens.Lens' UpdateMaintenanceWindowResponse (Lude.Maybe Lude.Natural)
umwrsDuration = Lens.lens (duration :: UpdateMaintenanceWindowResponse -> Lude.Maybe Lude.Natural) (\s a -> s {duration = a} :: UpdateMaintenanceWindowResponse)
{-# DEPRECATED umwrsDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

-- | The ID of the created maintenance window.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwrsWindowId :: Lens.Lens' UpdateMaintenanceWindowResponse (Lude.Maybe Lude.Text)
umwrsWindowId = Lens.lens (windowId :: UpdateMaintenanceWindowResponse -> Lude.Maybe Lude.Text) (\s a -> s {windowId = a} :: UpdateMaintenanceWindowResponse)
{-# DEPRECATED umwrsWindowId "Use generic-lens or generic-optics with 'windowId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwrsResponseStatus :: Lens.Lens' UpdateMaintenanceWindowResponse Lude.Int
umwrsResponseStatus = Lens.lens (responseStatus :: UpdateMaintenanceWindowResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateMaintenanceWindowResponse)
{-# DEPRECATED umwrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
