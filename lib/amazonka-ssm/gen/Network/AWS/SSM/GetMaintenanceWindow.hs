{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.GetMaintenanceWindow
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a maintenance window.
module Network.AWS.SSM.GetMaintenanceWindow
  ( -- * Creating a request
    GetMaintenanceWindow (..),
    mkGetMaintenanceWindow,

    -- ** Request lenses
    gmwWindowId,

    -- * Destructuring the response
    GetMaintenanceWindowResponse (..),
    mkGetMaintenanceWindowResponse,

    -- ** Response lenses
    gmwrsEnabled,
    gmwrsSchedule,
    gmwrsNextExecutionTime,
    gmwrsScheduleOffset,
    gmwrsEndDate,
    gmwrsScheduleTimezone,
    gmwrsStartDate,
    gmwrsCreatedDate,
    gmwrsName,
    gmwrsModifiedDate,
    gmwrsCutoff,
    gmwrsAllowUnassociatedTargets,
    gmwrsDescription,
    gmwrsDuration,
    gmwrsWindowId,
    gmwrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkGetMaintenanceWindow' smart constructor.
newtype GetMaintenanceWindow = GetMaintenanceWindow'
  { -- | The ID of the maintenance window for which you want to retrieve information.
    windowId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetMaintenanceWindow' with the minimum fields required to make a request.
--
-- * 'windowId' - The ID of the maintenance window for which you want to retrieve information.
mkGetMaintenanceWindow ::
  -- | 'windowId'
  Lude.Text ->
  GetMaintenanceWindow
mkGetMaintenanceWindow pWindowId_ =
  GetMaintenanceWindow' {windowId = pWindowId_}

-- | The ID of the maintenance window for which you want to retrieve information.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwWindowId :: Lens.Lens' GetMaintenanceWindow Lude.Text
gmwWindowId = Lens.lens (windowId :: GetMaintenanceWindow -> Lude.Text) (\s a -> s {windowId = a} :: GetMaintenanceWindow)
{-# DEPRECATED gmwWindowId "Use generic-lens or generic-optics with 'windowId' instead." #-}

instance Lude.AWSRequest GetMaintenanceWindow where
  type Rs GetMaintenanceWindow = GetMaintenanceWindowResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetMaintenanceWindowResponse'
            Lude.<$> (x Lude..?> "Enabled")
            Lude.<*> (x Lude..?> "Schedule")
            Lude.<*> (x Lude..?> "NextExecutionTime")
            Lude.<*> (x Lude..?> "ScheduleOffset")
            Lude.<*> (x Lude..?> "EndDate")
            Lude.<*> (x Lude..?> "ScheduleTimezone")
            Lude.<*> (x Lude..?> "StartDate")
            Lude.<*> (x Lude..?> "CreatedDate")
            Lude.<*> (x Lude..?> "Name")
            Lude.<*> (x Lude..?> "ModifiedDate")
            Lude.<*> (x Lude..?> "Cutoff")
            Lude.<*> (x Lude..?> "AllowUnassociatedTargets")
            Lude.<*> (x Lude..?> "Description")
            Lude.<*> (x Lude..?> "Duration")
            Lude.<*> (x Lude..?> "WindowId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetMaintenanceWindow where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.GetMaintenanceWindow" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetMaintenanceWindow where
  toJSON GetMaintenanceWindow' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("WindowId" Lude..= windowId)])

instance Lude.ToPath GetMaintenanceWindow where
  toPath = Lude.const "/"

instance Lude.ToQuery GetMaintenanceWindow where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetMaintenanceWindowResponse' smart constructor.
data GetMaintenanceWindowResponse = GetMaintenanceWindowResponse'
  { -- | Indicates whether the maintenance window is enabled.
    enabled :: Lude.Maybe Lude.Bool,
    -- | The schedule of the maintenance window in the form of a cron or rate expression.
    schedule :: Lude.Maybe Lude.Text,
    -- | The next time the maintenance window will actually run, taking into account any specified times for the maintenance window to become active or inactive.
    nextExecutionTime :: Lude.Maybe Lude.Text,
    -- | The number of days to wait to run a maintenance window after the scheduled CRON expression date and time.
    scheduleOffset :: Lude.Maybe Lude.Natural,
    -- | The date and time, in ISO-8601 Extended format, for when the maintenance window is scheduled to become inactive. The maintenance window will not run after this specified time.
    endDate :: Lude.Maybe Lude.Text,
    -- | The time zone that the scheduled maintenance window executions are based on, in Internet Assigned Numbers Authority (IANA) format. For example: "America/Los_Angeles", "etc/UTC", or "Asia/Seoul". For more information, see the <https://www.iana.org/time-zones Time Zone Database> on the IANA website.
    scheduleTimezone :: Lude.Maybe Lude.Text,
    -- | The date and time, in ISO-8601 Extended format, for when the maintenance window is scheduled to become active. The maintenance window will not run before this specified time.
    startDate :: Lude.Maybe Lude.Text,
    -- | The date the maintenance window was created.
    createdDate :: Lude.Maybe Lude.Timestamp,
    -- | The name of the maintenance window.
    name :: Lude.Maybe Lude.Text,
    -- | The date the maintenance window was last modified.
    modifiedDate :: Lude.Maybe Lude.Timestamp,
    -- | The number of hours before the end of the maintenance window that Systems Manager stops scheduling new tasks for execution.
    cutoff :: Lude.Maybe Lude.Natural,
    -- | Whether targets must be registered with the maintenance window before tasks can be defined for those targets.
    allowUnassociatedTargets :: Lude.Maybe Lude.Bool,
    -- | The description of the maintenance window.
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

-- | Creates a value of 'GetMaintenanceWindowResponse' with the minimum fields required to make a request.
--
-- * 'enabled' - Indicates whether the maintenance window is enabled.
-- * 'schedule' - The schedule of the maintenance window in the form of a cron or rate expression.
-- * 'nextExecutionTime' - The next time the maintenance window will actually run, taking into account any specified times for the maintenance window to become active or inactive.
-- * 'scheduleOffset' - The number of days to wait to run a maintenance window after the scheduled CRON expression date and time.
-- * 'endDate' - The date and time, in ISO-8601 Extended format, for when the maintenance window is scheduled to become inactive. The maintenance window will not run after this specified time.
-- * 'scheduleTimezone' - The time zone that the scheduled maintenance window executions are based on, in Internet Assigned Numbers Authority (IANA) format. For example: "America/Los_Angeles", "etc/UTC", or "Asia/Seoul". For more information, see the <https://www.iana.org/time-zones Time Zone Database> on the IANA website.
-- * 'startDate' - The date and time, in ISO-8601 Extended format, for when the maintenance window is scheduled to become active. The maintenance window will not run before this specified time.
-- * 'createdDate' - The date the maintenance window was created.
-- * 'name' - The name of the maintenance window.
-- * 'modifiedDate' - The date the maintenance window was last modified.
-- * 'cutoff' - The number of hours before the end of the maintenance window that Systems Manager stops scheduling new tasks for execution.
-- * 'allowUnassociatedTargets' - Whether targets must be registered with the maintenance window before tasks can be defined for those targets.
-- * 'description' - The description of the maintenance window.
-- * 'duration' - The duration of the maintenance window in hours.
-- * 'windowId' - The ID of the created maintenance window.
-- * 'responseStatus' - The response status code.
mkGetMaintenanceWindowResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetMaintenanceWindowResponse
mkGetMaintenanceWindowResponse pResponseStatus_ =
  GetMaintenanceWindowResponse'
    { enabled = Lude.Nothing,
      schedule = Lude.Nothing,
      nextExecutionTime = Lude.Nothing,
      scheduleOffset = Lude.Nothing,
      endDate = Lude.Nothing,
      scheduleTimezone = Lude.Nothing,
      startDate = Lude.Nothing,
      createdDate = Lude.Nothing,
      name = Lude.Nothing,
      modifiedDate = Lude.Nothing,
      cutoff = Lude.Nothing,
      allowUnassociatedTargets = Lude.Nothing,
      description = Lude.Nothing,
      duration = Lude.Nothing,
      windowId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Indicates whether the maintenance window is enabled.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwrsEnabled :: Lens.Lens' GetMaintenanceWindowResponse (Lude.Maybe Lude.Bool)
gmwrsEnabled = Lens.lens (enabled :: GetMaintenanceWindowResponse -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: GetMaintenanceWindowResponse)
{-# DEPRECATED gmwrsEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The schedule of the maintenance window in the form of a cron or rate expression.
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwrsSchedule :: Lens.Lens' GetMaintenanceWindowResponse (Lude.Maybe Lude.Text)
gmwrsSchedule = Lens.lens (schedule :: GetMaintenanceWindowResponse -> Lude.Maybe Lude.Text) (\s a -> s {schedule = a} :: GetMaintenanceWindowResponse)
{-# DEPRECATED gmwrsSchedule "Use generic-lens or generic-optics with 'schedule' instead." #-}

-- | The next time the maintenance window will actually run, taking into account any specified times for the maintenance window to become active or inactive.
--
-- /Note:/ Consider using 'nextExecutionTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwrsNextExecutionTime :: Lens.Lens' GetMaintenanceWindowResponse (Lude.Maybe Lude.Text)
gmwrsNextExecutionTime = Lens.lens (nextExecutionTime :: GetMaintenanceWindowResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextExecutionTime = a} :: GetMaintenanceWindowResponse)
{-# DEPRECATED gmwrsNextExecutionTime "Use generic-lens or generic-optics with 'nextExecutionTime' instead." #-}

-- | The number of days to wait to run a maintenance window after the scheduled CRON expression date and time.
--
-- /Note:/ Consider using 'scheduleOffset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwrsScheduleOffset :: Lens.Lens' GetMaintenanceWindowResponse (Lude.Maybe Lude.Natural)
gmwrsScheduleOffset = Lens.lens (scheduleOffset :: GetMaintenanceWindowResponse -> Lude.Maybe Lude.Natural) (\s a -> s {scheduleOffset = a} :: GetMaintenanceWindowResponse)
{-# DEPRECATED gmwrsScheduleOffset "Use generic-lens or generic-optics with 'scheduleOffset' instead." #-}

-- | The date and time, in ISO-8601 Extended format, for when the maintenance window is scheduled to become inactive. The maintenance window will not run after this specified time.
--
-- /Note:/ Consider using 'endDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwrsEndDate :: Lens.Lens' GetMaintenanceWindowResponse (Lude.Maybe Lude.Text)
gmwrsEndDate = Lens.lens (endDate :: GetMaintenanceWindowResponse -> Lude.Maybe Lude.Text) (\s a -> s {endDate = a} :: GetMaintenanceWindowResponse)
{-# DEPRECATED gmwrsEndDate "Use generic-lens or generic-optics with 'endDate' instead." #-}

-- | The time zone that the scheduled maintenance window executions are based on, in Internet Assigned Numbers Authority (IANA) format. For example: "America/Los_Angeles", "etc/UTC", or "Asia/Seoul". For more information, see the <https://www.iana.org/time-zones Time Zone Database> on the IANA website.
--
-- /Note:/ Consider using 'scheduleTimezone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwrsScheduleTimezone :: Lens.Lens' GetMaintenanceWindowResponse (Lude.Maybe Lude.Text)
gmwrsScheduleTimezone = Lens.lens (scheduleTimezone :: GetMaintenanceWindowResponse -> Lude.Maybe Lude.Text) (\s a -> s {scheduleTimezone = a} :: GetMaintenanceWindowResponse)
{-# DEPRECATED gmwrsScheduleTimezone "Use generic-lens or generic-optics with 'scheduleTimezone' instead." #-}

-- | The date and time, in ISO-8601 Extended format, for when the maintenance window is scheduled to become active. The maintenance window will not run before this specified time.
--
-- /Note:/ Consider using 'startDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwrsStartDate :: Lens.Lens' GetMaintenanceWindowResponse (Lude.Maybe Lude.Text)
gmwrsStartDate = Lens.lens (startDate :: GetMaintenanceWindowResponse -> Lude.Maybe Lude.Text) (\s a -> s {startDate = a} :: GetMaintenanceWindowResponse)
{-# DEPRECATED gmwrsStartDate "Use generic-lens or generic-optics with 'startDate' instead." #-}

-- | The date the maintenance window was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwrsCreatedDate :: Lens.Lens' GetMaintenanceWindowResponse (Lude.Maybe Lude.Timestamp)
gmwrsCreatedDate = Lens.lens (createdDate :: GetMaintenanceWindowResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdDate = a} :: GetMaintenanceWindowResponse)
{-# DEPRECATED gmwrsCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | The name of the maintenance window.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwrsName :: Lens.Lens' GetMaintenanceWindowResponse (Lude.Maybe Lude.Text)
gmwrsName = Lens.lens (name :: GetMaintenanceWindowResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: GetMaintenanceWindowResponse)
{-# DEPRECATED gmwrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The date the maintenance window was last modified.
--
-- /Note:/ Consider using 'modifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwrsModifiedDate :: Lens.Lens' GetMaintenanceWindowResponse (Lude.Maybe Lude.Timestamp)
gmwrsModifiedDate = Lens.lens (modifiedDate :: GetMaintenanceWindowResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {modifiedDate = a} :: GetMaintenanceWindowResponse)
{-# DEPRECATED gmwrsModifiedDate "Use generic-lens or generic-optics with 'modifiedDate' instead." #-}

-- | The number of hours before the end of the maintenance window that Systems Manager stops scheduling new tasks for execution.
--
-- /Note:/ Consider using 'cutoff' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwrsCutoff :: Lens.Lens' GetMaintenanceWindowResponse (Lude.Maybe Lude.Natural)
gmwrsCutoff = Lens.lens (cutoff :: GetMaintenanceWindowResponse -> Lude.Maybe Lude.Natural) (\s a -> s {cutoff = a} :: GetMaintenanceWindowResponse)
{-# DEPRECATED gmwrsCutoff "Use generic-lens or generic-optics with 'cutoff' instead." #-}

-- | Whether targets must be registered with the maintenance window before tasks can be defined for those targets.
--
-- /Note:/ Consider using 'allowUnassociatedTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwrsAllowUnassociatedTargets :: Lens.Lens' GetMaintenanceWindowResponse (Lude.Maybe Lude.Bool)
gmwrsAllowUnassociatedTargets = Lens.lens (allowUnassociatedTargets :: GetMaintenanceWindowResponse -> Lude.Maybe Lude.Bool) (\s a -> s {allowUnassociatedTargets = a} :: GetMaintenanceWindowResponse)
{-# DEPRECATED gmwrsAllowUnassociatedTargets "Use generic-lens or generic-optics with 'allowUnassociatedTargets' instead." #-}

-- | The description of the maintenance window.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwrsDescription :: Lens.Lens' GetMaintenanceWindowResponse (Lude.Maybe (Lude.Sensitive Lude.Text))
gmwrsDescription = Lens.lens (description :: GetMaintenanceWindowResponse -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {description = a} :: GetMaintenanceWindowResponse)
{-# DEPRECATED gmwrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The duration of the maintenance window in hours.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwrsDuration :: Lens.Lens' GetMaintenanceWindowResponse (Lude.Maybe Lude.Natural)
gmwrsDuration = Lens.lens (duration :: GetMaintenanceWindowResponse -> Lude.Maybe Lude.Natural) (\s a -> s {duration = a} :: GetMaintenanceWindowResponse)
{-# DEPRECATED gmwrsDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

-- | The ID of the created maintenance window.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwrsWindowId :: Lens.Lens' GetMaintenanceWindowResponse (Lude.Maybe Lude.Text)
gmwrsWindowId = Lens.lens (windowId :: GetMaintenanceWindowResponse -> Lude.Maybe Lude.Text) (\s a -> s {windowId = a} :: GetMaintenanceWindowResponse)
{-# DEPRECATED gmwrsWindowId "Use generic-lens or generic-optics with 'windowId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwrsResponseStatus :: Lens.Lens' GetMaintenanceWindowResponse Lude.Int
gmwrsResponseStatus = Lens.lens (responseStatus :: GetMaintenanceWindowResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetMaintenanceWindowResponse)
{-# DEPRECATED gmwrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
