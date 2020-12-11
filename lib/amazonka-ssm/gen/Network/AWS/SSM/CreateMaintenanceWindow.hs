{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.CreateMaintenanceWindow
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new maintenance window.
module Network.AWS.SSM.CreateMaintenanceWindow
  ( -- * Creating a request
    CreateMaintenanceWindow (..),
    mkCreateMaintenanceWindow,

    -- ** Request lenses
    cmwClientToken,
    cmwScheduleOffset,
    cmwEndDate,
    cmwScheduleTimezone,
    cmwStartDate,
    cmwDescription,
    cmwTags,
    cmwName,
    cmwSchedule,
    cmwDuration,
    cmwCutoff,
    cmwAllowUnassociatedTargets,

    -- * Destructuring the response
    CreateMaintenanceWindowResponse (..),
    mkCreateMaintenanceWindowResponse,

    -- ** Response lenses
    cmwrsWindowId,
    cmwrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkCreateMaintenanceWindow' smart constructor.
data CreateMaintenanceWindow = CreateMaintenanceWindow'
  { clientToken ::
      Lude.Maybe Lude.Text,
    scheduleOffset :: Lude.Maybe Lude.Natural,
    endDate :: Lude.Maybe Lude.Text,
    scheduleTimezone :: Lude.Maybe Lude.Text,
    startDate :: Lude.Maybe Lude.Text,
    description ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    tags :: Lude.Maybe [Tag],
    name :: Lude.Text,
    schedule :: Lude.Text,
    duration :: Lude.Natural,
    cutoff :: Lude.Natural,
    allowUnassociatedTargets :: Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateMaintenanceWindow' with the minimum fields required to make a request.
--
-- * 'allowUnassociatedTargets' - Enables a maintenance window task to run on managed instances, even if you have not registered those instances as targets. If enabled, then you must specify the unregistered instances (by instance ID) when you register a task with the maintenance window.
--
-- If you don't enable this option, then you must specify previously-registered targets when you register a task with the maintenance window.
-- * 'clientToken' - User-provided idempotency token.
-- * 'cutoff' - The number of hours before the end of the maintenance window that Systems Manager stops scheduling new tasks for execution.
-- * 'description' - An optional description for the maintenance window. We recommend specifying a description to help you organize your maintenance windows.
-- * 'duration' - The duration of the maintenance window in hours.
-- * 'endDate' - The date and time, in ISO-8601 Extended format, for when you want the maintenance window to become inactive. EndDate allows you to set a date and time in the future when the maintenance window will no longer run.
-- * 'name' - The name of the maintenance window.
-- * 'schedule' - The schedule of the maintenance window in the form of a cron or rate expression.
-- * 'scheduleOffset' - The number of days to wait after the date and time specified by a CRON expression before running the maintenance window.
--
-- For example, the following cron expression schedules a maintenance window to run on the third Tuesday of every month at 11:30 PM.
-- @cron(0 30 23 ? * TUE#3 *)@
-- If the schedule offset is @2@ , the maintenance window won't run until two days later.
-- * 'scheduleTimezone' - The time zone that the scheduled maintenance window executions are based on, in Internet Assigned Numbers Authority (IANA) format. For example: "America/Los_Angeles", "etc/UTC", or "Asia/Seoul". For more information, see the <https://www.iana.org/time-zones Time Zone Database> on the IANA website.
-- * 'startDate' - The date and time, in ISO-8601 Extended format, for when you want the maintenance window to become active. StartDate allows you to delay activation of the maintenance window until the specified future date.
-- * 'tags' - Optional metadata that you assign to a resource. Tags enable you to categorize a resource in different ways, such as by purpose, owner, or environment. For example, you might want to tag a maintenance window to identify the type of tasks it will run, the types of targets, and the environment it will run in. In this case, you could specify the following key name/value pairs:
--
--
--     * @Key=TaskType,Value=AgentUpdate@
--
--
--     * @Key=OS,Value=Windows@
--
--
--     * @Key=Environment,Value=Production@
mkCreateMaintenanceWindow ::
  -- | 'name'
  Lude.Text ->
  -- | 'schedule'
  Lude.Text ->
  -- | 'duration'
  Lude.Natural ->
  -- | 'cutoff'
  Lude.Natural ->
  -- | 'allowUnassociatedTargets'
  Lude.Bool ->
  CreateMaintenanceWindow
mkCreateMaintenanceWindow
  pName_
  pSchedule_
  pDuration_
  pCutoff_
  pAllowUnassociatedTargets_ =
    CreateMaintenanceWindow'
      { clientToken = Lude.Nothing,
        scheduleOffset = Lude.Nothing,
        endDate = Lude.Nothing,
        scheduleTimezone = Lude.Nothing,
        startDate = Lude.Nothing,
        description = Lude.Nothing,
        tags = Lude.Nothing,
        name = pName_,
        schedule = pSchedule_,
        duration = pDuration_,
        cutoff = pCutoff_,
        allowUnassociatedTargets = pAllowUnassociatedTargets_
      }

-- | User-provided idempotency token.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmwClientToken :: Lens.Lens' CreateMaintenanceWindow (Lude.Maybe Lude.Text)
cmwClientToken = Lens.lens (clientToken :: CreateMaintenanceWindow -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: CreateMaintenanceWindow)
{-# DEPRECATED cmwClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The number of days to wait after the date and time specified by a CRON expression before running the maintenance window.
--
-- For example, the following cron expression schedules a maintenance window to run on the third Tuesday of every month at 11:30 PM.
-- @cron(0 30 23 ? * TUE#3 *)@
-- If the schedule offset is @2@ , the maintenance window won't run until two days later.
--
-- /Note:/ Consider using 'scheduleOffset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmwScheduleOffset :: Lens.Lens' CreateMaintenanceWindow (Lude.Maybe Lude.Natural)
cmwScheduleOffset = Lens.lens (scheduleOffset :: CreateMaintenanceWindow -> Lude.Maybe Lude.Natural) (\s a -> s {scheduleOffset = a} :: CreateMaintenanceWindow)
{-# DEPRECATED cmwScheduleOffset "Use generic-lens or generic-optics with 'scheduleOffset' instead." #-}

-- | The date and time, in ISO-8601 Extended format, for when you want the maintenance window to become inactive. EndDate allows you to set a date and time in the future when the maintenance window will no longer run.
--
-- /Note:/ Consider using 'endDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmwEndDate :: Lens.Lens' CreateMaintenanceWindow (Lude.Maybe Lude.Text)
cmwEndDate = Lens.lens (endDate :: CreateMaintenanceWindow -> Lude.Maybe Lude.Text) (\s a -> s {endDate = a} :: CreateMaintenanceWindow)
{-# DEPRECATED cmwEndDate "Use generic-lens or generic-optics with 'endDate' instead." #-}

-- | The time zone that the scheduled maintenance window executions are based on, in Internet Assigned Numbers Authority (IANA) format. For example: "America/Los_Angeles", "etc/UTC", or "Asia/Seoul". For more information, see the <https://www.iana.org/time-zones Time Zone Database> on the IANA website.
--
-- /Note:/ Consider using 'scheduleTimezone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmwScheduleTimezone :: Lens.Lens' CreateMaintenanceWindow (Lude.Maybe Lude.Text)
cmwScheduleTimezone = Lens.lens (scheduleTimezone :: CreateMaintenanceWindow -> Lude.Maybe Lude.Text) (\s a -> s {scheduleTimezone = a} :: CreateMaintenanceWindow)
{-# DEPRECATED cmwScheduleTimezone "Use generic-lens or generic-optics with 'scheduleTimezone' instead." #-}

-- | The date and time, in ISO-8601 Extended format, for when you want the maintenance window to become active. StartDate allows you to delay activation of the maintenance window until the specified future date.
--
-- /Note:/ Consider using 'startDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmwStartDate :: Lens.Lens' CreateMaintenanceWindow (Lude.Maybe Lude.Text)
cmwStartDate = Lens.lens (startDate :: CreateMaintenanceWindow -> Lude.Maybe Lude.Text) (\s a -> s {startDate = a} :: CreateMaintenanceWindow)
{-# DEPRECATED cmwStartDate "Use generic-lens or generic-optics with 'startDate' instead." #-}

-- | An optional description for the maintenance window. We recommend specifying a description to help you organize your maintenance windows.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmwDescription :: Lens.Lens' CreateMaintenanceWindow (Lude.Maybe (Lude.Sensitive Lude.Text))
cmwDescription = Lens.lens (description :: CreateMaintenanceWindow -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {description = a} :: CreateMaintenanceWindow)
{-# DEPRECATED cmwDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Optional metadata that you assign to a resource. Tags enable you to categorize a resource in different ways, such as by purpose, owner, or environment. For example, you might want to tag a maintenance window to identify the type of tasks it will run, the types of targets, and the environment it will run in. In this case, you could specify the following key name/value pairs:
--
--
--     * @Key=TaskType,Value=AgentUpdate@
--
--
--     * @Key=OS,Value=Windows@
--
--
--     * @Key=Environment,Value=Production@
--
--
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmwTags :: Lens.Lens' CreateMaintenanceWindow (Lude.Maybe [Tag])
cmwTags = Lens.lens (tags :: CreateMaintenanceWindow -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateMaintenanceWindow)
{-# DEPRECATED cmwTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name of the maintenance window.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmwName :: Lens.Lens' CreateMaintenanceWindow Lude.Text
cmwName = Lens.lens (name :: CreateMaintenanceWindow -> Lude.Text) (\s a -> s {name = a} :: CreateMaintenanceWindow)
{-# DEPRECATED cmwName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The schedule of the maintenance window in the form of a cron or rate expression.
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmwSchedule :: Lens.Lens' CreateMaintenanceWindow Lude.Text
cmwSchedule = Lens.lens (schedule :: CreateMaintenanceWindow -> Lude.Text) (\s a -> s {schedule = a} :: CreateMaintenanceWindow)
{-# DEPRECATED cmwSchedule "Use generic-lens or generic-optics with 'schedule' instead." #-}

-- | The duration of the maintenance window in hours.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmwDuration :: Lens.Lens' CreateMaintenanceWindow Lude.Natural
cmwDuration = Lens.lens (duration :: CreateMaintenanceWindow -> Lude.Natural) (\s a -> s {duration = a} :: CreateMaintenanceWindow)
{-# DEPRECATED cmwDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

-- | The number of hours before the end of the maintenance window that Systems Manager stops scheduling new tasks for execution.
--
-- /Note:/ Consider using 'cutoff' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmwCutoff :: Lens.Lens' CreateMaintenanceWindow Lude.Natural
cmwCutoff = Lens.lens (cutoff :: CreateMaintenanceWindow -> Lude.Natural) (\s a -> s {cutoff = a} :: CreateMaintenanceWindow)
{-# DEPRECATED cmwCutoff "Use generic-lens or generic-optics with 'cutoff' instead." #-}

-- | Enables a maintenance window task to run on managed instances, even if you have not registered those instances as targets. If enabled, then you must specify the unregistered instances (by instance ID) when you register a task with the maintenance window.
--
-- If you don't enable this option, then you must specify previously-registered targets when you register a task with the maintenance window.
--
-- /Note:/ Consider using 'allowUnassociatedTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmwAllowUnassociatedTargets :: Lens.Lens' CreateMaintenanceWindow Lude.Bool
cmwAllowUnassociatedTargets = Lens.lens (allowUnassociatedTargets :: CreateMaintenanceWindow -> Lude.Bool) (\s a -> s {allowUnassociatedTargets = a} :: CreateMaintenanceWindow)
{-# DEPRECATED cmwAllowUnassociatedTargets "Use generic-lens or generic-optics with 'allowUnassociatedTargets' instead." #-}

instance Lude.AWSRequest CreateMaintenanceWindow where
  type Rs CreateMaintenanceWindow = CreateMaintenanceWindowResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateMaintenanceWindowResponse'
            Lude.<$> (x Lude..?> "WindowId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateMaintenanceWindow where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.CreateMaintenanceWindow" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateMaintenanceWindow where
  toJSON CreateMaintenanceWindow' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ClientToken" Lude..=) Lude.<$> clientToken,
            ("ScheduleOffset" Lude..=) Lude.<$> scheduleOffset,
            ("EndDate" Lude..=) Lude.<$> endDate,
            ("ScheduleTimezone" Lude..=) Lude.<$> scheduleTimezone,
            ("StartDate" Lude..=) Lude.<$> startDate,
            ("Description" Lude..=) Lude.<$> description,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("Name" Lude..= name),
            Lude.Just ("Schedule" Lude..= schedule),
            Lude.Just ("Duration" Lude..= duration),
            Lude.Just ("Cutoff" Lude..= cutoff),
            Lude.Just
              ("AllowUnassociatedTargets" Lude..= allowUnassociatedTargets)
          ]
      )

instance Lude.ToPath CreateMaintenanceWindow where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateMaintenanceWindow where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateMaintenanceWindowResponse' smart constructor.
data CreateMaintenanceWindowResponse = CreateMaintenanceWindowResponse'
  { windowId ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateMaintenanceWindowResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'windowId' - The ID of the created maintenance window.
mkCreateMaintenanceWindowResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateMaintenanceWindowResponse
mkCreateMaintenanceWindowResponse pResponseStatus_ =
  CreateMaintenanceWindowResponse'
    { windowId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the created maintenance window.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmwrsWindowId :: Lens.Lens' CreateMaintenanceWindowResponse (Lude.Maybe Lude.Text)
cmwrsWindowId = Lens.lens (windowId :: CreateMaintenanceWindowResponse -> Lude.Maybe Lude.Text) (\s a -> s {windowId = a} :: CreateMaintenanceWindowResponse)
{-# DEPRECATED cmwrsWindowId "Use generic-lens or generic-optics with 'windowId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmwrsResponseStatus :: Lens.Lens' CreateMaintenanceWindowResponse Lude.Int
cmwrsResponseStatus = Lens.lens (responseStatus :: CreateMaintenanceWindowResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateMaintenanceWindowResponse)
{-# DEPRECATED cmwrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
