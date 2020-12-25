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
    gmwrrsAllowUnassociatedTargets,
    gmwrrsCreatedDate,
    gmwrrsCutoff,
    gmwrrsDescription,
    gmwrrsDuration,
    gmwrrsEnabled,
    gmwrrsEndDate,
    gmwrrsModifiedDate,
    gmwrrsName,
    gmwrrsNextExecutionTime,
    gmwrrsSchedule,
    gmwrrsScheduleOffset,
    gmwrrsScheduleTimezone,
    gmwrrsStartDate,
    gmwrrsWindowId,
    gmwrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkGetMaintenanceWindow' smart constructor.
newtype GetMaintenanceWindow = GetMaintenanceWindow'
  { -- | The ID of the maintenance window for which you want to retrieve information.
    windowId :: Types.WindowId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetMaintenanceWindow' value with any optional fields omitted.
mkGetMaintenanceWindow ::
  -- | 'windowId'
  Types.WindowId ->
  GetMaintenanceWindow
mkGetMaintenanceWindow windowId = GetMaintenanceWindow' {windowId}

-- | The ID of the maintenance window for which you want to retrieve information.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwWindowId :: Lens.Lens' GetMaintenanceWindow Types.WindowId
gmwWindowId = Lens.field @"windowId"
{-# DEPRECATED gmwWindowId "Use generic-lens or generic-optics with 'windowId' instead." #-}

instance Core.FromJSON GetMaintenanceWindow where
  toJSON GetMaintenanceWindow {..} =
    Core.object
      (Core.catMaybes [Core.Just ("WindowId" Core..= windowId)])

instance Core.AWSRequest GetMaintenanceWindow where
  type Rs GetMaintenanceWindow = GetMaintenanceWindowResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonSSM.GetMaintenanceWindow")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMaintenanceWindowResponse'
            Core.<$> (x Core..:? "AllowUnassociatedTargets")
            Core.<*> (x Core..:? "CreatedDate")
            Core.<*> (x Core..:? "Cutoff")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "Duration")
            Core.<*> (x Core..:? "Enabled")
            Core.<*> (x Core..:? "EndDate")
            Core.<*> (x Core..:? "ModifiedDate")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "NextExecutionTime")
            Core.<*> (x Core..:? "Schedule")
            Core.<*> (x Core..:? "ScheduleOffset")
            Core.<*> (x Core..:? "ScheduleTimezone")
            Core.<*> (x Core..:? "StartDate")
            Core.<*> (x Core..:? "WindowId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetMaintenanceWindowResponse' smart constructor.
data GetMaintenanceWindowResponse = GetMaintenanceWindowResponse'
  { -- | Whether targets must be registered with the maintenance window before tasks can be defined for those targets.
    allowUnassociatedTargets :: Core.Maybe Core.Bool,
    -- | The date the maintenance window was created.
    createdDate :: Core.Maybe Core.NominalDiffTime,
    -- | The number of hours before the end of the maintenance window that Systems Manager stops scheduling new tasks for execution.
    cutoff :: Core.Maybe Core.Natural,
    -- | The description of the maintenance window.
    description :: Core.Maybe Types.Description,
    -- | The duration of the maintenance window in hours.
    duration :: Core.Maybe Core.Natural,
    -- | Indicates whether the maintenance window is enabled.
    enabled :: Core.Maybe Core.Bool,
    -- | The date and time, in ISO-8601 Extended format, for when the maintenance window is scheduled to become inactive. The maintenance window will not run after this specified time.
    endDate :: Core.Maybe Types.EndDate,
    -- | The date the maintenance window was last modified.
    modifiedDate :: Core.Maybe Core.NominalDiffTime,
    -- | The name of the maintenance window.
    name :: Core.Maybe Types.MaintenanceWindowName,
    -- | The next time the maintenance window will actually run, taking into account any specified times for the maintenance window to become active or inactive.
    nextExecutionTime :: Core.Maybe Types.NextExecutionTime,
    -- | The schedule of the maintenance window in the form of a cron or rate expression.
    schedule :: Core.Maybe Types.MaintenanceWindowSchedule,
    -- | The number of days to wait to run a maintenance window after the scheduled CRON expression date and time.
    scheduleOffset :: Core.Maybe Core.Natural,
    -- | The time zone that the scheduled maintenance window executions are based on, in Internet Assigned Numbers Authority (IANA) format. For example: "America/Los_Angeles", "etc/UTC", or "Asia/Seoul". For more information, see the <https://www.iana.org/time-zones Time Zone Database> on the IANA website.
    scheduleTimezone :: Core.Maybe Types.ScheduleTimezone,
    -- | The date and time, in ISO-8601 Extended format, for when the maintenance window is scheduled to become active. The maintenance window will not run before this specified time.
    startDate :: Core.Maybe Types.StartDate,
    -- | The ID of the created maintenance window.
    windowId :: Core.Maybe Types.WindowId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetMaintenanceWindowResponse' value with any optional fields omitted.
mkGetMaintenanceWindowResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetMaintenanceWindowResponse
mkGetMaintenanceWindowResponse responseStatus =
  GetMaintenanceWindowResponse'
    { allowUnassociatedTargets =
        Core.Nothing,
      createdDate = Core.Nothing,
      cutoff = Core.Nothing,
      description = Core.Nothing,
      duration = Core.Nothing,
      enabled = Core.Nothing,
      endDate = Core.Nothing,
      modifiedDate = Core.Nothing,
      name = Core.Nothing,
      nextExecutionTime = Core.Nothing,
      schedule = Core.Nothing,
      scheduleOffset = Core.Nothing,
      scheduleTimezone = Core.Nothing,
      startDate = Core.Nothing,
      windowId = Core.Nothing,
      responseStatus
    }

-- | Whether targets must be registered with the maintenance window before tasks can be defined for those targets.
--
-- /Note:/ Consider using 'allowUnassociatedTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwrrsAllowUnassociatedTargets :: Lens.Lens' GetMaintenanceWindowResponse (Core.Maybe Core.Bool)
gmwrrsAllowUnassociatedTargets = Lens.field @"allowUnassociatedTargets"
{-# DEPRECATED gmwrrsAllowUnassociatedTargets "Use generic-lens or generic-optics with 'allowUnassociatedTargets' instead." #-}

-- | The date the maintenance window was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwrrsCreatedDate :: Lens.Lens' GetMaintenanceWindowResponse (Core.Maybe Core.NominalDiffTime)
gmwrrsCreatedDate = Lens.field @"createdDate"
{-# DEPRECATED gmwrrsCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | The number of hours before the end of the maintenance window that Systems Manager stops scheduling new tasks for execution.
--
-- /Note:/ Consider using 'cutoff' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwrrsCutoff :: Lens.Lens' GetMaintenanceWindowResponse (Core.Maybe Core.Natural)
gmwrrsCutoff = Lens.field @"cutoff"
{-# DEPRECATED gmwrrsCutoff "Use generic-lens or generic-optics with 'cutoff' instead." #-}

-- | The description of the maintenance window.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwrrsDescription :: Lens.Lens' GetMaintenanceWindowResponse (Core.Maybe Types.Description)
gmwrrsDescription = Lens.field @"description"
{-# DEPRECATED gmwrrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The duration of the maintenance window in hours.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwrrsDuration :: Lens.Lens' GetMaintenanceWindowResponse (Core.Maybe Core.Natural)
gmwrrsDuration = Lens.field @"duration"
{-# DEPRECATED gmwrrsDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

-- | Indicates whether the maintenance window is enabled.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwrrsEnabled :: Lens.Lens' GetMaintenanceWindowResponse (Core.Maybe Core.Bool)
gmwrrsEnabled = Lens.field @"enabled"
{-# DEPRECATED gmwrrsEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The date and time, in ISO-8601 Extended format, for when the maintenance window is scheduled to become inactive. The maintenance window will not run after this specified time.
--
-- /Note:/ Consider using 'endDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwrrsEndDate :: Lens.Lens' GetMaintenanceWindowResponse (Core.Maybe Types.EndDate)
gmwrrsEndDate = Lens.field @"endDate"
{-# DEPRECATED gmwrrsEndDate "Use generic-lens or generic-optics with 'endDate' instead." #-}

-- | The date the maintenance window was last modified.
--
-- /Note:/ Consider using 'modifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwrrsModifiedDate :: Lens.Lens' GetMaintenanceWindowResponse (Core.Maybe Core.NominalDiffTime)
gmwrrsModifiedDate = Lens.field @"modifiedDate"
{-# DEPRECATED gmwrrsModifiedDate "Use generic-lens or generic-optics with 'modifiedDate' instead." #-}

-- | The name of the maintenance window.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwrrsName :: Lens.Lens' GetMaintenanceWindowResponse (Core.Maybe Types.MaintenanceWindowName)
gmwrrsName = Lens.field @"name"
{-# DEPRECATED gmwrrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The next time the maintenance window will actually run, taking into account any specified times for the maintenance window to become active or inactive.
--
-- /Note:/ Consider using 'nextExecutionTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwrrsNextExecutionTime :: Lens.Lens' GetMaintenanceWindowResponse (Core.Maybe Types.NextExecutionTime)
gmwrrsNextExecutionTime = Lens.field @"nextExecutionTime"
{-# DEPRECATED gmwrrsNextExecutionTime "Use generic-lens or generic-optics with 'nextExecutionTime' instead." #-}

-- | The schedule of the maintenance window in the form of a cron or rate expression.
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwrrsSchedule :: Lens.Lens' GetMaintenanceWindowResponse (Core.Maybe Types.MaintenanceWindowSchedule)
gmwrrsSchedule = Lens.field @"schedule"
{-# DEPRECATED gmwrrsSchedule "Use generic-lens or generic-optics with 'schedule' instead." #-}

-- | The number of days to wait to run a maintenance window after the scheduled CRON expression date and time.
--
-- /Note:/ Consider using 'scheduleOffset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwrrsScheduleOffset :: Lens.Lens' GetMaintenanceWindowResponse (Core.Maybe Core.Natural)
gmwrrsScheduleOffset = Lens.field @"scheduleOffset"
{-# DEPRECATED gmwrrsScheduleOffset "Use generic-lens or generic-optics with 'scheduleOffset' instead." #-}

-- | The time zone that the scheduled maintenance window executions are based on, in Internet Assigned Numbers Authority (IANA) format. For example: "America/Los_Angeles", "etc/UTC", or "Asia/Seoul". For more information, see the <https://www.iana.org/time-zones Time Zone Database> on the IANA website.
--
-- /Note:/ Consider using 'scheduleTimezone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwrrsScheduleTimezone :: Lens.Lens' GetMaintenanceWindowResponse (Core.Maybe Types.ScheduleTimezone)
gmwrrsScheduleTimezone = Lens.field @"scheduleTimezone"
{-# DEPRECATED gmwrrsScheduleTimezone "Use generic-lens or generic-optics with 'scheduleTimezone' instead." #-}

-- | The date and time, in ISO-8601 Extended format, for when the maintenance window is scheduled to become active. The maintenance window will not run before this specified time.
--
-- /Note:/ Consider using 'startDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwrrsStartDate :: Lens.Lens' GetMaintenanceWindowResponse (Core.Maybe Types.StartDate)
gmwrrsStartDate = Lens.field @"startDate"
{-# DEPRECATED gmwrrsStartDate "Use generic-lens or generic-optics with 'startDate' instead." #-}

-- | The ID of the created maintenance window.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwrrsWindowId :: Lens.Lens' GetMaintenanceWindowResponse (Core.Maybe Types.WindowId)
gmwrrsWindowId = Lens.field @"windowId"
{-# DEPRECATED gmwrrsWindowId "Use generic-lens or generic-optics with 'windowId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwrrsResponseStatus :: Lens.Lens' GetMaintenanceWindowResponse Core.Int
gmwrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gmwrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
