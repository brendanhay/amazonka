{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetMaintenanceWindow (..)
    , mkGetMaintenanceWindow
    -- ** Request lenses
    , gmwWindowId

    -- * Destructuring the response
    , GetMaintenanceWindowResponse (..)
    , mkGetMaintenanceWindowResponse
    -- ** Response lenses
    , gmwrrsAllowUnassociatedTargets
    , gmwrrsCreatedDate
    , gmwrrsCutoff
    , gmwrrsDescription
    , gmwrrsDuration
    , gmwrrsEnabled
    , gmwrrsEndDate
    , gmwrrsModifiedDate
    , gmwrrsName
    , gmwrrsNextExecutionTime
    , gmwrrsSchedule
    , gmwrrsScheduleOffset
    , gmwrrsScheduleTimezone
    , gmwrrsStartDate
    , gmwrrsWindowId
    , gmwrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkGetMaintenanceWindow' smart constructor.
newtype GetMaintenanceWindow = GetMaintenanceWindow'
  { windowId :: Types.WindowId
    -- ^ The ID of the maintenance window for which you want to retrieve information.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetMaintenanceWindow' value with any optional fields omitted.
mkGetMaintenanceWindow
    :: Types.WindowId -- ^ 'windowId'
    -> GetMaintenanceWindow
mkGetMaintenanceWindow windowId = GetMaintenanceWindow'{windowId}

-- | The ID of the maintenance window for which you want to retrieve information.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwWindowId :: Lens.Lens' GetMaintenanceWindow Types.WindowId
gmwWindowId = Lens.field @"windowId"
{-# INLINEABLE gmwWindowId #-}
{-# DEPRECATED windowId "Use generic-lens or generic-optics with 'windowId' instead"  #-}

instance Core.ToQuery GetMaintenanceWindow where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetMaintenanceWindow where
        toHeaders GetMaintenanceWindow{..}
          = Core.pure ("X-Amz-Target", "AmazonSSM.GetMaintenanceWindow")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetMaintenanceWindow where
        toJSON GetMaintenanceWindow{..}
          = Core.object
              (Core.catMaybes [Core.Just ("WindowId" Core..= windowId)])

instance Core.AWSRequest GetMaintenanceWindow where
        type Rs GetMaintenanceWindow = GetMaintenanceWindowResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetMaintenanceWindowResponse' Core.<$>
                   (x Core..:? "AllowUnassociatedTargets") Core.<*>
                     x Core..:? "CreatedDate"
                     Core.<*> x Core..:? "Cutoff"
                     Core.<*> x Core..:? "Description"
                     Core.<*> x Core..:? "Duration"
                     Core.<*> x Core..:? "Enabled"
                     Core.<*> x Core..:? "EndDate"
                     Core.<*> x Core..:? "ModifiedDate"
                     Core.<*> x Core..:? "Name"
                     Core.<*> x Core..:? "NextExecutionTime"
                     Core.<*> x Core..:? "Schedule"
                     Core.<*> x Core..:? "ScheduleOffset"
                     Core.<*> x Core..:? "ScheduleTimezone"
                     Core.<*> x Core..:? "StartDate"
                     Core.<*> x Core..:? "WindowId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetMaintenanceWindowResponse' smart constructor.
data GetMaintenanceWindowResponse = GetMaintenanceWindowResponse'
  { allowUnassociatedTargets :: Core.Maybe Core.Bool
    -- ^ Whether targets must be registered with the maintenance window before tasks can be defined for those targets.
  , createdDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date the maintenance window was created.
  , cutoff :: Core.Maybe Core.Natural
    -- ^ The number of hours before the end of the maintenance window that Systems Manager stops scheduling new tasks for execution.
  , description :: Core.Maybe Types.Description
    -- ^ The description of the maintenance window.
  , duration :: Core.Maybe Core.Natural
    -- ^ The duration of the maintenance window in hours.
  , enabled :: Core.Maybe Core.Bool
    -- ^ Indicates whether the maintenance window is enabled.
  , endDate :: Core.Maybe Types.EndDate
    -- ^ The date and time, in ISO-8601 Extended format, for when the maintenance window is scheduled to become inactive. The maintenance window will not run after this specified time.
  , modifiedDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date the maintenance window was last modified.
  , name :: Core.Maybe Types.MaintenanceWindowName
    -- ^ The name of the maintenance window.
  , nextExecutionTime :: Core.Maybe Types.NextExecutionTime
    -- ^ The next time the maintenance window will actually run, taking into account any specified times for the maintenance window to become active or inactive.
  , schedule :: Core.Maybe Types.MaintenanceWindowSchedule
    -- ^ The schedule of the maintenance window in the form of a cron or rate expression.
  , scheduleOffset :: Core.Maybe Core.Natural
    -- ^ The number of days to wait to run a maintenance window after the scheduled CRON expression date and time.
  , scheduleTimezone :: Core.Maybe Types.ScheduleTimezone
    -- ^ The time zone that the scheduled maintenance window executions are based on, in Internet Assigned Numbers Authority (IANA) format. For example: "America/Los_Angeles", "etc/UTC", or "Asia/Seoul". For more information, see the <https://www.iana.org/time-zones Time Zone Database> on the IANA website.
  , startDate :: Core.Maybe Types.StartDate
    -- ^ The date and time, in ISO-8601 Extended format, for when the maintenance window is scheduled to become active. The maintenance window will not run before this specified time.
  , windowId :: Core.Maybe Types.WindowId
    -- ^ The ID of the created maintenance window.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetMaintenanceWindowResponse' value with any optional fields omitted.
mkGetMaintenanceWindowResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetMaintenanceWindowResponse
mkGetMaintenanceWindowResponse responseStatus
  = GetMaintenanceWindowResponse'{allowUnassociatedTargets =
                                    Core.Nothing,
                                  createdDate = Core.Nothing, cutoff = Core.Nothing,
                                  description = Core.Nothing, duration = Core.Nothing,
                                  enabled = Core.Nothing, endDate = Core.Nothing,
                                  modifiedDate = Core.Nothing, name = Core.Nothing,
                                  nextExecutionTime = Core.Nothing, schedule = Core.Nothing,
                                  scheduleOffset = Core.Nothing, scheduleTimezone = Core.Nothing,
                                  startDate = Core.Nothing, windowId = Core.Nothing, responseStatus}

-- | Whether targets must be registered with the maintenance window before tasks can be defined for those targets.
--
-- /Note:/ Consider using 'allowUnassociatedTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwrrsAllowUnassociatedTargets :: Lens.Lens' GetMaintenanceWindowResponse (Core.Maybe Core.Bool)
gmwrrsAllowUnassociatedTargets = Lens.field @"allowUnassociatedTargets"
{-# INLINEABLE gmwrrsAllowUnassociatedTargets #-}
{-# DEPRECATED allowUnassociatedTargets "Use generic-lens or generic-optics with 'allowUnassociatedTargets' instead"  #-}

-- | The date the maintenance window was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwrrsCreatedDate :: Lens.Lens' GetMaintenanceWindowResponse (Core.Maybe Core.NominalDiffTime)
gmwrrsCreatedDate = Lens.field @"createdDate"
{-# INLINEABLE gmwrrsCreatedDate #-}
{-# DEPRECATED createdDate "Use generic-lens or generic-optics with 'createdDate' instead"  #-}

-- | The number of hours before the end of the maintenance window that Systems Manager stops scheduling new tasks for execution.
--
-- /Note:/ Consider using 'cutoff' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwrrsCutoff :: Lens.Lens' GetMaintenanceWindowResponse (Core.Maybe Core.Natural)
gmwrrsCutoff = Lens.field @"cutoff"
{-# INLINEABLE gmwrrsCutoff #-}
{-# DEPRECATED cutoff "Use generic-lens or generic-optics with 'cutoff' instead"  #-}

-- | The description of the maintenance window.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwrrsDescription :: Lens.Lens' GetMaintenanceWindowResponse (Core.Maybe Types.Description)
gmwrrsDescription = Lens.field @"description"
{-# INLINEABLE gmwrrsDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The duration of the maintenance window in hours.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwrrsDuration :: Lens.Lens' GetMaintenanceWindowResponse (Core.Maybe Core.Natural)
gmwrrsDuration = Lens.field @"duration"
{-# INLINEABLE gmwrrsDuration #-}
{-# DEPRECATED duration "Use generic-lens or generic-optics with 'duration' instead"  #-}

-- | Indicates whether the maintenance window is enabled.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwrrsEnabled :: Lens.Lens' GetMaintenanceWindowResponse (Core.Maybe Core.Bool)
gmwrrsEnabled = Lens.field @"enabled"
{-# INLINEABLE gmwrrsEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

-- | The date and time, in ISO-8601 Extended format, for when the maintenance window is scheduled to become inactive. The maintenance window will not run after this specified time.
--
-- /Note:/ Consider using 'endDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwrrsEndDate :: Lens.Lens' GetMaintenanceWindowResponse (Core.Maybe Types.EndDate)
gmwrrsEndDate = Lens.field @"endDate"
{-# INLINEABLE gmwrrsEndDate #-}
{-# DEPRECATED endDate "Use generic-lens or generic-optics with 'endDate' instead"  #-}

-- | The date the maintenance window was last modified.
--
-- /Note:/ Consider using 'modifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwrrsModifiedDate :: Lens.Lens' GetMaintenanceWindowResponse (Core.Maybe Core.NominalDiffTime)
gmwrrsModifiedDate = Lens.field @"modifiedDate"
{-# INLINEABLE gmwrrsModifiedDate #-}
{-# DEPRECATED modifiedDate "Use generic-lens or generic-optics with 'modifiedDate' instead"  #-}

-- | The name of the maintenance window.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwrrsName :: Lens.Lens' GetMaintenanceWindowResponse (Core.Maybe Types.MaintenanceWindowName)
gmwrrsName = Lens.field @"name"
{-# INLINEABLE gmwrrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The next time the maintenance window will actually run, taking into account any specified times for the maintenance window to become active or inactive.
--
-- /Note:/ Consider using 'nextExecutionTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwrrsNextExecutionTime :: Lens.Lens' GetMaintenanceWindowResponse (Core.Maybe Types.NextExecutionTime)
gmwrrsNextExecutionTime = Lens.field @"nextExecutionTime"
{-# INLINEABLE gmwrrsNextExecutionTime #-}
{-# DEPRECATED nextExecutionTime "Use generic-lens or generic-optics with 'nextExecutionTime' instead"  #-}

-- | The schedule of the maintenance window in the form of a cron or rate expression.
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwrrsSchedule :: Lens.Lens' GetMaintenanceWindowResponse (Core.Maybe Types.MaintenanceWindowSchedule)
gmwrrsSchedule = Lens.field @"schedule"
{-# INLINEABLE gmwrrsSchedule #-}
{-# DEPRECATED schedule "Use generic-lens or generic-optics with 'schedule' instead"  #-}

-- | The number of days to wait to run a maintenance window after the scheduled CRON expression date and time.
--
-- /Note:/ Consider using 'scheduleOffset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwrrsScheduleOffset :: Lens.Lens' GetMaintenanceWindowResponse (Core.Maybe Core.Natural)
gmwrrsScheduleOffset = Lens.field @"scheduleOffset"
{-# INLINEABLE gmwrrsScheduleOffset #-}
{-# DEPRECATED scheduleOffset "Use generic-lens or generic-optics with 'scheduleOffset' instead"  #-}

-- | The time zone that the scheduled maintenance window executions are based on, in Internet Assigned Numbers Authority (IANA) format. For example: "America/Los_Angeles", "etc/UTC", or "Asia/Seoul". For more information, see the <https://www.iana.org/time-zones Time Zone Database> on the IANA website.
--
-- /Note:/ Consider using 'scheduleTimezone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwrrsScheduleTimezone :: Lens.Lens' GetMaintenanceWindowResponse (Core.Maybe Types.ScheduleTimezone)
gmwrrsScheduleTimezone = Lens.field @"scheduleTimezone"
{-# INLINEABLE gmwrrsScheduleTimezone #-}
{-# DEPRECATED scheduleTimezone "Use generic-lens or generic-optics with 'scheduleTimezone' instead"  #-}

-- | The date and time, in ISO-8601 Extended format, for when the maintenance window is scheduled to become active. The maintenance window will not run before this specified time.
--
-- /Note:/ Consider using 'startDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwrrsStartDate :: Lens.Lens' GetMaintenanceWindowResponse (Core.Maybe Types.StartDate)
gmwrrsStartDate = Lens.field @"startDate"
{-# INLINEABLE gmwrrsStartDate #-}
{-# DEPRECATED startDate "Use generic-lens or generic-optics with 'startDate' instead"  #-}

-- | The ID of the created maintenance window.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwrrsWindowId :: Lens.Lens' GetMaintenanceWindowResponse (Core.Maybe Types.WindowId)
gmwrrsWindowId = Lens.field @"windowId"
{-# INLINEABLE gmwrrsWindowId #-}
{-# DEPRECATED windowId "Use generic-lens or generic-optics with 'windowId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmwrrsResponseStatus :: Lens.Lens' GetMaintenanceWindowResponse Core.Int
gmwrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gmwrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
