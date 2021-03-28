{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateMaintenanceWindow (..)
    , mkCreateMaintenanceWindow
    -- ** Request lenses
    , cmwName
    , cmwSchedule
    , cmwDuration
    , cmwCutoff
    , cmwAllowUnassociatedTargets
    , cmwClientToken
    , cmwDescription
    , cmwEndDate
    , cmwScheduleOffset
    , cmwScheduleTimezone
    , cmwStartDate
    , cmwTags

    -- * Destructuring the response
    , CreateMaintenanceWindowResponse (..)
    , mkCreateMaintenanceWindowResponse
    -- ** Response lenses
    , cmwrrsWindowId
    , cmwrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkCreateMaintenanceWindow' smart constructor.
data CreateMaintenanceWindow = CreateMaintenanceWindow'
  { name :: Types.MaintenanceWindowName
    -- ^ The name of the maintenance window.
  , schedule :: Types.MaintenanceWindowSchedule
    -- ^ The schedule of the maintenance window in the form of a cron or rate expression.
  , duration :: Core.Natural
    -- ^ The duration of the maintenance window in hours.
  , cutoff :: Core.Natural
    -- ^ The number of hours before the end of the maintenance window that Systems Manager stops scheduling new tasks for execution.
  , allowUnassociatedTargets :: Core.Bool
    -- ^ Enables a maintenance window task to run on managed instances, even if you have not registered those instances as targets. If enabled, then you must specify the unregistered instances (by instance ID) when you register a task with the maintenance window.
--
-- If you don't enable this option, then you must specify previously-registered targets when you register a task with the maintenance window.
  , clientToken :: Core.Maybe Types.ClientToken
    -- ^ User-provided idempotency token.
  , description :: Core.Maybe Types.Description
    -- ^ An optional description for the maintenance window. We recommend specifying a description to help you organize your maintenance windows. 
  , endDate :: Core.Maybe Types.EndDate
    -- ^ The date and time, in ISO-8601 Extended format, for when you want the maintenance window to become inactive. EndDate allows you to set a date and time in the future when the maintenance window will no longer run.
  , scheduleOffset :: Core.Maybe Core.Natural
    -- ^ The number of days to wait after the date and time specified by a CRON expression before running the maintenance window.
--
-- For example, the following cron expression schedules a maintenance window to run on the third Tuesday of every month at 11:30 PM.
-- @cron(0 30 23 ? * TUE#3 *)@ 
-- If the schedule offset is @2@ , the maintenance window won't run until two days later.
  , scheduleTimezone :: Core.Maybe Types.ScheduleTimezone
    -- ^ The time zone that the scheduled maintenance window executions are based on, in Internet Assigned Numbers Authority (IANA) format. For example: "America/Los_Angeles", "etc/UTC", or "Asia/Seoul". For more information, see the <https://www.iana.org/time-zones Time Zone Database> on the IANA website.
  , startDate :: Core.Maybe Types.StartDate
    -- ^ The date and time, in ISO-8601 Extended format, for when you want the maintenance window to become active. StartDate allows you to delay activation of the maintenance window until the specified future date.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Optional metadata that you assign to a resource. Tags enable you to categorize a resource in different ways, such as by purpose, owner, or environment. For example, you might want to tag a maintenance window to identify the type of tasks it will run, the types of targets, and the environment it will run in. In this case, you could specify the following key name/value pairs:
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateMaintenanceWindow' value with any optional fields omitted.
mkCreateMaintenanceWindow
    :: Types.MaintenanceWindowName -- ^ 'name'
    -> Types.MaintenanceWindowSchedule -- ^ 'schedule'
    -> Core.Natural -- ^ 'duration'
    -> Core.Natural -- ^ 'cutoff'
    -> Core.Bool -- ^ 'allowUnassociatedTargets'
    -> CreateMaintenanceWindow
mkCreateMaintenanceWindow name schedule duration cutoff
  allowUnassociatedTargets
  = CreateMaintenanceWindow'{name, schedule, duration, cutoff,
                             allowUnassociatedTargets, clientToken = Core.Nothing,
                             description = Core.Nothing, endDate = Core.Nothing,
                             scheduleOffset = Core.Nothing, scheduleTimezone = Core.Nothing,
                             startDate = Core.Nothing, tags = Core.Nothing}

-- | The name of the maintenance window.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmwName :: Lens.Lens' CreateMaintenanceWindow Types.MaintenanceWindowName
cmwName = Lens.field @"name"
{-# INLINEABLE cmwName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The schedule of the maintenance window in the form of a cron or rate expression.
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmwSchedule :: Lens.Lens' CreateMaintenanceWindow Types.MaintenanceWindowSchedule
cmwSchedule = Lens.field @"schedule"
{-# INLINEABLE cmwSchedule #-}
{-# DEPRECATED schedule "Use generic-lens or generic-optics with 'schedule' instead"  #-}

-- | The duration of the maintenance window in hours.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmwDuration :: Lens.Lens' CreateMaintenanceWindow Core.Natural
cmwDuration = Lens.field @"duration"
{-# INLINEABLE cmwDuration #-}
{-# DEPRECATED duration "Use generic-lens or generic-optics with 'duration' instead"  #-}

-- | The number of hours before the end of the maintenance window that Systems Manager stops scheduling new tasks for execution.
--
-- /Note:/ Consider using 'cutoff' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmwCutoff :: Lens.Lens' CreateMaintenanceWindow Core.Natural
cmwCutoff = Lens.field @"cutoff"
{-# INLINEABLE cmwCutoff #-}
{-# DEPRECATED cutoff "Use generic-lens or generic-optics with 'cutoff' instead"  #-}

-- | Enables a maintenance window task to run on managed instances, even if you have not registered those instances as targets. If enabled, then you must specify the unregistered instances (by instance ID) when you register a task with the maintenance window.
--
-- If you don't enable this option, then you must specify previously-registered targets when you register a task with the maintenance window.
--
-- /Note:/ Consider using 'allowUnassociatedTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmwAllowUnassociatedTargets :: Lens.Lens' CreateMaintenanceWindow Core.Bool
cmwAllowUnassociatedTargets = Lens.field @"allowUnassociatedTargets"
{-# INLINEABLE cmwAllowUnassociatedTargets #-}
{-# DEPRECATED allowUnassociatedTargets "Use generic-lens or generic-optics with 'allowUnassociatedTargets' instead"  #-}

-- | User-provided idempotency token.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmwClientToken :: Lens.Lens' CreateMaintenanceWindow (Core.Maybe Types.ClientToken)
cmwClientToken = Lens.field @"clientToken"
{-# INLINEABLE cmwClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | An optional description for the maintenance window. We recommend specifying a description to help you organize your maintenance windows. 
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmwDescription :: Lens.Lens' CreateMaintenanceWindow (Core.Maybe Types.Description)
cmwDescription = Lens.field @"description"
{-# INLINEABLE cmwDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The date and time, in ISO-8601 Extended format, for when you want the maintenance window to become inactive. EndDate allows you to set a date and time in the future when the maintenance window will no longer run.
--
-- /Note:/ Consider using 'endDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmwEndDate :: Lens.Lens' CreateMaintenanceWindow (Core.Maybe Types.EndDate)
cmwEndDate = Lens.field @"endDate"
{-# INLINEABLE cmwEndDate #-}
{-# DEPRECATED endDate "Use generic-lens or generic-optics with 'endDate' instead"  #-}

-- | The number of days to wait after the date and time specified by a CRON expression before running the maintenance window.
--
-- For example, the following cron expression schedules a maintenance window to run on the third Tuesday of every month at 11:30 PM.
-- @cron(0 30 23 ? * TUE#3 *)@ 
-- If the schedule offset is @2@ , the maintenance window won't run until two days later.
--
-- /Note:/ Consider using 'scheduleOffset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmwScheduleOffset :: Lens.Lens' CreateMaintenanceWindow (Core.Maybe Core.Natural)
cmwScheduleOffset = Lens.field @"scheduleOffset"
{-# INLINEABLE cmwScheduleOffset #-}
{-# DEPRECATED scheduleOffset "Use generic-lens or generic-optics with 'scheduleOffset' instead"  #-}

-- | The time zone that the scheduled maintenance window executions are based on, in Internet Assigned Numbers Authority (IANA) format. For example: "America/Los_Angeles", "etc/UTC", or "Asia/Seoul". For more information, see the <https://www.iana.org/time-zones Time Zone Database> on the IANA website.
--
-- /Note:/ Consider using 'scheduleTimezone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmwScheduleTimezone :: Lens.Lens' CreateMaintenanceWindow (Core.Maybe Types.ScheduleTimezone)
cmwScheduleTimezone = Lens.field @"scheduleTimezone"
{-# INLINEABLE cmwScheduleTimezone #-}
{-# DEPRECATED scheduleTimezone "Use generic-lens or generic-optics with 'scheduleTimezone' instead"  #-}

-- | The date and time, in ISO-8601 Extended format, for when you want the maintenance window to become active. StartDate allows you to delay activation of the maintenance window until the specified future date.
--
-- /Note:/ Consider using 'startDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmwStartDate :: Lens.Lens' CreateMaintenanceWindow (Core.Maybe Types.StartDate)
cmwStartDate = Lens.field @"startDate"
{-# INLINEABLE cmwStartDate #-}
{-# DEPRECATED startDate "Use generic-lens or generic-optics with 'startDate' instead"  #-}

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
cmwTags :: Lens.Lens' CreateMaintenanceWindow (Core.Maybe [Types.Tag])
cmwTags = Lens.field @"tags"
{-# INLINEABLE cmwTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateMaintenanceWindow where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateMaintenanceWindow where
        toHeaders CreateMaintenanceWindow{..}
          = Core.pure ("X-Amz-Target", "AmazonSSM.CreateMaintenanceWindow")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateMaintenanceWindow where
        toJSON CreateMaintenanceWindow{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  Core.Just ("Schedule" Core..= schedule),
                  Core.Just ("Duration" Core..= duration),
                  Core.Just ("Cutoff" Core..= cutoff),
                  Core.Just
                    ("AllowUnassociatedTargets" Core..= allowUnassociatedTargets),
                  ("ClientToken" Core..=) Core.<$> clientToken,
                  ("Description" Core..=) Core.<$> description,
                  ("EndDate" Core..=) Core.<$> endDate,
                  ("ScheduleOffset" Core..=) Core.<$> scheduleOffset,
                  ("ScheduleTimezone" Core..=) Core.<$> scheduleTimezone,
                  ("StartDate" Core..=) Core.<$> startDate,
                  ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateMaintenanceWindow where
        type Rs CreateMaintenanceWindow = CreateMaintenanceWindowResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateMaintenanceWindowResponse' Core.<$>
                   (x Core..:? "WindowId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateMaintenanceWindowResponse' smart constructor.
data CreateMaintenanceWindowResponse = CreateMaintenanceWindowResponse'
  { windowId :: Core.Maybe Types.MaintenanceWindowId
    -- ^ The ID of the created maintenance window.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateMaintenanceWindowResponse' value with any optional fields omitted.
mkCreateMaintenanceWindowResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateMaintenanceWindowResponse
mkCreateMaintenanceWindowResponse responseStatus
  = CreateMaintenanceWindowResponse'{windowId = Core.Nothing,
                                     responseStatus}

-- | The ID of the created maintenance window.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmwrrsWindowId :: Lens.Lens' CreateMaintenanceWindowResponse (Core.Maybe Types.MaintenanceWindowId)
cmwrrsWindowId = Lens.field @"windowId"
{-# INLINEABLE cmwrrsWindowId #-}
{-# DEPRECATED windowId "Use generic-lens or generic-optics with 'windowId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmwrrsResponseStatus :: Lens.Lens' CreateMaintenanceWindowResponse Core.Int
cmwrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cmwrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
