{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.CreateScheduledAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a scheduled action. A scheduled action contains a schedule and an Amazon Redshift API action. For example, you can create a schedule of when to run the @ResizeCluster@ API operation.
module Network.AWS.Redshift.CreateScheduledAction
  ( -- * Creating a request
    CreateScheduledAction (..),
    mkCreateScheduledAction,

    -- ** Request lenses
    csaScheduledActionName,
    csaTargetAction,
    csaSchedule,
    csaIamRole,
    csaEnable,
    csaEndTime,
    csaScheduledActionDescription,
    csaStartTime,

    -- * Destructuring the response
    Types.ScheduledAction (..),
    Types.mkScheduledAction,

    -- ** Response lenses
    Types.saEndTime,
    Types.saIamRole,
    Types.saNextInvocations,
    Types.saSchedule,
    Types.saScheduledActionDescription,
    Types.saScheduledActionName,
    Types.saStartTime,
    Types.saState,
    Types.saTargetAction,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateScheduledAction' smart constructor.
data CreateScheduledAction = CreateScheduledAction'
  { -- | The name of the scheduled action. The name must be unique within an account. For more information about this parameter, see 'ScheduledAction' .
    scheduledActionName :: Types.String,
    -- | A JSON format string of the Amazon Redshift API operation with input parameters. For more information about this parameter, see 'ScheduledAction' .
    targetAction :: Types.ScheduledActionType,
    -- | The schedule in @at( )@ or @cron( )@ format. For more information about this parameter, see 'ScheduledAction' .
    schedule :: Types.String,
    -- | The IAM role to assume to run the target action. For more information about this parameter, see 'ScheduledAction' .
    iamRole :: Types.String,
    -- | If true, the schedule is enabled. If false, the scheduled action does not trigger. For more information about @state@ of the scheduled action, see 'ScheduledAction' .
    enable :: Core.Maybe Core.Bool,
    -- | The end time in UTC of the scheduled action. After this time, the scheduled action does not trigger. For more information about this parameter, see 'ScheduledAction' .
    endTime :: Core.Maybe Core.UTCTime,
    -- | The description of the scheduled action.
    scheduledActionDescription :: Core.Maybe Types.String,
    -- | The start time in UTC of the scheduled action. Before this time, the scheduled action does not trigger. For more information about this parameter, see 'ScheduledAction' .
    startTime :: Core.Maybe Core.UTCTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateScheduledAction' value with any optional fields omitted.
mkCreateScheduledAction ::
  -- | 'scheduledActionName'
  Types.String ->
  -- | 'targetAction'
  Types.ScheduledActionType ->
  -- | 'schedule'
  Types.String ->
  -- | 'iamRole'
  Types.String ->
  CreateScheduledAction
mkCreateScheduledAction
  scheduledActionName
  targetAction
  schedule
  iamRole =
    CreateScheduledAction'
      { scheduledActionName,
        targetAction,
        schedule,
        iamRole,
        enable = Core.Nothing,
        endTime = Core.Nothing,
        scheduledActionDescription = Core.Nothing,
        startTime = Core.Nothing
      }

-- | The name of the scheduled action. The name must be unique within an account. For more information about this parameter, see 'ScheduledAction' .
--
-- /Note:/ Consider using 'scheduledActionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csaScheduledActionName :: Lens.Lens' CreateScheduledAction Types.String
csaScheduledActionName = Lens.field @"scheduledActionName"
{-# DEPRECATED csaScheduledActionName "Use generic-lens or generic-optics with 'scheduledActionName' instead." #-}

-- | A JSON format string of the Amazon Redshift API operation with input parameters. For more information about this parameter, see 'ScheduledAction' .
--
-- /Note:/ Consider using 'targetAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csaTargetAction :: Lens.Lens' CreateScheduledAction Types.ScheduledActionType
csaTargetAction = Lens.field @"targetAction"
{-# DEPRECATED csaTargetAction "Use generic-lens or generic-optics with 'targetAction' instead." #-}

-- | The schedule in @at( )@ or @cron( )@ format. For more information about this parameter, see 'ScheduledAction' .
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csaSchedule :: Lens.Lens' CreateScheduledAction Types.String
csaSchedule = Lens.field @"schedule"
{-# DEPRECATED csaSchedule "Use generic-lens or generic-optics with 'schedule' instead." #-}

-- | The IAM role to assume to run the target action. For more information about this parameter, see 'ScheduledAction' .
--
-- /Note:/ Consider using 'iamRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csaIamRole :: Lens.Lens' CreateScheduledAction Types.String
csaIamRole = Lens.field @"iamRole"
{-# DEPRECATED csaIamRole "Use generic-lens or generic-optics with 'iamRole' instead." #-}

-- | If true, the schedule is enabled. If false, the scheduled action does not trigger. For more information about @state@ of the scheduled action, see 'ScheduledAction' .
--
-- /Note:/ Consider using 'enable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csaEnable :: Lens.Lens' CreateScheduledAction (Core.Maybe Core.Bool)
csaEnable = Lens.field @"enable"
{-# DEPRECATED csaEnable "Use generic-lens or generic-optics with 'enable' instead." #-}

-- | The end time in UTC of the scheduled action. After this time, the scheduled action does not trigger. For more information about this parameter, see 'ScheduledAction' .
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csaEndTime :: Lens.Lens' CreateScheduledAction (Core.Maybe Core.UTCTime)
csaEndTime = Lens.field @"endTime"
{-# DEPRECATED csaEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The description of the scheduled action.
--
-- /Note:/ Consider using 'scheduledActionDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csaScheduledActionDescription :: Lens.Lens' CreateScheduledAction (Core.Maybe Types.String)
csaScheduledActionDescription = Lens.field @"scheduledActionDescription"
{-# DEPRECATED csaScheduledActionDescription "Use generic-lens or generic-optics with 'scheduledActionDescription' instead." #-}

-- | The start time in UTC of the scheduled action. Before this time, the scheduled action does not trigger. For more information about this parameter, see 'ScheduledAction' .
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csaStartTime :: Lens.Lens' CreateScheduledAction (Core.Maybe Core.UTCTime)
csaStartTime = Lens.field @"startTime"
{-# DEPRECATED csaStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

instance Core.AWSRequest CreateScheduledAction where
  type Rs CreateScheduledAction = Types.ScheduledAction
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "CreateScheduledAction")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> (Core.toQueryValue "ScheduledActionName" scheduledActionName)
                Core.<> (Core.toQueryValue "TargetAction" targetAction)
                Core.<> (Core.toQueryValue "Schedule" schedule)
                Core.<> (Core.toQueryValue "IamRole" iamRole)
                Core.<> (Core.toQueryValue "Enable" Core.<$> enable)
                Core.<> (Core.toQueryValue "EndTime" Core.<$> endTime)
                Core.<> ( Core.toQueryValue "ScheduledActionDescription"
                            Core.<$> scheduledActionDescription
                        )
                Core.<> (Core.toQueryValue "StartTime" Core.<$> startTime)
            )
      }
  response =
    Response.receiveXMLWrapper
      "CreateScheduledActionResult"
      (\s h x -> Core.parseXML x)
