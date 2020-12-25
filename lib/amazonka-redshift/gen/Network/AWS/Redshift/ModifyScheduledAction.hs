{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.ModifyScheduledAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a scheduled action.
module Network.AWS.Redshift.ModifyScheduledAction
  ( -- * Creating a request
    ModifyScheduledAction (..),
    mkModifyScheduledAction,

    -- ** Request lenses
    msaScheduledActionName,
    msaEnable,
    msaEndTime,
    msaIamRole,
    msaSchedule,
    msaScheduledActionDescription,
    msaStartTime,
    msaTargetAction,

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

-- | /See:/ 'mkModifyScheduledAction' smart constructor.
data ModifyScheduledAction = ModifyScheduledAction'
  { -- | The name of the scheduled action to modify.
    scheduledActionName :: Types.ScheduledActionName,
    -- | A modified enable flag of the scheduled action. If true, the scheduled action is active. If false, the scheduled action is disabled.
    enable :: Core.Maybe Core.Bool,
    -- | A modified end time of the scheduled action. For more information about this parameter, see 'ScheduledAction' .
    endTime :: Core.Maybe Core.UTCTime,
    -- | A different IAM role to assume to run the target action. For more information about this parameter, see 'ScheduledAction' .
    iamRole :: Core.Maybe Types.IamRole,
    -- | A modified schedule in either @at( )@ or @cron( )@ format. For more information about this parameter, see 'ScheduledAction' .
    schedule :: Core.Maybe Types.Schedule,
    -- | A modified description of the scheduled action.
    scheduledActionDescription :: Core.Maybe Types.ScheduledActionDescription,
    -- | A modified start time of the scheduled action. For more information about this parameter, see 'ScheduledAction' .
    startTime :: Core.Maybe Core.UTCTime,
    -- | A modified JSON format of the scheduled action. For more information about this parameter, see 'ScheduledAction' .
    targetAction :: Core.Maybe Types.ScheduledActionType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ModifyScheduledAction' value with any optional fields omitted.
mkModifyScheduledAction ::
  -- | 'scheduledActionName'
  Types.ScheduledActionName ->
  ModifyScheduledAction
mkModifyScheduledAction scheduledActionName =
  ModifyScheduledAction'
    { scheduledActionName,
      enable = Core.Nothing,
      endTime = Core.Nothing,
      iamRole = Core.Nothing,
      schedule = Core.Nothing,
      scheduledActionDescription = Core.Nothing,
      startTime = Core.Nothing,
      targetAction = Core.Nothing
    }

-- | The name of the scheduled action to modify.
--
-- /Note:/ Consider using 'scheduledActionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msaScheduledActionName :: Lens.Lens' ModifyScheduledAction Types.ScheduledActionName
msaScheduledActionName = Lens.field @"scheduledActionName"
{-# DEPRECATED msaScheduledActionName "Use generic-lens or generic-optics with 'scheduledActionName' instead." #-}

-- | A modified enable flag of the scheduled action. If true, the scheduled action is active. If false, the scheduled action is disabled.
--
-- /Note:/ Consider using 'enable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msaEnable :: Lens.Lens' ModifyScheduledAction (Core.Maybe Core.Bool)
msaEnable = Lens.field @"enable"
{-# DEPRECATED msaEnable "Use generic-lens or generic-optics with 'enable' instead." #-}

-- | A modified end time of the scheduled action. For more information about this parameter, see 'ScheduledAction' .
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msaEndTime :: Lens.Lens' ModifyScheduledAction (Core.Maybe Core.UTCTime)
msaEndTime = Lens.field @"endTime"
{-# DEPRECATED msaEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | A different IAM role to assume to run the target action. For more information about this parameter, see 'ScheduledAction' .
--
-- /Note:/ Consider using 'iamRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msaIamRole :: Lens.Lens' ModifyScheduledAction (Core.Maybe Types.IamRole)
msaIamRole = Lens.field @"iamRole"
{-# DEPRECATED msaIamRole "Use generic-lens or generic-optics with 'iamRole' instead." #-}

-- | A modified schedule in either @at( )@ or @cron( )@ format. For more information about this parameter, see 'ScheduledAction' .
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msaSchedule :: Lens.Lens' ModifyScheduledAction (Core.Maybe Types.Schedule)
msaSchedule = Lens.field @"schedule"
{-# DEPRECATED msaSchedule "Use generic-lens or generic-optics with 'schedule' instead." #-}

-- | A modified description of the scheduled action.
--
-- /Note:/ Consider using 'scheduledActionDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msaScheduledActionDescription :: Lens.Lens' ModifyScheduledAction (Core.Maybe Types.ScheduledActionDescription)
msaScheduledActionDescription = Lens.field @"scheduledActionDescription"
{-# DEPRECATED msaScheduledActionDescription "Use generic-lens or generic-optics with 'scheduledActionDescription' instead." #-}

-- | A modified start time of the scheduled action. For more information about this parameter, see 'ScheduledAction' .
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msaStartTime :: Lens.Lens' ModifyScheduledAction (Core.Maybe Core.UTCTime)
msaStartTime = Lens.field @"startTime"
{-# DEPRECATED msaStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | A modified JSON format of the scheduled action. For more information about this parameter, see 'ScheduledAction' .
--
-- /Note:/ Consider using 'targetAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msaTargetAction :: Lens.Lens' ModifyScheduledAction (Core.Maybe Types.ScheduledActionType)
msaTargetAction = Lens.field @"targetAction"
{-# DEPRECATED msaTargetAction "Use generic-lens or generic-optics with 'targetAction' instead." #-}

instance Core.AWSRequest ModifyScheduledAction where
  type Rs ModifyScheduledAction = Types.ScheduledAction
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
            ( Core.pure ("Action", "ModifyScheduledAction")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> (Core.toQueryValue "ScheduledActionName" scheduledActionName)
                Core.<> (Core.toQueryValue "Enable" Core.<$> enable)
                Core.<> (Core.toQueryValue "EndTime" Core.<$> endTime)
                Core.<> (Core.toQueryValue "IamRole" Core.<$> iamRole)
                Core.<> (Core.toQueryValue "Schedule" Core.<$> schedule)
                Core.<> ( Core.toQueryValue "ScheduledActionDescription"
                            Core.<$> scheduledActionDescription
                        )
                Core.<> (Core.toQueryValue "StartTime" Core.<$> startTime)
                Core.<> (Core.toQueryValue "TargetAction" Core.<$> targetAction)
            )
      }
  response =
    Response.receiveXMLWrapper
      "ModifyScheduledActionResult"
      (\s h x -> Core.parseXML x)
