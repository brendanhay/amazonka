{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ModifyScheduledAction (..)
    , mkModifyScheduledAction
    -- ** Request lenses
    , msaScheduledActionName
    , msaEnable
    , msaEndTime
    , msaIamRole
    , msaSchedule
    , msaScheduledActionDescription
    , msaStartTime
    , msaTargetAction

     -- * Destructuring the response
    , Types.ScheduledAction (..)
    , Types.mkScheduledAction
    -- ** Response lenses
    , Types.saEndTime
    , Types.saIamRole
    , Types.saNextInvocations
    , Types.saSchedule
    , Types.saScheduledActionDescription
    , Types.saScheduledActionName
    , Types.saStartTime
    , Types.saState
    , Types.saTargetAction
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyScheduledAction' smart constructor.
data ModifyScheduledAction = ModifyScheduledAction'
  { scheduledActionName :: Core.Text
    -- ^ The name of the scheduled action to modify. 
  , enable :: Core.Maybe Core.Bool
    -- ^ A modified enable flag of the scheduled action. If true, the scheduled action is active. If false, the scheduled action is disabled. 
  , endTime :: Core.Maybe Core.UTCTime
    -- ^ A modified end time of the scheduled action. For more information about this parameter, see 'ScheduledAction' . 
  , iamRole :: Core.Maybe Core.Text
    -- ^ A different IAM role to assume to run the target action. For more information about this parameter, see 'ScheduledAction' .
  , schedule :: Core.Maybe Core.Text
    -- ^ A modified schedule in either @at( )@ or @cron( )@ format. For more information about this parameter, see 'ScheduledAction' .
  , scheduledActionDescription :: Core.Maybe Core.Text
    -- ^ A modified description of the scheduled action. 
  , startTime :: Core.Maybe Core.UTCTime
    -- ^ A modified start time of the scheduled action. For more information about this parameter, see 'ScheduledAction' . 
  , targetAction :: Core.Maybe Types.ScheduledActionType
    -- ^ A modified JSON format of the scheduled action. For more information about this parameter, see 'ScheduledAction' . 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ModifyScheduledAction' value with any optional fields omitted.
mkModifyScheduledAction
    :: Core.Text -- ^ 'scheduledActionName'
    -> ModifyScheduledAction
mkModifyScheduledAction scheduledActionName
  = ModifyScheduledAction'{scheduledActionName,
                           enable = Core.Nothing, endTime = Core.Nothing,
                           iamRole = Core.Nothing, schedule = Core.Nothing,
                           scheduledActionDescription = Core.Nothing,
                           startTime = Core.Nothing, targetAction = Core.Nothing}

-- | The name of the scheduled action to modify. 
--
-- /Note:/ Consider using 'scheduledActionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msaScheduledActionName :: Lens.Lens' ModifyScheduledAction Core.Text
msaScheduledActionName = Lens.field @"scheduledActionName"
{-# INLINEABLE msaScheduledActionName #-}
{-# DEPRECATED scheduledActionName "Use generic-lens or generic-optics with 'scheduledActionName' instead"  #-}

-- | A modified enable flag of the scheduled action. If true, the scheduled action is active. If false, the scheduled action is disabled. 
--
-- /Note:/ Consider using 'enable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msaEnable :: Lens.Lens' ModifyScheduledAction (Core.Maybe Core.Bool)
msaEnable = Lens.field @"enable"
{-# INLINEABLE msaEnable #-}
{-# DEPRECATED enable "Use generic-lens or generic-optics with 'enable' instead"  #-}

-- | A modified end time of the scheduled action. For more information about this parameter, see 'ScheduledAction' . 
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msaEndTime :: Lens.Lens' ModifyScheduledAction (Core.Maybe Core.UTCTime)
msaEndTime = Lens.field @"endTime"
{-# INLINEABLE msaEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | A different IAM role to assume to run the target action. For more information about this parameter, see 'ScheduledAction' .
--
-- /Note:/ Consider using 'iamRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msaIamRole :: Lens.Lens' ModifyScheduledAction (Core.Maybe Core.Text)
msaIamRole = Lens.field @"iamRole"
{-# INLINEABLE msaIamRole #-}
{-# DEPRECATED iamRole "Use generic-lens or generic-optics with 'iamRole' instead"  #-}

-- | A modified schedule in either @at( )@ or @cron( )@ format. For more information about this parameter, see 'ScheduledAction' .
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msaSchedule :: Lens.Lens' ModifyScheduledAction (Core.Maybe Core.Text)
msaSchedule = Lens.field @"schedule"
{-# INLINEABLE msaSchedule #-}
{-# DEPRECATED schedule "Use generic-lens or generic-optics with 'schedule' instead"  #-}

-- | A modified description of the scheduled action. 
--
-- /Note:/ Consider using 'scheduledActionDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msaScheduledActionDescription :: Lens.Lens' ModifyScheduledAction (Core.Maybe Core.Text)
msaScheduledActionDescription = Lens.field @"scheduledActionDescription"
{-# INLINEABLE msaScheduledActionDescription #-}
{-# DEPRECATED scheduledActionDescription "Use generic-lens or generic-optics with 'scheduledActionDescription' instead"  #-}

-- | A modified start time of the scheduled action. For more information about this parameter, see 'ScheduledAction' . 
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msaStartTime :: Lens.Lens' ModifyScheduledAction (Core.Maybe Core.UTCTime)
msaStartTime = Lens.field @"startTime"
{-# INLINEABLE msaStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

-- | A modified JSON format of the scheduled action. For more information about this parameter, see 'ScheduledAction' . 
--
-- /Note:/ Consider using 'targetAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msaTargetAction :: Lens.Lens' ModifyScheduledAction (Core.Maybe Types.ScheduledActionType)
msaTargetAction = Lens.field @"targetAction"
{-# INLINEABLE msaTargetAction #-}
{-# DEPRECATED targetAction "Use generic-lens or generic-optics with 'targetAction' instead"  #-}

instance Core.ToQuery ModifyScheduledAction where
        toQuery ModifyScheduledAction{..}
          = Core.toQueryPair "Action" ("ModifyScheduledAction" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "ScheduledActionName" scheduledActionName
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Enable") enable
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "EndTime") endTime
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "IamRole") iamRole
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Schedule") schedule
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "ScheduledActionDescription")
                scheduledActionDescription
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "StartTime") startTime
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "TargetAction")
                targetAction

instance Core.ToHeaders ModifyScheduledAction where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyScheduledAction where
        type Rs ModifyScheduledAction = Types.ScheduledAction
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "ModifyScheduledActionResult"
              (\ s h x -> Core.parseXML x)
        
        {-# INLINE parseResponse #-}
