{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ManagedActionHistoryItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticBeanstalk.Types.ManagedActionHistoryItem
  ( ManagedActionHistoryItem (..)
  -- * Smart constructor
  , mkManagedActionHistoryItem
  -- * Lenses
  , mahiActionDescription
  , mahiActionId
  , mahiActionType
  , mahiExecutedTime
  , mahiFailureDescription
  , mahiFailureType
  , mahiFinishedTime
  , mahiStatus
  ) where

import qualified Network.AWS.ElasticBeanstalk.Types.ActionHistoryStatus as Types
import qualified Network.AWS.ElasticBeanstalk.Types.ActionType as Types
import qualified Network.AWS.ElasticBeanstalk.Types.FailureType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The record of a completed or failed managed action.
--
-- /See:/ 'mkManagedActionHistoryItem' smart constructor.
data ManagedActionHistoryItem = ManagedActionHistoryItem'
  { actionDescription :: Core.Maybe Core.Text
    -- ^ A description of the managed action.
  , actionId :: Core.Maybe Core.Text
    -- ^ A unique identifier for the managed action.
  , actionType :: Core.Maybe Types.ActionType
    -- ^ The type of the managed action.
  , executedTime :: Core.Maybe Core.UTCTime
    -- ^ The date and time that the action started executing.
  , failureDescription :: Core.Maybe Core.Text
    -- ^ If the action failed, a description of the failure.
  , failureType :: Core.Maybe Types.FailureType
    -- ^ If the action failed, the type of failure.
  , finishedTime :: Core.Maybe Core.UTCTime
    -- ^ The date and time that the action finished executing.
  , status :: Core.Maybe Types.ActionHistoryStatus
    -- ^ The status of the action.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ManagedActionHistoryItem' value with any optional fields omitted.
mkManagedActionHistoryItem
    :: ManagedActionHistoryItem
mkManagedActionHistoryItem
  = ManagedActionHistoryItem'{actionDescription = Core.Nothing,
                              actionId = Core.Nothing, actionType = Core.Nothing,
                              executedTime = Core.Nothing, failureDescription = Core.Nothing,
                              failureType = Core.Nothing, finishedTime = Core.Nothing,
                              status = Core.Nothing}

-- | A description of the managed action.
--
-- /Note:/ Consider using 'actionDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mahiActionDescription :: Lens.Lens' ManagedActionHistoryItem (Core.Maybe Core.Text)
mahiActionDescription = Lens.field @"actionDescription"
{-# INLINEABLE mahiActionDescription #-}
{-# DEPRECATED actionDescription "Use generic-lens or generic-optics with 'actionDescription' instead"  #-}

-- | A unique identifier for the managed action.
--
-- /Note:/ Consider using 'actionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mahiActionId :: Lens.Lens' ManagedActionHistoryItem (Core.Maybe Core.Text)
mahiActionId = Lens.field @"actionId"
{-# INLINEABLE mahiActionId #-}
{-# DEPRECATED actionId "Use generic-lens or generic-optics with 'actionId' instead"  #-}

-- | The type of the managed action.
--
-- /Note:/ Consider using 'actionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mahiActionType :: Lens.Lens' ManagedActionHistoryItem (Core.Maybe Types.ActionType)
mahiActionType = Lens.field @"actionType"
{-# INLINEABLE mahiActionType #-}
{-# DEPRECATED actionType "Use generic-lens or generic-optics with 'actionType' instead"  #-}

-- | The date and time that the action started executing.
--
-- /Note:/ Consider using 'executedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mahiExecutedTime :: Lens.Lens' ManagedActionHistoryItem (Core.Maybe Core.UTCTime)
mahiExecutedTime = Lens.field @"executedTime"
{-# INLINEABLE mahiExecutedTime #-}
{-# DEPRECATED executedTime "Use generic-lens or generic-optics with 'executedTime' instead"  #-}

-- | If the action failed, a description of the failure.
--
-- /Note:/ Consider using 'failureDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mahiFailureDescription :: Lens.Lens' ManagedActionHistoryItem (Core.Maybe Core.Text)
mahiFailureDescription = Lens.field @"failureDescription"
{-# INLINEABLE mahiFailureDescription #-}
{-# DEPRECATED failureDescription "Use generic-lens or generic-optics with 'failureDescription' instead"  #-}

-- | If the action failed, the type of failure.
--
-- /Note:/ Consider using 'failureType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mahiFailureType :: Lens.Lens' ManagedActionHistoryItem (Core.Maybe Types.FailureType)
mahiFailureType = Lens.field @"failureType"
{-# INLINEABLE mahiFailureType #-}
{-# DEPRECATED failureType "Use generic-lens or generic-optics with 'failureType' instead"  #-}

-- | The date and time that the action finished executing.
--
-- /Note:/ Consider using 'finishedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mahiFinishedTime :: Lens.Lens' ManagedActionHistoryItem (Core.Maybe Core.UTCTime)
mahiFinishedTime = Lens.field @"finishedTime"
{-# INLINEABLE mahiFinishedTime #-}
{-# DEPRECATED finishedTime "Use generic-lens or generic-optics with 'finishedTime' instead"  #-}

-- | The status of the action.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mahiStatus :: Lens.Lens' ManagedActionHistoryItem (Core.Maybe Types.ActionHistoryStatus)
mahiStatus = Lens.field @"status"
{-# INLINEABLE mahiStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromXML ManagedActionHistoryItem where
        parseXML x
          = ManagedActionHistoryItem' Core.<$>
              (x Core..@? "ActionDescription") Core.<*> x Core..@? "ActionId"
                Core.<*> x Core..@? "ActionType"
                Core.<*> x Core..@? "ExecutedTime"
                Core.<*> x Core..@? "FailureDescription"
                Core.<*> x Core..@? "FailureType"
                Core.<*> x Core..@? "FinishedTime"
                Core.<*> x Core..@? "Status"
