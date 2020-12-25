{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ManagedActionHistoryItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ManagedActionHistoryItem
  ( ManagedActionHistoryItem (..),

    -- * Smart constructor
    mkManagedActionHistoryItem,

    -- * Lenses
    mahiActionDescription,
    mahiActionId,
    mahiActionType,
    mahiExecutedTime,
    mahiFailureDescription,
    mahiFailureType,
    mahiFinishedTime,
    mahiStatus,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types.ActionHistoryStatus as Types
import qualified Network.AWS.ElasticBeanstalk.Types.ActionType as Types
import qualified Network.AWS.ElasticBeanstalk.Types.FailureType as Types
import qualified Network.AWS.ElasticBeanstalk.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The record of a completed or failed managed action.
--
-- /See:/ 'mkManagedActionHistoryItem' smart constructor.
data ManagedActionHistoryItem = ManagedActionHistoryItem'
  { -- | A description of the managed action.
    actionDescription :: Core.Maybe Types.String,
    -- | A unique identifier for the managed action.
    actionId :: Core.Maybe Types.String,
    -- | The type of the managed action.
    actionType :: Core.Maybe Types.ActionType,
    -- | The date and time that the action started executing.
    executedTime :: Core.Maybe Core.UTCTime,
    -- | If the action failed, a description of the failure.
    failureDescription :: Core.Maybe Types.String,
    -- | If the action failed, the type of failure.
    failureType :: Core.Maybe Types.FailureType,
    -- | The date and time that the action finished executing.
    finishedTime :: Core.Maybe Core.UTCTime,
    -- | The status of the action.
    status :: Core.Maybe Types.ActionHistoryStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ManagedActionHistoryItem' value with any optional fields omitted.
mkManagedActionHistoryItem ::
  ManagedActionHistoryItem
mkManagedActionHistoryItem =
  ManagedActionHistoryItem'
    { actionDescription = Core.Nothing,
      actionId = Core.Nothing,
      actionType = Core.Nothing,
      executedTime = Core.Nothing,
      failureDescription = Core.Nothing,
      failureType = Core.Nothing,
      finishedTime = Core.Nothing,
      status = Core.Nothing
    }

-- | A description of the managed action.
--
-- /Note:/ Consider using 'actionDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mahiActionDescription :: Lens.Lens' ManagedActionHistoryItem (Core.Maybe Types.String)
mahiActionDescription = Lens.field @"actionDescription"
{-# DEPRECATED mahiActionDescription "Use generic-lens or generic-optics with 'actionDescription' instead." #-}

-- | A unique identifier for the managed action.
--
-- /Note:/ Consider using 'actionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mahiActionId :: Lens.Lens' ManagedActionHistoryItem (Core.Maybe Types.String)
mahiActionId = Lens.field @"actionId"
{-# DEPRECATED mahiActionId "Use generic-lens or generic-optics with 'actionId' instead." #-}

-- | The type of the managed action.
--
-- /Note:/ Consider using 'actionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mahiActionType :: Lens.Lens' ManagedActionHistoryItem (Core.Maybe Types.ActionType)
mahiActionType = Lens.field @"actionType"
{-# DEPRECATED mahiActionType "Use generic-lens or generic-optics with 'actionType' instead." #-}

-- | The date and time that the action started executing.
--
-- /Note:/ Consider using 'executedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mahiExecutedTime :: Lens.Lens' ManagedActionHistoryItem (Core.Maybe Core.UTCTime)
mahiExecutedTime = Lens.field @"executedTime"
{-# DEPRECATED mahiExecutedTime "Use generic-lens or generic-optics with 'executedTime' instead." #-}

-- | If the action failed, a description of the failure.
--
-- /Note:/ Consider using 'failureDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mahiFailureDescription :: Lens.Lens' ManagedActionHistoryItem (Core.Maybe Types.String)
mahiFailureDescription = Lens.field @"failureDescription"
{-# DEPRECATED mahiFailureDescription "Use generic-lens or generic-optics with 'failureDescription' instead." #-}

-- | If the action failed, the type of failure.
--
-- /Note:/ Consider using 'failureType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mahiFailureType :: Lens.Lens' ManagedActionHistoryItem (Core.Maybe Types.FailureType)
mahiFailureType = Lens.field @"failureType"
{-# DEPRECATED mahiFailureType "Use generic-lens or generic-optics with 'failureType' instead." #-}

-- | The date and time that the action finished executing.
--
-- /Note:/ Consider using 'finishedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mahiFinishedTime :: Lens.Lens' ManagedActionHistoryItem (Core.Maybe Core.UTCTime)
mahiFinishedTime = Lens.field @"finishedTime"
{-# DEPRECATED mahiFinishedTime "Use generic-lens or generic-optics with 'finishedTime' instead." #-}

-- | The status of the action.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mahiStatus :: Lens.Lens' ManagedActionHistoryItem (Core.Maybe Types.ActionHistoryStatus)
mahiStatus = Lens.field @"status"
{-# DEPRECATED mahiStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromXML ManagedActionHistoryItem where
  parseXML x =
    ManagedActionHistoryItem'
      Core.<$> (x Core..@? "ActionDescription")
      Core.<*> (x Core..@? "ActionId")
      Core.<*> (x Core..@? "ActionType")
      Core.<*> (x Core..@? "ExecutedTime")
      Core.<*> (x Core..@? "FailureDescription")
      Core.<*> (x Core..@? "FailureType")
      Core.<*> (x Core..@? "FinishedTime")
      Core.<*> (x Core..@? "Status")
