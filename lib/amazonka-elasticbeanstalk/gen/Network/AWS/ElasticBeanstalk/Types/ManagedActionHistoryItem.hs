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
    mahiStatus,
    mahiFailureType,
    mahiActionId,
    mahiFailureDescription,
    mahiFinishedTime,
    mahiActionDescription,
    mahiExecutedTime,
    mahiActionType,
  )
where

import Network.AWS.ElasticBeanstalk.Types.ActionHistoryStatus
import Network.AWS.ElasticBeanstalk.Types.ActionType
import Network.AWS.ElasticBeanstalk.Types.FailureType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The record of a completed or failed managed action.
--
-- /See:/ 'mkManagedActionHistoryItem' smart constructor.
data ManagedActionHistoryItem = ManagedActionHistoryItem'
  { -- | The status of the action.
    status :: Lude.Maybe ActionHistoryStatus,
    -- | If the action failed, the type of failure.
    failureType :: Lude.Maybe FailureType,
    -- | A unique identifier for the managed action.
    actionId :: Lude.Maybe Lude.Text,
    -- | If the action failed, a description of the failure.
    failureDescription :: Lude.Maybe Lude.Text,
    -- | The date and time that the action finished executing.
    finishedTime :: Lude.Maybe Lude.DateTime,
    -- | A description of the managed action.
    actionDescription :: Lude.Maybe Lude.Text,
    -- | The date and time that the action started executing.
    executedTime :: Lude.Maybe Lude.DateTime,
    -- | The type of the managed action.
    actionType :: Lude.Maybe ActionType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ManagedActionHistoryItem' with the minimum fields required to make a request.
--
-- * 'status' - The status of the action.
-- * 'failureType' - If the action failed, the type of failure.
-- * 'actionId' - A unique identifier for the managed action.
-- * 'failureDescription' - If the action failed, a description of the failure.
-- * 'finishedTime' - The date and time that the action finished executing.
-- * 'actionDescription' - A description of the managed action.
-- * 'executedTime' - The date and time that the action started executing.
-- * 'actionType' - The type of the managed action.
mkManagedActionHistoryItem ::
  ManagedActionHistoryItem
mkManagedActionHistoryItem =
  ManagedActionHistoryItem'
    { status = Lude.Nothing,
      failureType = Lude.Nothing,
      actionId = Lude.Nothing,
      failureDescription = Lude.Nothing,
      finishedTime = Lude.Nothing,
      actionDescription = Lude.Nothing,
      executedTime = Lude.Nothing,
      actionType = Lude.Nothing
    }

-- | The status of the action.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mahiStatus :: Lens.Lens' ManagedActionHistoryItem (Lude.Maybe ActionHistoryStatus)
mahiStatus = Lens.lens (status :: ManagedActionHistoryItem -> Lude.Maybe ActionHistoryStatus) (\s a -> s {status = a} :: ManagedActionHistoryItem)
{-# DEPRECATED mahiStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | If the action failed, the type of failure.
--
-- /Note:/ Consider using 'failureType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mahiFailureType :: Lens.Lens' ManagedActionHistoryItem (Lude.Maybe FailureType)
mahiFailureType = Lens.lens (failureType :: ManagedActionHistoryItem -> Lude.Maybe FailureType) (\s a -> s {failureType = a} :: ManagedActionHistoryItem)
{-# DEPRECATED mahiFailureType "Use generic-lens or generic-optics with 'failureType' instead." #-}

-- | A unique identifier for the managed action.
--
-- /Note:/ Consider using 'actionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mahiActionId :: Lens.Lens' ManagedActionHistoryItem (Lude.Maybe Lude.Text)
mahiActionId = Lens.lens (actionId :: ManagedActionHistoryItem -> Lude.Maybe Lude.Text) (\s a -> s {actionId = a} :: ManagedActionHistoryItem)
{-# DEPRECATED mahiActionId "Use generic-lens or generic-optics with 'actionId' instead." #-}

-- | If the action failed, a description of the failure.
--
-- /Note:/ Consider using 'failureDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mahiFailureDescription :: Lens.Lens' ManagedActionHistoryItem (Lude.Maybe Lude.Text)
mahiFailureDescription = Lens.lens (failureDescription :: ManagedActionHistoryItem -> Lude.Maybe Lude.Text) (\s a -> s {failureDescription = a} :: ManagedActionHistoryItem)
{-# DEPRECATED mahiFailureDescription "Use generic-lens or generic-optics with 'failureDescription' instead." #-}

-- | The date and time that the action finished executing.
--
-- /Note:/ Consider using 'finishedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mahiFinishedTime :: Lens.Lens' ManagedActionHistoryItem (Lude.Maybe Lude.DateTime)
mahiFinishedTime = Lens.lens (finishedTime :: ManagedActionHistoryItem -> Lude.Maybe Lude.DateTime) (\s a -> s {finishedTime = a} :: ManagedActionHistoryItem)
{-# DEPRECATED mahiFinishedTime "Use generic-lens or generic-optics with 'finishedTime' instead." #-}

-- | A description of the managed action.
--
-- /Note:/ Consider using 'actionDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mahiActionDescription :: Lens.Lens' ManagedActionHistoryItem (Lude.Maybe Lude.Text)
mahiActionDescription = Lens.lens (actionDescription :: ManagedActionHistoryItem -> Lude.Maybe Lude.Text) (\s a -> s {actionDescription = a} :: ManagedActionHistoryItem)
{-# DEPRECATED mahiActionDescription "Use generic-lens or generic-optics with 'actionDescription' instead." #-}

-- | The date and time that the action started executing.
--
-- /Note:/ Consider using 'executedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mahiExecutedTime :: Lens.Lens' ManagedActionHistoryItem (Lude.Maybe Lude.DateTime)
mahiExecutedTime = Lens.lens (executedTime :: ManagedActionHistoryItem -> Lude.Maybe Lude.DateTime) (\s a -> s {executedTime = a} :: ManagedActionHistoryItem)
{-# DEPRECATED mahiExecutedTime "Use generic-lens or generic-optics with 'executedTime' instead." #-}

-- | The type of the managed action.
--
-- /Note:/ Consider using 'actionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mahiActionType :: Lens.Lens' ManagedActionHistoryItem (Lude.Maybe ActionType)
mahiActionType = Lens.lens (actionType :: ManagedActionHistoryItem -> Lude.Maybe ActionType) (\s a -> s {actionType = a} :: ManagedActionHistoryItem)
{-# DEPRECATED mahiActionType "Use generic-lens or generic-optics with 'actionType' instead." #-}

instance Lude.FromXML ManagedActionHistoryItem where
  parseXML x =
    ManagedActionHistoryItem'
      Lude.<$> (x Lude..@? "Status")
      Lude.<*> (x Lude..@? "FailureType")
      Lude.<*> (x Lude..@? "ActionId")
      Lude.<*> (x Lude..@? "FailureDescription")
      Lude.<*> (x Lude..@? "FinishedTime")
      Lude.<*> (x Lude..@? "ActionDescription")
      Lude.<*> (x Lude..@? "ExecutedTime")
      Lude.<*> (x Lude..@? "ActionType")
