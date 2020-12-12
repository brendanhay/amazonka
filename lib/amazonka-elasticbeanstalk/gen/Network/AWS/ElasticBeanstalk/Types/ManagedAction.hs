{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ManagedAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ManagedAction
  ( ManagedAction (..),

    -- * Smart constructor
    mkManagedAction,

    -- * Lenses
    maStatus,
    maActionId,
    maWindowStartTime,
    maActionDescription,
    maActionType,
  )
where

import Network.AWS.ElasticBeanstalk.Types.ActionStatus
import Network.AWS.ElasticBeanstalk.Types.ActionType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The record of an upcoming or in-progress managed action.
--
-- /See:/ 'mkManagedAction' smart constructor.
data ManagedAction = ManagedAction'
  { status ::
      Lude.Maybe ActionStatus,
    actionId :: Lude.Maybe Lude.Text,
    windowStartTime :: Lude.Maybe Lude.DateTime,
    actionDescription :: Lude.Maybe Lude.Text,
    actionType :: Lude.Maybe ActionType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ManagedAction' with the minimum fields required to make a request.
--
-- * 'actionDescription' - A description of the managed action.
-- * 'actionId' - A unique identifier for the managed action.
-- * 'actionType' - The type of managed action.
-- * 'status' - The status of the managed action. If the action is @Scheduled@ , you can apply it immediately with 'ApplyEnvironmentManagedAction' .
-- * 'windowStartTime' - The start time of the maintenance window in which the managed action will execute.
mkManagedAction ::
  ManagedAction
mkManagedAction =
  ManagedAction'
    { status = Lude.Nothing,
      actionId = Lude.Nothing,
      windowStartTime = Lude.Nothing,
      actionDescription = Lude.Nothing,
      actionType = Lude.Nothing
    }

-- | The status of the managed action. If the action is @Scheduled@ , you can apply it immediately with 'ApplyEnvironmentManagedAction' .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maStatus :: Lens.Lens' ManagedAction (Lude.Maybe ActionStatus)
maStatus = Lens.lens (status :: ManagedAction -> Lude.Maybe ActionStatus) (\s a -> s {status = a} :: ManagedAction)
{-# DEPRECATED maStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | A unique identifier for the managed action.
--
-- /Note:/ Consider using 'actionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maActionId :: Lens.Lens' ManagedAction (Lude.Maybe Lude.Text)
maActionId = Lens.lens (actionId :: ManagedAction -> Lude.Maybe Lude.Text) (\s a -> s {actionId = a} :: ManagedAction)
{-# DEPRECATED maActionId "Use generic-lens or generic-optics with 'actionId' instead." #-}

-- | The start time of the maintenance window in which the managed action will execute.
--
-- /Note:/ Consider using 'windowStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maWindowStartTime :: Lens.Lens' ManagedAction (Lude.Maybe Lude.DateTime)
maWindowStartTime = Lens.lens (windowStartTime :: ManagedAction -> Lude.Maybe Lude.DateTime) (\s a -> s {windowStartTime = a} :: ManagedAction)
{-# DEPRECATED maWindowStartTime "Use generic-lens or generic-optics with 'windowStartTime' instead." #-}

-- | A description of the managed action.
--
-- /Note:/ Consider using 'actionDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maActionDescription :: Lens.Lens' ManagedAction (Lude.Maybe Lude.Text)
maActionDescription = Lens.lens (actionDescription :: ManagedAction -> Lude.Maybe Lude.Text) (\s a -> s {actionDescription = a} :: ManagedAction)
{-# DEPRECATED maActionDescription "Use generic-lens or generic-optics with 'actionDescription' instead." #-}

-- | The type of managed action.
--
-- /Note:/ Consider using 'actionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maActionType :: Lens.Lens' ManagedAction (Lude.Maybe ActionType)
maActionType = Lens.lens (actionType :: ManagedAction -> Lude.Maybe ActionType) (\s a -> s {actionType = a} :: ManagedAction)
{-# DEPRECATED maActionType "Use generic-lens or generic-optics with 'actionType' instead." #-}

instance Lude.FromXML ManagedAction where
  parseXML x =
    ManagedAction'
      Lude.<$> (x Lude..@? "Status")
      Lude.<*> (x Lude..@? "ActionId")
      Lude.<*> (x Lude..@? "WindowStartTime")
      Lude.<*> (x Lude..@? "ActionDescription")
      Lude.<*> (x Lude..@? "ActionType")
