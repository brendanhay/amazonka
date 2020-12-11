{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DeleteScheduledAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified scheduled action.
module Network.AWS.AutoScaling.DeleteScheduledAction
  ( -- * Creating a request
    DeleteScheduledAction (..),
    mkDeleteScheduledAction,

    -- ** Request lenses
    dsaAutoScalingGroupName,
    dsaScheduledActionName,

    -- * Destructuring the response
    DeleteScheduledActionResponse (..),
    mkDeleteScheduledActionResponse,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteScheduledAction' smart constructor.
data DeleteScheduledAction = DeleteScheduledAction'
  { autoScalingGroupName ::
      Lude.Text,
    scheduledActionName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteScheduledAction' with the minimum fields required to make a request.
--
-- * 'autoScalingGroupName' - The name of the Auto Scaling group.
-- * 'scheduledActionName' - The name of the action to delete.
mkDeleteScheduledAction ::
  -- | 'autoScalingGroupName'
  Lude.Text ->
  -- | 'scheduledActionName'
  Lude.Text ->
  DeleteScheduledAction
mkDeleteScheduledAction
  pAutoScalingGroupName_
  pScheduledActionName_ =
    DeleteScheduledAction'
      { autoScalingGroupName =
          pAutoScalingGroupName_,
        scheduledActionName = pScheduledActionName_
      }

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsaAutoScalingGroupName :: Lens.Lens' DeleteScheduledAction Lude.Text
dsaAutoScalingGroupName = Lens.lens (autoScalingGroupName :: DeleteScheduledAction -> Lude.Text) (\s a -> s {autoScalingGroupName = a} :: DeleteScheduledAction)
{-# DEPRECATED dsaAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

-- | The name of the action to delete.
--
-- /Note:/ Consider using 'scheduledActionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsaScheduledActionName :: Lens.Lens' DeleteScheduledAction Lude.Text
dsaScheduledActionName = Lens.lens (scheduledActionName :: DeleteScheduledAction -> Lude.Text) (\s a -> s {scheduledActionName = a} :: DeleteScheduledAction)
{-# DEPRECATED dsaScheduledActionName "Use generic-lens or generic-optics with 'scheduledActionName' instead." #-}

instance Lude.AWSRequest DeleteScheduledAction where
  type Rs DeleteScheduledAction = DeleteScheduledActionResponse
  request = Req.postQuery autoScalingService
  response = Res.receiveNull DeleteScheduledActionResponse'

instance Lude.ToHeaders DeleteScheduledAction where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteScheduledAction where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteScheduledAction where
  toQuery DeleteScheduledAction' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteScheduledAction" :: Lude.ByteString),
        "Version" Lude.=: ("2011-01-01" :: Lude.ByteString),
        "AutoScalingGroupName" Lude.=: autoScalingGroupName,
        "ScheduledActionName" Lude.=: scheduledActionName
      ]

-- | /See:/ 'mkDeleteScheduledActionResponse' smart constructor.
data DeleteScheduledActionResponse = DeleteScheduledActionResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteScheduledActionResponse' with the minimum fields required to make a request.
mkDeleteScheduledActionResponse ::
  DeleteScheduledActionResponse
mkDeleteScheduledActionResponse = DeleteScheduledActionResponse'
