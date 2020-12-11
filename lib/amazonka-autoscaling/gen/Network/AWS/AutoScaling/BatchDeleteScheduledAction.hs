{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.BatchDeleteScheduledAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more scheduled actions for the specified Auto Scaling group.
module Network.AWS.AutoScaling.BatchDeleteScheduledAction
  ( -- * Creating a request
    BatchDeleteScheduledAction (..),
    mkBatchDeleteScheduledAction,

    -- ** Request lenses
    bdsaAutoScalingGroupName,
    bdsaScheduledActionNames,

    -- * Destructuring the response
    BatchDeleteScheduledActionResponse (..),
    mkBatchDeleteScheduledActionResponse,

    -- ** Response lenses
    bdsarsFailedScheduledActions,
    bdsarsResponseStatus,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkBatchDeleteScheduledAction' smart constructor.
data BatchDeleteScheduledAction = BatchDeleteScheduledAction'
  { autoScalingGroupName ::
      Lude.Text,
    scheduledActionNames :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchDeleteScheduledAction' with the minimum fields required to make a request.
--
-- * 'autoScalingGroupName' - The name of the Auto Scaling group.
-- * 'scheduledActionNames' - The names of the scheduled actions to delete. The maximum number allowed is 50.
mkBatchDeleteScheduledAction ::
  -- | 'autoScalingGroupName'
  Lude.Text ->
  BatchDeleteScheduledAction
mkBatchDeleteScheduledAction pAutoScalingGroupName_ =
  BatchDeleteScheduledAction'
    { autoScalingGroupName =
        pAutoScalingGroupName_,
      scheduledActionNames = Lude.mempty
    }

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsaAutoScalingGroupName :: Lens.Lens' BatchDeleteScheduledAction Lude.Text
bdsaAutoScalingGroupName = Lens.lens (autoScalingGroupName :: BatchDeleteScheduledAction -> Lude.Text) (\s a -> s {autoScalingGroupName = a} :: BatchDeleteScheduledAction)
{-# DEPRECATED bdsaAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

-- | The names of the scheduled actions to delete. The maximum number allowed is 50.
--
-- /Note:/ Consider using 'scheduledActionNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsaScheduledActionNames :: Lens.Lens' BatchDeleteScheduledAction [Lude.Text]
bdsaScheduledActionNames = Lens.lens (scheduledActionNames :: BatchDeleteScheduledAction -> [Lude.Text]) (\s a -> s {scheduledActionNames = a} :: BatchDeleteScheduledAction)
{-# DEPRECATED bdsaScheduledActionNames "Use generic-lens or generic-optics with 'scheduledActionNames' instead." #-}

instance Lude.AWSRequest BatchDeleteScheduledAction where
  type
    Rs BatchDeleteScheduledAction =
      BatchDeleteScheduledActionResponse
  request = Req.postQuery autoScalingService
  response =
    Res.receiveXMLWrapper
      "BatchDeleteScheduledActionResult"
      ( \s h x ->
          BatchDeleteScheduledActionResponse'
            Lude.<$> ( x Lude..@? "FailedScheduledActions" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchDeleteScheduledAction where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath BatchDeleteScheduledAction where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchDeleteScheduledAction where
  toQuery BatchDeleteScheduledAction' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("BatchDeleteScheduledAction" :: Lude.ByteString),
        "Version" Lude.=: ("2011-01-01" :: Lude.ByteString),
        "AutoScalingGroupName" Lude.=: autoScalingGroupName,
        "ScheduledActionNames"
          Lude.=: Lude.toQueryList "member" scheduledActionNames
      ]

-- | /See:/ 'mkBatchDeleteScheduledActionResponse' smart constructor.
data BatchDeleteScheduledActionResponse = BatchDeleteScheduledActionResponse'
  { failedScheduledActions ::
      Lude.Maybe
        [FailedScheduledUpdateGroupActionRequest],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchDeleteScheduledActionResponse' with the minimum fields required to make a request.
--
-- * 'failedScheduledActions' - The names of the scheduled actions that could not be deleted, including an error message.
-- * 'responseStatus' - The response status code.
mkBatchDeleteScheduledActionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchDeleteScheduledActionResponse
mkBatchDeleteScheduledActionResponse pResponseStatus_ =
  BatchDeleteScheduledActionResponse'
    { failedScheduledActions =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The names of the scheduled actions that could not be deleted, including an error message.
--
-- /Note:/ Consider using 'failedScheduledActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsarsFailedScheduledActions :: Lens.Lens' BatchDeleteScheduledActionResponse (Lude.Maybe [FailedScheduledUpdateGroupActionRequest])
bdsarsFailedScheduledActions = Lens.lens (failedScheduledActions :: BatchDeleteScheduledActionResponse -> Lude.Maybe [FailedScheduledUpdateGroupActionRequest]) (\s a -> s {failedScheduledActions = a} :: BatchDeleteScheduledActionResponse)
{-# DEPRECATED bdsarsFailedScheduledActions "Use generic-lens or generic-optics with 'failedScheduledActions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdsarsResponseStatus :: Lens.Lens' BatchDeleteScheduledActionResponse Lude.Int
bdsarsResponseStatus = Lens.lens (responseStatus :: BatchDeleteScheduledActionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchDeleteScheduledActionResponse)
{-# DEPRECATED bdsarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
