{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.BatchPutScheduledUpdateGroupAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates one or more scheduled scaling actions for an Auto Scaling group. If you leave a parameter unspecified when updating a scheduled scaling action, the corresponding value remains unchanged.
module Network.AWS.AutoScaling.BatchPutScheduledUpdateGroupAction
  ( -- * Creating a request
    BatchPutScheduledUpdateGroupAction (..),
    mkBatchPutScheduledUpdateGroupAction,

    -- ** Request lenses
    bpsugaScheduledUpdateGroupActions,
    bpsugaAutoScalingGroupName,

    -- * Destructuring the response
    BatchPutScheduledUpdateGroupActionResponse (..),
    mkBatchPutScheduledUpdateGroupActionResponse,

    -- ** Response lenses
    bpsugarsFailedScheduledUpdateGroupActions,
    bpsugarsResponseStatus,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkBatchPutScheduledUpdateGroupAction' smart constructor.
data BatchPutScheduledUpdateGroupAction = BatchPutScheduledUpdateGroupAction'
  { -- | One or more scheduled actions. The maximum number allowed is 50.
    scheduledUpdateGroupActions :: [ScheduledUpdateGroupActionRequest],
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchPutScheduledUpdateGroupAction' with the minimum fields required to make a request.
--
-- * 'scheduledUpdateGroupActions' - One or more scheduled actions. The maximum number allowed is 50.
-- * 'autoScalingGroupName' - The name of the Auto Scaling group.
mkBatchPutScheduledUpdateGroupAction ::
  -- | 'autoScalingGroupName'
  Lude.Text ->
  BatchPutScheduledUpdateGroupAction
mkBatchPutScheduledUpdateGroupAction pAutoScalingGroupName_ =
  BatchPutScheduledUpdateGroupAction'
    { scheduledUpdateGroupActions =
        Lude.mempty,
      autoScalingGroupName = pAutoScalingGroupName_
    }

-- | One or more scheduled actions. The maximum number allowed is 50.
--
-- /Note:/ Consider using 'scheduledUpdateGroupActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpsugaScheduledUpdateGroupActions :: Lens.Lens' BatchPutScheduledUpdateGroupAction [ScheduledUpdateGroupActionRequest]
bpsugaScheduledUpdateGroupActions = Lens.lens (scheduledUpdateGroupActions :: BatchPutScheduledUpdateGroupAction -> [ScheduledUpdateGroupActionRequest]) (\s a -> s {scheduledUpdateGroupActions = a} :: BatchPutScheduledUpdateGroupAction)
{-# DEPRECATED bpsugaScheduledUpdateGroupActions "Use generic-lens or generic-optics with 'scheduledUpdateGroupActions' instead." #-}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpsugaAutoScalingGroupName :: Lens.Lens' BatchPutScheduledUpdateGroupAction Lude.Text
bpsugaAutoScalingGroupName = Lens.lens (autoScalingGroupName :: BatchPutScheduledUpdateGroupAction -> Lude.Text) (\s a -> s {autoScalingGroupName = a} :: BatchPutScheduledUpdateGroupAction)
{-# DEPRECATED bpsugaAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

instance Lude.AWSRequest BatchPutScheduledUpdateGroupAction where
  type
    Rs BatchPutScheduledUpdateGroupAction =
      BatchPutScheduledUpdateGroupActionResponse
  request = Req.postQuery autoScalingService
  response =
    Res.receiveXMLWrapper
      "BatchPutScheduledUpdateGroupActionResult"
      ( \s h x ->
          BatchPutScheduledUpdateGroupActionResponse'
            Lude.<$> ( x Lude..@? "FailedScheduledUpdateGroupActions"
                         Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchPutScheduledUpdateGroupAction where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath BatchPutScheduledUpdateGroupAction where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchPutScheduledUpdateGroupAction where
  toQuery BatchPutScheduledUpdateGroupAction' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("BatchPutScheduledUpdateGroupAction" :: Lude.ByteString),
        "Version" Lude.=: ("2011-01-01" :: Lude.ByteString),
        "ScheduledUpdateGroupActions"
          Lude.=: Lude.toQueryList "member" scheduledUpdateGroupActions,
        "AutoScalingGroupName" Lude.=: autoScalingGroupName
      ]

-- | /See:/ 'mkBatchPutScheduledUpdateGroupActionResponse' smart constructor.
data BatchPutScheduledUpdateGroupActionResponse = BatchPutScheduledUpdateGroupActionResponse'
  { -- | The names of the scheduled actions that could not be created or updated, including an error message.
    failedScheduledUpdateGroupActions :: Lude.Maybe [FailedScheduledUpdateGroupActionRequest],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchPutScheduledUpdateGroupActionResponse' with the minimum fields required to make a request.
--
-- * 'failedScheduledUpdateGroupActions' - The names of the scheduled actions that could not be created or updated, including an error message.
-- * 'responseStatus' - The response status code.
mkBatchPutScheduledUpdateGroupActionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchPutScheduledUpdateGroupActionResponse
mkBatchPutScheduledUpdateGroupActionResponse pResponseStatus_ =
  BatchPutScheduledUpdateGroupActionResponse'
    { failedScheduledUpdateGroupActions =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The names of the scheduled actions that could not be created or updated, including an error message.
--
-- /Note:/ Consider using 'failedScheduledUpdateGroupActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpsugarsFailedScheduledUpdateGroupActions :: Lens.Lens' BatchPutScheduledUpdateGroupActionResponse (Lude.Maybe [FailedScheduledUpdateGroupActionRequest])
bpsugarsFailedScheduledUpdateGroupActions = Lens.lens (failedScheduledUpdateGroupActions :: BatchPutScheduledUpdateGroupActionResponse -> Lude.Maybe [FailedScheduledUpdateGroupActionRequest]) (\s a -> s {failedScheduledUpdateGroupActions = a} :: BatchPutScheduledUpdateGroupActionResponse)
{-# DEPRECATED bpsugarsFailedScheduledUpdateGroupActions "Use generic-lens or generic-optics with 'failedScheduledUpdateGroupActions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpsugarsResponseStatus :: Lens.Lens' BatchPutScheduledUpdateGroupActionResponse Lude.Int
bpsugarsResponseStatus = Lens.lens (responseStatus :: BatchPutScheduledUpdateGroupActionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchPutScheduledUpdateGroupActionResponse)
{-# DEPRECATED bpsugarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
