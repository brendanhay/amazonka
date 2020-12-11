{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.TerminateInstanceInAutoScalingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Terminates the specified instance and optionally adjusts the desired group size.
--
-- This call simply makes a termination request. The instance is not terminated immediately. When an instance is terminated, the instance status changes to @terminated@ . You can't connect to or start an instance after you've terminated it.
-- If you do not specify the option to decrement the desired capacity, Amazon EC2 Auto Scaling launches instances to replace the ones that are terminated.
-- By default, Amazon EC2 Auto Scaling balances instances across all Availability Zones. If you decrement the desired capacity, your Auto Scaling group can become unbalanced between Availability Zones. Amazon EC2 Auto Scaling tries to rebalance the group, and rebalancing might terminate instances in other zones. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/auto-scaling-benefits.html#AutoScalingBehavior.InstanceUsage Rebalancing activities> in the /Amazon EC2 Auto Scaling User Guide/ .
module Network.AWS.AutoScaling.TerminateInstanceInAutoScalingGroup
  ( -- * Creating a request
    TerminateInstanceInAutoScalingGroup (..),
    mkTerminateInstanceInAutoScalingGroup,

    -- ** Request lenses
    tiiasgInstanceId,
    tiiasgShouldDecrementDesiredCapacity,

    -- * Destructuring the response
    TerminateInstanceInAutoScalingGroupResponse (..),
    mkTerminateInstanceInAutoScalingGroupResponse,

    -- ** Response lenses
    tiiasgrsActivity,
    tiiasgrsResponseStatus,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkTerminateInstanceInAutoScalingGroup' smart constructor.
data TerminateInstanceInAutoScalingGroup = TerminateInstanceInAutoScalingGroup'
  { instanceId ::
      Lude.Text,
    shouldDecrementDesiredCapacity ::
      Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TerminateInstanceInAutoScalingGroup' with the minimum fields required to make a request.
--
-- * 'instanceId' - The ID of the instance.
-- * 'shouldDecrementDesiredCapacity' - Indicates whether terminating the instance also decrements the size of the Auto Scaling group.
mkTerminateInstanceInAutoScalingGroup ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'shouldDecrementDesiredCapacity'
  Lude.Bool ->
  TerminateInstanceInAutoScalingGroup
mkTerminateInstanceInAutoScalingGroup
  pInstanceId_
  pShouldDecrementDesiredCapacity_ =
    TerminateInstanceInAutoScalingGroup'
      { instanceId = pInstanceId_,
        shouldDecrementDesiredCapacity =
          pShouldDecrementDesiredCapacity_
      }

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiiasgInstanceId :: Lens.Lens' TerminateInstanceInAutoScalingGroup Lude.Text
tiiasgInstanceId = Lens.lens (instanceId :: TerminateInstanceInAutoScalingGroup -> Lude.Text) (\s a -> s {instanceId = a} :: TerminateInstanceInAutoScalingGroup)
{-# DEPRECATED tiiasgInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | Indicates whether terminating the instance also decrements the size of the Auto Scaling group.
--
-- /Note:/ Consider using 'shouldDecrementDesiredCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiiasgShouldDecrementDesiredCapacity :: Lens.Lens' TerminateInstanceInAutoScalingGroup Lude.Bool
tiiasgShouldDecrementDesiredCapacity = Lens.lens (shouldDecrementDesiredCapacity :: TerminateInstanceInAutoScalingGroup -> Lude.Bool) (\s a -> s {shouldDecrementDesiredCapacity = a} :: TerminateInstanceInAutoScalingGroup)
{-# DEPRECATED tiiasgShouldDecrementDesiredCapacity "Use generic-lens or generic-optics with 'shouldDecrementDesiredCapacity' instead." #-}

instance Lude.AWSRequest TerminateInstanceInAutoScalingGroup where
  type
    Rs TerminateInstanceInAutoScalingGroup =
      TerminateInstanceInAutoScalingGroupResponse
  request = Req.postQuery autoScalingService
  response =
    Res.receiveXMLWrapper
      "TerminateInstanceInAutoScalingGroupResult"
      ( \s h x ->
          TerminateInstanceInAutoScalingGroupResponse'
            Lude.<$> (x Lude..@? "Activity") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders TerminateInstanceInAutoScalingGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath TerminateInstanceInAutoScalingGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery TerminateInstanceInAutoScalingGroup where
  toQuery TerminateInstanceInAutoScalingGroup' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("TerminateInstanceInAutoScalingGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2011-01-01" :: Lude.ByteString),
        "InstanceId" Lude.=: instanceId,
        "ShouldDecrementDesiredCapacity"
          Lude.=: shouldDecrementDesiredCapacity
      ]

-- | /See:/ 'mkTerminateInstanceInAutoScalingGroupResponse' smart constructor.
data TerminateInstanceInAutoScalingGroupResponse = TerminateInstanceInAutoScalingGroupResponse'
  { activity ::
      Lude.Maybe
        Activity,
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

-- | Creates a value of 'TerminateInstanceInAutoScalingGroupResponse' with the minimum fields required to make a request.
--
-- * 'activity' - A scaling activity.
-- * 'responseStatus' - The response status code.
mkTerminateInstanceInAutoScalingGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  TerminateInstanceInAutoScalingGroupResponse
mkTerminateInstanceInAutoScalingGroupResponse pResponseStatus_ =
  TerminateInstanceInAutoScalingGroupResponse'
    { activity =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A scaling activity.
--
-- /Note:/ Consider using 'activity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiiasgrsActivity :: Lens.Lens' TerminateInstanceInAutoScalingGroupResponse (Lude.Maybe Activity)
tiiasgrsActivity = Lens.lens (activity :: TerminateInstanceInAutoScalingGroupResponse -> Lude.Maybe Activity) (\s a -> s {activity = a} :: TerminateInstanceInAutoScalingGroupResponse)
{-# DEPRECATED tiiasgrsActivity "Use generic-lens or generic-optics with 'activity' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiiasgrsResponseStatus :: Lens.Lens' TerminateInstanceInAutoScalingGroupResponse Lude.Int
tiiasgrsResponseStatus = Lens.lens (responseStatus :: TerminateInstanceInAutoScalingGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: TerminateInstanceInAutoScalingGroupResponse)
{-# DEPRECATED tiiasgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
