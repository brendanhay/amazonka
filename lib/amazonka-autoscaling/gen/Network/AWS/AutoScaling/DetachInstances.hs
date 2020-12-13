{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DetachInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes one or more instances from the specified Auto Scaling group.
--
-- After the instances are detached, you can manage them independent of the Auto Scaling group.
-- If you do not specify the option to decrement the desired capacity, Amazon EC2 Auto Scaling launches instances to replace the ones that are detached.
-- If there is a Classic Load Balancer attached to the Auto Scaling group, the instances are deregistered from the load balancer. If there are target groups attached to the Auto Scaling group, the instances are deregistered from the target groups.
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/detach-instance-asg.html Detach EC2 instances from your Auto Scaling group> in the /Amazon EC2 Auto Scaling User Guide/ .
module Network.AWS.AutoScaling.DetachInstances
  ( -- * Creating a request
    DetachInstances (..),
    mkDetachInstances,

    -- ** Request lenses
    diInstanceIds,
    diAutoScalingGroupName,
    diShouldDecrementDesiredCapacity,

    -- * Destructuring the response
    DetachInstancesResponse (..),
    mkDetachInstancesResponse,

    -- ** Response lenses
    dirsActivities,
    dirsResponseStatus,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDetachInstances' smart constructor.
data DetachInstances = DetachInstances'
  { -- | The IDs of the instances. You can specify up to 20 instances.
    instanceIds :: Lude.Maybe [Lude.Text],
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Lude.Text,
    -- | Indicates whether the Auto Scaling group decrements the desired capacity value by the number of instances detached.
    shouldDecrementDesiredCapacity :: Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetachInstances' with the minimum fields required to make a request.
--
-- * 'instanceIds' - The IDs of the instances. You can specify up to 20 instances.
-- * 'autoScalingGroupName' - The name of the Auto Scaling group.
-- * 'shouldDecrementDesiredCapacity' - Indicates whether the Auto Scaling group decrements the desired capacity value by the number of instances detached.
mkDetachInstances ::
  -- | 'autoScalingGroupName'
  Lude.Text ->
  -- | 'shouldDecrementDesiredCapacity'
  Lude.Bool ->
  DetachInstances
mkDetachInstances
  pAutoScalingGroupName_
  pShouldDecrementDesiredCapacity_ =
    DetachInstances'
      { instanceIds = Lude.Nothing,
        autoScalingGroupName = pAutoScalingGroupName_,
        shouldDecrementDesiredCapacity = pShouldDecrementDesiredCapacity_
      }

-- | The IDs of the instances. You can specify up to 20 instances.
--
-- /Note:/ Consider using 'instanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diInstanceIds :: Lens.Lens' DetachInstances (Lude.Maybe [Lude.Text])
diInstanceIds = Lens.lens (instanceIds :: DetachInstances -> Lude.Maybe [Lude.Text]) (\s a -> s {instanceIds = a} :: DetachInstances)
{-# DEPRECATED diInstanceIds "Use generic-lens or generic-optics with 'instanceIds' instead." #-}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diAutoScalingGroupName :: Lens.Lens' DetachInstances Lude.Text
diAutoScalingGroupName = Lens.lens (autoScalingGroupName :: DetachInstances -> Lude.Text) (\s a -> s {autoScalingGroupName = a} :: DetachInstances)
{-# DEPRECATED diAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

-- | Indicates whether the Auto Scaling group decrements the desired capacity value by the number of instances detached.
--
-- /Note:/ Consider using 'shouldDecrementDesiredCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diShouldDecrementDesiredCapacity :: Lens.Lens' DetachInstances Lude.Bool
diShouldDecrementDesiredCapacity = Lens.lens (shouldDecrementDesiredCapacity :: DetachInstances -> Lude.Bool) (\s a -> s {shouldDecrementDesiredCapacity = a} :: DetachInstances)
{-# DEPRECATED diShouldDecrementDesiredCapacity "Use generic-lens or generic-optics with 'shouldDecrementDesiredCapacity' instead." #-}

instance Lude.AWSRequest DetachInstances where
  type Rs DetachInstances = DetachInstancesResponse
  request = Req.postQuery autoScalingService
  response =
    Res.receiveXMLWrapper
      "DetachInstancesResult"
      ( \s h x ->
          DetachInstancesResponse'
            Lude.<$> ( x Lude..@? "Activities" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DetachInstances where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DetachInstances where
  toPath = Lude.const "/"

instance Lude.ToQuery DetachInstances where
  toQuery DetachInstances' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DetachInstances" :: Lude.ByteString),
        "Version" Lude.=: ("2011-01-01" :: Lude.ByteString),
        "InstanceIds"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> instanceIds),
        "AutoScalingGroupName" Lude.=: autoScalingGroupName,
        "ShouldDecrementDesiredCapacity"
          Lude.=: shouldDecrementDesiredCapacity
      ]

-- | /See:/ 'mkDetachInstancesResponse' smart constructor.
data DetachInstancesResponse = DetachInstancesResponse'
  { -- | The activities related to detaching the instances from the Auto Scaling group.
    activities :: Lude.Maybe [Activity],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetachInstancesResponse' with the minimum fields required to make a request.
--
-- * 'activities' - The activities related to detaching the instances from the Auto Scaling group.
-- * 'responseStatus' - The response status code.
mkDetachInstancesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DetachInstancesResponse
mkDetachInstancesResponse pResponseStatus_ =
  DetachInstancesResponse'
    { activities = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The activities related to detaching the instances from the Auto Scaling group.
--
-- /Note:/ Consider using 'activities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirsActivities :: Lens.Lens' DetachInstancesResponse (Lude.Maybe [Activity])
dirsActivities = Lens.lens (activities :: DetachInstancesResponse -> Lude.Maybe [Activity]) (\s a -> s {activities = a} :: DetachInstancesResponse)
{-# DEPRECATED dirsActivities "Use generic-lens or generic-optics with 'activities' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirsResponseStatus :: Lens.Lens' DetachInstancesResponse Lude.Int
dirsResponseStatus = Lens.lens (responseStatus :: DetachInstancesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DetachInstancesResponse)
{-# DEPRECATED dirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
