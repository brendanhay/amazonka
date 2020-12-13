{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.EnterStandby
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Moves the specified instances into the standby state.
--
-- If you choose to decrement the desired capacity of the Auto Scaling group, the instances can enter standby as long as the desired capacity of the Auto Scaling group after the instances are placed into standby is equal to or greater than the minimum capacity of the group.
-- If you choose not to decrement the desired capacity of the Auto Scaling group, the Auto Scaling group launches new instances to replace the instances on standby.
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-enter-exit-standby.html Temporarily removing instances from your Auto Scaling group> in the /Amazon EC2 Auto Scaling User Guide/ .
module Network.AWS.AutoScaling.EnterStandby
  ( -- * Creating a request
    EnterStandby (..),
    mkEnterStandby,

    -- ** Request lenses
    esInstanceIds,
    esAutoScalingGroupName,
    esShouldDecrementDesiredCapacity,

    -- * Destructuring the response
    EnterStandbyResponse (..),
    mkEnterStandbyResponse,

    -- ** Response lenses
    esrsActivities,
    esrsResponseStatus,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkEnterStandby' smart constructor.
data EnterStandby = EnterStandby'
  { -- | The IDs of the instances. You can specify up to 20 instances.
    instanceIds :: Lude.Maybe [Lude.Text],
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Lude.Text,
    -- | Indicates whether to decrement the desired capacity of the Auto Scaling group by the number of instances moved to @Standby@ mode.
    shouldDecrementDesiredCapacity :: Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnterStandby' with the minimum fields required to make a request.
--
-- * 'instanceIds' - The IDs of the instances. You can specify up to 20 instances.
-- * 'autoScalingGroupName' - The name of the Auto Scaling group.
-- * 'shouldDecrementDesiredCapacity' - Indicates whether to decrement the desired capacity of the Auto Scaling group by the number of instances moved to @Standby@ mode.
mkEnterStandby ::
  -- | 'autoScalingGroupName'
  Lude.Text ->
  -- | 'shouldDecrementDesiredCapacity'
  Lude.Bool ->
  EnterStandby
mkEnterStandby
  pAutoScalingGroupName_
  pShouldDecrementDesiredCapacity_ =
    EnterStandby'
      { instanceIds = Lude.Nothing,
        autoScalingGroupName = pAutoScalingGroupName_,
        shouldDecrementDesiredCapacity = pShouldDecrementDesiredCapacity_
      }

-- | The IDs of the instances. You can specify up to 20 instances.
--
-- /Note:/ Consider using 'instanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esInstanceIds :: Lens.Lens' EnterStandby (Lude.Maybe [Lude.Text])
esInstanceIds = Lens.lens (instanceIds :: EnterStandby -> Lude.Maybe [Lude.Text]) (\s a -> s {instanceIds = a} :: EnterStandby)
{-# DEPRECATED esInstanceIds "Use generic-lens or generic-optics with 'instanceIds' instead." #-}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esAutoScalingGroupName :: Lens.Lens' EnterStandby Lude.Text
esAutoScalingGroupName = Lens.lens (autoScalingGroupName :: EnterStandby -> Lude.Text) (\s a -> s {autoScalingGroupName = a} :: EnterStandby)
{-# DEPRECATED esAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

-- | Indicates whether to decrement the desired capacity of the Auto Scaling group by the number of instances moved to @Standby@ mode.
--
-- /Note:/ Consider using 'shouldDecrementDesiredCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esShouldDecrementDesiredCapacity :: Lens.Lens' EnterStandby Lude.Bool
esShouldDecrementDesiredCapacity = Lens.lens (shouldDecrementDesiredCapacity :: EnterStandby -> Lude.Bool) (\s a -> s {shouldDecrementDesiredCapacity = a} :: EnterStandby)
{-# DEPRECATED esShouldDecrementDesiredCapacity "Use generic-lens or generic-optics with 'shouldDecrementDesiredCapacity' instead." #-}

instance Lude.AWSRequest EnterStandby where
  type Rs EnterStandby = EnterStandbyResponse
  request = Req.postQuery autoScalingService
  response =
    Res.receiveXMLWrapper
      "EnterStandbyResult"
      ( \s h x ->
          EnterStandbyResponse'
            Lude.<$> ( x Lude..@? "Activities" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders EnterStandby where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath EnterStandby where
  toPath = Lude.const "/"

instance Lude.ToQuery EnterStandby where
  toQuery EnterStandby' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("EnterStandby" :: Lude.ByteString),
        "Version" Lude.=: ("2011-01-01" :: Lude.ByteString),
        "InstanceIds"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> instanceIds),
        "AutoScalingGroupName" Lude.=: autoScalingGroupName,
        "ShouldDecrementDesiredCapacity"
          Lude.=: shouldDecrementDesiredCapacity
      ]

-- | /See:/ 'mkEnterStandbyResponse' smart constructor.
data EnterStandbyResponse = EnterStandbyResponse'
  { -- | The activities related to moving instances into @Standby@ mode.
    activities :: Lude.Maybe [Activity],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnterStandbyResponse' with the minimum fields required to make a request.
--
-- * 'activities' - The activities related to moving instances into @Standby@ mode.
-- * 'responseStatus' - The response status code.
mkEnterStandbyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  EnterStandbyResponse
mkEnterStandbyResponse pResponseStatus_ =
  EnterStandbyResponse'
    { activities = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The activities related to moving instances into @Standby@ mode.
--
-- /Note:/ Consider using 'activities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esrsActivities :: Lens.Lens' EnterStandbyResponse (Lude.Maybe [Activity])
esrsActivities = Lens.lens (activities :: EnterStandbyResponse -> Lude.Maybe [Activity]) (\s a -> s {activities = a} :: EnterStandbyResponse)
{-# DEPRECATED esrsActivities "Use generic-lens or generic-optics with 'activities' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esrsResponseStatus :: Lens.Lens' EnterStandbyResponse Lude.Int
esrsResponseStatus = Lens.lens (responseStatus :: EnterStandbyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: EnterStandbyResponse)
{-# DEPRECATED esrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
