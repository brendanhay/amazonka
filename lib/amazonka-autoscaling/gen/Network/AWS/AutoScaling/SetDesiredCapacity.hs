{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.SetDesiredCapacity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the size of the specified Auto Scaling group.
--
-- If a scale-in activity occurs as a result of a new @DesiredCapacity@ value that is lower than the current size of the group, the Auto Scaling group uses its termination policy to determine which instances to terminate.
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-manual-scaling.html Manual scaling> in the /Amazon EC2 Auto Scaling User Guide/ .
module Network.AWS.AutoScaling.SetDesiredCapacity
  ( -- * Creating a request
    SetDesiredCapacity (..),
    mkSetDesiredCapacity,

    -- ** Request lenses
    sdcHonorCooldown,
    sdcDesiredCapacity,
    sdcAutoScalingGroupName,

    -- * Destructuring the response
    SetDesiredCapacityResponse (..),
    mkSetDesiredCapacityResponse,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSetDesiredCapacity' smart constructor.
data SetDesiredCapacity = SetDesiredCapacity'
  { -- | Indicates whether Amazon EC2 Auto Scaling waits for the cooldown period to complete before initiating a scaling activity to set your Auto Scaling group to its new capacity. By default, Amazon EC2 Auto Scaling does not honor the cooldown period during manual scaling activities.
    honorCooldown :: Lude.Maybe Lude.Bool,
    -- | The desired capacity is the initial capacity of the Auto Scaling group after this operation completes and the capacity it attempts to maintain.
    desiredCapacity :: Lude.Int,
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetDesiredCapacity' with the minimum fields required to make a request.
--
-- * 'honorCooldown' - Indicates whether Amazon EC2 Auto Scaling waits for the cooldown period to complete before initiating a scaling activity to set your Auto Scaling group to its new capacity. By default, Amazon EC2 Auto Scaling does not honor the cooldown period during manual scaling activities.
-- * 'desiredCapacity' - The desired capacity is the initial capacity of the Auto Scaling group after this operation completes and the capacity it attempts to maintain.
-- * 'autoScalingGroupName' - The name of the Auto Scaling group.
mkSetDesiredCapacity ::
  -- | 'desiredCapacity'
  Lude.Int ->
  -- | 'autoScalingGroupName'
  Lude.Text ->
  SetDesiredCapacity
mkSetDesiredCapacity pDesiredCapacity_ pAutoScalingGroupName_ =
  SetDesiredCapacity'
    { honorCooldown = Lude.Nothing,
      desiredCapacity = pDesiredCapacity_,
      autoScalingGroupName = pAutoScalingGroupName_
    }

-- | Indicates whether Amazon EC2 Auto Scaling waits for the cooldown period to complete before initiating a scaling activity to set your Auto Scaling group to its new capacity. By default, Amazon EC2 Auto Scaling does not honor the cooldown period during manual scaling activities.
--
-- /Note:/ Consider using 'honorCooldown' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcHonorCooldown :: Lens.Lens' SetDesiredCapacity (Lude.Maybe Lude.Bool)
sdcHonorCooldown = Lens.lens (honorCooldown :: SetDesiredCapacity -> Lude.Maybe Lude.Bool) (\s a -> s {honorCooldown = a} :: SetDesiredCapacity)
{-# DEPRECATED sdcHonorCooldown "Use generic-lens or generic-optics with 'honorCooldown' instead." #-}

-- | The desired capacity is the initial capacity of the Auto Scaling group after this operation completes and the capacity it attempts to maintain.
--
-- /Note:/ Consider using 'desiredCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcDesiredCapacity :: Lens.Lens' SetDesiredCapacity Lude.Int
sdcDesiredCapacity = Lens.lens (desiredCapacity :: SetDesiredCapacity -> Lude.Int) (\s a -> s {desiredCapacity = a} :: SetDesiredCapacity)
{-# DEPRECATED sdcDesiredCapacity "Use generic-lens or generic-optics with 'desiredCapacity' instead." #-}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcAutoScalingGroupName :: Lens.Lens' SetDesiredCapacity Lude.Text
sdcAutoScalingGroupName = Lens.lens (autoScalingGroupName :: SetDesiredCapacity -> Lude.Text) (\s a -> s {autoScalingGroupName = a} :: SetDesiredCapacity)
{-# DEPRECATED sdcAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

instance Lude.AWSRequest SetDesiredCapacity where
  type Rs SetDesiredCapacity = SetDesiredCapacityResponse
  request = Req.postQuery autoScalingService
  response = Res.receiveNull SetDesiredCapacityResponse'

instance Lude.ToHeaders SetDesiredCapacity where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath SetDesiredCapacity where
  toPath = Lude.const "/"

instance Lude.ToQuery SetDesiredCapacity where
  toQuery SetDesiredCapacity' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("SetDesiredCapacity" :: Lude.ByteString),
        "Version" Lude.=: ("2011-01-01" :: Lude.ByteString),
        "HonorCooldown" Lude.=: honorCooldown,
        "DesiredCapacity" Lude.=: desiredCapacity,
        "AutoScalingGroupName" Lude.=: autoScalingGroupName
      ]

-- | /See:/ 'mkSetDesiredCapacityResponse' smart constructor.
data SetDesiredCapacityResponse = SetDesiredCapacityResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetDesiredCapacityResponse' with the minimum fields required to make a request.
mkSetDesiredCapacityResponse ::
  SetDesiredCapacityResponse
mkSetDesiredCapacityResponse = SetDesiredCapacityResponse'
