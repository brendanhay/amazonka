{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.ExitStandby
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Moves the specified instances out of the standby state.
--
-- After you put the instances back in service, the desired capacity is incremented.
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-enter-exit-standby.html Temporarily removing instances from your Auto Scaling group> in the /Amazon EC2 Auto Scaling User Guide/ .
module Network.AWS.AutoScaling.ExitStandby
  ( -- * Creating a request
    ExitStandby (..),
    mkExitStandby,

    -- ** Request lenses
    eInstanceIds,
    eAutoScalingGroupName,

    -- * Destructuring the response
    ExitStandbyResponse (..),
    mkExitStandbyResponse,

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

-- | /See:/ 'mkExitStandby' smart constructor.
data ExitStandby = ExitStandby'
  { instanceIds ::
      Lude.Maybe [Lude.Text],
    autoScalingGroupName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExitStandby' with the minimum fields required to make a request.
--
-- * 'autoScalingGroupName' - The name of the Auto Scaling group.
-- * 'instanceIds' - The IDs of the instances. You can specify up to 20 instances.
mkExitStandby ::
  -- | 'autoScalingGroupName'
  Lude.Text ->
  ExitStandby
mkExitStandby pAutoScalingGroupName_ =
  ExitStandby'
    { instanceIds = Lude.Nothing,
      autoScalingGroupName = pAutoScalingGroupName_
    }

-- | The IDs of the instances. You can specify up to 20 instances.
--
-- /Note:/ Consider using 'instanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eInstanceIds :: Lens.Lens' ExitStandby (Lude.Maybe [Lude.Text])
eInstanceIds = Lens.lens (instanceIds :: ExitStandby -> Lude.Maybe [Lude.Text]) (\s a -> s {instanceIds = a} :: ExitStandby)
{-# DEPRECATED eInstanceIds "Use generic-lens or generic-optics with 'instanceIds' instead." #-}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eAutoScalingGroupName :: Lens.Lens' ExitStandby Lude.Text
eAutoScalingGroupName = Lens.lens (autoScalingGroupName :: ExitStandby -> Lude.Text) (\s a -> s {autoScalingGroupName = a} :: ExitStandby)
{-# DEPRECATED eAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

instance Lude.AWSRequest ExitStandby where
  type Rs ExitStandby = ExitStandbyResponse
  request = Req.postQuery autoScalingService
  response =
    Res.receiveXMLWrapper
      "ExitStandbyResult"
      ( \s h x ->
          ExitStandbyResponse'
            Lude.<$> ( x Lude..@? "Activities" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ExitStandby where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ExitStandby where
  toPath = Lude.const "/"

instance Lude.ToQuery ExitStandby where
  toQuery ExitStandby' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ExitStandby" :: Lude.ByteString),
        "Version" Lude.=: ("2011-01-01" :: Lude.ByteString),
        "InstanceIds"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> instanceIds),
        "AutoScalingGroupName" Lude.=: autoScalingGroupName
      ]

-- | /See:/ 'mkExitStandbyResponse' smart constructor.
data ExitStandbyResponse = ExitStandbyResponse'
  { activities ::
      Lude.Maybe [Activity],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExitStandbyResponse' with the minimum fields required to make a request.
--
-- * 'activities' - The activities related to moving instances out of @Standby@ mode.
-- * 'responseStatus' - The response status code.
mkExitStandbyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ExitStandbyResponse
mkExitStandbyResponse pResponseStatus_ =
  ExitStandbyResponse'
    { activities = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The activities related to moving instances out of @Standby@ mode.
--
-- /Note:/ Consider using 'activities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esrsActivities :: Lens.Lens' ExitStandbyResponse (Lude.Maybe [Activity])
esrsActivities = Lens.lens (activities :: ExitStandbyResponse -> Lude.Maybe [Activity]) (\s a -> s {activities = a} :: ExitStandbyResponse)
{-# DEPRECATED esrsActivities "Use generic-lens or generic-optics with 'activities' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esrsResponseStatus :: Lens.Lens' ExitStandbyResponse Lude.Int
esrsResponseStatus = Lens.lens (responseStatus :: ExitStandbyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ExitStandbyResponse)
{-# DEPRECATED esrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
