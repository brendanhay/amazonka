{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.AttachInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches one or more EC2 instances to the specified Auto Scaling group.
--
-- When you attach instances, Amazon EC2 Auto Scaling increases the desired capacity of the group by the number of instances being attached. If the number of instances being attached plus the desired capacity of the group exceeds the maximum size of the group, the operation fails.
-- If there is a Classic Load Balancer attached to your Auto Scaling group, the instances are also registered with the load balancer. If there are target groups attached to your Auto Scaling group, the instances are also registered with the target groups.
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/attach-instance-asg.html Attach EC2 instances to your Auto Scaling group> in the /Amazon EC2 Auto Scaling User Guide/ .
module Network.AWS.AutoScaling.AttachInstances
  ( -- * Creating a request
    AttachInstances (..),
    mkAttachInstances,

    -- ** Request lenses
    aiInstanceIds,
    aiAutoScalingGroupName,

    -- * Destructuring the response
    AttachInstancesResponse (..),
    mkAttachInstancesResponse,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAttachInstances' smart constructor.
data AttachInstances = AttachInstances'
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

-- | Creates a value of 'AttachInstances' with the minimum fields required to make a request.
--
-- * 'autoScalingGroupName' - The name of the Auto Scaling group.
-- * 'instanceIds' - The IDs of the instances. You can specify up to 20 instances.
mkAttachInstances ::
  -- | 'autoScalingGroupName'
  Lude.Text ->
  AttachInstances
mkAttachInstances pAutoScalingGroupName_ =
  AttachInstances'
    { instanceIds = Lude.Nothing,
      autoScalingGroupName = pAutoScalingGroupName_
    }

-- | The IDs of the instances. You can specify up to 20 instances.
--
-- /Note:/ Consider using 'instanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiInstanceIds :: Lens.Lens' AttachInstances (Lude.Maybe [Lude.Text])
aiInstanceIds = Lens.lens (instanceIds :: AttachInstances -> Lude.Maybe [Lude.Text]) (\s a -> s {instanceIds = a} :: AttachInstances)
{-# DEPRECATED aiInstanceIds "Use generic-lens or generic-optics with 'instanceIds' instead." #-}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiAutoScalingGroupName :: Lens.Lens' AttachInstances Lude.Text
aiAutoScalingGroupName = Lens.lens (autoScalingGroupName :: AttachInstances -> Lude.Text) (\s a -> s {autoScalingGroupName = a} :: AttachInstances)
{-# DEPRECATED aiAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

instance Lude.AWSRequest AttachInstances where
  type Rs AttachInstances = AttachInstancesResponse
  request = Req.postQuery autoScalingService
  response = Res.receiveNull AttachInstancesResponse'

instance Lude.ToHeaders AttachInstances where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AttachInstances where
  toPath = Lude.const "/"

instance Lude.ToQuery AttachInstances where
  toQuery AttachInstances' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("AttachInstances" :: Lude.ByteString),
        "Version" Lude.=: ("2011-01-01" :: Lude.ByteString),
        "InstanceIds"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> instanceIds),
        "AutoScalingGroupName" Lude.=: autoScalingGroupName
      ]

-- | /See:/ 'mkAttachInstancesResponse' smart constructor.
data AttachInstancesResponse = AttachInstancesResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttachInstancesResponse' with the minimum fields required to make a request.
mkAttachInstancesResponse ::
  AttachInstancesResponse
mkAttachInstancesResponse = AttachInstancesResponse'
