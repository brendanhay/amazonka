{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.AttachLoadBalancers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches one or more Classic Load Balancers to the specified Auto Scaling group. Amazon EC2 Auto Scaling registers the running instances with these Classic Load Balancers.
--
-- To describe the load balancers for an Auto Scaling group, call the 'DescribeLoadBalancers' API. To detach the load balancer from the Auto Scaling group, call the 'DetachLoadBalancers' API.
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-load-balancer.html Elastic Load Balancing and Amazon EC2 Auto Scaling> in the /Amazon EC2 Auto Scaling User Guide/ .
module Network.AWS.AutoScaling.AttachLoadBalancers
  ( -- * Creating a request
    AttachLoadBalancers (..),
    mkAttachLoadBalancers,

    -- ** Request lenses
    albAutoScalingGroupName,
    albLoadBalancerNames,

    -- * Destructuring the response
    AttachLoadBalancersResponse (..),
    mkAttachLoadBalancersResponse,

    -- ** Response lenses
    albrsResponseStatus,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAttachLoadBalancers' smart constructor.
data AttachLoadBalancers = AttachLoadBalancers'
  { autoScalingGroupName ::
      Lude.Text,
    loadBalancerNames :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttachLoadBalancers' with the minimum fields required to make a request.
--
-- * 'autoScalingGroupName' - The name of the Auto Scaling group.
-- * 'loadBalancerNames' - The names of the load balancers. You can specify up to 10 load balancers.
mkAttachLoadBalancers ::
  -- | 'autoScalingGroupName'
  Lude.Text ->
  AttachLoadBalancers
mkAttachLoadBalancers pAutoScalingGroupName_ =
  AttachLoadBalancers'
    { autoScalingGroupName =
        pAutoScalingGroupName_,
      loadBalancerNames = Lude.mempty
    }

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
albAutoScalingGroupName :: Lens.Lens' AttachLoadBalancers Lude.Text
albAutoScalingGroupName = Lens.lens (autoScalingGroupName :: AttachLoadBalancers -> Lude.Text) (\s a -> s {autoScalingGroupName = a} :: AttachLoadBalancers)
{-# DEPRECATED albAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

-- | The names of the load balancers. You can specify up to 10 load balancers.
--
-- /Note:/ Consider using 'loadBalancerNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
albLoadBalancerNames :: Lens.Lens' AttachLoadBalancers [Lude.Text]
albLoadBalancerNames = Lens.lens (loadBalancerNames :: AttachLoadBalancers -> [Lude.Text]) (\s a -> s {loadBalancerNames = a} :: AttachLoadBalancers)
{-# DEPRECATED albLoadBalancerNames "Use generic-lens or generic-optics with 'loadBalancerNames' instead." #-}

instance Lude.AWSRequest AttachLoadBalancers where
  type Rs AttachLoadBalancers = AttachLoadBalancersResponse
  request = Req.postQuery autoScalingService
  response =
    Res.receiveXMLWrapper
      "AttachLoadBalancersResult"
      ( \s h x ->
          AttachLoadBalancersResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AttachLoadBalancers where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AttachLoadBalancers where
  toPath = Lude.const "/"

instance Lude.ToQuery AttachLoadBalancers where
  toQuery AttachLoadBalancers' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("AttachLoadBalancers" :: Lude.ByteString),
        "Version" Lude.=: ("2011-01-01" :: Lude.ByteString),
        "AutoScalingGroupName" Lude.=: autoScalingGroupName,
        "LoadBalancerNames"
          Lude.=: Lude.toQueryList "member" loadBalancerNames
      ]

-- | /See:/ 'mkAttachLoadBalancersResponse' smart constructor.
newtype AttachLoadBalancersResponse = AttachLoadBalancersResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttachLoadBalancersResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAttachLoadBalancersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AttachLoadBalancersResponse
mkAttachLoadBalancersResponse pResponseStatus_ =
  AttachLoadBalancersResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
albrsResponseStatus :: Lens.Lens' AttachLoadBalancersResponse Lude.Int
albrsResponseStatus = Lens.lens (responseStatus :: AttachLoadBalancersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AttachLoadBalancersResponse)
{-# DEPRECATED albrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
