{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DetachLoadBalancers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches one or more Classic Load Balancers from the specified Auto Scaling group.
--
-- This operation detaches only Classic Load Balancers. If you have Application Load Balancers, Network Load Balancers, or Gateway Load Balancers, use the 'DetachLoadBalancerTargetGroups' API instead.
-- When you detach a load balancer, it enters the @Removing@ state while deregistering the instances in the group. When all instances are deregistered, then you can no longer describe the load balancer using the 'DescribeLoadBalancers' API call. The instances remain running.
module Network.AWS.AutoScaling.DetachLoadBalancers
  ( -- * Creating a request
    DetachLoadBalancers (..),
    mkDetachLoadBalancers,

    -- ** Request lenses
    dAutoScalingGroupName,
    dLoadBalancerNames,

    -- * Destructuring the response
    DetachLoadBalancersResponse (..),
    mkDetachLoadBalancersResponse,

    -- ** Response lenses
    dlbsrsResponseStatus,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDetachLoadBalancers' smart constructor.
data DetachLoadBalancers = DetachLoadBalancers'
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

-- | Creates a value of 'DetachLoadBalancers' with the minimum fields required to make a request.
--
-- * 'autoScalingGroupName' - The name of the Auto Scaling group.
-- * 'loadBalancerNames' - The names of the load balancers. You can specify up to 10 load balancers.
mkDetachLoadBalancers ::
  -- | 'autoScalingGroupName'
  Lude.Text ->
  DetachLoadBalancers
mkDetachLoadBalancers pAutoScalingGroupName_ =
  DetachLoadBalancers'
    { autoScalingGroupName =
        pAutoScalingGroupName_,
      loadBalancerNames = Lude.mempty
    }

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAutoScalingGroupName :: Lens.Lens' DetachLoadBalancers Lude.Text
dAutoScalingGroupName = Lens.lens (autoScalingGroupName :: DetachLoadBalancers -> Lude.Text) (\s a -> s {autoScalingGroupName = a} :: DetachLoadBalancers)
{-# DEPRECATED dAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

-- | The names of the load balancers. You can specify up to 10 load balancers.
--
-- /Note:/ Consider using 'loadBalancerNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dLoadBalancerNames :: Lens.Lens' DetachLoadBalancers [Lude.Text]
dLoadBalancerNames = Lens.lens (loadBalancerNames :: DetachLoadBalancers -> [Lude.Text]) (\s a -> s {loadBalancerNames = a} :: DetachLoadBalancers)
{-# DEPRECATED dLoadBalancerNames "Use generic-lens or generic-optics with 'loadBalancerNames' instead." #-}

instance Lude.AWSRequest DetachLoadBalancers where
  type Rs DetachLoadBalancers = DetachLoadBalancersResponse
  request = Req.postQuery autoScalingService
  response =
    Res.receiveXMLWrapper
      "DetachLoadBalancersResult"
      ( \s h x ->
          DetachLoadBalancersResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DetachLoadBalancers where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DetachLoadBalancers where
  toPath = Lude.const "/"

instance Lude.ToQuery DetachLoadBalancers where
  toQuery DetachLoadBalancers' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DetachLoadBalancers" :: Lude.ByteString),
        "Version" Lude.=: ("2011-01-01" :: Lude.ByteString),
        "AutoScalingGroupName" Lude.=: autoScalingGroupName,
        "LoadBalancerNames"
          Lude.=: Lude.toQueryList "member" loadBalancerNames
      ]

-- | /See:/ 'mkDetachLoadBalancersResponse' smart constructor.
newtype DetachLoadBalancersResponse = DetachLoadBalancersResponse'
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

-- | Creates a value of 'DetachLoadBalancersResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDetachLoadBalancersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DetachLoadBalancersResponse
mkDetachLoadBalancersResponse pResponseStatus_ =
  DetachLoadBalancersResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbsrsResponseStatus :: Lens.Lens' DetachLoadBalancersResponse Lude.Int
dlbsrsResponseStatus = Lens.lens (responseStatus :: DetachLoadBalancersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DetachLoadBalancersResponse)
{-# DEPRECATED dlbsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
