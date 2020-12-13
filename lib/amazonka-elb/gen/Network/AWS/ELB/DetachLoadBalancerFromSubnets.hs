{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.DetachLoadBalancerFromSubnets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified subnets from the set of configured subnets for the load balancer.
--
-- After a subnet is removed, all EC2 instances registered with the load balancer in the removed subnet go into the @OutOfService@ state. Then, the load balancer balances the traffic among the remaining routable subnets.
module Network.AWS.ELB.DetachLoadBalancerFromSubnets
  ( -- * Creating a request
    DetachLoadBalancerFromSubnets (..),
    mkDetachLoadBalancerFromSubnets,

    -- ** Request lenses
    dlbfsLoadBalancerName,
    dlbfsSubnets,

    -- * Destructuring the response
    DetachLoadBalancerFromSubnetsResponse (..),
    mkDetachLoadBalancerFromSubnetsResponse,

    -- ** Response lenses
    dlbfsrsSubnets,
    dlbfsrsResponseStatus,
  )
where

import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for DetachLoadBalancerFromSubnets.
--
-- /See:/ 'mkDetachLoadBalancerFromSubnets' smart constructor.
data DetachLoadBalancerFromSubnets = DetachLoadBalancerFromSubnets'
  { -- | The name of the load balancer.
    loadBalancerName :: Lude.Text,
    -- | The IDs of the subnets.
    subnets :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetachLoadBalancerFromSubnets' with the minimum fields required to make a request.
--
-- * 'loadBalancerName' - The name of the load balancer.
-- * 'subnets' - The IDs of the subnets.
mkDetachLoadBalancerFromSubnets ::
  -- | 'loadBalancerName'
  Lude.Text ->
  DetachLoadBalancerFromSubnets
mkDetachLoadBalancerFromSubnets pLoadBalancerName_ =
  DetachLoadBalancerFromSubnets'
    { loadBalancerName =
        pLoadBalancerName_,
      subnets = Lude.mempty
    }

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbfsLoadBalancerName :: Lens.Lens' DetachLoadBalancerFromSubnets Lude.Text
dlbfsLoadBalancerName = Lens.lens (loadBalancerName :: DetachLoadBalancerFromSubnets -> Lude.Text) (\s a -> s {loadBalancerName = a} :: DetachLoadBalancerFromSubnets)
{-# DEPRECATED dlbfsLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

-- | The IDs of the subnets.
--
-- /Note:/ Consider using 'subnets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbfsSubnets :: Lens.Lens' DetachLoadBalancerFromSubnets [Lude.Text]
dlbfsSubnets = Lens.lens (subnets :: DetachLoadBalancerFromSubnets -> [Lude.Text]) (\s a -> s {subnets = a} :: DetachLoadBalancerFromSubnets)
{-# DEPRECATED dlbfsSubnets "Use generic-lens or generic-optics with 'subnets' instead." #-}

instance Lude.AWSRequest DetachLoadBalancerFromSubnets where
  type
    Rs DetachLoadBalancerFromSubnets =
      DetachLoadBalancerFromSubnetsResponse
  request = Req.postQuery elbService
  response =
    Res.receiveXMLWrapper
      "DetachLoadBalancerFromSubnetsResult"
      ( \s h x ->
          DetachLoadBalancerFromSubnetsResponse'
            Lude.<$> ( x Lude..@? "Subnets" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DetachLoadBalancerFromSubnets where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DetachLoadBalancerFromSubnets where
  toPath = Lude.const "/"

instance Lude.ToQuery DetachLoadBalancerFromSubnets where
  toQuery DetachLoadBalancerFromSubnets' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DetachLoadBalancerFromSubnets" :: Lude.ByteString),
        "Version" Lude.=: ("2012-06-01" :: Lude.ByteString),
        "LoadBalancerName" Lude.=: loadBalancerName,
        "Subnets" Lude.=: Lude.toQueryList "member" subnets
      ]

-- | Contains the output of DetachLoadBalancerFromSubnets.
--
-- /See:/ 'mkDetachLoadBalancerFromSubnetsResponse' smart constructor.
data DetachLoadBalancerFromSubnetsResponse = DetachLoadBalancerFromSubnetsResponse'
  { -- | The IDs of the remaining subnets for the load balancer.
    subnets :: Lude.Maybe [Lude.Text],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetachLoadBalancerFromSubnetsResponse' with the minimum fields required to make a request.
--
-- * 'subnets' - The IDs of the remaining subnets for the load balancer.
-- * 'responseStatus' - The response status code.
mkDetachLoadBalancerFromSubnetsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DetachLoadBalancerFromSubnetsResponse
mkDetachLoadBalancerFromSubnetsResponse pResponseStatus_ =
  DetachLoadBalancerFromSubnetsResponse'
    { subnets = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The IDs of the remaining subnets for the load balancer.
--
-- /Note:/ Consider using 'subnets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbfsrsSubnets :: Lens.Lens' DetachLoadBalancerFromSubnetsResponse (Lude.Maybe [Lude.Text])
dlbfsrsSubnets = Lens.lens (subnets :: DetachLoadBalancerFromSubnetsResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {subnets = a} :: DetachLoadBalancerFromSubnetsResponse)
{-# DEPRECATED dlbfsrsSubnets "Use generic-lens or generic-optics with 'subnets' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbfsrsResponseStatus :: Lens.Lens' DetachLoadBalancerFromSubnetsResponse Lude.Int
dlbfsrsResponseStatus = Lens.lens (responseStatus :: DetachLoadBalancerFromSubnetsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DetachLoadBalancerFromSubnetsResponse)
{-# DEPRECATED dlbfsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
