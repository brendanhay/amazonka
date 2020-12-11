{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.AttachLoadBalancerToSubnets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more subnets to the set of configured subnets for the specified load balancer.
--
-- The load balancer evenly distributes requests across all registered subnets. For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-manage-subnets.html Add or Remove Subnets for Your Load Balancer in a VPC> in the /Classic Load Balancers Guide/ .
module Network.AWS.ELB.AttachLoadBalancerToSubnets
  ( -- * Creating a request
    AttachLoadBalancerToSubnets (..),
    mkAttachLoadBalancerToSubnets,

    -- ** Request lenses
    albtsLoadBalancerName,
    albtsSubnets,

    -- * Destructuring the response
    AttachLoadBalancerToSubnetsResponse (..),
    mkAttachLoadBalancerToSubnetsResponse,

    -- ** Response lenses
    albtsrsSubnets,
    albtsrsResponseStatus,
  )
where

import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for AttachLoaBalancerToSubnets.
--
-- /See:/ 'mkAttachLoadBalancerToSubnets' smart constructor.
data AttachLoadBalancerToSubnets = AttachLoadBalancerToSubnets'
  { loadBalancerName ::
      Lude.Text,
    subnets :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttachLoadBalancerToSubnets' with the minimum fields required to make a request.
--
-- * 'loadBalancerName' - The name of the load balancer.
-- * 'subnets' - The IDs of the subnets to add. You can add only one subnet per Availability Zone.
mkAttachLoadBalancerToSubnets ::
  -- | 'loadBalancerName'
  Lude.Text ->
  AttachLoadBalancerToSubnets
mkAttachLoadBalancerToSubnets pLoadBalancerName_ =
  AttachLoadBalancerToSubnets'
    { loadBalancerName =
        pLoadBalancerName_,
      subnets = Lude.mempty
    }

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
albtsLoadBalancerName :: Lens.Lens' AttachLoadBalancerToSubnets Lude.Text
albtsLoadBalancerName = Lens.lens (loadBalancerName :: AttachLoadBalancerToSubnets -> Lude.Text) (\s a -> s {loadBalancerName = a} :: AttachLoadBalancerToSubnets)
{-# DEPRECATED albtsLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

-- | The IDs of the subnets to add. You can add only one subnet per Availability Zone.
--
-- /Note:/ Consider using 'subnets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
albtsSubnets :: Lens.Lens' AttachLoadBalancerToSubnets [Lude.Text]
albtsSubnets = Lens.lens (subnets :: AttachLoadBalancerToSubnets -> [Lude.Text]) (\s a -> s {subnets = a} :: AttachLoadBalancerToSubnets)
{-# DEPRECATED albtsSubnets "Use generic-lens or generic-optics with 'subnets' instead." #-}

instance Lude.AWSRequest AttachLoadBalancerToSubnets where
  type
    Rs AttachLoadBalancerToSubnets =
      AttachLoadBalancerToSubnetsResponse
  request = Req.postQuery elbService
  response =
    Res.receiveXMLWrapper
      "AttachLoadBalancerToSubnetsResult"
      ( \s h x ->
          AttachLoadBalancerToSubnetsResponse'
            Lude.<$> ( x Lude..@? "Subnets" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AttachLoadBalancerToSubnets where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AttachLoadBalancerToSubnets where
  toPath = Lude.const "/"

instance Lude.ToQuery AttachLoadBalancerToSubnets where
  toQuery AttachLoadBalancerToSubnets' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("AttachLoadBalancerToSubnets" :: Lude.ByteString),
        "Version" Lude.=: ("2012-06-01" :: Lude.ByteString),
        "LoadBalancerName" Lude.=: loadBalancerName,
        "Subnets" Lude.=: Lude.toQueryList "member" subnets
      ]

-- | Contains the output of AttachLoadBalancerToSubnets.
--
-- /See:/ 'mkAttachLoadBalancerToSubnetsResponse' smart constructor.
data AttachLoadBalancerToSubnetsResponse = AttachLoadBalancerToSubnetsResponse'
  { subnets ::
      Lude.Maybe
        [Lude.Text],
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

-- | Creates a value of 'AttachLoadBalancerToSubnetsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'subnets' - The IDs of the subnets attached to the load balancer.
mkAttachLoadBalancerToSubnetsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AttachLoadBalancerToSubnetsResponse
mkAttachLoadBalancerToSubnetsResponse pResponseStatus_ =
  AttachLoadBalancerToSubnetsResponse'
    { subnets = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The IDs of the subnets attached to the load balancer.
--
-- /Note:/ Consider using 'subnets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
albtsrsSubnets :: Lens.Lens' AttachLoadBalancerToSubnetsResponse (Lude.Maybe [Lude.Text])
albtsrsSubnets = Lens.lens (subnets :: AttachLoadBalancerToSubnetsResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {subnets = a} :: AttachLoadBalancerToSubnetsResponse)
{-# DEPRECATED albtsrsSubnets "Use generic-lens or generic-optics with 'subnets' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
albtsrsResponseStatus :: Lens.Lens' AttachLoadBalancerToSubnetsResponse Lude.Int
albtsrsResponseStatus = Lens.lens (responseStatus :: AttachLoadBalancerToSubnetsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AttachLoadBalancerToSubnetsResponse)
{-# DEPRECATED albtsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
