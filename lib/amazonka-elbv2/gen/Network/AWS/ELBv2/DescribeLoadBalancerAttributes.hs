{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.DescribeLoadBalancerAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the attributes for the specified Application Load Balancer, Network Load Balancer, or Gateway Load Balancer.
--
-- For more information, see the following:
--
--     * <https://docs.aws.amazon.com/elasticloadbalancing/latest/application/application-load-balancers.html#load-balancer-attributes Load balancer attributes> in the /Application Load Balancers Guide/
--
--
--     * <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/network-load-balancers.html#load-balancer-attributes Load balancer attributes> in the /Network Load Balancers Guide/
--
--
--     * <https://docs.aws.amazon.com/elasticloadbalancing/latest/gateway/gateway-load-balancers.html#load-balancer-attributes Load balancer attributes> in the /Gateway Load Balancers Guide/
module Network.AWS.ELBv2.DescribeLoadBalancerAttributes
  ( -- * Creating a request
    DescribeLoadBalancerAttributes (..),
    mkDescribeLoadBalancerAttributes,

    -- ** Request lenses
    dlbaLoadBalancerARN,

    -- * Destructuring the response
    DescribeLoadBalancerAttributesResponse (..),
    mkDescribeLoadBalancerAttributesResponse,

    -- ** Response lenses
    dlbarsAttributes,
    dlbarsResponseStatus,
  )
where

import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeLoadBalancerAttributes' smart constructor.
newtype DescribeLoadBalancerAttributes = DescribeLoadBalancerAttributes'
  { loadBalancerARN ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLoadBalancerAttributes' with the minimum fields required to make a request.
--
-- * 'loadBalancerARN' - The Amazon Resource Name (ARN) of the load balancer.
mkDescribeLoadBalancerAttributes ::
  -- | 'loadBalancerARN'
  Lude.Text ->
  DescribeLoadBalancerAttributes
mkDescribeLoadBalancerAttributes pLoadBalancerARN_ =
  DescribeLoadBalancerAttributes'
    { loadBalancerARN =
        pLoadBalancerARN_
    }

-- | The Amazon Resource Name (ARN) of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbaLoadBalancerARN :: Lens.Lens' DescribeLoadBalancerAttributes Lude.Text
dlbaLoadBalancerARN = Lens.lens (loadBalancerARN :: DescribeLoadBalancerAttributes -> Lude.Text) (\s a -> s {loadBalancerARN = a} :: DescribeLoadBalancerAttributes)
{-# DEPRECATED dlbaLoadBalancerARN "Use generic-lens or generic-optics with 'loadBalancerARN' instead." #-}

instance Lude.AWSRequest DescribeLoadBalancerAttributes where
  type
    Rs DescribeLoadBalancerAttributes =
      DescribeLoadBalancerAttributesResponse
  request = Req.postQuery eLBv2Service
  response =
    Res.receiveXMLWrapper
      "DescribeLoadBalancerAttributesResult"
      ( \s h x ->
          DescribeLoadBalancerAttributesResponse'
            Lude.<$> ( x Lude..@? "Attributes" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeLoadBalancerAttributes where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeLoadBalancerAttributes where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeLoadBalancerAttributes where
  toQuery DescribeLoadBalancerAttributes' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeLoadBalancerAttributes" :: Lude.ByteString),
        "Version" Lude.=: ("2015-12-01" :: Lude.ByteString),
        "LoadBalancerArn" Lude.=: loadBalancerARN
      ]

-- | /See:/ 'mkDescribeLoadBalancerAttributesResponse' smart constructor.
data DescribeLoadBalancerAttributesResponse = DescribeLoadBalancerAttributesResponse'
  { attributes ::
      Lude.Maybe
        [LoadBalancerAttribute],
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

-- | Creates a value of 'DescribeLoadBalancerAttributesResponse' with the minimum fields required to make a request.
--
-- * 'attributes' - Information about the load balancer attributes.
-- * 'responseStatus' - The response status code.
mkDescribeLoadBalancerAttributesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeLoadBalancerAttributesResponse
mkDescribeLoadBalancerAttributesResponse pResponseStatus_ =
  DescribeLoadBalancerAttributesResponse'
    { attributes =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the load balancer attributes.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbarsAttributes :: Lens.Lens' DescribeLoadBalancerAttributesResponse (Lude.Maybe [LoadBalancerAttribute])
dlbarsAttributes = Lens.lens (attributes :: DescribeLoadBalancerAttributesResponse -> Lude.Maybe [LoadBalancerAttribute]) (\s a -> s {attributes = a} :: DescribeLoadBalancerAttributesResponse)
{-# DEPRECATED dlbarsAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbarsResponseStatus :: Lens.Lens' DescribeLoadBalancerAttributesResponse Lude.Int
dlbarsResponseStatus = Lens.lens (responseStatus :: DescribeLoadBalancerAttributesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeLoadBalancerAttributesResponse)
{-# DEPRECATED dlbarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
