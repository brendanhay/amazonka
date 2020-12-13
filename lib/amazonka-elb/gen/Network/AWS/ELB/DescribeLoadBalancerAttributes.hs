{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.DescribeLoadBalancerAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the attributes for the specified load balancer.
module Network.AWS.ELB.DescribeLoadBalancerAttributes
  ( -- * Creating a request
    DescribeLoadBalancerAttributes (..),
    mkDescribeLoadBalancerAttributes,

    -- ** Request lenses
    dlbaLoadBalancerName,

    -- * Destructuring the response
    DescribeLoadBalancerAttributesResponse (..),
    mkDescribeLoadBalancerAttributesResponse,

    -- ** Response lenses
    dlbarsLoadBalancerAttributes,
    dlbarsResponseStatus,
  )
where

import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for DescribeLoadBalancerAttributes.
--
-- /See:/ 'mkDescribeLoadBalancerAttributes' smart constructor.
newtype DescribeLoadBalancerAttributes = DescribeLoadBalancerAttributes'
  { -- | The name of the load balancer.
    loadBalancerName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLoadBalancerAttributes' with the minimum fields required to make a request.
--
-- * 'loadBalancerName' - The name of the load balancer.
mkDescribeLoadBalancerAttributes ::
  -- | 'loadBalancerName'
  Lude.Text ->
  DescribeLoadBalancerAttributes
mkDescribeLoadBalancerAttributes pLoadBalancerName_ =
  DescribeLoadBalancerAttributes'
    { loadBalancerName =
        pLoadBalancerName_
    }

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbaLoadBalancerName :: Lens.Lens' DescribeLoadBalancerAttributes Lude.Text
dlbaLoadBalancerName = Lens.lens (loadBalancerName :: DescribeLoadBalancerAttributes -> Lude.Text) (\s a -> s {loadBalancerName = a} :: DescribeLoadBalancerAttributes)
{-# DEPRECATED dlbaLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

instance Lude.AWSRequest DescribeLoadBalancerAttributes where
  type
    Rs DescribeLoadBalancerAttributes =
      DescribeLoadBalancerAttributesResponse
  request = Req.postQuery elbService
  response =
    Res.receiveXMLWrapper
      "DescribeLoadBalancerAttributesResult"
      ( \s h x ->
          DescribeLoadBalancerAttributesResponse'
            Lude.<$> (x Lude..@? "LoadBalancerAttributes")
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
        "Version" Lude.=: ("2012-06-01" :: Lude.ByteString),
        "LoadBalancerName" Lude.=: loadBalancerName
      ]

-- | Contains the output of DescribeLoadBalancerAttributes.
--
-- /See:/ 'mkDescribeLoadBalancerAttributesResponse' smart constructor.
data DescribeLoadBalancerAttributesResponse = DescribeLoadBalancerAttributesResponse'
  { -- | Information about the load balancer attributes.
    loadBalancerAttributes :: Lude.Maybe LoadBalancerAttributes,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLoadBalancerAttributesResponse' with the minimum fields required to make a request.
--
-- * 'loadBalancerAttributes' - Information about the load balancer attributes.
-- * 'responseStatus' - The response status code.
mkDescribeLoadBalancerAttributesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeLoadBalancerAttributesResponse
mkDescribeLoadBalancerAttributesResponse pResponseStatus_ =
  DescribeLoadBalancerAttributesResponse'
    { loadBalancerAttributes =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the load balancer attributes.
--
-- /Note:/ Consider using 'loadBalancerAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbarsLoadBalancerAttributes :: Lens.Lens' DescribeLoadBalancerAttributesResponse (Lude.Maybe LoadBalancerAttributes)
dlbarsLoadBalancerAttributes = Lens.lens (loadBalancerAttributes :: DescribeLoadBalancerAttributesResponse -> Lude.Maybe LoadBalancerAttributes) (\s a -> s {loadBalancerAttributes = a} :: DescribeLoadBalancerAttributesResponse)
{-# DEPRECATED dlbarsLoadBalancerAttributes "Use generic-lens or generic-optics with 'loadBalancerAttributes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbarsResponseStatus :: Lens.Lens' DescribeLoadBalancerAttributesResponse Lude.Int
dlbarsResponseStatus = Lens.lens (responseStatus :: DescribeLoadBalancerAttributesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeLoadBalancerAttributesResponse)
{-# DEPRECATED dlbarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
