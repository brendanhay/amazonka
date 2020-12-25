{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    dlbaLoadBalancerArn,

    -- * Destructuring the response
    DescribeLoadBalancerAttributesResponse (..),
    mkDescribeLoadBalancerAttributesResponse,

    -- ** Response lenses
    dlbarrsAttributes,
    dlbarrsResponseStatus,
  )
where

import qualified Network.AWS.ELBv2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeLoadBalancerAttributes' smart constructor.
newtype DescribeLoadBalancerAttributes = DescribeLoadBalancerAttributes'
  { -- | The Amazon Resource Name (ARN) of the load balancer.
    loadBalancerArn :: Types.LoadBalancerArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLoadBalancerAttributes' value with any optional fields omitted.
mkDescribeLoadBalancerAttributes ::
  -- | 'loadBalancerArn'
  Types.LoadBalancerArn ->
  DescribeLoadBalancerAttributes
mkDescribeLoadBalancerAttributes loadBalancerArn =
  DescribeLoadBalancerAttributes' {loadBalancerArn}

-- | The Amazon Resource Name (ARN) of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbaLoadBalancerArn :: Lens.Lens' DescribeLoadBalancerAttributes Types.LoadBalancerArn
dlbaLoadBalancerArn = Lens.field @"loadBalancerArn"
{-# DEPRECATED dlbaLoadBalancerArn "Use generic-lens or generic-optics with 'loadBalancerArn' instead." #-}

instance Core.AWSRequest DescribeLoadBalancerAttributes where
  type
    Rs DescribeLoadBalancerAttributes =
      DescribeLoadBalancerAttributesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DescribeLoadBalancerAttributes")
                Core.<> (Core.pure ("Version", "2015-12-01"))
                Core.<> (Core.toQueryValue "LoadBalancerArn" loadBalancerArn)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeLoadBalancerAttributesResult"
      ( \s h x ->
          DescribeLoadBalancerAttributesResponse'
            Core.<$> (x Core..@? "Attributes" Core..<@> Core.parseXMLList "member")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeLoadBalancerAttributesResponse' smart constructor.
data DescribeLoadBalancerAttributesResponse = DescribeLoadBalancerAttributesResponse'
  { -- | Information about the load balancer attributes.
    attributes :: Core.Maybe [Types.LoadBalancerAttribute],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLoadBalancerAttributesResponse' value with any optional fields omitted.
mkDescribeLoadBalancerAttributesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeLoadBalancerAttributesResponse
mkDescribeLoadBalancerAttributesResponse responseStatus =
  DescribeLoadBalancerAttributesResponse'
    { attributes =
        Core.Nothing,
      responseStatus
    }

-- | Information about the load balancer attributes.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbarrsAttributes :: Lens.Lens' DescribeLoadBalancerAttributesResponse (Core.Maybe [Types.LoadBalancerAttribute])
dlbarrsAttributes = Lens.field @"attributes"
{-# DEPRECATED dlbarrsAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbarrsResponseStatus :: Lens.Lens' DescribeLoadBalancerAttributesResponse Core.Int
dlbarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dlbarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
