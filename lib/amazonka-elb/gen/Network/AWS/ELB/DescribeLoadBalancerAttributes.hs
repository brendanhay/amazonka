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
    dlbarrsLoadBalancerAttributes,
    dlbarrsResponseStatus,
  )
where

import qualified Network.AWS.ELB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DescribeLoadBalancerAttributes.
--
-- /See:/ 'mkDescribeLoadBalancerAttributes' smart constructor.
newtype DescribeLoadBalancerAttributes = DescribeLoadBalancerAttributes'
  { -- | The name of the load balancer.
    loadBalancerName :: Types.AccessPointName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLoadBalancerAttributes' value with any optional fields omitted.
mkDescribeLoadBalancerAttributes ::
  -- | 'loadBalancerName'
  Types.AccessPointName ->
  DescribeLoadBalancerAttributes
mkDescribeLoadBalancerAttributes loadBalancerName =
  DescribeLoadBalancerAttributes' {loadBalancerName}

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbaLoadBalancerName :: Lens.Lens' DescribeLoadBalancerAttributes Types.AccessPointName
dlbaLoadBalancerName = Lens.field @"loadBalancerName"
{-# DEPRECATED dlbaLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

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
                Core.<> (Core.pure ("Version", "2012-06-01"))
                Core.<> (Core.toQueryValue "LoadBalancerName" loadBalancerName)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeLoadBalancerAttributesResult"
      ( \s h x ->
          DescribeLoadBalancerAttributesResponse'
            Core.<$> (x Core..@? "LoadBalancerAttributes")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the output of DescribeLoadBalancerAttributes.
--
-- /See:/ 'mkDescribeLoadBalancerAttributesResponse' smart constructor.
data DescribeLoadBalancerAttributesResponse = DescribeLoadBalancerAttributesResponse'
  { -- | Information about the load balancer attributes.
    loadBalancerAttributes :: Core.Maybe Types.LoadBalancerAttributes,
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
    { loadBalancerAttributes =
        Core.Nothing,
      responseStatus
    }

-- | Information about the load balancer attributes.
--
-- /Note:/ Consider using 'loadBalancerAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbarrsLoadBalancerAttributes :: Lens.Lens' DescribeLoadBalancerAttributesResponse (Core.Maybe Types.LoadBalancerAttributes)
dlbarrsLoadBalancerAttributes = Lens.field @"loadBalancerAttributes"
{-# DEPRECATED dlbarrsLoadBalancerAttributes "Use generic-lens or generic-optics with 'loadBalancerAttributes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbarrsResponseStatus :: Lens.Lens' DescribeLoadBalancerAttributesResponse Core.Int
dlbarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dlbarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
