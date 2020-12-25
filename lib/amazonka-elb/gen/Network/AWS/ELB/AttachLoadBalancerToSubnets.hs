{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    albtsrrsSubnets,
    albtsrrsResponseStatus,
  )
where

import qualified Network.AWS.ELB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for AttachLoaBalancerToSubnets.
--
-- /See:/ 'mkAttachLoadBalancerToSubnets' smart constructor.
data AttachLoadBalancerToSubnets = AttachLoadBalancerToSubnets'
  { -- | The name of the load balancer.
    loadBalancerName :: Types.LoadBalancerName,
    -- | The IDs of the subnets to add. You can add only one subnet per Availability Zone.
    subnets :: [Types.SubnetId]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttachLoadBalancerToSubnets' value with any optional fields omitted.
mkAttachLoadBalancerToSubnets ::
  -- | 'loadBalancerName'
  Types.LoadBalancerName ->
  AttachLoadBalancerToSubnets
mkAttachLoadBalancerToSubnets loadBalancerName =
  AttachLoadBalancerToSubnets'
    { loadBalancerName,
      subnets = Core.mempty
    }

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
albtsLoadBalancerName :: Lens.Lens' AttachLoadBalancerToSubnets Types.LoadBalancerName
albtsLoadBalancerName = Lens.field @"loadBalancerName"
{-# DEPRECATED albtsLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

-- | The IDs of the subnets to add. You can add only one subnet per Availability Zone.
--
-- /Note:/ Consider using 'subnets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
albtsSubnets :: Lens.Lens' AttachLoadBalancerToSubnets [Types.SubnetId]
albtsSubnets = Lens.field @"subnets"
{-# DEPRECATED albtsSubnets "Use generic-lens or generic-optics with 'subnets' instead." #-}

instance Core.AWSRequest AttachLoadBalancerToSubnets where
  type
    Rs AttachLoadBalancerToSubnets =
      AttachLoadBalancerToSubnetsResponse
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
            ( Core.pure ("Action", "AttachLoadBalancerToSubnets")
                Core.<> (Core.pure ("Version", "2012-06-01"))
                Core.<> (Core.toQueryValue "LoadBalancerName" loadBalancerName)
                Core.<> (Core.toQueryValue "Subnets" (Core.toQueryList "member" subnets))
            )
      }
  response =
    Response.receiveXMLWrapper
      "AttachLoadBalancerToSubnetsResult"
      ( \s h x ->
          AttachLoadBalancerToSubnetsResponse'
            Core.<$> (x Core..@? "Subnets" Core..<@> Core.parseXMLList "member")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the output of AttachLoadBalancerToSubnets.
--
-- /See:/ 'mkAttachLoadBalancerToSubnetsResponse' smart constructor.
data AttachLoadBalancerToSubnetsResponse = AttachLoadBalancerToSubnetsResponse'
  { -- | The IDs of the subnets attached to the load balancer.
    subnets :: Core.Maybe [Types.SubnetId],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttachLoadBalancerToSubnetsResponse' value with any optional fields omitted.
mkAttachLoadBalancerToSubnetsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AttachLoadBalancerToSubnetsResponse
mkAttachLoadBalancerToSubnetsResponse responseStatus =
  AttachLoadBalancerToSubnetsResponse'
    { subnets = Core.Nothing,
      responseStatus
    }

-- | The IDs of the subnets attached to the load balancer.
--
-- /Note:/ Consider using 'subnets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
albtsrrsSubnets :: Lens.Lens' AttachLoadBalancerToSubnetsResponse (Core.Maybe [Types.SubnetId])
albtsrrsSubnets = Lens.field @"subnets"
{-# DEPRECATED albtsrrsSubnets "Use generic-lens or generic-optics with 'subnets' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
albtsrrsResponseStatus :: Lens.Lens' AttachLoadBalancerToSubnetsResponse Core.Int
albtsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED albtsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
