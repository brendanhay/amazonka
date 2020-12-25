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
    dlbfsrrsSubnets,
    dlbfsrrsResponseStatus,
  )
where

import qualified Network.AWS.ELB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DetachLoadBalancerFromSubnets.
--
-- /See:/ 'mkDetachLoadBalancerFromSubnets' smart constructor.
data DetachLoadBalancerFromSubnets = DetachLoadBalancerFromSubnets'
  { -- | The name of the load balancer.
    loadBalancerName :: Types.AccessPointName,
    -- | The IDs of the subnets.
    subnets :: [Types.SubnetId]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetachLoadBalancerFromSubnets' value with any optional fields omitted.
mkDetachLoadBalancerFromSubnets ::
  -- | 'loadBalancerName'
  Types.AccessPointName ->
  DetachLoadBalancerFromSubnets
mkDetachLoadBalancerFromSubnets loadBalancerName =
  DetachLoadBalancerFromSubnets'
    { loadBalancerName,
      subnets = Core.mempty
    }

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbfsLoadBalancerName :: Lens.Lens' DetachLoadBalancerFromSubnets Types.AccessPointName
dlbfsLoadBalancerName = Lens.field @"loadBalancerName"
{-# DEPRECATED dlbfsLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

-- | The IDs of the subnets.
--
-- /Note:/ Consider using 'subnets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbfsSubnets :: Lens.Lens' DetachLoadBalancerFromSubnets [Types.SubnetId]
dlbfsSubnets = Lens.field @"subnets"
{-# DEPRECATED dlbfsSubnets "Use generic-lens or generic-optics with 'subnets' instead." #-}

instance Core.AWSRequest DetachLoadBalancerFromSubnets where
  type
    Rs DetachLoadBalancerFromSubnets =
      DetachLoadBalancerFromSubnetsResponse
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
            ( Core.pure ("Action", "DetachLoadBalancerFromSubnets")
                Core.<> (Core.pure ("Version", "2012-06-01"))
                Core.<> (Core.toQueryValue "LoadBalancerName" loadBalancerName)
                Core.<> (Core.toQueryValue "Subnets" (Core.toQueryList "member" subnets))
            )
      }
  response =
    Response.receiveXMLWrapper
      "DetachLoadBalancerFromSubnetsResult"
      ( \s h x ->
          DetachLoadBalancerFromSubnetsResponse'
            Core.<$> (x Core..@? "Subnets" Core..<@> Core.parseXMLList "member")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the output of DetachLoadBalancerFromSubnets.
--
-- /See:/ 'mkDetachLoadBalancerFromSubnetsResponse' smart constructor.
data DetachLoadBalancerFromSubnetsResponse = DetachLoadBalancerFromSubnetsResponse'
  { -- | The IDs of the remaining subnets for the load balancer.
    subnets :: Core.Maybe [Types.SubnetId],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetachLoadBalancerFromSubnetsResponse' value with any optional fields omitted.
mkDetachLoadBalancerFromSubnetsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DetachLoadBalancerFromSubnetsResponse
mkDetachLoadBalancerFromSubnetsResponse responseStatus =
  DetachLoadBalancerFromSubnetsResponse'
    { subnets = Core.Nothing,
      responseStatus
    }

-- | The IDs of the remaining subnets for the load balancer.
--
-- /Note:/ Consider using 'subnets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbfsrrsSubnets :: Lens.Lens' DetachLoadBalancerFromSubnetsResponse (Core.Maybe [Types.SubnetId])
dlbfsrrsSubnets = Lens.field @"subnets"
{-# DEPRECATED dlbfsrrsSubnets "Use generic-lens or generic-optics with 'subnets' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbfsrrsResponseStatus :: Lens.Lens' DetachLoadBalancerFromSubnetsResponse Core.Int
dlbfsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dlbfsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
