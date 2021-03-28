{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      AttachLoadBalancerToSubnets (..)
    , mkAttachLoadBalancerToSubnets
    -- ** Request lenses
    , albtsLoadBalancerName
    , albtsSubnets

    -- * Destructuring the response
    , AttachLoadBalancerToSubnetsResponse (..)
    , mkAttachLoadBalancerToSubnetsResponse
    -- ** Response lenses
    , albtsrrsSubnets
    , albtsrrsResponseStatus
    ) where

import qualified Network.AWS.ELB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for AttachLoaBalancerToSubnets.
--
-- /See:/ 'mkAttachLoadBalancerToSubnets' smart constructor.
data AttachLoadBalancerToSubnets = AttachLoadBalancerToSubnets'
  { loadBalancerName :: Types.LoadBalancerName
    -- ^ The name of the load balancer.
  , subnets :: [Types.SubnetId]
    -- ^ The IDs of the subnets to add. You can add only one subnet per Availability Zone.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttachLoadBalancerToSubnets' value with any optional fields omitted.
mkAttachLoadBalancerToSubnets
    :: Types.LoadBalancerName -- ^ 'loadBalancerName'
    -> AttachLoadBalancerToSubnets
mkAttachLoadBalancerToSubnets loadBalancerName
  = AttachLoadBalancerToSubnets'{loadBalancerName,
                                 subnets = Core.mempty}

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
albtsLoadBalancerName :: Lens.Lens' AttachLoadBalancerToSubnets Types.LoadBalancerName
albtsLoadBalancerName = Lens.field @"loadBalancerName"
{-# INLINEABLE albtsLoadBalancerName #-}
{-# DEPRECATED loadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead"  #-}

-- | The IDs of the subnets to add. You can add only one subnet per Availability Zone.
--
-- /Note:/ Consider using 'subnets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
albtsSubnets :: Lens.Lens' AttachLoadBalancerToSubnets [Types.SubnetId]
albtsSubnets = Lens.field @"subnets"
{-# INLINEABLE albtsSubnets #-}
{-# DEPRECATED subnets "Use generic-lens or generic-optics with 'subnets' instead"  #-}

instance Core.ToQuery AttachLoadBalancerToSubnets where
        toQuery AttachLoadBalancerToSubnets{..}
          = Core.toQueryPair "Action"
              ("AttachLoadBalancerToSubnets" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-06-01" :: Core.Text)
              Core.<> Core.toQueryPair "LoadBalancerName" loadBalancerName
              Core.<>
              Core.toQueryPair "Subnets" (Core.toQueryList "member" subnets)

instance Core.ToHeaders AttachLoadBalancerToSubnets where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest AttachLoadBalancerToSubnets where
        type Rs AttachLoadBalancerToSubnets =
             AttachLoadBalancerToSubnetsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "AttachLoadBalancerToSubnetsResult"
              (\ s h x ->
                 AttachLoadBalancerToSubnetsResponse' Core.<$>
                   (x Core..@? "Subnets" Core..<@> Core.parseXMLList "member")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the output of AttachLoadBalancerToSubnets.
--
-- /See:/ 'mkAttachLoadBalancerToSubnetsResponse' smart constructor.
data AttachLoadBalancerToSubnetsResponse = AttachLoadBalancerToSubnetsResponse'
  { subnets :: Core.Maybe [Types.SubnetId]
    -- ^ The IDs of the subnets attached to the load balancer.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttachLoadBalancerToSubnetsResponse' value with any optional fields omitted.
mkAttachLoadBalancerToSubnetsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AttachLoadBalancerToSubnetsResponse
mkAttachLoadBalancerToSubnetsResponse responseStatus
  = AttachLoadBalancerToSubnetsResponse'{subnets = Core.Nothing,
                                         responseStatus}

-- | The IDs of the subnets attached to the load balancer.
--
-- /Note:/ Consider using 'subnets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
albtsrrsSubnets :: Lens.Lens' AttachLoadBalancerToSubnetsResponse (Core.Maybe [Types.SubnetId])
albtsrrsSubnets = Lens.field @"subnets"
{-# INLINEABLE albtsrrsSubnets #-}
{-# DEPRECATED subnets "Use generic-lens or generic-optics with 'subnets' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
albtsrrsResponseStatus :: Lens.Lens' AttachLoadBalancerToSubnetsResponse Core.Int
albtsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE albtsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
