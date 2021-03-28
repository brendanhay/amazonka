{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DeleteLoadBalancer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Lightsail load balancer and all its associated SSL/TLS certificates. Once the load balancer is deleted, you will need to create a new load balancer, create a new certificate, and verify domain ownership again.
--
-- The @delete load balancer@ operation supports tag-based access control via resource tags applied to the resource identified by @load balancer name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.DeleteLoadBalancer
    (
    -- * Creating a request
      DeleteLoadBalancer (..)
    , mkDeleteLoadBalancer
    -- ** Request lenses
    , dlbLoadBalancerName

    -- * Destructuring the response
    , DeleteLoadBalancerResponse (..)
    , mkDeleteLoadBalancerResponse
    -- ** Response lenses
    , dlbrrsOperations
    , dlbrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteLoadBalancer' smart constructor.
newtype DeleteLoadBalancer = DeleteLoadBalancer'
  { loadBalancerName :: Types.LoadBalancerName
    -- ^ The name of the load balancer you want to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLoadBalancer' value with any optional fields omitted.
mkDeleteLoadBalancer
    :: Types.LoadBalancerName -- ^ 'loadBalancerName'
    -> DeleteLoadBalancer
mkDeleteLoadBalancer loadBalancerName
  = DeleteLoadBalancer'{loadBalancerName}

-- | The name of the load balancer you want to delete.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbLoadBalancerName :: Lens.Lens' DeleteLoadBalancer Types.LoadBalancerName
dlbLoadBalancerName = Lens.field @"loadBalancerName"
{-# INLINEABLE dlbLoadBalancerName #-}
{-# DEPRECATED loadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead"  #-}

instance Core.ToQuery DeleteLoadBalancer where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteLoadBalancer where
        toHeaders DeleteLoadBalancer{..}
          = Core.pure
              ("X-Amz-Target", "Lightsail_20161128.DeleteLoadBalancer")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteLoadBalancer where
        toJSON DeleteLoadBalancer{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("loadBalancerName" Core..= loadBalancerName)])

instance Core.AWSRequest DeleteLoadBalancer where
        type Rs DeleteLoadBalancer = DeleteLoadBalancerResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteLoadBalancerResponse' Core.<$>
                   (x Core..:? "operations") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteLoadBalancerResponse' smart constructor.
data DeleteLoadBalancerResponse = DeleteLoadBalancerResponse'
  { operations :: Core.Maybe [Types.Operation]
    -- ^ An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DeleteLoadBalancerResponse' value with any optional fields omitted.
mkDeleteLoadBalancerResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteLoadBalancerResponse
mkDeleteLoadBalancerResponse responseStatus
  = DeleteLoadBalancerResponse'{operations = Core.Nothing,
                                responseStatus}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbrrsOperations :: Lens.Lens' DeleteLoadBalancerResponse (Core.Maybe [Types.Operation])
dlbrrsOperations = Lens.field @"operations"
{-# INLINEABLE dlbrrsOperations #-}
{-# DEPRECATED operations "Use generic-lens or generic-optics with 'operations' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbrrsResponseStatus :: Lens.Lens' DeleteLoadBalancerResponse Core.Int
dlbrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dlbrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
