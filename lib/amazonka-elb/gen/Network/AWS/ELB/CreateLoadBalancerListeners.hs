{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.CreateLoadBalancerListeners
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates one or more listeners for the specified load balancer. If a listener with the specified port does not already exist, it is created; otherwise, the properties of the new listener must match the properties of the existing listener.
--
-- For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-listener-config.html Listeners for Your Classic Load Balancer> in the /Classic Load Balancers Guide/ .
module Network.AWS.ELB.CreateLoadBalancerListeners
    (
    -- * Creating a request
      CreateLoadBalancerListeners (..)
    , mkCreateLoadBalancerListeners
    -- ** Request lenses
    , clblLoadBalancerName
    , clblListeners

    -- * Destructuring the response
    , CreateLoadBalancerListenersResponse (..)
    , mkCreateLoadBalancerListenersResponse
    -- ** Response lenses
    , clblrrsResponseStatus
    ) where

import qualified Network.AWS.ELB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for CreateLoadBalancerListeners.
--
-- /See:/ 'mkCreateLoadBalancerListeners' smart constructor.
data CreateLoadBalancerListeners = CreateLoadBalancerListeners'
  { loadBalancerName :: Types.AccessPointName
    -- ^ The name of the load balancer.
  , listeners :: [Types.Listener]
    -- ^ The listeners.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateLoadBalancerListeners' value with any optional fields omitted.
mkCreateLoadBalancerListeners
    :: Types.AccessPointName -- ^ 'loadBalancerName'
    -> CreateLoadBalancerListeners
mkCreateLoadBalancerListeners loadBalancerName
  = CreateLoadBalancerListeners'{loadBalancerName,
                                 listeners = Core.mempty}

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clblLoadBalancerName :: Lens.Lens' CreateLoadBalancerListeners Types.AccessPointName
clblLoadBalancerName = Lens.field @"loadBalancerName"
{-# INLINEABLE clblLoadBalancerName #-}
{-# DEPRECATED loadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead"  #-}

-- | The listeners.
--
-- /Note:/ Consider using 'listeners' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clblListeners :: Lens.Lens' CreateLoadBalancerListeners [Types.Listener]
clblListeners = Lens.field @"listeners"
{-# INLINEABLE clblListeners #-}
{-# DEPRECATED listeners "Use generic-lens or generic-optics with 'listeners' instead"  #-}

instance Core.ToQuery CreateLoadBalancerListeners where
        toQuery CreateLoadBalancerListeners{..}
          = Core.toQueryPair "Action"
              ("CreateLoadBalancerListeners" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-06-01" :: Core.Text)
              Core.<> Core.toQueryPair "LoadBalancerName" loadBalancerName
              Core.<>
              Core.toQueryPair "Listeners" (Core.toQueryList "member" listeners)

instance Core.ToHeaders CreateLoadBalancerListeners where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateLoadBalancerListeners where
        type Rs CreateLoadBalancerListeners =
             CreateLoadBalancerListenersResponse
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
          = Response.receiveXMLWrapper "CreateLoadBalancerListenersResult"
              (\ s h x ->
                 CreateLoadBalancerListenersResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | Contains the parameters for CreateLoadBalancerListener.
--
-- /See:/ 'mkCreateLoadBalancerListenersResponse' smart constructor.
newtype CreateLoadBalancerListenersResponse = CreateLoadBalancerListenersResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateLoadBalancerListenersResponse' value with any optional fields omitted.
mkCreateLoadBalancerListenersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateLoadBalancerListenersResponse
mkCreateLoadBalancerListenersResponse responseStatus
  = CreateLoadBalancerListenersResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clblrrsResponseStatus :: Lens.Lens' CreateLoadBalancerListenersResponse Core.Int
clblrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE clblrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
