{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.ModifyLoadBalancerAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the attributes of the specified load balancer.
--
-- You can modify the load balancer attributes, such as @AccessLogs@ , @ConnectionDraining@ , and @CrossZoneLoadBalancing@ by either enabling or disabling them. Or, you can modify the load balancer attribute @ConnectionSettings@ by specifying an idle connection timeout value for your load balancer.
-- For more information, see the following in the /Classic Load Balancers Guide/ :
--
--     * <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/enable-disable-crosszone-lb.html Cross-Zone Load Balancing> 
--
--
--     * <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/config-conn-drain.html Connection Draining> 
--
--
--     * <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/access-log-collection.html Access Logs> 
--
--
--     * <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/config-idle-timeout.html Idle Connection Timeout> 
--
--
module Network.AWS.ELB.ModifyLoadBalancerAttributes
    (
    -- * Creating a request
      ModifyLoadBalancerAttributes (..)
    , mkModifyLoadBalancerAttributes
    -- ** Request lenses
    , mlbaLoadBalancerName
    , mlbaLoadBalancerAttributes

    -- * Destructuring the response
    , ModifyLoadBalancerAttributesResponse (..)
    , mkModifyLoadBalancerAttributesResponse
    -- ** Response lenses
    , mlbarrsLoadBalancerAttributes
    , mlbarrsLoadBalancerName
    , mlbarrsResponseStatus
    ) where

import qualified Network.AWS.ELB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for ModifyLoadBalancerAttributes.
--
-- /See:/ 'mkModifyLoadBalancerAttributes' smart constructor.
data ModifyLoadBalancerAttributes = ModifyLoadBalancerAttributes'
  { loadBalancerName :: Types.AccessPointName
    -- ^ The name of the load balancer.
  , loadBalancerAttributes :: Types.LoadBalancerAttributes
    -- ^ The attributes for the load balancer.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyLoadBalancerAttributes' value with any optional fields omitted.
mkModifyLoadBalancerAttributes
    :: Types.AccessPointName -- ^ 'loadBalancerName'
    -> Types.LoadBalancerAttributes -- ^ 'loadBalancerAttributes'
    -> ModifyLoadBalancerAttributes
mkModifyLoadBalancerAttributes loadBalancerName
  loadBalancerAttributes
  = ModifyLoadBalancerAttributes'{loadBalancerName,
                                  loadBalancerAttributes}

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlbaLoadBalancerName :: Lens.Lens' ModifyLoadBalancerAttributes Types.AccessPointName
mlbaLoadBalancerName = Lens.field @"loadBalancerName"
{-# INLINEABLE mlbaLoadBalancerName #-}
{-# DEPRECATED loadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead"  #-}

-- | The attributes for the load balancer.
--
-- /Note:/ Consider using 'loadBalancerAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlbaLoadBalancerAttributes :: Lens.Lens' ModifyLoadBalancerAttributes Types.LoadBalancerAttributes
mlbaLoadBalancerAttributes = Lens.field @"loadBalancerAttributes"
{-# INLINEABLE mlbaLoadBalancerAttributes #-}
{-# DEPRECATED loadBalancerAttributes "Use generic-lens or generic-optics with 'loadBalancerAttributes' instead"  #-}

instance Core.ToQuery ModifyLoadBalancerAttributes where
        toQuery ModifyLoadBalancerAttributes{..}
          = Core.toQueryPair "Action"
              ("ModifyLoadBalancerAttributes" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-06-01" :: Core.Text)
              Core.<> Core.toQueryPair "LoadBalancerName" loadBalancerName
              Core.<>
              Core.toQueryPair "LoadBalancerAttributes" loadBalancerAttributes

instance Core.ToHeaders ModifyLoadBalancerAttributes where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyLoadBalancerAttributes where
        type Rs ModifyLoadBalancerAttributes =
             ModifyLoadBalancerAttributesResponse
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
          = Response.receiveXMLWrapper "ModifyLoadBalancerAttributesResult"
              (\ s h x ->
                 ModifyLoadBalancerAttributesResponse' Core.<$>
                   (x Core..@? "LoadBalancerAttributes") Core.<*>
                     x Core..@? "LoadBalancerName"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the output of ModifyLoadBalancerAttributes.
--
-- /See:/ 'mkModifyLoadBalancerAttributesResponse' smart constructor.
data ModifyLoadBalancerAttributesResponse = ModifyLoadBalancerAttributesResponse'
  { loadBalancerAttributes :: Core.Maybe Types.LoadBalancerAttributes
    -- ^ Information about the load balancer attributes.
  , loadBalancerName :: Core.Maybe Types.AccessPointName
    -- ^ The name of the load balancer.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyLoadBalancerAttributesResponse' value with any optional fields omitted.
mkModifyLoadBalancerAttributesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyLoadBalancerAttributesResponse
mkModifyLoadBalancerAttributesResponse responseStatus
  = ModifyLoadBalancerAttributesResponse'{loadBalancerAttributes =
                                            Core.Nothing,
                                          loadBalancerName = Core.Nothing, responseStatus}

-- | Information about the load balancer attributes.
--
-- /Note:/ Consider using 'loadBalancerAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlbarrsLoadBalancerAttributes :: Lens.Lens' ModifyLoadBalancerAttributesResponse (Core.Maybe Types.LoadBalancerAttributes)
mlbarrsLoadBalancerAttributes = Lens.field @"loadBalancerAttributes"
{-# INLINEABLE mlbarrsLoadBalancerAttributes #-}
{-# DEPRECATED loadBalancerAttributes "Use generic-lens or generic-optics with 'loadBalancerAttributes' instead"  #-}

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlbarrsLoadBalancerName :: Lens.Lens' ModifyLoadBalancerAttributesResponse (Core.Maybe Types.AccessPointName)
mlbarrsLoadBalancerName = Lens.field @"loadBalancerName"
{-# INLINEABLE mlbarrsLoadBalancerName #-}
{-# DEPRECATED loadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlbarrsResponseStatus :: Lens.Lens' ModifyLoadBalancerAttributesResponse Core.Int
mlbarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mlbarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
