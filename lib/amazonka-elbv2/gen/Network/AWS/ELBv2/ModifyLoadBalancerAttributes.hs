{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.ModifyLoadBalancerAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified attributes of the specified Application Load Balancer, Network Load Balancer, or Gateway Load Balancer.
--
-- If any of the specified attributes can't be modified as requested, the call fails. Any existing attributes that you do not modify retain their current values.
module Network.AWS.ELBv2.ModifyLoadBalancerAttributes
    (
    -- * Creating a request
      ModifyLoadBalancerAttributes (..)
    , mkModifyLoadBalancerAttributes
    -- ** Request lenses
    , mlbaLoadBalancerArn
    , mlbaAttributes

    -- * Destructuring the response
    , ModifyLoadBalancerAttributesResponse (..)
    , mkModifyLoadBalancerAttributesResponse
    -- ** Response lenses
    , mlbarrsAttributes
    , mlbarrsResponseStatus
    ) where

import qualified Network.AWS.ELBv2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyLoadBalancerAttributes' smart constructor.
data ModifyLoadBalancerAttributes = ModifyLoadBalancerAttributes'
  { loadBalancerArn :: Types.LoadBalancerArn
    -- ^ The Amazon Resource Name (ARN) of the load balancer.
  , attributes :: [Types.LoadBalancerAttribute]
    -- ^ The load balancer attributes.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyLoadBalancerAttributes' value with any optional fields omitted.
mkModifyLoadBalancerAttributes
    :: Types.LoadBalancerArn -- ^ 'loadBalancerArn'
    -> ModifyLoadBalancerAttributes
mkModifyLoadBalancerAttributes loadBalancerArn
  = ModifyLoadBalancerAttributes'{loadBalancerArn,
                                  attributes = Core.mempty}

-- | The Amazon Resource Name (ARN) of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlbaLoadBalancerArn :: Lens.Lens' ModifyLoadBalancerAttributes Types.LoadBalancerArn
mlbaLoadBalancerArn = Lens.field @"loadBalancerArn"
{-# INLINEABLE mlbaLoadBalancerArn #-}
{-# DEPRECATED loadBalancerArn "Use generic-lens or generic-optics with 'loadBalancerArn' instead"  #-}

-- | The load balancer attributes.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlbaAttributes :: Lens.Lens' ModifyLoadBalancerAttributes [Types.LoadBalancerAttribute]
mlbaAttributes = Lens.field @"attributes"
{-# INLINEABLE mlbaAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

instance Core.ToQuery ModifyLoadBalancerAttributes where
        toQuery ModifyLoadBalancerAttributes{..}
          = Core.toQueryPair "Action"
              ("ModifyLoadBalancerAttributes" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2015-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "LoadBalancerArn" loadBalancerArn
              Core.<>
              Core.toQueryPair "Attributes"
                (Core.toQueryList "member" attributes)

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
                   (x Core..@? "Attributes" Core..<@> Core.parseXMLList "member")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyLoadBalancerAttributesResponse' smart constructor.
data ModifyLoadBalancerAttributesResponse = ModifyLoadBalancerAttributesResponse'
  { attributes :: Core.Maybe [Types.LoadBalancerAttribute]
    -- ^ Information about the load balancer attributes.
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
  = ModifyLoadBalancerAttributesResponse'{attributes = Core.Nothing,
                                          responseStatus}

-- | Information about the load balancer attributes.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlbarrsAttributes :: Lens.Lens' ModifyLoadBalancerAttributesResponse (Core.Maybe [Types.LoadBalancerAttribute])
mlbarrsAttributes = Lens.field @"attributes"
{-# INLINEABLE mlbarrsAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlbarrsResponseStatus :: Lens.Lens' ModifyLoadBalancerAttributesResponse Core.Int
mlbarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mlbarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
