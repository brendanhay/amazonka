{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.UpdateLoadBalancerAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified attribute for a load balancer. You can only update one attribute at a time.
--
-- The @update load balancer attribute@ operation supports tag-based access control via resource tags applied to the resource identified by @load balancer name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.UpdateLoadBalancerAttribute
    (
    -- * Creating a request
      UpdateLoadBalancerAttribute (..)
    , mkUpdateLoadBalancerAttribute
    -- ** Request lenses
    , ulbaLoadBalancerName
    , ulbaAttributeName
    , ulbaAttributeValue

    -- * Destructuring the response
    , UpdateLoadBalancerAttributeResponse (..)
    , mkUpdateLoadBalancerAttributeResponse
    -- ** Response lenses
    , ulbarrsOperations
    , ulbarrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateLoadBalancerAttribute' smart constructor.
data UpdateLoadBalancerAttribute = UpdateLoadBalancerAttribute'
  { loadBalancerName :: Types.ResourceName
    -- ^ The name of the load balancer that you want to modify (e.g., @my-load-balancer@ .
  , attributeName :: Types.LoadBalancerAttributeName
    -- ^ The name of the attribute you want to update. Valid values are below.
  , attributeValue :: Types.AttributeValue
    -- ^ The value that you want to specify for the attribute name.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateLoadBalancerAttribute' value with any optional fields omitted.
mkUpdateLoadBalancerAttribute
    :: Types.ResourceName -- ^ 'loadBalancerName'
    -> Types.LoadBalancerAttributeName -- ^ 'attributeName'
    -> Types.AttributeValue -- ^ 'attributeValue'
    -> UpdateLoadBalancerAttribute
mkUpdateLoadBalancerAttribute loadBalancerName attributeName
  attributeValue
  = UpdateLoadBalancerAttribute'{loadBalancerName, attributeName,
                                 attributeValue}

-- | The name of the load balancer that you want to modify (e.g., @my-load-balancer@ .
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulbaLoadBalancerName :: Lens.Lens' UpdateLoadBalancerAttribute Types.ResourceName
ulbaLoadBalancerName = Lens.field @"loadBalancerName"
{-# INLINEABLE ulbaLoadBalancerName #-}
{-# DEPRECATED loadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead"  #-}

-- | The name of the attribute you want to update. Valid values are below.
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulbaAttributeName :: Lens.Lens' UpdateLoadBalancerAttribute Types.LoadBalancerAttributeName
ulbaAttributeName = Lens.field @"attributeName"
{-# INLINEABLE ulbaAttributeName #-}
{-# DEPRECATED attributeName "Use generic-lens or generic-optics with 'attributeName' instead"  #-}

-- | The value that you want to specify for the attribute name.
--
-- /Note:/ Consider using 'attributeValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulbaAttributeValue :: Lens.Lens' UpdateLoadBalancerAttribute Types.AttributeValue
ulbaAttributeValue = Lens.field @"attributeValue"
{-# INLINEABLE ulbaAttributeValue #-}
{-# DEPRECATED attributeValue "Use generic-lens or generic-optics with 'attributeValue' instead"  #-}

instance Core.ToQuery UpdateLoadBalancerAttribute where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateLoadBalancerAttribute where
        toHeaders UpdateLoadBalancerAttribute{..}
          = Core.pure
              ("X-Amz-Target", "Lightsail_20161128.UpdateLoadBalancerAttribute")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateLoadBalancerAttribute where
        toJSON UpdateLoadBalancerAttribute{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("loadBalancerName" Core..= loadBalancerName),
                  Core.Just ("attributeName" Core..= attributeName),
                  Core.Just ("attributeValue" Core..= attributeValue)])

instance Core.AWSRequest UpdateLoadBalancerAttribute where
        type Rs UpdateLoadBalancerAttribute =
             UpdateLoadBalancerAttributeResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateLoadBalancerAttributeResponse' Core.<$>
                   (x Core..:? "operations") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateLoadBalancerAttributeResponse' smart constructor.
data UpdateLoadBalancerAttributeResponse = UpdateLoadBalancerAttributeResponse'
  { operations :: Core.Maybe [Types.Operation]
    -- ^ An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateLoadBalancerAttributeResponse' value with any optional fields omitted.
mkUpdateLoadBalancerAttributeResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateLoadBalancerAttributeResponse
mkUpdateLoadBalancerAttributeResponse responseStatus
  = UpdateLoadBalancerAttributeResponse'{operations = Core.Nothing,
                                         responseStatus}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulbarrsOperations :: Lens.Lens' UpdateLoadBalancerAttributeResponse (Core.Maybe [Types.Operation])
ulbarrsOperations = Lens.field @"operations"
{-# INLINEABLE ulbarrsOperations #-}
{-# DEPRECATED operations "Use generic-lens or generic-optics with 'operations' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulbarrsResponseStatus :: Lens.Lens' UpdateLoadBalancerAttributeResponse Core.Int
ulbarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ulbarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
