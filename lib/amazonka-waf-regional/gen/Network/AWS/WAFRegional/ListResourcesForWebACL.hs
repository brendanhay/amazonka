{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.ListResourcesForWebACL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of resources associated with the specified web ACL.
module Network.AWS.WAFRegional.ListResourcesForWebACL
    (
    -- * Creating a request
      ListResourcesForWebACL (..)
    , mkListResourcesForWebACL
    -- ** Request lenses
    , lrfwaclWebACLId
    , lrfwaclResourceType

    -- * Destructuring the response
    , ListResourcesForWebACLResponse (..)
    , mkListResourcesForWebACLResponse
    -- ** Response lenses
    , lrfwaclrrsResourceArns
    , lrfwaclrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAFRegional.Types as Types

-- | /See:/ 'mkListResourcesForWebACL' smart constructor.
data ListResourcesForWebACL = ListResourcesForWebACL'
  { webACLId :: Types.WebACLId
    -- ^ The unique identifier (ID) of the web ACL for which to list the associated resources.
  , resourceType :: Core.Maybe Types.ResourceType
    -- ^ The type of resource to list, either an application load balancer or Amazon API Gateway.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListResourcesForWebACL' value with any optional fields omitted.
mkListResourcesForWebACL
    :: Types.WebACLId -- ^ 'webACLId'
    -> ListResourcesForWebACL
mkListResourcesForWebACL webACLId
  = ListResourcesForWebACL'{webACLId, resourceType = Core.Nothing}

-- | The unique identifier (ID) of the web ACL for which to list the associated resources.
--
-- /Note:/ Consider using 'webACLId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrfwaclWebACLId :: Lens.Lens' ListResourcesForWebACL Types.WebACLId
lrfwaclWebACLId = Lens.field @"webACLId"
{-# INLINEABLE lrfwaclWebACLId #-}
{-# DEPRECATED webACLId "Use generic-lens or generic-optics with 'webACLId' instead"  #-}

-- | The type of resource to list, either an application load balancer or Amazon API Gateway.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrfwaclResourceType :: Lens.Lens' ListResourcesForWebACL (Core.Maybe Types.ResourceType)
lrfwaclResourceType = Lens.field @"resourceType"
{-# INLINEABLE lrfwaclResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

instance Core.ToQuery ListResourcesForWebACL where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListResourcesForWebACL where
        toHeaders ListResourcesForWebACL{..}
          = Core.pure
              ("X-Amz-Target", "AWSWAF_Regional_20161128.ListResourcesForWebACL")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListResourcesForWebACL where
        toJSON ListResourcesForWebACL{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("WebACLId" Core..= webACLId),
                  ("ResourceType" Core..=) Core.<$> resourceType])

instance Core.AWSRequest ListResourcesForWebACL where
        type Rs ListResourcesForWebACL = ListResourcesForWebACLResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListResourcesForWebACLResponse' Core.<$>
                   (x Core..:? "ResourceArns") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListResourcesForWebACLResponse' smart constructor.
data ListResourcesForWebACLResponse = ListResourcesForWebACLResponse'
  { resourceArns :: Core.Maybe [Types.ResourceArn]
    -- ^ An array of ARNs (Amazon Resource Names) of the resources associated with the specified web ACL. An array with zero elements is returned if there are no resources associated with the web ACL.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListResourcesForWebACLResponse' value with any optional fields omitted.
mkListResourcesForWebACLResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListResourcesForWebACLResponse
mkListResourcesForWebACLResponse responseStatus
  = ListResourcesForWebACLResponse'{resourceArns = Core.Nothing,
                                    responseStatus}

-- | An array of ARNs (Amazon Resource Names) of the resources associated with the specified web ACL. An array with zero elements is returned if there are no resources associated with the web ACL.
--
-- /Note:/ Consider using 'resourceArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrfwaclrrsResourceArns :: Lens.Lens' ListResourcesForWebACLResponse (Core.Maybe [Types.ResourceArn])
lrfwaclrrsResourceArns = Lens.field @"resourceArns"
{-# INLINEABLE lrfwaclrrsResourceArns #-}
{-# DEPRECATED resourceArns "Use generic-lens or generic-optics with 'resourceArns' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrfwaclrrsResponseStatus :: Lens.Lens' ListResourcesForWebACLResponse Core.Int
lrfwaclrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lrfwaclrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
