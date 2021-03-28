{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.ListResourcesForTagOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the resources associated with the specified TagOption.
--
-- This operation returns paginated results.
module Network.AWS.ServiceCatalog.ListResourcesForTagOption
    (
    -- * Creating a request
      ListResourcesForTagOption (..)
    , mkListResourcesForTagOption
    -- ** Request lenses
    , lrftoTagOptionId
    , lrftoPageSize
    , lrftoPageToken
    , lrftoResourceType

    -- * Destructuring the response
    , ListResourcesForTagOptionResponse (..)
    , mkListResourcesForTagOptionResponse
    -- ** Response lenses
    , lrftorrsPageToken
    , lrftorrsResourceDetails
    , lrftorrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkListResourcesForTagOption' smart constructor.
data ListResourcesForTagOption = ListResourcesForTagOption'
  { tagOptionId :: Types.TagOptionId
    -- ^ The TagOption identifier.
  , pageSize :: Core.Maybe Core.Natural
    -- ^ The maximum number of items to return with this call.
  , pageToken :: Core.Maybe Types.PageToken
    -- ^ The page token for the next set of results. To retrieve the first set of results, use null.
  , resourceType :: Core.Maybe Types.ResourceType
    -- ^ The resource type.
--
--
--     * @Portfolio@ 
--
--
--     * @Product@ 
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListResourcesForTagOption' value with any optional fields omitted.
mkListResourcesForTagOption
    :: Types.TagOptionId -- ^ 'tagOptionId'
    -> ListResourcesForTagOption
mkListResourcesForTagOption tagOptionId
  = ListResourcesForTagOption'{tagOptionId, pageSize = Core.Nothing,
                               pageToken = Core.Nothing, resourceType = Core.Nothing}

-- | The TagOption identifier.
--
-- /Note:/ Consider using 'tagOptionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrftoTagOptionId :: Lens.Lens' ListResourcesForTagOption Types.TagOptionId
lrftoTagOptionId = Lens.field @"tagOptionId"
{-# INLINEABLE lrftoTagOptionId #-}
{-# DEPRECATED tagOptionId "Use generic-lens or generic-optics with 'tagOptionId' instead"  #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrftoPageSize :: Lens.Lens' ListResourcesForTagOption (Core.Maybe Core.Natural)
lrftoPageSize = Lens.field @"pageSize"
{-# INLINEABLE lrftoPageSize #-}
{-# DEPRECATED pageSize "Use generic-lens or generic-optics with 'pageSize' instead"  #-}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrftoPageToken :: Lens.Lens' ListResourcesForTagOption (Core.Maybe Types.PageToken)
lrftoPageToken = Lens.field @"pageToken"
{-# INLINEABLE lrftoPageToken #-}
{-# DEPRECATED pageToken "Use generic-lens or generic-optics with 'pageToken' instead"  #-}

-- | The resource type.
--
--
--     * @Portfolio@ 
--
--
--     * @Product@ 
--
--
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrftoResourceType :: Lens.Lens' ListResourcesForTagOption (Core.Maybe Types.ResourceType)
lrftoResourceType = Lens.field @"resourceType"
{-# INLINEABLE lrftoResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

instance Core.ToQuery ListResourcesForTagOption where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListResourcesForTagOption where
        toHeaders ListResourcesForTagOption{..}
          = Core.pure
              ("X-Amz-Target",
               "AWS242ServiceCatalogService.ListResourcesForTagOption")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListResourcesForTagOption where
        toJSON ListResourcesForTagOption{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("TagOptionId" Core..= tagOptionId),
                  ("PageSize" Core..=) Core.<$> pageSize,
                  ("PageToken" Core..=) Core.<$> pageToken,
                  ("ResourceType" Core..=) Core.<$> resourceType])

instance Core.AWSRequest ListResourcesForTagOption where
        type Rs ListResourcesForTagOption =
             ListResourcesForTagOptionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListResourcesForTagOptionResponse' Core.<$>
                   (x Core..:? "PageToken") Core.<*> x Core..:? "ResourceDetails"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListResourcesForTagOption where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"pageToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"resourceDetails" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"pageToken" Lens..~ rs Lens.^. Lens.field @"pageToken")

-- | /See:/ 'mkListResourcesForTagOptionResponse' smart constructor.
data ListResourcesForTagOptionResponse = ListResourcesForTagOptionResponse'
  { pageToken :: Core.Maybe Types.PageToken
    -- ^ The page token for the next set of results. To retrieve the first set of results, use null.
  , resourceDetails :: Core.Maybe [Types.ResourceDetail]
    -- ^ Information about the resources.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListResourcesForTagOptionResponse' value with any optional fields omitted.
mkListResourcesForTagOptionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListResourcesForTagOptionResponse
mkListResourcesForTagOptionResponse responseStatus
  = ListResourcesForTagOptionResponse'{pageToken = Core.Nothing,
                                       resourceDetails = Core.Nothing, responseStatus}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrftorrsPageToken :: Lens.Lens' ListResourcesForTagOptionResponse (Core.Maybe Types.PageToken)
lrftorrsPageToken = Lens.field @"pageToken"
{-# INLINEABLE lrftorrsPageToken #-}
{-# DEPRECATED pageToken "Use generic-lens or generic-optics with 'pageToken' instead"  #-}

-- | Information about the resources.
--
-- /Note:/ Consider using 'resourceDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrftorrsResourceDetails :: Lens.Lens' ListResourcesForTagOptionResponse (Core.Maybe [Types.ResourceDetail])
lrftorrsResourceDetails = Lens.field @"resourceDetails"
{-# INLINEABLE lrftorrsResourceDetails #-}
{-# DEPRECATED resourceDetails "Use generic-lens or generic-optics with 'resourceDetails' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrftorrsResponseStatus :: Lens.Lens' ListResourcesForTagOptionResponse Core.Int
lrftorrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lrftorrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
