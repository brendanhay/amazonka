{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.ListTagsForResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all tags for a top-level EFS resource. You must provide the ID of the resource that you want to retrieve the tags for.
--
-- This operation requires permissions for the @elasticfilesystem:DescribeAccessPoints@ action.
module Network.AWS.EFS.ListTagsForResource
    (
    -- * Creating a request
      ListTagsForResource (..)
    , mkListTagsForResource
    -- ** Request lenses
    , ltfrResourceId
    , ltfrMaxResults
    , ltfrNextToken

    -- * Destructuring the response
    , ListTagsForResourceResponse (..)
    , mkListTagsForResourceResponse
    -- ** Response lenses
    , ltfrrrsNextToken
    , ltfrrrsTags
    , ltfrrrsResponseStatus
    ) where

import qualified Network.AWS.EFS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListTagsForResource' smart constructor.
data ListTagsForResource = ListTagsForResource'
  { resourceId :: Types.ResourceId
    -- ^ Specifies the EFS resource you want to retrieve tags for. You can retrieve tags for EFS file systems and access points using this API endpoint.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ (Optional) Specifies the maximum number of tag objects to return in the response. The default value is 100.
  , nextToken :: Core.Maybe Types.Token
    -- ^ You can use @NextToken@ in a subsequent request to fetch the next page of access point descriptions if the response payload was paginated.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTagsForResource' value with any optional fields omitted.
mkListTagsForResource
    :: Types.ResourceId -- ^ 'resourceId'
    -> ListTagsForResource
mkListTagsForResource resourceId
  = ListTagsForResource'{resourceId, maxResults = Core.Nothing,
                         nextToken = Core.Nothing}

-- | Specifies the EFS resource you want to retrieve tags for. You can retrieve tags for EFS file systems and access points using this API endpoint.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrResourceId :: Lens.Lens' ListTagsForResource Types.ResourceId
ltfrResourceId = Lens.field @"resourceId"
{-# INLINEABLE ltfrResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | (Optional) Specifies the maximum number of tag objects to return in the response. The default value is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrMaxResults :: Lens.Lens' ListTagsForResource (Core.Maybe Core.Natural)
ltfrMaxResults = Lens.field @"maxResults"
{-# INLINEABLE ltfrMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | You can use @NextToken@ in a subsequent request to fetch the next page of access point descriptions if the response payload was paginated.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrNextToken :: Lens.Lens' ListTagsForResource (Core.Maybe Types.Token)
ltfrNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltfrNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListTagsForResource where
        toQuery ListTagsForResource{..}
          = Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders ListTagsForResource where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListTagsForResource where
        type Rs ListTagsForResource = ListTagsForResourceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/2015-02-01/resource-tags/" Core.<> Core.toText resourceId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListTagsForResourceResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "Tags" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListTagsForResourceResponse' smart constructor.
data ListTagsForResourceResponse = ListTagsForResourceResponse'
  { nextToken :: Core.Maybe Types.Token
    -- ^ @NextToken@ is present if the response payload is paginated. You can use @NextToken@ in a subsequent request to fetch the next page of access point descriptions.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ An array of the tags for the specified EFS resource.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTagsForResourceResponse' value with any optional fields omitted.
mkListTagsForResourceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListTagsForResourceResponse
mkListTagsForResourceResponse responseStatus
  = ListTagsForResourceResponse'{nextToken = Core.Nothing,
                                 tags = Core.Nothing, responseStatus}

-- | @NextToken@ is present if the response payload is paginated. You can use @NextToken@ in a subsequent request to fetch the next page of access point descriptions.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrrrsNextToken :: Lens.Lens' ListTagsForResourceResponse (Core.Maybe Types.Token)
ltfrrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltfrrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | An array of the tags for the specified EFS resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrrrsTags :: Lens.Lens' ListTagsForResourceResponse (Core.Maybe [Types.Tag])
ltfrrrsTags = Lens.field @"tags"
{-# INLINEABLE ltfrrrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrrrsResponseStatus :: Lens.Lens' ListTagsForResourceResponse Core.Int
ltfrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ltfrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
