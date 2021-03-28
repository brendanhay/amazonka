{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.ListTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of tags for the specified AWS CloudHSM cluster.
--
-- This is a paginated operation, which means that each response might contain only a subset of all the tags. When the response contains only a subset of tags, it includes a @NextToken@ value. Use this value in a subsequent @ListTags@ request to get more tags. When you receive a response with no @NextToken@ (or an empty or null value), that means there are no more tags to get.
--
-- This operation returns paginated results.
module Network.AWS.CloudHSMv2.ListTags
    (
    -- * Creating a request
      ListTags (..)
    , mkListTags
    -- ** Request lenses
    , ltResourceId
    , ltMaxResults
    , ltNextToken

    -- * Destructuring the response
    , ListTagsResponse (..)
    , mkListTagsResponse
    -- ** Response lenses
    , ltrrsTagList
    , ltrrsNextToken
    , ltrrsResponseStatus
    ) where

import qualified Network.AWS.CloudHSMv2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListTags' smart constructor.
data ListTags = ListTags'
  { resourceId :: Types.ResourceId
    -- ^ The cluster identifier (ID) for the cluster whose tags you are getting. To find the cluster ID, use 'DescribeClusters' .
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of tags to return in the response. When there are more tags than the number you specify, the response contains a @NextToken@ value.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The @NextToken@ value that you received in the previous response. Use this value to get more tags.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTags' value with any optional fields omitted.
mkListTags
    :: Types.ResourceId -- ^ 'resourceId'
    -> ListTags
mkListTags resourceId
  = ListTags'{resourceId, maxResults = Core.Nothing,
              nextToken = Core.Nothing}

-- | The cluster identifier (ID) for the cluster whose tags you are getting. To find the cluster ID, use 'DescribeClusters' .
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltResourceId :: Lens.Lens' ListTags Types.ResourceId
ltResourceId = Lens.field @"resourceId"
{-# INLINEABLE ltResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | The maximum number of tags to return in the response. When there are more tags than the number you specify, the response contains a @NextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltMaxResults :: Lens.Lens' ListTags (Core.Maybe Core.Natural)
ltMaxResults = Lens.field @"maxResults"
{-# INLINEABLE ltMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The @NextToken@ value that you received in the previous response. Use this value to get more tags.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltNextToken :: Lens.Lens' ListTags (Core.Maybe Types.NextToken)
ltNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListTags where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListTags where
        toHeaders ListTags{..}
          = Core.pure ("X-Amz-Target", "BaldrApiService.ListTags") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListTags where
        toJSON ListTags{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ResourceId" Core..= resourceId),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListTags where
        type Rs ListTags = ListTagsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListTagsResponse' Core.<$>
                   (x Core..:? "TagList" Core..!= Core.mempty) Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListTags where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^. Lens.field @"tagList") = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListTagsResponse' smart constructor.
data ListTagsResponse = ListTagsResponse'
  { tagList :: [Types.Tag]
    -- ^ A list of tags.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ An opaque string that indicates that the response contains only a subset of tags. Use this value in a subsequent @ListTags@ request to get more tags.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTagsResponse' value with any optional fields omitted.
mkListTagsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListTagsResponse
mkListTagsResponse responseStatus
  = ListTagsResponse'{tagList = Core.mempty,
                      nextToken = Core.Nothing, responseStatus}

-- | A list of tags.
--
-- /Note:/ Consider using 'tagList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsTagList :: Lens.Lens' ListTagsResponse [Types.Tag]
ltrrsTagList = Lens.field @"tagList"
{-# INLINEABLE ltrrsTagList #-}
{-# DEPRECATED tagList "Use generic-lens or generic-optics with 'tagList' instead"  #-}

-- | An opaque string that indicates that the response contains only a subset of tags. Use this value in a subsequent @ListTags@ request to get more tags.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsNextToken :: Lens.Lens' ListTagsResponse (Core.Maybe Types.NextToken)
ltrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsResponseStatus :: Lens.Lens' ListTagsResponse Core.Int
ltrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ltrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
