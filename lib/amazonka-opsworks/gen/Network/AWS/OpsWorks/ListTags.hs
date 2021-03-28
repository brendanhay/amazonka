{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.ListTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of tags that are applied to the specified stack or layer.
module Network.AWS.OpsWorks.ListTags
    (
    -- * Creating a request
      ListTags (..)
    , mkListTags
    -- ** Request lenses
    , ltResourceArn
    , ltMaxResults
    , ltNextToken

    -- * Destructuring the response
    , ListTagsResponse (..)
    , mkListTagsResponse
    -- ** Response lenses
    , ltrrsNextToken
    , ltrrsTags
    , ltrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListTags' smart constructor.
data ListTags = ListTags'
  { resourceArn :: Types.ResourceArn
    -- ^ The stack or layer's Amazon Resource Number (ARN).
  , maxResults :: Core.Maybe Core.Int
    -- ^ Do not use. A validation exception occurs if you add a @MaxResults@ parameter to a @ListTagsRequest@ call. 
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ Do not use. A validation exception occurs if you add a @NextToken@ parameter to a @ListTagsRequest@ call. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTags' value with any optional fields omitted.
mkListTags
    :: Types.ResourceArn -- ^ 'resourceArn'
    -> ListTags
mkListTags resourceArn
  = ListTags'{resourceArn, maxResults = Core.Nothing,
              nextToken = Core.Nothing}

-- | The stack or layer's Amazon Resource Number (ARN).
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltResourceArn :: Lens.Lens' ListTags Types.ResourceArn
ltResourceArn = Lens.field @"resourceArn"
{-# INLINEABLE ltResourceArn #-}
{-# DEPRECATED resourceArn "Use generic-lens or generic-optics with 'resourceArn' instead"  #-}

-- | Do not use. A validation exception occurs if you add a @MaxResults@ parameter to a @ListTagsRequest@ call. 
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltMaxResults :: Lens.Lens' ListTags (Core.Maybe Core.Int)
ltMaxResults = Lens.field @"maxResults"
{-# INLINEABLE ltMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | Do not use. A validation exception occurs if you add a @NextToken@ parameter to a @ListTagsRequest@ call. 
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
          = Core.pure ("X-Amz-Target", "OpsWorks_20130218.ListTags") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListTags where
        toJSON ListTags{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ResourceArn" Core..= resourceArn),
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
                   (x Core..:? "NextToken") Core.<*> x Core..:? "Tags" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the response to a @ListTags@ request.
--
-- /See:/ 'mkListTagsResponse' smart constructor.
data ListTagsResponse = ListTagsResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ If a paginated request does not return all of the remaining results, this parameter is set to a token that you can assign to the request object's @NextToken@ parameter to get the next set of results. If the previous paginated request returned all of the remaining results, this parameter is set to @null@ . 
  , tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue)
    -- ^ A set of key-value pairs that contain tag keys and tag values that are attached to a stack or layer.
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
  = ListTagsResponse'{nextToken = Core.Nothing, tags = Core.Nothing,
                      responseStatus}

-- | If a paginated request does not return all of the remaining results, this parameter is set to a token that you can assign to the request object's @NextToken@ parameter to get the next set of results. If the previous paginated request returned all of the remaining results, this parameter is set to @null@ . 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsNextToken :: Lens.Lens' ListTagsResponse (Core.Maybe Types.NextToken)
ltrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A set of key-value pairs that contain tag keys and tag values that are attached to a stack or layer.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsTags :: Lens.Lens' ListTagsResponse (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
ltrrsTags = Lens.field @"tags"
{-# INLINEABLE ltrrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsResponseStatus :: Lens.Lens' ListTagsResponse Core.Int
ltrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ltrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
