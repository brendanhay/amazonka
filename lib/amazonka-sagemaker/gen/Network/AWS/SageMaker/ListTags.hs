{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the tags for the specified Amazon SageMaker resource.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListTags
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
    , ltrfrsNextToken
    , ltrfrsTags
    , ltrfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkListTags' smart constructor.
data ListTags = ListTags'
  { resourceArn :: Types.ResourceArn
    -- ^ The Amazon Resource Name (ARN) of the resource whose tags you want to retrieve.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ Maximum number of tags to return.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If the response to the previous @ListTags@ request is truncated, Amazon SageMaker returns this token. To retrieve the next set of tags, use it in the subsequent request. 
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

-- | The Amazon Resource Name (ARN) of the resource whose tags you want to retrieve.
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltResourceArn :: Lens.Lens' ListTags Types.ResourceArn
ltResourceArn = Lens.field @"resourceArn"
{-# INLINEABLE ltResourceArn #-}
{-# DEPRECATED resourceArn "Use generic-lens or generic-optics with 'resourceArn' instead"  #-}

-- | Maximum number of tags to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltMaxResults :: Lens.Lens' ListTags (Core.Maybe Core.Natural)
ltMaxResults = Lens.field @"maxResults"
{-# INLINEABLE ltMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | If the response to the previous @ListTags@ request is truncated, Amazon SageMaker returns this token. To retrieve the next set of tags, use it in the subsequent request. 
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
          = Core.pure ("X-Amz-Target", "SageMaker.ListTags") Core.<>
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

instance Pager.AWSPager ListTags where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"tags" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListTagsResponse' smart constructor.
data ListTagsResponse = ListTagsResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ If response is truncated, Amazon SageMaker includes a token in the response. You can use this token in your subsequent request to fetch next set of tokens. 
  , tags :: Core.Maybe [Types.Tag]
    -- ^ An array of @Tag@ objects, each with a tag key and a value.
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

-- | If response is truncated, Amazon SageMaker includes a token in the response. You can use this token in your subsequent request to fetch next set of tokens. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrfrsNextToken :: Lens.Lens' ListTagsResponse (Core.Maybe Types.NextToken)
ltrfrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltrfrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | An array of @Tag@ objects, each with a tag key and a value.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrfrsTags :: Lens.Lens' ListTagsResponse (Core.Maybe [Types.Tag])
ltrfrsTags = Lens.field @"tags"
{-# INLINEABLE ltrfrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrfrsResponseStatus :: Lens.Lens' ListTagsResponse Core.Int
ltrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ltrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
