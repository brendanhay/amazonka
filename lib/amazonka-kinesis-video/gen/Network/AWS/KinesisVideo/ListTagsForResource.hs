{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.ListTagsForResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of tags associated with the specified signaling channel.
module Network.AWS.KinesisVideo.ListTagsForResource
    (
    -- * Creating a request
      ListTagsForResource (..)
    , mkListTagsForResource
    -- ** Request lenses
    , ltfrResourceARN
    , ltfrNextToken

    -- * Destructuring the response
    , ListTagsForResourceResponse (..)
    , mkListTagsForResourceResponse
    -- ** Response lenses
    , ltfrrrsNextToken
    , ltfrrrsTags
    , ltfrrrsResponseStatus
    ) where

import qualified Network.AWS.KinesisVideo.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListTagsForResource' smart constructor.
data ListTagsForResource = ListTagsForResource'
  { resourceARN :: Types.ResourceARN
    -- ^ The Amazon Resource Name (ARN) of the signaling channel for which you want to list tags.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If you specify this parameter and the result of a @ListTagsForResource@ call is truncated, the response includes a token that you can use in the next request to fetch the next batch of tags. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTagsForResource' value with any optional fields omitted.
mkListTagsForResource
    :: Types.ResourceARN -- ^ 'resourceARN'
    -> ListTagsForResource
mkListTagsForResource resourceARN
  = ListTagsForResource'{resourceARN, nextToken = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the signaling channel for which you want to list tags.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrResourceARN :: Lens.Lens' ListTagsForResource Types.ResourceARN
ltfrResourceARN = Lens.field @"resourceARN"
{-# INLINEABLE ltfrResourceARN #-}
{-# DEPRECATED resourceARN "Use generic-lens or generic-optics with 'resourceARN' instead"  #-}

-- | If you specify this parameter and the result of a @ListTagsForResource@ call is truncated, the response includes a token that you can use in the next request to fetch the next batch of tags. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrNextToken :: Lens.Lens' ListTagsForResource (Core.Maybe Types.NextToken)
ltfrNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltfrNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListTagsForResource where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListTagsForResource where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON ListTagsForResource where
        toJSON ListTagsForResource{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ResourceARN" Core..= resourceARN),
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListTagsForResource where
        type Rs ListTagsForResource = ListTagsForResourceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/ListTagsForResource",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
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
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ If you specify this parameter and the result of a @ListTagsForResource@ call is truncated, the response includes a token that you can use in the next request to fetch the next set of tags. 
  , tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue)
    -- ^ A map of tag keys and values associated with the specified signaling channel.
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

-- | If you specify this parameter and the result of a @ListTagsForResource@ call is truncated, the response includes a token that you can use in the next request to fetch the next set of tags. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrrrsNextToken :: Lens.Lens' ListTagsForResourceResponse (Core.Maybe Types.NextToken)
ltfrrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltfrrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A map of tag keys and values associated with the specified signaling channel.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrrrsTags :: Lens.Lens' ListTagsForResourceResponse (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
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
