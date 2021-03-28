{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.ListTagsForStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of tags associated with the specified stream.
--
-- In the request, you must specify either the @StreamName@ or the @StreamARN@ . 
module Network.AWS.KinesisVideo.ListTagsForStream
    (
    -- * Creating a request
      ListTagsForStream (..)
    , mkListTagsForStream
    -- ** Request lenses
    , ltfsNextToken
    , ltfsStreamARN
    , ltfsStreamName

    -- * Destructuring the response
    , ListTagsForStreamResponse (..)
    , mkListTagsForStreamResponse
    -- ** Response lenses
    , ltfsrrsNextToken
    , ltfsrrsTags
    , ltfsrrsResponseStatus
    ) where

import qualified Network.AWS.KinesisVideo.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListTagsForStream' smart constructor.
data ListTagsForStream = ListTagsForStream'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ If you specify this parameter and the result of a @ListTagsForStream@ call is truncated, the response includes a token that you can use in the next request to fetch the next batch of tags.
  , streamARN :: Core.Maybe Types.StreamARN
    -- ^ The Amazon Resource Name (ARN) of the stream that you want to list tags for.
  , streamName :: Core.Maybe Types.StreamName
    -- ^ The name of the stream that you want to list tags for.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTagsForStream' value with any optional fields omitted.
mkListTagsForStream
    :: ListTagsForStream
mkListTagsForStream
  = ListTagsForStream'{nextToken = Core.Nothing,
                       streamARN = Core.Nothing, streamName = Core.Nothing}

-- | If you specify this parameter and the result of a @ListTagsForStream@ call is truncated, the response includes a token that you can use in the next request to fetch the next batch of tags.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfsNextToken :: Lens.Lens' ListTagsForStream (Core.Maybe Types.NextToken)
ltfsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltfsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The Amazon Resource Name (ARN) of the stream that you want to list tags for.
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfsStreamARN :: Lens.Lens' ListTagsForStream (Core.Maybe Types.StreamARN)
ltfsStreamARN = Lens.field @"streamARN"
{-# INLINEABLE ltfsStreamARN #-}
{-# DEPRECATED streamARN "Use generic-lens or generic-optics with 'streamARN' instead"  #-}

-- | The name of the stream that you want to list tags for.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfsStreamName :: Lens.Lens' ListTagsForStream (Core.Maybe Types.StreamName)
ltfsStreamName = Lens.field @"streamName"
{-# INLINEABLE ltfsStreamName #-}
{-# DEPRECATED streamName "Use generic-lens or generic-optics with 'streamName' instead"  #-}

instance Core.ToQuery ListTagsForStream where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListTagsForStream where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON ListTagsForStream where
        toJSON ListTagsForStream{..}
          = Core.object
              (Core.catMaybes
                 [("NextToken" Core..=) Core.<$> nextToken,
                  ("StreamARN" Core..=) Core.<$> streamARN,
                  ("StreamName" Core..=) Core.<$> streamName])

instance Core.AWSRequest ListTagsForStream where
        type Rs ListTagsForStream = ListTagsForStreamResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/listTagsForStream",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListTagsForStreamResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "Tags" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListTagsForStreamResponse' smart constructor.
data ListTagsForStreamResponse = ListTagsForStreamResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ If you specify this parameter and the result of a @ListTags@ call is truncated, the response includes a token that you can use in the next request to fetch the next set of tags.
  , tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue)
    -- ^ A map of tag keys and values associated with the specified stream.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTagsForStreamResponse' value with any optional fields omitted.
mkListTagsForStreamResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListTagsForStreamResponse
mkListTagsForStreamResponse responseStatus
  = ListTagsForStreamResponse'{nextToken = Core.Nothing,
                               tags = Core.Nothing, responseStatus}

-- | If you specify this parameter and the result of a @ListTags@ call is truncated, the response includes a token that you can use in the next request to fetch the next set of tags.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfsrrsNextToken :: Lens.Lens' ListTagsForStreamResponse (Core.Maybe Types.NextToken)
ltfsrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltfsrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A map of tag keys and values associated with the specified stream.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfsrrsTags :: Lens.Lens' ListTagsForStreamResponse (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
ltfsrrsTags = Lens.field @"tags"
{-# INLINEABLE ltfsrrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfsrrsResponseStatus :: Lens.Lens' ListTagsForStreamResponse Core.Int
ltfsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ltfsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
