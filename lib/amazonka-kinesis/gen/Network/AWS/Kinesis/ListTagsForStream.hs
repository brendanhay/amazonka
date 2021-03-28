{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.ListTagsForStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tags for the specified Kinesis data stream. This operation has a limit of five transactions per second per account.
module Network.AWS.Kinesis.ListTagsForStream
    (
    -- * Creating a request
      ListTagsForStream (..)
    , mkListTagsForStream
    -- ** Request lenses
    , ltfsStreamName
    , ltfsExclusiveStartTagKey
    , ltfsLimit

    -- * Destructuring the response
    , ListTagsForStreamResponse (..)
    , mkListTagsForStreamResponse
    -- ** Response lenses
    , ltfsrrsTags
    , ltfsrrsHasMoreTags
    , ltfsrrsResponseStatus
    ) where

import qualified Network.AWS.Kinesis.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for @ListTagsForStream@ .
--
-- /See:/ 'mkListTagsForStream' smart constructor.
data ListTagsForStream = ListTagsForStream'
  { streamName :: Types.StreamName
    -- ^ The name of the stream.
  , exclusiveStartTagKey :: Core.Maybe Types.ExclusiveStartTagKey
    -- ^ The key to use as the starting point for the list of tags. If this parameter is set, @ListTagsForStream@ gets all tags that occur after @ExclusiveStartTagKey@ . 
  , limit :: Core.Maybe Core.Natural
    -- ^ The number of tags to return. If this number is less than the total number of tags associated with the stream, @HasMoreTags@ is set to @true@ . To list additional tags, set @ExclusiveStartTagKey@ to the last key in the response.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTagsForStream' value with any optional fields omitted.
mkListTagsForStream
    :: Types.StreamName -- ^ 'streamName'
    -> ListTagsForStream
mkListTagsForStream streamName
  = ListTagsForStream'{streamName,
                       exclusiveStartTagKey = Core.Nothing, limit = Core.Nothing}

-- | The name of the stream.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfsStreamName :: Lens.Lens' ListTagsForStream Types.StreamName
ltfsStreamName = Lens.field @"streamName"
{-# INLINEABLE ltfsStreamName #-}
{-# DEPRECATED streamName "Use generic-lens or generic-optics with 'streamName' instead"  #-}

-- | The key to use as the starting point for the list of tags. If this parameter is set, @ListTagsForStream@ gets all tags that occur after @ExclusiveStartTagKey@ . 
--
-- /Note:/ Consider using 'exclusiveStartTagKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfsExclusiveStartTagKey :: Lens.Lens' ListTagsForStream (Core.Maybe Types.ExclusiveStartTagKey)
ltfsExclusiveStartTagKey = Lens.field @"exclusiveStartTagKey"
{-# INLINEABLE ltfsExclusiveStartTagKey #-}
{-# DEPRECATED exclusiveStartTagKey "Use generic-lens or generic-optics with 'exclusiveStartTagKey' instead"  #-}

-- | The number of tags to return. If this number is less than the total number of tags associated with the stream, @HasMoreTags@ is set to @true@ . To list additional tags, set @ExclusiveStartTagKey@ to the last key in the response.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfsLimit :: Lens.Lens' ListTagsForStream (Core.Maybe Core.Natural)
ltfsLimit = Lens.field @"limit"
{-# INLINEABLE ltfsLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

instance Core.ToQuery ListTagsForStream where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListTagsForStream where
        toHeaders ListTagsForStream{..}
          = Core.pure ("X-Amz-Target", "Kinesis_20131202.ListTagsForStream")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListTagsForStream where
        toJSON ListTagsForStream{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("StreamName" Core..= streamName),
                  ("ExclusiveStartTagKey" Core..=) Core.<$> exclusiveStartTagKey,
                  ("Limit" Core..=) Core.<$> limit])

instance Core.AWSRequest ListTagsForStream where
        type Rs ListTagsForStream = ListTagsForStreamResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListTagsForStreamResponse' Core.<$>
                   (x Core..:? "Tags" Core..!= Core.mempty) Core.<*>
                     x Core..: "HasMoreTags"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the output for @ListTagsForStream@ .
--
-- /See:/ 'mkListTagsForStreamResponse' smart constructor.
data ListTagsForStreamResponse = ListTagsForStreamResponse'
  { tags :: [Types.Tag]
    -- ^ A list of tags associated with @StreamName@ , starting with the first tag after @ExclusiveStartTagKey@ and up to the specified @Limit@ . 
  , hasMoreTags :: Core.Bool
    -- ^ If set to @true@ , more tags are available. To request additional tags, set @ExclusiveStartTagKey@ to the key of the last tag returned.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTagsForStreamResponse' value with any optional fields omitted.
mkListTagsForStreamResponse
    :: Core.Bool -- ^ 'hasMoreTags'
    -> Core.Int -- ^ 'responseStatus'
    -> ListTagsForStreamResponse
mkListTagsForStreamResponse hasMoreTags responseStatus
  = ListTagsForStreamResponse'{tags = Core.mempty, hasMoreTags,
                               responseStatus}

-- | A list of tags associated with @StreamName@ , starting with the first tag after @ExclusiveStartTagKey@ and up to the specified @Limit@ . 
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfsrrsTags :: Lens.Lens' ListTagsForStreamResponse [Types.Tag]
ltfsrrsTags = Lens.field @"tags"
{-# INLINEABLE ltfsrrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | If set to @true@ , more tags are available. To request additional tags, set @ExclusiveStartTagKey@ to the key of the last tag returned.
--
-- /Note:/ Consider using 'hasMoreTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfsrrsHasMoreTags :: Lens.Lens' ListTagsForStreamResponse Core.Bool
ltfsrrsHasMoreTags = Lens.field @"hasMoreTags"
{-# INLINEABLE ltfsrrsHasMoreTags #-}
{-# DEPRECATED hasMoreTags "Use generic-lens or generic-optics with 'hasMoreTags' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfsrrsResponseStatus :: Lens.Lens' ListTagsForStreamResponse Core.Int
ltfsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ltfsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
