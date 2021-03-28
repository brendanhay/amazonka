{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.ListTagsForDeliveryStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tags for the specified delivery stream. This operation has a limit of five transactions per second per account. 
module Network.AWS.Firehose.ListTagsForDeliveryStream
    (
    -- * Creating a request
      ListTagsForDeliveryStream (..)
    , mkListTagsForDeliveryStream
    -- ** Request lenses
    , ltfdsDeliveryStreamName
    , ltfdsExclusiveStartTagKey
    , ltfdsLimit

    -- * Destructuring the response
    , ListTagsForDeliveryStreamResponse (..)
    , mkListTagsForDeliveryStreamResponse
    -- ** Response lenses
    , ltfdsrrsTags
    , ltfdsrrsHasMoreTags
    , ltfdsrrsResponseStatus
    ) where

import qualified Network.AWS.Firehose.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListTagsForDeliveryStream' smart constructor.
data ListTagsForDeliveryStream = ListTagsForDeliveryStream'
  { deliveryStreamName :: Types.DeliveryStreamName
    -- ^ The name of the delivery stream whose tags you want to list.
  , exclusiveStartTagKey :: Core.Maybe Types.TagKey
    -- ^ The key to use as the starting point for the list of tags. If you set this parameter, @ListTagsForDeliveryStream@ gets all tags that occur after @ExclusiveStartTagKey@ .
  , limit :: Core.Maybe Core.Natural
    -- ^ The number of tags to return. If this number is less than the total number of tags associated with the delivery stream, @HasMoreTags@ is set to @true@ in the response. To list additional tags, set @ExclusiveStartTagKey@ to the last key in the response. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTagsForDeliveryStream' value with any optional fields omitted.
mkListTagsForDeliveryStream
    :: Types.DeliveryStreamName -- ^ 'deliveryStreamName'
    -> ListTagsForDeliveryStream
mkListTagsForDeliveryStream deliveryStreamName
  = ListTagsForDeliveryStream'{deliveryStreamName,
                               exclusiveStartTagKey = Core.Nothing, limit = Core.Nothing}

-- | The name of the delivery stream whose tags you want to list.
--
-- /Note:/ Consider using 'deliveryStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfdsDeliveryStreamName :: Lens.Lens' ListTagsForDeliveryStream Types.DeliveryStreamName
ltfdsDeliveryStreamName = Lens.field @"deliveryStreamName"
{-# INLINEABLE ltfdsDeliveryStreamName #-}
{-# DEPRECATED deliveryStreamName "Use generic-lens or generic-optics with 'deliveryStreamName' instead"  #-}

-- | The key to use as the starting point for the list of tags. If you set this parameter, @ListTagsForDeliveryStream@ gets all tags that occur after @ExclusiveStartTagKey@ .
--
-- /Note:/ Consider using 'exclusiveStartTagKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfdsExclusiveStartTagKey :: Lens.Lens' ListTagsForDeliveryStream (Core.Maybe Types.TagKey)
ltfdsExclusiveStartTagKey = Lens.field @"exclusiveStartTagKey"
{-# INLINEABLE ltfdsExclusiveStartTagKey #-}
{-# DEPRECATED exclusiveStartTagKey "Use generic-lens or generic-optics with 'exclusiveStartTagKey' instead"  #-}

-- | The number of tags to return. If this number is less than the total number of tags associated with the delivery stream, @HasMoreTags@ is set to @true@ in the response. To list additional tags, set @ExclusiveStartTagKey@ to the last key in the response. 
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfdsLimit :: Lens.Lens' ListTagsForDeliveryStream (Core.Maybe Core.Natural)
ltfdsLimit = Lens.field @"limit"
{-# INLINEABLE ltfdsLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

instance Core.ToQuery ListTagsForDeliveryStream where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListTagsForDeliveryStream where
        toHeaders ListTagsForDeliveryStream{..}
          = Core.pure
              ("X-Amz-Target", "Firehose_20150804.ListTagsForDeliveryStream")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListTagsForDeliveryStream where
        toJSON ListTagsForDeliveryStream{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DeliveryStreamName" Core..= deliveryStreamName),
                  ("ExclusiveStartTagKey" Core..=) Core.<$> exclusiveStartTagKey,
                  ("Limit" Core..=) Core.<$> limit])

instance Core.AWSRequest ListTagsForDeliveryStream where
        type Rs ListTagsForDeliveryStream =
             ListTagsForDeliveryStreamResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListTagsForDeliveryStreamResponse' Core.<$>
                   (x Core..:? "Tags" Core..!= Core.mempty) Core.<*>
                     x Core..: "HasMoreTags"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListTagsForDeliveryStreamResponse' smart constructor.
data ListTagsForDeliveryStreamResponse = ListTagsForDeliveryStreamResponse'
  { tags :: [Types.Tag]
    -- ^ A list of tags associated with @DeliveryStreamName@ , starting with the first tag after @ExclusiveStartTagKey@ and up to the specified @Limit@ .
  , hasMoreTags :: Core.Bool
    -- ^ If this is @true@ in the response, more tags are available. To list the remaining tags, set @ExclusiveStartTagKey@ to the key of the last tag returned and call @ListTagsForDeliveryStream@ again.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTagsForDeliveryStreamResponse' value with any optional fields omitted.
mkListTagsForDeliveryStreamResponse
    :: Core.Bool -- ^ 'hasMoreTags'
    -> Core.Int -- ^ 'responseStatus'
    -> ListTagsForDeliveryStreamResponse
mkListTagsForDeliveryStreamResponse hasMoreTags responseStatus
  = ListTagsForDeliveryStreamResponse'{tags = Core.mempty,
                                       hasMoreTags, responseStatus}

-- | A list of tags associated with @DeliveryStreamName@ , starting with the first tag after @ExclusiveStartTagKey@ and up to the specified @Limit@ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfdsrrsTags :: Lens.Lens' ListTagsForDeliveryStreamResponse [Types.Tag]
ltfdsrrsTags = Lens.field @"tags"
{-# INLINEABLE ltfdsrrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | If this is @true@ in the response, more tags are available. To list the remaining tags, set @ExclusiveStartTagKey@ to the key of the last tag returned and call @ListTagsForDeliveryStream@ again.
--
-- /Note:/ Consider using 'hasMoreTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfdsrrsHasMoreTags :: Lens.Lens' ListTagsForDeliveryStreamResponse Core.Bool
ltfdsrrsHasMoreTags = Lens.field @"hasMoreTags"
{-# INLINEABLE ltfdsrrsHasMoreTags #-}
{-# DEPRECATED hasMoreTags "Use generic-lens or generic-optics with 'hasMoreTags' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfdsrrsResponseStatus :: Lens.Lens' ListTagsForDeliveryStreamResponse Core.Int
ltfdsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ltfdsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
