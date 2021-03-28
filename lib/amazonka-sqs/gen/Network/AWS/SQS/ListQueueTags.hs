{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.ListQueueTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all cost allocation tags added to the specified Amazon SQS queue. For an overview, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-queue-tags.html Tagging Your Amazon SQS Queues> in the /Amazon Simple Queue Service Developer Guide/ .
module Network.AWS.SQS.ListQueueTags
    (
    -- * Creating a request
      ListQueueTags (..)
    , mkListQueueTags
    -- ** Request lenses
    , lqtQueueUrl

    -- * Destructuring the response
    , ListQueueTagsResponse (..)
    , mkListQueueTagsResponse
    -- ** Response lenses
    , lqtrrsTags
    , lqtrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SQS.Types as Types

-- | /See:/ 'mkListQueueTags' smart constructor.
newtype ListQueueTags = ListQueueTags'
  { queueUrl :: Core.Text
    -- ^ The URL of the queue.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ListQueueTags' value with any optional fields omitted.
mkListQueueTags
    :: Core.Text -- ^ 'queueUrl'
    -> ListQueueTags
mkListQueueTags queueUrl = ListQueueTags'{queueUrl}

-- | The URL of the queue.
--
-- /Note:/ Consider using 'queueUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqtQueueUrl :: Lens.Lens' ListQueueTags Core.Text
lqtQueueUrl = Lens.field @"queueUrl"
{-# INLINEABLE lqtQueueUrl #-}
{-# DEPRECATED queueUrl "Use generic-lens or generic-optics with 'queueUrl' instead"  #-}

instance Core.ToQuery ListQueueTags where
        toQuery ListQueueTags{..}
          = Core.toQueryPair "Action" ("ListQueueTags" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2012-11-05" :: Core.Text)
              Core.<> Core.toQueryPair "QueueUrl" queueUrl

instance Core.ToHeaders ListQueueTags where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListQueueTags where
        type Rs ListQueueTags = ListQueueTagsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "ListQueueTagsResult"
              (\ s h x ->
                 ListQueueTagsResponse' Core.<$>
                   (x Core..@? "Tag") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListQueueTagsResponse' smart constructor.
data ListQueueTagsResponse = ListQueueTagsResponse'
  { tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue)
    -- ^ The list of all tags added to the specified queue.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListQueueTagsResponse' value with any optional fields omitted.
mkListQueueTagsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListQueueTagsResponse
mkListQueueTagsResponse responseStatus
  = ListQueueTagsResponse'{tags = Core.Nothing, responseStatus}

-- | The list of all tags added to the specified queue.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqtrrsTags :: Lens.Lens' ListQueueTagsResponse (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
lqtrrsTags = Lens.field @"tags"
{-# INLINEABLE lqtrrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqtrrsResponseStatus :: Lens.Lens' ListQueueTagsResponse Core.Int
lqtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lqtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
