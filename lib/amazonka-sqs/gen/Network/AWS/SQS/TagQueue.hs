{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.TagQueue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Add cost allocation tags to the specified Amazon SQS queue. For an overview, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-queue-tags.html Tagging Your Amazon SQS Queues> in the /Amazon Simple Queue Service Developer Guide/ .
--
-- When you use queue tags, keep the following guidelines in mind:
--
--     * Adding more than 50 tags to a queue isn't recommended.
--
--
--     * Tags don't have any semantic meaning. Amazon SQS interprets tags as character strings.
--
--
--     * Tags are case-sensitive.
--
--
--     * A new tag with a key identical to that of an existing tag overwrites the existing tag.
--
--
-- For a full list of tag restrictions, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-limits.html#limits-queues Limits Related to Queues> in the /Amazon Simple Queue Service Developer Guide/ .
module Network.AWS.SQS.TagQueue
    (
    -- * Creating a request
      TagQueue (..)
    , mkTagQueue
    -- ** Request lenses
    , tqQueueUrl
    , tqTags

    -- * Destructuring the response
    , TagQueueResponse (..)
    , mkTagQueueResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SQS.Types as Types

-- | /See:/ 'mkTagQueue' smart constructor.
data TagQueue = TagQueue'
  { queueUrl :: Core.Text
    -- ^ The URL of the queue.
  , tags :: Core.HashMap Types.TagKey Types.TagValue
    -- ^ The list of tags to be added to the specified queue.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TagQueue' value with any optional fields omitted.
mkTagQueue
    :: Core.Text -- ^ 'queueUrl'
    -> TagQueue
mkTagQueue queueUrl = TagQueue'{queueUrl, tags = Core.mempty}

-- | The URL of the queue.
--
-- /Note:/ Consider using 'queueUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tqQueueUrl :: Lens.Lens' TagQueue Core.Text
tqQueueUrl = Lens.field @"queueUrl"
{-# INLINEABLE tqQueueUrl #-}
{-# DEPRECATED queueUrl "Use generic-lens or generic-optics with 'queueUrl' instead"  #-}

-- | The list of tags to be added to the specified queue.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tqTags :: Lens.Lens' TagQueue (Core.HashMap Types.TagKey Types.TagValue)
tqTags = Lens.field @"tags"
{-# INLINEABLE tqTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery TagQueue where
        toQuery TagQueue{..}
          = Core.toQueryPair "Action" ("TagQueue" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2012-11-05" :: Core.Text)
              Core.<> Core.toQueryPair "QueueUrl" queueUrl
              Core.<> Core.toQueryMap "Tags" "Key" "Value" tags

instance Core.ToHeaders TagQueue where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest TagQueue where
        type Rs TagQueue = TagQueueResponse
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
        parseResponse = Response.receiveNull TagQueueResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkTagQueueResponse' smart constructor.
data TagQueueResponse = TagQueueResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TagQueueResponse' value with any optional fields omitted.
mkTagQueueResponse
    :: TagQueueResponse
mkTagQueueResponse = TagQueueResponse'
