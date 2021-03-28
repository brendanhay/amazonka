{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.UntagQueue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove cost allocation tags from the specified Amazon SQS queue. For an overview, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-queue-tags.html Tagging Your Amazon SQS Queues> in the /Amazon Simple Queue Service Developer Guide/ .
module Network.AWS.SQS.UntagQueue
    (
    -- * Creating a request
      UntagQueue (..)
    , mkUntagQueue
    -- ** Request lenses
    , uqQueueUrl
    , uqTagKeys

    -- * Destructuring the response
    , UntagQueueResponse (..)
    , mkUntagQueueResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SQS.Types as Types

-- | /See:/ 'mkUntagQueue' smart constructor.
data UntagQueue = UntagQueue'
  { queueUrl :: Core.Text
    -- ^ The URL of the queue.
  , tagKeys :: [Types.TagKey]
    -- ^ The list of tags to be removed from the specified queue.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UntagQueue' value with any optional fields omitted.
mkUntagQueue
    :: Core.Text -- ^ 'queueUrl'
    -> UntagQueue
mkUntagQueue queueUrl
  = UntagQueue'{queueUrl, tagKeys = Core.mempty}

-- | The URL of the queue.
--
-- /Note:/ Consider using 'queueUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uqQueueUrl :: Lens.Lens' UntagQueue Core.Text
uqQueueUrl = Lens.field @"queueUrl"
{-# INLINEABLE uqQueueUrl #-}
{-# DEPRECATED queueUrl "Use generic-lens or generic-optics with 'queueUrl' instead"  #-}

-- | The list of tags to be removed from the specified queue.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uqTagKeys :: Lens.Lens' UntagQueue [Types.TagKey]
uqTagKeys = Lens.field @"tagKeys"
{-# INLINEABLE uqTagKeys #-}
{-# DEPRECATED tagKeys "Use generic-lens or generic-optics with 'tagKeys' instead"  #-}

instance Core.ToQuery UntagQueue where
        toQuery UntagQueue{..}
          = Core.toQueryPair "Action" ("UntagQueue" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2012-11-05" :: Core.Text)
              Core.<> Core.toQueryPair "QueueUrl" queueUrl
              Core.<> Core.toQueryList "TagKey" tagKeys

instance Core.ToHeaders UntagQueue where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest UntagQueue where
        type Rs UntagQueue = UntagQueueResponse
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
        parseResponse = Response.receiveNull UntagQueueResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUntagQueueResponse' smart constructor.
data UntagQueueResponse = UntagQueueResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UntagQueueResponse' value with any optional fields omitted.
mkUntagQueueResponse
    :: UntagQueueResponse
mkUntagQueueResponse = UntagQueueResponse'
