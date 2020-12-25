{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    UntagQueue (..),
    mkUntagQueue,

    -- ** Request lenses
    uqQueueUrl,
    uqTagKeys,

    -- * Destructuring the response
    UntagQueueResponse (..),
    mkUntagQueueResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SQS.Types as Types

-- | /See:/ 'mkUntagQueue' smart constructor.
data UntagQueue = UntagQueue'
  { -- | The URL of the queue.
    queueUrl :: Types.String,
    -- | The list of tags to be removed from the specified queue.
    tagKeys :: [Types.TagKey]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UntagQueue' value with any optional fields omitted.
mkUntagQueue ::
  -- | 'queueUrl'
  Types.String ->
  UntagQueue
mkUntagQueue queueUrl =
  UntagQueue' {queueUrl, tagKeys = Core.mempty}

-- | The URL of the queue.
--
-- /Note:/ Consider using 'queueUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uqQueueUrl :: Lens.Lens' UntagQueue Types.String
uqQueueUrl = Lens.field @"queueUrl"
{-# DEPRECATED uqQueueUrl "Use generic-lens or generic-optics with 'queueUrl' instead." #-}

-- | The list of tags to be removed from the specified queue.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uqTagKeys :: Lens.Lens' UntagQueue [Types.TagKey]
uqTagKeys = Lens.field @"tagKeys"
{-# DEPRECATED uqTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

instance Core.AWSRequest UntagQueue where
  type Rs UntagQueue = UntagQueueResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "UntagQueue")
                Core.<> (Core.pure ("Version", "2012-11-05"))
                Core.<> (Core.toQueryValue "QueueUrl" queueUrl)
                Core.<> (Core.toQueryList "TagKey" tagKeys)
            )
      }
  response = Response.receiveNull UntagQueueResponse'

-- | /See:/ 'mkUntagQueueResponse' smart constructor.
data UntagQueueResponse = UntagQueueResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UntagQueueResponse' value with any optional fields omitted.
mkUntagQueueResponse ::
  UntagQueueResponse
mkUntagQueueResponse = UntagQueueResponse'
