{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ListQueueTags (..),
    mkListQueueTags,

    -- ** Request lenses
    lqtQueueUrl,

    -- * Destructuring the response
    ListQueueTagsResponse (..),
    mkListQueueTagsResponse,

    -- ** Response lenses
    lqtrrsTags,
    lqtrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SQS.Types as Types

-- | /See:/ 'mkListQueueTags' smart constructor.
newtype ListQueueTags = ListQueueTags'
  { -- | The URL of the queue.
    queueUrl :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ListQueueTags' value with any optional fields omitted.
mkListQueueTags ::
  -- | 'queueUrl'
  Types.String ->
  ListQueueTags
mkListQueueTags queueUrl = ListQueueTags' {queueUrl}

-- | The URL of the queue.
--
-- /Note:/ Consider using 'queueUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqtQueueUrl :: Lens.Lens' ListQueueTags Types.String
lqtQueueUrl = Lens.field @"queueUrl"
{-# DEPRECATED lqtQueueUrl "Use generic-lens or generic-optics with 'queueUrl' instead." #-}

instance Core.AWSRequest ListQueueTags where
  type Rs ListQueueTags = ListQueueTagsResponse
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
            ( Core.pure ("Action", "ListQueueTags")
                Core.<> (Core.pure ("Version", "2012-11-05"))
                Core.<> (Core.toQueryValue "QueueUrl" queueUrl)
            )
      }
  response =
    Response.receiveXMLWrapper
      "ListQueueTagsResult"
      ( \s h x ->
          ListQueueTagsResponse'
            Core.<$> (x Core..@? "Tag") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListQueueTagsResponse' smart constructor.
data ListQueueTagsResponse = ListQueueTagsResponse'
  { -- | The list of all tags added to the specified queue.
    tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue),
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListQueueTagsResponse' value with any optional fields omitted.
mkListQueueTagsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListQueueTagsResponse
mkListQueueTagsResponse responseStatus =
  ListQueueTagsResponse' {tags = Core.Nothing, responseStatus}

-- | The list of all tags added to the specified queue.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqtrrsTags :: Lens.Lens' ListQueueTagsResponse (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
lqtrrsTags = Lens.field @"tags"
{-# DEPRECATED lqtrrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqtrrsResponseStatus :: Lens.Lens' ListQueueTagsResponse Core.Int
lqtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lqtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
