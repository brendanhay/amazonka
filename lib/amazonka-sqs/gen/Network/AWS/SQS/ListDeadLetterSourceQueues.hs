{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.ListDeadLetterSourceQueues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of your queues that have the @RedrivePolicy@ queue attribute configured with a dead-letter queue.
--
-- The @ListDeadLetterSourceQueues@ methods supports pagination. Set parameter @MaxResults@ in the request to specify the maximum number of results to be returned in the response. If you do not set @MaxResults@ , the response includes a maximum of 1,000 results. If you set @MaxResults@ and there are additional results to display, the response includes a value for @NextToken@ . Use @NextToken@ as a parameter in your next request to @ListDeadLetterSourceQueues@ to receive the next page of results.
-- For more information about using dead-letter queues, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-dead-letter-queues.html Using Amazon SQS Dead-Letter Queues> in the /Amazon Simple Queue Service Developer Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.SQS.ListDeadLetterSourceQueues
  ( -- * Creating a request
    ListDeadLetterSourceQueues (..),
    mkListDeadLetterSourceQueues,

    -- ** Request lenses
    ldlsqQueueUrl,
    ldlsqMaxResults,
    ldlsqNextToken,

    -- * Destructuring the response
    ListDeadLetterSourceQueuesResponse (..),
    mkListDeadLetterSourceQueuesResponse,

    -- ** Response lenses
    ldlsqrrsQueueUrls,
    ldlsqrrsNextToken,
    ldlsqrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SQS.Types as Types

-- |
--
-- /See:/ 'mkListDeadLetterSourceQueues' smart constructor.
data ListDeadLetterSourceQueues = ListDeadLetterSourceQueues'
  { -- | The URL of a dead-letter queue.
    --
    -- Queue URLs and names are case-sensitive.
    queueUrl :: Types.String,
    -- | Maximum number of results to include in the response. Value range is 1 to 1000. You must set @MaxResults@ to receive a value for @NextToken@ in the response.
    maxResults :: Core.Maybe Core.Int,
    -- | Pagination token to request the next set of results.
    nextToken :: Core.Maybe Types.Token
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDeadLetterSourceQueues' value with any optional fields omitted.
mkListDeadLetterSourceQueues ::
  -- | 'queueUrl'
  Types.String ->
  ListDeadLetterSourceQueues
mkListDeadLetterSourceQueues queueUrl =
  ListDeadLetterSourceQueues'
    { queueUrl,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The URL of a dead-letter queue.
--
-- Queue URLs and names are case-sensitive.
--
-- /Note:/ Consider using 'queueUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldlsqQueueUrl :: Lens.Lens' ListDeadLetterSourceQueues Types.String
ldlsqQueueUrl = Lens.field @"queueUrl"
{-# DEPRECATED ldlsqQueueUrl "Use generic-lens or generic-optics with 'queueUrl' instead." #-}

-- | Maximum number of results to include in the response. Value range is 1 to 1000. You must set @MaxResults@ to receive a value for @NextToken@ in the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldlsqMaxResults :: Lens.Lens' ListDeadLetterSourceQueues (Core.Maybe Core.Int)
ldlsqMaxResults = Lens.field @"maxResults"
{-# DEPRECATED ldlsqMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Pagination token to request the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldlsqNextToken :: Lens.Lens' ListDeadLetterSourceQueues (Core.Maybe Types.Token)
ldlsqNextToken = Lens.field @"nextToken"
{-# DEPRECATED ldlsqNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListDeadLetterSourceQueues where
  type
    Rs ListDeadLetterSourceQueues =
      ListDeadLetterSourceQueuesResponse
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
            ( Core.pure ("Action", "ListDeadLetterSourceQueues")
                Core.<> (Core.pure ("Version", "2012-11-05"))
                Core.<> (Core.toQueryValue "QueueUrl" queueUrl)
                Core.<> (Core.toQueryValue "MaxResults" Core.<$> maxResults)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
            )
      }
  response =
    Response.receiveXMLWrapper
      "ListDeadLetterSourceQueuesResult"
      ( \s h x ->
          ListDeadLetterSourceQueuesResponse'
            Core.<$> (x Core..@? "QueueUrl" Core..@! Core.mempty)
            Core.<*> (x Core..@? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListDeadLetterSourceQueues where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^. Lens.field @"queueUrls") = Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | A list of your dead letter source queues.
--
-- /See:/ 'mkListDeadLetterSourceQueuesResponse' smart constructor.
data ListDeadLetterSourceQueuesResponse = ListDeadLetterSourceQueuesResponse'
  { -- | A list of source queue URLs that have the @RedrivePolicy@ queue attribute configured with a dead-letter queue.
    queueUrls :: [Types.String],
    -- | Pagination token to include in the next request. Token value is @null@ if there are no additional results to request, or if you did not set @MaxResults@ in the request.
    nextToken :: Core.Maybe Types.Token,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDeadLetterSourceQueuesResponse' value with any optional fields omitted.
mkListDeadLetterSourceQueuesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListDeadLetterSourceQueuesResponse
mkListDeadLetterSourceQueuesResponse responseStatus =
  ListDeadLetterSourceQueuesResponse'
    { queueUrls = Core.mempty,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list of source queue URLs that have the @RedrivePolicy@ queue attribute configured with a dead-letter queue.
--
-- /Note:/ Consider using 'queueUrls' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldlsqrrsQueueUrls :: Lens.Lens' ListDeadLetterSourceQueuesResponse [Types.String]
ldlsqrrsQueueUrls = Lens.field @"queueUrls"
{-# DEPRECATED ldlsqrrsQueueUrls "Use generic-lens or generic-optics with 'queueUrls' instead." #-}

-- | Pagination token to include in the next request. Token value is @null@ if there are no additional results to request, or if you did not set @MaxResults@ in the request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldlsqrrsNextToken :: Lens.Lens' ListDeadLetterSourceQueuesResponse (Core.Maybe Types.Token)
ldlsqrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ldlsqrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldlsqrrsResponseStatus :: Lens.Lens' ListDeadLetterSourceQueuesResponse Core.Int
ldlsqrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ldlsqrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
