{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.ListQueues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of your queues in the current region. The response includes a maximum of 1,000 results. If you specify a value for the optional @QueueNamePrefix@ parameter, only queues with a name that begins with the specified value are returned.
--
-- The @listQueues@ methods supports pagination. Set parameter @MaxResults@ in the request to specify the maximum number of results to be returned in the response. If you do not set @MaxResults@ , the response includes a maximum of 1,000 results. If you set @MaxResults@ and there are additional results to display, the response includes a value for @NextToken@ . Use @NextToken@ as a parameter in your next request to @listQueues@ to receive the next page of results.
--
-- This operation returns paginated results.
module Network.AWS.SQS.ListQueues
  ( -- * Creating a request
    ListQueues (..),
    mkListQueues,

    -- ** Request lenses
    lqMaxResults,
    lqNextToken,
    lqQueueNamePrefix,

    -- * Destructuring the response
    ListQueuesResponse (..),
    mkListQueuesResponse,

    -- ** Response lenses
    lqrrsNextToken,
    lqrrsQueueUrls,
    lqrrsResponseStatus,
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
-- /See:/ 'mkListQueues' smart constructor.
data ListQueues = ListQueues'
  { -- | Maximum number of results to include in the response. Value range is 1 to 1000. You must set @MaxResults@ to receive a value for @NextToken@ in the response.
    maxResults :: Core.Maybe Core.Int,
    -- | Pagination token to request the next set of results.
    nextToken :: Core.Maybe Types.Token,
    -- | A string to use for filtering the list results. Only those queues whose name begins with the specified string are returned.
    --
    -- Queue URLs and names are case-sensitive.
    queueNamePrefix :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListQueues' value with any optional fields omitted.
mkListQueues ::
  ListQueues
mkListQueues =
  ListQueues'
    { maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      queueNamePrefix = Core.Nothing
    }

-- | Maximum number of results to include in the response. Value range is 1 to 1000. You must set @MaxResults@ to receive a value for @NextToken@ in the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqMaxResults :: Lens.Lens' ListQueues (Core.Maybe Core.Int)
lqMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lqMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Pagination token to request the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqNextToken :: Lens.Lens' ListQueues (Core.Maybe Types.Token)
lqNextToken = Lens.field @"nextToken"
{-# DEPRECATED lqNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A string to use for filtering the list results. Only those queues whose name begins with the specified string are returned.
--
-- Queue URLs and names are case-sensitive.
--
-- /Note:/ Consider using 'queueNamePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqQueueNamePrefix :: Lens.Lens' ListQueues (Core.Maybe Types.String)
lqQueueNamePrefix = Lens.field @"queueNamePrefix"
{-# DEPRECATED lqQueueNamePrefix "Use generic-lens or generic-optics with 'queueNamePrefix' instead." #-}

instance Core.AWSRequest ListQueues where
  type Rs ListQueues = ListQueuesResponse
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
            ( Core.pure ("Action", "ListQueues")
                Core.<> (Core.pure ("Version", "2012-11-05"))
                Core.<> (Core.toQueryValue "MaxResults" Core.<$> maxResults)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
                Core.<> (Core.toQueryValue "QueueNamePrefix" Core.<$> queueNamePrefix)
            )
      }
  response =
    Response.receiveXMLWrapper
      "ListQueuesResult"
      ( \s h x ->
          ListQueuesResponse'
            Core.<$> (x Core..@? "NextToken")
            Core.<*> (x Core..@? "QueueUrl")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListQueues where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"queueUrls" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | A list of your queues.
--
-- /See:/ 'mkListQueuesResponse' smart constructor.
data ListQueuesResponse = ListQueuesResponse'
  { -- | Pagination token to include in the next request. Token value is @null@ if there are no additional results to request, or if you did not set @MaxResults@ in the request.
    nextToken :: Core.Maybe Types.Token,
    -- | A list of queue URLs, up to 1,000 entries, or the value of MaxResults that you sent in the request.
    queueUrls :: Core.Maybe [Types.String],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListQueuesResponse' value with any optional fields omitted.
mkListQueuesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListQueuesResponse
mkListQueuesResponse responseStatus =
  ListQueuesResponse'
    { nextToken = Core.Nothing,
      queueUrls = Core.Nothing,
      responseStatus
    }

-- | Pagination token to include in the next request. Token value is @null@ if there are no additional results to request, or if you did not set @MaxResults@ in the request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqrrsNextToken :: Lens.Lens' ListQueuesResponse (Core.Maybe Types.Token)
lqrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lqrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of queue URLs, up to 1,000 entries, or the value of MaxResults that you sent in the request.
--
-- /Note:/ Consider using 'queueUrls' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqrrsQueueUrls :: Lens.Lens' ListQueuesResponse (Core.Maybe [Types.String])
lqrrsQueueUrls = Lens.field @"queueUrls"
{-# DEPRECATED lqrrsQueueUrls "Use generic-lens or generic-optics with 'queueUrls' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqrrsResponseStatus :: Lens.Lens' ListQueuesResponse Core.Int
lqrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lqrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
