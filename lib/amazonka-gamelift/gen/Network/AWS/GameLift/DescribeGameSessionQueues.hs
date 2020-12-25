{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DescribeGameSessionQueues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the properties for one or more game session queues. When requesting multiple queues, use the pagination parameters to retrieve results as a set of sequential pages. If successful, a 'GameSessionQueue' object is returned for each requested queue. When specifying a list of queues, objects are returned only for queues that currently exist in the Region.
--
-- __Learn more__
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/queues-console.html View Your Queues>
-- __Related operations__
--
--     * 'CreateGameSessionQueue'
--
--
--     * 'DescribeGameSessionQueues'
--
--
--     * 'UpdateGameSessionQueue'
--
--
--     * 'DeleteGameSessionQueue'
--
--
--
-- This operation returns paginated results.
module Network.AWS.GameLift.DescribeGameSessionQueues
  ( -- * Creating a request
    DescribeGameSessionQueues (..),
    mkDescribeGameSessionQueues,

    -- ** Request lenses
    dgsqLimit,
    dgsqNames,
    dgsqNextToken,

    -- * Destructuring the response
    DescribeGameSessionQueuesResponse (..),
    mkDescribeGameSessionQueuesResponse,

    -- ** Response lenses
    dgsqrfrsGameSessionQueues,
    dgsqrfrsNextToken,
    dgsqrfrsResponseStatus,
  )
where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkDescribeGameSessionQueues' smart constructor.
data DescribeGameSessionQueues = DescribeGameSessionQueues'
  { -- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages. You can request up to 50 results.
    limit :: Core.Maybe Core.Natural,
    -- | A list of queue names to retrieve information for. You can use either the queue ID or ARN value. To request settings for all queues, leave this parameter empty.
    names :: Core.Maybe [Types.GameSessionQueueNameOrArn],
    -- | A token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
    nextToken :: Core.Maybe Types.NonZeroAndMaxString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeGameSessionQueues' value with any optional fields omitted.
mkDescribeGameSessionQueues ::
  DescribeGameSessionQueues
mkDescribeGameSessionQueues =
  DescribeGameSessionQueues'
    { limit = Core.Nothing,
      names = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages. You can request up to 50 results.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsqLimit :: Lens.Lens' DescribeGameSessionQueues (Core.Maybe Core.Natural)
dgsqLimit = Lens.field @"limit"
{-# DEPRECATED dgsqLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | A list of queue names to retrieve information for. You can use either the queue ID or ARN value. To request settings for all queues, leave this parameter empty.
--
-- /Note:/ Consider using 'names' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsqNames :: Lens.Lens' DescribeGameSessionQueues (Core.Maybe [Types.GameSessionQueueNameOrArn])
dgsqNames = Lens.field @"names"
{-# DEPRECATED dgsqNames "Use generic-lens or generic-optics with 'names' instead." #-}

-- | A token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsqNextToken :: Lens.Lens' DescribeGameSessionQueues (Core.Maybe Types.NonZeroAndMaxString)
dgsqNextToken = Lens.field @"nextToken"
{-# DEPRECATED dgsqNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON DescribeGameSessionQueues where
  toJSON DescribeGameSessionQueues {..} =
    Core.object
      ( Core.catMaybes
          [ ("Limit" Core..=) Core.<$> limit,
            ("Names" Core..=) Core.<$> names,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest DescribeGameSessionQueues where
  type
    Rs DescribeGameSessionQueues =
      DescribeGameSessionQueuesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "GameLift.DescribeGameSessionQueues")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeGameSessionQueuesResponse'
            Core.<$> (x Core..:? "GameSessionQueues")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeGameSessionQueues where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"gameSessionQueues" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkDescribeGameSessionQueuesResponse' smart constructor.
data DescribeGameSessionQueuesResponse = DescribeGameSessionQueuesResponse'
  { -- | A collection of objects that describe the requested game session queues.
    gameSessionQueues :: Core.Maybe [Types.GameSessionQueue],
    -- | A token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
    nextToken :: Core.Maybe Types.NonZeroAndMaxString,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeGameSessionQueuesResponse' value with any optional fields omitted.
mkDescribeGameSessionQueuesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeGameSessionQueuesResponse
mkDescribeGameSessionQueuesResponse responseStatus =
  DescribeGameSessionQueuesResponse'
    { gameSessionQueues =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A collection of objects that describe the requested game session queues.
--
-- /Note:/ Consider using 'gameSessionQueues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsqrfrsGameSessionQueues :: Lens.Lens' DescribeGameSessionQueuesResponse (Core.Maybe [Types.GameSessionQueue])
dgsqrfrsGameSessionQueues = Lens.field @"gameSessionQueues"
{-# DEPRECATED dgsqrfrsGameSessionQueues "Use generic-lens or generic-optics with 'gameSessionQueues' instead." #-}

-- | A token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsqrfrsNextToken :: Lens.Lens' DescribeGameSessionQueuesResponse (Core.Maybe Types.NonZeroAndMaxString)
dgsqrfrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dgsqrfrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsqrfrsResponseStatus :: Lens.Lens' DescribeGameSessionQueuesResponse Core.Int
dgsqrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dgsqrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
