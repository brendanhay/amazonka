{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.DescribeDestinations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all your destinations. The results are ASCII-sorted by destination name.
--
-- This operation returns paginated results.
module Network.AWS.CloudWatchLogs.DescribeDestinations
  ( -- * Creating a request
    DescribeDestinations (..),
    mkDescribeDestinations,

    -- ** Request lenses
    ddDestinationNamePrefix,
    ddLimit,
    ddNextToken,

    -- * Destructuring the response
    DescribeDestinationsResponse (..),
    mkDescribeDestinationsResponse,

    -- ** Response lenses
    ddrrsDestinations,
    ddrrsNextToken,
    ddrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudWatchLogs.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeDestinations' smart constructor.
data DescribeDestinations = DescribeDestinations'
  { -- | The prefix to match. If you don't specify a value, no prefix filter is applied.
    destinationNamePrefix :: Core.Maybe Types.DestinationName,
    -- | The maximum number of items returned. If you don't specify a value, the default is up to 50 items.
    limit :: Core.Maybe Core.Natural,
    -- | The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDestinations' value with any optional fields omitted.
mkDescribeDestinations ::
  DescribeDestinations
mkDescribeDestinations =
  DescribeDestinations'
    { destinationNamePrefix = Core.Nothing,
      limit = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The prefix to match. If you don't specify a value, no prefix filter is applied.
--
-- /Note:/ Consider using 'destinationNamePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDestinationNamePrefix :: Lens.Lens' DescribeDestinations (Core.Maybe Types.DestinationName)
ddDestinationNamePrefix = Lens.field @"destinationNamePrefix"
{-# DEPRECATED ddDestinationNamePrefix "Use generic-lens or generic-optics with 'destinationNamePrefix' instead." #-}

-- | The maximum number of items returned. If you don't specify a value, the default is up to 50 items.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddLimit :: Lens.Lens' DescribeDestinations (Core.Maybe Core.Natural)
ddLimit = Lens.field @"limit"
{-# DEPRECATED ddLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddNextToken :: Lens.Lens' DescribeDestinations (Core.Maybe Types.NextToken)
ddNextToken = Lens.field @"nextToken"
{-# DEPRECATED ddNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON DescribeDestinations where
  toJSON DescribeDestinations {..} =
    Core.object
      ( Core.catMaybes
          [ ("DestinationNamePrefix" Core..=) Core.<$> destinationNamePrefix,
            ("limit" Core..=) Core.<$> limit,
            ("nextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest DescribeDestinations where
  type Rs DescribeDestinations = DescribeDestinationsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Logs_20140328.DescribeDestinations")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDestinationsResponse'
            Core.<$> (x Core..:? "destinations")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeDestinations where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"destinations" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeDestinationsResponse' smart constructor.
data DescribeDestinationsResponse = DescribeDestinationsResponse'
  { -- | The destinations.
    destinations :: Core.Maybe [Types.Destination],
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDestinationsResponse' value with any optional fields omitted.
mkDescribeDestinationsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeDestinationsResponse
mkDescribeDestinationsResponse responseStatus =
  DescribeDestinationsResponse'
    { destinations = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The destinations.
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsDestinations :: Lens.Lens' DescribeDestinationsResponse (Core.Maybe [Types.Destination])
ddrrsDestinations = Lens.field @"destinations"
{-# DEPRECATED ddrrsDestinations "Use generic-lens or generic-optics with 'destinations' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsNextToken :: Lens.Lens' DescribeDestinationsResponse (Core.Maybe Types.NextToken)
ddrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ddrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsResponseStatus :: Lens.Lens' DescribeDestinationsResponse Core.Int
ddrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
