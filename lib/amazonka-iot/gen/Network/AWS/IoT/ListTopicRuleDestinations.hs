{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListTopicRuleDestinations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the topic rule destinations in your AWS account.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListTopicRuleDestinations
  ( -- * Creating a request
    ListTopicRuleDestinations (..),
    mkListTopicRuleDestinations,

    -- ** Request lenses
    ltrdMaxResults,
    ltrdNextToken,

    -- * Destructuring the response
    ListTopicRuleDestinationsResponse (..),
    mkListTopicRuleDestinationsResponse,

    -- ** Response lenses
    ltrdrrsDestinationSummaries,
    ltrdrrsNextToken,
    ltrdrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListTopicRuleDestinations' smart constructor.
data ListTopicRuleDestinations = ListTopicRuleDestinations'
  { -- | The maximum number of results to return at one time.
    maxResults :: Core.Maybe Core.Natural,
    -- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTopicRuleDestinations' value with any optional fields omitted.
mkListTopicRuleDestinations ::
  ListTopicRuleDestinations
mkListTopicRuleDestinations =
  ListTopicRuleDestinations'
    { maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The maximum number of results to return at one time.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrdMaxResults :: Lens.Lens' ListTopicRuleDestinations (Core.Maybe Core.Natural)
ltrdMaxResults = Lens.field @"maxResults"
{-# DEPRECATED ltrdMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrdNextToken :: Lens.Lens' ListTopicRuleDestinations (Core.Maybe Types.NextToken)
ltrdNextToken = Lens.field @"nextToken"
{-# DEPRECATED ltrdNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListTopicRuleDestinations where
  type
    Rs ListTopicRuleDestinations =
      ListTopicRuleDestinationsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/destinations",
        Core._rqQuery =
          Core.toQueryValue "maxResults" Core.<$> maxResults
            Core.<> (Core.toQueryValue "nextToken" Core.<$> nextToken),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTopicRuleDestinationsResponse'
            Core.<$> (x Core..:? "destinationSummaries")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListTopicRuleDestinations where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"destinationSummaries" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListTopicRuleDestinationsResponse' smart constructor.
data ListTopicRuleDestinationsResponse = ListTopicRuleDestinationsResponse'
  { -- | Information about a topic rule destination.
    destinationSummaries :: Core.Maybe [Types.TopicRuleDestinationSummary],
    -- | The token to use to get the next set of results, or __null__ if there are no additional results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTopicRuleDestinationsResponse' value with any optional fields omitted.
mkListTopicRuleDestinationsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListTopicRuleDestinationsResponse
mkListTopicRuleDestinationsResponse responseStatus =
  ListTopicRuleDestinationsResponse'
    { destinationSummaries =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Information about a topic rule destination.
--
-- /Note:/ Consider using 'destinationSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrdrrsDestinationSummaries :: Lens.Lens' ListTopicRuleDestinationsResponse (Core.Maybe [Types.TopicRuleDestinationSummary])
ltrdrrsDestinationSummaries = Lens.field @"destinationSummaries"
{-# DEPRECATED ltrdrrsDestinationSummaries "Use generic-lens or generic-optics with 'destinationSummaries' instead." #-}

-- | The token to use to get the next set of results, or __null__ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrdrrsNextToken :: Lens.Lens' ListTopicRuleDestinationsResponse (Core.Maybe Types.NextToken)
ltrdrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ltrdrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrdrrsResponseStatus :: Lens.Lens' ListTopicRuleDestinationsResponse Core.Int
ltrdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ltrdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
