{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.DescribePullRequestEvents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about one or more pull request events.
--
-- This operation returns paginated results.
module Network.AWS.CodeCommit.DescribePullRequestEvents
  ( -- * Creating a request
    DescribePullRequestEvents (..),
    mkDescribePullRequestEvents,

    -- ** Request lenses
    dprePullRequestId,
    dpreActorArn,
    dpreMaxResults,
    dpreNextToken,
    dprePullRequestEventType,

    -- * Destructuring the response
    DescribePullRequestEventsResponse (..),
    mkDescribePullRequestEventsResponse,

    -- ** Response lenses
    dprerrsPullRequestEvents,
    dprerrsNextToken,
    dprerrsResponseStatus,
  )
where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribePullRequestEvents' smart constructor.
data DescribePullRequestEvents = DescribePullRequestEvents'
  { -- | The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
    pullRequestId :: Types.PullRequestId,
    -- | The Amazon Resource Name (ARN) of the user whose actions resulted in the event. Examples include updating the pull request with more commits or changing the status of a pull request.
    actorArn :: Core.Maybe Types.Arn,
    -- | A non-zero, non-negative integer used to limit the number of returned results. The default is 100 events, which is also the maximum number of events that can be returned in a result.
    maxResults :: Core.Maybe Core.Int,
    -- | An enumeration token that, when provided in a request, returns the next batch of the results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | Optional. The pull request event type about which you want to return information.
    pullRequestEventType :: Core.Maybe Types.PullRequestEventType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribePullRequestEvents' value with any optional fields omitted.
mkDescribePullRequestEvents ::
  -- | 'pullRequestId'
  Types.PullRequestId ->
  DescribePullRequestEvents
mkDescribePullRequestEvents pullRequestId =
  DescribePullRequestEvents'
    { pullRequestId,
      actorArn = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      pullRequestEventType = Core.Nothing
    }

-- | The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
--
-- /Note:/ Consider using 'pullRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprePullRequestId :: Lens.Lens' DescribePullRequestEvents Types.PullRequestId
dprePullRequestId = Lens.field @"pullRequestId"
{-# DEPRECATED dprePullRequestId "Use generic-lens or generic-optics with 'pullRequestId' instead." #-}

-- | The Amazon Resource Name (ARN) of the user whose actions resulted in the event. Examples include updating the pull request with more commits or changing the status of a pull request.
--
-- /Note:/ Consider using 'actorArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpreActorArn :: Lens.Lens' DescribePullRequestEvents (Core.Maybe Types.Arn)
dpreActorArn = Lens.field @"actorArn"
{-# DEPRECATED dpreActorArn "Use generic-lens or generic-optics with 'actorArn' instead." #-}

-- | A non-zero, non-negative integer used to limit the number of returned results. The default is 100 events, which is also the maximum number of events that can be returned in a result.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpreMaxResults :: Lens.Lens' DescribePullRequestEvents (Core.Maybe Core.Int)
dpreMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dpreMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | An enumeration token that, when provided in a request, returns the next batch of the results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpreNextToken :: Lens.Lens' DescribePullRequestEvents (Core.Maybe Types.NextToken)
dpreNextToken = Lens.field @"nextToken"
{-# DEPRECATED dpreNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Optional. The pull request event type about which you want to return information.
--
-- /Note:/ Consider using 'pullRequestEventType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprePullRequestEventType :: Lens.Lens' DescribePullRequestEvents (Core.Maybe Types.PullRequestEventType)
dprePullRequestEventType = Lens.field @"pullRequestEventType"
{-# DEPRECATED dprePullRequestEventType "Use generic-lens or generic-optics with 'pullRequestEventType' instead." #-}

instance Core.FromJSON DescribePullRequestEvents where
  toJSON DescribePullRequestEvents {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("pullRequestId" Core..= pullRequestId),
            ("actorArn" Core..=) Core.<$> actorArn,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("nextToken" Core..=) Core.<$> nextToken,
            ("pullRequestEventType" Core..=) Core.<$> pullRequestEventType
          ]
      )

instance Core.AWSRequest DescribePullRequestEvents where
  type
    Rs DescribePullRequestEvents =
      DescribePullRequestEventsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodeCommit_20150413.DescribePullRequestEvents")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePullRequestEventsResponse'
            Core.<$> (x Core..:? "pullRequestEvents" Core..!= Core.mempty)
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribePullRequestEvents where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^. Lens.field @"pullRequestEvents") =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribePullRequestEventsResponse' smart constructor.
data DescribePullRequestEventsResponse = DescribePullRequestEventsResponse'
  { -- | Information about the pull request events.
    pullRequestEvents :: [Types.PullRequestEvent],
    -- | An enumeration token that can be used in a request to return the next batch of the results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribePullRequestEventsResponse' value with any optional fields omitted.
mkDescribePullRequestEventsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribePullRequestEventsResponse
mkDescribePullRequestEventsResponse responseStatus =
  DescribePullRequestEventsResponse'
    { pullRequestEvents =
        Core.mempty,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Information about the pull request events.
--
-- /Note:/ Consider using 'pullRequestEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprerrsPullRequestEvents :: Lens.Lens' DescribePullRequestEventsResponse [Types.PullRequestEvent]
dprerrsPullRequestEvents = Lens.field @"pullRequestEvents"
{-# DEPRECATED dprerrsPullRequestEvents "Use generic-lens or generic-optics with 'pullRequestEvents' instead." #-}

-- | An enumeration token that can be used in a request to return the next batch of the results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprerrsNextToken :: Lens.Lens' DescribePullRequestEventsResponse (Core.Maybe Types.NextToken)
dprerrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dprerrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprerrsResponseStatus :: Lens.Lens' DescribePullRequestEventsResponse Core.Int
dprerrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dprerrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
