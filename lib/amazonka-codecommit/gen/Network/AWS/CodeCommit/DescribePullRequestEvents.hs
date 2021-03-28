{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribePullRequestEvents (..)
    , mkDescribePullRequestEvents
    -- ** Request lenses
    , dprePullRequestId
    , dpreActorArn
    , dpreMaxResults
    , dpreNextToken
    , dprePullRequestEventType

    -- * Destructuring the response
    , DescribePullRequestEventsResponse (..)
    , mkDescribePullRequestEventsResponse
    -- ** Response lenses
    , dprerrsPullRequestEvents
    , dprerrsNextToken
    , dprerrsResponseStatus
    ) where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribePullRequestEvents' smart constructor.
data DescribePullRequestEvents = DescribePullRequestEvents'
  { pullRequestId :: Types.PullRequestId
    -- ^ The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
  , actorArn :: Core.Maybe Types.Arn
    -- ^ The Amazon Resource Name (ARN) of the user whose actions resulted in the event. Examples include updating the pull request with more commits or changing the status of a pull request.
  , maxResults :: Core.Maybe Core.Int
    -- ^ A non-zero, non-negative integer used to limit the number of returned results. The default is 100 events, which is also the maximum number of events that can be returned in a result.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ An enumeration token that, when provided in a request, returns the next batch of the results.
  , pullRequestEventType :: Core.Maybe Types.PullRequestEventType
    -- ^ Optional. The pull request event type about which you want to return information.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribePullRequestEvents' value with any optional fields omitted.
mkDescribePullRequestEvents
    :: Types.PullRequestId -- ^ 'pullRequestId'
    -> DescribePullRequestEvents
mkDescribePullRequestEvents pullRequestId
  = DescribePullRequestEvents'{pullRequestId,
                               actorArn = Core.Nothing, maxResults = Core.Nothing,
                               nextToken = Core.Nothing, pullRequestEventType = Core.Nothing}

-- | The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
--
-- /Note:/ Consider using 'pullRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprePullRequestId :: Lens.Lens' DescribePullRequestEvents Types.PullRequestId
dprePullRequestId = Lens.field @"pullRequestId"
{-# INLINEABLE dprePullRequestId #-}
{-# DEPRECATED pullRequestId "Use generic-lens or generic-optics with 'pullRequestId' instead"  #-}

-- | The Amazon Resource Name (ARN) of the user whose actions resulted in the event. Examples include updating the pull request with more commits or changing the status of a pull request.
--
-- /Note:/ Consider using 'actorArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpreActorArn :: Lens.Lens' DescribePullRequestEvents (Core.Maybe Types.Arn)
dpreActorArn = Lens.field @"actorArn"
{-# INLINEABLE dpreActorArn #-}
{-# DEPRECATED actorArn "Use generic-lens or generic-optics with 'actorArn' instead"  #-}

-- | A non-zero, non-negative integer used to limit the number of returned results. The default is 100 events, which is also the maximum number of events that can be returned in a result.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpreMaxResults :: Lens.Lens' DescribePullRequestEvents (Core.Maybe Core.Int)
dpreMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dpreMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | An enumeration token that, when provided in a request, returns the next batch of the results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpreNextToken :: Lens.Lens' DescribePullRequestEvents (Core.Maybe Types.NextToken)
dpreNextToken = Lens.field @"nextToken"
{-# INLINEABLE dpreNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Optional. The pull request event type about which you want to return information.
--
-- /Note:/ Consider using 'pullRequestEventType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprePullRequestEventType :: Lens.Lens' DescribePullRequestEvents (Core.Maybe Types.PullRequestEventType)
dprePullRequestEventType = Lens.field @"pullRequestEventType"
{-# INLINEABLE dprePullRequestEventType #-}
{-# DEPRECATED pullRequestEventType "Use generic-lens or generic-optics with 'pullRequestEventType' instead"  #-}

instance Core.ToQuery DescribePullRequestEvents where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribePullRequestEvents where
        toHeaders DescribePullRequestEvents{..}
          = Core.pure
              ("X-Amz-Target", "CodeCommit_20150413.DescribePullRequestEvents")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribePullRequestEvents where
        toJSON DescribePullRequestEvents{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("pullRequestId" Core..= pullRequestId),
                  ("actorArn" Core..=) Core.<$> actorArn,
                  ("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken,
                  ("pullRequestEventType" Core..=) Core.<$> pullRequestEventType])

instance Core.AWSRequest DescribePullRequestEvents where
        type Rs DescribePullRequestEvents =
             DescribePullRequestEventsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribePullRequestEventsResponse' Core.<$>
                   (x Core..:? "pullRequestEvents" Core..!= Core.mempty) Core.<*>
                     x Core..:? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribePullRequestEvents where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^. Lens.field @"pullRequestEvents") =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribePullRequestEventsResponse' smart constructor.
data DescribePullRequestEventsResponse = DescribePullRequestEventsResponse'
  { pullRequestEvents :: [Types.PullRequestEvent]
    -- ^ Information about the pull request events.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ An enumeration token that can be used in a request to return the next batch of the results.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribePullRequestEventsResponse' value with any optional fields omitted.
mkDescribePullRequestEventsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribePullRequestEventsResponse
mkDescribePullRequestEventsResponse responseStatus
  = DescribePullRequestEventsResponse'{pullRequestEvents =
                                         Core.mempty,
                                       nextToken = Core.Nothing, responseStatus}

-- | Information about the pull request events.
--
-- /Note:/ Consider using 'pullRequestEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprerrsPullRequestEvents :: Lens.Lens' DescribePullRequestEventsResponse [Types.PullRequestEvent]
dprerrsPullRequestEvents = Lens.field @"pullRequestEvents"
{-# INLINEABLE dprerrsPullRequestEvents #-}
{-# DEPRECATED pullRequestEvents "Use generic-lens or generic-optics with 'pullRequestEvents' instead"  #-}

-- | An enumeration token that can be used in a request to return the next batch of the results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprerrsNextToken :: Lens.Lens' DescribePullRequestEventsResponse (Core.Maybe Types.NextToken)
dprerrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dprerrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprerrsResponseStatus :: Lens.Lens' DescribePullRequestEventsResponse Core.Int
dprerrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dprerrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
