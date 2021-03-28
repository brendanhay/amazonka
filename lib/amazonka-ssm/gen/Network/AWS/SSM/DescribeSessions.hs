{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeSessions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of all active sessions (both connected and disconnected) or terminated sessions from the past 30 days.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeSessions
    (
    -- * Creating a request
      DescribeSessions (..)
    , mkDescribeSessions
    -- ** Request lenses
    , dsState
    , dsFilters
    , dsMaxResults
    , dsNextToken

    -- * Destructuring the response
    , DescribeSessionsResponse (..)
    , mkDescribeSessionsResponse
    -- ** Response lenses
    , dsrrsNextToken
    , dsrrsSessions
    , dsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkDescribeSessions' smart constructor.
data DescribeSessions = DescribeSessions'
  { state :: Types.SessionState
    -- ^ The session status to retrieve a list of sessions for. For example, "Active".
  , filters :: Core.Maybe (Core.NonEmpty Types.SessionFilter)
    -- ^ One or more filters to limit the type of sessions returned by the request.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next set of items to return. (You received this token from a previous call.)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSessions' value with any optional fields omitted.
mkDescribeSessions
    :: Types.SessionState -- ^ 'state'
    -> DescribeSessions
mkDescribeSessions state
  = DescribeSessions'{state, filters = Core.Nothing,
                      maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The session status to retrieve a list of sessions for. For example, "Active".
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsState :: Lens.Lens' DescribeSessions Types.SessionState
dsState = Lens.field @"state"
{-# INLINEABLE dsState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | One or more filters to limit the type of sessions returned by the request.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsFilters :: Lens.Lens' DescribeSessions (Core.Maybe (Core.NonEmpty Types.SessionFilter))
dsFilters = Lens.field @"filters"
{-# INLINEABLE dsFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsMaxResults :: Lens.Lens' DescribeSessions (Core.Maybe Core.Natural)
dsMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dsMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsNextToken :: Lens.Lens' DescribeSessions (Core.Maybe Types.NextToken)
dsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeSessions where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeSessions where
        toHeaders DescribeSessions{..}
          = Core.pure ("X-Amz-Target", "AmazonSSM.DescribeSessions") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeSessions where
        toJSON DescribeSessions{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("State" Core..= state),
                  ("Filters" Core..=) Core.<$> filters,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest DescribeSessions where
        type Rs DescribeSessions = DescribeSessionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeSessionsResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "Sessions" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeSessions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"sessions" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeSessionsResponse' smart constructor.
data DescribeSessionsResponse = DescribeSessionsResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next set of items to return. (You received this token from a previous call.)
  , sessions :: Core.Maybe [Types.Session]
    -- ^ A list of sessions meeting the request parameters.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeSessionsResponse' value with any optional fields omitted.
mkDescribeSessionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeSessionsResponse
mkDescribeSessionsResponse responseStatus
  = DescribeSessionsResponse'{nextToken = Core.Nothing,
                              sessions = Core.Nothing, responseStatus}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsNextToken :: Lens.Lens' DescribeSessionsResponse (Core.Maybe Types.NextToken)
dsrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dsrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A list of sessions meeting the request parameters.
--
-- /Note:/ Consider using 'sessions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsSessions :: Lens.Lens' DescribeSessionsResponse (Core.Maybe [Types.Session])
dsrrsSessions = Lens.field @"sessions"
{-# INLINEABLE dsrrsSessions #-}
{-# DEPRECATED sessions "Use generic-lens or generic-optics with 'sessions' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsResponseStatus :: Lens.Lens' DescribeSessionsResponse Core.Int
dsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
