{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.DescribeEventAggregates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the number of events of each event type (issue, scheduled change, and account notification). If no filter is specified, the counts of all events in each category are returned.
--
-- This operation returns paginated results.
module Network.AWS.AWSHealth.DescribeEventAggregates
    (
    -- * Creating a request
      DescribeEventAggregates (..)
    , mkDescribeEventAggregates
    -- ** Request lenses
    , deaAggregateField
    , deaFilter
    , deaMaxResults
    , deaNextToken

    -- * Destructuring the response
    , DescribeEventAggregatesResponse (..)
    , mkDescribeEventAggregatesResponse
    -- ** Response lenses
    , drsEventAggregates
    , drsNextToken
    , drsResponseStatus
    ) where

import qualified Network.AWS.AWSHealth.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeEventAggregates' smart constructor.
data DescribeEventAggregates = DescribeEventAggregates'
  { aggregateField :: Types.EventAggregateField
    -- ^ The only currently supported value is @eventTypeCategory@ .
  , filter :: Core.Maybe Types.EventFilter
    -- ^ Values to narrow the results returned.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of items to return in one batch, between 10 and 100, inclusive.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeEventAggregates' value with any optional fields omitted.
mkDescribeEventAggregates
    :: Types.EventAggregateField -- ^ 'aggregateField'
    -> DescribeEventAggregates
mkDescribeEventAggregates aggregateField
  = DescribeEventAggregates'{aggregateField, filter = Core.Nothing,
                             maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The only currently supported value is @eventTypeCategory@ .
--
-- /Note:/ Consider using 'aggregateField' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deaAggregateField :: Lens.Lens' DescribeEventAggregates Types.EventAggregateField
deaAggregateField = Lens.field @"aggregateField"
{-# INLINEABLE deaAggregateField #-}
{-# DEPRECATED aggregateField "Use generic-lens or generic-optics with 'aggregateField' instead"  #-}

-- | Values to narrow the results returned.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deaFilter :: Lens.Lens' DescribeEventAggregates (Core.Maybe Types.EventFilter)
deaFilter = Lens.field @"filter"
{-# INLINEABLE deaFilter #-}
{-# DEPRECATED filter "Use generic-lens or generic-optics with 'filter' instead"  #-}

-- | The maximum number of items to return in one batch, between 10 and 100, inclusive.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deaMaxResults :: Lens.Lens' DescribeEventAggregates (Core.Maybe Core.Natural)
deaMaxResults = Lens.field @"maxResults"
{-# INLINEABLE deaMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deaNextToken :: Lens.Lens' DescribeEventAggregates (Core.Maybe Types.NextToken)
deaNextToken = Lens.field @"nextToken"
{-# INLINEABLE deaNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeEventAggregates where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeEventAggregates where
        toHeaders DescribeEventAggregates{..}
          = Core.pure
              ("X-Amz-Target", "AWSHealth_20160804.DescribeEventAggregates")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeEventAggregates where
        toJSON DescribeEventAggregates{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("aggregateField" Core..= aggregateField),
                  ("filter" Core..=) Core.<$> filter,
                  ("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest DescribeEventAggregates where
        type Rs DescribeEventAggregates = DescribeEventAggregatesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeEventAggregatesResponse' Core.<$>
                   (x Core..:? "eventAggregates") Core.<*> x Core..:? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeEventAggregates where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"eventAggregates" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeEventAggregatesResponse' smart constructor.
data DescribeEventAggregatesResponse = DescribeEventAggregatesResponse'
  { eventAggregates :: Core.Maybe [Types.EventAggregate]
    -- ^ The number of events in each category that meet the optional filter criteria.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEventAggregatesResponse' value with any optional fields omitted.
mkDescribeEventAggregatesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeEventAggregatesResponse
mkDescribeEventAggregatesResponse responseStatus
  = DescribeEventAggregatesResponse'{eventAggregates = Core.Nothing,
                                     nextToken = Core.Nothing, responseStatus}

-- | The number of events in each category that meet the optional filter criteria.
--
-- /Note:/ Consider using 'eventAggregates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsEventAggregates :: Lens.Lens' DescribeEventAggregatesResponse (Core.Maybe [Types.EventAggregate])
drsEventAggregates = Lens.field @"eventAggregates"
{-# INLINEABLE drsEventAggregates #-}
{-# DEPRECATED eventAggregates "Use generic-lens or generic-optics with 'eventAggregates' instead"  #-}

-- | If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsNextToken :: Lens.Lens' DescribeEventAggregatesResponse (Core.Maybe Types.NextToken)
drsNextToken = Lens.field @"nextToken"
{-# INLINEABLE drsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeEventAggregatesResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
