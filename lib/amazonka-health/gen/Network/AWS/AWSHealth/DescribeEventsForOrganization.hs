{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.DescribeEventsForOrganization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about events across your organization in AWS Organizations. You can use the@filters@ parameter to specify the events that you want to return. Events are returned in a summary form and don't include the affected accounts, detailed description, any additional metadata that depends on the event type, or any affected resources. To retrieve that information, use the following operations:
--
--
--     * <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeAffectedAccountsForOrganization.html DescribeAffectedAccountsForOrganization> 
--
--
--     * <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventDetailsForOrganization.html DescribeEventDetailsForOrganization> 
--
--
--     * <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeAffectedEntitiesForOrganization.html DescribeAffectedEntitiesForOrganization> 
--
--
-- If you don't specify a @filter@ , the @DescribeEventsForOrganizations@ returns all events across your organization. Results are sorted by @lastModifiedTime@ , starting with the most recent event. 
-- For more information about the different types of AWS Health events, see <https://docs.aws.amazon.com/health/latest/APIReference/API_Event.html Event> .
-- Before you can call this operation, you must first enable AWS Health to work with AWS Organizations. To do this, call the <https://docs.aws.amazon.com/health/latest/APIReference/API_EnableHealthServiceAccessForOrganization.html EnableHealthServiceAccessForOrganization> operation from your organization's master AWS account.
--
-- This operation returns paginated results.
module Network.AWS.AWSHealth.DescribeEventsForOrganization
    (
    -- * Creating a request
      DescribeEventsForOrganization (..)
    , mkDescribeEventsForOrganization
    -- ** Request lenses
    , defoFilter
    , defoLocale
    , defoMaxResults
    , defoNextToken

    -- * Destructuring the response
    , DescribeEventsForOrganizationResponse (..)
    , mkDescribeEventsForOrganizationResponse
    -- ** Response lenses
    , deforrsEvents
    , deforrsNextToken
    , deforrsResponseStatus
    ) where

import qualified Network.AWS.AWSHealth.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeEventsForOrganization' smart constructor.
data DescribeEventsForOrganization = DescribeEventsForOrganization'
  { filter :: Core.Maybe Types.OrganizationEventFilter
    -- ^ Values to narrow the results returned.
  , locale :: Core.Maybe Types.Locale
    -- ^ The locale (language) to return information in. English (en) is the default and the only supported value at this time.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of items to return in one batch, between 10 and 100, inclusive.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeEventsForOrganization' value with any optional fields omitted.
mkDescribeEventsForOrganization
    :: DescribeEventsForOrganization
mkDescribeEventsForOrganization
  = DescribeEventsForOrganization'{filter = Core.Nothing,
                                   locale = Core.Nothing, maxResults = Core.Nothing,
                                   nextToken = Core.Nothing}

-- | Values to narrow the results returned.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
defoFilter :: Lens.Lens' DescribeEventsForOrganization (Core.Maybe Types.OrganizationEventFilter)
defoFilter = Lens.field @"filter"
{-# INLINEABLE defoFilter #-}
{-# DEPRECATED filter "Use generic-lens or generic-optics with 'filter' instead"  #-}

-- | The locale (language) to return information in. English (en) is the default and the only supported value at this time.
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
defoLocale :: Lens.Lens' DescribeEventsForOrganization (Core.Maybe Types.Locale)
defoLocale = Lens.field @"locale"
{-# INLINEABLE defoLocale #-}
{-# DEPRECATED locale "Use generic-lens or generic-optics with 'locale' instead"  #-}

-- | The maximum number of items to return in one batch, between 10 and 100, inclusive.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
defoMaxResults :: Lens.Lens' DescribeEventsForOrganization (Core.Maybe Core.Natural)
defoMaxResults = Lens.field @"maxResults"
{-# INLINEABLE defoMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
defoNextToken :: Lens.Lens' DescribeEventsForOrganization (Core.Maybe Types.NextToken)
defoNextToken = Lens.field @"nextToken"
{-# INLINEABLE defoNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeEventsForOrganization where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeEventsForOrganization where
        toHeaders DescribeEventsForOrganization{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSHealth_20160804.DescribeEventsForOrganization")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeEventsForOrganization where
        toJSON DescribeEventsForOrganization{..}
          = Core.object
              (Core.catMaybes
                 [("filter" Core..=) Core.<$> filter,
                  ("locale" Core..=) Core.<$> locale,
                  ("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest DescribeEventsForOrganization where
        type Rs DescribeEventsForOrganization =
             DescribeEventsForOrganizationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeEventsForOrganizationResponse' Core.<$>
                   (x Core..:? "events") Core.<*> x Core..:? "nextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeEventsForOrganization where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"events" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeEventsForOrganizationResponse' smart constructor.
data DescribeEventsForOrganizationResponse = DescribeEventsForOrganizationResponse'
  { events :: Core.Maybe [Types.OrganizationEvent]
    -- ^ The events that match the specified filter criteria.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeEventsForOrganizationResponse' value with any optional fields omitted.
mkDescribeEventsForOrganizationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeEventsForOrganizationResponse
mkDescribeEventsForOrganizationResponse responseStatus
  = DescribeEventsForOrganizationResponse'{events = Core.Nothing,
                                           nextToken = Core.Nothing, responseStatus}

-- | The events that match the specified filter criteria.
--
-- /Note:/ Consider using 'events' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deforrsEvents :: Lens.Lens' DescribeEventsForOrganizationResponse (Core.Maybe [Types.OrganizationEvent])
deforrsEvents = Lens.field @"events"
{-# INLINEABLE deforrsEvents #-}
{-# DEPRECATED events "Use generic-lens or generic-optics with 'events' instead"  #-}

-- | If the results of a search are large, only a portion of the results are returned, and a @nextToken@ pagination token is returned in the response. To retrieve the next batch of results, reissue the search request and include the returned token. When all results have been returned, the response does not contain a pagination token value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deforrsNextToken :: Lens.Lens' DescribeEventsForOrganizationResponse (Core.Maybe Types.NextToken)
deforrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE deforrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deforrsResponseStatus :: Lens.Lens' DescribeEventsForOrganizationResponse Core.Int
deforrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE deforrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
