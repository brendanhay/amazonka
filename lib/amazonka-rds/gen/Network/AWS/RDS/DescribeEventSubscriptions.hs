{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeEventSubscriptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the subscription descriptions for a customer account. The description for a subscription includes @SubscriptionName@ , @SNSTopicARN@ , @CustomerID@ , @SourceType@ , @SourceID@ , @CreationTime@ , and @Status@ .
--
-- If you specify a @SubscriptionName@ , lists the description for that subscription.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeEventSubscriptions
    (
    -- * Creating a request
      DescribeEventSubscriptions (..)
    , mkDescribeEventSubscriptions
    -- ** Request lenses
    , dessFilters
    , dessMarker
    , dessMaxRecords
    , dessSubscriptionName

    -- * Destructuring the response
    , DescribeEventSubscriptionsResponse (..)
    , mkDescribeEventSubscriptionsResponse
    -- ** Response lenses
    , desrfrsEventSubscriptionsList
    , desrfrsMarker
    , desrfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDescribeEventSubscriptions' smart constructor.
data DescribeEventSubscriptions = DescribeEventSubscriptions'
  { filters :: Core.Maybe [Types.Filter]
    -- ^ This parameter isn't currently supported.
  , marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous DescribeOrderableDBInstanceOptions request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
  , subscriptionName :: Core.Maybe Core.Text
    -- ^ The name of the RDS event notification subscription you want to describe.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEventSubscriptions' value with any optional fields omitted.
mkDescribeEventSubscriptions
    :: DescribeEventSubscriptions
mkDescribeEventSubscriptions
  = DescribeEventSubscriptions'{filters = Core.Nothing,
                                marker = Core.Nothing, maxRecords = Core.Nothing,
                                subscriptionName = Core.Nothing}

-- | This parameter isn't currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dessFilters :: Lens.Lens' DescribeEventSubscriptions (Core.Maybe [Types.Filter])
dessFilters = Lens.field @"filters"
{-# INLINEABLE dessFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | An optional pagination token provided by a previous DescribeOrderableDBInstanceOptions request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dessMarker :: Lens.Lens' DescribeEventSubscriptions (Core.Maybe Core.Text)
dessMarker = Lens.field @"marker"
{-# INLINEABLE dessMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dessMaxRecords :: Lens.Lens' DescribeEventSubscriptions (Core.Maybe Core.Int)
dessMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE dessMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

-- | The name of the RDS event notification subscription you want to describe.
--
-- /Note:/ Consider using 'subscriptionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dessSubscriptionName :: Lens.Lens' DescribeEventSubscriptions (Core.Maybe Core.Text)
dessSubscriptionName = Lens.field @"subscriptionName"
{-# INLINEABLE dessSubscriptionName #-}
{-# DEPRECATED subscriptionName "Use generic-lens or generic-optics with 'subscriptionName' instead"  #-}

instance Core.ToQuery DescribeEventSubscriptions where
        toQuery DescribeEventSubscriptions{..}
          = Core.toQueryPair "Action"
              ("DescribeEventSubscriptions" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.toQueryPair "Filters"
                (Core.maybe Core.mempty (Core.toQueryList "Filter") filters)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxRecords") maxRecords
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SubscriptionName")
                subscriptionName

instance Core.ToHeaders DescribeEventSubscriptions where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeEventSubscriptions where
        type Rs DescribeEventSubscriptions =
             DescribeEventSubscriptionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "DescribeEventSubscriptionsResult"
              (\ s h x ->
                 DescribeEventSubscriptionsResponse' Core.<$>
                   (x Core..@? "EventSubscriptionsList" Core..<@>
                      Core.parseXMLList "EventSubscription")
                     Core.<*> x Core..@? "Marker"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeEventSubscriptions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"eventSubscriptionsList" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | Data returned by the __DescribeEventSubscriptions__ action.
--
-- /See:/ 'mkDescribeEventSubscriptionsResponse' smart constructor.
data DescribeEventSubscriptionsResponse = DescribeEventSubscriptionsResponse'
  { eventSubscriptionsList :: Core.Maybe [Types.EventSubscription]
    -- ^ A list of EventSubscriptions data types.
  , marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous DescribeOrderableDBInstanceOptions request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEventSubscriptionsResponse' value with any optional fields omitted.
mkDescribeEventSubscriptionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeEventSubscriptionsResponse
mkDescribeEventSubscriptionsResponse responseStatus
  = DescribeEventSubscriptionsResponse'{eventSubscriptionsList =
                                          Core.Nothing,
                                        marker = Core.Nothing, responseStatus}

-- | A list of EventSubscriptions data types.
--
-- /Note:/ Consider using 'eventSubscriptionsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrfrsEventSubscriptionsList :: Lens.Lens' DescribeEventSubscriptionsResponse (Core.Maybe [Types.EventSubscription])
desrfrsEventSubscriptionsList = Lens.field @"eventSubscriptionsList"
{-# INLINEABLE desrfrsEventSubscriptionsList #-}
{-# DEPRECATED eventSubscriptionsList "Use generic-lens or generic-optics with 'eventSubscriptionsList' instead"  #-}

-- | An optional pagination token provided by a previous DescribeOrderableDBInstanceOptions request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrfrsMarker :: Lens.Lens' DescribeEventSubscriptionsResponse (Core.Maybe Core.Text)
desrfrsMarker = Lens.field @"marker"
{-# INLINEABLE desrfrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrfrsResponseStatus :: Lens.Lens' DescribeEventSubscriptionsResponse Core.Int
desrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE desrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
