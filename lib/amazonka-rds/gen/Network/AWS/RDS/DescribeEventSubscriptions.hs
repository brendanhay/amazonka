{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeEventSubscriptions (..),
    mkDescribeEventSubscriptions,

    -- ** Request lenses
    dessFilters,
    dessMarker,
    dessMaxRecords,
    dessSubscriptionName,

    -- * Destructuring the response
    DescribeEventSubscriptionsResponse (..),
    mkDescribeEventSubscriptionsResponse,

    -- ** Response lenses
    desrfrsEventSubscriptionsList,
    desrfrsMarker,
    desrfrsResponseStatus,
  )
where

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
  { -- | This parameter isn't currently supported.
    filters :: Core.Maybe [Types.Filter],
    -- | An optional pagination token provided by a previous DescribeOrderableDBInstanceOptions request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Core.Maybe Types.Marker,
    -- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
    --
    -- Default: 100
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Int,
    -- | The name of the RDS event notification subscription you want to describe.
    subscriptionName :: Core.Maybe Types.SubscriptionName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEventSubscriptions' value with any optional fields omitted.
mkDescribeEventSubscriptions ::
  DescribeEventSubscriptions
mkDescribeEventSubscriptions =
  DescribeEventSubscriptions'
    { filters = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing,
      subscriptionName = Core.Nothing
    }

-- | This parameter isn't currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dessFilters :: Lens.Lens' DescribeEventSubscriptions (Core.Maybe [Types.Filter])
dessFilters = Lens.field @"filters"
{-# DEPRECATED dessFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | An optional pagination token provided by a previous DescribeOrderableDBInstanceOptions request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dessMarker :: Lens.Lens' DescribeEventSubscriptions (Core.Maybe Types.Marker)
dessMarker = Lens.field @"marker"
{-# DEPRECATED dessMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dessMaxRecords :: Lens.Lens' DescribeEventSubscriptions (Core.Maybe Core.Int)
dessMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED dessMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The name of the RDS event notification subscription you want to describe.
--
-- /Note:/ Consider using 'subscriptionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dessSubscriptionName :: Lens.Lens' DescribeEventSubscriptions (Core.Maybe Types.SubscriptionName)
dessSubscriptionName = Lens.field @"subscriptionName"
{-# DEPRECATED dessSubscriptionName "Use generic-lens or generic-optics with 'subscriptionName' instead." #-}

instance Core.AWSRequest DescribeEventSubscriptions where
  type
    Rs DescribeEventSubscriptions =
      DescribeEventSubscriptionsResponse
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
            ( Core.pure ("Action", "DescribeEventSubscriptions")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> ( Core.toQueryValue
                            "Filters"
                            (Core.toQueryList "Filter" Core.<$> filters)
                        )
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> (Core.toQueryValue "MaxRecords" Core.<$> maxRecords)
                Core.<> (Core.toQueryValue "SubscriptionName" Core.<$> subscriptionName)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeEventSubscriptionsResult"
      ( \s h x ->
          DescribeEventSubscriptionsResponse'
            Core.<$> ( x Core..@? "EventSubscriptionsList"
                         Core..<@> Core.parseXMLList "EventSubscription"
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeEventSubscriptions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"eventSubscriptionsList" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | Data returned by the __DescribeEventSubscriptions__ action.
--
-- /See:/ 'mkDescribeEventSubscriptionsResponse' smart constructor.
data DescribeEventSubscriptionsResponse = DescribeEventSubscriptionsResponse'
  { -- | A list of EventSubscriptions data types.
    eventSubscriptionsList :: Core.Maybe [Types.EventSubscription],
    -- | An optional pagination token provided by a previous DescribeOrderableDBInstanceOptions request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Core.Maybe Types.Marker,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEventSubscriptionsResponse' value with any optional fields omitted.
mkDescribeEventSubscriptionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeEventSubscriptionsResponse
mkDescribeEventSubscriptionsResponse responseStatus =
  DescribeEventSubscriptionsResponse'
    { eventSubscriptionsList =
        Core.Nothing,
      marker = Core.Nothing,
      responseStatus
    }

-- | A list of EventSubscriptions data types.
--
-- /Note:/ Consider using 'eventSubscriptionsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrfrsEventSubscriptionsList :: Lens.Lens' DescribeEventSubscriptionsResponse (Core.Maybe [Types.EventSubscription])
desrfrsEventSubscriptionsList = Lens.field @"eventSubscriptionsList"
{-# DEPRECATED desrfrsEventSubscriptionsList "Use generic-lens or generic-optics with 'eventSubscriptionsList' instead." #-}

-- | An optional pagination token provided by a previous DescribeOrderableDBInstanceOptions request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrfrsMarker :: Lens.Lens' DescribeEventSubscriptionsResponse (Core.Maybe Types.Marker)
desrfrsMarker = Lens.field @"marker"
{-# DEPRECATED desrfrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrfrsResponseStatus :: Lens.Lens' DescribeEventSubscriptionsResponse Core.Int
desrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED desrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
