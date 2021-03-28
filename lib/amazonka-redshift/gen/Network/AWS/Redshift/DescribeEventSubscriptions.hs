{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeEventSubscriptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists descriptions of all the Amazon Redshift event notification subscriptions for a customer account. If you specify a subscription name, lists the description for that subscription.
--
-- If you specify both tag keys and tag values in the same request, Amazon Redshift returns all event notification subscriptions that match any combination of the specified keys and values. For example, if you have @owner@ and @environment@ for tag keys, and @admin@ and @test@ for tag values, all subscriptions that have any combination of those values are returned.
-- If both tag keys and values are omitted from the request, subscriptions are returned regardless of whether they have tag keys or values associated with them.
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeEventSubscriptions
    (
    -- * Creating a request
      DescribeEventSubscriptions (..)
    , mkDescribeEventSubscriptions
    -- ** Request lenses
    , dessMarker
    , dessMaxRecords
    , dessSubscriptionName
    , dessTagKeys
    , dessTagValues

    -- * Destructuring the response
    , DescribeEventSubscriptionsResponse (..)
    , mkDescribeEventSubscriptionsResponse
    -- ** Response lenses
    , desrrsEventSubscriptionsList
    , desrrsMarker
    , desrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDescribeEventSubscriptions' smart constructor.
data DescribeEventSubscriptions = DescribeEventSubscriptions'
  { marker :: Core.Maybe Core.Text
    -- ^ An optional parameter that specifies the starting point to return a set of response records. When the results of a DescribeEventSubscriptions request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request. 
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value. 
--
-- Default: @100@ 
-- Constraints: minimum 20, maximum 100.
  , subscriptionName :: Core.Maybe Core.Text
    -- ^ The name of the Amazon Redshift event notification subscription to be described.
  , tagKeys :: Core.Maybe [Core.Text]
    -- ^ A tag key or keys for which you want to return all matching event notification subscriptions that are associated with the specified key or keys. For example, suppose that you have subscriptions that are tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with the subscriptions that have either or both of these tag keys associated with them.
  , tagValues :: Core.Maybe [Core.Text]
    -- ^ A tag value or values for which you want to return all matching event notification subscriptions that are associated with the specified tag value or values. For example, suppose that you have subscriptions that are tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with the subscriptions that have either or both of these tag values associated with them.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEventSubscriptions' value with any optional fields omitted.
mkDescribeEventSubscriptions
    :: DescribeEventSubscriptions
mkDescribeEventSubscriptions
  = DescribeEventSubscriptions'{marker = Core.Nothing,
                                maxRecords = Core.Nothing, subscriptionName = Core.Nothing,
                                tagKeys = Core.Nothing, tagValues = Core.Nothing}

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a DescribeEventSubscriptions request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request. 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dessMarker :: Lens.Lens' DescribeEventSubscriptions (Core.Maybe Core.Text)
dessMarker = Lens.field @"marker"
{-# INLINEABLE dessMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value. 
--
-- Default: @100@ 
-- Constraints: minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dessMaxRecords :: Lens.Lens' DescribeEventSubscriptions (Core.Maybe Core.Int)
dessMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE dessMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

-- | The name of the Amazon Redshift event notification subscription to be described.
--
-- /Note:/ Consider using 'subscriptionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dessSubscriptionName :: Lens.Lens' DescribeEventSubscriptions (Core.Maybe Core.Text)
dessSubscriptionName = Lens.field @"subscriptionName"
{-# INLINEABLE dessSubscriptionName #-}
{-# DEPRECATED subscriptionName "Use generic-lens or generic-optics with 'subscriptionName' instead"  #-}

-- | A tag key or keys for which you want to return all matching event notification subscriptions that are associated with the specified key or keys. For example, suppose that you have subscriptions that are tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with the subscriptions that have either or both of these tag keys associated with them.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dessTagKeys :: Lens.Lens' DescribeEventSubscriptions (Core.Maybe [Core.Text])
dessTagKeys = Lens.field @"tagKeys"
{-# INLINEABLE dessTagKeys #-}
{-# DEPRECATED tagKeys "Use generic-lens or generic-optics with 'tagKeys' instead"  #-}

-- | A tag value or values for which you want to return all matching event notification subscriptions that are associated with the specified tag value or values. For example, suppose that you have subscriptions that are tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with the subscriptions that have either or both of these tag values associated with them.
--
-- /Note:/ Consider using 'tagValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dessTagValues :: Lens.Lens' DescribeEventSubscriptions (Core.Maybe [Core.Text])
dessTagValues = Lens.field @"tagValues"
{-# INLINEABLE dessTagValues #-}
{-# DEPRECATED tagValues "Use generic-lens or generic-optics with 'tagValues' instead"  #-}

instance Core.ToQuery DescribeEventSubscriptions where
        toQuery DescribeEventSubscriptions{..}
          = Core.toQueryPair "Action"
              ("DescribeEventSubscriptions" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-12-01" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxRecords") maxRecords
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SubscriptionName")
                subscriptionName
              Core.<>
              Core.toQueryPair "TagKeys"
                (Core.maybe Core.mempty (Core.toQueryList "TagKey") tagKeys)
              Core.<>
              Core.toQueryPair "TagValues"
                (Core.maybe Core.mempty (Core.toQueryList "TagValue") tagValues)

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

-- | 
--
-- /See:/ 'mkDescribeEventSubscriptionsResponse' smart constructor.
data DescribeEventSubscriptionsResponse = DescribeEventSubscriptionsResponse'
  { eventSubscriptionsList :: Core.Maybe [Types.EventSubscription]
    -- ^ A list of event subscriptions.
  , marker :: Core.Maybe Core.Text
    -- ^ A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeEventSubscriptionsResponse' value with any optional fields omitted.
mkDescribeEventSubscriptionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeEventSubscriptionsResponse
mkDescribeEventSubscriptionsResponse responseStatus
  = DescribeEventSubscriptionsResponse'{eventSubscriptionsList =
                                          Core.Nothing,
                                        marker = Core.Nothing, responseStatus}

-- | A list of event subscriptions.
--
-- /Note:/ Consider using 'eventSubscriptionsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrrsEventSubscriptionsList :: Lens.Lens' DescribeEventSubscriptionsResponse (Core.Maybe [Types.EventSubscription])
desrrsEventSubscriptionsList = Lens.field @"eventSubscriptionsList"
{-# INLINEABLE desrrsEventSubscriptionsList #-}
{-# DEPRECATED eventSubscriptionsList "Use generic-lens or generic-optics with 'eventSubscriptionsList' instead"  #-}

-- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request. 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrrsMarker :: Lens.Lens' DescribeEventSubscriptionsResponse (Core.Maybe Core.Text)
desrrsMarker = Lens.field @"marker"
{-# INLINEABLE desrrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrrsResponseStatus :: Lens.Lens' DescribeEventSubscriptionsResponse Core.Int
desrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE desrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
