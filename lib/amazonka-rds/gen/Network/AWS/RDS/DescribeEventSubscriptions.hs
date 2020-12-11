{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    dSubscriptionName,
    dFilters,
    dMarker,
    dMaxRecords,

    -- * Destructuring the response
    DescribeEventSubscriptionsResponse (..),
    mkDescribeEventSubscriptionsResponse,

    -- ** Response lenses
    desrsEventSubscriptionsList,
    desrsMarker,
    desrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDescribeEventSubscriptions' smart constructor.
data DescribeEventSubscriptions = DescribeEventSubscriptions'
  { subscriptionName ::
      Lude.Maybe Lude.Text,
    filters :: Lude.Maybe [Filter],
    marker :: Lude.Maybe Lude.Text,
    maxRecords :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEventSubscriptions' with the minimum fields required to make a request.
--
-- * 'filters' - This parameter isn't currently supported.
-- * 'marker' - An optional pagination token provided by a previous DescribeOrderableDBInstanceOptions request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
-- * 'subscriptionName' - The name of the RDS event notification subscription you want to describe.
mkDescribeEventSubscriptions ::
  DescribeEventSubscriptions
mkDescribeEventSubscriptions =
  DescribeEventSubscriptions'
    { subscriptionName = Lude.Nothing,
      filters = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing
    }

-- | The name of the RDS event notification subscription you want to describe.
--
-- /Note:/ Consider using 'subscriptionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dSubscriptionName :: Lens.Lens' DescribeEventSubscriptions (Lude.Maybe Lude.Text)
dSubscriptionName = Lens.lens (subscriptionName :: DescribeEventSubscriptions -> Lude.Maybe Lude.Text) (\s a -> s {subscriptionName = a} :: DescribeEventSubscriptions)
{-# DEPRECATED dSubscriptionName "Use generic-lens or generic-optics with 'subscriptionName' instead." #-}

-- | This parameter isn't currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dFilters :: Lens.Lens' DescribeEventSubscriptions (Lude.Maybe [Filter])
dFilters = Lens.lens (filters :: DescribeEventSubscriptions -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeEventSubscriptions)
{-# DEPRECATED dFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | An optional pagination token provided by a previous DescribeOrderableDBInstanceOptions request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMarker :: Lens.Lens' DescribeEventSubscriptions (Lude.Maybe Lude.Text)
dMarker = Lens.lens (marker :: DescribeEventSubscriptions -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeEventSubscriptions)
{-# DEPRECATED dMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMaxRecords :: Lens.Lens' DescribeEventSubscriptions (Lude.Maybe Lude.Int)
dMaxRecords = Lens.lens (maxRecords :: DescribeEventSubscriptions -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeEventSubscriptions)
{-# DEPRECATED dMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Page.AWSPager DescribeEventSubscriptions where
  page rq rs
    | Page.stop (rs Lens.^. desrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. desrsEventSubscriptionsList) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& dMarker Lens..~ rs Lens.^. desrsMarker

instance Lude.AWSRequest DescribeEventSubscriptions where
  type
    Rs DescribeEventSubscriptions =
      DescribeEventSubscriptionsResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "DescribeEventSubscriptionsResult"
      ( \s h x ->
          DescribeEventSubscriptionsResponse'
            Lude.<$> ( x Lude..@? "EventSubscriptionsList" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "EventSubscription")
                     )
            Lude.<*> (x Lude..@? "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeEventSubscriptions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeEventSubscriptions where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeEventSubscriptions where
  toQuery DescribeEventSubscriptions' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeEventSubscriptions" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "SubscriptionName" Lude.=: subscriptionName,
        "Filters"
          Lude.=: Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords
      ]

-- | Data returned by the __DescribeEventSubscriptions__ action.
--
-- /See:/ 'mkDescribeEventSubscriptionsResponse' smart constructor.
data DescribeEventSubscriptionsResponse = DescribeEventSubscriptionsResponse'
  { eventSubscriptionsList ::
      Lude.Maybe
        [EventSubscription],
    marker ::
      Lude.Maybe Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEventSubscriptionsResponse' with the minimum fields required to make a request.
--
-- * 'eventSubscriptionsList' - A list of EventSubscriptions data types.
-- * 'marker' - An optional pagination token provided by a previous DescribeOrderableDBInstanceOptions request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'responseStatus' - The response status code.
mkDescribeEventSubscriptionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeEventSubscriptionsResponse
mkDescribeEventSubscriptionsResponse pResponseStatus_ =
  DescribeEventSubscriptionsResponse'
    { eventSubscriptionsList =
        Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of EventSubscriptions data types.
--
-- /Note:/ Consider using 'eventSubscriptionsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsEventSubscriptionsList :: Lens.Lens' DescribeEventSubscriptionsResponse (Lude.Maybe [EventSubscription])
desrsEventSubscriptionsList = Lens.lens (eventSubscriptionsList :: DescribeEventSubscriptionsResponse -> Lude.Maybe [EventSubscription]) (\s a -> s {eventSubscriptionsList = a} :: DescribeEventSubscriptionsResponse)
{-# DEPRECATED desrsEventSubscriptionsList "Use generic-lens or generic-optics with 'eventSubscriptionsList' instead." #-}

-- | An optional pagination token provided by a previous DescribeOrderableDBInstanceOptions request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsMarker :: Lens.Lens' DescribeEventSubscriptionsResponse (Lude.Maybe Lude.Text)
desrsMarker = Lens.lens (marker :: DescribeEventSubscriptionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeEventSubscriptionsResponse)
{-# DEPRECATED desrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsResponseStatus :: Lens.Lens' DescribeEventSubscriptionsResponse Lude.Int
desrsResponseStatus = Lens.lens (responseStatus :: DescribeEventSubscriptionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeEventSubscriptionsResponse)
{-# DEPRECATED desrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
