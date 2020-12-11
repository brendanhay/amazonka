{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DescribeEventSubscriptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the event subscriptions for a customer account. The description of a subscription includes @SubscriptionName@ , @SNSTopicARN@ , @CustomerID@ , @SourceType@ , @SourceID@ , @CreationTime@ , and @Status@ .
--
-- If you specify @SubscriptionName@ , this action lists the description for that subscription.
--
-- This operation returns paginated results.
module Network.AWS.DMS.DescribeEventSubscriptions
  ( -- * Creating a request
    DescribeEventSubscriptions (..),
    mkDescribeEventSubscriptions,

    -- ** Request lenses
    dessSubscriptionName,
    dessFilters,
    dessMarker,
    dessMaxRecords,

    -- * Destructuring the response
    DescribeEventSubscriptionsResponse (..),
    mkDescribeEventSubscriptionsResponse,

    -- ** Response lenses
    desrsEventSubscriptionsList,
    desrsMarker,
    desrsResponseStatus,
  )
where

import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
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
-- * 'filters' - Filters applied to event subscriptions.
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
-- * 'subscriptionName' - The name of the AWS DMS event subscription to be described.
mkDescribeEventSubscriptions ::
  DescribeEventSubscriptions
mkDescribeEventSubscriptions =
  DescribeEventSubscriptions'
    { subscriptionName = Lude.Nothing,
      filters = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing
    }

-- | The name of the AWS DMS event subscription to be described.
--
-- /Note:/ Consider using 'subscriptionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dessSubscriptionName :: Lens.Lens' DescribeEventSubscriptions (Lude.Maybe Lude.Text)
dessSubscriptionName = Lens.lens (subscriptionName :: DescribeEventSubscriptions -> Lude.Maybe Lude.Text) (\s a -> s {subscriptionName = a} :: DescribeEventSubscriptions)
{-# DEPRECATED dessSubscriptionName "Use generic-lens or generic-optics with 'subscriptionName' instead." #-}

-- | Filters applied to event subscriptions.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dessFilters :: Lens.Lens' DescribeEventSubscriptions (Lude.Maybe [Filter])
dessFilters = Lens.lens (filters :: DescribeEventSubscriptions -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeEventSubscriptions)
{-# DEPRECATED dessFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dessMarker :: Lens.Lens' DescribeEventSubscriptions (Lude.Maybe Lude.Text)
dessMarker = Lens.lens (marker :: DescribeEventSubscriptions -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeEventSubscriptions)
{-# DEPRECATED dessMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dessMaxRecords :: Lens.Lens' DescribeEventSubscriptions (Lude.Maybe Lude.Int)
dessMaxRecords = Lens.lens (maxRecords :: DescribeEventSubscriptions -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeEventSubscriptions)
{-# DEPRECATED dessMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Page.AWSPager DescribeEventSubscriptions where
  page rq rs
    | Page.stop (rs Lens.^. desrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. desrsEventSubscriptionsList) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dessMarker Lens..~ rs Lens.^. desrsMarker

instance Lude.AWSRequest DescribeEventSubscriptions where
  type
    Rs DescribeEventSubscriptions =
      DescribeEventSubscriptionsResponse
  request = Req.postJSON dmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeEventSubscriptionsResponse'
            Lude.<$> (x Lude..?> "EventSubscriptionsList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeEventSubscriptions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonDMSv20160101.DescribeEventSubscriptions" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeEventSubscriptions where
  toJSON DescribeEventSubscriptions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SubscriptionName" Lude..=) Lude.<$> subscriptionName,
            ("Filters" Lude..=) Lude.<$> filters,
            ("Marker" Lude..=) Lude.<$> marker,
            ("MaxRecords" Lude..=) Lude.<$> maxRecords
          ]
      )

instance Lude.ToPath DescribeEventSubscriptions where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeEventSubscriptions where
  toQuery = Lude.const Lude.mempty

-- |
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
-- * 'eventSubscriptionsList' - A list of event subscriptions.
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
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

-- | A list of event subscriptions.
--
-- /Note:/ Consider using 'eventSubscriptionsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsEventSubscriptionsList :: Lens.Lens' DescribeEventSubscriptionsResponse (Lude.Maybe [EventSubscription])
desrsEventSubscriptionsList = Lens.lens (eventSubscriptionsList :: DescribeEventSubscriptionsResponse -> Lude.Maybe [EventSubscription]) (\s a -> s {eventSubscriptionsList = a} :: DescribeEventSubscriptionsResponse)
{-# DEPRECATED desrsEventSubscriptionsList "Use generic-lens or generic-optics with 'eventSubscriptionsList' instead." #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
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
