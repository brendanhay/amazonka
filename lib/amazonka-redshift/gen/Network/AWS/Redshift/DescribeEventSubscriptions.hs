{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeEventSubscriptions (..),
    mkDescribeEventSubscriptions,

    -- ** Request lenses
    dessSubscriptionName,
    dessTagValues,
    dessTagKeys,
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDescribeEventSubscriptions' smart constructor.
data DescribeEventSubscriptions = DescribeEventSubscriptions'
  { subscriptionName ::
      Lude.Maybe Lude.Text,
    tagValues :: Lude.Maybe [Lude.Text],
    tagKeys :: Lude.Maybe [Lude.Text],
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
-- * 'marker' - An optional parameter that specifies the starting point to return a set of response records. When the results of a DescribeEventSubscriptions request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
-- * 'maxRecords' - The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
--
-- Default: @100@
-- Constraints: minimum 20, maximum 100.
-- * 'subscriptionName' - The name of the Amazon Redshift event notification subscription to be described.
-- * 'tagKeys' - A tag key or keys for which you want to return all matching event notification subscriptions that are associated with the specified key or keys. For example, suppose that you have subscriptions that are tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with the subscriptions that have either or both of these tag keys associated with them.
-- * 'tagValues' - A tag value or values for which you want to return all matching event notification subscriptions that are associated with the specified tag value or values. For example, suppose that you have subscriptions that are tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with the subscriptions that have either or both of these tag values associated with them.
mkDescribeEventSubscriptions ::
  DescribeEventSubscriptions
mkDescribeEventSubscriptions =
  DescribeEventSubscriptions'
    { subscriptionName = Lude.Nothing,
      tagValues = Lude.Nothing,
      tagKeys = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing
    }

-- | The name of the Amazon Redshift event notification subscription to be described.
--
-- /Note:/ Consider using 'subscriptionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dessSubscriptionName :: Lens.Lens' DescribeEventSubscriptions (Lude.Maybe Lude.Text)
dessSubscriptionName = Lens.lens (subscriptionName :: DescribeEventSubscriptions -> Lude.Maybe Lude.Text) (\s a -> s {subscriptionName = a} :: DescribeEventSubscriptions)
{-# DEPRECATED dessSubscriptionName "Use generic-lens or generic-optics with 'subscriptionName' instead." #-}

-- | A tag value or values for which you want to return all matching event notification subscriptions that are associated with the specified tag value or values. For example, suppose that you have subscriptions that are tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with the subscriptions that have either or both of these tag values associated with them.
--
-- /Note:/ Consider using 'tagValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dessTagValues :: Lens.Lens' DescribeEventSubscriptions (Lude.Maybe [Lude.Text])
dessTagValues = Lens.lens (tagValues :: DescribeEventSubscriptions -> Lude.Maybe [Lude.Text]) (\s a -> s {tagValues = a} :: DescribeEventSubscriptions)
{-# DEPRECATED dessTagValues "Use generic-lens or generic-optics with 'tagValues' instead." #-}

-- | A tag key or keys for which you want to return all matching event notification subscriptions that are associated with the specified key or keys. For example, suppose that you have subscriptions that are tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with the subscriptions that have either or both of these tag keys associated with them.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dessTagKeys :: Lens.Lens' DescribeEventSubscriptions (Lude.Maybe [Lude.Text])
dessTagKeys = Lens.lens (tagKeys :: DescribeEventSubscriptions -> Lude.Maybe [Lude.Text]) (\s a -> s {tagKeys = a} :: DescribeEventSubscriptions)
{-# DEPRECATED dessTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a DescribeEventSubscriptions request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dessMarker :: Lens.Lens' DescribeEventSubscriptions (Lude.Maybe Lude.Text)
dessMarker = Lens.lens (marker :: DescribeEventSubscriptions -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeEventSubscriptions)
{-# DEPRECATED dessMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
--
-- Default: @100@
-- Constraints: minimum 20, maximum 100.
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
  request = Req.postQuery redshiftService
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
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "SubscriptionName" Lude.=: subscriptionName,
        "TagValues"
          Lude.=: Lude.toQuery (Lude.toQueryList "TagValue" Lude.<$> tagValues),
        "TagKeys"
          Lude.=: Lude.toQuery (Lude.toQueryList "TagKey" Lude.<$> tagKeys),
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords
      ]

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
-- * 'marker' - A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
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

-- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
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
