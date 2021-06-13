{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeEventSubscriptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists descriptions of all the Amazon Redshift event notification
-- subscriptions for a customer account. If you specify a subscription
-- name, lists the description for that subscription.
--
-- If you specify both tag keys and tag values in the same request, Amazon
-- Redshift returns all event notification subscriptions that match any
-- combination of the specified keys and values. For example, if you have
-- @owner@ and @environment@ for tag keys, and @admin@ and @test@ for tag
-- values, all subscriptions that have any combination of those values are
-- returned.
--
-- If both tag keys and values are omitted from the request, subscriptions
-- are returned regardless of whether they have tag keys or values
-- associated with them.
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeEventSubscriptions
  ( -- * Creating a Request
    DescribeEventSubscriptions (..),
    newDescribeEventSubscriptions,

    -- * Request Lenses
    describeEventSubscriptions_subscriptionName,
    describeEventSubscriptions_tagKeys,
    describeEventSubscriptions_tagValues,
    describeEventSubscriptions_marker,
    describeEventSubscriptions_maxRecords,

    -- * Destructuring the Response
    DescribeEventSubscriptionsResponse (..),
    newDescribeEventSubscriptionsResponse,

    -- * Response Lenses
    describeEventSubscriptionsResponse_eventSubscriptionsList,
    describeEventSubscriptionsResponse_marker,
    describeEventSubscriptionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeEventSubscriptions' smart constructor.
data DescribeEventSubscriptions = DescribeEventSubscriptions'
  { -- | The name of the Amazon Redshift event notification subscription to be
    -- described.
    subscriptionName :: Prelude.Maybe Prelude.Text,
    -- | A tag key or keys for which you want to return all matching event
    -- notification subscriptions that are associated with the specified key or
    -- keys. For example, suppose that you have subscriptions that are tagged
    -- with keys called @owner@ and @environment@. If you specify both of these
    -- tag keys in the request, Amazon Redshift returns a response with the
    -- subscriptions that have either or both of these tag keys associated with
    -- them.
    tagKeys :: Prelude.Maybe [Prelude.Text],
    -- | A tag value or values for which you want to return all matching event
    -- notification subscriptions that are associated with the specified tag
    -- value or values. For example, suppose that you have subscriptions that
    -- are tagged with values called @admin@ and @test@. If you specify both of
    -- these tag values in the request, Amazon Redshift returns a response with
    -- the subscriptions that have either or both of these tag values
    -- associated with them.
    tagValues :: Prelude.Maybe [Prelude.Text],
    -- | An optional parameter that specifies the starting point to return a set
    -- of response records. When the results of a DescribeEventSubscriptions
    -- request exceed the value specified in @MaxRecords@, AWS returns a value
    -- in the @Marker@ field of the response. You can retrieve the next set of
    -- response records by providing the returned marker value in the @Marker@
    -- parameter and retrying the request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of response records to return in each call. If the
    -- number of remaining response records exceeds the specified @MaxRecords@
    -- value, a value is returned in a @marker@ field of the response. You can
    -- retrieve the next set of records by retrying the command with the
    -- returned marker value.
    --
    -- Default: @100@
    --
    -- Constraints: minimum 20, maximum 100.
    maxRecords :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEventSubscriptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subscriptionName', 'describeEventSubscriptions_subscriptionName' - The name of the Amazon Redshift event notification subscription to be
-- described.
--
-- 'tagKeys', 'describeEventSubscriptions_tagKeys' - A tag key or keys for which you want to return all matching event
-- notification subscriptions that are associated with the specified key or
-- keys. For example, suppose that you have subscriptions that are tagged
-- with keys called @owner@ and @environment@. If you specify both of these
-- tag keys in the request, Amazon Redshift returns a response with the
-- subscriptions that have either or both of these tag keys associated with
-- them.
--
-- 'tagValues', 'describeEventSubscriptions_tagValues' - A tag value or values for which you want to return all matching event
-- notification subscriptions that are associated with the specified tag
-- value or values. For example, suppose that you have subscriptions that
-- are tagged with values called @admin@ and @test@. If you specify both of
-- these tag values in the request, Amazon Redshift returns a response with
-- the subscriptions that have either or both of these tag values
-- associated with them.
--
-- 'marker', 'describeEventSubscriptions_marker' - An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeEventSubscriptions
-- request exceed the value specified in @MaxRecords@, AWS returns a value
-- in the @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
--
-- 'maxRecords', 'describeEventSubscriptions_maxRecords' - The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
newDescribeEventSubscriptions ::
  DescribeEventSubscriptions
newDescribeEventSubscriptions =
  DescribeEventSubscriptions'
    { subscriptionName =
        Prelude.Nothing,
      tagKeys = Prelude.Nothing,
      tagValues = Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | The name of the Amazon Redshift event notification subscription to be
-- described.
describeEventSubscriptions_subscriptionName :: Lens.Lens' DescribeEventSubscriptions (Prelude.Maybe Prelude.Text)
describeEventSubscriptions_subscriptionName = Lens.lens (\DescribeEventSubscriptions' {subscriptionName} -> subscriptionName) (\s@DescribeEventSubscriptions' {} a -> s {subscriptionName = a} :: DescribeEventSubscriptions)

-- | A tag key or keys for which you want to return all matching event
-- notification subscriptions that are associated with the specified key or
-- keys. For example, suppose that you have subscriptions that are tagged
-- with keys called @owner@ and @environment@. If you specify both of these
-- tag keys in the request, Amazon Redshift returns a response with the
-- subscriptions that have either or both of these tag keys associated with
-- them.
describeEventSubscriptions_tagKeys :: Lens.Lens' DescribeEventSubscriptions (Prelude.Maybe [Prelude.Text])
describeEventSubscriptions_tagKeys = Lens.lens (\DescribeEventSubscriptions' {tagKeys} -> tagKeys) (\s@DescribeEventSubscriptions' {} a -> s {tagKeys = a} :: DescribeEventSubscriptions) Prelude.. Lens.mapping Lens._Coerce

-- | A tag value or values for which you want to return all matching event
-- notification subscriptions that are associated with the specified tag
-- value or values. For example, suppose that you have subscriptions that
-- are tagged with values called @admin@ and @test@. If you specify both of
-- these tag values in the request, Amazon Redshift returns a response with
-- the subscriptions that have either or both of these tag values
-- associated with them.
describeEventSubscriptions_tagValues :: Lens.Lens' DescribeEventSubscriptions (Prelude.Maybe [Prelude.Text])
describeEventSubscriptions_tagValues = Lens.lens (\DescribeEventSubscriptions' {tagValues} -> tagValues) (\s@DescribeEventSubscriptions' {} a -> s {tagValues = a} :: DescribeEventSubscriptions) Prelude.. Lens.mapping Lens._Coerce

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeEventSubscriptions
-- request exceed the value specified in @MaxRecords@, AWS returns a value
-- in the @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
describeEventSubscriptions_marker :: Lens.Lens' DescribeEventSubscriptions (Prelude.Maybe Prelude.Text)
describeEventSubscriptions_marker = Lens.lens (\DescribeEventSubscriptions' {marker} -> marker) (\s@DescribeEventSubscriptions' {} a -> s {marker = a} :: DescribeEventSubscriptions)

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
describeEventSubscriptions_maxRecords :: Lens.Lens' DescribeEventSubscriptions (Prelude.Maybe Prelude.Int)
describeEventSubscriptions_maxRecords = Lens.lens (\DescribeEventSubscriptions' {maxRecords} -> maxRecords) (\s@DescribeEventSubscriptions' {} a -> s {maxRecords = a} :: DescribeEventSubscriptions)

instance Core.AWSPager DescribeEventSubscriptions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeEventSubscriptionsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeEventSubscriptionsResponse_eventSubscriptionsList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeEventSubscriptions_marker
          Lens..~ rs
          Lens.^? describeEventSubscriptionsResponse_marker
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeEventSubscriptions where
  type
    AWSResponse DescribeEventSubscriptions =
      DescribeEventSubscriptionsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeEventSubscriptionsResult"
      ( \s h x ->
          DescribeEventSubscriptionsResponse'
            Prelude.<$> ( x Core..@? "EventSubscriptionsList"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "EventSubscription")
                        )
            Prelude.<*> (x Core..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeEventSubscriptions

instance Prelude.NFData DescribeEventSubscriptions

instance Core.ToHeaders DescribeEventSubscriptions where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeEventSubscriptions where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeEventSubscriptions where
  toQuery DescribeEventSubscriptions' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeEventSubscriptions" :: Prelude.ByteString),
        "Version"
          Core.=: ("2012-12-01" :: Prelude.ByteString),
        "SubscriptionName" Core.=: subscriptionName,
        "TagKeys"
          Core.=: Core.toQuery
            (Core.toQueryList "TagKey" Prelude.<$> tagKeys),
        "TagValues"
          Core.=: Core.toQuery
            (Core.toQueryList "TagValue" Prelude.<$> tagValues),
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords
      ]

-- |
--
-- /See:/ 'newDescribeEventSubscriptionsResponse' smart constructor.
data DescribeEventSubscriptionsResponse = DescribeEventSubscriptionsResponse'
  { -- | A list of event subscriptions.
    eventSubscriptionsList :: Prelude.Maybe [EventSubscription],
    -- | A value that indicates the starting point for the next set of response
    -- records in a subsequent request. If a value is returned in a response,
    -- you can retrieve the next set of records by providing this returned
    -- marker value in the @Marker@ parameter and retrying the command. If the
    -- @Marker@ field is empty, all response records have been retrieved for
    -- the request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEventSubscriptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventSubscriptionsList', 'describeEventSubscriptionsResponse_eventSubscriptionsList' - A list of event subscriptions.
--
-- 'marker', 'describeEventSubscriptionsResponse_marker' - A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
--
-- 'httpStatus', 'describeEventSubscriptionsResponse_httpStatus' - The response's http status code.
newDescribeEventSubscriptionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeEventSubscriptionsResponse
newDescribeEventSubscriptionsResponse pHttpStatus_ =
  DescribeEventSubscriptionsResponse'
    { eventSubscriptionsList =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of event subscriptions.
describeEventSubscriptionsResponse_eventSubscriptionsList :: Lens.Lens' DescribeEventSubscriptionsResponse (Prelude.Maybe [EventSubscription])
describeEventSubscriptionsResponse_eventSubscriptionsList = Lens.lens (\DescribeEventSubscriptionsResponse' {eventSubscriptionsList} -> eventSubscriptionsList) (\s@DescribeEventSubscriptionsResponse' {} a -> s {eventSubscriptionsList = a} :: DescribeEventSubscriptionsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
describeEventSubscriptionsResponse_marker :: Lens.Lens' DescribeEventSubscriptionsResponse (Prelude.Maybe Prelude.Text)
describeEventSubscriptionsResponse_marker = Lens.lens (\DescribeEventSubscriptionsResponse' {marker} -> marker) (\s@DescribeEventSubscriptionsResponse' {} a -> s {marker = a} :: DescribeEventSubscriptionsResponse)

-- | The response's http status code.
describeEventSubscriptionsResponse_httpStatus :: Lens.Lens' DescribeEventSubscriptionsResponse Prelude.Int
describeEventSubscriptionsResponse_httpStatus = Lens.lens (\DescribeEventSubscriptionsResponse' {httpStatus} -> httpStatus) (\s@DescribeEventSubscriptionsResponse' {} a -> s {httpStatus = a} :: DescribeEventSubscriptionsResponse)

instance
  Prelude.NFData
    DescribeEventSubscriptionsResponse
