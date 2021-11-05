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
-- Module      : Amazonka.RDS.DescribeEventSubscriptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the subscription descriptions for a customer account. The
-- description for a subscription includes @SubscriptionName@,
-- @SNSTopicARN@, @CustomerID@, @SourceType@, @SourceID@, @CreationTime@,
-- and @Status@.
--
-- If you specify a @SubscriptionName@, lists the description for that
-- subscription.
--
-- This operation returns paginated results.
module Amazonka.RDS.DescribeEventSubscriptions
  ( -- * Creating a Request
    DescribeEventSubscriptions (..),
    newDescribeEventSubscriptions,

    -- * Request Lenses
    describeEventSubscriptions_subscriptionName,
    describeEventSubscriptions_filters,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newDescribeEventSubscriptions' smart constructor.
data DescribeEventSubscriptions = DescribeEventSubscriptions'
  { -- | The name of the RDS event notification subscription you want to
    -- describe.
    subscriptionName :: Prelude.Maybe Prelude.Text,
    -- | This parameter isn\'t currently supported.
    filters :: Prelude.Maybe [Filter],
    -- | An optional pagination token provided by a previous
    -- DescribeOrderableDBInstanceOptions request. If this parameter is
    -- specified, the response includes only records beyond the marker, up to
    -- the value specified by @MaxRecords@ .
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so that you can retrieve the
    -- remaining results.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
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
-- 'subscriptionName', 'describeEventSubscriptions_subscriptionName' - The name of the RDS event notification subscription you want to
-- describe.
--
-- 'filters', 'describeEventSubscriptions_filters' - This parameter isn\'t currently supported.
--
-- 'marker', 'describeEventSubscriptions_marker' - An optional pagination token provided by a previous
-- DescribeOrderableDBInstanceOptions request. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@ .
--
-- 'maxRecords', 'describeEventSubscriptions_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
newDescribeEventSubscriptions ::
  DescribeEventSubscriptions
newDescribeEventSubscriptions =
  DescribeEventSubscriptions'
    { subscriptionName =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | The name of the RDS event notification subscription you want to
-- describe.
describeEventSubscriptions_subscriptionName :: Lens.Lens' DescribeEventSubscriptions (Prelude.Maybe Prelude.Text)
describeEventSubscriptions_subscriptionName = Lens.lens (\DescribeEventSubscriptions' {subscriptionName} -> subscriptionName) (\s@DescribeEventSubscriptions' {} a -> s {subscriptionName = a} :: DescribeEventSubscriptions)

-- | This parameter isn\'t currently supported.
describeEventSubscriptions_filters :: Lens.Lens' DescribeEventSubscriptions (Prelude.Maybe [Filter])
describeEventSubscriptions_filters = Lens.lens (\DescribeEventSubscriptions' {filters} -> filters) (\s@DescribeEventSubscriptions' {} a -> s {filters = a} :: DescribeEventSubscriptions) Prelude.. Lens.mapping Lens.coerced

-- | An optional pagination token provided by a previous
-- DescribeOrderableDBInstanceOptions request. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@ .
describeEventSubscriptions_marker :: Lens.Lens' DescribeEventSubscriptions (Prelude.Maybe Prelude.Text)
describeEventSubscriptions_marker = Lens.lens (\DescribeEventSubscriptions' {marker} -> marker) (\s@DescribeEventSubscriptions' {} a -> s {marker = a} :: DescribeEventSubscriptions)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
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
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "SubscriptionName" Core.=: subscriptionName,
        "Filters"
          Core.=: Core.toQuery
            (Core.toQueryList "Filter" Prelude.<$> filters),
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords
      ]

-- | Data returned by the __DescribeEventSubscriptions__ action.
--
-- /See:/ 'newDescribeEventSubscriptionsResponse' smart constructor.
data DescribeEventSubscriptionsResponse = DescribeEventSubscriptionsResponse'
  { -- | A list of EventSubscriptions data types.
    eventSubscriptionsList :: Prelude.Maybe [EventSubscription],
    -- | An optional pagination token provided by a previous
    -- DescribeOrderableDBInstanceOptions request. If this parameter is
    -- specified, the response includes only records beyond the marker, up to
    -- the value specified by @MaxRecords@.
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
-- 'eventSubscriptionsList', 'describeEventSubscriptionsResponse_eventSubscriptionsList' - A list of EventSubscriptions data types.
--
-- 'marker', 'describeEventSubscriptionsResponse_marker' - An optional pagination token provided by a previous
-- DescribeOrderableDBInstanceOptions request. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
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

-- | A list of EventSubscriptions data types.
describeEventSubscriptionsResponse_eventSubscriptionsList :: Lens.Lens' DescribeEventSubscriptionsResponse (Prelude.Maybe [EventSubscription])
describeEventSubscriptionsResponse_eventSubscriptionsList = Lens.lens (\DescribeEventSubscriptionsResponse' {eventSubscriptionsList} -> eventSubscriptionsList) (\s@DescribeEventSubscriptionsResponse' {} a -> s {eventSubscriptionsList = a} :: DescribeEventSubscriptionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | An optional pagination token provided by a previous
-- DescribeOrderableDBInstanceOptions request. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
describeEventSubscriptionsResponse_marker :: Lens.Lens' DescribeEventSubscriptionsResponse (Prelude.Maybe Prelude.Text)
describeEventSubscriptionsResponse_marker = Lens.lens (\DescribeEventSubscriptionsResponse' {marker} -> marker) (\s@DescribeEventSubscriptionsResponse' {} a -> s {marker = a} :: DescribeEventSubscriptionsResponse)

-- | The response's http status code.
describeEventSubscriptionsResponse_httpStatus :: Lens.Lens' DescribeEventSubscriptionsResponse Prelude.Int
describeEventSubscriptionsResponse_httpStatus = Lens.lens (\DescribeEventSubscriptionsResponse' {httpStatus} -> httpStatus) (\s@DescribeEventSubscriptionsResponse' {} a -> s {httpStatus = a} :: DescribeEventSubscriptionsResponse)

instance
  Prelude.NFData
    DescribeEventSubscriptionsResponse
