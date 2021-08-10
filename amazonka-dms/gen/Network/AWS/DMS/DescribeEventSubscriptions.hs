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
-- Module      : Network.AWS.DMS.DescribeEventSubscriptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the event subscriptions for a customer account. The
-- description of a subscription includes @SubscriptionName@,
-- @SNSTopicARN@, @CustomerID@, @SourceType@, @SourceID@, @CreationTime@,
-- and @Status@.
--
-- If you specify @SubscriptionName@, this action lists the description for
-- that subscription.
--
-- This operation returns paginated results.
module Network.AWS.DMS.DescribeEventSubscriptions
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

import qualified Network.AWS.Core as Core
import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeEventSubscriptions' smart constructor.
data DescribeEventSubscriptions = DescribeEventSubscriptions'
  { -- | The name of the AWS DMS event subscription to be described.
    subscriptionName :: Prelude.Maybe Prelude.Text,
    -- | Filters applied to event subscriptions.
    filters :: Prelude.Maybe [Filter],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so that the remaining
    -- results can be retrieved.
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
-- 'subscriptionName', 'describeEventSubscriptions_subscriptionName' - The name of the AWS DMS event subscription to be described.
--
-- 'filters', 'describeEventSubscriptions_filters' - Filters applied to event subscriptions.
--
-- 'marker', 'describeEventSubscriptions_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'maxRecords', 'describeEventSubscriptions_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
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

-- | The name of the AWS DMS event subscription to be described.
describeEventSubscriptions_subscriptionName :: Lens.Lens' DescribeEventSubscriptions (Prelude.Maybe Prelude.Text)
describeEventSubscriptions_subscriptionName = Lens.lens (\DescribeEventSubscriptions' {subscriptionName} -> subscriptionName) (\s@DescribeEventSubscriptions' {} a -> s {subscriptionName = a} :: DescribeEventSubscriptions)

-- | Filters applied to event subscriptions.
describeEventSubscriptions_filters :: Lens.Lens' DescribeEventSubscriptions (Prelude.Maybe [Filter])
describeEventSubscriptions_filters = Lens.lens (\DescribeEventSubscriptions' {filters} -> filters) (\s@DescribeEventSubscriptions' {} a -> s {filters = a} :: DescribeEventSubscriptions) Prelude.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeEventSubscriptions_marker :: Lens.Lens' DescribeEventSubscriptions (Prelude.Maybe Prelude.Text)
describeEventSubscriptions_marker = Lens.lens (\DescribeEventSubscriptions' {marker} -> marker) (\s@DescribeEventSubscriptions' {} a -> s {marker = a} :: DescribeEventSubscriptions)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEventSubscriptionsResponse'
            Prelude.<$> ( x Core..?> "EventSubscriptionsList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeEventSubscriptions

instance Prelude.NFData DescribeEventSubscriptions

instance Core.ToHeaders DescribeEventSubscriptions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.DescribeEventSubscriptions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeEventSubscriptions where
  toJSON DescribeEventSubscriptions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SubscriptionName" Core..=)
              Prelude.<$> subscriptionName,
            ("Filters" Core..=) Prelude.<$> filters,
            ("Marker" Core..=) Prelude.<$> marker,
            ("MaxRecords" Core..=) Prelude.<$> maxRecords
          ]
      )

instance Core.ToPath DescribeEventSubscriptions where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeEventSubscriptions where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newDescribeEventSubscriptionsResponse' smart constructor.
data DescribeEventSubscriptionsResponse = DescribeEventSubscriptionsResponse'
  { -- | A list of event subscriptions.
    eventSubscriptionsList :: Prelude.Maybe [EventSubscription],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
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
-- 'marker', 'describeEventSubscriptionsResponse_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
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

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeEventSubscriptionsResponse_marker :: Lens.Lens' DescribeEventSubscriptionsResponse (Prelude.Maybe Prelude.Text)
describeEventSubscriptionsResponse_marker = Lens.lens (\DescribeEventSubscriptionsResponse' {marker} -> marker) (\s@DescribeEventSubscriptionsResponse' {} a -> s {marker = a} :: DescribeEventSubscriptionsResponse)

-- | The response's http status code.
describeEventSubscriptionsResponse_httpStatus :: Lens.Lens' DescribeEventSubscriptionsResponse Prelude.Int
describeEventSubscriptionsResponse_httpStatus = Lens.lens (\DescribeEventSubscriptionsResponse' {httpStatus} -> httpStatus) (\s@DescribeEventSubscriptionsResponse' {} a -> s {httpStatus = a} :: DescribeEventSubscriptionsResponse)

instance
  Prelude.NFData
    DescribeEventSubscriptionsResponse
