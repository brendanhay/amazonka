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
-- Module      : Network.AWS.WorkDocs.DescribeNotificationSubscriptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the specified notification subscriptions.
--
-- This operation returns paginated results.
module Network.AWS.WorkDocs.DescribeNotificationSubscriptions
  ( -- * Creating a Request
    DescribeNotificationSubscriptions (..),
    newDescribeNotificationSubscriptions,

    -- * Request Lenses
    describeNotificationSubscriptions_limit,
    describeNotificationSubscriptions_marker,
    describeNotificationSubscriptions_organizationId,

    -- * Destructuring the Response
    DescribeNotificationSubscriptionsResponse (..),
    newDescribeNotificationSubscriptionsResponse,

    -- * Response Lenses
    describeNotificationSubscriptionsResponse_subscriptions,
    describeNotificationSubscriptionsResponse_marker,
    describeNotificationSubscriptionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkDocs.Types

-- | /See:/ 'newDescribeNotificationSubscriptions' smart constructor.
data DescribeNotificationSubscriptions = DescribeNotificationSubscriptions'
  { -- | The maximum number of items to return with this call.
    limit :: Core.Maybe Core.Natural,
    -- | The marker for the next set of results. (You received this marker from a
    -- previous call.)
    marker :: Core.Maybe Core.Text,
    -- | The ID of the organization.
    organizationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeNotificationSubscriptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'describeNotificationSubscriptions_limit' - The maximum number of items to return with this call.
--
-- 'marker', 'describeNotificationSubscriptions_marker' - The marker for the next set of results. (You received this marker from a
-- previous call.)
--
-- 'organizationId', 'describeNotificationSubscriptions_organizationId' - The ID of the organization.
newDescribeNotificationSubscriptions ::
  -- | 'organizationId'
  Core.Text ->
  DescribeNotificationSubscriptions
newDescribeNotificationSubscriptions pOrganizationId_ =
  DescribeNotificationSubscriptions'
    { limit =
        Core.Nothing,
      marker = Core.Nothing,
      organizationId = pOrganizationId_
    }

-- | The maximum number of items to return with this call.
describeNotificationSubscriptions_limit :: Lens.Lens' DescribeNotificationSubscriptions (Core.Maybe Core.Natural)
describeNotificationSubscriptions_limit = Lens.lens (\DescribeNotificationSubscriptions' {limit} -> limit) (\s@DescribeNotificationSubscriptions' {} a -> s {limit = a} :: DescribeNotificationSubscriptions)

-- | The marker for the next set of results. (You received this marker from a
-- previous call.)
describeNotificationSubscriptions_marker :: Lens.Lens' DescribeNotificationSubscriptions (Core.Maybe Core.Text)
describeNotificationSubscriptions_marker = Lens.lens (\DescribeNotificationSubscriptions' {marker} -> marker) (\s@DescribeNotificationSubscriptions' {} a -> s {marker = a} :: DescribeNotificationSubscriptions)

-- | The ID of the organization.
describeNotificationSubscriptions_organizationId :: Lens.Lens' DescribeNotificationSubscriptions Core.Text
describeNotificationSubscriptions_organizationId = Lens.lens (\DescribeNotificationSubscriptions' {organizationId} -> organizationId) (\s@DescribeNotificationSubscriptions' {} a -> s {organizationId = a} :: DescribeNotificationSubscriptions)

instance
  Core.AWSPager
    DescribeNotificationSubscriptions
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeNotificationSubscriptionsResponse_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeNotificationSubscriptionsResponse_subscriptions
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeNotificationSubscriptions_marker
          Lens..~ rs
          Lens.^? describeNotificationSubscriptionsResponse_marker
            Core.. Lens._Just

instance
  Core.AWSRequest
    DescribeNotificationSubscriptions
  where
  type
    AWSResponse DescribeNotificationSubscriptions =
      DescribeNotificationSubscriptionsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeNotificationSubscriptionsResponse'
            Core.<$> (x Core..?> "Subscriptions" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeNotificationSubscriptions

instance
  Core.NFData
    DescribeNotificationSubscriptions

instance
  Core.ToHeaders
    DescribeNotificationSubscriptions
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToPath
    DescribeNotificationSubscriptions
  where
  toPath DescribeNotificationSubscriptions' {..} =
    Core.mconcat
      [ "/api/v1/organizations/",
        Core.toBS organizationId,
        "/subscriptions"
      ]

instance
  Core.ToQuery
    DescribeNotificationSubscriptions
  where
  toQuery DescribeNotificationSubscriptions' {..} =
    Core.mconcat
      ["limit" Core.=: limit, "marker" Core.=: marker]

-- | /See:/ 'newDescribeNotificationSubscriptionsResponse' smart constructor.
data DescribeNotificationSubscriptionsResponse = DescribeNotificationSubscriptionsResponse'
  { -- | The subscriptions.
    subscriptions :: Core.Maybe [Subscription],
    -- | The marker to use when requesting the next set of results. If there are
    -- no additional results, the string is empty.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeNotificationSubscriptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subscriptions', 'describeNotificationSubscriptionsResponse_subscriptions' - The subscriptions.
--
-- 'marker', 'describeNotificationSubscriptionsResponse_marker' - The marker to use when requesting the next set of results. If there are
-- no additional results, the string is empty.
--
-- 'httpStatus', 'describeNotificationSubscriptionsResponse_httpStatus' - The response's http status code.
newDescribeNotificationSubscriptionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeNotificationSubscriptionsResponse
newDescribeNotificationSubscriptionsResponse
  pHttpStatus_ =
    DescribeNotificationSubscriptionsResponse'
      { subscriptions =
          Core.Nothing,
        marker = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The subscriptions.
describeNotificationSubscriptionsResponse_subscriptions :: Lens.Lens' DescribeNotificationSubscriptionsResponse (Core.Maybe [Subscription])
describeNotificationSubscriptionsResponse_subscriptions = Lens.lens (\DescribeNotificationSubscriptionsResponse' {subscriptions} -> subscriptions) (\s@DescribeNotificationSubscriptionsResponse' {} a -> s {subscriptions = a} :: DescribeNotificationSubscriptionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The marker to use when requesting the next set of results. If there are
-- no additional results, the string is empty.
describeNotificationSubscriptionsResponse_marker :: Lens.Lens' DescribeNotificationSubscriptionsResponse (Core.Maybe Core.Text)
describeNotificationSubscriptionsResponse_marker = Lens.lens (\DescribeNotificationSubscriptionsResponse' {marker} -> marker) (\s@DescribeNotificationSubscriptionsResponse' {} a -> s {marker = a} :: DescribeNotificationSubscriptionsResponse)

-- | The response's http status code.
describeNotificationSubscriptionsResponse_httpStatus :: Lens.Lens' DescribeNotificationSubscriptionsResponse Core.Int
describeNotificationSubscriptionsResponse_httpStatus = Lens.lens (\DescribeNotificationSubscriptionsResponse' {httpStatus} -> httpStatus) (\s@DescribeNotificationSubscriptionsResponse' {} a -> s {httpStatus = a} :: DescribeNotificationSubscriptionsResponse)

instance
  Core.NFData
    DescribeNotificationSubscriptionsResponse
