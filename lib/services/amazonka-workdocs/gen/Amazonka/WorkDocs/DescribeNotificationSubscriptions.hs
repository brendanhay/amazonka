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
-- Module      : Amazonka.WorkDocs.DescribeNotificationSubscriptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the specified notification subscriptions.
--
-- This operation returns paginated results.
module Amazonka.WorkDocs.DescribeNotificationSubscriptions
  ( -- * Creating a Request
    DescribeNotificationSubscriptions (..),
    newDescribeNotificationSubscriptions,

    -- * Request Lenses
    describeNotificationSubscriptions_marker,
    describeNotificationSubscriptions_limit,
    describeNotificationSubscriptions_organizationId,

    -- * Destructuring the Response
    DescribeNotificationSubscriptionsResponse (..),
    newDescribeNotificationSubscriptionsResponse,

    -- * Response Lenses
    describeNotificationSubscriptionsResponse_marker,
    describeNotificationSubscriptionsResponse_subscriptions,
    describeNotificationSubscriptionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkDocs.Types

-- | /See:/ 'newDescribeNotificationSubscriptions' smart constructor.
data DescribeNotificationSubscriptions = DescribeNotificationSubscriptions'
  { -- | The marker for the next set of results. (You received this marker from a
    -- previous call.)
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return with this call.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the organization.
    organizationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeNotificationSubscriptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeNotificationSubscriptions_marker' - The marker for the next set of results. (You received this marker from a
-- previous call.)
--
-- 'limit', 'describeNotificationSubscriptions_limit' - The maximum number of items to return with this call.
--
-- 'organizationId', 'describeNotificationSubscriptions_organizationId' - The ID of the organization.
newDescribeNotificationSubscriptions ::
  -- | 'organizationId'
  Prelude.Text ->
  DescribeNotificationSubscriptions
newDescribeNotificationSubscriptions pOrganizationId_ =
  DescribeNotificationSubscriptions'
    { marker =
        Prelude.Nothing,
      limit = Prelude.Nothing,
      organizationId = pOrganizationId_
    }

-- | The marker for the next set of results. (You received this marker from a
-- previous call.)
describeNotificationSubscriptions_marker :: Lens.Lens' DescribeNotificationSubscriptions (Prelude.Maybe Prelude.Text)
describeNotificationSubscriptions_marker = Lens.lens (\DescribeNotificationSubscriptions' {marker} -> marker) (\s@DescribeNotificationSubscriptions' {} a -> s {marker = a} :: DescribeNotificationSubscriptions)

-- | The maximum number of items to return with this call.
describeNotificationSubscriptions_limit :: Lens.Lens' DescribeNotificationSubscriptions (Prelude.Maybe Prelude.Natural)
describeNotificationSubscriptions_limit = Lens.lens (\DescribeNotificationSubscriptions' {limit} -> limit) (\s@DescribeNotificationSubscriptions' {} a -> s {limit = a} :: DescribeNotificationSubscriptions)

-- | The ID of the organization.
describeNotificationSubscriptions_organizationId :: Lens.Lens' DescribeNotificationSubscriptions Prelude.Text
describeNotificationSubscriptions_organizationId = Lens.lens (\DescribeNotificationSubscriptions' {organizationId} -> organizationId) (\s@DescribeNotificationSubscriptions' {} a -> s {organizationId = a} :: DescribeNotificationSubscriptions)

instance
  Core.AWSPager
    DescribeNotificationSubscriptions
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeNotificationSubscriptionsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeNotificationSubscriptionsResponse_subscriptions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeNotificationSubscriptions_marker
          Lens..~ rs
          Lens.^? describeNotificationSubscriptionsResponse_marker
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeNotificationSubscriptions
  where
  type
    AWSResponse DescribeNotificationSubscriptions =
      DescribeNotificationSubscriptionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeNotificationSubscriptionsResponse'
            Prelude.<$> (x Data..?> "Marker")
              Prelude.<*> (x Data..?> "Subscriptions" Core..!@ Prelude.mempty)
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeNotificationSubscriptions
  where
  hashWithSalt
    _salt
    DescribeNotificationSubscriptions' {..} =
      _salt `Prelude.hashWithSalt` marker
        `Prelude.hashWithSalt` limit
        `Prelude.hashWithSalt` organizationId

instance
  Prelude.NFData
    DescribeNotificationSubscriptions
  where
  rnf DescribeNotificationSubscriptions' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf organizationId

instance
  Data.ToHeaders
    DescribeNotificationSubscriptions
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToPath
    DescribeNotificationSubscriptions
  where
  toPath DescribeNotificationSubscriptions' {..} =
    Prelude.mconcat
      [ "/api/v1/organizations/",
        Data.toBS organizationId,
        "/subscriptions"
      ]

instance
  Data.ToQuery
    DescribeNotificationSubscriptions
  where
  toQuery DescribeNotificationSubscriptions' {..} =
    Prelude.mconcat
      ["marker" Data.=: marker, "limit" Data.=: limit]

-- | /See:/ 'newDescribeNotificationSubscriptionsResponse' smart constructor.
data DescribeNotificationSubscriptionsResponse = DescribeNotificationSubscriptionsResponse'
  { -- | The marker to use when requesting the next set of results. If there are
    -- no additional results, the string is empty.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The subscriptions.
    subscriptions :: Prelude.Maybe [Subscription],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeNotificationSubscriptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeNotificationSubscriptionsResponse_marker' - The marker to use when requesting the next set of results. If there are
-- no additional results, the string is empty.
--
-- 'subscriptions', 'describeNotificationSubscriptionsResponse_subscriptions' - The subscriptions.
--
-- 'httpStatus', 'describeNotificationSubscriptionsResponse_httpStatus' - The response's http status code.
newDescribeNotificationSubscriptionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeNotificationSubscriptionsResponse
newDescribeNotificationSubscriptionsResponse
  pHttpStatus_ =
    DescribeNotificationSubscriptionsResponse'
      { marker =
          Prelude.Nothing,
        subscriptions = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The marker to use when requesting the next set of results. If there are
-- no additional results, the string is empty.
describeNotificationSubscriptionsResponse_marker :: Lens.Lens' DescribeNotificationSubscriptionsResponse (Prelude.Maybe Prelude.Text)
describeNotificationSubscriptionsResponse_marker = Lens.lens (\DescribeNotificationSubscriptionsResponse' {marker} -> marker) (\s@DescribeNotificationSubscriptionsResponse' {} a -> s {marker = a} :: DescribeNotificationSubscriptionsResponse)

-- | The subscriptions.
describeNotificationSubscriptionsResponse_subscriptions :: Lens.Lens' DescribeNotificationSubscriptionsResponse (Prelude.Maybe [Subscription])
describeNotificationSubscriptionsResponse_subscriptions = Lens.lens (\DescribeNotificationSubscriptionsResponse' {subscriptions} -> subscriptions) (\s@DescribeNotificationSubscriptionsResponse' {} a -> s {subscriptions = a} :: DescribeNotificationSubscriptionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeNotificationSubscriptionsResponse_httpStatus :: Lens.Lens' DescribeNotificationSubscriptionsResponse Prelude.Int
describeNotificationSubscriptionsResponse_httpStatus = Lens.lens (\DescribeNotificationSubscriptionsResponse' {httpStatus} -> httpStatus) (\s@DescribeNotificationSubscriptionsResponse' {} a -> s {httpStatus = a} :: DescribeNotificationSubscriptionsResponse)

instance
  Prelude.NFData
    DescribeNotificationSubscriptionsResponse
  where
  rnf DescribeNotificationSubscriptionsResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf subscriptions
      `Prelude.seq` Prelude.rnf httpStatus
