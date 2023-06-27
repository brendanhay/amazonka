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
-- Module      : Amazonka.EC2.DescribeVpcEndpointConnectionNotifications
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the connection notifications for VPC endpoints and VPC
-- endpoint services.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeVpcEndpointConnectionNotifications
  ( -- * Creating a Request
    DescribeVpcEndpointConnectionNotifications (..),
    newDescribeVpcEndpointConnectionNotifications,

    -- * Request Lenses
    describeVpcEndpointConnectionNotifications_connectionNotificationId,
    describeVpcEndpointConnectionNotifications_dryRun,
    describeVpcEndpointConnectionNotifications_filters,
    describeVpcEndpointConnectionNotifications_maxResults,
    describeVpcEndpointConnectionNotifications_nextToken,

    -- * Destructuring the Response
    DescribeVpcEndpointConnectionNotificationsResponse (..),
    newDescribeVpcEndpointConnectionNotificationsResponse,

    -- * Response Lenses
    describeVpcEndpointConnectionNotificationsResponse_connectionNotificationSet,
    describeVpcEndpointConnectionNotificationsResponse_nextToken,
    describeVpcEndpointConnectionNotificationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeVpcEndpointConnectionNotifications' smart constructor.
data DescribeVpcEndpointConnectionNotifications = DescribeVpcEndpointConnectionNotifications'
  { -- | The ID of the notification.
    connectionNotificationId :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The filters.
    --
    -- -   @connection-notification-arn@ - The ARN of the SNS topic for the
    --     notification.
    --
    -- -   @connection-notification-id@ - The ID of the notification.
    --
    -- -   @connection-notification-state@ - The state of the notification
    --     (@Enabled@ | @Disabled@).
    --
    -- -   @connection-notification-type@ - The type of notification (@Topic@).
    --
    -- -   @service-id@ - The ID of the endpoint service.
    --
    -- -   @vpc-endpoint-id@ - The ID of the VPC endpoint.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of results to return in a single call. To retrieve
    -- the remaining results, make another request with the returned
    -- @NextToken@ value.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The token to request the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVpcEndpointConnectionNotifications' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionNotificationId', 'describeVpcEndpointConnectionNotifications_connectionNotificationId' - The ID of the notification.
--
-- 'dryRun', 'describeVpcEndpointConnectionNotifications_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeVpcEndpointConnectionNotifications_filters' - The filters.
--
-- -   @connection-notification-arn@ - The ARN of the SNS topic for the
--     notification.
--
-- -   @connection-notification-id@ - The ID of the notification.
--
-- -   @connection-notification-state@ - The state of the notification
--     (@Enabled@ | @Disabled@).
--
-- -   @connection-notification-type@ - The type of notification (@Topic@).
--
-- -   @service-id@ - The ID of the endpoint service.
--
-- -   @vpc-endpoint-id@ - The ID of the VPC endpoint.
--
-- 'maxResults', 'describeVpcEndpointConnectionNotifications_maxResults' - The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another request with the returned
-- @NextToken@ value.
--
-- 'nextToken', 'describeVpcEndpointConnectionNotifications_nextToken' - The token to request the next page of results.
newDescribeVpcEndpointConnectionNotifications ::
  DescribeVpcEndpointConnectionNotifications
newDescribeVpcEndpointConnectionNotifications =
  DescribeVpcEndpointConnectionNotifications'
    { connectionNotificationId =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The ID of the notification.
describeVpcEndpointConnectionNotifications_connectionNotificationId :: Lens.Lens' DescribeVpcEndpointConnectionNotifications (Prelude.Maybe Prelude.Text)
describeVpcEndpointConnectionNotifications_connectionNotificationId = Lens.lens (\DescribeVpcEndpointConnectionNotifications' {connectionNotificationId} -> connectionNotificationId) (\s@DescribeVpcEndpointConnectionNotifications' {} a -> s {connectionNotificationId = a} :: DescribeVpcEndpointConnectionNotifications)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeVpcEndpointConnectionNotifications_dryRun :: Lens.Lens' DescribeVpcEndpointConnectionNotifications (Prelude.Maybe Prelude.Bool)
describeVpcEndpointConnectionNotifications_dryRun = Lens.lens (\DescribeVpcEndpointConnectionNotifications' {dryRun} -> dryRun) (\s@DescribeVpcEndpointConnectionNotifications' {} a -> s {dryRun = a} :: DescribeVpcEndpointConnectionNotifications)

-- | The filters.
--
-- -   @connection-notification-arn@ - The ARN of the SNS topic for the
--     notification.
--
-- -   @connection-notification-id@ - The ID of the notification.
--
-- -   @connection-notification-state@ - The state of the notification
--     (@Enabled@ | @Disabled@).
--
-- -   @connection-notification-type@ - The type of notification (@Topic@).
--
-- -   @service-id@ - The ID of the endpoint service.
--
-- -   @vpc-endpoint-id@ - The ID of the VPC endpoint.
describeVpcEndpointConnectionNotifications_filters :: Lens.Lens' DescribeVpcEndpointConnectionNotifications (Prelude.Maybe [Filter])
describeVpcEndpointConnectionNotifications_filters = Lens.lens (\DescribeVpcEndpointConnectionNotifications' {filters} -> filters) (\s@DescribeVpcEndpointConnectionNotifications' {} a -> s {filters = a} :: DescribeVpcEndpointConnectionNotifications) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another request with the returned
-- @NextToken@ value.
describeVpcEndpointConnectionNotifications_maxResults :: Lens.Lens' DescribeVpcEndpointConnectionNotifications (Prelude.Maybe Prelude.Int)
describeVpcEndpointConnectionNotifications_maxResults = Lens.lens (\DescribeVpcEndpointConnectionNotifications' {maxResults} -> maxResults) (\s@DescribeVpcEndpointConnectionNotifications' {} a -> s {maxResults = a} :: DescribeVpcEndpointConnectionNotifications)

-- | The token to request the next page of results.
describeVpcEndpointConnectionNotifications_nextToken :: Lens.Lens' DescribeVpcEndpointConnectionNotifications (Prelude.Maybe Prelude.Text)
describeVpcEndpointConnectionNotifications_nextToken = Lens.lens (\DescribeVpcEndpointConnectionNotifications' {nextToken} -> nextToken) (\s@DescribeVpcEndpointConnectionNotifications' {} a -> s {nextToken = a} :: DescribeVpcEndpointConnectionNotifications)

instance
  Core.AWSPager
    DescribeVpcEndpointConnectionNotifications
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeVpcEndpointConnectionNotificationsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeVpcEndpointConnectionNotificationsResponse_connectionNotificationSet
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeVpcEndpointConnectionNotifications_nextToken
          Lens..~ rs
          Lens.^? describeVpcEndpointConnectionNotificationsResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeVpcEndpointConnectionNotifications
  where
  type
    AWSResponse
      DescribeVpcEndpointConnectionNotifications =
      DescribeVpcEndpointConnectionNotificationsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeVpcEndpointConnectionNotificationsResponse'
            Prelude.<$> ( x
                            Data..@? "connectionNotificationSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeVpcEndpointConnectionNotifications
  where
  hashWithSalt
    _salt
    DescribeVpcEndpointConnectionNotifications' {..} =
      _salt
        `Prelude.hashWithSalt` connectionNotificationId
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken

instance
  Prelude.NFData
    DescribeVpcEndpointConnectionNotifications
  where
  rnf DescribeVpcEndpointConnectionNotifications' {..} =
    Prelude.rnf connectionNotificationId
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance
  Data.ToHeaders
    DescribeVpcEndpointConnectionNotifications
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DescribeVpcEndpointConnectionNotifications
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeVpcEndpointConnectionNotifications
  where
  toQuery
    DescribeVpcEndpointConnectionNotifications' {..} =
      Prelude.mconcat
        [ "Action"
            Data.=: ( "DescribeVpcEndpointConnectionNotifications" ::
                        Prelude.ByteString
                    ),
          "Version"
            Data.=: ("2016-11-15" :: Prelude.ByteString),
          "ConnectionNotificationId"
            Data.=: connectionNotificationId,
          "DryRun" Data.=: dryRun,
          Data.toQuery
            (Data.toQueryList "Filter" Prelude.<$> filters),
          "MaxResults" Data.=: maxResults,
          "NextToken" Data.=: nextToken
        ]

-- | /See:/ 'newDescribeVpcEndpointConnectionNotificationsResponse' smart constructor.
data DescribeVpcEndpointConnectionNotificationsResponse = DescribeVpcEndpointConnectionNotificationsResponse'
  { -- | The notifications.
    connectionNotificationSet :: Prelude.Maybe [ConnectionNotification],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVpcEndpointConnectionNotificationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionNotificationSet', 'describeVpcEndpointConnectionNotificationsResponse_connectionNotificationSet' - The notifications.
--
-- 'nextToken', 'describeVpcEndpointConnectionNotificationsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'describeVpcEndpointConnectionNotificationsResponse_httpStatus' - The response's http status code.
newDescribeVpcEndpointConnectionNotificationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeVpcEndpointConnectionNotificationsResponse
newDescribeVpcEndpointConnectionNotificationsResponse
  pHttpStatus_ =
    DescribeVpcEndpointConnectionNotificationsResponse'
      { connectionNotificationSet =
          Prelude.Nothing,
        nextToken =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | The notifications.
describeVpcEndpointConnectionNotificationsResponse_connectionNotificationSet :: Lens.Lens' DescribeVpcEndpointConnectionNotificationsResponse (Prelude.Maybe [ConnectionNotification])
describeVpcEndpointConnectionNotificationsResponse_connectionNotificationSet = Lens.lens (\DescribeVpcEndpointConnectionNotificationsResponse' {connectionNotificationSet} -> connectionNotificationSet) (\s@DescribeVpcEndpointConnectionNotificationsResponse' {} a -> s {connectionNotificationSet = a} :: DescribeVpcEndpointConnectionNotificationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeVpcEndpointConnectionNotificationsResponse_nextToken :: Lens.Lens' DescribeVpcEndpointConnectionNotificationsResponse (Prelude.Maybe Prelude.Text)
describeVpcEndpointConnectionNotificationsResponse_nextToken = Lens.lens (\DescribeVpcEndpointConnectionNotificationsResponse' {nextToken} -> nextToken) (\s@DescribeVpcEndpointConnectionNotificationsResponse' {} a -> s {nextToken = a} :: DescribeVpcEndpointConnectionNotificationsResponse)

-- | The response's http status code.
describeVpcEndpointConnectionNotificationsResponse_httpStatus :: Lens.Lens' DescribeVpcEndpointConnectionNotificationsResponse Prelude.Int
describeVpcEndpointConnectionNotificationsResponse_httpStatus = Lens.lens (\DescribeVpcEndpointConnectionNotificationsResponse' {httpStatus} -> httpStatus) (\s@DescribeVpcEndpointConnectionNotificationsResponse' {} a -> s {httpStatus = a} :: DescribeVpcEndpointConnectionNotificationsResponse)

instance
  Prelude.NFData
    DescribeVpcEndpointConnectionNotificationsResponse
  where
  rnf
    DescribeVpcEndpointConnectionNotificationsResponse' {..} =
      Prelude.rnf connectionNotificationSet
        `Prelude.seq` Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf httpStatus
