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
-- Module      : Network.AWS.EC2.DescribeVpcEndpointConnectionNotifications
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the connection notifications for VPC endpoints and VPC
-- endpoint services.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeVpcEndpointConnectionNotifications
  ( -- * Creating a Request
    DescribeVpcEndpointConnectionNotifications (..),
    newDescribeVpcEndpointConnectionNotifications,

    -- * Request Lenses
    describeVpcEndpointConnectionNotifications_nextToken,
    describeVpcEndpointConnectionNotifications_dryRun,
    describeVpcEndpointConnectionNotifications_connectionNotificationId,
    describeVpcEndpointConnectionNotifications_maxResults,
    describeVpcEndpointConnectionNotifications_filters,

    -- * Destructuring the Response
    DescribeVpcEndpointConnectionNotificationsResponse (..),
    newDescribeVpcEndpointConnectionNotificationsResponse,

    -- * Response Lenses
    describeVpcEndpointConnectionNotificationsResponse_nextToken,
    describeVpcEndpointConnectionNotificationsResponse_connectionNotificationSet,
    describeVpcEndpointConnectionNotificationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeVpcEndpointConnectionNotifications' smart constructor.
data DescribeVpcEndpointConnectionNotifications = DescribeVpcEndpointConnectionNotifications'
  { -- | The token to request the next page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the notification.
    connectionNotificationId :: Core.Maybe Core.Text,
    -- | The maximum number of results to return in a single call. To retrieve
    -- the remaining results, make another request with the returned
    -- @NextToken@ value.
    maxResults :: Core.Maybe Core.Int,
    -- | One or more filters.
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
    filters :: Core.Maybe [Filter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeVpcEndpointConnectionNotifications' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeVpcEndpointConnectionNotifications_nextToken' - The token to request the next page of results.
--
-- 'dryRun', 'describeVpcEndpointConnectionNotifications_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'connectionNotificationId', 'describeVpcEndpointConnectionNotifications_connectionNotificationId' - The ID of the notification.
--
-- 'maxResults', 'describeVpcEndpointConnectionNotifications_maxResults' - The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another request with the returned
-- @NextToken@ value.
--
-- 'filters', 'describeVpcEndpointConnectionNotifications_filters' - One or more filters.
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
newDescribeVpcEndpointConnectionNotifications ::
  DescribeVpcEndpointConnectionNotifications
newDescribeVpcEndpointConnectionNotifications =
  DescribeVpcEndpointConnectionNotifications'
    { nextToken =
        Core.Nothing,
      dryRun = Core.Nothing,
      connectionNotificationId =
        Core.Nothing,
      maxResults = Core.Nothing,
      filters = Core.Nothing
    }

-- | The token to request the next page of results.
describeVpcEndpointConnectionNotifications_nextToken :: Lens.Lens' DescribeVpcEndpointConnectionNotifications (Core.Maybe Core.Text)
describeVpcEndpointConnectionNotifications_nextToken = Lens.lens (\DescribeVpcEndpointConnectionNotifications' {nextToken} -> nextToken) (\s@DescribeVpcEndpointConnectionNotifications' {} a -> s {nextToken = a} :: DescribeVpcEndpointConnectionNotifications)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeVpcEndpointConnectionNotifications_dryRun :: Lens.Lens' DescribeVpcEndpointConnectionNotifications (Core.Maybe Core.Bool)
describeVpcEndpointConnectionNotifications_dryRun = Lens.lens (\DescribeVpcEndpointConnectionNotifications' {dryRun} -> dryRun) (\s@DescribeVpcEndpointConnectionNotifications' {} a -> s {dryRun = a} :: DescribeVpcEndpointConnectionNotifications)

-- | The ID of the notification.
describeVpcEndpointConnectionNotifications_connectionNotificationId :: Lens.Lens' DescribeVpcEndpointConnectionNotifications (Core.Maybe Core.Text)
describeVpcEndpointConnectionNotifications_connectionNotificationId = Lens.lens (\DescribeVpcEndpointConnectionNotifications' {connectionNotificationId} -> connectionNotificationId) (\s@DescribeVpcEndpointConnectionNotifications' {} a -> s {connectionNotificationId = a} :: DescribeVpcEndpointConnectionNotifications)

-- | The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another request with the returned
-- @NextToken@ value.
describeVpcEndpointConnectionNotifications_maxResults :: Lens.Lens' DescribeVpcEndpointConnectionNotifications (Core.Maybe Core.Int)
describeVpcEndpointConnectionNotifications_maxResults = Lens.lens (\DescribeVpcEndpointConnectionNotifications' {maxResults} -> maxResults) (\s@DescribeVpcEndpointConnectionNotifications' {} a -> s {maxResults = a} :: DescribeVpcEndpointConnectionNotifications)

-- | One or more filters.
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
describeVpcEndpointConnectionNotifications_filters :: Lens.Lens' DescribeVpcEndpointConnectionNotifications (Core.Maybe [Filter])
describeVpcEndpointConnectionNotifications_filters = Lens.lens (\DescribeVpcEndpointConnectionNotifications' {filters} -> filters) (\s@DescribeVpcEndpointConnectionNotifications' {} a -> s {filters = a} :: DescribeVpcEndpointConnectionNotifications) Core.. Lens.mapping Lens._Coerce

instance
  Core.AWSPager
    DescribeVpcEndpointConnectionNotifications
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeVpcEndpointConnectionNotificationsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeVpcEndpointConnectionNotificationsResponse_connectionNotificationSet
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeVpcEndpointConnectionNotifications_nextToken
          Lens..~ rs
            Lens.^? describeVpcEndpointConnectionNotificationsResponse_nextToken
              Core.. Lens._Just

instance
  Core.AWSRequest
    DescribeVpcEndpointConnectionNotifications
  where
  type
    AWSResponse
      DescribeVpcEndpointConnectionNotifications =
      DescribeVpcEndpointConnectionNotificationsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeVpcEndpointConnectionNotificationsResponse'
            Core.<$> (x Core..@? "nextToken")
              Core.<*> ( x Core..@? "connectionNotificationSet"
                           Core..!@ Core.mempty
                           Core.>>= Core.may (Core.parseXMLList "item")
                       )
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeVpcEndpointConnectionNotifications

instance
  Core.NFData
    DescribeVpcEndpointConnectionNotifications

instance
  Core.ToHeaders
    DescribeVpcEndpointConnectionNotifications
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    DescribeVpcEndpointConnectionNotifications
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeVpcEndpointConnectionNotifications
  where
  toQuery
    DescribeVpcEndpointConnectionNotifications' {..} =
      Core.mconcat
        [ "Action"
            Core.=: ( "DescribeVpcEndpointConnectionNotifications" ::
                        Core.ByteString
                    ),
          "Version" Core.=: ("2016-11-15" :: Core.ByteString),
          "NextToken" Core.=: nextToken,
          "DryRun" Core.=: dryRun,
          "ConnectionNotificationId"
            Core.=: connectionNotificationId,
          "MaxResults" Core.=: maxResults,
          Core.toQuery
            (Core.toQueryList "Filter" Core.<$> filters)
        ]

-- | /See:/ 'newDescribeVpcEndpointConnectionNotificationsResponse' smart constructor.
data DescribeVpcEndpointConnectionNotificationsResponse = DescribeVpcEndpointConnectionNotificationsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | One or more notifications.
    connectionNotificationSet :: Core.Maybe [ConnectionNotification],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeVpcEndpointConnectionNotificationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeVpcEndpointConnectionNotificationsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'connectionNotificationSet', 'describeVpcEndpointConnectionNotificationsResponse_connectionNotificationSet' - One or more notifications.
--
-- 'httpStatus', 'describeVpcEndpointConnectionNotificationsResponse_httpStatus' - The response's http status code.
newDescribeVpcEndpointConnectionNotificationsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeVpcEndpointConnectionNotificationsResponse
newDescribeVpcEndpointConnectionNotificationsResponse
  pHttpStatus_ =
    DescribeVpcEndpointConnectionNotificationsResponse'
      { nextToken =
          Core.Nothing,
        connectionNotificationSet =
          Core.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeVpcEndpointConnectionNotificationsResponse_nextToken :: Lens.Lens' DescribeVpcEndpointConnectionNotificationsResponse (Core.Maybe Core.Text)
describeVpcEndpointConnectionNotificationsResponse_nextToken = Lens.lens (\DescribeVpcEndpointConnectionNotificationsResponse' {nextToken} -> nextToken) (\s@DescribeVpcEndpointConnectionNotificationsResponse' {} a -> s {nextToken = a} :: DescribeVpcEndpointConnectionNotificationsResponse)

-- | One or more notifications.
describeVpcEndpointConnectionNotificationsResponse_connectionNotificationSet :: Lens.Lens' DescribeVpcEndpointConnectionNotificationsResponse (Core.Maybe [ConnectionNotification])
describeVpcEndpointConnectionNotificationsResponse_connectionNotificationSet = Lens.lens (\DescribeVpcEndpointConnectionNotificationsResponse' {connectionNotificationSet} -> connectionNotificationSet) (\s@DescribeVpcEndpointConnectionNotificationsResponse' {} a -> s {connectionNotificationSet = a} :: DescribeVpcEndpointConnectionNotificationsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeVpcEndpointConnectionNotificationsResponse_httpStatus :: Lens.Lens' DescribeVpcEndpointConnectionNotificationsResponse Core.Int
describeVpcEndpointConnectionNotificationsResponse_httpStatus = Lens.lens (\DescribeVpcEndpointConnectionNotificationsResponse' {httpStatus} -> httpStatus) (\s@DescribeVpcEndpointConnectionNotificationsResponse' {} a -> s {httpStatus = a} :: DescribeVpcEndpointConnectionNotificationsResponse)

instance
  Core.NFData
    DescribeVpcEndpointConnectionNotificationsResponse
