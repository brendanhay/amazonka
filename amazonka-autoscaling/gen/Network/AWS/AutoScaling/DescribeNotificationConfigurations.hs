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
-- Module      : Network.AWS.AutoScaling.DescribeNotificationConfigurations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the notification actions associated with the specified Auto
-- Scaling group.
--
-- This operation returns paginated results.
module Network.AWS.AutoScaling.DescribeNotificationConfigurations
  ( -- * Creating a Request
    DescribeNotificationConfigurations (..),
    newDescribeNotificationConfigurations,

    -- * Request Lenses
    describeNotificationConfigurations_nextToken,
    describeNotificationConfigurations_autoScalingGroupNames,
    describeNotificationConfigurations_maxRecords,

    -- * Destructuring the Response
    DescribeNotificationConfigurationsResponse (..),
    newDescribeNotificationConfigurationsResponse,

    -- * Response Lenses
    describeNotificationConfigurationsResponse_nextToken,
    describeNotificationConfigurationsResponse_httpStatus,
    describeNotificationConfigurationsResponse_notificationConfigurations,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeNotificationConfigurations' smart constructor.
data DescribeNotificationConfigurations = DescribeNotificationConfigurations'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Core.Maybe Core.Text,
    -- | The name of the Auto Scaling group.
    autoScalingGroupNames :: Core.Maybe [Core.Text],
    -- | The maximum number of items to return with this call. The default value
    -- is @50@ and the maximum value is @100@.
    maxRecords :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeNotificationConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeNotificationConfigurations_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'autoScalingGroupNames', 'describeNotificationConfigurations_autoScalingGroupNames' - The name of the Auto Scaling group.
--
-- 'maxRecords', 'describeNotificationConfigurations_maxRecords' - The maximum number of items to return with this call. The default value
-- is @50@ and the maximum value is @100@.
newDescribeNotificationConfigurations ::
  DescribeNotificationConfigurations
newDescribeNotificationConfigurations =
  DescribeNotificationConfigurations'
    { nextToken =
        Core.Nothing,
      autoScalingGroupNames = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeNotificationConfigurations_nextToken :: Lens.Lens' DescribeNotificationConfigurations (Core.Maybe Core.Text)
describeNotificationConfigurations_nextToken = Lens.lens (\DescribeNotificationConfigurations' {nextToken} -> nextToken) (\s@DescribeNotificationConfigurations' {} a -> s {nextToken = a} :: DescribeNotificationConfigurations)

-- | The name of the Auto Scaling group.
describeNotificationConfigurations_autoScalingGroupNames :: Lens.Lens' DescribeNotificationConfigurations (Core.Maybe [Core.Text])
describeNotificationConfigurations_autoScalingGroupNames = Lens.lens (\DescribeNotificationConfigurations' {autoScalingGroupNames} -> autoScalingGroupNames) (\s@DescribeNotificationConfigurations' {} a -> s {autoScalingGroupNames = a} :: DescribeNotificationConfigurations) Core.. Lens.mapping Lens._Coerce

-- | The maximum number of items to return with this call. The default value
-- is @50@ and the maximum value is @100@.
describeNotificationConfigurations_maxRecords :: Lens.Lens' DescribeNotificationConfigurations (Core.Maybe Core.Int)
describeNotificationConfigurations_maxRecords = Lens.lens (\DescribeNotificationConfigurations' {maxRecords} -> maxRecords) (\s@DescribeNotificationConfigurations' {} a -> s {maxRecords = a} :: DescribeNotificationConfigurations)

instance
  Core.AWSPager
    DescribeNotificationConfigurations
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeNotificationConfigurationsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^. describeNotificationConfigurationsResponse_notificationConfigurations
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeNotificationConfigurations_nextToken
          Lens..~ rs
          Lens.^? describeNotificationConfigurationsResponse_nextToken
            Core.. Lens._Just

instance
  Core.AWSRequest
    DescribeNotificationConfigurations
  where
  type
    AWSResponse DescribeNotificationConfigurations =
      DescribeNotificationConfigurationsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeNotificationConfigurationsResult"
      ( \s h x ->
          DescribeNotificationConfigurationsResponse'
            Core.<$> (x Core..@? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> ( x Core..@? "NotificationConfigurations"
                         Core..!@ Core.mempty
                         Core.>>= Core.parseXMLList "member"
                     )
      )

instance
  Core.Hashable
    DescribeNotificationConfigurations

instance
  Core.NFData
    DescribeNotificationConfigurations

instance
  Core.ToHeaders
    DescribeNotificationConfigurations
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    DescribeNotificationConfigurations
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeNotificationConfigurations
  where
  toQuery DescribeNotificationConfigurations' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "DescribeNotificationConfigurations" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2011-01-01" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "AutoScalingGroupNames"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Core.<$> autoScalingGroupNames
            ),
        "MaxRecords" Core.=: maxRecords
      ]

-- | /See:/ 'newDescribeNotificationConfigurationsResponse' smart constructor.
data DescribeNotificationConfigurationsResponse = DescribeNotificationConfigurationsResponse'
  { -- | A string that indicates that the response contains more items than can
    -- be returned in a single response. To receive additional items, specify
    -- this string for the @NextToken@ value when requesting the next set of
    -- items. This value is null when there are no more items to return.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The notification configurations.
    notificationConfigurations :: [NotificationConfiguration]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeNotificationConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeNotificationConfigurationsResponse_nextToken' - A string that indicates that the response contains more items than can
-- be returned in a single response. To receive additional items, specify
-- this string for the @NextToken@ value when requesting the next set of
-- items. This value is null when there are no more items to return.
--
-- 'httpStatus', 'describeNotificationConfigurationsResponse_httpStatus' - The response's http status code.
--
-- 'notificationConfigurations', 'describeNotificationConfigurationsResponse_notificationConfigurations' - The notification configurations.
newDescribeNotificationConfigurationsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeNotificationConfigurationsResponse
newDescribeNotificationConfigurationsResponse
  pHttpStatus_ =
    DescribeNotificationConfigurationsResponse'
      { nextToken =
          Core.Nothing,
        httpStatus = pHttpStatus_,
        notificationConfigurations =
          Core.mempty
      }

-- | A string that indicates that the response contains more items than can
-- be returned in a single response. To receive additional items, specify
-- this string for the @NextToken@ value when requesting the next set of
-- items. This value is null when there are no more items to return.
describeNotificationConfigurationsResponse_nextToken :: Lens.Lens' DescribeNotificationConfigurationsResponse (Core.Maybe Core.Text)
describeNotificationConfigurationsResponse_nextToken = Lens.lens (\DescribeNotificationConfigurationsResponse' {nextToken} -> nextToken) (\s@DescribeNotificationConfigurationsResponse' {} a -> s {nextToken = a} :: DescribeNotificationConfigurationsResponse)

-- | The response's http status code.
describeNotificationConfigurationsResponse_httpStatus :: Lens.Lens' DescribeNotificationConfigurationsResponse Core.Int
describeNotificationConfigurationsResponse_httpStatus = Lens.lens (\DescribeNotificationConfigurationsResponse' {httpStatus} -> httpStatus) (\s@DescribeNotificationConfigurationsResponse' {} a -> s {httpStatus = a} :: DescribeNotificationConfigurationsResponse)

-- | The notification configurations.
describeNotificationConfigurationsResponse_notificationConfigurations :: Lens.Lens' DescribeNotificationConfigurationsResponse [NotificationConfiguration]
describeNotificationConfigurationsResponse_notificationConfigurations = Lens.lens (\DescribeNotificationConfigurationsResponse' {notificationConfigurations} -> notificationConfigurations) (\s@DescribeNotificationConfigurationsResponse' {} a -> s {notificationConfigurations = a} :: DescribeNotificationConfigurationsResponse) Core.. Lens._Coerce

instance
  Core.NFData
    DescribeNotificationConfigurationsResponse
