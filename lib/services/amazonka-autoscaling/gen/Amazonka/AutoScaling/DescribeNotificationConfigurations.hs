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
-- Module      : Amazonka.AutoScaling.DescribeNotificationConfigurations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the Amazon SNS notifications that are configured
-- for one or more Auto Scaling groups.
--
-- This operation returns paginated results.
module Amazonka.AutoScaling.DescribeNotificationConfigurations
  ( -- * Creating a Request
    DescribeNotificationConfigurations (..),
    newDescribeNotificationConfigurations,

    -- * Request Lenses
    describeNotificationConfigurations_autoScalingGroupNames,
    describeNotificationConfigurations_maxRecords,
    describeNotificationConfigurations_nextToken,

    -- * Destructuring the Response
    DescribeNotificationConfigurationsResponse (..),
    newDescribeNotificationConfigurationsResponse,

    -- * Response Lenses
    describeNotificationConfigurationsResponse_nextToken,
    describeNotificationConfigurationsResponse_httpStatus,
    describeNotificationConfigurationsResponse_notificationConfigurations,
  )
where

import Amazonka.AutoScaling.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeNotificationConfigurations' smart constructor.
data DescribeNotificationConfigurations = DescribeNotificationConfigurations'
  { -- | The name of the Auto Scaling group.
    autoScalingGroupNames :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of items to return with this call. The default value
    -- is @50@ and the maximum value is @100@.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeNotificationConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoScalingGroupNames', 'describeNotificationConfigurations_autoScalingGroupNames' - The name of the Auto Scaling group.
--
-- 'maxRecords', 'describeNotificationConfigurations_maxRecords' - The maximum number of items to return with this call. The default value
-- is @50@ and the maximum value is @100@.
--
-- 'nextToken', 'describeNotificationConfigurations_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
newDescribeNotificationConfigurations ::
  DescribeNotificationConfigurations
newDescribeNotificationConfigurations =
  DescribeNotificationConfigurations'
    { autoScalingGroupNames =
        Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The name of the Auto Scaling group.
describeNotificationConfigurations_autoScalingGroupNames :: Lens.Lens' DescribeNotificationConfigurations (Prelude.Maybe [Prelude.Text])
describeNotificationConfigurations_autoScalingGroupNames = Lens.lens (\DescribeNotificationConfigurations' {autoScalingGroupNames} -> autoScalingGroupNames) (\s@DescribeNotificationConfigurations' {} a -> s {autoScalingGroupNames = a} :: DescribeNotificationConfigurations) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of items to return with this call. The default value
-- is @50@ and the maximum value is @100@.
describeNotificationConfigurations_maxRecords :: Lens.Lens' DescribeNotificationConfigurations (Prelude.Maybe Prelude.Int)
describeNotificationConfigurations_maxRecords = Lens.lens (\DescribeNotificationConfigurations' {maxRecords} -> maxRecords) (\s@DescribeNotificationConfigurations' {} a -> s {maxRecords = a} :: DescribeNotificationConfigurations)

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeNotificationConfigurations_nextToken :: Lens.Lens' DescribeNotificationConfigurations (Prelude.Maybe Prelude.Text)
describeNotificationConfigurations_nextToken = Lens.lens (\DescribeNotificationConfigurations' {nextToken} -> nextToken) (\s@DescribeNotificationConfigurations' {} a -> s {nextToken = a} :: DescribeNotificationConfigurations)

instance
  Core.AWSPager
    DescribeNotificationConfigurations
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeNotificationConfigurationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. describeNotificationConfigurationsResponse_notificationConfigurations
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeNotificationConfigurations_nextToken
          Lens..~ rs
          Lens.^? describeNotificationConfigurationsResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeNotificationConfigurations
  where
  type
    AWSResponse DescribeNotificationConfigurations =
      DescribeNotificationConfigurationsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeNotificationConfigurationsResult"
      ( \s h x ->
          DescribeNotificationConfigurationsResponse'
            Prelude.<$> (x Data..@? "NextToken")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
              Prelude.<*> ( x Data..@? "NotificationConfigurations"
                              Core..!@ Prelude.mempty
                              Prelude.>>= Data.parseXMLList "member"
                          )
      )

instance
  Prelude.Hashable
    DescribeNotificationConfigurations
  where
  hashWithSalt
    _salt
    DescribeNotificationConfigurations' {..} =
      _salt `Prelude.hashWithSalt` autoScalingGroupNames
        `Prelude.hashWithSalt` maxRecords
        `Prelude.hashWithSalt` nextToken

instance
  Prelude.NFData
    DescribeNotificationConfigurations
  where
  rnf DescribeNotificationConfigurations' {..} =
    Prelude.rnf autoScalingGroupNames
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf nextToken

instance
  Data.ToHeaders
    DescribeNotificationConfigurations
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DescribeNotificationConfigurations
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeNotificationConfigurations
  where
  toQuery DescribeNotificationConfigurations' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeNotificationConfigurations" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2011-01-01" :: Prelude.ByteString),
        "AutoScalingGroupNames"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> autoScalingGroupNames
            ),
        "MaxRecords" Data.=: maxRecords,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newDescribeNotificationConfigurationsResponse' smart constructor.
data DescribeNotificationConfigurationsResponse = DescribeNotificationConfigurationsResponse'
  { -- | A string that indicates that the response contains more items than can
    -- be returned in a single response. To receive additional items, specify
    -- this string for the @NextToken@ value when requesting the next set of
    -- items. This value is null when there are no more items to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The notification configurations.
    notificationConfigurations :: [NotificationConfiguration]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeNotificationConfigurationsResponse
newDescribeNotificationConfigurationsResponse
  pHttpStatus_ =
    DescribeNotificationConfigurationsResponse'
      { nextToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        notificationConfigurations =
          Prelude.mempty
      }

-- | A string that indicates that the response contains more items than can
-- be returned in a single response. To receive additional items, specify
-- this string for the @NextToken@ value when requesting the next set of
-- items. This value is null when there are no more items to return.
describeNotificationConfigurationsResponse_nextToken :: Lens.Lens' DescribeNotificationConfigurationsResponse (Prelude.Maybe Prelude.Text)
describeNotificationConfigurationsResponse_nextToken = Lens.lens (\DescribeNotificationConfigurationsResponse' {nextToken} -> nextToken) (\s@DescribeNotificationConfigurationsResponse' {} a -> s {nextToken = a} :: DescribeNotificationConfigurationsResponse)

-- | The response's http status code.
describeNotificationConfigurationsResponse_httpStatus :: Lens.Lens' DescribeNotificationConfigurationsResponse Prelude.Int
describeNotificationConfigurationsResponse_httpStatus = Lens.lens (\DescribeNotificationConfigurationsResponse' {httpStatus} -> httpStatus) (\s@DescribeNotificationConfigurationsResponse' {} a -> s {httpStatus = a} :: DescribeNotificationConfigurationsResponse)

-- | The notification configurations.
describeNotificationConfigurationsResponse_notificationConfigurations :: Lens.Lens' DescribeNotificationConfigurationsResponse [NotificationConfiguration]
describeNotificationConfigurationsResponse_notificationConfigurations = Lens.lens (\DescribeNotificationConfigurationsResponse' {notificationConfigurations} -> notificationConfigurations) (\s@DescribeNotificationConfigurationsResponse' {} a -> s {notificationConfigurations = a} :: DescribeNotificationConfigurationsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    DescribeNotificationConfigurationsResponse
  where
  rnf DescribeNotificationConfigurationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf notificationConfigurations
