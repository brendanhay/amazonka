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
-- Module      : Network.AWS.AutoScaling.DescribeAutoScalingInstances
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more Auto Scaling instances.
--
-- This operation returns paginated results.
module Network.AWS.AutoScaling.DescribeAutoScalingInstances
  ( -- * Creating a Request
    DescribeAutoScalingInstances (..),
    newDescribeAutoScalingInstances,

    -- * Request Lenses
    describeAutoScalingInstances_instanceIds,
    describeAutoScalingInstances_nextToken,
    describeAutoScalingInstances_maxRecords,

    -- * Destructuring the Response
    DescribeAutoScalingInstancesResponse (..),
    newDescribeAutoScalingInstancesResponse,

    -- * Response Lenses
    describeAutoScalingInstancesResponse_nextToken,
    describeAutoScalingInstancesResponse_autoScalingInstances,
    describeAutoScalingInstancesResponse_httpStatus,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeAutoScalingInstances' smart constructor.
data DescribeAutoScalingInstances = DescribeAutoScalingInstances'
  { -- | The IDs of the instances. You can specify up to @MaxRecords@ IDs. If you
    -- omit this parameter, all Auto Scaling instances are described. If you
    -- specify an ID that does not exist, it is ignored with no error.
    instanceIds :: Core.Maybe [Core.Text],
    -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of items to return with this call. The default value
    -- is @50@ and the maximum value is @50@.
    maxRecords :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeAutoScalingInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceIds', 'describeAutoScalingInstances_instanceIds' - The IDs of the instances. You can specify up to @MaxRecords@ IDs. If you
-- omit this parameter, all Auto Scaling instances are described. If you
-- specify an ID that does not exist, it is ignored with no error.
--
-- 'nextToken', 'describeAutoScalingInstances_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'maxRecords', 'describeAutoScalingInstances_maxRecords' - The maximum number of items to return with this call. The default value
-- is @50@ and the maximum value is @50@.
newDescribeAutoScalingInstances ::
  DescribeAutoScalingInstances
newDescribeAutoScalingInstances =
  DescribeAutoScalingInstances'
    { instanceIds =
        Core.Nothing,
      nextToken = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | The IDs of the instances. You can specify up to @MaxRecords@ IDs. If you
-- omit this parameter, all Auto Scaling instances are described. If you
-- specify an ID that does not exist, it is ignored with no error.
describeAutoScalingInstances_instanceIds :: Lens.Lens' DescribeAutoScalingInstances (Core.Maybe [Core.Text])
describeAutoScalingInstances_instanceIds = Lens.lens (\DescribeAutoScalingInstances' {instanceIds} -> instanceIds) (\s@DescribeAutoScalingInstances' {} a -> s {instanceIds = a} :: DescribeAutoScalingInstances) Core.. Lens.mapping Lens._Coerce

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeAutoScalingInstances_nextToken :: Lens.Lens' DescribeAutoScalingInstances (Core.Maybe Core.Text)
describeAutoScalingInstances_nextToken = Lens.lens (\DescribeAutoScalingInstances' {nextToken} -> nextToken) (\s@DescribeAutoScalingInstances' {} a -> s {nextToken = a} :: DescribeAutoScalingInstances)

-- | The maximum number of items to return with this call. The default value
-- is @50@ and the maximum value is @50@.
describeAutoScalingInstances_maxRecords :: Lens.Lens' DescribeAutoScalingInstances (Core.Maybe Core.Int)
describeAutoScalingInstances_maxRecords = Lens.lens (\DescribeAutoScalingInstances' {maxRecords} -> maxRecords) (\s@DescribeAutoScalingInstances' {} a -> s {maxRecords = a} :: DescribeAutoScalingInstances)

instance Core.AWSPager DescribeAutoScalingInstances where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeAutoScalingInstancesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeAutoScalingInstancesResponse_autoScalingInstances
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeAutoScalingInstances_nextToken
          Lens..~ rs
          Lens.^? describeAutoScalingInstancesResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeAutoScalingInstances where
  type
    AWSResponse DescribeAutoScalingInstances =
      DescribeAutoScalingInstancesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeAutoScalingInstancesResult"
      ( \s h x ->
          DescribeAutoScalingInstancesResponse'
            Core.<$> (x Core..@? "NextToken")
            Core.<*> ( x Core..@? "AutoScalingInstances"
                         Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeAutoScalingInstances

instance Core.NFData DescribeAutoScalingInstances

instance Core.ToHeaders DescribeAutoScalingInstances where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeAutoScalingInstances where
  toPath = Core.const "/"

instance Core.ToQuery DescribeAutoScalingInstances where
  toQuery DescribeAutoScalingInstances' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeAutoScalingInstances" :: Core.ByteString),
        "Version" Core.=: ("2011-01-01" :: Core.ByteString),
        "InstanceIds"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> instanceIds),
        "NextToken" Core.=: nextToken,
        "MaxRecords" Core.=: maxRecords
      ]

-- | /See:/ 'newDescribeAutoScalingInstancesResponse' smart constructor.
data DescribeAutoScalingInstancesResponse = DescribeAutoScalingInstancesResponse'
  { -- | A string that indicates that the response contains more items than can
    -- be returned in a single response. To receive additional items, specify
    -- this string for the @NextToken@ value when requesting the next set of
    -- items. This value is null when there are no more items to return.
    nextToken :: Core.Maybe Core.Text,
    -- | The instances.
    autoScalingInstances :: Core.Maybe [AutoScalingInstanceDetails],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeAutoScalingInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeAutoScalingInstancesResponse_nextToken' - A string that indicates that the response contains more items than can
-- be returned in a single response. To receive additional items, specify
-- this string for the @NextToken@ value when requesting the next set of
-- items. This value is null when there are no more items to return.
--
-- 'autoScalingInstances', 'describeAutoScalingInstancesResponse_autoScalingInstances' - The instances.
--
-- 'httpStatus', 'describeAutoScalingInstancesResponse_httpStatus' - The response's http status code.
newDescribeAutoScalingInstancesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeAutoScalingInstancesResponse
newDescribeAutoScalingInstancesResponse pHttpStatus_ =
  DescribeAutoScalingInstancesResponse'
    { nextToken =
        Core.Nothing,
      autoScalingInstances = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A string that indicates that the response contains more items than can
-- be returned in a single response. To receive additional items, specify
-- this string for the @NextToken@ value when requesting the next set of
-- items. This value is null when there are no more items to return.
describeAutoScalingInstancesResponse_nextToken :: Lens.Lens' DescribeAutoScalingInstancesResponse (Core.Maybe Core.Text)
describeAutoScalingInstancesResponse_nextToken = Lens.lens (\DescribeAutoScalingInstancesResponse' {nextToken} -> nextToken) (\s@DescribeAutoScalingInstancesResponse' {} a -> s {nextToken = a} :: DescribeAutoScalingInstancesResponse)

-- | The instances.
describeAutoScalingInstancesResponse_autoScalingInstances :: Lens.Lens' DescribeAutoScalingInstancesResponse (Core.Maybe [AutoScalingInstanceDetails])
describeAutoScalingInstancesResponse_autoScalingInstances = Lens.lens (\DescribeAutoScalingInstancesResponse' {autoScalingInstances} -> autoScalingInstances) (\s@DescribeAutoScalingInstancesResponse' {} a -> s {autoScalingInstances = a} :: DescribeAutoScalingInstancesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeAutoScalingInstancesResponse_httpStatus :: Lens.Lens' DescribeAutoScalingInstancesResponse Core.Int
describeAutoScalingInstancesResponse_httpStatus = Lens.lens (\DescribeAutoScalingInstancesResponse' {httpStatus} -> httpStatus) (\s@DescribeAutoScalingInstancesResponse' {} a -> s {httpStatus = a} :: DescribeAutoScalingInstancesResponse)

instance
  Core.NFData
    DescribeAutoScalingInstancesResponse
