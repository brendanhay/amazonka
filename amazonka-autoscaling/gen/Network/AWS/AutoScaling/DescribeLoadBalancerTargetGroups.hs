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
-- Module      : Network.AWS.AutoScaling.DescribeLoadBalancerTargetGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the target groups for the specified Auto Scaling group.
--
-- This operation returns paginated results.
module Network.AWS.AutoScaling.DescribeLoadBalancerTargetGroups
  ( -- * Creating a Request
    DescribeLoadBalancerTargetGroups (..),
    newDescribeLoadBalancerTargetGroups,

    -- * Request Lenses
    describeLoadBalancerTargetGroups_nextToken,
    describeLoadBalancerTargetGroups_maxRecords,
    describeLoadBalancerTargetGroups_autoScalingGroupName,

    -- * Destructuring the Response
    DescribeLoadBalancerTargetGroupsResponse (..),
    newDescribeLoadBalancerTargetGroupsResponse,

    -- * Response Lenses
    describeLoadBalancerTargetGroupsResponse_loadBalancerTargetGroups,
    describeLoadBalancerTargetGroupsResponse_nextToken,
    describeLoadBalancerTargetGroupsResponse_httpStatus,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeLoadBalancerTargetGroups' smart constructor.
data DescribeLoadBalancerTargetGroups = DescribeLoadBalancerTargetGroups'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of items to return with this call. The default value
    -- is @100@ and the maximum value is @100@.
    maxRecords :: Core.Maybe Core.Int,
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeLoadBalancerTargetGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeLoadBalancerTargetGroups_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'maxRecords', 'describeLoadBalancerTargetGroups_maxRecords' - The maximum number of items to return with this call. The default value
-- is @100@ and the maximum value is @100@.
--
-- 'autoScalingGroupName', 'describeLoadBalancerTargetGroups_autoScalingGroupName' - The name of the Auto Scaling group.
newDescribeLoadBalancerTargetGroups ::
  -- | 'autoScalingGroupName'
  Core.Text ->
  DescribeLoadBalancerTargetGroups
newDescribeLoadBalancerTargetGroups
  pAutoScalingGroupName_ =
    DescribeLoadBalancerTargetGroups'
      { nextToken =
          Core.Nothing,
        maxRecords = Core.Nothing,
        autoScalingGroupName =
          pAutoScalingGroupName_
      }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeLoadBalancerTargetGroups_nextToken :: Lens.Lens' DescribeLoadBalancerTargetGroups (Core.Maybe Core.Text)
describeLoadBalancerTargetGroups_nextToken = Lens.lens (\DescribeLoadBalancerTargetGroups' {nextToken} -> nextToken) (\s@DescribeLoadBalancerTargetGroups' {} a -> s {nextToken = a} :: DescribeLoadBalancerTargetGroups)

-- | The maximum number of items to return with this call. The default value
-- is @100@ and the maximum value is @100@.
describeLoadBalancerTargetGroups_maxRecords :: Lens.Lens' DescribeLoadBalancerTargetGroups (Core.Maybe Core.Int)
describeLoadBalancerTargetGroups_maxRecords = Lens.lens (\DescribeLoadBalancerTargetGroups' {maxRecords} -> maxRecords) (\s@DescribeLoadBalancerTargetGroups' {} a -> s {maxRecords = a} :: DescribeLoadBalancerTargetGroups)

-- | The name of the Auto Scaling group.
describeLoadBalancerTargetGroups_autoScalingGroupName :: Lens.Lens' DescribeLoadBalancerTargetGroups Core.Text
describeLoadBalancerTargetGroups_autoScalingGroupName = Lens.lens (\DescribeLoadBalancerTargetGroups' {autoScalingGroupName} -> autoScalingGroupName) (\s@DescribeLoadBalancerTargetGroups' {} a -> s {autoScalingGroupName = a} :: DescribeLoadBalancerTargetGroups)

instance
  Core.AWSPager
    DescribeLoadBalancerTargetGroups
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeLoadBalancerTargetGroupsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeLoadBalancerTargetGroupsResponse_loadBalancerTargetGroups
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeLoadBalancerTargetGroups_nextToken
          Lens..~ rs
          Lens.^? describeLoadBalancerTargetGroupsResponse_nextToken
            Core.. Lens._Just

instance
  Core.AWSRequest
    DescribeLoadBalancerTargetGroups
  where
  type
    AWSResponse DescribeLoadBalancerTargetGroups =
      DescribeLoadBalancerTargetGroupsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeLoadBalancerTargetGroupsResult"
      ( \s h x ->
          DescribeLoadBalancerTargetGroupsResponse'
            Core.<$> ( x Core..@? "LoadBalancerTargetGroups"
                         Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (x Core..@? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeLoadBalancerTargetGroups

instance Core.NFData DescribeLoadBalancerTargetGroups

instance
  Core.ToHeaders
    DescribeLoadBalancerTargetGroups
  where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeLoadBalancerTargetGroups where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeLoadBalancerTargetGroups
  where
  toQuery DescribeLoadBalancerTargetGroups' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "DescribeLoadBalancerTargetGroups" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2011-01-01" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "MaxRecords" Core.=: maxRecords,
        "AutoScalingGroupName" Core.=: autoScalingGroupName
      ]

-- | /See:/ 'newDescribeLoadBalancerTargetGroupsResponse' smart constructor.
data DescribeLoadBalancerTargetGroupsResponse = DescribeLoadBalancerTargetGroupsResponse'
  { -- | Information about the target groups.
    loadBalancerTargetGroups :: Core.Maybe [LoadBalancerTargetGroupState],
    -- | A string that indicates that the response contains more items than can
    -- be returned in a single response. To receive additional items, specify
    -- this string for the @NextToken@ value when requesting the next set of
    -- items. This value is null when there are no more items to return.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeLoadBalancerTargetGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loadBalancerTargetGroups', 'describeLoadBalancerTargetGroupsResponse_loadBalancerTargetGroups' - Information about the target groups.
--
-- 'nextToken', 'describeLoadBalancerTargetGroupsResponse_nextToken' - A string that indicates that the response contains more items than can
-- be returned in a single response. To receive additional items, specify
-- this string for the @NextToken@ value when requesting the next set of
-- items. This value is null when there are no more items to return.
--
-- 'httpStatus', 'describeLoadBalancerTargetGroupsResponse_httpStatus' - The response's http status code.
newDescribeLoadBalancerTargetGroupsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeLoadBalancerTargetGroupsResponse
newDescribeLoadBalancerTargetGroupsResponse
  pHttpStatus_ =
    DescribeLoadBalancerTargetGroupsResponse'
      { loadBalancerTargetGroups =
          Core.Nothing,
        nextToken = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the target groups.
describeLoadBalancerTargetGroupsResponse_loadBalancerTargetGroups :: Lens.Lens' DescribeLoadBalancerTargetGroupsResponse (Core.Maybe [LoadBalancerTargetGroupState])
describeLoadBalancerTargetGroupsResponse_loadBalancerTargetGroups = Lens.lens (\DescribeLoadBalancerTargetGroupsResponse' {loadBalancerTargetGroups} -> loadBalancerTargetGroups) (\s@DescribeLoadBalancerTargetGroupsResponse' {} a -> s {loadBalancerTargetGroups = a} :: DescribeLoadBalancerTargetGroupsResponse) Core.. Lens.mapping Lens._Coerce

-- | A string that indicates that the response contains more items than can
-- be returned in a single response. To receive additional items, specify
-- this string for the @NextToken@ value when requesting the next set of
-- items. This value is null when there are no more items to return.
describeLoadBalancerTargetGroupsResponse_nextToken :: Lens.Lens' DescribeLoadBalancerTargetGroupsResponse (Core.Maybe Core.Text)
describeLoadBalancerTargetGroupsResponse_nextToken = Lens.lens (\DescribeLoadBalancerTargetGroupsResponse' {nextToken} -> nextToken) (\s@DescribeLoadBalancerTargetGroupsResponse' {} a -> s {nextToken = a} :: DescribeLoadBalancerTargetGroupsResponse)

-- | The response's http status code.
describeLoadBalancerTargetGroupsResponse_httpStatus :: Lens.Lens' DescribeLoadBalancerTargetGroupsResponse Core.Int
describeLoadBalancerTargetGroupsResponse_httpStatus = Lens.lens (\DescribeLoadBalancerTargetGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeLoadBalancerTargetGroupsResponse' {} a -> s {httpStatus = a} :: DescribeLoadBalancerTargetGroupsResponse)

instance
  Core.NFData
    DescribeLoadBalancerTargetGroupsResponse
