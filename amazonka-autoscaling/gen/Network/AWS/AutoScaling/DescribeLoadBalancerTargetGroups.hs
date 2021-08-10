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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeLoadBalancerTargetGroups' smart constructor.
data DescribeLoadBalancerTargetGroups = DescribeLoadBalancerTargetGroups'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return with this call. The default value
    -- is @100@ and the maximum value is @100@.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DescribeLoadBalancerTargetGroups
newDescribeLoadBalancerTargetGroups
  pAutoScalingGroupName_ =
    DescribeLoadBalancerTargetGroups'
      { nextToken =
          Prelude.Nothing,
        maxRecords = Prelude.Nothing,
        autoScalingGroupName =
          pAutoScalingGroupName_
      }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeLoadBalancerTargetGroups_nextToken :: Lens.Lens' DescribeLoadBalancerTargetGroups (Prelude.Maybe Prelude.Text)
describeLoadBalancerTargetGroups_nextToken = Lens.lens (\DescribeLoadBalancerTargetGroups' {nextToken} -> nextToken) (\s@DescribeLoadBalancerTargetGroups' {} a -> s {nextToken = a} :: DescribeLoadBalancerTargetGroups)

-- | The maximum number of items to return with this call. The default value
-- is @100@ and the maximum value is @100@.
describeLoadBalancerTargetGroups_maxRecords :: Lens.Lens' DescribeLoadBalancerTargetGroups (Prelude.Maybe Prelude.Int)
describeLoadBalancerTargetGroups_maxRecords = Lens.lens (\DescribeLoadBalancerTargetGroups' {maxRecords} -> maxRecords) (\s@DescribeLoadBalancerTargetGroups' {} a -> s {maxRecords = a} :: DescribeLoadBalancerTargetGroups)

-- | The name of the Auto Scaling group.
describeLoadBalancerTargetGroups_autoScalingGroupName :: Lens.Lens' DescribeLoadBalancerTargetGroups Prelude.Text
describeLoadBalancerTargetGroups_autoScalingGroupName = Lens.lens (\DescribeLoadBalancerTargetGroups' {autoScalingGroupName} -> autoScalingGroupName) (\s@DescribeLoadBalancerTargetGroups' {} a -> s {autoScalingGroupName = a} :: DescribeLoadBalancerTargetGroups)

instance
  Core.AWSPager
    DescribeLoadBalancerTargetGroups
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeLoadBalancerTargetGroupsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeLoadBalancerTargetGroupsResponse_loadBalancerTargetGroups
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeLoadBalancerTargetGroups_nextToken
          Lens..~ rs
          Lens.^? describeLoadBalancerTargetGroupsResponse_nextToken
            Prelude.. Lens._Just

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
            Prelude.<$> ( x Core..@? "LoadBalancerTargetGroups"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (x Core..@? "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeLoadBalancerTargetGroups

instance
  Prelude.NFData
    DescribeLoadBalancerTargetGroups

instance
  Core.ToHeaders
    DescribeLoadBalancerTargetGroups
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeLoadBalancerTargetGroups where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DescribeLoadBalancerTargetGroups
  where
  toQuery DescribeLoadBalancerTargetGroups' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DescribeLoadBalancerTargetGroups" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2011-01-01" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        "MaxRecords" Core.=: maxRecords,
        "AutoScalingGroupName" Core.=: autoScalingGroupName
      ]

-- | /See:/ 'newDescribeLoadBalancerTargetGroupsResponse' smart constructor.
data DescribeLoadBalancerTargetGroupsResponse = DescribeLoadBalancerTargetGroupsResponse'
  { -- | Information about the target groups.
    loadBalancerTargetGroups :: Prelude.Maybe [LoadBalancerTargetGroupState],
    -- | A string that indicates that the response contains more items than can
    -- be returned in a single response. To receive additional items, specify
    -- this string for the @NextToken@ value when requesting the next set of
    -- items. This value is null when there are no more items to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeLoadBalancerTargetGroupsResponse
newDescribeLoadBalancerTargetGroupsResponse
  pHttpStatus_ =
    DescribeLoadBalancerTargetGroupsResponse'
      { loadBalancerTargetGroups =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the target groups.
describeLoadBalancerTargetGroupsResponse_loadBalancerTargetGroups :: Lens.Lens' DescribeLoadBalancerTargetGroupsResponse (Prelude.Maybe [LoadBalancerTargetGroupState])
describeLoadBalancerTargetGroupsResponse_loadBalancerTargetGroups = Lens.lens (\DescribeLoadBalancerTargetGroupsResponse' {loadBalancerTargetGroups} -> loadBalancerTargetGroups) (\s@DescribeLoadBalancerTargetGroupsResponse' {} a -> s {loadBalancerTargetGroups = a} :: DescribeLoadBalancerTargetGroupsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | A string that indicates that the response contains more items than can
-- be returned in a single response. To receive additional items, specify
-- this string for the @NextToken@ value when requesting the next set of
-- items. This value is null when there are no more items to return.
describeLoadBalancerTargetGroupsResponse_nextToken :: Lens.Lens' DescribeLoadBalancerTargetGroupsResponse (Prelude.Maybe Prelude.Text)
describeLoadBalancerTargetGroupsResponse_nextToken = Lens.lens (\DescribeLoadBalancerTargetGroupsResponse' {nextToken} -> nextToken) (\s@DescribeLoadBalancerTargetGroupsResponse' {} a -> s {nextToken = a} :: DescribeLoadBalancerTargetGroupsResponse)

-- | The response's http status code.
describeLoadBalancerTargetGroupsResponse_httpStatus :: Lens.Lens' DescribeLoadBalancerTargetGroupsResponse Prelude.Int
describeLoadBalancerTargetGroupsResponse_httpStatus = Lens.lens (\DescribeLoadBalancerTargetGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeLoadBalancerTargetGroupsResponse' {} a -> s {httpStatus = a} :: DescribeLoadBalancerTargetGroupsResponse)

instance
  Prelude.NFData
    DescribeLoadBalancerTargetGroupsResponse
