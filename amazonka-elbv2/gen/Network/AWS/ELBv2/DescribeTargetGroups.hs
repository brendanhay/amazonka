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
-- Module      : Network.AWS.ELBv2.DescribeTargetGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified target groups or all of your target groups. By
-- default, all target groups are described. Alternatively, you can specify
-- one of the following to filter the results: the ARN of the load
-- balancer, the names of one or more target groups, or the ARNs of one or
-- more target groups.
--
-- This operation returns paginated results.
module Network.AWS.ELBv2.DescribeTargetGroups
  ( -- * Creating a Request
    DescribeTargetGroups (..),
    newDescribeTargetGroups,

    -- * Request Lenses
    describeTargetGroups_loadBalancerArn,
    describeTargetGroups_names,
    describeTargetGroups_pageSize,
    describeTargetGroups_targetGroupArns,
    describeTargetGroups_marker,

    -- * Destructuring the Response
    DescribeTargetGroupsResponse (..),
    newDescribeTargetGroupsResponse,

    -- * Response Lenses
    describeTargetGroupsResponse_targetGroups,
    describeTargetGroupsResponse_nextMarker,
    describeTargetGroupsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeTargetGroups' smart constructor.
data DescribeTargetGroups = DescribeTargetGroups'
  { -- | The Amazon Resource Name (ARN) of the load balancer.
    loadBalancerArn :: Core.Maybe Core.Text,
    -- | The names of the target groups.
    names :: Core.Maybe [Core.Text],
    -- | The maximum number of results to return with this call.
    pageSize :: Core.Maybe Core.Natural,
    -- | The Amazon Resource Names (ARN) of the target groups.
    targetGroupArns :: Core.Maybe [Core.Text],
    -- | The marker for the next set of results. (You received this marker from a
    -- previous call.)
    marker :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeTargetGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loadBalancerArn', 'describeTargetGroups_loadBalancerArn' - The Amazon Resource Name (ARN) of the load balancer.
--
-- 'names', 'describeTargetGroups_names' - The names of the target groups.
--
-- 'pageSize', 'describeTargetGroups_pageSize' - The maximum number of results to return with this call.
--
-- 'targetGroupArns', 'describeTargetGroups_targetGroupArns' - The Amazon Resource Names (ARN) of the target groups.
--
-- 'marker', 'describeTargetGroups_marker' - The marker for the next set of results. (You received this marker from a
-- previous call.)
newDescribeTargetGroups ::
  DescribeTargetGroups
newDescribeTargetGroups =
  DescribeTargetGroups'
    { loadBalancerArn =
        Core.Nothing,
      names = Core.Nothing,
      pageSize = Core.Nothing,
      targetGroupArns = Core.Nothing,
      marker = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the load balancer.
describeTargetGroups_loadBalancerArn :: Lens.Lens' DescribeTargetGroups (Core.Maybe Core.Text)
describeTargetGroups_loadBalancerArn = Lens.lens (\DescribeTargetGroups' {loadBalancerArn} -> loadBalancerArn) (\s@DescribeTargetGroups' {} a -> s {loadBalancerArn = a} :: DescribeTargetGroups)

-- | The names of the target groups.
describeTargetGroups_names :: Lens.Lens' DescribeTargetGroups (Core.Maybe [Core.Text])
describeTargetGroups_names = Lens.lens (\DescribeTargetGroups' {names} -> names) (\s@DescribeTargetGroups' {} a -> s {names = a} :: DescribeTargetGroups) Core.. Lens.mapping Lens._Coerce

-- | The maximum number of results to return with this call.
describeTargetGroups_pageSize :: Lens.Lens' DescribeTargetGroups (Core.Maybe Core.Natural)
describeTargetGroups_pageSize = Lens.lens (\DescribeTargetGroups' {pageSize} -> pageSize) (\s@DescribeTargetGroups' {} a -> s {pageSize = a} :: DescribeTargetGroups)

-- | The Amazon Resource Names (ARN) of the target groups.
describeTargetGroups_targetGroupArns :: Lens.Lens' DescribeTargetGroups (Core.Maybe [Core.Text])
describeTargetGroups_targetGroupArns = Lens.lens (\DescribeTargetGroups' {targetGroupArns} -> targetGroupArns) (\s@DescribeTargetGroups' {} a -> s {targetGroupArns = a} :: DescribeTargetGroups) Core.. Lens.mapping Lens._Coerce

-- | The marker for the next set of results. (You received this marker from a
-- previous call.)
describeTargetGroups_marker :: Lens.Lens' DescribeTargetGroups (Core.Maybe Core.Text)
describeTargetGroups_marker = Lens.lens (\DescribeTargetGroups' {marker} -> marker) (\s@DescribeTargetGroups' {} a -> s {marker = a} :: DescribeTargetGroups)

instance Core.AWSPager DescribeTargetGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeTargetGroupsResponse_nextMarker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeTargetGroupsResponse_targetGroups
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeTargetGroups_marker
          Lens..~ rs
          Lens.^? describeTargetGroupsResponse_nextMarker
            Core.. Lens._Just

instance Core.AWSRequest DescribeTargetGroups where
  type
    AWSResponse DescribeTargetGroups =
      DescribeTargetGroupsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeTargetGroupsResult"
      ( \s h x ->
          DescribeTargetGroupsResponse'
            Core.<$> ( x Core..@? "TargetGroups" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (x Core..@? "NextMarker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeTargetGroups

instance Core.NFData DescribeTargetGroups

instance Core.ToHeaders DescribeTargetGroups where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeTargetGroups where
  toPath = Core.const "/"

instance Core.ToQuery DescribeTargetGroups where
  toQuery DescribeTargetGroups' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeTargetGroups" :: Core.ByteString),
        "Version" Core.=: ("2015-12-01" :: Core.ByteString),
        "LoadBalancerArn" Core.=: loadBalancerArn,
        "Names"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> names),
        "PageSize" Core.=: pageSize,
        "TargetGroupArns"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> targetGroupArns),
        "Marker" Core.=: marker
      ]

-- | /See:/ 'newDescribeTargetGroupsResponse' smart constructor.
data DescribeTargetGroupsResponse = DescribeTargetGroupsResponse'
  { -- | Information about the target groups.
    targetGroups :: Core.Maybe [TargetGroup],
    -- | If there are additional results, this is the marker for the next set of
    -- results. Otherwise, this is null.
    nextMarker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeTargetGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetGroups', 'describeTargetGroupsResponse_targetGroups' - Information about the target groups.
--
-- 'nextMarker', 'describeTargetGroupsResponse_nextMarker' - If there are additional results, this is the marker for the next set of
-- results. Otherwise, this is null.
--
-- 'httpStatus', 'describeTargetGroupsResponse_httpStatus' - The response's http status code.
newDescribeTargetGroupsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeTargetGroupsResponse
newDescribeTargetGroupsResponse pHttpStatus_ =
  DescribeTargetGroupsResponse'
    { targetGroups =
        Core.Nothing,
      nextMarker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the target groups.
describeTargetGroupsResponse_targetGroups :: Lens.Lens' DescribeTargetGroupsResponse (Core.Maybe [TargetGroup])
describeTargetGroupsResponse_targetGroups = Lens.lens (\DescribeTargetGroupsResponse' {targetGroups} -> targetGroups) (\s@DescribeTargetGroupsResponse' {} a -> s {targetGroups = a} :: DescribeTargetGroupsResponse) Core.. Lens.mapping Lens._Coerce

-- | If there are additional results, this is the marker for the next set of
-- results. Otherwise, this is null.
describeTargetGroupsResponse_nextMarker :: Lens.Lens' DescribeTargetGroupsResponse (Core.Maybe Core.Text)
describeTargetGroupsResponse_nextMarker = Lens.lens (\DescribeTargetGroupsResponse' {nextMarker} -> nextMarker) (\s@DescribeTargetGroupsResponse' {} a -> s {nextMarker = a} :: DescribeTargetGroupsResponse)

-- | The response's http status code.
describeTargetGroupsResponse_httpStatus :: Lens.Lens' DescribeTargetGroupsResponse Core.Int
describeTargetGroupsResponse_httpStatus = Lens.lens (\DescribeTargetGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeTargetGroupsResponse' {} a -> s {httpStatus = a} :: DescribeTargetGroupsResponse)

instance Core.NFData DescribeTargetGroupsResponse
