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
-- Module      : Network.AWS.ELB.DescribeLoadBalancers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified the load balancers. If no load balancers are
-- specified, the call describes all of your load balancers.
--
-- This operation returns paginated results.
module Network.AWS.ELB.DescribeLoadBalancers
  ( -- * Creating a Request
    DescribeLoadBalancers (..),
    newDescribeLoadBalancers,

    -- * Request Lenses
    describeLoadBalancers_pageSize,
    describeLoadBalancers_loadBalancerNames,
    describeLoadBalancers_marker,

    -- * Destructuring the Response
    DescribeLoadBalancersResponse (..),
    newDescribeLoadBalancersResponse,

    -- * Response Lenses
    describeLoadBalancersResponse_loadBalancerDescriptions,
    describeLoadBalancersResponse_nextMarker,
    describeLoadBalancersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DescribeLoadBalancers.
--
-- /See:/ 'newDescribeLoadBalancers' smart constructor.
data DescribeLoadBalancers = DescribeLoadBalancers'
  { -- | The maximum number of results to return with this call (a number from 1
    -- to 400). The default is 400.
    pageSize :: Core.Maybe Core.Natural,
    -- | The names of the load balancers.
    loadBalancerNames :: Core.Maybe [Core.Text],
    -- | The marker for the next set of results. (You received this marker from a
    -- previous call.)
    marker :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeLoadBalancers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageSize', 'describeLoadBalancers_pageSize' - The maximum number of results to return with this call (a number from 1
-- to 400). The default is 400.
--
-- 'loadBalancerNames', 'describeLoadBalancers_loadBalancerNames' - The names of the load balancers.
--
-- 'marker', 'describeLoadBalancers_marker' - The marker for the next set of results. (You received this marker from a
-- previous call.)
newDescribeLoadBalancers ::
  DescribeLoadBalancers
newDescribeLoadBalancers =
  DescribeLoadBalancers'
    { pageSize = Core.Nothing,
      loadBalancerNames = Core.Nothing,
      marker = Core.Nothing
    }

-- | The maximum number of results to return with this call (a number from 1
-- to 400). The default is 400.
describeLoadBalancers_pageSize :: Lens.Lens' DescribeLoadBalancers (Core.Maybe Core.Natural)
describeLoadBalancers_pageSize = Lens.lens (\DescribeLoadBalancers' {pageSize} -> pageSize) (\s@DescribeLoadBalancers' {} a -> s {pageSize = a} :: DescribeLoadBalancers)

-- | The names of the load balancers.
describeLoadBalancers_loadBalancerNames :: Lens.Lens' DescribeLoadBalancers (Core.Maybe [Core.Text])
describeLoadBalancers_loadBalancerNames = Lens.lens (\DescribeLoadBalancers' {loadBalancerNames} -> loadBalancerNames) (\s@DescribeLoadBalancers' {} a -> s {loadBalancerNames = a} :: DescribeLoadBalancers) Core.. Lens.mapping Lens._Coerce

-- | The marker for the next set of results. (You received this marker from a
-- previous call.)
describeLoadBalancers_marker :: Lens.Lens' DescribeLoadBalancers (Core.Maybe Core.Text)
describeLoadBalancers_marker = Lens.lens (\DescribeLoadBalancers' {marker} -> marker) (\s@DescribeLoadBalancers' {} a -> s {marker = a} :: DescribeLoadBalancers)

instance Core.AWSPager DescribeLoadBalancers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeLoadBalancersResponse_nextMarker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeLoadBalancersResponse_loadBalancerDescriptions
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeLoadBalancers_marker
          Lens..~ rs
          Lens.^? describeLoadBalancersResponse_nextMarker
            Core.. Lens._Just

instance Core.AWSRequest DescribeLoadBalancers where
  type
    AWSResponse DescribeLoadBalancers =
      DescribeLoadBalancersResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeLoadBalancersResult"
      ( \s h x ->
          DescribeLoadBalancersResponse'
            Core.<$> ( x Core..@? "LoadBalancerDescriptions"
                         Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (x Core..@? "NextMarker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeLoadBalancers

instance Core.NFData DescribeLoadBalancers

instance Core.ToHeaders DescribeLoadBalancers where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeLoadBalancers where
  toPath = Core.const "/"

instance Core.ToQuery DescribeLoadBalancers where
  toQuery DescribeLoadBalancers' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeLoadBalancers" :: Core.ByteString),
        "Version" Core.=: ("2012-06-01" :: Core.ByteString),
        "PageSize" Core.=: pageSize,
        "LoadBalancerNames"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Core.<$> loadBalancerNames
            ),
        "Marker" Core.=: marker
      ]

-- | Contains the parameters for DescribeLoadBalancers.
--
-- /See:/ 'newDescribeLoadBalancersResponse' smart constructor.
data DescribeLoadBalancersResponse = DescribeLoadBalancersResponse'
  { -- | Information about the load balancers.
    loadBalancerDescriptions :: Core.Maybe [LoadBalancerDescription],
    -- | The marker to use when requesting the next set of results. If there are
    -- no additional results, the string is empty.
    nextMarker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeLoadBalancersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loadBalancerDescriptions', 'describeLoadBalancersResponse_loadBalancerDescriptions' - Information about the load balancers.
--
-- 'nextMarker', 'describeLoadBalancersResponse_nextMarker' - The marker to use when requesting the next set of results. If there are
-- no additional results, the string is empty.
--
-- 'httpStatus', 'describeLoadBalancersResponse_httpStatus' - The response's http status code.
newDescribeLoadBalancersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeLoadBalancersResponse
newDescribeLoadBalancersResponse pHttpStatus_ =
  DescribeLoadBalancersResponse'
    { loadBalancerDescriptions =
        Core.Nothing,
      nextMarker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the load balancers.
describeLoadBalancersResponse_loadBalancerDescriptions :: Lens.Lens' DescribeLoadBalancersResponse (Core.Maybe [LoadBalancerDescription])
describeLoadBalancersResponse_loadBalancerDescriptions = Lens.lens (\DescribeLoadBalancersResponse' {loadBalancerDescriptions} -> loadBalancerDescriptions) (\s@DescribeLoadBalancersResponse' {} a -> s {loadBalancerDescriptions = a} :: DescribeLoadBalancersResponse) Core.. Lens.mapping Lens._Coerce

-- | The marker to use when requesting the next set of results. If there are
-- no additional results, the string is empty.
describeLoadBalancersResponse_nextMarker :: Lens.Lens' DescribeLoadBalancersResponse (Core.Maybe Core.Text)
describeLoadBalancersResponse_nextMarker = Lens.lens (\DescribeLoadBalancersResponse' {nextMarker} -> nextMarker) (\s@DescribeLoadBalancersResponse' {} a -> s {nextMarker = a} :: DescribeLoadBalancersResponse)

-- | The response's http status code.
describeLoadBalancersResponse_httpStatus :: Lens.Lens' DescribeLoadBalancersResponse Core.Int
describeLoadBalancersResponse_httpStatus = Lens.lens (\DescribeLoadBalancersResponse' {httpStatus} -> httpStatus) (\s@DescribeLoadBalancersResponse' {} a -> s {httpStatus = a} :: DescribeLoadBalancersResponse)

instance Core.NFData DescribeLoadBalancersResponse
