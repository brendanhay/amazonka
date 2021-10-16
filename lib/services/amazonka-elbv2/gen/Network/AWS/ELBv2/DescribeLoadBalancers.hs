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
-- Module      : Network.AWS.ELBv2.DescribeLoadBalancers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified load balancers or all of your load balancers.
--
-- This operation returns paginated results.
module Network.AWS.ELBv2.DescribeLoadBalancers
  ( -- * Creating a Request
    DescribeLoadBalancers (..),
    newDescribeLoadBalancers,

    -- * Request Lenses
    describeLoadBalancers_names,
    describeLoadBalancers_pageSize,
    describeLoadBalancers_loadBalancerArns,
    describeLoadBalancers_marker,

    -- * Destructuring the Response
    DescribeLoadBalancersResponse (..),
    newDescribeLoadBalancersResponse,

    -- * Response Lenses
    describeLoadBalancersResponse_nextMarker,
    describeLoadBalancersResponse_loadBalancers,
    describeLoadBalancersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeLoadBalancers' smart constructor.
data DescribeLoadBalancers = DescribeLoadBalancers'
  { -- | The names of the load balancers.
    names :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of results to return with this call.
    pageSize :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Names (ARN) of the load balancers. You can specify
    -- up to 20 load balancers in a single call.
    loadBalancerArns :: Prelude.Maybe [Prelude.Text],
    -- | The marker for the next set of results. (You received this marker from a
    -- previous call.)
    marker :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLoadBalancers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'names', 'describeLoadBalancers_names' - The names of the load balancers.
--
-- 'pageSize', 'describeLoadBalancers_pageSize' - The maximum number of results to return with this call.
--
-- 'loadBalancerArns', 'describeLoadBalancers_loadBalancerArns' - The Amazon Resource Names (ARN) of the load balancers. You can specify
-- up to 20 load balancers in a single call.
--
-- 'marker', 'describeLoadBalancers_marker' - The marker for the next set of results. (You received this marker from a
-- previous call.)
newDescribeLoadBalancers ::
  DescribeLoadBalancers
newDescribeLoadBalancers =
  DescribeLoadBalancers'
    { names = Prelude.Nothing,
      pageSize = Prelude.Nothing,
      loadBalancerArns = Prelude.Nothing,
      marker = Prelude.Nothing
    }

-- | The names of the load balancers.
describeLoadBalancers_names :: Lens.Lens' DescribeLoadBalancers (Prelude.Maybe [Prelude.Text])
describeLoadBalancers_names = Lens.lens (\DescribeLoadBalancers' {names} -> names) (\s@DescribeLoadBalancers' {} a -> s {names = a} :: DescribeLoadBalancers) Prelude.. Lens.mapping Lens._Coerce

-- | The maximum number of results to return with this call.
describeLoadBalancers_pageSize :: Lens.Lens' DescribeLoadBalancers (Prelude.Maybe Prelude.Natural)
describeLoadBalancers_pageSize = Lens.lens (\DescribeLoadBalancers' {pageSize} -> pageSize) (\s@DescribeLoadBalancers' {} a -> s {pageSize = a} :: DescribeLoadBalancers)

-- | The Amazon Resource Names (ARN) of the load balancers. You can specify
-- up to 20 load balancers in a single call.
describeLoadBalancers_loadBalancerArns :: Lens.Lens' DescribeLoadBalancers (Prelude.Maybe [Prelude.Text])
describeLoadBalancers_loadBalancerArns = Lens.lens (\DescribeLoadBalancers' {loadBalancerArns} -> loadBalancerArns) (\s@DescribeLoadBalancers' {} a -> s {loadBalancerArns = a} :: DescribeLoadBalancers) Prelude.. Lens.mapping Lens._Coerce

-- | The marker for the next set of results. (You received this marker from a
-- previous call.)
describeLoadBalancers_marker :: Lens.Lens' DescribeLoadBalancers (Prelude.Maybe Prelude.Text)
describeLoadBalancers_marker = Lens.lens (\DescribeLoadBalancers' {marker} -> marker) (\s@DescribeLoadBalancers' {} a -> s {marker = a} :: DescribeLoadBalancers)

instance Core.AWSPager DescribeLoadBalancers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeLoadBalancersResponse_nextMarker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeLoadBalancersResponse_loadBalancers
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeLoadBalancers_marker
          Lens..~ rs
          Lens.^? describeLoadBalancersResponse_nextMarker
            Prelude.. Lens._Just

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
            Prelude.<$> (x Core..@? "NextMarker")
            Prelude.<*> ( x Core..@? "LoadBalancers" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeLoadBalancers

instance Prelude.NFData DescribeLoadBalancers

instance Core.ToHeaders DescribeLoadBalancers where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeLoadBalancers where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeLoadBalancers where
  toQuery DescribeLoadBalancers' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeLoadBalancers" :: Prelude.ByteString),
        "Version"
          Core.=: ("2015-12-01" :: Prelude.ByteString),
        "Names"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> names),
        "PageSize" Core.=: pageSize,
        "LoadBalancerArns"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> loadBalancerArns
            ),
        "Marker" Core.=: marker
      ]

-- | /See:/ 'newDescribeLoadBalancersResponse' smart constructor.
data DescribeLoadBalancersResponse = DescribeLoadBalancersResponse'
  { -- | If there are additional results, this is the marker for the next set of
    -- results. Otherwise, this is null.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | Information about the load balancers.
    loadBalancers :: Prelude.Maybe [LoadBalancer],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLoadBalancersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextMarker', 'describeLoadBalancersResponse_nextMarker' - If there are additional results, this is the marker for the next set of
-- results. Otherwise, this is null.
--
-- 'loadBalancers', 'describeLoadBalancersResponse_loadBalancers' - Information about the load balancers.
--
-- 'httpStatus', 'describeLoadBalancersResponse_httpStatus' - The response's http status code.
newDescribeLoadBalancersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeLoadBalancersResponse
newDescribeLoadBalancersResponse pHttpStatus_ =
  DescribeLoadBalancersResponse'
    { nextMarker =
        Prelude.Nothing,
      loadBalancers = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are additional results, this is the marker for the next set of
-- results. Otherwise, this is null.
describeLoadBalancersResponse_nextMarker :: Lens.Lens' DescribeLoadBalancersResponse (Prelude.Maybe Prelude.Text)
describeLoadBalancersResponse_nextMarker = Lens.lens (\DescribeLoadBalancersResponse' {nextMarker} -> nextMarker) (\s@DescribeLoadBalancersResponse' {} a -> s {nextMarker = a} :: DescribeLoadBalancersResponse)

-- | Information about the load balancers.
describeLoadBalancersResponse_loadBalancers :: Lens.Lens' DescribeLoadBalancersResponse (Prelude.Maybe [LoadBalancer])
describeLoadBalancersResponse_loadBalancers = Lens.lens (\DescribeLoadBalancersResponse' {loadBalancers} -> loadBalancers) (\s@DescribeLoadBalancersResponse' {} a -> s {loadBalancers = a} :: DescribeLoadBalancersResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeLoadBalancersResponse_httpStatus :: Lens.Lens' DescribeLoadBalancersResponse Prelude.Int
describeLoadBalancersResponse_httpStatus = Lens.lens (\DescribeLoadBalancersResponse' {httpStatus} -> httpStatus) (\s@DescribeLoadBalancersResponse' {} a -> s {httpStatus = a} :: DescribeLoadBalancersResponse)

instance Prelude.NFData DescribeLoadBalancersResponse
