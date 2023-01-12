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
-- Module      : Amazonka.ELB.DescribeLoadBalancers
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified the load balancers. If no load balancers are
-- specified, the call describes all of your load balancers.
--
-- This operation returns paginated results.
module Amazonka.ELB.DescribeLoadBalancers
  ( -- * Creating a Request
    DescribeLoadBalancers (..),
    newDescribeLoadBalancers,

    -- * Request Lenses
    describeLoadBalancers_loadBalancerNames,
    describeLoadBalancers_marker,
    describeLoadBalancers_pageSize,

    -- * Destructuring the Response
    DescribeLoadBalancersResponse (..),
    newDescribeLoadBalancersResponse,

    -- * Response Lenses
    describeLoadBalancersResponse_loadBalancerDescriptions,
    describeLoadBalancersResponse_nextMarker,
    describeLoadBalancersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ELB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for DescribeLoadBalancers.
--
-- /See:/ 'newDescribeLoadBalancers' smart constructor.
data DescribeLoadBalancers = DescribeLoadBalancers'
  { -- | The names of the load balancers.
    loadBalancerNames :: Prelude.Maybe [Prelude.Text],
    -- | The marker for the next set of results. (You received this marker from a
    -- previous call.)
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return with this call (a number from 1
    -- to 400). The default is 400.
    pageSize :: Prelude.Maybe Prelude.Natural
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
-- 'loadBalancerNames', 'describeLoadBalancers_loadBalancerNames' - The names of the load balancers.
--
-- 'marker', 'describeLoadBalancers_marker' - The marker for the next set of results. (You received this marker from a
-- previous call.)
--
-- 'pageSize', 'describeLoadBalancers_pageSize' - The maximum number of results to return with this call (a number from 1
-- to 400). The default is 400.
newDescribeLoadBalancers ::
  DescribeLoadBalancers
newDescribeLoadBalancers =
  DescribeLoadBalancers'
    { loadBalancerNames =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      pageSize = Prelude.Nothing
    }

-- | The names of the load balancers.
describeLoadBalancers_loadBalancerNames :: Lens.Lens' DescribeLoadBalancers (Prelude.Maybe [Prelude.Text])
describeLoadBalancers_loadBalancerNames = Lens.lens (\DescribeLoadBalancers' {loadBalancerNames} -> loadBalancerNames) (\s@DescribeLoadBalancers' {} a -> s {loadBalancerNames = a} :: DescribeLoadBalancers) Prelude.. Lens.mapping Lens.coerced

-- | The marker for the next set of results. (You received this marker from a
-- previous call.)
describeLoadBalancers_marker :: Lens.Lens' DescribeLoadBalancers (Prelude.Maybe Prelude.Text)
describeLoadBalancers_marker = Lens.lens (\DescribeLoadBalancers' {marker} -> marker) (\s@DescribeLoadBalancers' {} a -> s {marker = a} :: DescribeLoadBalancers)

-- | The maximum number of results to return with this call (a number from 1
-- to 400). The default is 400.
describeLoadBalancers_pageSize :: Lens.Lens' DescribeLoadBalancers (Prelude.Maybe Prelude.Natural)
describeLoadBalancers_pageSize = Lens.lens (\DescribeLoadBalancers' {pageSize} -> pageSize) (\s@DescribeLoadBalancers' {} a -> s {pageSize = a} :: DescribeLoadBalancers)

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
            Lens.^? describeLoadBalancersResponse_loadBalancerDescriptions
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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeLoadBalancersResult"
      ( \s h x ->
          DescribeLoadBalancersResponse'
            Prelude.<$> ( x Data..@? "LoadBalancerDescriptions"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (x Data..@? "NextMarker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeLoadBalancers where
  hashWithSalt _salt DescribeLoadBalancers' {..} =
    _salt `Prelude.hashWithSalt` loadBalancerNames
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` pageSize

instance Prelude.NFData DescribeLoadBalancers where
  rnf DescribeLoadBalancers' {..} =
    Prelude.rnf loadBalancerNames
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf pageSize

instance Data.ToHeaders DescribeLoadBalancers where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeLoadBalancers where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeLoadBalancers where
  toQuery DescribeLoadBalancers' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeLoadBalancers" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-06-01" :: Prelude.ByteString),
        "LoadBalancerNames"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> loadBalancerNames
            ),
        "Marker" Data.=: marker,
        "PageSize" Data.=: pageSize
      ]

-- | Contains the parameters for DescribeLoadBalancers.
--
-- /See:/ 'newDescribeLoadBalancersResponse' smart constructor.
data DescribeLoadBalancersResponse = DescribeLoadBalancersResponse'
  { -- | Information about the load balancers.
    loadBalancerDescriptions :: Prelude.Maybe [LoadBalancerDescription],
    -- | The marker to use when requesting the next set of results. If there are
    -- no additional results, the string is empty.
    nextMarker :: Prelude.Maybe Prelude.Text,
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
-- 'loadBalancerDescriptions', 'describeLoadBalancersResponse_loadBalancerDescriptions' - Information about the load balancers.
--
-- 'nextMarker', 'describeLoadBalancersResponse_nextMarker' - The marker to use when requesting the next set of results. If there are
-- no additional results, the string is empty.
--
-- 'httpStatus', 'describeLoadBalancersResponse_httpStatus' - The response's http status code.
newDescribeLoadBalancersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeLoadBalancersResponse
newDescribeLoadBalancersResponse pHttpStatus_ =
  DescribeLoadBalancersResponse'
    { loadBalancerDescriptions =
        Prelude.Nothing,
      nextMarker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the load balancers.
describeLoadBalancersResponse_loadBalancerDescriptions :: Lens.Lens' DescribeLoadBalancersResponse (Prelude.Maybe [LoadBalancerDescription])
describeLoadBalancersResponse_loadBalancerDescriptions = Lens.lens (\DescribeLoadBalancersResponse' {loadBalancerDescriptions} -> loadBalancerDescriptions) (\s@DescribeLoadBalancersResponse' {} a -> s {loadBalancerDescriptions = a} :: DescribeLoadBalancersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The marker to use when requesting the next set of results. If there are
-- no additional results, the string is empty.
describeLoadBalancersResponse_nextMarker :: Lens.Lens' DescribeLoadBalancersResponse (Prelude.Maybe Prelude.Text)
describeLoadBalancersResponse_nextMarker = Lens.lens (\DescribeLoadBalancersResponse' {nextMarker} -> nextMarker) (\s@DescribeLoadBalancersResponse' {} a -> s {nextMarker = a} :: DescribeLoadBalancersResponse)

-- | The response's http status code.
describeLoadBalancersResponse_httpStatus :: Lens.Lens' DescribeLoadBalancersResponse Prelude.Int
describeLoadBalancersResponse_httpStatus = Lens.lens (\DescribeLoadBalancersResponse' {httpStatus} -> httpStatus) (\s@DescribeLoadBalancersResponse' {} a -> s {httpStatus = a} :: DescribeLoadBalancersResponse)

instance Prelude.NFData DescribeLoadBalancersResponse where
  rnf DescribeLoadBalancersResponse' {..} =
    Prelude.rnf loadBalancerDescriptions
      `Prelude.seq` Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf httpStatus
