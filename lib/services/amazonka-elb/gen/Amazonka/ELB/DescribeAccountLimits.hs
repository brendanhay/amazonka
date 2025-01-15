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
-- Module      : Amazonka.ELB.DescribeAccountLimits
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the current Elastic Load Balancing resource limits for your
-- AWS account.
--
-- For more information, see
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-limits.html Limits for Your Classic Load Balancer>
-- in the /Classic Load Balancers Guide/.
--
-- This operation returns paginated results.
module Amazonka.ELB.DescribeAccountLimits
  ( -- * Creating a Request
    DescribeAccountLimits (..),
    newDescribeAccountLimits,

    -- * Request Lenses
    describeAccountLimits_marker,
    describeAccountLimits_pageSize,

    -- * Destructuring the Response
    DescribeAccountLimitsResponse (..),
    newDescribeAccountLimitsResponse,

    -- * Response Lenses
    describeAccountLimitsResponse_limits,
    describeAccountLimitsResponse_nextMarker,
    describeAccountLimitsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ELB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAccountLimits' smart constructor.
data DescribeAccountLimits = DescribeAccountLimits'
  { -- | The marker for the next set of results. (You received this marker from a
    -- previous call.)
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return with this call.
    pageSize :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAccountLimits' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeAccountLimits_marker' - The marker for the next set of results. (You received this marker from a
-- previous call.)
--
-- 'pageSize', 'describeAccountLimits_pageSize' - The maximum number of results to return with this call.
newDescribeAccountLimits ::
  DescribeAccountLimits
newDescribeAccountLimits =
  DescribeAccountLimits'
    { marker = Prelude.Nothing,
      pageSize = Prelude.Nothing
    }

-- | The marker for the next set of results. (You received this marker from a
-- previous call.)
describeAccountLimits_marker :: Lens.Lens' DescribeAccountLimits (Prelude.Maybe Prelude.Text)
describeAccountLimits_marker = Lens.lens (\DescribeAccountLimits' {marker} -> marker) (\s@DescribeAccountLimits' {} a -> s {marker = a} :: DescribeAccountLimits)

-- | The maximum number of results to return with this call.
describeAccountLimits_pageSize :: Lens.Lens' DescribeAccountLimits (Prelude.Maybe Prelude.Natural)
describeAccountLimits_pageSize = Lens.lens (\DescribeAccountLimits' {pageSize} -> pageSize) (\s@DescribeAccountLimits' {} a -> s {pageSize = a} :: DescribeAccountLimits)

instance Core.AWSPager DescribeAccountLimits where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeAccountLimitsResponse_nextMarker
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeAccountLimitsResponse_limits
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& describeAccountLimits_marker
              Lens..~ rs
              Lens.^? describeAccountLimitsResponse_nextMarker
              Prelude.. Lens._Just

instance Core.AWSRequest DescribeAccountLimits where
  type
    AWSResponse DescribeAccountLimits =
      DescribeAccountLimitsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeAccountLimitsResult"
      ( \s h x ->
          DescribeAccountLimitsResponse'
            Prelude.<$> ( x Data..@? "Limits" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (x Data..@? "NextMarker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAccountLimits where
  hashWithSalt _salt DescribeAccountLimits' {..} =
    _salt
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` pageSize

instance Prelude.NFData DescribeAccountLimits where
  rnf DescribeAccountLimits' {..} =
    Prelude.rnf marker `Prelude.seq`
      Prelude.rnf pageSize

instance Data.ToHeaders DescribeAccountLimits where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeAccountLimits where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeAccountLimits where
  toQuery DescribeAccountLimits' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeAccountLimits" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-06-01" :: Prelude.ByteString),
        "Marker" Data.=: marker,
        "PageSize" Data.=: pageSize
      ]

-- | /See:/ 'newDescribeAccountLimitsResponse' smart constructor.
data DescribeAccountLimitsResponse = DescribeAccountLimitsResponse'
  { -- | Information about the limits.
    limits :: Prelude.Maybe [Limit],
    -- | The marker to use when requesting the next set of results. If there are
    -- no additional results, the string is empty.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAccountLimitsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limits', 'describeAccountLimitsResponse_limits' - Information about the limits.
--
-- 'nextMarker', 'describeAccountLimitsResponse_nextMarker' - The marker to use when requesting the next set of results. If there are
-- no additional results, the string is empty.
--
-- 'httpStatus', 'describeAccountLimitsResponse_httpStatus' - The response's http status code.
newDescribeAccountLimitsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAccountLimitsResponse
newDescribeAccountLimitsResponse pHttpStatus_ =
  DescribeAccountLimitsResponse'
    { limits =
        Prelude.Nothing,
      nextMarker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the limits.
describeAccountLimitsResponse_limits :: Lens.Lens' DescribeAccountLimitsResponse (Prelude.Maybe [Limit])
describeAccountLimitsResponse_limits = Lens.lens (\DescribeAccountLimitsResponse' {limits} -> limits) (\s@DescribeAccountLimitsResponse' {} a -> s {limits = a} :: DescribeAccountLimitsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The marker to use when requesting the next set of results. If there are
-- no additional results, the string is empty.
describeAccountLimitsResponse_nextMarker :: Lens.Lens' DescribeAccountLimitsResponse (Prelude.Maybe Prelude.Text)
describeAccountLimitsResponse_nextMarker = Lens.lens (\DescribeAccountLimitsResponse' {nextMarker} -> nextMarker) (\s@DescribeAccountLimitsResponse' {} a -> s {nextMarker = a} :: DescribeAccountLimitsResponse)

-- | The response's http status code.
describeAccountLimitsResponse_httpStatus :: Lens.Lens' DescribeAccountLimitsResponse Prelude.Int
describeAccountLimitsResponse_httpStatus = Lens.lens (\DescribeAccountLimitsResponse' {httpStatus} -> httpStatus) (\s@DescribeAccountLimitsResponse' {} a -> s {httpStatus = a} :: DescribeAccountLimitsResponse)

instance Prelude.NFData DescribeAccountLimitsResponse where
  rnf DescribeAccountLimitsResponse' {..} =
    Prelude.rnf limits `Prelude.seq`
      Prelude.rnf nextMarker `Prelude.seq`
        Prelude.rnf httpStatus
