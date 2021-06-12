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
-- Module      : Network.AWS.ELBv2.DescribeSSLPolicies
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified policies or all policies used for SSL
-- negotiation.
--
-- For more information, see
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/application/create-https-listener.html#describe-ssl-policies Security policies>
-- in the /Application Load Balancers Guide/ or
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/create-tls-listener.html#describe-ssl-policies Security policies>
-- in the /Network Load Balancers Guide/.
--
-- This operation returns paginated results.
module Network.AWS.ELBv2.DescribeSSLPolicies
  ( -- * Creating a Request
    DescribeSSLPolicies (..),
    newDescribeSSLPolicies,

    -- * Request Lenses
    describeSSLPolicies_names,
    describeSSLPolicies_pageSize,
    describeSSLPolicies_marker,

    -- * Destructuring the Response
    DescribeSSLPoliciesResponse (..),
    newDescribeSSLPoliciesResponse,

    -- * Response Lenses
    describeSSLPoliciesResponse_nextMarker,
    describeSSLPoliciesResponse_sslPolicies,
    describeSSLPoliciesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeSSLPolicies' smart constructor.
data DescribeSSLPolicies = DescribeSSLPolicies'
  { -- | The names of the policies.
    names :: Core.Maybe [Core.Text],
    -- | The maximum number of results to return with this call.
    pageSize :: Core.Maybe Core.Natural,
    -- | The marker for the next set of results. (You received this marker from a
    -- previous call.)
    marker :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeSSLPolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'names', 'describeSSLPolicies_names' - The names of the policies.
--
-- 'pageSize', 'describeSSLPolicies_pageSize' - The maximum number of results to return with this call.
--
-- 'marker', 'describeSSLPolicies_marker' - The marker for the next set of results. (You received this marker from a
-- previous call.)
newDescribeSSLPolicies ::
  DescribeSSLPolicies
newDescribeSSLPolicies =
  DescribeSSLPolicies'
    { names = Core.Nothing,
      pageSize = Core.Nothing,
      marker = Core.Nothing
    }

-- | The names of the policies.
describeSSLPolicies_names :: Lens.Lens' DescribeSSLPolicies (Core.Maybe [Core.Text])
describeSSLPolicies_names = Lens.lens (\DescribeSSLPolicies' {names} -> names) (\s@DescribeSSLPolicies' {} a -> s {names = a} :: DescribeSSLPolicies) Core.. Lens.mapping Lens._Coerce

-- | The maximum number of results to return with this call.
describeSSLPolicies_pageSize :: Lens.Lens' DescribeSSLPolicies (Core.Maybe Core.Natural)
describeSSLPolicies_pageSize = Lens.lens (\DescribeSSLPolicies' {pageSize} -> pageSize) (\s@DescribeSSLPolicies' {} a -> s {pageSize = a} :: DescribeSSLPolicies)

-- | The marker for the next set of results. (You received this marker from a
-- previous call.)
describeSSLPolicies_marker :: Lens.Lens' DescribeSSLPolicies (Core.Maybe Core.Text)
describeSSLPolicies_marker = Lens.lens (\DescribeSSLPolicies' {marker} -> marker) (\s@DescribeSSLPolicies' {} a -> s {marker = a} :: DescribeSSLPolicies)

instance Core.AWSPager DescribeSSLPolicies where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeSSLPoliciesResponse_nextMarker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeSSLPoliciesResponse_sslPolicies
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeSSLPolicies_marker
          Lens..~ rs
          Lens.^? describeSSLPoliciesResponse_nextMarker
            Core.. Lens._Just

instance Core.AWSRequest DescribeSSLPolicies where
  type
    AWSResponse DescribeSSLPolicies =
      DescribeSSLPoliciesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeSSLPoliciesResult"
      ( \s h x ->
          DescribeSSLPoliciesResponse'
            Core.<$> (x Core..@? "NextMarker")
            Core.<*> ( x Core..@? "SslPolicies" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeSSLPolicies

instance Core.NFData DescribeSSLPolicies

instance Core.ToHeaders DescribeSSLPolicies where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeSSLPolicies where
  toPath = Core.const "/"

instance Core.ToQuery DescribeSSLPolicies where
  toQuery DescribeSSLPolicies' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeSSLPolicies" :: Core.ByteString),
        "Version" Core.=: ("2015-12-01" :: Core.ByteString),
        "Names"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> names),
        "PageSize" Core.=: pageSize,
        "Marker" Core.=: marker
      ]

-- | /See:/ 'newDescribeSSLPoliciesResponse' smart constructor.
data DescribeSSLPoliciesResponse = DescribeSSLPoliciesResponse'
  { -- | If there are additional results, this is the marker for the next set of
    -- results. Otherwise, this is null.
    nextMarker :: Core.Maybe Core.Text,
    -- | Information about the security policies.
    sslPolicies :: Core.Maybe [SslPolicy],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeSSLPoliciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextMarker', 'describeSSLPoliciesResponse_nextMarker' - If there are additional results, this is the marker for the next set of
-- results. Otherwise, this is null.
--
-- 'sslPolicies', 'describeSSLPoliciesResponse_sslPolicies' - Information about the security policies.
--
-- 'httpStatus', 'describeSSLPoliciesResponse_httpStatus' - The response's http status code.
newDescribeSSLPoliciesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeSSLPoliciesResponse
newDescribeSSLPoliciesResponse pHttpStatus_ =
  DescribeSSLPoliciesResponse'
    { nextMarker =
        Core.Nothing,
      sslPolicies = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are additional results, this is the marker for the next set of
-- results. Otherwise, this is null.
describeSSLPoliciesResponse_nextMarker :: Lens.Lens' DescribeSSLPoliciesResponse (Core.Maybe Core.Text)
describeSSLPoliciesResponse_nextMarker = Lens.lens (\DescribeSSLPoliciesResponse' {nextMarker} -> nextMarker) (\s@DescribeSSLPoliciesResponse' {} a -> s {nextMarker = a} :: DescribeSSLPoliciesResponse)

-- | Information about the security policies.
describeSSLPoliciesResponse_sslPolicies :: Lens.Lens' DescribeSSLPoliciesResponse (Core.Maybe [SslPolicy])
describeSSLPoliciesResponse_sslPolicies = Lens.lens (\DescribeSSLPoliciesResponse' {sslPolicies} -> sslPolicies) (\s@DescribeSSLPoliciesResponse' {} a -> s {sslPolicies = a} :: DescribeSSLPoliciesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeSSLPoliciesResponse_httpStatus :: Lens.Lens' DescribeSSLPoliciesResponse Core.Int
describeSSLPoliciesResponse_httpStatus = Lens.lens (\DescribeSSLPoliciesResponse' {httpStatus} -> httpStatus) (\s@DescribeSSLPoliciesResponse' {} a -> s {httpStatus = a} :: DescribeSSLPoliciesResponse)

instance Core.NFData DescribeSSLPoliciesResponse
