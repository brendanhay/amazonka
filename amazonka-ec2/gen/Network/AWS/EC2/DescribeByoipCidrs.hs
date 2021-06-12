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
-- Module      : Network.AWS.EC2.DescribeByoipCidrs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the IP address ranges that were specified in calls to
-- ProvisionByoipCidr.
--
-- To describe the address pools that were created when you provisioned the
-- address ranges, use DescribePublicIpv4Pools or DescribeIpv6Pools.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeByoipCidrs
  ( -- * Creating a Request
    DescribeByoipCidrs (..),
    newDescribeByoipCidrs,

    -- * Request Lenses
    describeByoipCidrs_nextToken,
    describeByoipCidrs_dryRun,
    describeByoipCidrs_maxResults,

    -- * Destructuring the Response
    DescribeByoipCidrsResponse (..),
    newDescribeByoipCidrsResponse,

    -- * Response Lenses
    describeByoipCidrsResponse_nextToken,
    describeByoipCidrsResponse_byoipCidrs,
    describeByoipCidrsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeByoipCidrs' smart constructor.
data DescribeByoipCidrs = DescribeByoipCidrs'
  { -- | The token for the next page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeByoipCidrs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeByoipCidrs_nextToken' - The token for the next page of results.
--
-- 'dryRun', 'describeByoipCidrs_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeByoipCidrs_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
newDescribeByoipCidrs ::
  -- | 'maxResults'
  Core.Natural ->
  DescribeByoipCidrs
newDescribeByoipCidrs pMaxResults_ =
  DescribeByoipCidrs'
    { nextToken = Core.Nothing,
      dryRun = Core.Nothing,
      maxResults = pMaxResults_
    }

-- | The token for the next page of results.
describeByoipCidrs_nextToken :: Lens.Lens' DescribeByoipCidrs (Core.Maybe Core.Text)
describeByoipCidrs_nextToken = Lens.lens (\DescribeByoipCidrs' {nextToken} -> nextToken) (\s@DescribeByoipCidrs' {} a -> s {nextToken = a} :: DescribeByoipCidrs)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeByoipCidrs_dryRun :: Lens.Lens' DescribeByoipCidrs (Core.Maybe Core.Bool)
describeByoipCidrs_dryRun = Lens.lens (\DescribeByoipCidrs' {dryRun} -> dryRun) (\s@DescribeByoipCidrs' {} a -> s {dryRun = a} :: DescribeByoipCidrs)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeByoipCidrs_maxResults :: Lens.Lens' DescribeByoipCidrs Core.Natural
describeByoipCidrs_maxResults = Lens.lens (\DescribeByoipCidrs' {maxResults} -> maxResults) (\s@DescribeByoipCidrs' {} a -> s {maxResults = a} :: DescribeByoipCidrs)

instance Core.AWSPager DescribeByoipCidrs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeByoipCidrsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeByoipCidrsResponse_byoipCidrs
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeByoipCidrs_nextToken
          Lens..~ rs
          Lens.^? describeByoipCidrsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeByoipCidrs where
  type
    AWSResponse DescribeByoipCidrs =
      DescribeByoipCidrsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeByoipCidrsResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> ( x Core..@? "byoipCidrSet" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeByoipCidrs

instance Core.NFData DescribeByoipCidrs

instance Core.ToHeaders DescribeByoipCidrs where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeByoipCidrs where
  toPath = Core.const "/"

instance Core.ToQuery DescribeByoipCidrs where
  toQuery DescribeByoipCidrs' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeByoipCidrs" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults
      ]

-- | /See:/ 'newDescribeByoipCidrsResponse' smart constructor.
data DescribeByoipCidrsResponse = DescribeByoipCidrsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about your address ranges.
    byoipCidrs :: Core.Maybe [ByoipCidr],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeByoipCidrsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeByoipCidrsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'byoipCidrs', 'describeByoipCidrsResponse_byoipCidrs' - Information about your address ranges.
--
-- 'httpStatus', 'describeByoipCidrsResponse_httpStatus' - The response's http status code.
newDescribeByoipCidrsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeByoipCidrsResponse
newDescribeByoipCidrsResponse pHttpStatus_ =
  DescribeByoipCidrsResponse'
    { nextToken =
        Core.Nothing,
      byoipCidrs = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeByoipCidrsResponse_nextToken :: Lens.Lens' DescribeByoipCidrsResponse (Core.Maybe Core.Text)
describeByoipCidrsResponse_nextToken = Lens.lens (\DescribeByoipCidrsResponse' {nextToken} -> nextToken) (\s@DescribeByoipCidrsResponse' {} a -> s {nextToken = a} :: DescribeByoipCidrsResponse)

-- | Information about your address ranges.
describeByoipCidrsResponse_byoipCidrs :: Lens.Lens' DescribeByoipCidrsResponse (Core.Maybe [ByoipCidr])
describeByoipCidrsResponse_byoipCidrs = Lens.lens (\DescribeByoipCidrsResponse' {byoipCidrs} -> byoipCidrs) (\s@DescribeByoipCidrsResponse' {} a -> s {byoipCidrs = a} :: DescribeByoipCidrsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeByoipCidrsResponse_httpStatus :: Lens.Lens' DescribeByoipCidrsResponse Core.Int
describeByoipCidrsResponse_httpStatus = Lens.lens (\DescribeByoipCidrsResponse' {httpStatus} -> httpStatus) (\s@DescribeByoipCidrsResponse' {} a -> s {httpStatus = a} :: DescribeByoipCidrsResponse)

instance Core.NFData DescribeByoipCidrsResponse
