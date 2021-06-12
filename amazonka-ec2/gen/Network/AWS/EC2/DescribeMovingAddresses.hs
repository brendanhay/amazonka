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
-- Module      : Network.AWS.EC2.DescribeMovingAddresses
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes your Elastic IP addresses that are being moved to the EC2-VPC
-- platform, or that are being restored to the EC2-Classic platform. This
-- request does not return information about any other Elastic IP addresses
-- in your account.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeMovingAddresses
  ( -- * Creating a Request
    DescribeMovingAddresses (..),
    newDescribeMovingAddresses,

    -- * Request Lenses
    describeMovingAddresses_nextToken,
    describeMovingAddresses_dryRun,
    describeMovingAddresses_maxResults,
    describeMovingAddresses_publicIps,
    describeMovingAddresses_filters,

    -- * Destructuring the Response
    DescribeMovingAddressesResponse (..),
    newDescribeMovingAddressesResponse,

    -- * Response Lenses
    describeMovingAddressesResponse_nextToken,
    describeMovingAddressesResponse_movingAddressStatuses,
    describeMovingAddressesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeMovingAddresses' smart constructor.
data DescribeMovingAddresses = DescribeMovingAddresses'
  { -- | The token for the next page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The maximum number of results to return for the request in a single
    -- page. The remaining results of the initial request can be seen by
    -- sending another request with the returned @NextToken@ value. This value
    -- can be between 5 and 1000; if @MaxResults@ is given a value outside of
    -- this range, an error is returned.
    --
    -- Default: If no value is provided, the default is 1000.
    maxResults :: Core.Maybe Core.Natural,
    -- | One or more Elastic IP addresses.
    publicIps :: Core.Maybe [Core.Text],
    -- | One or more filters.
    --
    -- -   @moving-status@ - The status of the Elastic IP address
    --     (@MovingToVpc@ | @RestoringToClassic@).
    filters :: Core.Maybe [Filter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeMovingAddresses' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeMovingAddresses_nextToken' - The token for the next page of results.
--
-- 'dryRun', 'describeMovingAddresses_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeMovingAddresses_maxResults' - The maximum number of results to return for the request in a single
-- page. The remaining results of the initial request can be seen by
-- sending another request with the returned @NextToken@ value. This value
-- can be between 5 and 1000; if @MaxResults@ is given a value outside of
-- this range, an error is returned.
--
-- Default: If no value is provided, the default is 1000.
--
-- 'publicIps', 'describeMovingAddresses_publicIps' - One or more Elastic IP addresses.
--
-- 'filters', 'describeMovingAddresses_filters' - One or more filters.
--
-- -   @moving-status@ - The status of the Elastic IP address
--     (@MovingToVpc@ | @RestoringToClassic@).
newDescribeMovingAddresses ::
  DescribeMovingAddresses
newDescribeMovingAddresses =
  DescribeMovingAddresses'
    { nextToken = Core.Nothing,
      dryRun = Core.Nothing,
      maxResults = Core.Nothing,
      publicIps = Core.Nothing,
      filters = Core.Nothing
    }

-- | The token for the next page of results.
describeMovingAddresses_nextToken :: Lens.Lens' DescribeMovingAddresses (Core.Maybe Core.Text)
describeMovingAddresses_nextToken = Lens.lens (\DescribeMovingAddresses' {nextToken} -> nextToken) (\s@DescribeMovingAddresses' {} a -> s {nextToken = a} :: DescribeMovingAddresses)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeMovingAddresses_dryRun :: Lens.Lens' DescribeMovingAddresses (Core.Maybe Core.Bool)
describeMovingAddresses_dryRun = Lens.lens (\DescribeMovingAddresses' {dryRun} -> dryRun) (\s@DescribeMovingAddresses' {} a -> s {dryRun = a} :: DescribeMovingAddresses)

-- | The maximum number of results to return for the request in a single
-- page. The remaining results of the initial request can be seen by
-- sending another request with the returned @NextToken@ value. This value
-- can be between 5 and 1000; if @MaxResults@ is given a value outside of
-- this range, an error is returned.
--
-- Default: If no value is provided, the default is 1000.
describeMovingAddresses_maxResults :: Lens.Lens' DescribeMovingAddresses (Core.Maybe Core.Natural)
describeMovingAddresses_maxResults = Lens.lens (\DescribeMovingAddresses' {maxResults} -> maxResults) (\s@DescribeMovingAddresses' {} a -> s {maxResults = a} :: DescribeMovingAddresses)

-- | One or more Elastic IP addresses.
describeMovingAddresses_publicIps :: Lens.Lens' DescribeMovingAddresses (Core.Maybe [Core.Text])
describeMovingAddresses_publicIps = Lens.lens (\DescribeMovingAddresses' {publicIps} -> publicIps) (\s@DescribeMovingAddresses' {} a -> s {publicIps = a} :: DescribeMovingAddresses) Core.. Lens.mapping Lens._Coerce

-- | One or more filters.
--
-- -   @moving-status@ - The status of the Elastic IP address
--     (@MovingToVpc@ | @RestoringToClassic@).
describeMovingAddresses_filters :: Lens.Lens' DescribeMovingAddresses (Core.Maybe [Filter])
describeMovingAddresses_filters = Lens.lens (\DescribeMovingAddresses' {filters} -> filters) (\s@DescribeMovingAddresses' {} a -> s {filters = a} :: DescribeMovingAddresses) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeMovingAddresses where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeMovingAddressesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeMovingAddressesResponse_movingAddressStatuses
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeMovingAddresses_nextToken
          Lens..~ rs
          Lens.^? describeMovingAddressesResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeMovingAddresses where
  type
    AWSResponse DescribeMovingAddresses =
      DescribeMovingAddressesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeMovingAddressesResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> ( x Core..@? "movingAddressStatusSet"
                         Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeMovingAddresses

instance Core.NFData DescribeMovingAddresses

instance Core.ToHeaders DescribeMovingAddresses where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeMovingAddresses where
  toPath = Core.const "/"

instance Core.ToQuery DescribeMovingAddresses where
  toQuery DescribeMovingAddresses' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeMovingAddresses" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          (Core.toQueryList "PublicIp" Core.<$> publicIps),
        Core.toQuery
          (Core.toQueryList "Filter" Core.<$> filters)
      ]

-- | /See:/ 'newDescribeMovingAddressesResponse' smart constructor.
data DescribeMovingAddressesResponse = DescribeMovingAddressesResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | The status for each Elastic IP address.
    movingAddressStatuses :: Core.Maybe [MovingAddressStatus],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeMovingAddressesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeMovingAddressesResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'movingAddressStatuses', 'describeMovingAddressesResponse_movingAddressStatuses' - The status for each Elastic IP address.
--
-- 'httpStatus', 'describeMovingAddressesResponse_httpStatus' - The response's http status code.
newDescribeMovingAddressesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeMovingAddressesResponse
newDescribeMovingAddressesResponse pHttpStatus_ =
  DescribeMovingAddressesResponse'
    { nextToken =
        Core.Nothing,
      movingAddressStatuses = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeMovingAddressesResponse_nextToken :: Lens.Lens' DescribeMovingAddressesResponse (Core.Maybe Core.Text)
describeMovingAddressesResponse_nextToken = Lens.lens (\DescribeMovingAddressesResponse' {nextToken} -> nextToken) (\s@DescribeMovingAddressesResponse' {} a -> s {nextToken = a} :: DescribeMovingAddressesResponse)

-- | The status for each Elastic IP address.
describeMovingAddressesResponse_movingAddressStatuses :: Lens.Lens' DescribeMovingAddressesResponse (Core.Maybe [MovingAddressStatus])
describeMovingAddressesResponse_movingAddressStatuses = Lens.lens (\DescribeMovingAddressesResponse' {movingAddressStatuses} -> movingAddressStatuses) (\s@DescribeMovingAddressesResponse' {} a -> s {movingAddressStatuses = a} :: DescribeMovingAddressesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeMovingAddressesResponse_httpStatus :: Lens.Lens' DescribeMovingAddressesResponse Core.Int
describeMovingAddressesResponse_httpStatus = Lens.lens (\DescribeMovingAddressesResponse' {httpStatus} -> httpStatus) (\s@DescribeMovingAddressesResponse' {} a -> s {httpStatus = a} :: DescribeMovingAddressesResponse)

instance Core.NFData DescribeMovingAddressesResponse
