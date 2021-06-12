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
-- Module      : Network.AWS.EC2.DescribeVpcEndpointConnections
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the VPC endpoint connections to your VPC endpoint services,
-- including any endpoints that are pending your acceptance.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeVpcEndpointConnections
  ( -- * Creating a Request
    DescribeVpcEndpointConnections (..),
    newDescribeVpcEndpointConnections,

    -- * Request Lenses
    describeVpcEndpointConnections_nextToken,
    describeVpcEndpointConnections_dryRun,
    describeVpcEndpointConnections_maxResults,
    describeVpcEndpointConnections_filters,

    -- * Destructuring the Response
    DescribeVpcEndpointConnectionsResponse (..),
    newDescribeVpcEndpointConnectionsResponse,

    -- * Response Lenses
    describeVpcEndpointConnectionsResponse_nextToken,
    describeVpcEndpointConnectionsResponse_vpcEndpointConnections,
    describeVpcEndpointConnectionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeVpcEndpointConnections' smart constructor.
data DescribeVpcEndpointConnections = DescribeVpcEndpointConnections'
  { -- | The token to retrieve the next page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The maximum number of results to return for the request in a single
    -- page. The remaining results of the initial request can be seen by
    -- sending another request with the returned @NextToken@ value. This value
    -- can be between 5 and 1,000; if @MaxResults@ is given a value larger than
    -- 1,000, only 1,000 results are returned.
    maxResults :: Core.Maybe Core.Int,
    -- | One or more filters.
    --
    -- -   @service-id@ - The ID of the service.
    --
    -- -   @vpc-endpoint-owner@ - The AWS account number of the owner of the
    --     endpoint.
    --
    -- -   @vpc-endpoint-state@ - The state of the endpoint
    --     (@pendingAcceptance@ | @pending@ | @available@ | @deleting@ |
    --     @deleted@ | @rejected@ | @failed@).
    --
    -- -   @vpc-endpoint-id@ - The ID of the endpoint.
    filters :: Core.Maybe [Filter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeVpcEndpointConnections' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeVpcEndpointConnections_nextToken' - The token to retrieve the next page of results.
--
-- 'dryRun', 'describeVpcEndpointConnections_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeVpcEndpointConnections_maxResults' - The maximum number of results to return for the request in a single
-- page. The remaining results of the initial request can be seen by
-- sending another request with the returned @NextToken@ value. This value
-- can be between 5 and 1,000; if @MaxResults@ is given a value larger than
-- 1,000, only 1,000 results are returned.
--
-- 'filters', 'describeVpcEndpointConnections_filters' - One or more filters.
--
-- -   @service-id@ - The ID of the service.
--
-- -   @vpc-endpoint-owner@ - The AWS account number of the owner of the
--     endpoint.
--
-- -   @vpc-endpoint-state@ - The state of the endpoint
--     (@pendingAcceptance@ | @pending@ | @available@ | @deleting@ |
--     @deleted@ | @rejected@ | @failed@).
--
-- -   @vpc-endpoint-id@ - The ID of the endpoint.
newDescribeVpcEndpointConnections ::
  DescribeVpcEndpointConnections
newDescribeVpcEndpointConnections =
  DescribeVpcEndpointConnections'
    { nextToken =
        Core.Nothing,
      dryRun = Core.Nothing,
      maxResults = Core.Nothing,
      filters = Core.Nothing
    }

-- | The token to retrieve the next page of results.
describeVpcEndpointConnections_nextToken :: Lens.Lens' DescribeVpcEndpointConnections (Core.Maybe Core.Text)
describeVpcEndpointConnections_nextToken = Lens.lens (\DescribeVpcEndpointConnections' {nextToken} -> nextToken) (\s@DescribeVpcEndpointConnections' {} a -> s {nextToken = a} :: DescribeVpcEndpointConnections)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeVpcEndpointConnections_dryRun :: Lens.Lens' DescribeVpcEndpointConnections (Core.Maybe Core.Bool)
describeVpcEndpointConnections_dryRun = Lens.lens (\DescribeVpcEndpointConnections' {dryRun} -> dryRun) (\s@DescribeVpcEndpointConnections' {} a -> s {dryRun = a} :: DescribeVpcEndpointConnections)

-- | The maximum number of results to return for the request in a single
-- page. The remaining results of the initial request can be seen by
-- sending another request with the returned @NextToken@ value. This value
-- can be between 5 and 1,000; if @MaxResults@ is given a value larger than
-- 1,000, only 1,000 results are returned.
describeVpcEndpointConnections_maxResults :: Lens.Lens' DescribeVpcEndpointConnections (Core.Maybe Core.Int)
describeVpcEndpointConnections_maxResults = Lens.lens (\DescribeVpcEndpointConnections' {maxResults} -> maxResults) (\s@DescribeVpcEndpointConnections' {} a -> s {maxResults = a} :: DescribeVpcEndpointConnections)

-- | One or more filters.
--
-- -   @service-id@ - The ID of the service.
--
-- -   @vpc-endpoint-owner@ - The AWS account number of the owner of the
--     endpoint.
--
-- -   @vpc-endpoint-state@ - The state of the endpoint
--     (@pendingAcceptance@ | @pending@ | @available@ | @deleting@ |
--     @deleted@ | @rejected@ | @failed@).
--
-- -   @vpc-endpoint-id@ - The ID of the endpoint.
describeVpcEndpointConnections_filters :: Lens.Lens' DescribeVpcEndpointConnections (Core.Maybe [Filter])
describeVpcEndpointConnections_filters = Lens.lens (\DescribeVpcEndpointConnections' {filters} -> filters) (\s@DescribeVpcEndpointConnections' {} a -> s {filters = a} :: DescribeVpcEndpointConnections) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeVpcEndpointConnections where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeVpcEndpointConnectionsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeVpcEndpointConnectionsResponse_vpcEndpointConnections
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeVpcEndpointConnections_nextToken
          Lens..~ rs
          Lens.^? describeVpcEndpointConnectionsResponse_nextToken
            Core.. Lens._Just

instance
  Core.AWSRequest
    DescribeVpcEndpointConnections
  where
  type
    AWSResponse DescribeVpcEndpointConnections =
      DescribeVpcEndpointConnectionsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeVpcEndpointConnectionsResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> ( x Core..@? "vpcEndpointConnectionSet"
                         Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeVpcEndpointConnections

instance Core.NFData DescribeVpcEndpointConnections

instance
  Core.ToHeaders
    DescribeVpcEndpointConnections
  where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeVpcEndpointConnections where
  toPath = Core.const "/"

instance Core.ToQuery DescribeVpcEndpointConnections where
  toQuery DescribeVpcEndpointConnections' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "DescribeVpcEndpointConnections" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          (Core.toQueryList "Filter" Core.<$> filters)
      ]

-- | /See:/ 'newDescribeVpcEndpointConnectionsResponse' smart constructor.
data DescribeVpcEndpointConnectionsResponse = DescribeVpcEndpointConnectionsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about one or more VPC endpoint connections.
    vpcEndpointConnections :: Core.Maybe [VpcEndpointConnection],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeVpcEndpointConnectionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeVpcEndpointConnectionsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'vpcEndpointConnections', 'describeVpcEndpointConnectionsResponse_vpcEndpointConnections' - Information about one or more VPC endpoint connections.
--
-- 'httpStatus', 'describeVpcEndpointConnectionsResponse_httpStatus' - The response's http status code.
newDescribeVpcEndpointConnectionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeVpcEndpointConnectionsResponse
newDescribeVpcEndpointConnectionsResponse
  pHttpStatus_ =
    DescribeVpcEndpointConnectionsResponse'
      { nextToken =
          Core.Nothing,
        vpcEndpointConnections =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeVpcEndpointConnectionsResponse_nextToken :: Lens.Lens' DescribeVpcEndpointConnectionsResponse (Core.Maybe Core.Text)
describeVpcEndpointConnectionsResponse_nextToken = Lens.lens (\DescribeVpcEndpointConnectionsResponse' {nextToken} -> nextToken) (\s@DescribeVpcEndpointConnectionsResponse' {} a -> s {nextToken = a} :: DescribeVpcEndpointConnectionsResponse)

-- | Information about one or more VPC endpoint connections.
describeVpcEndpointConnectionsResponse_vpcEndpointConnections :: Lens.Lens' DescribeVpcEndpointConnectionsResponse (Core.Maybe [VpcEndpointConnection])
describeVpcEndpointConnectionsResponse_vpcEndpointConnections = Lens.lens (\DescribeVpcEndpointConnectionsResponse' {vpcEndpointConnections} -> vpcEndpointConnections) (\s@DescribeVpcEndpointConnectionsResponse' {} a -> s {vpcEndpointConnections = a} :: DescribeVpcEndpointConnectionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeVpcEndpointConnectionsResponse_httpStatus :: Lens.Lens' DescribeVpcEndpointConnectionsResponse Core.Int
describeVpcEndpointConnectionsResponse_httpStatus = Lens.lens (\DescribeVpcEndpointConnectionsResponse' {httpStatus} -> httpStatus) (\s@DescribeVpcEndpointConnectionsResponse' {} a -> s {httpStatus = a} :: DescribeVpcEndpointConnectionsResponse)

instance
  Core.NFData
    DescribeVpcEndpointConnectionsResponse
