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
-- Module      : Amazonka.EC2.DescribeVpcEndpointConnections
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the VPC endpoint connections to your VPC endpoint services,
-- including any endpoints that are pending your acceptance.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeVpcEndpointConnections
  ( -- * Creating a Request
    DescribeVpcEndpointConnections (..),
    newDescribeVpcEndpointConnections,

    -- * Request Lenses
    describeVpcEndpointConnections_dryRun,
    describeVpcEndpointConnections_filters,
    describeVpcEndpointConnections_maxResults,
    describeVpcEndpointConnections_nextToken,

    -- * Destructuring the Response
    DescribeVpcEndpointConnectionsResponse (..),
    newDescribeVpcEndpointConnectionsResponse,

    -- * Response Lenses
    describeVpcEndpointConnectionsResponse_nextToken,
    describeVpcEndpointConnectionsResponse_vpcEndpointConnections,
    describeVpcEndpointConnectionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeVpcEndpointConnections' smart constructor.
data DescribeVpcEndpointConnections = DescribeVpcEndpointConnections'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | One or more filters.
    --
    -- -   @ip-address-type@ - The IP address type (@ipv4@ | @ipv6@).
    --
    -- -   @service-id@ - The ID of the service.
    --
    -- -   @vpc-endpoint-owner@ - The ID of the Amazon Web Services account ID
    --     that owns the endpoint.
    --
    -- -   @vpc-endpoint-state@ - The state of the endpoint
    --     (@pendingAcceptance@ | @pending@ | @available@ | @deleting@ |
    --     @deleted@ | @rejected@ | @failed@).
    --
    -- -   @vpc-endpoint-id@ - The ID of the endpoint.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of results to return for the request in a single
    -- page. The remaining results of the initial request can be seen by
    -- sending another request with the returned @NextToken@ value. This value
    -- can be between 5 and 1,000; if @MaxResults@ is given a value larger than
    -- 1,000, only 1,000 results are returned.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The token to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVpcEndpointConnections' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeVpcEndpointConnections_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeVpcEndpointConnections_filters' - One or more filters.
--
-- -   @ip-address-type@ - The IP address type (@ipv4@ | @ipv6@).
--
-- -   @service-id@ - The ID of the service.
--
-- -   @vpc-endpoint-owner@ - The ID of the Amazon Web Services account ID
--     that owns the endpoint.
--
-- -   @vpc-endpoint-state@ - The state of the endpoint
--     (@pendingAcceptance@ | @pending@ | @available@ | @deleting@ |
--     @deleted@ | @rejected@ | @failed@).
--
-- -   @vpc-endpoint-id@ - The ID of the endpoint.
--
-- 'maxResults', 'describeVpcEndpointConnections_maxResults' - The maximum number of results to return for the request in a single
-- page. The remaining results of the initial request can be seen by
-- sending another request with the returned @NextToken@ value. This value
-- can be between 5 and 1,000; if @MaxResults@ is given a value larger than
-- 1,000, only 1,000 results are returned.
--
-- 'nextToken', 'describeVpcEndpointConnections_nextToken' - The token to retrieve the next page of results.
newDescribeVpcEndpointConnections ::
  DescribeVpcEndpointConnections
newDescribeVpcEndpointConnections =
  DescribeVpcEndpointConnections'
    { dryRun =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeVpcEndpointConnections_dryRun :: Lens.Lens' DescribeVpcEndpointConnections (Prelude.Maybe Prelude.Bool)
describeVpcEndpointConnections_dryRun = Lens.lens (\DescribeVpcEndpointConnections' {dryRun} -> dryRun) (\s@DescribeVpcEndpointConnections' {} a -> s {dryRun = a} :: DescribeVpcEndpointConnections)

-- | One or more filters.
--
-- -   @ip-address-type@ - The IP address type (@ipv4@ | @ipv6@).
--
-- -   @service-id@ - The ID of the service.
--
-- -   @vpc-endpoint-owner@ - The ID of the Amazon Web Services account ID
--     that owns the endpoint.
--
-- -   @vpc-endpoint-state@ - The state of the endpoint
--     (@pendingAcceptance@ | @pending@ | @available@ | @deleting@ |
--     @deleted@ | @rejected@ | @failed@).
--
-- -   @vpc-endpoint-id@ - The ID of the endpoint.
describeVpcEndpointConnections_filters :: Lens.Lens' DescribeVpcEndpointConnections (Prelude.Maybe [Filter])
describeVpcEndpointConnections_filters = Lens.lens (\DescribeVpcEndpointConnections' {filters} -> filters) (\s@DescribeVpcEndpointConnections' {} a -> s {filters = a} :: DescribeVpcEndpointConnections) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return for the request in a single
-- page. The remaining results of the initial request can be seen by
-- sending another request with the returned @NextToken@ value. This value
-- can be between 5 and 1,000; if @MaxResults@ is given a value larger than
-- 1,000, only 1,000 results are returned.
describeVpcEndpointConnections_maxResults :: Lens.Lens' DescribeVpcEndpointConnections (Prelude.Maybe Prelude.Int)
describeVpcEndpointConnections_maxResults = Lens.lens (\DescribeVpcEndpointConnections' {maxResults} -> maxResults) (\s@DescribeVpcEndpointConnections' {} a -> s {maxResults = a} :: DescribeVpcEndpointConnections)

-- | The token to retrieve the next page of results.
describeVpcEndpointConnections_nextToken :: Lens.Lens' DescribeVpcEndpointConnections (Prelude.Maybe Prelude.Text)
describeVpcEndpointConnections_nextToken = Lens.lens (\DescribeVpcEndpointConnections' {nextToken} -> nextToken) (\s@DescribeVpcEndpointConnections' {} a -> s {nextToken = a} :: DescribeVpcEndpointConnections)

instance Core.AWSPager DescribeVpcEndpointConnections where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeVpcEndpointConnectionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeVpcEndpointConnectionsResponse_vpcEndpointConnections
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeVpcEndpointConnections_nextToken
          Lens..~ rs
          Lens.^? describeVpcEndpointConnectionsResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeVpcEndpointConnections
  where
  type
    AWSResponse DescribeVpcEndpointConnections =
      DescribeVpcEndpointConnectionsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeVpcEndpointConnectionsResponse'
            Prelude.<$> (x Data..@? "nextToken")
            Prelude.<*> ( x
                            Data..@? "vpcEndpointConnectionSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeVpcEndpointConnections
  where
  hashWithSalt
    _salt
    DescribeVpcEndpointConnections' {..} =
      _salt
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken

instance
  Prelude.NFData
    DescribeVpcEndpointConnections
  where
  rnf DescribeVpcEndpointConnections' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance
  Data.ToHeaders
    DescribeVpcEndpointConnections
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeVpcEndpointConnections where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeVpcEndpointConnections where
  toQuery DescribeVpcEndpointConnections' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeVpcEndpointConnections" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newDescribeVpcEndpointConnectionsResponse' smart constructor.
data DescribeVpcEndpointConnectionsResponse = DescribeVpcEndpointConnectionsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about one or more VPC endpoint connections.
    vpcEndpointConnections :: Prelude.Maybe [VpcEndpointConnection],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeVpcEndpointConnectionsResponse
newDescribeVpcEndpointConnectionsResponse
  pHttpStatus_ =
    DescribeVpcEndpointConnectionsResponse'
      { nextToken =
          Prelude.Nothing,
        vpcEndpointConnections =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeVpcEndpointConnectionsResponse_nextToken :: Lens.Lens' DescribeVpcEndpointConnectionsResponse (Prelude.Maybe Prelude.Text)
describeVpcEndpointConnectionsResponse_nextToken = Lens.lens (\DescribeVpcEndpointConnectionsResponse' {nextToken} -> nextToken) (\s@DescribeVpcEndpointConnectionsResponse' {} a -> s {nextToken = a} :: DescribeVpcEndpointConnectionsResponse)

-- | Information about one or more VPC endpoint connections.
describeVpcEndpointConnectionsResponse_vpcEndpointConnections :: Lens.Lens' DescribeVpcEndpointConnectionsResponse (Prelude.Maybe [VpcEndpointConnection])
describeVpcEndpointConnectionsResponse_vpcEndpointConnections = Lens.lens (\DescribeVpcEndpointConnectionsResponse' {vpcEndpointConnections} -> vpcEndpointConnections) (\s@DescribeVpcEndpointConnectionsResponse' {} a -> s {vpcEndpointConnections = a} :: DescribeVpcEndpointConnectionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeVpcEndpointConnectionsResponse_httpStatus :: Lens.Lens' DescribeVpcEndpointConnectionsResponse Prelude.Int
describeVpcEndpointConnectionsResponse_httpStatus = Lens.lens (\DescribeVpcEndpointConnectionsResponse' {httpStatus} -> httpStatus) (\s@DescribeVpcEndpointConnectionsResponse' {} a -> s {httpStatus = a} :: DescribeVpcEndpointConnectionsResponse)

instance
  Prelude.NFData
    DescribeVpcEndpointConnectionsResponse
  where
  rnf DescribeVpcEndpointConnectionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf vpcEndpointConnections
      `Prelude.seq` Prelude.rnf httpStatus
