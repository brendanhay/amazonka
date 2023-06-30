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
-- Module      : Amazonka.EC2.DescribeRouteTables
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your route tables.
--
-- Each subnet in your VPC must be associated with a route table. If a
-- subnet is not explicitly associated with any route table, it is
-- implicitly associated with the main route table. This command does not
-- return the subnet ID for implicit associations.
--
-- For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Route_Tables.html Route tables>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeRouteTables
  ( -- * Creating a Request
    DescribeRouteTables (..),
    newDescribeRouteTables,

    -- * Request Lenses
    describeRouteTables_dryRun,
    describeRouteTables_filters,
    describeRouteTables_maxResults,
    describeRouteTables_nextToken,
    describeRouteTables_routeTableIds,

    -- * Destructuring the Response
    DescribeRouteTablesResponse (..),
    newDescribeRouteTablesResponse,

    -- * Response Lenses
    describeRouteTablesResponse_nextToken,
    describeRouteTablesResponse_routeTables,
    describeRouteTablesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeRouteTables' smart constructor.
data DescribeRouteTables = DescribeRouteTables'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | One or more filters.
    --
    -- -   @association.route-table-association-id@ - The ID of an association
    --     ID for the route table.
    --
    -- -   @association.route-table-id@ - The ID of the route table involved in
    --     the association.
    --
    -- -   @association.subnet-id@ - The ID of the subnet involved in the
    --     association.
    --
    -- -   @association.main@ - Indicates whether the route table is the main
    --     route table for the VPC (@true@ | @false@). Route tables that do not
    --     have an association ID are not returned in the response.
    --
    -- -   @owner-id@ - The ID of the Amazon Web Services account that owns the
    --     route table.
    --
    -- -   @route-table-id@ - The ID of the route table.
    --
    -- -   @route.destination-cidr-block@ - The IPv4 CIDR range specified in a
    --     route in the table.
    --
    -- -   @route.destination-ipv6-cidr-block@ - The IPv6 CIDR range specified
    --     in a route in the route table.
    --
    -- -   @route.destination-prefix-list-id@ - The ID (prefix) of the Amazon
    --     Web Service specified in a route in the table.
    --
    -- -   @route.egress-only-internet-gateway-id@ - The ID of an egress-only
    --     Internet gateway specified in a route in the route table.
    --
    -- -   @route.gateway-id@ - The ID of a gateway specified in a route in the
    --     table.
    --
    -- -   @route.instance-id@ - The ID of an instance specified in a route in
    --     the table.
    --
    -- -   @route.nat-gateway-id@ - The ID of a NAT gateway.
    --
    -- -   @route.transit-gateway-id@ - The ID of a transit gateway.
    --
    -- -   @route.origin@ - Describes how the route was created.
    --     @CreateRouteTable@ indicates that the route was automatically
    --     created when the route table was created; @CreateRoute@ indicates
    --     that the route was manually added to the route table;
    --     @EnableVgwRoutePropagation@ indicates that the route was propagated
    --     by route propagation.
    --
    -- -   @route.state@ - The state of a route in the route table (@active@ |
    --     @blackhole@). The blackhole state indicates that the route\'s target
    --     isn\'t available (for example, the specified gateway isn\'t attached
    --     to the VPC, the specified NAT instance has been terminated, and so
    --     on).
    --
    -- -   @route.vpc-peering-connection-id@ - The ID of a VPC peering
    --     connection specified in a route in the table.
    --
    -- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
    --     resource. Use the tag key in the filter name and the tag value as
    --     the filter value. For example, to find all resources that have a tag
    --     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
    --     the filter name and @TeamA@ for the filter value.
    --
    -- -   @tag-key@ - The key of a tag assigned to the resource. Use this
    --     filter to find all resources assigned a tag with a specific key,
    --     regardless of the tag value.
    --
    -- -   @vpc-id@ - The ID of the VPC for the route table.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | One or more route table IDs.
    --
    -- Default: Describes all your route tables.
    routeTableIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRouteTables' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeRouteTables_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeRouteTables_filters' - One or more filters.
--
-- -   @association.route-table-association-id@ - The ID of an association
--     ID for the route table.
--
-- -   @association.route-table-id@ - The ID of the route table involved in
--     the association.
--
-- -   @association.subnet-id@ - The ID of the subnet involved in the
--     association.
--
-- -   @association.main@ - Indicates whether the route table is the main
--     route table for the VPC (@true@ | @false@). Route tables that do not
--     have an association ID are not returned in the response.
--
-- -   @owner-id@ - The ID of the Amazon Web Services account that owns the
--     route table.
--
-- -   @route-table-id@ - The ID of the route table.
--
-- -   @route.destination-cidr-block@ - The IPv4 CIDR range specified in a
--     route in the table.
--
-- -   @route.destination-ipv6-cidr-block@ - The IPv6 CIDR range specified
--     in a route in the route table.
--
-- -   @route.destination-prefix-list-id@ - The ID (prefix) of the Amazon
--     Web Service specified in a route in the table.
--
-- -   @route.egress-only-internet-gateway-id@ - The ID of an egress-only
--     Internet gateway specified in a route in the route table.
--
-- -   @route.gateway-id@ - The ID of a gateway specified in a route in the
--     table.
--
-- -   @route.instance-id@ - The ID of an instance specified in a route in
--     the table.
--
-- -   @route.nat-gateway-id@ - The ID of a NAT gateway.
--
-- -   @route.transit-gateway-id@ - The ID of a transit gateway.
--
-- -   @route.origin@ - Describes how the route was created.
--     @CreateRouteTable@ indicates that the route was automatically
--     created when the route table was created; @CreateRoute@ indicates
--     that the route was manually added to the route table;
--     @EnableVgwRoutePropagation@ indicates that the route was propagated
--     by route propagation.
--
-- -   @route.state@ - The state of a route in the route table (@active@ |
--     @blackhole@). The blackhole state indicates that the route\'s target
--     isn\'t available (for example, the specified gateway isn\'t attached
--     to the VPC, the specified NAT instance has been terminated, and so
--     on).
--
-- -   @route.vpc-peering-connection-id@ - The ID of a VPC peering
--     connection specified in a route in the table.
--
-- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
--     resource. Use the tag key in the filter name and the tag value as
--     the filter value. For example, to find all resources that have a tag
--     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
--     the filter name and @TeamA@ for the filter value.
--
-- -   @tag-key@ - The key of a tag assigned to the resource. Use this
--     filter to find all resources assigned a tag with a specific key,
--     regardless of the tag value.
--
-- -   @vpc-id@ - The ID of the VPC for the route table.
--
-- 'maxResults', 'describeRouteTables_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'nextToken', 'describeRouteTables_nextToken' - The token for the next page of results.
--
-- 'routeTableIds', 'describeRouteTables_routeTableIds' - One or more route table IDs.
--
-- Default: Describes all your route tables.
newDescribeRouteTables ::
  DescribeRouteTables
newDescribeRouteTables =
  DescribeRouteTables'
    { dryRun = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      routeTableIds = Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeRouteTables_dryRun :: Lens.Lens' DescribeRouteTables (Prelude.Maybe Prelude.Bool)
describeRouteTables_dryRun = Lens.lens (\DescribeRouteTables' {dryRun} -> dryRun) (\s@DescribeRouteTables' {} a -> s {dryRun = a} :: DescribeRouteTables)

-- | One or more filters.
--
-- -   @association.route-table-association-id@ - The ID of an association
--     ID for the route table.
--
-- -   @association.route-table-id@ - The ID of the route table involved in
--     the association.
--
-- -   @association.subnet-id@ - The ID of the subnet involved in the
--     association.
--
-- -   @association.main@ - Indicates whether the route table is the main
--     route table for the VPC (@true@ | @false@). Route tables that do not
--     have an association ID are not returned in the response.
--
-- -   @owner-id@ - The ID of the Amazon Web Services account that owns the
--     route table.
--
-- -   @route-table-id@ - The ID of the route table.
--
-- -   @route.destination-cidr-block@ - The IPv4 CIDR range specified in a
--     route in the table.
--
-- -   @route.destination-ipv6-cidr-block@ - The IPv6 CIDR range specified
--     in a route in the route table.
--
-- -   @route.destination-prefix-list-id@ - The ID (prefix) of the Amazon
--     Web Service specified in a route in the table.
--
-- -   @route.egress-only-internet-gateway-id@ - The ID of an egress-only
--     Internet gateway specified in a route in the route table.
--
-- -   @route.gateway-id@ - The ID of a gateway specified in a route in the
--     table.
--
-- -   @route.instance-id@ - The ID of an instance specified in a route in
--     the table.
--
-- -   @route.nat-gateway-id@ - The ID of a NAT gateway.
--
-- -   @route.transit-gateway-id@ - The ID of a transit gateway.
--
-- -   @route.origin@ - Describes how the route was created.
--     @CreateRouteTable@ indicates that the route was automatically
--     created when the route table was created; @CreateRoute@ indicates
--     that the route was manually added to the route table;
--     @EnableVgwRoutePropagation@ indicates that the route was propagated
--     by route propagation.
--
-- -   @route.state@ - The state of a route in the route table (@active@ |
--     @blackhole@). The blackhole state indicates that the route\'s target
--     isn\'t available (for example, the specified gateway isn\'t attached
--     to the VPC, the specified NAT instance has been terminated, and so
--     on).
--
-- -   @route.vpc-peering-connection-id@ - The ID of a VPC peering
--     connection specified in a route in the table.
--
-- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
--     resource. Use the tag key in the filter name and the tag value as
--     the filter value. For example, to find all resources that have a tag
--     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
--     the filter name and @TeamA@ for the filter value.
--
-- -   @tag-key@ - The key of a tag assigned to the resource. Use this
--     filter to find all resources assigned a tag with a specific key,
--     regardless of the tag value.
--
-- -   @vpc-id@ - The ID of the VPC for the route table.
describeRouteTables_filters :: Lens.Lens' DescribeRouteTables (Prelude.Maybe [Filter])
describeRouteTables_filters = Lens.lens (\DescribeRouteTables' {filters} -> filters) (\s@DescribeRouteTables' {} a -> s {filters = a} :: DescribeRouteTables) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeRouteTables_maxResults :: Lens.Lens' DescribeRouteTables (Prelude.Maybe Prelude.Natural)
describeRouteTables_maxResults = Lens.lens (\DescribeRouteTables' {maxResults} -> maxResults) (\s@DescribeRouteTables' {} a -> s {maxResults = a} :: DescribeRouteTables)

-- | The token for the next page of results.
describeRouteTables_nextToken :: Lens.Lens' DescribeRouteTables (Prelude.Maybe Prelude.Text)
describeRouteTables_nextToken = Lens.lens (\DescribeRouteTables' {nextToken} -> nextToken) (\s@DescribeRouteTables' {} a -> s {nextToken = a} :: DescribeRouteTables)

-- | One or more route table IDs.
--
-- Default: Describes all your route tables.
describeRouteTables_routeTableIds :: Lens.Lens' DescribeRouteTables (Prelude.Maybe [Prelude.Text])
describeRouteTables_routeTableIds = Lens.lens (\DescribeRouteTables' {routeTableIds} -> routeTableIds) (\s@DescribeRouteTables' {} a -> s {routeTableIds = a} :: DescribeRouteTables) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSPager DescribeRouteTables where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeRouteTablesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeRouteTablesResponse_routeTables
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeRouteTables_nextToken
          Lens..~ rs
          Lens.^? describeRouteTablesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeRouteTables where
  type
    AWSResponse DescribeRouteTables =
      DescribeRouteTablesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeRouteTablesResponse'
            Prelude.<$> (x Data..@? "nextToken")
            Prelude.<*> ( x
                            Data..@? "routeTableSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeRouteTables where
  hashWithSalt _salt DescribeRouteTables' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` routeTableIds

instance Prelude.NFData DescribeRouteTables where
  rnf DescribeRouteTables' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf routeTableIds

instance Data.ToHeaders DescribeRouteTables where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeRouteTables where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeRouteTables where
  toQuery DescribeRouteTables' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeRouteTables" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        Data.toQuery
          ( Data.toQueryList "RouteTableId"
              Prelude.<$> routeTableIds
          )
      ]

-- | Contains the output of DescribeRouteTables.
--
-- /See:/ 'newDescribeRouteTablesResponse' smart constructor.
data DescribeRouteTablesResponse = DescribeRouteTablesResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about one or more route tables.
    routeTables :: Prelude.Maybe [RouteTable],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRouteTablesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeRouteTablesResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'routeTables', 'describeRouteTablesResponse_routeTables' - Information about one or more route tables.
--
-- 'httpStatus', 'describeRouteTablesResponse_httpStatus' - The response's http status code.
newDescribeRouteTablesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeRouteTablesResponse
newDescribeRouteTablesResponse pHttpStatus_ =
  DescribeRouteTablesResponse'
    { nextToken =
        Prelude.Nothing,
      routeTables = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeRouteTablesResponse_nextToken :: Lens.Lens' DescribeRouteTablesResponse (Prelude.Maybe Prelude.Text)
describeRouteTablesResponse_nextToken = Lens.lens (\DescribeRouteTablesResponse' {nextToken} -> nextToken) (\s@DescribeRouteTablesResponse' {} a -> s {nextToken = a} :: DescribeRouteTablesResponse)

-- | Information about one or more route tables.
describeRouteTablesResponse_routeTables :: Lens.Lens' DescribeRouteTablesResponse (Prelude.Maybe [RouteTable])
describeRouteTablesResponse_routeTables = Lens.lens (\DescribeRouteTablesResponse' {routeTables} -> routeTables) (\s@DescribeRouteTablesResponse' {} a -> s {routeTables = a} :: DescribeRouteTablesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeRouteTablesResponse_httpStatus :: Lens.Lens' DescribeRouteTablesResponse Prelude.Int
describeRouteTablesResponse_httpStatus = Lens.lens (\DescribeRouteTablesResponse' {httpStatus} -> httpStatus) (\s@DescribeRouteTablesResponse' {} a -> s {httpStatus = a} :: DescribeRouteTablesResponse)

instance Prelude.NFData DescribeRouteTablesResponse where
  rnf DescribeRouteTablesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf routeTables
      `Prelude.seq` Prelude.rnf httpStatus
