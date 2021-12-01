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
-- Module      : Amazonka.EC2.DescribeTransitGatewayRouteTables
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more transit gateway route tables. By default, all
-- transit gateway route tables are described. Alternatively, you can
-- filter the results.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeTransitGatewayRouteTables
  ( -- * Creating a Request
    DescribeTransitGatewayRouteTables (..),
    newDescribeTransitGatewayRouteTables,

    -- * Request Lenses
    describeTransitGatewayRouteTables_filters,
    describeTransitGatewayRouteTables_nextToken,
    describeTransitGatewayRouteTables_dryRun,
    describeTransitGatewayRouteTables_transitGatewayRouteTableIds,
    describeTransitGatewayRouteTables_maxResults,

    -- * Destructuring the Response
    DescribeTransitGatewayRouteTablesResponse (..),
    newDescribeTransitGatewayRouteTablesResponse,

    -- * Response Lenses
    describeTransitGatewayRouteTablesResponse_transitGatewayRouteTables,
    describeTransitGatewayRouteTablesResponse_nextToken,
    describeTransitGatewayRouteTablesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeTransitGatewayRouteTables' smart constructor.
data DescribeTransitGatewayRouteTables = DescribeTransitGatewayRouteTables'
  { -- | One or more filters. The possible values are:
    --
    -- -   @default-association-route-table@ - Indicates whether this is the
    --     default association route table for the transit gateway (@true@ |
    --     @false@).
    --
    -- -   @default-propagation-route-table@ - Indicates whether this is the
    --     default propagation route table for the transit gateway (@true@ |
    --     @false@).
    --
    -- -   @state@ - The state of the route table (@available@ | @deleting@ |
    --     @deleted@ | @pending@).
    --
    -- -   @transit-gateway-id@ - The ID of the transit gateway.
    --
    -- -   @transit-gateway-route-table-id@ - The ID of the transit gateway
    --     route table.
    filters :: Prelude.Maybe [Filter],
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The IDs of the transit gateway route tables.
    transitGatewayRouteTableIds :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTransitGatewayRouteTables' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'describeTransitGatewayRouteTables_filters' - One or more filters. The possible values are:
--
-- -   @default-association-route-table@ - Indicates whether this is the
--     default association route table for the transit gateway (@true@ |
--     @false@).
--
-- -   @default-propagation-route-table@ - Indicates whether this is the
--     default propagation route table for the transit gateway (@true@ |
--     @false@).
--
-- -   @state@ - The state of the route table (@available@ | @deleting@ |
--     @deleted@ | @pending@).
--
-- -   @transit-gateway-id@ - The ID of the transit gateway.
--
-- -   @transit-gateway-route-table-id@ - The ID of the transit gateway
--     route table.
--
-- 'nextToken', 'describeTransitGatewayRouteTables_nextToken' - The token for the next page of results.
--
-- 'dryRun', 'describeTransitGatewayRouteTables_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'transitGatewayRouteTableIds', 'describeTransitGatewayRouteTables_transitGatewayRouteTableIds' - The IDs of the transit gateway route tables.
--
-- 'maxResults', 'describeTransitGatewayRouteTables_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
newDescribeTransitGatewayRouteTables ::
  DescribeTransitGatewayRouteTables
newDescribeTransitGatewayRouteTables =
  DescribeTransitGatewayRouteTables'
    { filters =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      transitGatewayRouteTableIds =
        Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | One or more filters. The possible values are:
--
-- -   @default-association-route-table@ - Indicates whether this is the
--     default association route table for the transit gateway (@true@ |
--     @false@).
--
-- -   @default-propagation-route-table@ - Indicates whether this is the
--     default propagation route table for the transit gateway (@true@ |
--     @false@).
--
-- -   @state@ - The state of the route table (@available@ | @deleting@ |
--     @deleted@ | @pending@).
--
-- -   @transit-gateway-id@ - The ID of the transit gateway.
--
-- -   @transit-gateway-route-table-id@ - The ID of the transit gateway
--     route table.
describeTransitGatewayRouteTables_filters :: Lens.Lens' DescribeTransitGatewayRouteTables (Prelude.Maybe [Filter])
describeTransitGatewayRouteTables_filters = Lens.lens (\DescribeTransitGatewayRouteTables' {filters} -> filters) (\s@DescribeTransitGatewayRouteTables' {} a -> s {filters = a} :: DescribeTransitGatewayRouteTables) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next page of results.
describeTransitGatewayRouteTables_nextToken :: Lens.Lens' DescribeTransitGatewayRouteTables (Prelude.Maybe Prelude.Text)
describeTransitGatewayRouteTables_nextToken = Lens.lens (\DescribeTransitGatewayRouteTables' {nextToken} -> nextToken) (\s@DescribeTransitGatewayRouteTables' {} a -> s {nextToken = a} :: DescribeTransitGatewayRouteTables)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeTransitGatewayRouteTables_dryRun :: Lens.Lens' DescribeTransitGatewayRouteTables (Prelude.Maybe Prelude.Bool)
describeTransitGatewayRouteTables_dryRun = Lens.lens (\DescribeTransitGatewayRouteTables' {dryRun} -> dryRun) (\s@DescribeTransitGatewayRouteTables' {} a -> s {dryRun = a} :: DescribeTransitGatewayRouteTables)

-- | The IDs of the transit gateway route tables.
describeTransitGatewayRouteTables_transitGatewayRouteTableIds :: Lens.Lens' DescribeTransitGatewayRouteTables (Prelude.Maybe [Prelude.Text])
describeTransitGatewayRouteTables_transitGatewayRouteTableIds = Lens.lens (\DescribeTransitGatewayRouteTables' {transitGatewayRouteTableIds} -> transitGatewayRouteTableIds) (\s@DescribeTransitGatewayRouteTables' {} a -> s {transitGatewayRouteTableIds = a} :: DescribeTransitGatewayRouteTables) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeTransitGatewayRouteTables_maxResults :: Lens.Lens' DescribeTransitGatewayRouteTables (Prelude.Maybe Prelude.Natural)
describeTransitGatewayRouteTables_maxResults = Lens.lens (\DescribeTransitGatewayRouteTables' {maxResults} -> maxResults) (\s@DescribeTransitGatewayRouteTables' {} a -> s {maxResults = a} :: DescribeTransitGatewayRouteTables)

instance
  Core.AWSPager
    DescribeTransitGatewayRouteTables
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeTransitGatewayRouteTablesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeTransitGatewayRouteTablesResponse_transitGatewayRouteTables
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeTransitGatewayRouteTables_nextToken
          Lens..~ rs
          Lens.^? describeTransitGatewayRouteTablesResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeTransitGatewayRouteTables
  where
  type
    AWSResponse DescribeTransitGatewayRouteTables =
      DescribeTransitGatewayRouteTablesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeTransitGatewayRouteTablesResponse'
            Prelude.<$> ( x Core..@? "transitGatewayRouteTables"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
              Prelude.<*> (x Core..@? "nextToken")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeTransitGatewayRouteTables
  where
  hashWithSalt
    salt'
    DescribeTransitGatewayRouteTables' {..} =
      salt' `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` transitGatewayRouteTableIds
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` filters

instance
  Prelude.NFData
    DescribeTransitGatewayRouteTables
  where
  rnf DescribeTransitGatewayRouteTables' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf transitGatewayRouteTableIds
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf nextToken

instance
  Core.ToHeaders
    DescribeTransitGatewayRouteTables
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    DescribeTransitGatewayRouteTables
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DescribeTransitGatewayRouteTables
  where
  toQuery DescribeTransitGatewayRouteTables' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DescribeTransitGatewayRouteTables" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        Core.toQuery
          (Core.toQueryList "Filter" Prelude.<$> filters),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        Core.toQuery
          ( Core.toQueryList "TransitGatewayRouteTableIds"
              Prelude.<$> transitGatewayRouteTableIds
          ),
        "MaxResults" Core.=: maxResults
      ]

-- | /See:/ 'newDescribeTransitGatewayRouteTablesResponse' smart constructor.
data DescribeTransitGatewayRouteTablesResponse = DescribeTransitGatewayRouteTablesResponse'
  { -- | Information about the transit gateway route tables.
    transitGatewayRouteTables :: Prelude.Maybe [TransitGatewayRouteTable],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTransitGatewayRouteTablesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitGatewayRouteTables', 'describeTransitGatewayRouteTablesResponse_transitGatewayRouteTables' - Information about the transit gateway route tables.
--
-- 'nextToken', 'describeTransitGatewayRouteTablesResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'describeTransitGatewayRouteTablesResponse_httpStatus' - The response's http status code.
newDescribeTransitGatewayRouteTablesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTransitGatewayRouteTablesResponse
newDescribeTransitGatewayRouteTablesResponse
  pHttpStatus_ =
    DescribeTransitGatewayRouteTablesResponse'
      { transitGatewayRouteTables =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the transit gateway route tables.
describeTransitGatewayRouteTablesResponse_transitGatewayRouteTables :: Lens.Lens' DescribeTransitGatewayRouteTablesResponse (Prelude.Maybe [TransitGatewayRouteTable])
describeTransitGatewayRouteTablesResponse_transitGatewayRouteTables = Lens.lens (\DescribeTransitGatewayRouteTablesResponse' {transitGatewayRouteTables} -> transitGatewayRouteTables) (\s@DescribeTransitGatewayRouteTablesResponse' {} a -> s {transitGatewayRouteTables = a} :: DescribeTransitGatewayRouteTablesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeTransitGatewayRouteTablesResponse_nextToken :: Lens.Lens' DescribeTransitGatewayRouteTablesResponse (Prelude.Maybe Prelude.Text)
describeTransitGatewayRouteTablesResponse_nextToken = Lens.lens (\DescribeTransitGatewayRouteTablesResponse' {nextToken} -> nextToken) (\s@DescribeTransitGatewayRouteTablesResponse' {} a -> s {nextToken = a} :: DescribeTransitGatewayRouteTablesResponse)

-- | The response's http status code.
describeTransitGatewayRouteTablesResponse_httpStatus :: Lens.Lens' DescribeTransitGatewayRouteTablesResponse Prelude.Int
describeTransitGatewayRouteTablesResponse_httpStatus = Lens.lens (\DescribeTransitGatewayRouteTablesResponse' {httpStatus} -> httpStatus) (\s@DescribeTransitGatewayRouteTablesResponse' {} a -> s {httpStatus = a} :: DescribeTransitGatewayRouteTablesResponse)

instance
  Prelude.NFData
    DescribeTransitGatewayRouteTablesResponse
  where
  rnf DescribeTransitGatewayRouteTablesResponse' {..} =
    Prelude.rnf transitGatewayRouteTables
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf nextToken
