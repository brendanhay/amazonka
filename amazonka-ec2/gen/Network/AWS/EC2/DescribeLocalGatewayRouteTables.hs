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
-- Module      : Network.AWS.EC2.DescribeLocalGatewayRouteTables
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more local gateway route tables. By default, all local
-- gateway route tables are described. Alternatively, you can filter the
-- results.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeLocalGatewayRouteTables
  ( -- * Creating a Request
    DescribeLocalGatewayRouteTables (..),
    newDescribeLocalGatewayRouteTables,

    -- * Request Lenses
    describeLocalGatewayRouteTables_localGatewayRouteTableIds,
    describeLocalGatewayRouteTables_nextToken,
    describeLocalGatewayRouteTables_dryRun,
    describeLocalGatewayRouteTables_maxResults,
    describeLocalGatewayRouteTables_filters,

    -- * Destructuring the Response
    DescribeLocalGatewayRouteTablesResponse (..),
    newDescribeLocalGatewayRouteTablesResponse,

    -- * Response Lenses
    describeLocalGatewayRouteTablesResponse_nextToken,
    describeLocalGatewayRouteTablesResponse_localGatewayRouteTables,
    describeLocalGatewayRouteTablesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeLocalGatewayRouteTables' smart constructor.
data DescribeLocalGatewayRouteTables = DescribeLocalGatewayRouteTables'
  { -- | The IDs of the local gateway route tables.
    localGatewayRouteTableIds :: Prelude.Maybe [Prelude.Text],
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | One or more filters.
    --
    -- -   @local-gateway-id@ - The ID of a local gateway.
    --
    -- -   @local-gateway-route-table-id@ - The ID of a local gateway route
    --     table.
    --
    -- -   @outpost-arn@ - The Amazon Resource Name (ARN) of the Outpost.
    --
    -- -   @state@ - The state of the local gateway route table.
    filters :: Prelude.Maybe [Filter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLocalGatewayRouteTables' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'localGatewayRouteTableIds', 'describeLocalGatewayRouteTables_localGatewayRouteTableIds' - The IDs of the local gateway route tables.
--
-- 'nextToken', 'describeLocalGatewayRouteTables_nextToken' - The token for the next page of results.
--
-- 'dryRun', 'describeLocalGatewayRouteTables_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeLocalGatewayRouteTables_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'filters', 'describeLocalGatewayRouteTables_filters' - One or more filters.
--
-- -   @local-gateway-id@ - The ID of a local gateway.
--
-- -   @local-gateway-route-table-id@ - The ID of a local gateway route
--     table.
--
-- -   @outpost-arn@ - The Amazon Resource Name (ARN) of the Outpost.
--
-- -   @state@ - The state of the local gateway route table.
newDescribeLocalGatewayRouteTables ::
  DescribeLocalGatewayRouteTables
newDescribeLocalGatewayRouteTables =
  DescribeLocalGatewayRouteTables'
    { localGatewayRouteTableIds =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      filters = Prelude.Nothing
    }

-- | The IDs of the local gateway route tables.
describeLocalGatewayRouteTables_localGatewayRouteTableIds :: Lens.Lens' DescribeLocalGatewayRouteTables (Prelude.Maybe [Prelude.Text])
describeLocalGatewayRouteTables_localGatewayRouteTableIds = Lens.lens (\DescribeLocalGatewayRouteTables' {localGatewayRouteTableIds} -> localGatewayRouteTableIds) (\s@DescribeLocalGatewayRouteTables' {} a -> s {localGatewayRouteTableIds = a} :: DescribeLocalGatewayRouteTables) Prelude.. Lens.mapping Lens._Coerce

-- | The token for the next page of results.
describeLocalGatewayRouteTables_nextToken :: Lens.Lens' DescribeLocalGatewayRouteTables (Prelude.Maybe Prelude.Text)
describeLocalGatewayRouteTables_nextToken = Lens.lens (\DescribeLocalGatewayRouteTables' {nextToken} -> nextToken) (\s@DescribeLocalGatewayRouteTables' {} a -> s {nextToken = a} :: DescribeLocalGatewayRouteTables)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeLocalGatewayRouteTables_dryRun :: Lens.Lens' DescribeLocalGatewayRouteTables (Prelude.Maybe Prelude.Bool)
describeLocalGatewayRouteTables_dryRun = Lens.lens (\DescribeLocalGatewayRouteTables' {dryRun} -> dryRun) (\s@DescribeLocalGatewayRouteTables' {} a -> s {dryRun = a} :: DescribeLocalGatewayRouteTables)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeLocalGatewayRouteTables_maxResults :: Lens.Lens' DescribeLocalGatewayRouteTables (Prelude.Maybe Prelude.Natural)
describeLocalGatewayRouteTables_maxResults = Lens.lens (\DescribeLocalGatewayRouteTables' {maxResults} -> maxResults) (\s@DescribeLocalGatewayRouteTables' {} a -> s {maxResults = a} :: DescribeLocalGatewayRouteTables)

-- | One or more filters.
--
-- -   @local-gateway-id@ - The ID of a local gateway.
--
-- -   @local-gateway-route-table-id@ - The ID of a local gateway route
--     table.
--
-- -   @outpost-arn@ - The Amazon Resource Name (ARN) of the Outpost.
--
-- -   @state@ - The state of the local gateway route table.
describeLocalGatewayRouteTables_filters :: Lens.Lens' DescribeLocalGatewayRouteTables (Prelude.Maybe [Filter])
describeLocalGatewayRouteTables_filters = Lens.lens (\DescribeLocalGatewayRouteTables' {filters} -> filters) (\s@DescribeLocalGatewayRouteTables' {} a -> s {filters = a} :: DescribeLocalGatewayRouteTables) Prelude.. Lens.mapping Lens._Coerce

instance
  Core.AWSPager
    DescribeLocalGatewayRouteTables
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeLocalGatewayRouteTablesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeLocalGatewayRouteTablesResponse_localGatewayRouteTables
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeLocalGatewayRouteTables_nextToken
          Lens..~ rs
          Lens.^? describeLocalGatewayRouteTablesResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeLocalGatewayRouteTables
  where
  type
    AWSResponse DescribeLocalGatewayRouteTables =
      DescribeLocalGatewayRouteTablesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeLocalGatewayRouteTablesResponse'
            Prelude.<$> (x Core..@? "nextToken")
            Prelude.<*> ( x Core..@? "localGatewayRouteTableSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeLocalGatewayRouteTables

instance
  Prelude.NFData
    DescribeLocalGatewayRouteTables

instance
  Core.ToHeaders
    DescribeLocalGatewayRouteTables
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeLocalGatewayRouteTables where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeLocalGatewayRouteTables where
  toQuery DescribeLocalGatewayRouteTables' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DescribeLocalGatewayRouteTables" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        Core.toQuery
          ( Core.toQueryList "LocalGatewayRouteTableId"
              Prelude.<$> localGatewayRouteTableIds
          ),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          (Core.toQueryList "Filter" Prelude.<$> filters)
      ]

-- | /See:/ 'newDescribeLocalGatewayRouteTablesResponse' smart constructor.
data DescribeLocalGatewayRouteTablesResponse = DescribeLocalGatewayRouteTablesResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the local gateway route tables.
    localGatewayRouteTables :: Prelude.Maybe [LocalGatewayRouteTable],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLocalGatewayRouteTablesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeLocalGatewayRouteTablesResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'localGatewayRouteTables', 'describeLocalGatewayRouteTablesResponse_localGatewayRouteTables' - Information about the local gateway route tables.
--
-- 'httpStatus', 'describeLocalGatewayRouteTablesResponse_httpStatus' - The response's http status code.
newDescribeLocalGatewayRouteTablesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeLocalGatewayRouteTablesResponse
newDescribeLocalGatewayRouteTablesResponse
  pHttpStatus_ =
    DescribeLocalGatewayRouteTablesResponse'
      { nextToken =
          Prelude.Nothing,
        localGatewayRouteTables =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeLocalGatewayRouteTablesResponse_nextToken :: Lens.Lens' DescribeLocalGatewayRouteTablesResponse (Prelude.Maybe Prelude.Text)
describeLocalGatewayRouteTablesResponse_nextToken = Lens.lens (\DescribeLocalGatewayRouteTablesResponse' {nextToken} -> nextToken) (\s@DescribeLocalGatewayRouteTablesResponse' {} a -> s {nextToken = a} :: DescribeLocalGatewayRouteTablesResponse)

-- | Information about the local gateway route tables.
describeLocalGatewayRouteTablesResponse_localGatewayRouteTables :: Lens.Lens' DescribeLocalGatewayRouteTablesResponse (Prelude.Maybe [LocalGatewayRouteTable])
describeLocalGatewayRouteTablesResponse_localGatewayRouteTables = Lens.lens (\DescribeLocalGatewayRouteTablesResponse' {localGatewayRouteTables} -> localGatewayRouteTables) (\s@DescribeLocalGatewayRouteTablesResponse' {} a -> s {localGatewayRouteTables = a} :: DescribeLocalGatewayRouteTablesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeLocalGatewayRouteTablesResponse_httpStatus :: Lens.Lens' DescribeLocalGatewayRouteTablesResponse Prelude.Int
describeLocalGatewayRouteTablesResponse_httpStatus = Lens.lens (\DescribeLocalGatewayRouteTablesResponse' {httpStatus} -> httpStatus) (\s@DescribeLocalGatewayRouteTablesResponse' {} a -> s {httpStatus = a} :: DescribeLocalGatewayRouteTablesResponse)

instance
  Prelude.NFData
    DescribeLocalGatewayRouteTablesResponse
