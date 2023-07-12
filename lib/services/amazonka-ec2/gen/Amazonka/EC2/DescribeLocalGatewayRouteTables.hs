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
-- Module      : Amazonka.EC2.DescribeLocalGatewayRouteTables
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more local gateway route tables. By default, all local
-- gateway route tables are described. Alternatively, you can filter the
-- results.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeLocalGatewayRouteTables
  ( -- * Creating a Request
    DescribeLocalGatewayRouteTables (..),
    newDescribeLocalGatewayRouteTables,

    -- * Request Lenses
    describeLocalGatewayRouteTables_dryRun,
    describeLocalGatewayRouteTables_filters,
    describeLocalGatewayRouteTables_localGatewayRouteTableIds,
    describeLocalGatewayRouteTables_maxResults,
    describeLocalGatewayRouteTables_nextToken,

    -- * Destructuring the Response
    DescribeLocalGatewayRouteTablesResponse (..),
    newDescribeLocalGatewayRouteTablesResponse,

    -- * Response Lenses
    describeLocalGatewayRouteTablesResponse_localGatewayRouteTables,
    describeLocalGatewayRouteTablesResponse_nextToken,
    describeLocalGatewayRouteTablesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeLocalGatewayRouteTables' smart constructor.
data DescribeLocalGatewayRouteTables = DescribeLocalGatewayRouteTables'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | One or more filters.
    --
    -- -   @local-gateway-id@ - The ID of a local gateway.
    --
    -- -   @local-gateway-route-table-arn@ - The Amazon Resource Name (ARN) of
    --     the local gateway route table.
    --
    -- -   @local-gateway-route-table-id@ - The ID of a local gateway route
    --     table.
    --
    -- -   @outpost-arn@ - The Amazon Resource Name (ARN) of the Outpost.
    --
    -- -   @owner-id@ - The ID of the Amazon Web Services account that owns the
    --     local gateway route table.
    --
    -- -   @state@ - The state of the local gateway route table.
    filters :: Prelude.Maybe [Filter],
    -- | The IDs of the local gateway route tables.
    localGatewayRouteTableIds :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
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
-- 'dryRun', 'describeLocalGatewayRouteTables_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeLocalGatewayRouteTables_filters' - One or more filters.
--
-- -   @local-gateway-id@ - The ID of a local gateway.
--
-- -   @local-gateway-route-table-arn@ - The Amazon Resource Name (ARN) of
--     the local gateway route table.
--
-- -   @local-gateway-route-table-id@ - The ID of a local gateway route
--     table.
--
-- -   @outpost-arn@ - The Amazon Resource Name (ARN) of the Outpost.
--
-- -   @owner-id@ - The ID of the Amazon Web Services account that owns the
--     local gateway route table.
--
-- -   @state@ - The state of the local gateway route table.
--
-- 'localGatewayRouteTableIds', 'describeLocalGatewayRouteTables_localGatewayRouteTableIds' - The IDs of the local gateway route tables.
--
-- 'maxResults', 'describeLocalGatewayRouteTables_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'nextToken', 'describeLocalGatewayRouteTables_nextToken' - The token for the next page of results.
newDescribeLocalGatewayRouteTables ::
  DescribeLocalGatewayRouteTables
newDescribeLocalGatewayRouteTables =
  DescribeLocalGatewayRouteTables'
    { dryRun =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      localGatewayRouteTableIds =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeLocalGatewayRouteTables_dryRun :: Lens.Lens' DescribeLocalGatewayRouteTables (Prelude.Maybe Prelude.Bool)
describeLocalGatewayRouteTables_dryRun = Lens.lens (\DescribeLocalGatewayRouteTables' {dryRun} -> dryRun) (\s@DescribeLocalGatewayRouteTables' {} a -> s {dryRun = a} :: DescribeLocalGatewayRouteTables)

-- | One or more filters.
--
-- -   @local-gateway-id@ - The ID of a local gateway.
--
-- -   @local-gateway-route-table-arn@ - The Amazon Resource Name (ARN) of
--     the local gateway route table.
--
-- -   @local-gateway-route-table-id@ - The ID of a local gateway route
--     table.
--
-- -   @outpost-arn@ - The Amazon Resource Name (ARN) of the Outpost.
--
-- -   @owner-id@ - The ID of the Amazon Web Services account that owns the
--     local gateway route table.
--
-- -   @state@ - The state of the local gateway route table.
describeLocalGatewayRouteTables_filters :: Lens.Lens' DescribeLocalGatewayRouteTables (Prelude.Maybe [Filter])
describeLocalGatewayRouteTables_filters = Lens.lens (\DescribeLocalGatewayRouteTables' {filters} -> filters) (\s@DescribeLocalGatewayRouteTables' {} a -> s {filters = a} :: DescribeLocalGatewayRouteTables) Prelude.. Lens.mapping Lens.coerced

-- | The IDs of the local gateway route tables.
describeLocalGatewayRouteTables_localGatewayRouteTableIds :: Lens.Lens' DescribeLocalGatewayRouteTables (Prelude.Maybe [Prelude.Text])
describeLocalGatewayRouteTables_localGatewayRouteTableIds = Lens.lens (\DescribeLocalGatewayRouteTables' {localGatewayRouteTableIds} -> localGatewayRouteTableIds) (\s@DescribeLocalGatewayRouteTables' {} a -> s {localGatewayRouteTableIds = a} :: DescribeLocalGatewayRouteTables) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeLocalGatewayRouteTables_maxResults :: Lens.Lens' DescribeLocalGatewayRouteTables (Prelude.Maybe Prelude.Natural)
describeLocalGatewayRouteTables_maxResults = Lens.lens (\DescribeLocalGatewayRouteTables' {maxResults} -> maxResults) (\s@DescribeLocalGatewayRouteTables' {} a -> s {maxResults = a} :: DescribeLocalGatewayRouteTables)

-- | The token for the next page of results.
describeLocalGatewayRouteTables_nextToken :: Lens.Lens' DescribeLocalGatewayRouteTables (Prelude.Maybe Prelude.Text)
describeLocalGatewayRouteTables_nextToken = Lens.lens (\DescribeLocalGatewayRouteTables' {nextToken} -> nextToken) (\s@DescribeLocalGatewayRouteTables' {} a -> s {nextToken = a} :: DescribeLocalGatewayRouteTables)

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
        Prelude.Just
          Prelude.$ rq
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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeLocalGatewayRouteTablesResponse'
            Prelude.<$> ( x
                            Data..@? "localGatewayRouteTableSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeLocalGatewayRouteTables
  where
  hashWithSalt
    _salt
    DescribeLocalGatewayRouteTables' {..} =
      _salt
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` localGatewayRouteTableIds
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken

instance
  Prelude.NFData
    DescribeLocalGatewayRouteTables
  where
  rnf DescribeLocalGatewayRouteTables' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf localGatewayRouteTableIds
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance
  Data.ToHeaders
    DescribeLocalGatewayRouteTables
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeLocalGatewayRouteTables where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeLocalGatewayRouteTables where
  toQuery DescribeLocalGatewayRouteTables' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeLocalGatewayRouteTables" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        Data.toQuery
          ( Data.toQueryList "LocalGatewayRouteTableId"
              Prelude.<$> localGatewayRouteTableIds
          ),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newDescribeLocalGatewayRouteTablesResponse' smart constructor.
data DescribeLocalGatewayRouteTablesResponse = DescribeLocalGatewayRouteTablesResponse'
  { -- | Information about the local gateway route tables.
    localGatewayRouteTables :: Prelude.Maybe [LocalGatewayRouteTable],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'localGatewayRouteTables', 'describeLocalGatewayRouteTablesResponse_localGatewayRouteTables' - Information about the local gateway route tables.
--
-- 'nextToken', 'describeLocalGatewayRouteTablesResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'describeLocalGatewayRouteTablesResponse_httpStatus' - The response's http status code.
newDescribeLocalGatewayRouteTablesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeLocalGatewayRouteTablesResponse
newDescribeLocalGatewayRouteTablesResponse
  pHttpStatus_ =
    DescribeLocalGatewayRouteTablesResponse'
      { localGatewayRouteTables =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the local gateway route tables.
describeLocalGatewayRouteTablesResponse_localGatewayRouteTables :: Lens.Lens' DescribeLocalGatewayRouteTablesResponse (Prelude.Maybe [LocalGatewayRouteTable])
describeLocalGatewayRouteTablesResponse_localGatewayRouteTables = Lens.lens (\DescribeLocalGatewayRouteTablesResponse' {localGatewayRouteTables} -> localGatewayRouteTables) (\s@DescribeLocalGatewayRouteTablesResponse' {} a -> s {localGatewayRouteTables = a} :: DescribeLocalGatewayRouteTablesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeLocalGatewayRouteTablesResponse_nextToken :: Lens.Lens' DescribeLocalGatewayRouteTablesResponse (Prelude.Maybe Prelude.Text)
describeLocalGatewayRouteTablesResponse_nextToken = Lens.lens (\DescribeLocalGatewayRouteTablesResponse' {nextToken} -> nextToken) (\s@DescribeLocalGatewayRouteTablesResponse' {} a -> s {nextToken = a} :: DescribeLocalGatewayRouteTablesResponse)

-- | The response's http status code.
describeLocalGatewayRouteTablesResponse_httpStatus :: Lens.Lens' DescribeLocalGatewayRouteTablesResponse Prelude.Int
describeLocalGatewayRouteTablesResponse_httpStatus = Lens.lens (\DescribeLocalGatewayRouteTablesResponse' {httpStatus} -> httpStatus) (\s@DescribeLocalGatewayRouteTablesResponse' {} a -> s {httpStatus = a} :: DescribeLocalGatewayRouteTablesResponse)

instance
  Prelude.NFData
    DescribeLocalGatewayRouteTablesResponse
  where
  rnf DescribeLocalGatewayRouteTablesResponse' {..} =
    Prelude.rnf localGatewayRouteTables
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
