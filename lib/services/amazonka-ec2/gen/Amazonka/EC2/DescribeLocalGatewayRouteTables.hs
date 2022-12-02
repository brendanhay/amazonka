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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.EC2.DescribeLocalGatewayRouteTables
  ( -- * Creating a Request
    DescribeLocalGatewayRouteTables (..),
    newDescribeLocalGatewayRouteTables,

    -- * Request Lenses
    describeLocalGatewayRouteTables_localGatewayRouteTableIds,
    describeLocalGatewayRouteTables_nextToken,
    describeLocalGatewayRouteTables_filters,
    describeLocalGatewayRouteTables_dryRun,
    describeLocalGatewayRouteTables_maxResults,

    -- * Destructuring the Response
    DescribeLocalGatewayRouteTablesResponse (..),
    newDescribeLocalGatewayRouteTablesResponse,

    -- * Response Lenses
    describeLocalGatewayRouteTablesResponse_nextToken,
    describeLocalGatewayRouteTablesResponse_localGatewayRouteTables,
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
  { -- | The IDs of the local gateway route tables.
    localGatewayRouteTableIds :: Prelude.Maybe [Prelude.Text],
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
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
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural
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
-- 'dryRun', 'describeLocalGatewayRouteTables_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeLocalGatewayRouteTables_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
newDescribeLocalGatewayRouteTables ::
  DescribeLocalGatewayRouteTables
newDescribeLocalGatewayRouteTables =
  DescribeLocalGatewayRouteTables'
    { localGatewayRouteTableIds =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      filters = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The IDs of the local gateway route tables.
describeLocalGatewayRouteTables_localGatewayRouteTableIds :: Lens.Lens' DescribeLocalGatewayRouteTables (Prelude.Maybe [Prelude.Text])
describeLocalGatewayRouteTables_localGatewayRouteTableIds = Lens.lens (\DescribeLocalGatewayRouteTables' {localGatewayRouteTableIds} -> localGatewayRouteTableIds) (\s@DescribeLocalGatewayRouteTables' {} a -> s {localGatewayRouteTableIds = a} :: DescribeLocalGatewayRouteTables) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next page of results.
describeLocalGatewayRouteTables_nextToken :: Lens.Lens' DescribeLocalGatewayRouteTables (Prelude.Maybe Prelude.Text)
describeLocalGatewayRouteTables_nextToken = Lens.lens (\DescribeLocalGatewayRouteTables' {nextToken} -> nextToken) (\s@DescribeLocalGatewayRouteTables' {} a -> s {nextToken = a} :: DescribeLocalGatewayRouteTables)

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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeLocalGatewayRouteTablesResponse'
            Prelude.<$> (x Data..@? "nextToken")
            Prelude.<*> ( x Data..@? "localGatewayRouteTableSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
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
        `Prelude.hashWithSalt` localGatewayRouteTableIds
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` maxResults

instance
  Prelude.NFData
    DescribeLocalGatewayRouteTables
  where
  rnf DescribeLocalGatewayRouteTables' {..} =
    Prelude.rnf localGatewayRouteTableIds
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf maxResults

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
        Data.toQuery
          ( Data.toQueryList "LocalGatewayRouteTableId"
              Prelude.<$> localGatewayRouteTableIds
          ),
        "NextToken" Data.=: nextToken,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        "DryRun" Data.=: dryRun,
        "MaxResults" Data.=: maxResults
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
describeLocalGatewayRouteTablesResponse_localGatewayRouteTables = Lens.lens (\DescribeLocalGatewayRouteTablesResponse' {localGatewayRouteTables} -> localGatewayRouteTables) (\s@DescribeLocalGatewayRouteTablesResponse' {} a -> s {localGatewayRouteTables = a} :: DescribeLocalGatewayRouteTablesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeLocalGatewayRouteTablesResponse_httpStatus :: Lens.Lens' DescribeLocalGatewayRouteTablesResponse Prelude.Int
describeLocalGatewayRouteTablesResponse_httpStatus = Lens.lens (\DescribeLocalGatewayRouteTablesResponse' {httpStatus} -> httpStatus) (\s@DescribeLocalGatewayRouteTablesResponse' {} a -> s {httpStatus = a} :: DescribeLocalGatewayRouteTablesResponse)

instance
  Prelude.NFData
    DescribeLocalGatewayRouteTablesResponse
  where
  rnf DescribeLocalGatewayRouteTablesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf localGatewayRouteTables
      `Prelude.seq` Prelude.rnf httpStatus
