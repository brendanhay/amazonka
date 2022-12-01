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
-- Module      : Amazonka.EC2.DescribeTransitGatewayPolicyTables
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more transit gateway route policy tables.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeTransitGatewayPolicyTables
  ( -- * Creating a Request
    DescribeTransitGatewayPolicyTables (..),
    newDescribeTransitGatewayPolicyTables,

    -- * Request Lenses
    describeTransitGatewayPolicyTables_nextToken,
    describeTransitGatewayPolicyTables_filters,
    describeTransitGatewayPolicyTables_dryRun,
    describeTransitGatewayPolicyTables_transitGatewayPolicyTableIds,
    describeTransitGatewayPolicyTables_maxResults,

    -- * Destructuring the Response
    DescribeTransitGatewayPolicyTablesResponse (..),
    newDescribeTransitGatewayPolicyTablesResponse,

    -- * Response Lenses
    describeTransitGatewayPolicyTablesResponse_nextToken,
    describeTransitGatewayPolicyTablesResponse_transitGatewayPolicyTables,
    describeTransitGatewayPolicyTablesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeTransitGatewayPolicyTables' smart constructor.
data DescribeTransitGatewayPolicyTables = DescribeTransitGatewayPolicyTables'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The filters associated with the transit gateway policy table.
    filters :: Prelude.Maybe [Filter],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The IDs of the transit gateway policy tables.
    transitGatewayPolicyTableIds :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTransitGatewayPolicyTables' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeTransitGatewayPolicyTables_nextToken' - The token for the next page of results.
--
-- 'filters', 'describeTransitGatewayPolicyTables_filters' - The filters associated with the transit gateway policy table.
--
-- 'dryRun', 'describeTransitGatewayPolicyTables_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'transitGatewayPolicyTableIds', 'describeTransitGatewayPolicyTables_transitGatewayPolicyTableIds' - The IDs of the transit gateway policy tables.
--
-- 'maxResults', 'describeTransitGatewayPolicyTables_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
newDescribeTransitGatewayPolicyTables ::
  DescribeTransitGatewayPolicyTables
newDescribeTransitGatewayPolicyTables =
  DescribeTransitGatewayPolicyTables'
    { nextToken =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      transitGatewayPolicyTableIds =
        Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token for the next page of results.
describeTransitGatewayPolicyTables_nextToken :: Lens.Lens' DescribeTransitGatewayPolicyTables (Prelude.Maybe Prelude.Text)
describeTransitGatewayPolicyTables_nextToken = Lens.lens (\DescribeTransitGatewayPolicyTables' {nextToken} -> nextToken) (\s@DescribeTransitGatewayPolicyTables' {} a -> s {nextToken = a} :: DescribeTransitGatewayPolicyTables)

-- | The filters associated with the transit gateway policy table.
describeTransitGatewayPolicyTables_filters :: Lens.Lens' DescribeTransitGatewayPolicyTables (Prelude.Maybe [Filter])
describeTransitGatewayPolicyTables_filters = Lens.lens (\DescribeTransitGatewayPolicyTables' {filters} -> filters) (\s@DescribeTransitGatewayPolicyTables' {} a -> s {filters = a} :: DescribeTransitGatewayPolicyTables) Prelude.. Lens.mapping Lens.coerced

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeTransitGatewayPolicyTables_dryRun :: Lens.Lens' DescribeTransitGatewayPolicyTables (Prelude.Maybe Prelude.Bool)
describeTransitGatewayPolicyTables_dryRun = Lens.lens (\DescribeTransitGatewayPolicyTables' {dryRun} -> dryRun) (\s@DescribeTransitGatewayPolicyTables' {} a -> s {dryRun = a} :: DescribeTransitGatewayPolicyTables)

-- | The IDs of the transit gateway policy tables.
describeTransitGatewayPolicyTables_transitGatewayPolicyTableIds :: Lens.Lens' DescribeTransitGatewayPolicyTables (Prelude.Maybe [Prelude.Text])
describeTransitGatewayPolicyTables_transitGatewayPolicyTableIds = Lens.lens (\DescribeTransitGatewayPolicyTables' {transitGatewayPolicyTableIds} -> transitGatewayPolicyTableIds) (\s@DescribeTransitGatewayPolicyTables' {} a -> s {transitGatewayPolicyTableIds = a} :: DescribeTransitGatewayPolicyTables) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeTransitGatewayPolicyTables_maxResults :: Lens.Lens' DescribeTransitGatewayPolicyTables (Prelude.Maybe Prelude.Natural)
describeTransitGatewayPolicyTables_maxResults = Lens.lens (\DescribeTransitGatewayPolicyTables' {maxResults} -> maxResults) (\s@DescribeTransitGatewayPolicyTables' {} a -> s {maxResults = a} :: DescribeTransitGatewayPolicyTables)

instance
  Core.AWSPager
    DescribeTransitGatewayPolicyTables
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeTransitGatewayPolicyTablesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeTransitGatewayPolicyTablesResponse_transitGatewayPolicyTables
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeTransitGatewayPolicyTables_nextToken
          Lens..~ rs
          Lens.^? describeTransitGatewayPolicyTablesResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeTransitGatewayPolicyTables
  where
  type
    AWSResponse DescribeTransitGatewayPolicyTables =
      DescribeTransitGatewayPolicyTablesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeTransitGatewayPolicyTablesResponse'
            Prelude.<$> (x Core..@? "nextToken")
              Prelude.<*> ( x Core..@? "transitGatewayPolicyTables"
                              Core..!@ Prelude.mempty
                              Prelude.>>= Core.may (Core.parseXMLList "item")
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeTransitGatewayPolicyTables
  where
  hashWithSalt
    _salt
    DescribeTransitGatewayPolicyTables' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` transitGatewayPolicyTableIds
        `Prelude.hashWithSalt` maxResults

instance
  Prelude.NFData
    DescribeTransitGatewayPolicyTables
  where
  rnf DescribeTransitGatewayPolicyTables' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf transitGatewayPolicyTableIds
      `Prelude.seq` Prelude.rnf maxResults

instance
  Core.ToHeaders
    DescribeTransitGatewayPolicyTables
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    DescribeTransitGatewayPolicyTables
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DescribeTransitGatewayPolicyTables
  where
  toQuery DescribeTransitGatewayPolicyTables' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DescribeTransitGatewayPolicyTables" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        Core.toQuery
          (Core.toQueryList "Filter" Prelude.<$> filters),
        "DryRun" Core.=: dryRun,
        Core.toQuery
          ( Core.toQueryList "TransitGatewayPolicyTableIds"
              Prelude.<$> transitGatewayPolicyTableIds
          ),
        "MaxResults" Core.=: maxResults
      ]

-- | /See:/ 'newDescribeTransitGatewayPolicyTablesResponse' smart constructor.
data DescribeTransitGatewayPolicyTablesResponse = DescribeTransitGatewayPolicyTablesResponse'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Describes the transit gateway policy tables.
    transitGatewayPolicyTables :: Prelude.Maybe [TransitGatewayPolicyTable],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTransitGatewayPolicyTablesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeTransitGatewayPolicyTablesResponse_nextToken' - The token for the next page of results.
--
-- 'transitGatewayPolicyTables', 'describeTransitGatewayPolicyTablesResponse_transitGatewayPolicyTables' - Describes the transit gateway policy tables.
--
-- 'httpStatus', 'describeTransitGatewayPolicyTablesResponse_httpStatus' - The response's http status code.
newDescribeTransitGatewayPolicyTablesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTransitGatewayPolicyTablesResponse
newDescribeTransitGatewayPolicyTablesResponse
  pHttpStatus_ =
    DescribeTransitGatewayPolicyTablesResponse'
      { nextToken =
          Prelude.Nothing,
        transitGatewayPolicyTables =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token for the next page of results.
describeTransitGatewayPolicyTablesResponse_nextToken :: Lens.Lens' DescribeTransitGatewayPolicyTablesResponse (Prelude.Maybe Prelude.Text)
describeTransitGatewayPolicyTablesResponse_nextToken = Lens.lens (\DescribeTransitGatewayPolicyTablesResponse' {nextToken} -> nextToken) (\s@DescribeTransitGatewayPolicyTablesResponse' {} a -> s {nextToken = a} :: DescribeTransitGatewayPolicyTablesResponse)

-- | Describes the transit gateway policy tables.
describeTransitGatewayPolicyTablesResponse_transitGatewayPolicyTables :: Lens.Lens' DescribeTransitGatewayPolicyTablesResponse (Prelude.Maybe [TransitGatewayPolicyTable])
describeTransitGatewayPolicyTablesResponse_transitGatewayPolicyTables = Lens.lens (\DescribeTransitGatewayPolicyTablesResponse' {transitGatewayPolicyTables} -> transitGatewayPolicyTables) (\s@DescribeTransitGatewayPolicyTablesResponse' {} a -> s {transitGatewayPolicyTables = a} :: DescribeTransitGatewayPolicyTablesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeTransitGatewayPolicyTablesResponse_httpStatus :: Lens.Lens' DescribeTransitGatewayPolicyTablesResponse Prelude.Int
describeTransitGatewayPolicyTablesResponse_httpStatus = Lens.lens (\DescribeTransitGatewayPolicyTablesResponse' {httpStatus} -> httpStatus) (\s@DescribeTransitGatewayPolicyTablesResponse' {} a -> s {httpStatus = a} :: DescribeTransitGatewayPolicyTablesResponse)

instance
  Prelude.NFData
    DescribeTransitGatewayPolicyTablesResponse
  where
  rnf DescribeTransitGatewayPolicyTablesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf transitGatewayPolicyTables
      `Prelude.seq` Prelude.rnf httpStatus
