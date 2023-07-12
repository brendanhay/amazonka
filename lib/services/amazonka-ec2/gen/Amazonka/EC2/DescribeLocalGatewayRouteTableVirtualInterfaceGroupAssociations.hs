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
-- Module      : Amazonka.EC2.DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the associations between virtual interface groups and local
-- gateway route tables.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations
  ( -- * Creating a Request
    DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations (..),
    newDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations,

    -- * Request Lenses
    describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_dryRun,
    describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_filters,
    describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_localGatewayRouteTableVirtualInterfaceGroupAssociationIds,
    describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_maxResults,
    describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_nextToken,

    -- * Destructuring the Response
    DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse (..),
    newDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse,

    -- * Response Lenses
    describeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse_localGatewayRouteTableVirtualInterfaceGroupAssociations,
    describeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse_nextToken,
    describeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations' smart constructor.
data DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations = DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations'
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
    --     the local gateway route table for the virtual interface group.
    --
    -- -   @local-gateway-route-table-id@ - The ID of the local gateway route
    --     table.
    --
    -- -   @local-gateway-route-table-virtual-interface-group-association-id@ -
    --     The ID of the association.
    --
    -- -   @local-gateway-route-table-virtual-interface-group-id@ - The ID of
    --     the virtual interface group.
    --
    -- -   @owner-id@ - The ID of the Amazon Web Services account that owns the
    --     local gateway virtual interface group association.
    --
    -- -   @state@ - The state of the association.
    filters :: Prelude.Maybe [Filter],
    -- | The IDs of the associations.
    localGatewayRouteTableVirtualInterfaceGroupAssociationIds :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_filters' - One or more filters.
--
-- -   @local-gateway-id@ - The ID of a local gateway.
--
-- -   @local-gateway-route-table-arn@ - The Amazon Resource Name (ARN) of
--     the local gateway route table for the virtual interface group.
--
-- -   @local-gateway-route-table-id@ - The ID of the local gateway route
--     table.
--
-- -   @local-gateway-route-table-virtual-interface-group-association-id@ -
--     The ID of the association.
--
-- -   @local-gateway-route-table-virtual-interface-group-id@ - The ID of
--     the virtual interface group.
--
-- -   @owner-id@ - The ID of the Amazon Web Services account that owns the
--     local gateway virtual interface group association.
--
-- -   @state@ - The state of the association.
--
-- 'localGatewayRouteTableVirtualInterfaceGroupAssociationIds', 'describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_localGatewayRouteTableVirtualInterfaceGroupAssociationIds' - The IDs of the associations.
--
-- 'maxResults', 'describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'nextToken', 'describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_nextToken' - The token for the next page of results.
newDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations ::
  DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations
newDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations =
  DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations'
    { dryRun =
        Prelude.Nothing,
      filters =
        Prelude.Nothing,
      localGatewayRouteTableVirtualInterfaceGroupAssociationIds =
        Prelude.Nothing,
      maxResults =
        Prelude.Nothing,
      nextToken =
        Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_dryRun :: Lens.Lens' DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations (Prelude.Maybe Prelude.Bool)
describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_dryRun = Lens.lens (\DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations' {dryRun} -> dryRun) (\s@DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations' {} a -> s {dryRun = a} :: DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations)

-- | One or more filters.
--
-- -   @local-gateway-id@ - The ID of a local gateway.
--
-- -   @local-gateway-route-table-arn@ - The Amazon Resource Name (ARN) of
--     the local gateway route table for the virtual interface group.
--
-- -   @local-gateway-route-table-id@ - The ID of the local gateway route
--     table.
--
-- -   @local-gateway-route-table-virtual-interface-group-association-id@ -
--     The ID of the association.
--
-- -   @local-gateway-route-table-virtual-interface-group-id@ - The ID of
--     the virtual interface group.
--
-- -   @owner-id@ - The ID of the Amazon Web Services account that owns the
--     local gateway virtual interface group association.
--
-- -   @state@ - The state of the association.
describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_filters :: Lens.Lens' DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations (Prelude.Maybe [Filter])
describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_filters = Lens.lens (\DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations' {filters} -> filters) (\s@DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations' {} a -> s {filters = a} :: DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations) Prelude.. Lens.mapping Lens.coerced

-- | The IDs of the associations.
describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_localGatewayRouteTableVirtualInterfaceGroupAssociationIds :: Lens.Lens' DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations (Prelude.Maybe [Prelude.Text])
describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_localGatewayRouteTableVirtualInterfaceGroupAssociationIds = Lens.lens (\DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations' {localGatewayRouteTableVirtualInterfaceGroupAssociationIds} -> localGatewayRouteTableVirtualInterfaceGroupAssociationIds) (\s@DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations' {} a -> s {localGatewayRouteTableVirtualInterfaceGroupAssociationIds = a} :: DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_maxResults :: Lens.Lens' DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations (Prelude.Maybe Prelude.Natural)
describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_maxResults = Lens.lens (\DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations' {maxResults} -> maxResults) (\s@DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations' {} a -> s {maxResults = a} :: DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations)

-- | The token for the next page of results.
describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_nextToken :: Lens.Lens' DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations (Prelude.Maybe Prelude.Text)
describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_nextToken = Lens.lens (\DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations' {nextToken} -> nextToken) (\s@DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations' {} a -> s {nextToken = a} :: DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations)

instance
  Core.AWSPager
    DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse_localGatewayRouteTableVirtualInterfaceGroupAssociations
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_nextToken
          Lens..~ rs
          Lens.^? describeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations
  where
  type
    AWSResponse
      DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations =
      DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse'
            Prelude.<$> ( x
                            Data..@? "localGatewayRouteTableVirtualInterfaceGroupAssociationSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations
  where
  hashWithSalt
    _salt
    DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations' {..} =
      _salt
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` localGatewayRouteTableVirtualInterfaceGroupAssociationIds
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken

instance
  Prelude.NFData
    DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations
  where
  rnf
    DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations' {..} =
      Prelude.rnf dryRun
        `Prelude.seq` Prelude.rnf filters
        `Prelude.seq` Prelude.rnf
          localGatewayRouteTableVirtualInterfaceGroupAssociationIds
        `Prelude.seq` Prelude.rnf maxResults
        `Prelude.seq` Prelude.rnf nextToken

instance
  Data.ToHeaders
    DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations
  where
  toQuery
    DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations' {..} =
      Prelude.mconcat
        [ "Action"
            Data.=: ( "DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations" ::
                        Prelude.ByteString
                    ),
          "Version"
            Data.=: ("2016-11-15" :: Prelude.ByteString),
          "DryRun" Data.=: dryRun,
          Data.toQuery
            (Data.toQueryList "Filter" Prelude.<$> filters),
          Data.toQuery
            ( Data.toQueryList
                "LocalGatewayRouteTableVirtualInterfaceGroupAssociationId"
                Prelude.<$> localGatewayRouteTableVirtualInterfaceGroupAssociationIds
            ),
          "MaxResults" Data.=: maxResults,
          "NextToken" Data.=: nextToken
        ]

-- | /See:/ 'newDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse' smart constructor.
data DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse = DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse'
  { -- | Information about the associations.
    localGatewayRouteTableVirtualInterfaceGroupAssociations :: Prelude.Maybe [LocalGatewayRouteTableVirtualInterfaceGroupAssociation],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'localGatewayRouteTableVirtualInterfaceGroupAssociations', 'describeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse_localGatewayRouteTableVirtualInterfaceGroupAssociations' - Information about the associations.
--
-- 'nextToken', 'describeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'describeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse_httpStatus' - The response's http status code.
newDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse
newDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse
  pHttpStatus_ =
    DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse'
      { localGatewayRouteTableVirtualInterfaceGroupAssociations =
          Prelude.Nothing,
        nextToken =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Information about the associations.
describeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse_localGatewayRouteTableVirtualInterfaceGroupAssociations :: Lens.Lens' DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse (Prelude.Maybe [LocalGatewayRouteTableVirtualInterfaceGroupAssociation])
describeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse_localGatewayRouteTableVirtualInterfaceGroupAssociations = Lens.lens (\DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse' {localGatewayRouteTableVirtualInterfaceGroupAssociations} -> localGatewayRouteTableVirtualInterfaceGroupAssociations) (\s@DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse' {} a -> s {localGatewayRouteTableVirtualInterfaceGroupAssociations = a} :: DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse_nextToken :: Lens.Lens' DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse (Prelude.Maybe Prelude.Text)
describeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse_nextToken = Lens.lens (\DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse' {nextToken} -> nextToken) (\s@DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse' {} a -> s {nextToken = a} :: DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse)

-- | The response's http status code.
describeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse_httpStatus :: Lens.Lens' DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse Prelude.Int
describeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse_httpStatus = Lens.lens (\DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse' {httpStatus} -> httpStatus) (\s@DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse' {} a -> s {httpStatus = a} :: DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse)

instance
  Prelude.NFData
    DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse
  where
  rnf
    DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse' {..} =
      Prelude.rnf
        localGatewayRouteTableVirtualInterfaceGroupAssociations
        `Prelude.seq` Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf httpStatus
