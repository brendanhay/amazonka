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
-- Module      : Network.AWS.EC2.DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the associations between virtual interface groups and local
-- gateway route tables.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations
  ( -- * Creating a Request
    DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations (..),
    newDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations,

    -- * Request Lenses
    describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_nextToken,
    describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_dryRun,
    describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_localGatewayRouteTableVirtualInterfaceGroupAssociationIds,
    describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_maxResults,
    describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_filters,

    -- * Destructuring the Response
    DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse (..),
    newDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse,

    -- * Response Lenses
    describeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse_nextToken,
    describeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse_localGatewayRouteTableVirtualInterfaceGroupAssociations,
    describeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations' smart constructor.
data DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations = DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The IDs of the associations.
    localGatewayRouteTableVirtualInterfaceGroupAssociationIds :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | One or more filters.
    --
    -- -   @local-gateway-id@ - The ID of a local gateway.
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
    -- -   @state@ - The state of the association.
    filters :: Prelude.Maybe [Filter]
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
-- 'nextToken', 'describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_nextToken' - The token for the next page of results.
--
-- 'dryRun', 'describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'localGatewayRouteTableVirtualInterfaceGroupAssociationIds', 'describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_localGatewayRouteTableVirtualInterfaceGroupAssociationIds' - The IDs of the associations.
--
-- 'maxResults', 'describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'filters', 'describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_filters' - One or more filters.
--
-- -   @local-gateway-id@ - The ID of a local gateway.
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
-- -   @state@ - The state of the association.
newDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations ::
  DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations
newDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations =
  DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations'
    { nextToken =
        Prelude.Nothing,
      dryRun =
        Prelude.Nothing,
      localGatewayRouteTableVirtualInterfaceGroupAssociationIds =
        Prelude.Nothing,
      maxResults =
        Prelude.Nothing,
      filters =
        Prelude.Nothing
    }

-- | The token for the next page of results.
describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_nextToken :: Lens.Lens' DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations (Prelude.Maybe Prelude.Text)
describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_nextToken = Lens.lens (\DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations' {nextToken} -> nextToken) (\s@DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations' {} a -> s {nextToken = a} :: DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_dryRun :: Lens.Lens' DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations (Prelude.Maybe Prelude.Bool)
describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_dryRun = Lens.lens (\DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations' {dryRun} -> dryRun) (\s@DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations' {} a -> s {dryRun = a} :: DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations)

-- | The IDs of the associations.
describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_localGatewayRouteTableVirtualInterfaceGroupAssociationIds :: Lens.Lens' DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations (Prelude.Maybe [Prelude.Text])
describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_localGatewayRouteTableVirtualInterfaceGroupAssociationIds = Lens.lens (\DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations' {localGatewayRouteTableVirtualInterfaceGroupAssociationIds} -> localGatewayRouteTableVirtualInterfaceGroupAssociationIds) (\s@DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations' {} a -> s {localGatewayRouteTableVirtualInterfaceGroupAssociationIds = a} :: DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations) Prelude.. Lens.mapping Lens._Coerce

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_maxResults :: Lens.Lens' DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations (Prelude.Maybe Prelude.Natural)
describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_maxResults = Lens.lens (\DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations' {maxResults} -> maxResults) (\s@DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations' {} a -> s {maxResults = a} :: DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations)

-- | One or more filters.
--
-- -   @local-gateway-id@ - The ID of a local gateway.
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
-- -   @state@ - The state of the association.
describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_filters :: Lens.Lens' DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations (Prelude.Maybe [Filter])
describeLocalGatewayRouteTableVirtualInterfaceGroupAssociations_filters = Lens.lens (\DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations' {filters} -> filters) (\s@DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations' {} a -> s {filters = a} :: DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations) Prelude.. Lens.mapping Lens._Coerce

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
      Prelude.Just Prelude.$
        rq
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
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse'
            Prelude.<$> (x Core..@? "nextToken")
              Prelude.<*> ( x
                              Core..@? "localGatewayRouteTableVirtualInterfaceGroupAssociationSet"
                              Core..!@ Prelude.mempty
                              Prelude.>>= Core.may (Core.parseXMLList "item")
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations

instance
  Prelude.NFData
    DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations

instance
  Core.ToHeaders
    DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations
  where
  toQuery
    DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations' {..} =
      Prelude.mconcat
        [ "Action"
            Core.=: ( "DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations" ::
                        Prelude.ByteString
                    ),
          "Version"
            Core.=: ("2016-11-15" :: Prelude.ByteString),
          "NextToken" Core.=: nextToken,
          "DryRun" Core.=: dryRun,
          Core.toQuery
            ( Core.toQueryList
                "LocalGatewayRouteTableVirtualInterfaceGroupAssociationId"
                Prelude.<$> localGatewayRouteTableVirtualInterfaceGroupAssociationIds
            ),
          "MaxResults" Core.=: maxResults,
          Core.toQuery
            (Core.toQueryList "Filter" Prelude.<$> filters)
        ]

-- | /See:/ 'newDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse' smart constructor.
data DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse = DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the associations.
    localGatewayRouteTableVirtualInterfaceGroupAssociations :: Prelude.Maybe [LocalGatewayRouteTableVirtualInterfaceGroupAssociation],
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
-- 'nextToken', 'describeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'localGatewayRouteTableVirtualInterfaceGroupAssociations', 'describeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse_localGatewayRouteTableVirtualInterfaceGroupAssociations' - Information about the associations.
--
-- 'httpStatus', 'describeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse_httpStatus' - The response's http status code.
newDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse
newDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse
  pHttpStatus_ =
    DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse'
      { nextToken =
          Prelude.Nothing,
        localGatewayRouteTableVirtualInterfaceGroupAssociations =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse_nextToken :: Lens.Lens' DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse (Prelude.Maybe Prelude.Text)
describeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse_nextToken = Lens.lens (\DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse' {nextToken} -> nextToken) (\s@DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse' {} a -> s {nextToken = a} :: DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse)

-- | Information about the associations.
describeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse_localGatewayRouteTableVirtualInterfaceGroupAssociations :: Lens.Lens' DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse (Prelude.Maybe [LocalGatewayRouteTableVirtualInterfaceGroupAssociation])
describeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse_localGatewayRouteTableVirtualInterfaceGroupAssociations = Lens.lens (\DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse' {localGatewayRouteTableVirtualInterfaceGroupAssociations} -> localGatewayRouteTableVirtualInterfaceGroupAssociations) (\s@DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse' {} a -> s {localGatewayRouteTableVirtualInterfaceGroupAssociations = a} :: DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse_httpStatus :: Lens.Lens' DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse Prelude.Int
describeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse_httpStatus = Lens.lens (\DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse' {httpStatus} -> httpStatus) (\s@DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse' {} a -> s {httpStatus = a} :: DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse)

instance
  Prelude.NFData
    DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse
