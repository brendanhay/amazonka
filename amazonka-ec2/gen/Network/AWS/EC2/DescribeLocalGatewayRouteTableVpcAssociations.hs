{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.DescribeLocalGatewayRouteTableVpcAssociations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified associations between VPCs and local gateway
-- route tables.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeLocalGatewayRouteTableVpcAssociations
  ( -- * Creating a Request
    DescribeLocalGatewayRouteTableVpcAssociations (..),
    newDescribeLocalGatewayRouteTableVpcAssociations,

    -- * Request Lenses
    describeLocalGatewayRouteTableVpcAssociations_nextToken,
    describeLocalGatewayRouteTableVpcAssociations_dryRun,
    describeLocalGatewayRouteTableVpcAssociations_maxResults,
    describeLocalGatewayRouteTableVpcAssociations_filters,
    describeLocalGatewayRouteTableVpcAssociations_localGatewayRouteTableVpcAssociationIds,

    -- * Destructuring the Response
    DescribeLocalGatewayRouteTableVpcAssociationsResponse (..),
    newDescribeLocalGatewayRouteTableVpcAssociationsResponse,

    -- * Response Lenses
    describeLocalGatewayRouteTableVpcAssociationsResponse_nextToken,
    describeLocalGatewayRouteTableVpcAssociationsResponse_localGatewayRouteTableVpcAssociations,
    describeLocalGatewayRouteTableVpcAssociationsResponse_httpStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeLocalGatewayRouteTableVpcAssociations' smart constructor.
data DescribeLocalGatewayRouteTableVpcAssociations = DescribeLocalGatewayRouteTableVpcAssociations'
  { -- | The token for the next page of results.
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
    -- -   @local-gateway-route-table-id@ - The ID of the local gateway route
    --     table.
    --
    -- -   @local-gateway-route-table-vpc-association-id@ - The ID of the
    --     association.
    --
    -- -   @state@ - The state of the association.
    --
    -- -   @vpc-id@ - The ID of the VPC.
    filters :: Prelude.Maybe [Filter],
    -- | The IDs of the associations.
    localGatewayRouteTableVpcAssociationIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeLocalGatewayRouteTableVpcAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeLocalGatewayRouteTableVpcAssociations_nextToken' - The token for the next page of results.
--
-- 'dryRun', 'describeLocalGatewayRouteTableVpcAssociations_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeLocalGatewayRouteTableVpcAssociations_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'filters', 'describeLocalGatewayRouteTableVpcAssociations_filters' - One or more filters.
--
-- -   @local-gateway-id@ - The ID of a local gateway.
--
-- -   @local-gateway-route-table-id@ - The ID of the local gateway route
--     table.
--
-- -   @local-gateway-route-table-vpc-association-id@ - The ID of the
--     association.
--
-- -   @state@ - The state of the association.
--
-- -   @vpc-id@ - The ID of the VPC.
--
-- 'localGatewayRouteTableVpcAssociationIds', 'describeLocalGatewayRouteTableVpcAssociations_localGatewayRouteTableVpcAssociationIds' - The IDs of the associations.
newDescribeLocalGatewayRouteTableVpcAssociations ::
  DescribeLocalGatewayRouteTableVpcAssociations
newDescribeLocalGatewayRouteTableVpcAssociations =
  DescribeLocalGatewayRouteTableVpcAssociations'
    { nextToken =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      filters = Prelude.Nothing,
      localGatewayRouteTableVpcAssociationIds =
        Prelude.Nothing
    }

-- | The token for the next page of results.
describeLocalGatewayRouteTableVpcAssociations_nextToken :: Lens.Lens' DescribeLocalGatewayRouteTableVpcAssociations (Prelude.Maybe Prelude.Text)
describeLocalGatewayRouteTableVpcAssociations_nextToken = Lens.lens (\DescribeLocalGatewayRouteTableVpcAssociations' {nextToken} -> nextToken) (\s@DescribeLocalGatewayRouteTableVpcAssociations' {} a -> s {nextToken = a} :: DescribeLocalGatewayRouteTableVpcAssociations)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeLocalGatewayRouteTableVpcAssociations_dryRun :: Lens.Lens' DescribeLocalGatewayRouteTableVpcAssociations (Prelude.Maybe Prelude.Bool)
describeLocalGatewayRouteTableVpcAssociations_dryRun = Lens.lens (\DescribeLocalGatewayRouteTableVpcAssociations' {dryRun} -> dryRun) (\s@DescribeLocalGatewayRouteTableVpcAssociations' {} a -> s {dryRun = a} :: DescribeLocalGatewayRouteTableVpcAssociations)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeLocalGatewayRouteTableVpcAssociations_maxResults :: Lens.Lens' DescribeLocalGatewayRouteTableVpcAssociations (Prelude.Maybe Prelude.Natural)
describeLocalGatewayRouteTableVpcAssociations_maxResults = Lens.lens (\DescribeLocalGatewayRouteTableVpcAssociations' {maxResults} -> maxResults) (\s@DescribeLocalGatewayRouteTableVpcAssociations' {} a -> s {maxResults = a} :: DescribeLocalGatewayRouteTableVpcAssociations)

-- | One or more filters.
--
-- -   @local-gateway-id@ - The ID of a local gateway.
--
-- -   @local-gateway-route-table-id@ - The ID of the local gateway route
--     table.
--
-- -   @local-gateway-route-table-vpc-association-id@ - The ID of the
--     association.
--
-- -   @state@ - The state of the association.
--
-- -   @vpc-id@ - The ID of the VPC.
describeLocalGatewayRouteTableVpcAssociations_filters :: Lens.Lens' DescribeLocalGatewayRouteTableVpcAssociations (Prelude.Maybe [Filter])
describeLocalGatewayRouteTableVpcAssociations_filters = Lens.lens (\DescribeLocalGatewayRouteTableVpcAssociations' {filters} -> filters) (\s@DescribeLocalGatewayRouteTableVpcAssociations' {} a -> s {filters = a} :: DescribeLocalGatewayRouteTableVpcAssociations) Prelude.. Lens.mapping Prelude._Coerce

-- | The IDs of the associations.
describeLocalGatewayRouteTableVpcAssociations_localGatewayRouteTableVpcAssociationIds :: Lens.Lens' DescribeLocalGatewayRouteTableVpcAssociations (Prelude.Maybe [Prelude.Text])
describeLocalGatewayRouteTableVpcAssociations_localGatewayRouteTableVpcAssociationIds = Lens.lens (\DescribeLocalGatewayRouteTableVpcAssociations' {localGatewayRouteTableVpcAssociationIds} -> localGatewayRouteTableVpcAssociationIds) (\s@DescribeLocalGatewayRouteTableVpcAssociations' {} a -> s {localGatewayRouteTableVpcAssociationIds = a} :: DescribeLocalGatewayRouteTableVpcAssociations) Prelude.. Lens.mapping Prelude._Coerce

instance
  Pager.AWSPager
    DescribeLocalGatewayRouteTableVpcAssociations
  where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeLocalGatewayRouteTableVpcAssociationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeLocalGatewayRouteTableVpcAssociationsResponse_localGatewayRouteTableVpcAssociations
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeLocalGatewayRouteTableVpcAssociations_nextToken
          Lens..~ rs
            Lens.^? describeLocalGatewayRouteTableVpcAssociationsResponse_nextToken
              Prelude.. Lens._Just

instance
  Prelude.AWSRequest
    DescribeLocalGatewayRouteTableVpcAssociations
  where
  type
    Rs DescribeLocalGatewayRouteTableVpcAssociations =
      DescribeLocalGatewayRouteTableVpcAssociationsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeLocalGatewayRouteTableVpcAssociationsResponse'
            Prelude.<$> (x Prelude..@? "nextToken")
              Prelude.<*> ( x
                              Prelude..@? "localGatewayRouteTableVpcAssociationSet"
                              Prelude..!@ Prelude.mempty
                              Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeLocalGatewayRouteTableVpcAssociations

instance
  Prelude.NFData
    DescribeLocalGatewayRouteTableVpcAssociations

instance
  Prelude.ToHeaders
    DescribeLocalGatewayRouteTableVpcAssociations
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    DescribeLocalGatewayRouteTableVpcAssociations
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DescribeLocalGatewayRouteTableVpcAssociations
  where
  toQuery
    DescribeLocalGatewayRouteTableVpcAssociations' {..} =
      Prelude.mconcat
        [ "Action"
            Prelude.=: ( "DescribeLocalGatewayRouteTableVpcAssociations" ::
                           Prelude.ByteString
                       ),
          "Version"
            Prelude.=: ("2016-11-15" :: Prelude.ByteString),
          "NextToken" Prelude.=: nextToken,
          "DryRun" Prelude.=: dryRun,
          "MaxResults" Prelude.=: maxResults,
          Prelude.toQuery
            (Prelude.toQueryList "Filter" Prelude.<$> filters),
          Prelude.toQuery
            ( Prelude.toQueryList
                "LocalGatewayRouteTableVpcAssociationId"
                Prelude.<$> localGatewayRouteTableVpcAssociationIds
            )
        ]

-- | /See:/ 'newDescribeLocalGatewayRouteTableVpcAssociationsResponse' smart constructor.
data DescribeLocalGatewayRouteTableVpcAssociationsResponse = DescribeLocalGatewayRouteTableVpcAssociationsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the associations.
    localGatewayRouteTableVpcAssociations :: Prelude.Maybe [LocalGatewayRouteTableVpcAssociation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeLocalGatewayRouteTableVpcAssociationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeLocalGatewayRouteTableVpcAssociationsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'localGatewayRouteTableVpcAssociations', 'describeLocalGatewayRouteTableVpcAssociationsResponse_localGatewayRouteTableVpcAssociations' - Information about the associations.
--
-- 'httpStatus', 'describeLocalGatewayRouteTableVpcAssociationsResponse_httpStatus' - The response's http status code.
newDescribeLocalGatewayRouteTableVpcAssociationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeLocalGatewayRouteTableVpcAssociationsResponse
newDescribeLocalGatewayRouteTableVpcAssociationsResponse
  pHttpStatus_ =
    DescribeLocalGatewayRouteTableVpcAssociationsResponse'
      { nextToken =
          Prelude.Nothing,
        localGatewayRouteTableVpcAssociations =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeLocalGatewayRouteTableVpcAssociationsResponse_nextToken :: Lens.Lens' DescribeLocalGatewayRouteTableVpcAssociationsResponse (Prelude.Maybe Prelude.Text)
describeLocalGatewayRouteTableVpcAssociationsResponse_nextToken = Lens.lens (\DescribeLocalGatewayRouteTableVpcAssociationsResponse' {nextToken} -> nextToken) (\s@DescribeLocalGatewayRouteTableVpcAssociationsResponse' {} a -> s {nextToken = a} :: DescribeLocalGatewayRouteTableVpcAssociationsResponse)

-- | Information about the associations.
describeLocalGatewayRouteTableVpcAssociationsResponse_localGatewayRouteTableVpcAssociations :: Lens.Lens' DescribeLocalGatewayRouteTableVpcAssociationsResponse (Prelude.Maybe [LocalGatewayRouteTableVpcAssociation])
describeLocalGatewayRouteTableVpcAssociationsResponse_localGatewayRouteTableVpcAssociations = Lens.lens (\DescribeLocalGatewayRouteTableVpcAssociationsResponse' {localGatewayRouteTableVpcAssociations} -> localGatewayRouteTableVpcAssociations) (\s@DescribeLocalGatewayRouteTableVpcAssociationsResponse' {} a -> s {localGatewayRouteTableVpcAssociations = a} :: DescribeLocalGatewayRouteTableVpcAssociationsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeLocalGatewayRouteTableVpcAssociationsResponse_httpStatus :: Lens.Lens' DescribeLocalGatewayRouteTableVpcAssociationsResponse Prelude.Int
describeLocalGatewayRouteTableVpcAssociationsResponse_httpStatus = Lens.lens (\DescribeLocalGatewayRouteTableVpcAssociationsResponse' {httpStatus} -> httpStatus) (\s@DescribeLocalGatewayRouteTableVpcAssociationsResponse' {} a -> s {httpStatus = a} :: DescribeLocalGatewayRouteTableVpcAssociationsResponse)

instance
  Prelude.NFData
    DescribeLocalGatewayRouteTableVpcAssociationsResponse
