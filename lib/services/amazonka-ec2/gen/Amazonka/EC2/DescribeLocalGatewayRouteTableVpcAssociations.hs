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
-- Module      : Amazonka.EC2.DescribeLocalGatewayRouteTableVpcAssociations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified associations between VPCs and local gateway
-- route tables.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeLocalGatewayRouteTableVpcAssociations
  ( -- * Creating a Request
    DescribeLocalGatewayRouteTableVpcAssociations (..),
    newDescribeLocalGatewayRouteTableVpcAssociations,

    -- * Request Lenses
    describeLocalGatewayRouteTableVpcAssociations_dryRun,
    describeLocalGatewayRouteTableVpcAssociations_filters,
    describeLocalGatewayRouteTableVpcAssociations_localGatewayRouteTableVpcAssociationIds,
    describeLocalGatewayRouteTableVpcAssociations_maxResults,
    describeLocalGatewayRouteTableVpcAssociations_nextToken,

    -- * Destructuring the Response
    DescribeLocalGatewayRouteTableVpcAssociationsResponse (..),
    newDescribeLocalGatewayRouteTableVpcAssociationsResponse,

    -- * Response Lenses
    describeLocalGatewayRouteTableVpcAssociationsResponse_localGatewayRouteTableVpcAssociations,
    describeLocalGatewayRouteTableVpcAssociationsResponse_nextToken,
    describeLocalGatewayRouteTableVpcAssociationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeLocalGatewayRouteTableVpcAssociations' smart constructor.
data DescribeLocalGatewayRouteTableVpcAssociations = DescribeLocalGatewayRouteTableVpcAssociations'
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
    --     the local gateway route table for the association.
    --
    -- -   @local-gateway-route-table-id@ - The ID of the local gateway route
    --     table.
    --
    -- -   @local-gateway-route-table-vpc-association-id@ - The ID of the
    --     association.
    --
    -- -   @owner-id@ - The ID of the Amazon Web Services account that owns the
    --     local gateway route table for the association.
    --
    -- -   @state@ - The state of the association.
    --
    -- -   @vpc-id@ - The ID of the VPC.
    filters :: Prelude.Maybe [Filter],
    -- | The IDs of the associations.
    localGatewayRouteTableVpcAssociationIds :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLocalGatewayRouteTableVpcAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeLocalGatewayRouteTableVpcAssociations_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeLocalGatewayRouteTableVpcAssociations_filters' - One or more filters.
--
-- -   @local-gateway-id@ - The ID of a local gateway.
--
-- -   @local-gateway-route-table-arn@ - The Amazon Resource Name (ARN) of
--     the local gateway route table for the association.
--
-- -   @local-gateway-route-table-id@ - The ID of the local gateway route
--     table.
--
-- -   @local-gateway-route-table-vpc-association-id@ - The ID of the
--     association.
--
-- -   @owner-id@ - The ID of the Amazon Web Services account that owns the
--     local gateway route table for the association.
--
-- -   @state@ - The state of the association.
--
-- -   @vpc-id@ - The ID of the VPC.
--
-- 'localGatewayRouteTableVpcAssociationIds', 'describeLocalGatewayRouteTableVpcAssociations_localGatewayRouteTableVpcAssociationIds' - The IDs of the associations.
--
-- 'maxResults', 'describeLocalGatewayRouteTableVpcAssociations_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'nextToken', 'describeLocalGatewayRouteTableVpcAssociations_nextToken' - The token for the next page of results.
newDescribeLocalGatewayRouteTableVpcAssociations ::
  DescribeLocalGatewayRouteTableVpcAssociations
newDescribeLocalGatewayRouteTableVpcAssociations =
  DescribeLocalGatewayRouteTableVpcAssociations'
    { dryRun =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      localGatewayRouteTableVpcAssociationIds =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeLocalGatewayRouteTableVpcAssociations_dryRun :: Lens.Lens' DescribeLocalGatewayRouteTableVpcAssociations (Prelude.Maybe Prelude.Bool)
describeLocalGatewayRouteTableVpcAssociations_dryRun = Lens.lens (\DescribeLocalGatewayRouteTableVpcAssociations' {dryRun} -> dryRun) (\s@DescribeLocalGatewayRouteTableVpcAssociations' {} a -> s {dryRun = a} :: DescribeLocalGatewayRouteTableVpcAssociations)

-- | One or more filters.
--
-- -   @local-gateway-id@ - The ID of a local gateway.
--
-- -   @local-gateway-route-table-arn@ - The Amazon Resource Name (ARN) of
--     the local gateway route table for the association.
--
-- -   @local-gateway-route-table-id@ - The ID of the local gateway route
--     table.
--
-- -   @local-gateway-route-table-vpc-association-id@ - The ID of the
--     association.
--
-- -   @owner-id@ - The ID of the Amazon Web Services account that owns the
--     local gateway route table for the association.
--
-- -   @state@ - The state of the association.
--
-- -   @vpc-id@ - The ID of the VPC.
describeLocalGatewayRouteTableVpcAssociations_filters :: Lens.Lens' DescribeLocalGatewayRouteTableVpcAssociations (Prelude.Maybe [Filter])
describeLocalGatewayRouteTableVpcAssociations_filters = Lens.lens (\DescribeLocalGatewayRouteTableVpcAssociations' {filters} -> filters) (\s@DescribeLocalGatewayRouteTableVpcAssociations' {} a -> s {filters = a} :: DescribeLocalGatewayRouteTableVpcAssociations) Prelude.. Lens.mapping Lens.coerced

-- | The IDs of the associations.
describeLocalGatewayRouteTableVpcAssociations_localGatewayRouteTableVpcAssociationIds :: Lens.Lens' DescribeLocalGatewayRouteTableVpcAssociations (Prelude.Maybe [Prelude.Text])
describeLocalGatewayRouteTableVpcAssociations_localGatewayRouteTableVpcAssociationIds = Lens.lens (\DescribeLocalGatewayRouteTableVpcAssociations' {localGatewayRouteTableVpcAssociationIds} -> localGatewayRouteTableVpcAssociationIds) (\s@DescribeLocalGatewayRouteTableVpcAssociations' {} a -> s {localGatewayRouteTableVpcAssociationIds = a} :: DescribeLocalGatewayRouteTableVpcAssociations) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeLocalGatewayRouteTableVpcAssociations_maxResults :: Lens.Lens' DescribeLocalGatewayRouteTableVpcAssociations (Prelude.Maybe Prelude.Natural)
describeLocalGatewayRouteTableVpcAssociations_maxResults = Lens.lens (\DescribeLocalGatewayRouteTableVpcAssociations' {maxResults} -> maxResults) (\s@DescribeLocalGatewayRouteTableVpcAssociations' {} a -> s {maxResults = a} :: DescribeLocalGatewayRouteTableVpcAssociations)

-- | The token for the next page of results.
describeLocalGatewayRouteTableVpcAssociations_nextToken :: Lens.Lens' DescribeLocalGatewayRouteTableVpcAssociations (Prelude.Maybe Prelude.Text)
describeLocalGatewayRouteTableVpcAssociations_nextToken = Lens.lens (\DescribeLocalGatewayRouteTableVpcAssociations' {nextToken} -> nextToken) (\s@DescribeLocalGatewayRouteTableVpcAssociations' {} a -> s {nextToken = a} :: DescribeLocalGatewayRouteTableVpcAssociations)

instance
  Core.AWSPager
    DescribeLocalGatewayRouteTableVpcAssociations
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeLocalGatewayRouteTableVpcAssociationsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeLocalGatewayRouteTableVpcAssociationsResponse_localGatewayRouteTableVpcAssociations
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& describeLocalGatewayRouteTableVpcAssociations_nextToken
              Lens..~ rs
              Lens.^? describeLocalGatewayRouteTableVpcAssociationsResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeLocalGatewayRouteTableVpcAssociations
  where
  type
    AWSResponse
      DescribeLocalGatewayRouteTableVpcAssociations =
      DescribeLocalGatewayRouteTableVpcAssociationsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeLocalGatewayRouteTableVpcAssociationsResponse'
            Prelude.<$> ( x
                            Data..@? "localGatewayRouteTableVpcAssociationSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeLocalGatewayRouteTableVpcAssociations
  where
  hashWithSalt
    _salt
    DescribeLocalGatewayRouteTableVpcAssociations' {..} =
      _salt
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` localGatewayRouteTableVpcAssociationIds
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken

instance
  Prelude.NFData
    DescribeLocalGatewayRouteTableVpcAssociations
  where
  rnf
    DescribeLocalGatewayRouteTableVpcAssociations' {..} =
      Prelude.rnf dryRun `Prelude.seq`
        Prelude.rnf filters `Prelude.seq`
          Prelude.rnf localGatewayRouteTableVpcAssociationIds `Prelude.seq`
            Prelude.rnf maxResults `Prelude.seq`
              Prelude.rnf nextToken

instance
  Data.ToHeaders
    DescribeLocalGatewayRouteTableVpcAssociations
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DescribeLocalGatewayRouteTableVpcAssociations
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeLocalGatewayRouteTableVpcAssociations
  where
  toQuery
    DescribeLocalGatewayRouteTableVpcAssociations' {..} =
      Prelude.mconcat
        [ "Action"
            Data.=: ( "DescribeLocalGatewayRouteTableVpcAssociations" ::
                        Prelude.ByteString
                    ),
          "Version"
            Data.=: ("2016-11-15" :: Prelude.ByteString),
          "DryRun" Data.=: dryRun,
          Data.toQuery
            (Data.toQueryList "Filter" Prelude.<$> filters),
          Data.toQuery
            ( Data.toQueryList
                "LocalGatewayRouteTableVpcAssociationId"
                Prelude.<$> localGatewayRouteTableVpcAssociationIds
            ),
          "MaxResults" Data.=: maxResults,
          "NextToken" Data.=: nextToken
        ]

-- | /See:/ 'newDescribeLocalGatewayRouteTableVpcAssociationsResponse' smart constructor.
data DescribeLocalGatewayRouteTableVpcAssociationsResponse = DescribeLocalGatewayRouteTableVpcAssociationsResponse'
  { -- | Information about the associations.
    localGatewayRouteTableVpcAssociations :: Prelude.Maybe [LocalGatewayRouteTableVpcAssociation],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLocalGatewayRouteTableVpcAssociationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'localGatewayRouteTableVpcAssociations', 'describeLocalGatewayRouteTableVpcAssociationsResponse_localGatewayRouteTableVpcAssociations' - Information about the associations.
--
-- 'nextToken', 'describeLocalGatewayRouteTableVpcAssociationsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'describeLocalGatewayRouteTableVpcAssociationsResponse_httpStatus' - The response's http status code.
newDescribeLocalGatewayRouteTableVpcAssociationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeLocalGatewayRouteTableVpcAssociationsResponse
newDescribeLocalGatewayRouteTableVpcAssociationsResponse
  pHttpStatus_ =
    DescribeLocalGatewayRouteTableVpcAssociationsResponse'
      { localGatewayRouteTableVpcAssociations =
          Prelude.Nothing,
        nextToken =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Information about the associations.
describeLocalGatewayRouteTableVpcAssociationsResponse_localGatewayRouteTableVpcAssociations :: Lens.Lens' DescribeLocalGatewayRouteTableVpcAssociationsResponse (Prelude.Maybe [LocalGatewayRouteTableVpcAssociation])
describeLocalGatewayRouteTableVpcAssociationsResponse_localGatewayRouteTableVpcAssociations = Lens.lens (\DescribeLocalGatewayRouteTableVpcAssociationsResponse' {localGatewayRouteTableVpcAssociations} -> localGatewayRouteTableVpcAssociations) (\s@DescribeLocalGatewayRouteTableVpcAssociationsResponse' {} a -> s {localGatewayRouteTableVpcAssociations = a} :: DescribeLocalGatewayRouteTableVpcAssociationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeLocalGatewayRouteTableVpcAssociationsResponse_nextToken :: Lens.Lens' DescribeLocalGatewayRouteTableVpcAssociationsResponse (Prelude.Maybe Prelude.Text)
describeLocalGatewayRouteTableVpcAssociationsResponse_nextToken = Lens.lens (\DescribeLocalGatewayRouteTableVpcAssociationsResponse' {nextToken} -> nextToken) (\s@DescribeLocalGatewayRouteTableVpcAssociationsResponse' {} a -> s {nextToken = a} :: DescribeLocalGatewayRouteTableVpcAssociationsResponse)

-- | The response's http status code.
describeLocalGatewayRouteTableVpcAssociationsResponse_httpStatus :: Lens.Lens' DescribeLocalGatewayRouteTableVpcAssociationsResponse Prelude.Int
describeLocalGatewayRouteTableVpcAssociationsResponse_httpStatus = Lens.lens (\DescribeLocalGatewayRouteTableVpcAssociationsResponse' {httpStatus} -> httpStatus) (\s@DescribeLocalGatewayRouteTableVpcAssociationsResponse' {} a -> s {httpStatus = a} :: DescribeLocalGatewayRouteTableVpcAssociationsResponse)

instance
  Prelude.NFData
    DescribeLocalGatewayRouteTableVpcAssociationsResponse
  where
  rnf
    DescribeLocalGatewayRouteTableVpcAssociationsResponse' {..} =
      Prelude.rnf localGatewayRouteTableVpcAssociations `Prelude.seq`
        Prelude.rnf nextToken `Prelude.seq`
          Prelude.rnf httpStatus
