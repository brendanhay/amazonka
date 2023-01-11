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
-- Module      : Amazonka.EC2.GetTransitGatewayRouteTableAssociations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the associations for the specified transit
-- gateway route table.
--
-- This operation returns paginated results.
module Amazonka.EC2.GetTransitGatewayRouteTableAssociations
  ( -- * Creating a Request
    GetTransitGatewayRouteTableAssociations (..),
    newGetTransitGatewayRouteTableAssociations,

    -- * Request Lenses
    getTransitGatewayRouteTableAssociations_dryRun,
    getTransitGatewayRouteTableAssociations_filters,
    getTransitGatewayRouteTableAssociations_maxResults,
    getTransitGatewayRouteTableAssociations_nextToken,
    getTransitGatewayRouteTableAssociations_transitGatewayRouteTableId,

    -- * Destructuring the Response
    GetTransitGatewayRouteTableAssociationsResponse (..),
    newGetTransitGatewayRouteTableAssociationsResponse,

    -- * Response Lenses
    getTransitGatewayRouteTableAssociationsResponse_associations,
    getTransitGatewayRouteTableAssociationsResponse_nextToken,
    getTransitGatewayRouteTableAssociationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetTransitGatewayRouteTableAssociations' smart constructor.
data GetTransitGatewayRouteTableAssociations = GetTransitGatewayRouteTableAssociations'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | One or more filters. The possible values are:
    --
    -- -   @resource-id@ - The ID of the resource.
    --
    -- -   @resource-type@ - The resource type. Valid values are @vpc@ | @vpn@
    --     | @direct-connect-gateway@ | @peering@ | @connect@.
    --
    -- -   @transit-gateway-attachment-id@ - The ID of the attachment.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the transit gateway route table.
    transitGatewayRouteTableId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTransitGatewayRouteTableAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'getTransitGatewayRouteTableAssociations_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'getTransitGatewayRouteTableAssociations_filters' - One or more filters. The possible values are:
--
-- -   @resource-id@ - The ID of the resource.
--
-- -   @resource-type@ - The resource type. Valid values are @vpc@ | @vpn@
--     | @direct-connect-gateway@ | @peering@ | @connect@.
--
-- -   @transit-gateway-attachment-id@ - The ID of the attachment.
--
-- 'maxResults', 'getTransitGatewayRouteTableAssociations_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'nextToken', 'getTransitGatewayRouteTableAssociations_nextToken' - The token for the next page of results.
--
-- 'transitGatewayRouteTableId', 'getTransitGatewayRouteTableAssociations_transitGatewayRouteTableId' - The ID of the transit gateway route table.
newGetTransitGatewayRouteTableAssociations ::
  -- | 'transitGatewayRouteTableId'
  Prelude.Text ->
  GetTransitGatewayRouteTableAssociations
newGetTransitGatewayRouteTableAssociations
  pTransitGatewayRouteTableId_ =
    GetTransitGatewayRouteTableAssociations'
      { dryRun =
          Prelude.Nothing,
        filters = Prelude.Nothing,
        maxResults = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        transitGatewayRouteTableId =
          pTransitGatewayRouteTableId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
getTransitGatewayRouteTableAssociations_dryRun :: Lens.Lens' GetTransitGatewayRouteTableAssociations (Prelude.Maybe Prelude.Bool)
getTransitGatewayRouteTableAssociations_dryRun = Lens.lens (\GetTransitGatewayRouteTableAssociations' {dryRun} -> dryRun) (\s@GetTransitGatewayRouteTableAssociations' {} a -> s {dryRun = a} :: GetTransitGatewayRouteTableAssociations)

-- | One or more filters. The possible values are:
--
-- -   @resource-id@ - The ID of the resource.
--
-- -   @resource-type@ - The resource type. Valid values are @vpc@ | @vpn@
--     | @direct-connect-gateway@ | @peering@ | @connect@.
--
-- -   @transit-gateway-attachment-id@ - The ID of the attachment.
getTransitGatewayRouteTableAssociations_filters :: Lens.Lens' GetTransitGatewayRouteTableAssociations (Prelude.Maybe [Filter])
getTransitGatewayRouteTableAssociations_filters = Lens.lens (\GetTransitGatewayRouteTableAssociations' {filters} -> filters) (\s@GetTransitGatewayRouteTableAssociations' {} a -> s {filters = a} :: GetTransitGatewayRouteTableAssociations) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
getTransitGatewayRouteTableAssociations_maxResults :: Lens.Lens' GetTransitGatewayRouteTableAssociations (Prelude.Maybe Prelude.Natural)
getTransitGatewayRouteTableAssociations_maxResults = Lens.lens (\GetTransitGatewayRouteTableAssociations' {maxResults} -> maxResults) (\s@GetTransitGatewayRouteTableAssociations' {} a -> s {maxResults = a} :: GetTransitGatewayRouteTableAssociations)

-- | The token for the next page of results.
getTransitGatewayRouteTableAssociations_nextToken :: Lens.Lens' GetTransitGatewayRouteTableAssociations (Prelude.Maybe Prelude.Text)
getTransitGatewayRouteTableAssociations_nextToken = Lens.lens (\GetTransitGatewayRouteTableAssociations' {nextToken} -> nextToken) (\s@GetTransitGatewayRouteTableAssociations' {} a -> s {nextToken = a} :: GetTransitGatewayRouteTableAssociations)

-- | The ID of the transit gateway route table.
getTransitGatewayRouteTableAssociations_transitGatewayRouteTableId :: Lens.Lens' GetTransitGatewayRouteTableAssociations Prelude.Text
getTransitGatewayRouteTableAssociations_transitGatewayRouteTableId = Lens.lens (\GetTransitGatewayRouteTableAssociations' {transitGatewayRouteTableId} -> transitGatewayRouteTableId) (\s@GetTransitGatewayRouteTableAssociations' {} a -> s {transitGatewayRouteTableId = a} :: GetTransitGatewayRouteTableAssociations)

instance
  Core.AWSPager
    GetTransitGatewayRouteTableAssociations
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getTransitGatewayRouteTableAssociationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getTransitGatewayRouteTableAssociationsResponse_associations
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getTransitGatewayRouteTableAssociations_nextToken
          Lens..~ rs
            Lens.^? getTransitGatewayRouteTableAssociationsResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    GetTransitGatewayRouteTableAssociations
  where
  type
    AWSResponse
      GetTransitGatewayRouteTableAssociations =
      GetTransitGatewayRouteTableAssociationsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetTransitGatewayRouteTableAssociationsResponse'
            Prelude.<$> ( x Data..@? "associations" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
              Prelude.<*> (x Data..@? "nextToken")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetTransitGatewayRouteTableAssociations
  where
  hashWithSalt
    _salt
    GetTransitGatewayRouteTableAssociations' {..} =
      _salt `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` transitGatewayRouteTableId

instance
  Prelude.NFData
    GetTransitGatewayRouteTableAssociations
  where
  rnf GetTransitGatewayRouteTableAssociations' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf transitGatewayRouteTableId

instance
  Data.ToHeaders
    GetTransitGatewayRouteTableAssociations
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    GetTransitGatewayRouteTableAssociations
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    GetTransitGatewayRouteTableAssociations
  where
  toQuery GetTransitGatewayRouteTableAssociations' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "GetTransitGatewayRouteTableAssociations" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        "TransitGatewayRouteTableId"
          Data.=: transitGatewayRouteTableId
      ]

-- | /See:/ 'newGetTransitGatewayRouteTableAssociationsResponse' smart constructor.
data GetTransitGatewayRouteTableAssociationsResponse = GetTransitGatewayRouteTableAssociationsResponse'
  { -- | Information about the associations.
    associations :: Prelude.Maybe [TransitGatewayRouteTableAssociation],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTransitGatewayRouteTableAssociationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associations', 'getTransitGatewayRouteTableAssociationsResponse_associations' - Information about the associations.
--
-- 'nextToken', 'getTransitGatewayRouteTableAssociationsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'getTransitGatewayRouteTableAssociationsResponse_httpStatus' - The response's http status code.
newGetTransitGatewayRouteTableAssociationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetTransitGatewayRouteTableAssociationsResponse
newGetTransitGatewayRouteTableAssociationsResponse
  pHttpStatus_ =
    GetTransitGatewayRouteTableAssociationsResponse'
      { associations =
          Prelude.Nothing,
        nextToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the associations.
getTransitGatewayRouteTableAssociationsResponse_associations :: Lens.Lens' GetTransitGatewayRouteTableAssociationsResponse (Prelude.Maybe [TransitGatewayRouteTableAssociation])
getTransitGatewayRouteTableAssociationsResponse_associations = Lens.lens (\GetTransitGatewayRouteTableAssociationsResponse' {associations} -> associations) (\s@GetTransitGatewayRouteTableAssociationsResponse' {} a -> s {associations = a} :: GetTransitGatewayRouteTableAssociationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
getTransitGatewayRouteTableAssociationsResponse_nextToken :: Lens.Lens' GetTransitGatewayRouteTableAssociationsResponse (Prelude.Maybe Prelude.Text)
getTransitGatewayRouteTableAssociationsResponse_nextToken = Lens.lens (\GetTransitGatewayRouteTableAssociationsResponse' {nextToken} -> nextToken) (\s@GetTransitGatewayRouteTableAssociationsResponse' {} a -> s {nextToken = a} :: GetTransitGatewayRouteTableAssociationsResponse)

-- | The response's http status code.
getTransitGatewayRouteTableAssociationsResponse_httpStatus :: Lens.Lens' GetTransitGatewayRouteTableAssociationsResponse Prelude.Int
getTransitGatewayRouteTableAssociationsResponse_httpStatus = Lens.lens (\GetTransitGatewayRouteTableAssociationsResponse' {httpStatus} -> httpStatus) (\s@GetTransitGatewayRouteTableAssociationsResponse' {} a -> s {httpStatus = a} :: GetTransitGatewayRouteTableAssociationsResponse)

instance
  Prelude.NFData
    GetTransitGatewayRouteTableAssociationsResponse
  where
  rnf
    GetTransitGatewayRouteTableAssociationsResponse' {..} =
      Prelude.rnf associations
        `Prelude.seq` Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf httpStatus
