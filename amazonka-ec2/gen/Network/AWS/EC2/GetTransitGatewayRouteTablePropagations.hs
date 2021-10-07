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
-- Module      : Network.AWS.EC2.GetTransitGatewayRouteTablePropagations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the route table propagations for the specified
-- transit gateway route table.
--
-- This operation returns paginated results.
module Network.AWS.EC2.GetTransitGatewayRouteTablePropagations
  ( -- * Creating a Request
    GetTransitGatewayRouteTablePropagations (..),
    newGetTransitGatewayRouteTablePropagations,

    -- * Request Lenses
    getTransitGatewayRouteTablePropagations_nextToken,
    getTransitGatewayRouteTablePropagations_maxResults,
    getTransitGatewayRouteTablePropagations_dryRun,
    getTransitGatewayRouteTablePropagations_filters,
    getTransitGatewayRouteTablePropagations_transitGatewayRouteTableId,

    -- * Destructuring the Response
    GetTransitGatewayRouteTablePropagationsResponse (..),
    newGetTransitGatewayRouteTablePropagationsResponse,

    -- * Response Lenses
    getTransitGatewayRouteTablePropagationsResponse_nextToken,
    getTransitGatewayRouteTablePropagationsResponse_transitGatewayRouteTablePropagations,
    getTransitGatewayRouteTablePropagationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetTransitGatewayRouteTablePropagations' smart constructor.
data GetTransitGatewayRouteTablePropagations = GetTransitGatewayRouteTablePropagations'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Checks whether you have the required permissions for the action, without
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
    -- | The ID of the transit gateway route table.
    transitGatewayRouteTableId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTransitGatewayRouteTablePropagations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getTransitGatewayRouteTablePropagations_nextToken' - The token for the next page of results.
--
-- 'maxResults', 'getTransitGatewayRouteTablePropagations_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'dryRun', 'getTransitGatewayRouteTablePropagations_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'getTransitGatewayRouteTablePropagations_filters' - One or more filters. The possible values are:
--
-- -   @resource-id@ - The ID of the resource.
--
-- -   @resource-type@ - The resource type. Valid values are @vpc@ | @vpn@
--     | @direct-connect-gateway@ | @peering@ | @connect@.
--
-- -   @transit-gateway-attachment-id@ - The ID of the attachment.
--
-- 'transitGatewayRouteTableId', 'getTransitGatewayRouteTablePropagations_transitGatewayRouteTableId' - The ID of the transit gateway route table.
newGetTransitGatewayRouteTablePropagations ::
  -- | 'transitGatewayRouteTableId'
  Prelude.Text ->
  GetTransitGatewayRouteTablePropagations
newGetTransitGatewayRouteTablePropagations
  pTransitGatewayRouteTableId_ =
    GetTransitGatewayRouteTablePropagations'
      { nextToken =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        dryRun = Prelude.Nothing,
        filters = Prelude.Nothing,
        transitGatewayRouteTableId =
          pTransitGatewayRouteTableId_
      }

-- | The token for the next page of results.
getTransitGatewayRouteTablePropagations_nextToken :: Lens.Lens' GetTransitGatewayRouteTablePropagations (Prelude.Maybe Prelude.Text)
getTransitGatewayRouteTablePropagations_nextToken = Lens.lens (\GetTransitGatewayRouteTablePropagations' {nextToken} -> nextToken) (\s@GetTransitGatewayRouteTablePropagations' {} a -> s {nextToken = a} :: GetTransitGatewayRouteTablePropagations)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
getTransitGatewayRouteTablePropagations_maxResults :: Lens.Lens' GetTransitGatewayRouteTablePropagations (Prelude.Maybe Prelude.Natural)
getTransitGatewayRouteTablePropagations_maxResults = Lens.lens (\GetTransitGatewayRouteTablePropagations' {maxResults} -> maxResults) (\s@GetTransitGatewayRouteTablePropagations' {} a -> s {maxResults = a} :: GetTransitGatewayRouteTablePropagations)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
getTransitGatewayRouteTablePropagations_dryRun :: Lens.Lens' GetTransitGatewayRouteTablePropagations (Prelude.Maybe Prelude.Bool)
getTransitGatewayRouteTablePropagations_dryRun = Lens.lens (\GetTransitGatewayRouteTablePropagations' {dryRun} -> dryRun) (\s@GetTransitGatewayRouteTablePropagations' {} a -> s {dryRun = a} :: GetTransitGatewayRouteTablePropagations)

-- | One or more filters. The possible values are:
--
-- -   @resource-id@ - The ID of the resource.
--
-- -   @resource-type@ - The resource type. Valid values are @vpc@ | @vpn@
--     | @direct-connect-gateway@ | @peering@ | @connect@.
--
-- -   @transit-gateway-attachment-id@ - The ID of the attachment.
getTransitGatewayRouteTablePropagations_filters :: Lens.Lens' GetTransitGatewayRouteTablePropagations (Prelude.Maybe [Filter])
getTransitGatewayRouteTablePropagations_filters = Lens.lens (\GetTransitGatewayRouteTablePropagations' {filters} -> filters) (\s@GetTransitGatewayRouteTablePropagations' {} a -> s {filters = a} :: GetTransitGatewayRouteTablePropagations) Prelude.. Lens.mapping Lens._Coerce

-- | The ID of the transit gateway route table.
getTransitGatewayRouteTablePropagations_transitGatewayRouteTableId :: Lens.Lens' GetTransitGatewayRouteTablePropagations Prelude.Text
getTransitGatewayRouteTablePropagations_transitGatewayRouteTableId = Lens.lens (\GetTransitGatewayRouteTablePropagations' {transitGatewayRouteTableId} -> transitGatewayRouteTableId) (\s@GetTransitGatewayRouteTablePropagations' {} a -> s {transitGatewayRouteTableId = a} :: GetTransitGatewayRouteTablePropagations)

instance
  Core.AWSPager
    GetTransitGatewayRouteTablePropagations
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getTransitGatewayRouteTablePropagationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getTransitGatewayRouteTablePropagationsResponse_transitGatewayRouteTablePropagations
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getTransitGatewayRouteTablePropagations_nextToken
          Lens..~ rs
            Lens.^? getTransitGatewayRouteTablePropagationsResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    GetTransitGatewayRouteTablePropagations
  where
  type
    AWSResponse
      GetTransitGatewayRouteTablePropagations =
      GetTransitGatewayRouteTablePropagationsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetTransitGatewayRouteTablePropagationsResponse'
            Prelude.<$> (x Core..@? "nextToken")
              Prelude.<*> ( x Core..@? "transitGatewayRouteTablePropagations"
                              Core..!@ Prelude.mempty
                              Prelude.>>= Core.may (Core.parseXMLList "item")
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetTransitGatewayRouteTablePropagations

instance
  Prelude.NFData
    GetTransitGatewayRouteTablePropagations

instance
  Core.ToHeaders
    GetTransitGatewayRouteTablePropagations
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    GetTransitGatewayRouteTablePropagations
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    GetTransitGatewayRouteTablePropagations
  where
  toQuery GetTransitGatewayRouteTablePropagations' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "GetTransitGatewayRouteTablePropagations" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        "MaxResults" Core.=: maxResults,
        "DryRun" Core.=: dryRun,
        Core.toQuery
          (Core.toQueryList "Filter" Prelude.<$> filters),
        "TransitGatewayRouteTableId"
          Core.=: transitGatewayRouteTableId
      ]

-- | /See:/ 'newGetTransitGatewayRouteTablePropagationsResponse' smart constructor.
data GetTransitGatewayRouteTablePropagationsResponse = GetTransitGatewayRouteTablePropagationsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the route table propagations.
    transitGatewayRouteTablePropagations :: Prelude.Maybe [TransitGatewayRouteTablePropagation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTransitGatewayRouteTablePropagationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getTransitGatewayRouteTablePropagationsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'transitGatewayRouteTablePropagations', 'getTransitGatewayRouteTablePropagationsResponse_transitGatewayRouteTablePropagations' - Information about the route table propagations.
--
-- 'httpStatus', 'getTransitGatewayRouteTablePropagationsResponse_httpStatus' - The response's http status code.
newGetTransitGatewayRouteTablePropagationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetTransitGatewayRouteTablePropagationsResponse
newGetTransitGatewayRouteTablePropagationsResponse
  pHttpStatus_ =
    GetTransitGatewayRouteTablePropagationsResponse'
      { nextToken =
          Prelude.Nothing,
        transitGatewayRouteTablePropagations =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
getTransitGatewayRouteTablePropagationsResponse_nextToken :: Lens.Lens' GetTransitGatewayRouteTablePropagationsResponse (Prelude.Maybe Prelude.Text)
getTransitGatewayRouteTablePropagationsResponse_nextToken = Lens.lens (\GetTransitGatewayRouteTablePropagationsResponse' {nextToken} -> nextToken) (\s@GetTransitGatewayRouteTablePropagationsResponse' {} a -> s {nextToken = a} :: GetTransitGatewayRouteTablePropagationsResponse)

-- | Information about the route table propagations.
getTransitGatewayRouteTablePropagationsResponse_transitGatewayRouteTablePropagations :: Lens.Lens' GetTransitGatewayRouteTablePropagationsResponse (Prelude.Maybe [TransitGatewayRouteTablePropagation])
getTransitGatewayRouteTablePropagationsResponse_transitGatewayRouteTablePropagations = Lens.lens (\GetTransitGatewayRouteTablePropagationsResponse' {transitGatewayRouteTablePropagations} -> transitGatewayRouteTablePropagations) (\s@GetTransitGatewayRouteTablePropagationsResponse' {} a -> s {transitGatewayRouteTablePropagations = a} :: GetTransitGatewayRouteTablePropagationsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getTransitGatewayRouteTablePropagationsResponse_httpStatus :: Lens.Lens' GetTransitGatewayRouteTablePropagationsResponse Prelude.Int
getTransitGatewayRouteTablePropagationsResponse_httpStatus = Lens.lens (\GetTransitGatewayRouteTablePropagationsResponse' {httpStatus} -> httpStatus) (\s@GetTransitGatewayRouteTablePropagationsResponse' {} a -> s {httpStatus = a} :: GetTransitGatewayRouteTablePropagationsResponse)

instance
  Prelude.NFData
    GetTransitGatewayRouteTablePropagationsResponse
