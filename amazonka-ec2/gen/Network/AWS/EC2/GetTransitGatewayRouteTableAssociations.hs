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
-- Module      : Network.AWS.EC2.GetTransitGatewayRouteTableAssociations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the associations for the specified transit
-- gateway route table.
--
-- This operation returns paginated results.
module Network.AWS.EC2.GetTransitGatewayRouteTableAssociations
  ( -- * Creating a Request
    GetTransitGatewayRouteTableAssociations (..),
    newGetTransitGatewayRouteTableAssociations,

    -- * Request Lenses
    getTransitGatewayRouteTableAssociations_nextToken,
    getTransitGatewayRouteTableAssociations_dryRun,
    getTransitGatewayRouteTableAssociations_maxResults,
    getTransitGatewayRouteTableAssociations_filters,
    getTransitGatewayRouteTableAssociations_transitGatewayRouteTableId,

    -- * Destructuring the Response
    GetTransitGatewayRouteTableAssociationsResponse (..),
    newGetTransitGatewayRouteTableAssociationsResponse,

    -- * Response Lenses
    getTransitGatewayRouteTableAssociationsResponse_nextToken,
    getTransitGatewayRouteTableAssociationsResponse_associations,
    getTransitGatewayRouteTableAssociationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetTransitGatewayRouteTableAssociations' smart constructor.
data GetTransitGatewayRouteTableAssociations = GetTransitGatewayRouteTableAssociations'
  { -- | The token for the next page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Core.Maybe Core.Natural,
    -- | One or more filters. The possible values are:
    --
    -- -   @resource-id@ - The ID of the resource.
    --
    -- -   @resource-type@ - The resource type. Valid values are @vpc@ | @vpn@
    --     | @direct-connect-gateway@ | @peering@ | @connect@.
    --
    -- -   @transit-gateway-attachment-id@ - The ID of the attachment.
    filters :: Core.Maybe [Filter],
    -- | The ID of the transit gateway route table.
    transitGatewayRouteTableId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetTransitGatewayRouteTableAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getTransitGatewayRouteTableAssociations_nextToken' - The token for the next page of results.
--
-- 'dryRun', 'getTransitGatewayRouteTableAssociations_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'getTransitGatewayRouteTableAssociations_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
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
-- 'transitGatewayRouteTableId', 'getTransitGatewayRouteTableAssociations_transitGatewayRouteTableId' - The ID of the transit gateway route table.
newGetTransitGatewayRouteTableAssociations ::
  -- | 'transitGatewayRouteTableId'
  Core.Text ->
  GetTransitGatewayRouteTableAssociations
newGetTransitGatewayRouteTableAssociations
  pTransitGatewayRouteTableId_ =
    GetTransitGatewayRouteTableAssociations'
      { nextToken =
          Core.Nothing,
        dryRun = Core.Nothing,
        maxResults = Core.Nothing,
        filters = Core.Nothing,
        transitGatewayRouteTableId =
          pTransitGatewayRouteTableId_
      }

-- | The token for the next page of results.
getTransitGatewayRouteTableAssociations_nextToken :: Lens.Lens' GetTransitGatewayRouteTableAssociations (Core.Maybe Core.Text)
getTransitGatewayRouteTableAssociations_nextToken = Lens.lens (\GetTransitGatewayRouteTableAssociations' {nextToken} -> nextToken) (\s@GetTransitGatewayRouteTableAssociations' {} a -> s {nextToken = a} :: GetTransitGatewayRouteTableAssociations)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
getTransitGatewayRouteTableAssociations_dryRun :: Lens.Lens' GetTransitGatewayRouteTableAssociations (Core.Maybe Core.Bool)
getTransitGatewayRouteTableAssociations_dryRun = Lens.lens (\GetTransitGatewayRouteTableAssociations' {dryRun} -> dryRun) (\s@GetTransitGatewayRouteTableAssociations' {} a -> s {dryRun = a} :: GetTransitGatewayRouteTableAssociations)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
getTransitGatewayRouteTableAssociations_maxResults :: Lens.Lens' GetTransitGatewayRouteTableAssociations (Core.Maybe Core.Natural)
getTransitGatewayRouteTableAssociations_maxResults = Lens.lens (\GetTransitGatewayRouteTableAssociations' {maxResults} -> maxResults) (\s@GetTransitGatewayRouteTableAssociations' {} a -> s {maxResults = a} :: GetTransitGatewayRouteTableAssociations)

-- | One or more filters. The possible values are:
--
-- -   @resource-id@ - The ID of the resource.
--
-- -   @resource-type@ - The resource type. Valid values are @vpc@ | @vpn@
--     | @direct-connect-gateway@ | @peering@ | @connect@.
--
-- -   @transit-gateway-attachment-id@ - The ID of the attachment.
getTransitGatewayRouteTableAssociations_filters :: Lens.Lens' GetTransitGatewayRouteTableAssociations (Core.Maybe [Filter])
getTransitGatewayRouteTableAssociations_filters = Lens.lens (\GetTransitGatewayRouteTableAssociations' {filters} -> filters) (\s@GetTransitGatewayRouteTableAssociations' {} a -> s {filters = a} :: GetTransitGatewayRouteTableAssociations) Core.. Lens.mapping Lens._Coerce

-- | The ID of the transit gateway route table.
getTransitGatewayRouteTableAssociations_transitGatewayRouteTableId :: Lens.Lens' GetTransitGatewayRouteTableAssociations Core.Text
getTransitGatewayRouteTableAssociations_transitGatewayRouteTableId = Lens.lens (\GetTransitGatewayRouteTableAssociations' {transitGatewayRouteTableId} -> transitGatewayRouteTableId) (\s@GetTransitGatewayRouteTableAssociations' {} a -> s {transitGatewayRouteTableId = a} :: GetTransitGatewayRouteTableAssociations)

instance
  Core.AWSPager
    GetTransitGatewayRouteTableAssociations
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getTransitGatewayRouteTableAssociationsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getTransitGatewayRouteTableAssociationsResponse_associations
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getTransitGatewayRouteTableAssociations_nextToken
          Lens..~ rs
            Lens.^? getTransitGatewayRouteTableAssociationsResponse_nextToken
              Core.. Lens._Just

instance
  Core.AWSRequest
    GetTransitGatewayRouteTableAssociations
  where
  type
    AWSResponse
      GetTransitGatewayRouteTableAssociations =
      GetTransitGatewayRouteTableAssociationsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetTransitGatewayRouteTableAssociationsResponse'
            Core.<$> (x Core..@? "nextToken")
              Core.<*> ( x Core..@? "associations" Core..!@ Core.mempty
                           Core.>>= Core.may (Core.parseXMLList "item")
                       )
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    GetTransitGatewayRouteTableAssociations

instance
  Core.NFData
    GetTransitGatewayRouteTableAssociations

instance
  Core.ToHeaders
    GetTransitGatewayRouteTableAssociations
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    GetTransitGatewayRouteTableAssociations
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    GetTransitGatewayRouteTableAssociations
  where
  toQuery GetTransitGatewayRouteTableAssociations' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "GetTransitGatewayRouteTableAssociations" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          (Core.toQueryList "Filter" Core.<$> filters),
        "TransitGatewayRouteTableId"
          Core.=: transitGatewayRouteTableId
      ]

-- | /See:/ 'newGetTransitGatewayRouteTableAssociationsResponse' smart constructor.
data GetTransitGatewayRouteTableAssociationsResponse = GetTransitGatewayRouteTableAssociationsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the associations.
    associations :: Core.Maybe [TransitGatewayRouteTableAssociation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetTransitGatewayRouteTableAssociationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getTransitGatewayRouteTableAssociationsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'associations', 'getTransitGatewayRouteTableAssociationsResponse_associations' - Information about the associations.
--
-- 'httpStatus', 'getTransitGatewayRouteTableAssociationsResponse_httpStatus' - The response's http status code.
newGetTransitGatewayRouteTableAssociationsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetTransitGatewayRouteTableAssociationsResponse
newGetTransitGatewayRouteTableAssociationsResponse
  pHttpStatus_ =
    GetTransitGatewayRouteTableAssociationsResponse'
      { nextToken =
          Core.Nothing,
        associations =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
getTransitGatewayRouteTableAssociationsResponse_nextToken :: Lens.Lens' GetTransitGatewayRouteTableAssociationsResponse (Core.Maybe Core.Text)
getTransitGatewayRouteTableAssociationsResponse_nextToken = Lens.lens (\GetTransitGatewayRouteTableAssociationsResponse' {nextToken} -> nextToken) (\s@GetTransitGatewayRouteTableAssociationsResponse' {} a -> s {nextToken = a} :: GetTransitGatewayRouteTableAssociationsResponse)

-- | Information about the associations.
getTransitGatewayRouteTableAssociationsResponse_associations :: Lens.Lens' GetTransitGatewayRouteTableAssociationsResponse (Core.Maybe [TransitGatewayRouteTableAssociation])
getTransitGatewayRouteTableAssociationsResponse_associations = Lens.lens (\GetTransitGatewayRouteTableAssociationsResponse' {associations} -> associations) (\s@GetTransitGatewayRouteTableAssociationsResponse' {} a -> s {associations = a} :: GetTransitGatewayRouteTableAssociationsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getTransitGatewayRouteTableAssociationsResponse_httpStatus :: Lens.Lens' GetTransitGatewayRouteTableAssociationsResponse Core.Int
getTransitGatewayRouteTableAssociationsResponse_httpStatus = Lens.lens (\GetTransitGatewayRouteTableAssociationsResponse' {httpStatus} -> httpStatus) (\s@GetTransitGatewayRouteTableAssociationsResponse' {} a -> s {httpStatus = a} :: GetTransitGatewayRouteTableAssociationsResponse)

instance
  Core.NFData
    GetTransitGatewayRouteTableAssociationsResponse
