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
-- Module      : Network.AWS.EC2.SearchTransitGatewayRoutes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches for routes in the specified transit gateway route table.
module Network.AWS.EC2.SearchTransitGatewayRoutes
  ( -- * Creating a Request
    SearchTransitGatewayRoutes (..),
    newSearchTransitGatewayRoutes,

    -- * Request Lenses
    searchTransitGatewayRoutes_dryRun,
    searchTransitGatewayRoutes_maxResults,
    searchTransitGatewayRoutes_transitGatewayRouteTableId,
    searchTransitGatewayRoutes_filters,

    -- * Destructuring the Response
    SearchTransitGatewayRoutesResponse (..),
    newSearchTransitGatewayRoutesResponse,

    -- * Response Lenses
    searchTransitGatewayRoutesResponse_routes,
    searchTransitGatewayRoutesResponse_additionalRoutesAvailable,
    searchTransitGatewayRoutesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSearchTransitGatewayRoutes' smart constructor.
data SearchTransitGatewayRoutes = SearchTransitGatewayRoutes'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The maximum number of routes to return.
    maxResults :: Core.Maybe Core.Natural,
    -- | The ID of the transit gateway route table.
    transitGatewayRouteTableId :: Core.Text,
    -- | One or more filters. The possible values are:
    --
    -- -   @attachment.transit-gateway-attachment-id@- The id of the transit
    --     gateway attachment.
    --
    -- -   @attachment.resource-id@ - The resource id of the transit gateway
    --     attachment.
    --
    -- -   @attachment.resource-type@ - The attachment resource type. Valid
    --     values are @vpc@ | @vpn@ | @direct-connect-gateway@ | @peering@ |
    --     @connect@.
    --
    -- -   @prefix-list-id@ - The ID of the prefix list.
    --
    -- -   @route-search.exact-match@ - The exact match of the specified
    --     filter.
    --
    -- -   @route-search.longest-prefix-match@ - The longest prefix that
    --     matches the route.
    --
    -- -   @route-search.subnet-of-match@ - The routes with a subnet that match
    --     the specified CIDR filter.
    --
    -- -   @route-search.supernet-of-match@ - The routes with a CIDR that
    --     encompass the CIDR filter. For example, if you have 10.0.1.0\/29 and
    --     10.0.1.0\/31 routes in your route table and you specify
    --     supernet-of-match as 10.0.1.0\/30, then the result returns
    --     10.0.1.0\/29.
    --
    -- -   @state@ - The state of the route (@active@ | @blackhole@).
    --
    -- -   @type@ - The type of route (@propagated@ | @static@).
    filters :: [Filter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SearchTransitGatewayRoutes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'searchTransitGatewayRoutes_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'searchTransitGatewayRoutes_maxResults' - The maximum number of routes to return.
--
-- 'transitGatewayRouteTableId', 'searchTransitGatewayRoutes_transitGatewayRouteTableId' - The ID of the transit gateway route table.
--
-- 'filters', 'searchTransitGatewayRoutes_filters' - One or more filters. The possible values are:
--
-- -   @attachment.transit-gateway-attachment-id@- The id of the transit
--     gateway attachment.
--
-- -   @attachment.resource-id@ - The resource id of the transit gateway
--     attachment.
--
-- -   @attachment.resource-type@ - The attachment resource type. Valid
--     values are @vpc@ | @vpn@ | @direct-connect-gateway@ | @peering@ |
--     @connect@.
--
-- -   @prefix-list-id@ - The ID of the prefix list.
--
-- -   @route-search.exact-match@ - The exact match of the specified
--     filter.
--
-- -   @route-search.longest-prefix-match@ - The longest prefix that
--     matches the route.
--
-- -   @route-search.subnet-of-match@ - The routes with a subnet that match
--     the specified CIDR filter.
--
-- -   @route-search.supernet-of-match@ - The routes with a CIDR that
--     encompass the CIDR filter. For example, if you have 10.0.1.0\/29 and
--     10.0.1.0\/31 routes in your route table and you specify
--     supernet-of-match as 10.0.1.0\/30, then the result returns
--     10.0.1.0\/29.
--
-- -   @state@ - The state of the route (@active@ | @blackhole@).
--
-- -   @type@ - The type of route (@propagated@ | @static@).
newSearchTransitGatewayRoutes ::
  -- | 'transitGatewayRouteTableId'
  Core.Text ->
  SearchTransitGatewayRoutes
newSearchTransitGatewayRoutes
  pTransitGatewayRouteTableId_ =
    SearchTransitGatewayRoutes'
      { dryRun = Core.Nothing,
        maxResults = Core.Nothing,
        transitGatewayRouteTableId =
          pTransitGatewayRouteTableId_,
        filters = Core.mempty
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
searchTransitGatewayRoutes_dryRun :: Lens.Lens' SearchTransitGatewayRoutes (Core.Maybe Core.Bool)
searchTransitGatewayRoutes_dryRun = Lens.lens (\SearchTransitGatewayRoutes' {dryRun} -> dryRun) (\s@SearchTransitGatewayRoutes' {} a -> s {dryRun = a} :: SearchTransitGatewayRoutes)

-- | The maximum number of routes to return.
searchTransitGatewayRoutes_maxResults :: Lens.Lens' SearchTransitGatewayRoutes (Core.Maybe Core.Natural)
searchTransitGatewayRoutes_maxResults = Lens.lens (\SearchTransitGatewayRoutes' {maxResults} -> maxResults) (\s@SearchTransitGatewayRoutes' {} a -> s {maxResults = a} :: SearchTransitGatewayRoutes)

-- | The ID of the transit gateway route table.
searchTransitGatewayRoutes_transitGatewayRouteTableId :: Lens.Lens' SearchTransitGatewayRoutes Core.Text
searchTransitGatewayRoutes_transitGatewayRouteTableId = Lens.lens (\SearchTransitGatewayRoutes' {transitGatewayRouteTableId} -> transitGatewayRouteTableId) (\s@SearchTransitGatewayRoutes' {} a -> s {transitGatewayRouteTableId = a} :: SearchTransitGatewayRoutes)

-- | One or more filters. The possible values are:
--
-- -   @attachment.transit-gateway-attachment-id@- The id of the transit
--     gateway attachment.
--
-- -   @attachment.resource-id@ - The resource id of the transit gateway
--     attachment.
--
-- -   @attachment.resource-type@ - The attachment resource type. Valid
--     values are @vpc@ | @vpn@ | @direct-connect-gateway@ | @peering@ |
--     @connect@.
--
-- -   @prefix-list-id@ - The ID of the prefix list.
--
-- -   @route-search.exact-match@ - The exact match of the specified
--     filter.
--
-- -   @route-search.longest-prefix-match@ - The longest prefix that
--     matches the route.
--
-- -   @route-search.subnet-of-match@ - The routes with a subnet that match
--     the specified CIDR filter.
--
-- -   @route-search.supernet-of-match@ - The routes with a CIDR that
--     encompass the CIDR filter. For example, if you have 10.0.1.0\/29 and
--     10.0.1.0\/31 routes in your route table and you specify
--     supernet-of-match as 10.0.1.0\/30, then the result returns
--     10.0.1.0\/29.
--
-- -   @state@ - The state of the route (@active@ | @blackhole@).
--
-- -   @type@ - The type of route (@propagated@ | @static@).
searchTransitGatewayRoutes_filters :: Lens.Lens' SearchTransitGatewayRoutes [Filter]
searchTransitGatewayRoutes_filters = Lens.lens (\SearchTransitGatewayRoutes' {filters} -> filters) (\s@SearchTransitGatewayRoutes' {} a -> s {filters = a} :: SearchTransitGatewayRoutes) Core.. Lens._Coerce

instance Core.AWSRequest SearchTransitGatewayRoutes where
  type
    AWSResponse SearchTransitGatewayRoutes =
      SearchTransitGatewayRoutesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          SearchTransitGatewayRoutesResponse'
            Core.<$> ( x Core..@? "routeSet" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (x Core..@? "additionalRoutesAvailable")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable SearchTransitGatewayRoutes

instance Core.NFData SearchTransitGatewayRoutes

instance Core.ToHeaders SearchTransitGatewayRoutes where
  toHeaders = Core.const Core.mempty

instance Core.ToPath SearchTransitGatewayRoutes where
  toPath = Core.const "/"

instance Core.ToQuery SearchTransitGatewayRoutes where
  toQuery SearchTransitGatewayRoutes' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("SearchTransitGatewayRoutes" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        "TransitGatewayRouteTableId"
          Core.=: transitGatewayRouteTableId,
        Core.toQueryList "Filter" filters
      ]

-- | /See:/ 'newSearchTransitGatewayRoutesResponse' smart constructor.
data SearchTransitGatewayRoutesResponse = SearchTransitGatewayRoutesResponse'
  { -- | Information about the routes.
    routes :: Core.Maybe [TransitGatewayRoute],
    -- | Indicates whether there are additional routes available.
    additionalRoutesAvailable :: Core.Maybe Core.Bool,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SearchTransitGatewayRoutesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'routes', 'searchTransitGatewayRoutesResponse_routes' - Information about the routes.
--
-- 'additionalRoutesAvailable', 'searchTransitGatewayRoutesResponse_additionalRoutesAvailable' - Indicates whether there are additional routes available.
--
-- 'httpStatus', 'searchTransitGatewayRoutesResponse_httpStatus' - The response's http status code.
newSearchTransitGatewayRoutesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  SearchTransitGatewayRoutesResponse
newSearchTransitGatewayRoutesResponse pHttpStatus_ =
  SearchTransitGatewayRoutesResponse'
    { routes =
        Core.Nothing,
      additionalRoutesAvailable =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the routes.
searchTransitGatewayRoutesResponse_routes :: Lens.Lens' SearchTransitGatewayRoutesResponse (Core.Maybe [TransitGatewayRoute])
searchTransitGatewayRoutesResponse_routes = Lens.lens (\SearchTransitGatewayRoutesResponse' {routes} -> routes) (\s@SearchTransitGatewayRoutesResponse' {} a -> s {routes = a} :: SearchTransitGatewayRoutesResponse) Core.. Lens.mapping Lens._Coerce

-- | Indicates whether there are additional routes available.
searchTransitGatewayRoutesResponse_additionalRoutesAvailable :: Lens.Lens' SearchTransitGatewayRoutesResponse (Core.Maybe Core.Bool)
searchTransitGatewayRoutesResponse_additionalRoutesAvailable = Lens.lens (\SearchTransitGatewayRoutesResponse' {additionalRoutesAvailable} -> additionalRoutesAvailable) (\s@SearchTransitGatewayRoutesResponse' {} a -> s {additionalRoutesAvailable = a} :: SearchTransitGatewayRoutesResponse)

-- | The response's http status code.
searchTransitGatewayRoutesResponse_httpStatus :: Lens.Lens' SearchTransitGatewayRoutesResponse Core.Int
searchTransitGatewayRoutesResponse_httpStatus = Lens.lens (\SearchTransitGatewayRoutesResponse' {httpStatus} -> httpStatus) (\s@SearchTransitGatewayRoutesResponse' {} a -> s {httpStatus = a} :: SearchTransitGatewayRoutesResponse)

instance
  Core.NFData
    SearchTransitGatewayRoutesResponse
