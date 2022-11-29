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
-- Module      : Amazonka.EC2.SearchTransitGatewayRoutes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches for routes in the specified transit gateway route table.
module Amazonka.EC2.SearchTransitGatewayRoutes
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
    searchTransitGatewayRoutesResponse_additionalRoutesAvailable,
    searchTransitGatewayRoutesResponse_routes,
    searchTransitGatewayRoutesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchTransitGatewayRoutes' smart constructor.
data SearchTransitGatewayRoutes = SearchTransitGatewayRoutes'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of routes to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the transit gateway route table.
    transitGatewayRouteTableId :: Prelude.Text,
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  SearchTransitGatewayRoutes
newSearchTransitGatewayRoutes
  pTransitGatewayRouteTableId_ =
    SearchTransitGatewayRoutes'
      { dryRun =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        transitGatewayRouteTableId =
          pTransitGatewayRouteTableId_,
        filters = Prelude.mempty
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
searchTransitGatewayRoutes_dryRun :: Lens.Lens' SearchTransitGatewayRoutes (Prelude.Maybe Prelude.Bool)
searchTransitGatewayRoutes_dryRun = Lens.lens (\SearchTransitGatewayRoutes' {dryRun} -> dryRun) (\s@SearchTransitGatewayRoutes' {} a -> s {dryRun = a} :: SearchTransitGatewayRoutes)

-- | The maximum number of routes to return.
searchTransitGatewayRoutes_maxResults :: Lens.Lens' SearchTransitGatewayRoutes (Prelude.Maybe Prelude.Natural)
searchTransitGatewayRoutes_maxResults = Lens.lens (\SearchTransitGatewayRoutes' {maxResults} -> maxResults) (\s@SearchTransitGatewayRoutes' {} a -> s {maxResults = a} :: SearchTransitGatewayRoutes)

-- | The ID of the transit gateway route table.
searchTransitGatewayRoutes_transitGatewayRouteTableId :: Lens.Lens' SearchTransitGatewayRoutes Prelude.Text
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
searchTransitGatewayRoutes_filters = Lens.lens (\SearchTransitGatewayRoutes' {filters} -> filters) (\s@SearchTransitGatewayRoutes' {} a -> s {filters = a} :: SearchTransitGatewayRoutes) Prelude.. Lens.coerced

instance Core.AWSRequest SearchTransitGatewayRoutes where
  type
    AWSResponse SearchTransitGatewayRoutes =
      SearchTransitGatewayRoutesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          SearchTransitGatewayRoutesResponse'
            Prelude.<$> (x Core..@? "additionalRoutesAvailable")
            Prelude.<*> ( x Core..@? "routeSet" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchTransitGatewayRoutes where
  hashWithSalt _salt SearchTransitGatewayRoutes' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` transitGatewayRouteTableId
      `Prelude.hashWithSalt` filters

instance Prelude.NFData SearchTransitGatewayRoutes where
  rnf SearchTransitGatewayRoutes' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf transitGatewayRouteTableId
      `Prelude.seq` Prelude.rnf filters

instance Core.ToHeaders SearchTransitGatewayRoutes where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath SearchTransitGatewayRoutes where
  toPath = Prelude.const "/"

instance Core.ToQuery SearchTransitGatewayRoutes where
  toQuery SearchTransitGatewayRoutes' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("SearchTransitGatewayRoutes" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        "TransitGatewayRouteTableId"
          Core.=: transitGatewayRouteTableId,
        Core.toQueryList "Filter" filters
      ]

-- | /See:/ 'newSearchTransitGatewayRoutesResponse' smart constructor.
data SearchTransitGatewayRoutesResponse = SearchTransitGatewayRoutesResponse'
  { -- | Indicates whether there are additional routes available.
    additionalRoutesAvailable :: Prelude.Maybe Prelude.Bool,
    -- | Information about the routes.
    routes :: Prelude.Maybe [TransitGatewayRoute],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchTransitGatewayRoutesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalRoutesAvailable', 'searchTransitGatewayRoutesResponse_additionalRoutesAvailable' - Indicates whether there are additional routes available.
--
-- 'routes', 'searchTransitGatewayRoutesResponse_routes' - Information about the routes.
--
-- 'httpStatus', 'searchTransitGatewayRoutesResponse_httpStatus' - The response's http status code.
newSearchTransitGatewayRoutesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchTransitGatewayRoutesResponse
newSearchTransitGatewayRoutesResponse pHttpStatus_ =
  SearchTransitGatewayRoutesResponse'
    { additionalRoutesAvailable =
        Prelude.Nothing,
      routes = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Indicates whether there are additional routes available.
searchTransitGatewayRoutesResponse_additionalRoutesAvailable :: Lens.Lens' SearchTransitGatewayRoutesResponse (Prelude.Maybe Prelude.Bool)
searchTransitGatewayRoutesResponse_additionalRoutesAvailable = Lens.lens (\SearchTransitGatewayRoutesResponse' {additionalRoutesAvailable} -> additionalRoutesAvailable) (\s@SearchTransitGatewayRoutesResponse' {} a -> s {additionalRoutesAvailable = a} :: SearchTransitGatewayRoutesResponse)

-- | Information about the routes.
searchTransitGatewayRoutesResponse_routes :: Lens.Lens' SearchTransitGatewayRoutesResponse (Prelude.Maybe [TransitGatewayRoute])
searchTransitGatewayRoutesResponse_routes = Lens.lens (\SearchTransitGatewayRoutesResponse' {routes} -> routes) (\s@SearchTransitGatewayRoutesResponse' {} a -> s {routes = a} :: SearchTransitGatewayRoutesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
searchTransitGatewayRoutesResponse_httpStatus :: Lens.Lens' SearchTransitGatewayRoutesResponse Prelude.Int
searchTransitGatewayRoutesResponse_httpStatus = Lens.lens (\SearchTransitGatewayRoutesResponse' {httpStatus} -> httpStatus) (\s@SearchTransitGatewayRoutesResponse' {} a -> s {httpStatus = a} :: SearchTransitGatewayRoutesResponse)

instance
  Prelude.NFData
    SearchTransitGatewayRoutesResponse
  where
  rnf SearchTransitGatewayRoutesResponse' {..} =
    Prelude.rnf additionalRoutesAvailable
      `Prelude.seq` Prelude.rnf routes
      `Prelude.seq` Prelude.rnf httpStatus
