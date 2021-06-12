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
-- Module      : Network.AWS.EC2.SearchLocalGatewayRoutes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches for routes in the specified local gateway route table.
--
-- This operation returns paginated results.
module Network.AWS.EC2.SearchLocalGatewayRoutes
  ( -- * Creating a Request
    SearchLocalGatewayRoutes (..),
    newSearchLocalGatewayRoutes,

    -- * Request Lenses
    searchLocalGatewayRoutes_nextToken,
    searchLocalGatewayRoutes_dryRun,
    searchLocalGatewayRoutes_maxResults,
    searchLocalGatewayRoutes_localGatewayRouteTableId,
    searchLocalGatewayRoutes_filters,

    -- * Destructuring the Response
    SearchLocalGatewayRoutesResponse (..),
    newSearchLocalGatewayRoutesResponse,

    -- * Response Lenses
    searchLocalGatewayRoutesResponse_nextToken,
    searchLocalGatewayRoutesResponse_routes,
    searchLocalGatewayRoutesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSearchLocalGatewayRoutes' smart constructor.
data SearchLocalGatewayRoutes = SearchLocalGatewayRoutes'
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
    maxResults :: Core.Maybe Core.Int,
    -- | The ID of the local gateway route table.
    localGatewayRouteTableId :: Core.Text,
    -- | One or more filters.
    filters :: [Filter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SearchLocalGatewayRoutes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchLocalGatewayRoutes_nextToken' - The token for the next page of results.
--
-- 'dryRun', 'searchLocalGatewayRoutes_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'searchLocalGatewayRoutes_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'localGatewayRouteTableId', 'searchLocalGatewayRoutes_localGatewayRouteTableId' - The ID of the local gateway route table.
--
-- 'filters', 'searchLocalGatewayRoutes_filters' - One or more filters.
newSearchLocalGatewayRoutes ::
  -- | 'localGatewayRouteTableId'
  Core.Text ->
  SearchLocalGatewayRoutes
newSearchLocalGatewayRoutes
  pLocalGatewayRouteTableId_ =
    SearchLocalGatewayRoutes'
      { nextToken = Core.Nothing,
        dryRun = Core.Nothing,
        maxResults = Core.Nothing,
        localGatewayRouteTableId =
          pLocalGatewayRouteTableId_,
        filters = Core.mempty
      }

-- | The token for the next page of results.
searchLocalGatewayRoutes_nextToken :: Lens.Lens' SearchLocalGatewayRoutes (Core.Maybe Core.Text)
searchLocalGatewayRoutes_nextToken = Lens.lens (\SearchLocalGatewayRoutes' {nextToken} -> nextToken) (\s@SearchLocalGatewayRoutes' {} a -> s {nextToken = a} :: SearchLocalGatewayRoutes)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
searchLocalGatewayRoutes_dryRun :: Lens.Lens' SearchLocalGatewayRoutes (Core.Maybe Core.Bool)
searchLocalGatewayRoutes_dryRun = Lens.lens (\SearchLocalGatewayRoutes' {dryRun} -> dryRun) (\s@SearchLocalGatewayRoutes' {} a -> s {dryRun = a} :: SearchLocalGatewayRoutes)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
searchLocalGatewayRoutes_maxResults :: Lens.Lens' SearchLocalGatewayRoutes (Core.Maybe Core.Int)
searchLocalGatewayRoutes_maxResults = Lens.lens (\SearchLocalGatewayRoutes' {maxResults} -> maxResults) (\s@SearchLocalGatewayRoutes' {} a -> s {maxResults = a} :: SearchLocalGatewayRoutes)

-- | The ID of the local gateway route table.
searchLocalGatewayRoutes_localGatewayRouteTableId :: Lens.Lens' SearchLocalGatewayRoutes Core.Text
searchLocalGatewayRoutes_localGatewayRouteTableId = Lens.lens (\SearchLocalGatewayRoutes' {localGatewayRouteTableId} -> localGatewayRouteTableId) (\s@SearchLocalGatewayRoutes' {} a -> s {localGatewayRouteTableId = a} :: SearchLocalGatewayRoutes)

-- | One or more filters.
searchLocalGatewayRoutes_filters :: Lens.Lens' SearchLocalGatewayRoutes [Filter]
searchLocalGatewayRoutes_filters = Lens.lens (\SearchLocalGatewayRoutes' {filters} -> filters) (\s@SearchLocalGatewayRoutes' {} a -> s {filters = a} :: SearchLocalGatewayRoutes) Core.. Lens._Coerce

instance Core.AWSPager SearchLocalGatewayRoutes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? searchLocalGatewayRoutesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? searchLocalGatewayRoutesResponse_routes
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& searchLocalGatewayRoutes_nextToken
          Lens..~ rs
          Lens.^? searchLocalGatewayRoutesResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest SearchLocalGatewayRoutes where
  type
    AWSResponse SearchLocalGatewayRoutes =
      SearchLocalGatewayRoutesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          SearchLocalGatewayRoutesResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> ( x Core..@? "routeSet" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable SearchLocalGatewayRoutes

instance Core.NFData SearchLocalGatewayRoutes

instance Core.ToHeaders SearchLocalGatewayRoutes where
  toHeaders = Core.const Core.mempty

instance Core.ToPath SearchLocalGatewayRoutes where
  toPath = Core.const "/"

instance Core.ToQuery SearchLocalGatewayRoutes where
  toQuery SearchLocalGatewayRoutes' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("SearchLocalGatewayRoutes" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        "LocalGatewayRouteTableId"
          Core.=: localGatewayRouteTableId,
        Core.toQueryList "Filter" filters
      ]

-- | /See:/ 'newSearchLocalGatewayRoutesResponse' smart constructor.
data SearchLocalGatewayRoutesResponse = SearchLocalGatewayRoutesResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the routes.
    routes :: Core.Maybe [LocalGatewayRoute],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SearchLocalGatewayRoutesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchLocalGatewayRoutesResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'routes', 'searchLocalGatewayRoutesResponse_routes' - Information about the routes.
--
-- 'httpStatus', 'searchLocalGatewayRoutesResponse_httpStatus' - The response's http status code.
newSearchLocalGatewayRoutesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  SearchLocalGatewayRoutesResponse
newSearchLocalGatewayRoutesResponse pHttpStatus_ =
  SearchLocalGatewayRoutesResponse'
    { nextToken =
        Core.Nothing,
      routes = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
searchLocalGatewayRoutesResponse_nextToken :: Lens.Lens' SearchLocalGatewayRoutesResponse (Core.Maybe Core.Text)
searchLocalGatewayRoutesResponse_nextToken = Lens.lens (\SearchLocalGatewayRoutesResponse' {nextToken} -> nextToken) (\s@SearchLocalGatewayRoutesResponse' {} a -> s {nextToken = a} :: SearchLocalGatewayRoutesResponse)

-- | Information about the routes.
searchLocalGatewayRoutesResponse_routes :: Lens.Lens' SearchLocalGatewayRoutesResponse (Core.Maybe [LocalGatewayRoute])
searchLocalGatewayRoutesResponse_routes = Lens.lens (\SearchLocalGatewayRoutesResponse' {routes} -> routes) (\s@SearchLocalGatewayRoutesResponse' {} a -> s {routes = a} :: SearchLocalGatewayRoutesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
searchLocalGatewayRoutesResponse_httpStatus :: Lens.Lens' SearchLocalGatewayRoutesResponse Core.Int
searchLocalGatewayRoutesResponse_httpStatus = Lens.lens (\SearchLocalGatewayRoutesResponse' {httpStatus} -> httpStatus) (\s@SearchLocalGatewayRoutesResponse' {} a -> s {httpStatus = a} :: SearchLocalGatewayRoutesResponse)

instance Core.NFData SearchLocalGatewayRoutesResponse
