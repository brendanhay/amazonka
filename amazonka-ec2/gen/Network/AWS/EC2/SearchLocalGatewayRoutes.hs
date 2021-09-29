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
    searchLocalGatewayRoutes_maxResults,
    searchLocalGatewayRoutes_dryRun,
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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSearchLocalGatewayRoutes' smart constructor.
data SearchLocalGatewayRoutes = SearchLocalGatewayRoutes'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the local gateway route table.
    localGatewayRouteTableId :: Prelude.Text,
    -- | One or more filters.
    filters :: [Filter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'maxResults', 'searchLocalGatewayRoutes_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'dryRun', 'searchLocalGatewayRoutes_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'localGatewayRouteTableId', 'searchLocalGatewayRoutes_localGatewayRouteTableId' - The ID of the local gateway route table.
--
-- 'filters', 'searchLocalGatewayRoutes_filters' - One or more filters.
newSearchLocalGatewayRoutes ::
  -- | 'localGatewayRouteTableId'
  Prelude.Text ->
  SearchLocalGatewayRoutes
newSearchLocalGatewayRoutes
  pLocalGatewayRouteTableId_ =
    SearchLocalGatewayRoutes'
      { nextToken =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        dryRun = Prelude.Nothing,
        localGatewayRouteTableId =
          pLocalGatewayRouteTableId_,
        filters = Prelude.mempty
      }

-- | The token for the next page of results.
searchLocalGatewayRoutes_nextToken :: Lens.Lens' SearchLocalGatewayRoutes (Prelude.Maybe Prelude.Text)
searchLocalGatewayRoutes_nextToken = Lens.lens (\SearchLocalGatewayRoutes' {nextToken} -> nextToken) (\s@SearchLocalGatewayRoutes' {} a -> s {nextToken = a} :: SearchLocalGatewayRoutes)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
searchLocalGatewayRoutes_maxResults :: Lens.Lens' SearchLocalGatewayRoutes (Prelude.Maybe Prelude.Int)
searchLocalGatewayRoutes_maxResults = Lens.lens (\SearchLocalGatewayRoutes' {maxResults} -> maxResults) (\s@SearchLocalGatewayRoutes' {} a -> s {maxResults = a} :: SearchLocalGatewayRoutes)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
searchLocalGatewayRoutes_dryRun :: Lens.Lens' SearchLocalGatewayRoutes (Prelude.Maybe Prelude.Bool)
searchLocalGatewayRoutes_dryRun = Lens.lens (\SearchLocalGatewayRoutes' {dryRun} -> dryRun) (\s@SearchLocalGatewayRoutes' {} a -> s {dryRun = a} :: SearchLocalGatewayRoutes)

-- | The ID of the local gateway route table.
searchLocalGatewayRoutes_localGatewayRouteTableId :: Lens.Lens' SearchLocalGatewayRoutes Prelude.Text
searchLocalGatewayRoutes_localGatewayRouteTableId = Lens.lens (\SearchLocalGatewayRoutes' {localGatewayRouteTableId} -> localGatewayRouteTableId) (\s@SearchLocalGatewayRoutes' {} a -> s {localGatewayRouteTableId = a} :: SearchLocalGatewayRoutes)

-- | One or more filters.
searchLocalGatewayRoutes_filters :: Lens.Lens' SearchLocalGatewayRoutes [Filter]
searchLocalGatewayRoutes_filters = Lens.lens (\SearchLocalGatewayRoutes' {filters} -> filters) (\s@SearchLocalGatewayRoutes' {} a -> s {filters = a} :: SearchLocalGatewayRoutes) Prelude.. Lens._Coerce

instance Core.AWSPager SearchLocalGatewayRoutes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? searchLocalGatewayRoutesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? searchLocalGatewayRoutesResponse_routes
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& searchLocalGatewayRoutes_nextToken
          Lens..~ rs
          Lens.^? searchLocalGatewayRoutesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest SearchLocalGatewayRoutes where
  type
    AWSResponse SearchLocalGatewayRoutes =
      SearchLocalGatewayRoutesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          SearchLocalGatewayRoutesResponse'
            Prelude.<$> (x Core..@? "nextToken")
            Prelude.<*> ( x Core..@? "routeSet" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchLocalGatewayRoutes

instance Prelude.NFData SearchLocalGatewayRoutes

instance Core.ToHeaders SearchLocalGatewayRoutes where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath SearchLocalGatewayRoutes where
  toPath = Prelude.const "/"

instance Core.ToQuery SearchLocalGatewayRoutes where
  toQuery SearchLocalGatewayRoutes' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("SearchLocalGatewayRoutes" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        "MaxResults" Core.=: maxResults,
        "DryRun" Core.=: dryRun,
        "LocalGatewayRouteTableId"
          Core.=: localGatewayRouteTableId,
        Core.toQueryList "Filter" filters
      ]

-- | /See:/ 'newSearchLocalGatewayRoutesResponse' smart constructor.
data SearchLocalGatewayRoutesResponse = SearchLocalGatewayRoutesResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the routes.
    routes :: Prelude.Maybe [LocalGatewayRoute],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  SearchLocalGatewayRoutesResponse
newSearchLocalGatewayRoutesResponse pHttpStatus_ =
  SearchLocalGatewayRoutesResponse'
    { nextToken =
        Prelude.Nothing,
      routes = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
searchLocalGatewayRoutesResponse_nextToken :: Lens.Lens' SearchLocalGatewayRoutesResponse (Prelude.Maybe Prelude.Text)
searchLocalGatewayRoutesResponse_nextToken = Lens.lens (\SearchLocalGatewayRoutesResponse' {nextToken} -> nextToken) (\s@SearchLocalGatewayRoutesResponse' {} a -> s {nextToken = a} :: SearchLocalGatewayRoutesResponse)

-- | Information about the routes.
searchLocalGatewayRoutesResponse_routes :: Lens.Lens' SearchLocalGatewayRoutesResponse (Prelude.Maybe [LocalGatewayRoute])
searchLocalGatewayRoutesResponse_routes = Lens.lens (\SearchLocalGatewayRoutesResponse' {routes} -> routes) (\s@SearchLocalGatewayRoutesResponse' {} a -> s {routes = a} :: SearchLocalGatewayRoutesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
searchLocalGatewayRoutesResponse_httpStatus :: Lens.Lens' SearchLocalGatewayRoutesResponse Prelude.Int
searchLocalGatewayRoutesResponse_httpStatus = Lens.lens (\SearchLocalGatewayRoutesResponse' {httpStatus} -> httpStatus) (\s@SearchLocalGatewayRoutesResponse' {} a -> s {httpStatus = a} :: SearchLocalGatewayRoutesResponse)

instance
  Prelude.NFData
    SearchLocalGatewayRoutesResponse
