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
-- Module      : Network.AWS.EC2.DescribeClientVpnRoutes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the routes for the specified Client VPN endpoint.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeClientVpnRoutes
  ( -- * Creating a Request
    DescribeClientVpnRoutes (..),
    newDescribeClientVpnRoutes,

    -- * Request Lenses
    describeClientVpnRoutes_nextToken,
    describeClientVpnRoutes_dryRun,
    describeClientVpnRoutes_maxResults,
    describeClientVpnRoutes_filters,
    describeClientVpnRoutes_clientVpnEndpointId,

    -- * Destructuring the Response
    DescribeClientVpnRoutesResponse (..),
    newDescribeClientVpnRoutesResponse,

    -- * Response Lenses
    describeClientVpnRoutesResponse_nextToken,
    describeClientVpnRoutesResponse_routes,
    describeClientVpnRoutesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeClientVpnRoutes' smart constructor.
data DescribeClientVpnRoutes = DescribeClientVpnRoutes'
  { -- | The token to retrieve the next page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The maximum number of results to return for the request in a single
    -- page. The remaining results can be seen by sending another request with
    -- the nextToken value.
    maxResults :: Core.Maybe Core.Natural,
    -- | One or more filters. Filter names and values are case-sensitive.
    --
    -- -   @destination-cidr@ - The CIDR of the route destination.
    --
    -- -   @origin@ - How the route was associated with the Client VPN endpoint
    --     (@associate@ | @add-route@).
    --
    -- -   @target-subnet@ - The ID of the subnet through which traffic is
    --     routed.
    filters :: Core.Maybe [Filter],
    -- | The ID of the Client VPN endpoint.
    clientVpnEndpointId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeClientVpnRoutes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeClientVpnRoutes_nextToken' - The token to retrieve the next page of results.
--
-- 'dryRun', 'describeClientVpnRoutes_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeClientVpnRoutes_maxResults' - The maximum number of results to return for the request in a single
-- page. The remaining results can be seen by sending another request with
-- the nextToken value.
--
-- 'filters', 'describeClientVpnRoutes_filters' - One or more filters. Filter names and values are case-sensitive.
--
-- -   @destination-cidr@ - The CIDR of the route destination.
--
-- -   @origin@ - How the route was associated with the Client VPN endpoint
--     (@associate@ | @add-route@).
--
-- -   @target-subnet@ - The ID of the subnet through which traffic is
--     routed.
--
-- 'clientVpnEndpointId', 'describeClientVpnRoutes_clientVpnEndpointId' - The ID of the Client VPN endpoint.
newDescribeClientVpnRoutes ::
  -- | 'clientVpnEndpointId'
  Core.Text ->
  DescribeClientVpnRoutes
newDescribeClientVpnRoutes pClientVpnEndpointId_ =
  DescribeClientVpnRoutes'
    { nextToken = Core.Nothing,
      dryRun = Core.Nothing,
      maxResults = Core.Nothing,
      filters = Core.Nothing,
      clientVpnEndpointId = pClientVpnEndpointId_
    }

-- | The token to retrieve the next page of results.
describeClientVpnRoutes_nextToken :: Lens.Lens' DescribeClientVpnRoutes (Core.Maybe Core.Text)
describeClientVpnRoutes_nextToken = Lens.lens (\DescribeClientVpnRoutes' {nextToken} -> nextToken) (\s@DescribeClientVpnRoutes' {} a -> s {nextToken = a} :: DescribeClientVpnRoutes)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeClientVpnRoutes_dryRun :: Lens.Lens' DescribeClientVpnRoutes (Core.Maybe Core.Bool)
describeClientVpnRoutes_dryRun = Lens.lens (\DescribeClientVpnRoutes' {dryRun} -> dryRun) (\s@DescribeClientVpnRoutes' {} a -> s {dryRun = a} :: DescribeClientVpnRoutes)

-- | The maximum number of results to return for the request in a single
-- page. The remaining results can be seen by sending another request with
-- the nextToken value.
describeClientVpnRoutes_maxResults :: Lens.Lens' DescribeClientVpnRoutes (Core.Maybe Core.Natural)
describeClientVpnRoutes_maxResults = Lens.lens (\DescribeClientVpnRoutes' {maxResults} -> maxResults) (\s@DescribeClientVpnRoutes' {} a -> s {maxResults = a} :: DescribeClientVpnRoutes)

-- | One or more filters. Filter names and values are case-sensitive.
--
-- -   @destination-cidr@ - The CIDR of the route destination.
--
-- -   @origin@ - How the route was associated with the Client VPN endpoint
--     (@associate@ | @add-route@).
--
-- -   @target-subnet@ - The ID of the subnet through which traffic is
--     routed.
describeClientVpnRoutes_filters :: Lens.Lens' DescribeClientVpnRoutes (Core.Maybe [Filter])
describeClientVpnRoutes_filters = Lens.lens (\DescribeClientVpnRoutes' {filters} -> filters) (\s@DescribeClientVpnRoutes' {} a -> s {filters = a} :: DescribeClientVpnRoutes) Core.. Lens.mapping Lens._Coerce

-- | The ID of the Client VPN endpoint.
describeClientVpnRoutes_clientVpnEndpointId :: Lens.Lens' DescribeClientVpnRoutes Core.Text
describeClientVpnRoutes_clientVpnEndpointId = Lens.lens (\DescribeClientVpnRoutes' {clientVpnEndpointId} -> clientVpnEndpointId) (\s@DescribeClientVpnRoutes' {} a -> s {clientVpnEndpointId = a} :: DescribeClientVpnRoutes)

instance Core.AWSPager DescribeClientVpnRoutes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeClientVpnRoutesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeClientVpnRoutesResponse_routes
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeClientVpnRoutes_nextToken
          Lens..~ rs
          Lens.^? describeClientVpnRoutesResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeClientVpnRoutes where
  type
    AWSResponse DescribeClientVpnRoutes =
      DescribeClientVpnRoutesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeClientVpnRoutesResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> ( x Core..@? "routes" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeClientVpnRoutes

instance Core.NFData DescribeClientVpnRoutes

instance Core.ToHeaders DescribeClientVpnRoutes where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeClientVpnRoutes where
  toPath = Core.const "/"

instance Core.ToQuery DescribeClientVpnRoutes where
  toQuery DescribeClientVpnRoutes' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeClientVpnRoutes" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          (Core.toQueryList "Filter" Core.<$> filters),
        "ClientVpnEndpointId" Core.=: clientVpnEndpointId
      ]

-- | /See:/ 'newDescribeClientVpnRoutesResponse' smart constructor.
data DescribeClientVpnRoutesResponse = DescribeClientVpnRoutesResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the Client VPN endpoint routes.
    routes :: Core.Maybe [ClientVpnRoute],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeClientVpnRoutesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeClientVpnRoutesResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'routes', 'describeClientVpnRoutesResponse_routes' - Information about the Client VPN endpoint routes.
--
-- 'httpStatus', 'describeClientVpnRoutesResponse_httpStatus' - The response's http status code.
newDescribeClientVpnRoutesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeClientVpnRoutesResponse
newDescribeClientVpnRoutesResponse pHttpStatus_ =
  DescribeClientVpnRoutesResponse'
    { nextToken =
        Core.Nothing,
      routes = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeClientVpnRoutesResponse_nextToken :: Lens.Lens' DescribeClientVpnRoutesResponse (Core.Maybe Core.Text)
describeClientVpnRoutesResponse_nextToken = Lens.lens (\DescribeClientVpnRoutesResponse' {nextToken} -> nextToken) (\s@DescribeClientVpnRoutesResponse' {} a -> s {nextToken = a} :: DescribeClientVpnRoutesResponse)

-- | Information about the Client VPN endpoint routes.
describeClientVpnRoutesResponse_routes :: Lens.Lens' DescribeClientVpnRoutesResponse (Core.Maybe [ClientVpnRoute])
describeClientVpnRoutesResponse_routes = Lens.lens (\DescribeClientVpnRoutesResponse' {routes} -> routes) (\s@DescribeClientVpnRoutesResponse' {} a -> s {routes = a} :: DescribeClientVpnRoutesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeClientVpnRoutesResponse_httpStatus :: Lens.Lens' DescribeClientVpnRoutesResponse Core.Int
describeClientVpnRoutesResponse_httpStatus = Lens.lens (\DescribeClientVpnRoutesResponse' {httpStatus} -> httpStatus) (\s@DescribeClientVpnRoutesResponse' {} a -> s {httpStatus = a} :: DescribeClientVpnRoutesResponse)

instance Core.NFData DescribeClientVpnRoutesResponse
