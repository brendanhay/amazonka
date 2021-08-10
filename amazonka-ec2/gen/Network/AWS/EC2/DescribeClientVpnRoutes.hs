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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeClientVpnRoutes' smart constructor.
data DescribeClientVpnRoutes = DescribeClientVpnRoutes'
  { -- | The token to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return for the request in a single
    -- page. The remaining results can be seen by sending another request with
    -- the nextToken value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | One or more filters. Filter names and values are case-sensitive.
    --
    -- -   @destination-cidr@ - The CIDR of the route destination.
    --
    -- -   @origin@ - How the route was associated with the Client VPN endpoint
    --     (@associate@ | @add-route@).
    --
    -- -   @target-subnet@ - The ID of the subnet through which traffic is
    --     routed.
    filters :: Prelude.Maybe [Filter],
    -- | The ID of the Client VPN endpoint.
    clientVpnEndpointId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DescribeClientVpnRoutes
newDescribeClientVpnRoutes pClientVpnEndpointId_ =
  DescribeClientVpnRoutes'
    { nextToken =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      filters = Prelude.Nothing,
      clientVpnEndpointId = pClientVpnEndpointId_
    }

-- | The token to retrieve the next page of results.
describeClientVpnRoutes_nextToken :: Lens.Lens' DescribeClientVpnRoutes (Prelude.Maybe Prelude.Text)
describeClientVpnRoutes_nextToken = Lens.lens (\DescribeClientVpnRoutes' {nextToken} -> nextToken) (\s@DescribeClientVpnRoutes' {} a -> s {nextToken = a} :: DescribeClientVpnRoutes)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeClientVpnRoutes_dryRun :: Lens.Lens' DescribeClientVpnRoutes (Prelude.Maybe Prelude.Bool)
describeClientVpnRoutes_dryRun = Lens.lens (\DescribeClientVpnRoutes' {dryRun} -> dryRun) (\s@DescribeClientVpnRoutes' {} a -> s {dryRun = a} :: DescribeClientVpnRoutes)

-- | The maximum number of results to return for the request in a single
-- page. The remaining results can be seen by sending another request with
-- the nextToken value.
describeClientVpnRoutes_maxResults :: Lens.Lens' DescribeClientVpnRoutes (Prelude.Maybe Prelude.Natural)
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
describeClientVpnRoutes_filters :: Lens.Lens' DescribeClientVpnRoutes (Prelude.Maybe [Filter])
describeClientVpnRoutes_filters = Lens.lens (\DescribeClientVpnRoutes' {filters} -> filters) (\s@DescribeClientVpnRoutes' {} a -> s {filters = a} :: DescribeClientVpnRoutes) Prelude.. Lens.mapping Lens._Coerce

-- | The ID of the Client VPN endpoint.
describeClientVpnRoutes_clientVpnEndpointId :: Lens.Lens' DescribeClientVpnRoutes Prelude.Text
describeClientVpnRoutes_clientVpnEndpointId = Lens.lens (\DescribeClientVpnRoutes' {clientVpnEndpointId} -> clientVpnEndpointId) (\s@DescribeClientVpnRoutes' {} a -> s {clientVpnEndpointId = a} :: DescribeClientVpnRoutes)

instance Core.AWSPager DescribeClientVpnRoutes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeClientVpnRoutesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeClientVpnRoutesResponse_routes
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeClientVpnRoutes_nextToken
          Lens..~ rs
          Lens.^? describeClientVpnRoutesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeClientVpnRoutes where
  type
    AWSResponse DescribeClientVpnRoutes =
      DescribeClientVpnRoutesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeClientVpnRoutesResponse'
            Prelude.<$> (x Core..@? "nextToken")
            Prelude.<*> ( x Core..@? "routes" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeClientVpnRoutes

instance Prelude.NFData DescribeClientVpnRoutes

instance Core.ToHeaders DescribeClientVpnRoutes where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeClientVpnRoutes where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeClientVpnRoutes where
  toQuery DescribeClientVpnRoutes' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeClientVpnRoutes" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          (Core.toQueryList "Filter" Prelude.<$> filters),
        "ClientVpnEndpointId" Core.=: clientVpnEndpointId
      ]

-- | /See:/ 'newDescribeClientVpnRoutesResponse' smart constructor.
data DescribeClientVpnRoutesResponse = DescribeClientVpnRoutesResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the Client VPN endpoint routes.
    routes :: Prelude.Maybe [ClientVpnRoute],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeClientVpnRoutesResponse
newDescribeClientVpnRoutesResponse pHttpStatus_ =
  DescribeClientVpnRoutesResponse'
    { nextToken =
        Prelude.Nothing,
      routes = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeClientVpnRoutesResponse_nextToken :: Lens.Lens' DescribeClientVpnRoutesResponse (Prelude.Maybe Prelude.Text)
describeClientVpnRoutesResponse_nextToken = Lens.lens (\DescribeClientVpnRoutesResponse' {nextToken} -> nextToken) (\s@DescribeClientVpnRoutesResponse' {} a -> s {nextToken = a} :: DescribeClientVpnRoutesResponse)

-- | Information about the Client VPN endpoint routes.
describeClientVpnRoutesResponse_routes :: Lens.Lens' DescribeClientVpnRoutesResponse (Prelude.Maybe [ClientVpnRoute])
describeClientVpnRoutesResponse_routes = Lens.lens (\DescribeClientVpnRoutesResponse' {routes} -> routes) (\s@DescribeClientVpnRoutesResponse' {} a -> s {routes = a} :: DescribeClientVpnRoutesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeClientVpnRoutesResponse_httpStatus :: Lens.Lens' DescribeClientVpnRoutesResponse Prelude.Int
describeClientVpnRoutesResponse_httpStatus = Lens.lens (\DescribeClientVpnRoutesResponse' {httpStatus} -> httpStatus) (\s@DescribeClientVpnRoutesResponse' {} a -> s {httpStatus = a} :: DescribeClientVpnRoutesResponse)

instance
  Prelude.NFData
    DescribeClientVpnRoutesResponse
