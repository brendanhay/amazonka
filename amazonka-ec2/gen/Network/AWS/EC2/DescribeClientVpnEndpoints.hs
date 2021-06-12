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
-- Module      : Network.AWS.EC2.DescribeClientVpnEndpoints
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more Client VPN endpoints in the account.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeClientVpnEndpoints
  ( -- * Creating a Request
    DescribeClientVpnEndpoints (..),
    newDescribeClientVpnEndpoints,

    -- * Request Lenses
    describeClientVpnEndpoints_nextToken,
    describeClientVpnEndpoints_dryRun,
    describeClientVpnEndpoints_maxResults,
    describeClientVpnEndpoints_clientVpnEndpointIds,
    describeClientVpnEndpoints_filters,

    -- * Destructuring the Response
    DescribeClientVpnEndpointsResponse (..),
    newDescribeClientVpnEndpointsResponse,

    -- * Response Lenses
    describeClientVpnEndpointsResponse_nextToken,
    describeClientVpnEndpointsResponse_clientVpnEndpoints,
    describeClientVpnEndpointsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeClientVpnEndpoints' smart constructor.
data DescribeClientVpnEndpoints = DescribeClientVpnEndpoints'
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
    -- | The ID of the Client VPN endpoint.
    clientVpnEndpointIds :: Core.Maybe [Core.Text],
    -- | One or more filters. Filter names and values are case-sensitive.
    --
    -- -   @endpoint-id@ - The ID of the Client VPN endpoint.
    --
    -- -   @transport-protocol@ - The transport protocol (@tcp@ | @udp@).
    filters :: Core.Maybe [Filter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeClientVpnEndpoints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeClientVpnEndpoints_nextToken' - The token to retrieve the next page of results.
--
-- 'dryRun', 'describeClientVpnEndpoints_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeClientVpnEndpoints_maxResults' - The maximum number of results to return for the request in a single
-- page. The remaining results can be seen by sending another request with
-- the nextToken value.
--
-- 'clientVpnEndpointIds', 'describeClientVpnEndpoints_clientVpnEndpointIds' - The ID of the Client VPN endpoint.
--
-- 'filters', 'describeClientVpnEndpoints_filters' - One or more filters. Filter names and values are case-sensitive.
--
-- -   @endpoint-id@ - The ID of the Client VPN endpoint.
--
-- -   @transport-protocol@ - The transport protocol (@tcp@ | @udp@).
newDescribeClientVpnEndpoints ::
  DescribeClientVpnEndpoints
newDescribeClientVpnEndpoints =
  DescribeClientVpnEndpoints'
    { nextToken =
        Core.Nothing,
      dryRun = Core.Nothing,
      maxResults = Core.Nothing,
      clientVpnEndpointIds = Core.Nothing,
      filters = Core.Nothing
    }

-- | The token to retrieve the next page of results.
describeClientVpnEndpoints_nextToken :: Lens.Lens' DescribeClientVpnEndpoints (Core.Maybe Core.Text)
describeClientVpnEndpoints_nextToken = Lens.lens (\DescribeClientVpnEndpoints' {nextToken} -> nextToken) (\s@DescribeClientVpnEndpoints' {} a -> s {nextToken = a} :: DescribeClientVpnEndpoints)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeClientVpnEndpoints_dryRun :: Lens.Lens' DescribeClientVpnEndpoints (Core.Maybe Core.Bool)
describeClientVpnEndpoints_dryRun = Lens.lens (\DescribeClientVpnEndpoints' {dryRun} -> dryRun) (\s@DescribeClientVpnEndpoints' {} a -> s {dryRun = a} :: DescribeClientVpnEndpoints)

-- | The maximum number of results to return for the request in a single
-- page. The remaining results can be seen by sending another request with
-- the nextToken value.
describeClientVpnEndpoints_maxResults :: Lens.Lens' DescribeClientVpnEndpoints (Core.Maybe Core.Natural)
describeClientVpnEndpoints_maxResults = Lens.lens (\DescribeClientVpnEndpoints' {maxResults} -> maxResults) (\s@DescribeClientVpnEndpoints' {} a -> s {maxResults = a} :: DescribeClientVpnEndpoints)

-- | The ID of the Client VPN endpoint.
describeClientVpnEndpoints_clientVpnEndpointIds :: Lens.Lens' DescribeClientVpnEndpoints (Core.Maybe [Core.Text])
describeClientVpnEndpoints_clientVpnEndpointIds = Lens.lens (\DescribeClientVpnEndpoints' {clientVpnEndpointIds} -> clientVpnEndpointIds) (\s@DescribeClientVpnEndpoints' {} a -> s {clientVpnEndpointIds = a} :: DescribeClientVpnEndpoints) Core.. Lens.mapping Lens._Coerce

-- | One or more filters. Filter names and values are case-sensitive.
--
-- -   @endpoint-id@ - The ID of the Client VPN endpoint.
--
-- -   @transport-protocol@ - The transport protocol (@tcp@ | @udp@).
describeClientVpnEndpoints_filters :: Lens.Lens' DescribeClientVpnEndpoints (Core.Maybe [Filter])
describeClientVpnEndpoints_filters = Lens.lens (\DescribeClientVpnEndpoints' {filters} -> filters) (\s@DescribeClientVpnEndpoints' {} a -> s {filters = a} :: DescribeClientVpnEndpoints) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeClientVpnEndpoints where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeClientVpnEndpointsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeClientVpnEndpointsResponse_clientVpnEndpoints
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeClientVpnEndpoints_nextToken
          Lens..~ rs
          Lens.^? describeClientVpnEndpointsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeClientVpnEndpoints where
  type
    AWSResponse DescribeClientVpnEndpoints =
      DescribeClientVpnEndpointsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeClientVpnEndpointsResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> ( x Core..@? "clientVpnEndpoint" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeClientVpnEndpoints

instance Core.NFData DescribeClientVpnEndpoints

instance Core.ToHeaders DescribeClientVpnEndpoints where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeClientVpnEndpoints where
  toPath = Core.const "/"

instance Core.ToQuery DescribeClientVpnEndpoints where
  toQuery DescribeClientVpnEndpoints' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeClientVpnEndpoints" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          ( Core.toQueryList "ClientVpnEndpointId"
              Core.<$> clientVpnEndpointIds
          ),
        Core.toQuery
          (Core.toQueryList "Filter" Core.<$> filters)
      ]

-- | /See:/ 'newDescribeClientVpnEndpointsResponse' smart constructor.
data DescribeClientVpnEndpointsResponse = DescribeClientVpnEndpointsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the Client VPN endpoints.
    clientVpnEndpoints :: Core.Maybe [ClientVpnEndpoint],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeClientVpnEndpointsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeClientVpnEndpointsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'clientVpnEndpoints', 'describeClientVpnEndpointsResponse_clientVpnEndpoints' - Information about the Client VPN endpoints.
--
-- 'httpStatus', 'describeClientVpnEndpointsResponse_httpStatus' - The response's http status code.
newDescribeClientVpnEndpointsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeClientVpnEndpointsResponse
newDescribeClientVpnEndpointsResponse pHttpStatus_ =
  DescribeClientVpnEndpointsResponse'
    { nextToken =
        Core.Nothing,
      clientVpnEndpoints = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeClientVpnEndpointsResponse_nextToken :: Lens.Lens' DescribeClientVpnEndpointsResponse (Core.Maybe Core.Text)
describeClientVpnEndpointsResponse_nextToken = Lens.lens (\DescribeClientVpnEndpointsResponse' {nextToken} -> nextToken) (\s@DescribeClientVpnEndpointsResponse' {} a -> s {nextToken = a} :: DescribeClientVpnEndpointsResponse)

-- | Information about the Client VPN endpoints.
describeClientVpnEndpointsResponse_clientVpnEndpoints :: Lens.Lens' DescribeClientVpnEndpointsResponse (Core.Maybe [ClientVpnEndpoint])
describeClientVpnEndpointsResponse_clientVpnEndpoints = Lens.lens (\DescribeClientVpnEndpointsResponse' {clientVpnEndpoints} -> clientVpnEndpoints) (\s@DescribeClientVpnEndpointsResponse' {} a -> s {clientVpnEndpoints = a} :: DescribeClientVpnEndpointsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeClientVpnEndpointsResponse_httpStatus :: Lens.Lens' DescribeClientVpnEndpointsResponse Core.Int
describeClientVpnEndpointsResponse_httpStatus = Lens.lens (\DescribeClientVpnEndpointsResponse' {httpStatus} -> httpStatus) (\s@DescribeClientVpnEndpointsResponse' {} a -> s {httpStatus = a} :: DescribeClientVpnEndpointsResponse)

instance
  Core.NFData
    DescribeClientVpnEndpointsResponse
