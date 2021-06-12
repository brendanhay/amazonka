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
-- Module      : Network.AWS.EC2.DescribeTransitGatewayConnectPeers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more Connect peers.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeTransitGatewayConnectPeers
  ( -- * Creating a Request
    DescribeTransitGatewayConnectPeers (..),
    newDescribeTransitGatewayConnectPeers,

    -- * Request Lenses
    describeTransitGatewayConnectPeers_nextToken,
    describeTransitGatewayConnectPeers_dryRun,
    describeTransitGatewayConnectPeers_maxResults,
    describeTransitGatewayConnectPeers_filters,
    describeTransitGatewayConnectPeers_transitGatewayConnectPeerIds,

    -- * Destructuring the Response
    DescribeTransitGatewayConnectPeersResponse (..),
    newDescribeTransitGatewayConnectPeersResponse,

    -- * Response Lenses
    describeTransitGatewayConnectPeersResponse_nextToken,
    describeTransitGatewayConnectPeersResponse_transitGatewayConnectPeers,
    describeTransitGatewayConnectPeersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeTransitGatewayConnectPeers' smart constructor.
data DescribeTransitGatewayConnectPeers = DescribeTransitGatewayConnectPeers'
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
    -- -   @state@ - The state of the Connect peer (@pending@ | @available@ |
    --     @deleting@ | @deleted@).
    --
    -- -   @transit-gateway-attachment-id@ - The ID of the attachment.
    --
    -- -   @transit-gateway-connect-peer-id@ - The ID of the Connect peer.
    filters :: Core.Maybe [Filter],
    -- | The IDs of the Connect peers.
    transitGatewayConnectPeerIds :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeTransitGatewayConnectPeers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeTransitGatewayConnectPeers_nextToken' - The token for the next page of results.
--
-- 'dryRun', 'describeTransitGatewayConnectPeers_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeTransitGatewayConnectPeers_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'filters', 'describeTransitGatewayConnectPeers_filters' - One or more filters. The possible values are:
--
-- -   @state@ - The state of the Connect peer (@pending@ | @available@ |
--     @deleting@ | @deleted@).
--
-- -   @transit-gateway-attachment-id@ - The ID of the attachment.
--
-- -   @transit-gateway-connect-peer-id@ - The ID of the Connect peer.
--
-- 'transitGatewayConnectPeerIds', 'describeTransitGatewayConnectPeers_transitGatewayConnectPeerIds' - The IDs of the Connect peers.
newDescribeTransitGatewayConnectPeers ::
  DescribeTransitGatewayConnectPeers
newDescribeTransitGatewayConnectPeers =
  DescribeTransitGatewayConnectPeers'
    { nextToken =
        Core.Nothing,
      dryRun = Core.Nothing,
      maxResults = Core.Nothing,
      filters = Core.Nothing,
      transitGatewayConnectPeerIds =
        Core.Nothing
    }

-- | The token for the next page of results.
describeTransitGatewayConnectPeers_nextToken :: Lens.Lens' DescribeTransitGatewayConnectPeers (Core.Maybe Core.Text)
describeTransitGatewayConnectPeers_nextToken = Lens.lens (\DescribeTransitGatewayConnectPeers' {nextToken} -> nextToken) (\s@DescribeTransitGatewayConnectPeers' {} a -> s {nextToken = a} :: DescribeTransitGatewayConnectPeers)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeTransitGatewayConnectPeers_dryRun :: Lens.Lens' DescribeTransitGatewayConnectPeers (Core.Maybe Core.Bool)
describeTransitGatewayConnectPeers_dryRun = Lens.lens (\DescribeTransitGatewayConnectPeers' {dryRun} -> dryRun) (\s@DescribeTransitGatewayConnectPeers' {} a -> s {dryRun = a} :: DescribeTransitGatewayConnectPeers)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeTransitGatewayConnectPeers_maxResults :: Lens.Lens' DescribeTransitGatewayConnectPeers (Core.Maybe Core.Natural)
describeTransitGatewayConnectPeers_maxResults = Lens.lens (\DescribeTransitGatewayConnectPeers' {maxResults} -> maxResults) (\s@DescribeTransitGatewayConnectPeers' {} a -> s {maxResults = a} :: DescribeTransitGatewayConnectPeers)

-- | One or more filters. The possible values are:
--
-- -   @state@ - The state of the Connect peer (@pending@ | @available@ |
--     @deleting@ | @deleted@).
--
-- -   @transit-gateway-attachment-id@ - The ID of the attachment.
--
-- -   @transit-gateway-connect-peer-id@ - The ID of the Connect peer.
describeTransitGatewayConnectPeers_filters :: Lens.Lens' DescribeTransitGatewayConnectPeers (Core.Maybe [Filter])
describeTransitGatewayConnectPeers_filters = Lens.lens (\DescribeTransitGatewayConnectPeers' {filters} -> filters) (\s@DescribeTransitGatewayConnectPeers' {} a -> s {filters = a} :: DescribeTransitGatewayConnectPeers) Core.. Lens.mapping Lens._Coerce

-- | The IDs of the Connect peers.
describeTransitGatewayConnectPeers_transitGatewayConnectPeerIds :: Lens.Lens' DescribeTransitGatewayConnectPeers (Core.Maybe [Core.Text])
describeTransitGatewayConnectPeers_transitGatewayConnectPeerIds = Lens.lens (\DescribeTransitGatewayConnectPeers' {transitGatewayConnectPeerIds} -> transitGatewayConnectPeerIds) (\s@DescribeTransitGatewayConnectPeers' {} a -> s {transitGatewayConnectPeerIds = a} :: DescribeTransitGatewayConnectPeers) Core.. Lens.mapping Lens._Coerce

instance
  Core.AWSPager
    DescribeTransitGatewayConnectPeers
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeTransitGatewayConnectPeersResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeTransitGatewayConnectPeersResponse_transitGatewayConnectPeers
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeTransitGatewayConnectPeers_nextToken
          Lens..~ rs
          Lens.^? describeTransitGatewayConnectPeersResponse_nextToken
            Core.. Lens._Just

instance
  Core.AWSRequest
    DescribeTransitGatewayConnectPeers
  where
  type
    AWSResponse DescribeTransitGatewayConnectPeers =
      DescribeTransitGatewayConnectPeersResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeTransitGatewayConnectPeersResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> ( x Core..@? "transitGatewayConnectPeerSet"
                         Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeTransitGatewayConnectPeers

instance
  Core.NFData
    DescribeTransitGatewayConnectPeers

instance
  Core.ToHeaders
    DescribeTransitGatewayConnectPeers
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    DescribeTransitGatewayConnectPeers
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeTransitGatewayConnectPeers
  where
  toQuery DescribeTransitGatewayConnectPeers' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "DescribeTransitGatewayConnectPeers" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          (Core.toQueryList "Filter" Core.<$> filters),
        Core.toQuery
          ( Core.toQueryList "TransitGatewayConnectPeerIds"
              Core.<$> transitGatewayConnectPeerIds
          )
      ]

-- | /See:/ 'newDescribeTransitGatewayConnectPeersResponse' smart constructor.
data DescribeTransitGatewayConnectPeersResponse = DescribeTransitGatewayConnectPeersResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the Connect peers.
    transitGatewayConnectPeers :: Core.Maybe [TransitGatewayConnectPeer],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeTransitGatewayConnectPeersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeTransitGatewayConnectPeersResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'transitGatewayConnectPeers', 'describeTransitGatewayConnectPeersResponse_transitGatewayConnectPeers' - Information about the Connect peers.
--
-- 'httpStatus', 'describeTransitGatewayConnectPeersResponse_httpStatus' - The response's http status code.
newDescribeTransitGatewayConnectPeersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeTransitGatewayConnectPeersResponse
newDescribeTransitGatewayConnectPeersResponse
  pHttpStatus_ =
    DescribeTransitGatewayConnectPeersResponse'
      { nextToken =
          Core.Nothing,
        transitGatewayConnectPeers =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeTransitGatewayConnectPeersResponse_nextToken :: Lens.Lens' DescribeTransitGatewayConnectPeersResponse (Core.Maybe Core.Text)
describeTransitGatewayConnectPeersResponse_nextToken = Lens.lens (\DescribeTransitGatewayConnectPeersResponse' {nextToken} -> nextToken) (\s@DescribeTransitGatewayConnectPeersResponse' {} a -> s {nextToken = a} :: DescribeTransitGatewayConnectPeersResponse)

-- | Information about the Connect peers.
describeTransitGatewayConnectPeersResponse_transitGatewayConnectPeers :: Lens.Lens' DescribeTransitGatewayConnectPeersResponse (Core.Maybe [TransitGatewayConnectPeer])
describeTransitGatewayConnectPeersResponse_transitGatewayConnectPeers = Lens.lens (\DescribeTransitGatewayConnectPeersResponse' {transitGatewayConnectPeers} -> transitGatewayConnectPeers) (\s@DescribeTransitGatewayConnectPeersResponse' {} a -> s {transitGatewayConnectPeers = a} :: DescribeTransitGatewayConnectPeersResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeTransitGatewayConnectPeersResponse_httpStatus :: Lens.Lens' DescribeTransitGatewayConnectPeersResponse Core.Int
describeTransitGatewayConnectPeersResponse_httpStatus = Lens.lens (\DescribeTransitGatewayConnectPeersResponse' {httpStatus} -> httpStatus) (\s@DescribeTransitGatewayConnectPeersResponse' {} a -> s {httpStatus = a} :: DescribeTransitGatewayConnectPeersResponse)

instance
  Core.NFData
    DescribeTransitGatewayConnectPeersResponse
