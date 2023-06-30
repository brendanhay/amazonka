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
-- Module      : Amazonka.EC2.DescribeTransitGatewayConnectPeers
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more Connect peers.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeTransitGatewayConnectPeers
  ( -- * Creating a Request
    DescribeTransitGatewayConnectPeers (..),
    newDescribeTransitGatewayConnectPeers,

    -- * Request Lenses
    describeTransitGatewayConnectPeers_dryRun,
    describeTransitGatewayConnectPeers_filters,
    describeTransitGatewayConnectPeers_maxResults,
    describeTransitGatewayConnectPeers_nextToken,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeTransitGatewayConnectPeers' smart constructor.
data DescribeTransitGatewayConnectPeers = DescribeTransitGatewayConnectPeers'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | One or more filters. The possible values are:
    --
    -- -   @state@ - The state of the Connect peer (@pending@ | @available@ |
    --     @deleting@ | @deleted@).
    --
    -- -   @transit-gateway-attachment-id@ - The ID of the attachment.
    --
    -- -   @transit-gateway-connect-peer-id@ - The ID of the Connect peer.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The IDs of the Connect peers.
    transitGatewayConnectPeerIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTransitGatewayConnectPeers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeTransitGatewayConnectPeers_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
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
-- 'maxResults', 'describeTransitGatewayConnectPeers_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'nextToken', 'describeTransitGatewayConnectPeers_nextToken' - The token for the next page of results.
--
-- 'transitGatewayConnectPeerIds', 'describeTransitGatewayConnectPeers_transitGatewayConnectPeerIds' - The IDs of the Connect peers.
newDescribeTransitGatewayConnectPeers ::
  DescribeTransitGatewayConnectPeers
newDescribeTransitGatewayConnectPeers =
  DescribeTransitGatewayConnectPeers'
    { dryRun =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      transitGatewayConnectPeerIds =
        Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeTransitGatewayConnectPeers_dryRun :: Lens.Lens' DescribeTransitGatewayConnectPeers (Prelude.Maybe Prelude.Bool)
describeTransitGatewayConnectPeers_dryRun = Lens.lens (\DescribeTransitGatewayConnectPeers' {dryRun} -> dryRun) (\s@DescribeTransitGatewayConnectPeers' {} a -> s {dryRun = a} :: DescribeTransitGatewayConnectPeers)

-- | One or more filters. The possible values are:
--
-- -   @state@ - The state of the Connect peer (@pending@ | @available@ |
--     @deleting@ | @deleted@).
--
-- -   @transit-gateway-attachment-id@ - The ID of the attachment.
--
-- -   @transit-gateway-connect-peer-id@ - The ID of the Connect peer.
describeTransitGatewayConnectPeers_filters :: Lens.Lens' DescribeTransitGatewayConnectPeers (Prelude.Maybe [Filter])
describeTransitGatewayConnectPeers_filters = Lens.lens (\DescribeTransitGatewayConnectPeers' {filters} -> filters) (\s@DescribeTransitGatewayConnectPeers' {} a -> s {filters = a} :: DescribeTransitGatewayConnectPeers) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeTransitGatewayConnectPeers_maxResults :: Lens.Lens' DescribeTransitGatewayConnectPeers (Prelude.Maybe Prelude.Natural)
describeTransitGatewayConnectPeers_maxResults = Lens.lens (\DescribeTransitGatewayConnectPeers' {maxResults} -> maxResults) (\s@DescribeTransitGatewayConnectPeers' {} a -> s {maxResults = a} :: DescribeTransitGatewayConnectPeers)

-- | The token for the next page of results.
describeTransitGatewayConnectPeers_nextToken :: Lens.Lens' DescribeTransitGatewayConnectPeers (Prelude.Maybe Prelude.Text)
describeTransitGatewayConnectPeers_nextToken = Lens.lens (\DescribeTransitGatewayConnectPeers' {nextToken} -> nextToken) (\s@DescribeTransitGatewayConnectPeers' {} a -> s {nextToken = a} :: DescribeTransitGatewayConnectPeers)

-- | The IDs of the Connect peers.
describeTransitGatewayConnectPeers_transitGatewayConnectPeerIds :: Lens.Lens' DescribeTransitGatewayConnectPeers (Prelude.Maybe [Prelude.Text])
describeTransitGatewayConnectPeers_transitGatewayConnectPeerIds = Lens.lens (\DescribeTransitGatewayConnectPeers' {transitGatewayConnectPeerIds} -> transitGatewayConnectPeerIds) (\s@DescribeTransitGatewayConnectPeers' {} a -> s {transitGatewayConnectPeerIds = a} :: DescribeTransitGatewayConnectPeers) Prelude.. Lens.mapping Lens.coerced

instance
  Core.AWSPager
    DescribeTransitGatewayConnectPeers
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeTransitGatewayConnectPeersResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeTransitGatewayConnectPeersResponse_transitGatewayConnectPeers
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeTransitGatewayConnectPeers_nextToken
          Lens..~ rs
          Lens.^? describeTransitGatewayConnectPeersResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeTransitGatewayConnectPeers
  where
  type
    AWSResponse DescribeTransitGatewayConnectPeers =
      DescribeTransitGatewayConnectPeersResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeTransitGatewayConnectPeersResponse'
            Prelude.<$> (x Data..@? "nextToken")
            Prelude.<*> ( x
                            Data..@? "transitGatewayConnectPeerSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeTransitGatewayConnectPeers
  where
  hashWithSalt
    _salt
    DescribeTransitGatewayConnectPeers' {..} =
      _salt
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` transitGatewayConnectPeerIds

instance
  Prelude.NFData
    DescribeTransitGatewayConnectPeers
  where
  rnf DescribeTransitGatewayConnectPeers' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf transitGatewayConnectPeerIds

instance
  Data.ToHeaders
    DescribeTransitGatewayConnectPeers
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DescribeTransitGatewayConnectPeers
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeTransitGatewayConnectPeers
  where
  toQuery DescribeTransitGatewayConnectPeers' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeTransitGatewayConnectPeers" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        Data.toQuery
          ( Data.toQueryList "TransitGatewayConnectPeerIds"
              Prelude.<$> transitGatewayConnectPeerIds
          )
      ]

-- | /See:/ 'newDescribeTransitGatewayConnectPeersResponse' smart constructor.
data DescribeTransitGatewayConnectPeersResponse = DescribeTransitGatewayConnectPeersResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the Connect peers.
    transitGatewayConnectPeers :: Prelude.Maybe [TransitGatewayConnectPeer],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeTransitGatewayConnectPeersResponse
newDescribeTransitGatewayConnectPeersResponse
  pHttpStatus_ =
    DescribeTransitGatewayConnectPeersResponse'
      { nextToken =
          Prelude.Nothing,
        transitGatewayConnectPeers =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeTransitGatewayConnectPeersResponse_nextToken :: Lens.Lens' DescribeTransitGatewayConnectPeersResponse (Prelude.Maybe Prelude.Text)
describeTransitGatewayConnectPeersResponse_nextToken = Lens.lens (\DescribeTransitGatewayConnectPeersResponse' {nextToken} -> nextToken) (\s@DescribeTransitGatewayConnectPeersResponse' {} a -> s {nextToken = a} :: DescribeTransitGatewayConnectPeersResponse)

-- | Information about the Connect peers.
describeTransitGatewayConnectPeersResponse_transitGatewayConnectPeers :: Lens.Lens' DescribeTransitGatewayConnectPeersResponse (Prelude.Maybe [TransitGatewayConnectPeer])
describeTransitGatewayConnectPeersResponse_transitGatewayConnectPeers = Lens.lens (\DescribeTransitGatewayConnectPeersResponse' {transitGatewayConnectPeers} -> transitGatewayConnectPeers) (\s@DescribeTransitGatewayConnectPeersResponse' {} a -> s {transitGatewayConnectPeers = a} :: DescribeTransitGatewayConnectPeersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeTransitGatewayConnectPeersResponse_httpStatus :: Lens.Lens' DescribeTransitGatewayConnectPeersResponse Prelude.Int
describeTransitGatewayConnectPeersResponse_httpStatus = Lens.lens (\DescribeTransitGatewayConnectPeersResponse' {httpStatus} -> httpStatus) (\s@DescribeTransitGatewayConnectPeersResponse' {} a -> s {httpStatus = a} :: DescribeTransitGatewayConnectPeersResponse)

instance
  Prelude.NFData
    DescribeTransitGatewayConnectPeersResponse
  where
  rnf DescribeTransitGatewayConnectPeersResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf transitGatewayConnectPeers
      `Prelude.seq` Prelude.rnf httpStatus
