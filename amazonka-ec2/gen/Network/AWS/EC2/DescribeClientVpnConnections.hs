{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.DescribeClientVpnConnections
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes active client connections and connections that have been
-- terminated within the last 60 minutes for the specified Client VPN
-- endpoint.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeClientVpnConnections
  ( -- * Creating a Request
    DescribeClientVpnConnections (..),
    newDescribeClientVpnConnections,

    -- * Request Lenses
    describeClientVpnConnections_nextToken,
    describeClientVpnConnections_dryRun,
    describeClientVpnConnections_maxResults,
    describeClientVpnConnections_filters,
    describeClientVpnConnections_clientVpnEndpointId,

    -- * Destructuring the Response
    DescribeClientVpnConnectionsResponse (..),
    newDescribeClientVpnConnectionsResponse,

    -- * Response Lenses
    describeClientVpnConnectionsResponse_nextToken,
    describeClientVpnConnectionsResponse_connections,
    describeClientVpnConnectionsResponse_httpStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeClientVpnConnections' smart constructor.
data DescribeClientVpnConnections = DescribeClientVpnConnections'
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
    -- -   @connection-id@ - The ID of the connection.
    --
    -- -   @username@ - For Active Directory client authentication, the user
    --     name of the client who established the client connection.
    filters :: Prelude.Maybe [Filter],
    -- | The ID of the Client VPN endpoint.
    clientVpnEndpointId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeClientVpnConnections' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeClientVpnConnections_nextToken' - The token to retrieve the next page of results.
--
-- 'dryRun', 'describeClientVpnConnections_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeClientVpnConnections_maxResults' - The maximum number of results to return for the request in a single
-- page. The remaining results can be seen by sending another request with
-- the nextToken value.
--
-- 'filters', 'describeClientVpnConnections_filters' - One or more filters. Filter names and values are case-sensitive.
--
-- -   @connection-id@ - The ID of the connection.
--
-- -   @username@ - For Active Directory client authentication, the user
--     name of the client who established the client connection.
--
-- 'clientVpnEndpointId', 'describeClientVpnConnections_clientVpnEndpointId' - The ID of the Client VPN endpoint.
newDescribeClientVpnConnections ::
  -- | 'clientVpnEndpointId'
  Prelude.Text ->
  DescribeClientVpnConnections
newDescribeClientVpnConnections pClientVpnEndpointId_ =
  DescribeClientVpnConnections'
    { nextToken =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      filters = Prelude.Nothing,
      clientVpnEndpointId = pClientVpnEndpointId_
    }

-- | The token to retrieve the next page of results.
describeClientVpnConnections_nextToken :: Lens.Lens' DescribeClientVpnConnections (Prelude.Maybe Prelude.Text)
describeClientVpnConnections_nextToken = Lens.lens (\DescribeClientVpnConnections' {nextToken} -> nextToken) (\s@DescribeClientVpnConnections' {} a -> s {nextToken = a} :: DescribeClientVpnConnections)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeClientVpnConnections_dryRun :: Lens.Lens' DescribeClientVpnConnections (Prelude.Maybe Prelude.Bool)
describeClientVpnConnections_dryRun = Lens.lens (\DescribeClientVpnConnections' {dryRun} -> dryRun) (\s@DescribeClientVpnConnections' {} a -> s {dryRun = a} :: DescribeClientVpnConnections)

-- | The maximum number of results to return for the request in a single
-- page. The remaining results can be seen by sending another request with
-- the nextToken value.
describeClientVpnConnections_maxResults :: Lens.Lens' DescribeClientVpnConnections (Prelude.Maybe Prelude.Natural)
describeClientVpnConnections_maxResults = Lens.lens (\DescribeClientVpnConnections' {maxResults} -> maxResults) (\s@DescribeClientVpnConnections' {} a -> s {maxResults = a} :: DescribeClientVpnConnections)

-- | One or more filters. Filter names and values are case-sensitive.
--
-- -   @connection-id@ - The ID of the connection.
--
-- -   @username@ - For Active Directory client authentication, the user
--     name of the client who established the client connection.
describeClientVpnConnections_filters :: Lens.Lens' DescribeClientVpnConnections (Prelude.Maybe [Filter])
describeClientVpnConnections_filters = Lens.lens (\DescribeClientVpnConnections' {filters} -> filters) (\s@DescribeClientVpnConnections' {} a -> s {filters = a} :: DescribeClientVpnConnections) Prelude.. Lens.mapping Prelude._Coerce

-- | The ID of the Client VPN endpoint.
describeClientVpnConnections_clientVpnEndpointId :: Lens.Lens' DescribeClientVpnConnections Prelude.Text
describeClientVpnConnections_clientVpnEndpointId = Lens.lens (\DescribeClientVpnConnections' {clientVpnEndpointId} -> clientVpnEndpointId) (\s@DescribeClientVpnConnections' {} a -> s {clientVpnEndpointId = a} :: DescribeClientVpnConnections)

instance Pager.AWSPager DescribeClientVpnConnections where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeClientVpnConnectionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeClientVpnConnectionsResponse_connections
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeClientVpnConnections_nextToken
          Lens..~ rs
          Lens.^? describeClientVpnConnectionsResponse_nextToken
            Prelude.. Lens._Just

instance
  Prelude.AWSRequest
    DescribeClientVpnConnections
  where
  type
    Rs DescribeClientVpnConnections =
      DescribeClientVpnConnectionsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeClientVpnConnectionsResponse'
            Prelude.<$> (x Prelude..@? "nextToken")
            Prelude.<*> ( x Prelude..@? "connections"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeClientVpnConnections

instance Prelude.NFData DescribeClientVpnConnections

instance
  Prelude.ToHeaders
    DescribeClientVpnConnections
  where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DescribeClientVpnConnections where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeClientVpnConnections where
  toQuery DescribeClientVpnConnections' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "DescribeClientVpnConnections" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Prelude.=: nextToken,
        "DryRun" Prelude.=: dryRun,
        "MaxResults" Prelude.=: maxResults,
        Prelude.toQuery
          (Prelude.toQueryList "Filter" Prelude.<$> filters),
        "ClientVpnEndpointId" Prelude.=: clientVpnEndpointId
      ]

-- | /See:/ 'newDescribeClientVpnConnectionsResponse' smart constructor.
data DescribeClientVpnConnectionsResponse = DescribeClientVpnConnectionsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the active and terminated client connections.
    connections :: Prelude.Maybe [ClientVpnConnection],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeClientVpnConnectionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeClientVpnConnectionsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'connections', 'describeClientVpnConnectionsResponse_connections' - Information about the active and terminated client connections.
--
-- 'httpStatus', 'describeClientVpnConnectionsResponse_httpStatus' - The response's http status code.
newDescribeClientVpnConnectionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeClientVpnConnectionsResponse
newDescribeClientVpnConnectionsResponse pHttpStatus_ =
  DescribeClientVpnConnectionsResponse'
    { nextToken =
        Prelude.Nothing,
      connections = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeClientVpnConnectionsResponse_nextToken :: Lens.Lens' DescribeClientVpnConnectionsResponse (Prelude.Maybe Prelude.Text)
describeClientVpnConnectionsResponse_nextToken = Lens.lens (\DescribeClientVpnConnectionsResponse' {nextToken} -> nextToken) (\s@DescribeClientVpnConnectionsResponse' {} a -> s {nextToken = a} :: DescribeClientVpnConnectionsResponse)

-- | Information about the active and terminated client connections.
describeClientVpnConnectionsResponse_connections :: Lens.Lens' DescribeClientVpnConnectionsResponse (Prelude.Maybe [ClientVpnConnection])
describeClientVpnConnectionsResponse_connections = Lens.lens (\DescribeClientVpnConnectionsResponse' {connections} -> connections) (\s@DescribeClientVpnConnectionsResponse' {} a -> s {connections = a} :: DescribeClientVpnConnectionsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeClientVpnConnectionsResponse_httpStatus :: Lens.Lens' DescribeClientVpnConnectionsResponse Prelude.Int
describeClientVpnConnectionsResponse_httpStatus = Lens.lens (\DescribeClientVpnConnectionsResponse' {httpStatus} -> httpStatus) (\s@DescribeClientVpnConnectionsResponse' {} a -> s {httpStatus = a} :: DescribeClientVpnConnectionsResponse)

instance
  Prelude.NFData
    DescribeClientVpnConnectionsResponse
