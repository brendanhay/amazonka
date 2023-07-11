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
-- Module      : Amazonka.NetworkManager.GetConnections
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about one or more of your connections in a global
-- network.
--
-- This operation returns paginated results.
module Amazonka.NetworkManager.GetConnections
  ( -- * Creating a Request
    GetConnections (..),
    newGetConnections,

    -- * Request Lenses
    getConnections_connectionIds,
    getConnections_deviceId,
    getConnections_maxResults,
    getConnections_nextToken,
    getConnections_globalNetworkId,

    -- * Destructuring the Response
    GetConnectionsResponse (..),
    newGetConnectionsResponse,

    -- * Response Lenses
    getConnectionsResponse_connections,
    getConnectionsResponse_nextToken,
    getConnectionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetConnections' smart constructor.
data GetConnections = GetConnections'
  { -- | One or more connection IDs.
    connectionIds :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the device.
    deviceId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the global network.
    globalNetworkId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetConnections' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionIds', 'getConnections_connectionIds' - One or more connection IDs.
--
-- 'deviceId', 'getConnections_deviceId' - The ID of the device.
--
-- 'maxResults', 'getConnections_maxResults' - The maximum number of results to return.
--
-- 'nextToken', 'getConnections_nextToken' - The token for the next page of results.
--
-- 'globalNetworkId', 'getConnections_globalNetworkId' - The ID of the global network.
newGetConnections ::
  -- | 'globalNetworkId'
  Prelude.Text ->
  GetConnections
newGetConnections pGlobalNetworkId_ =
  GetConnections'
    { connectionIds = Prelude.Nothing,
      deviceId = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      globalNetworkId = pGlobalNetworkId_
    }

-- | One or more connection IDs.
getConnections_connectionIds :: Lens.Lens' GetConnections (Prelude.Maybe [Prelude.Text])
getConnections_connectionIds = Lens.lens (\GetConnections' {connectionIds} -> connectionIds) (\s@GetConnections' {} a -> s {connectionIds = a} :: GetConnections) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the device.
getConnections_deviceId :: Lens.Lens' GetConnections (Prelude.Maybe Prelude.Text)
getConnections_deviceId = Lens.lens (\GetConnections' {deviceId} -> deviceId) (\s@GetConnections' {} a -> s {deviceId = a} :: GetConnections)

-- | The maximum number of results to return.
getConnections_maxResults :: Lens.Lens' GetConnections (Prelude.Maybe Prelude.Natural)
getConnections_maxResults = Lens.lens (\GetConnections' {maxResults} -> maxResults) (\s@GetConnections' {} a -> s {maxResults = a} :: GetConnections)

-- | The token for the next page of results.
getConnections_nextToken :: Lens.Lens' GetConnections (Prelude.Maybe Prelude.Text)
getConnections_nextToken = Lens.lens (\GetConnections' {nextToken} -> nextToken) (\s@GetConnections' {} a -> s {nextToken = a} :: GetConnections)

-- | The ID of the global network.
getConnections_globalNetworkId :: Lens.Lens' GetConnections Prelude.Text
getConnections_globalNetworkId = Lens.lens (\GetConnections' {globalNetworkId} -> globalNetworkId) (\s@GetConnections' {} a -> s {globalNetworkId = a} :: GetConnections)

instance Core.AWSPager GetConnections where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getConnectionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getConnectionsResponse_connections
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& getConnections_nextToken
          Lens..~ rs
          Lens.^? getConnectionsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest GetConnections where
  type
    AWSResponse GetConnections =
      GetConnectionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetConnectionsResponse'
            Prelude.<$> (x Data..?> "Connections" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetConnections where
  hashWithSalt _salt GetConnections' {..} =
    _salt
      `Prelude.hashWithSalt` connectionIds
      `Prelude.hashWithSalt` deviceId
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` globalNetworkId

instance Prelude.NFData GetConnections where
  rnf GetConnections' {..} =
    Prelude.rnf connectionIds
      `Prelude.seq` Prelude.rnf deviceId
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf globalNetworkId

instance Data.ToHeaders GetConnections where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetConnections where
  toPath GetConnections' {..} =
    Prelude.mconcat
      [ "/global-networks/",
        Data.toBS globalNetworkId,
        "/connections"
      ]

instance Data.ToQuery GetConnections where
  toQuery GetConnections' {..} =
    Prelude.mconcat
      [ "connectionIds"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> connectionIds
            ),
        "deviceId" Data.=: deviceId,
        "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newGetConnectionsResponse' smart constructor.
data GetConnectionsResponse = GetConnectionsResponse'
  { -- | Information about the connections.
    connections :: Prelude.Maybe [Connection],
    -- | The token to use for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetConnectionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connections', 'getConnectionsResponse_connections' - Information about the connections.
--
-- 'nextToken', 'getConnectionsResponse_nextToken' - The token to use for the next page of results.
--
-- 'httpStatus', 'getConnectionsResponse_httpStatus' - The response's http status code.
newGetConnectionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetConnectionsResponse
newGetConnectionsResponse pHttpStatus_ =
  GetConnectionsResponse'
    { connections =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the connections.
getConnectionsResponse_connections :: Lens.Lens' GetConnectionsResponse (Prelude.Maybe [Connection])
getConnectionsResponse_connections = Lens.lens (\GetConnectionsResponse' {connections} -> connections) (\s@GetConnectionsResponse' {} a -> s {connections = a} :: GetConnectionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use for the next page of results.
getConnectionsResponse_nextToken :: Lens.Lens' GetConnectionsResponse (Prelude.Maybe Prelude.Text)
getConnectionsResponse_nextToken = Lens.lens (\GetConnectionsResponse' {nextToken} -> nextToken) (\s@GetConnectionsResponse' {} a -> s {nextToken = a} :: GetConnectionsResponse)

-- | The response's http status code.
getConnectionsResponse_httpStatus :: Lens.Lens' GetConnectionsResponse Prelude.Int
getConnectionsResponse_httpStatus = Lens.lens (\GetConnectionsResponse' {httpStatus} -> httpStatus) (\s@GetConnectionsResponse' {} a -> s {httpStatus = a} :: GetConnectionsResponse)

instance Prelude.NFData GetConnectionsResponse where
  rnf GetConnectionsResponse' {..} =
    Prelude.rnf connections
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
