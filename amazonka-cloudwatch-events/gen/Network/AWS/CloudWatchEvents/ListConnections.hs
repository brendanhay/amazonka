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
-- Module      : Network.AWS.CloudWatchEvents.ListConnections
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of connections from the account.
module Network.AWS.CloudWatchEvents.ListConnections
  ( -- * Creating a Request
    ListConnections (..),
    newListConnections,

    -- * Request Lenses
    listConnections_nextToken,
    listConnections_connectionState,
    listConnections_namePrefix,
    listConnections_limit,

    -- * Destructuring the Response
    ListConnectionsResponse (..),
    newListConnectionsResponse,

    -- * Response Lenses
    listConnectionsResponse_nextToken,
    listConnectionsResponse_connections,
    listConnectionsResponse_httpStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListConnections' smart constructor.
data ListConnections = ListConnections'
  { -- | The token returned by a previous call to retrieve the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | The state of the connection.
    connectionState :: Core.Maybe ConnectionState,
    -- | A name prefix to filter results returned. Only connections with a name
    -- that starts with the prefix are returned.
    namePrefix :: Core.Maybe Core.Text,
    -- | The maximum number of connections to return.
    limit :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListConnections' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listConnections_nextToken' - The token returned by a previous call to retrieve the next set of
-- results.
--
-- 'connectionState', 'listConnections_connectionState' - The state of the connection.
--
-- 'namePrefix', 'listConnections_namePrefix' - A name prefix to filter results returned. Only connections with a name
-- that starts with the prefix are returned.
--
-- 'limit', 'listConnections_limit' - The maximum number of connections to return.
newListConnections ::
  ListConnections
newListConnections =
  ListConnections'
    { nextToken = Core.Nothing,
      connectionState = Core.Nothing,
      namePrefix = Core.Nothing,
      limit = Core.Nothing
    }

-- | The token returned by a previous call to retrieve the next set of
-- results.
listConnections_nextToken :: Lens.Lens' ListConnections (Core.Maybe Core.Text)
listConnections_nextToken = Lens.lens (\ListConnections' {nextToken} -> nextToken) (\s@ListConnections' {} a -> s {nextToken = a} :: ListConnections)

-- | The state of the connection.
listConnections_connectionState :: Lens.Lens' ListConnections (Core.Maybe ConnectionState)
listConnections_connectionState = Lens.lens (\ListConnections' {connectionState} -> connectionState) (\s@ListConnections' {} a -> s {connectionState = a} :: ListConnections)

-- | A name prefix to filter results returned. Only connections with a name
-- that starts with the prefix are returned.
listConnections_namePrefix :: Lens.Lens' ListConnections (Core.Maybe Core.Text)
listConnections_namePrefix = Lens.lens (\ListConnections' {namePrefix} -> namePrefix) (\s@ListConnections' {} a -> s {namePrefix = a} :: ListConnections)

-- | The maximum number of connections to return.
listConnections_limit :: Lens.Lens' ListConnections (Core.Maybe Core.Natural)
listConnections_limit = Lens.lens (\ListConnections' {limit} -> limit) (\s@ListConnections' {} a -> s {limit = a} :: ListConnections)

instance Core.AWSRequest ListConnections where
  type
    AWSResponse ListConnections =
      ListConnectionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListConnectionsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Connections" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListConnections

instance Core.NFData ListConnections

instance Core.ToHeaders ListConnections where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSEvents.ListConnections" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListConnections where
  toJSON ListConnections' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("ConnectionState" Core..=) Core.<$> connectionState,
            ("NamePrefix" Core..=) Core.<$> namePrefix,
            ("Limit" Core..=) Core.<$> limit
          ]
      )

instance Core.ToPath ListConnections where
  toPath = Core.const "/"

instance Core.ToQuery ListConnections where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListConnectionsResponse' smart constructor.
data ListConnectionsResponse = ListConnectionsResponse'
  { -- | A token you can use in a subsequent request to retrieve the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | An array of connections objects that include details about the
    -- connections.
    connections :: Core.Maybe [Connection],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListConnectionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listConnectionsResponse_nextToken' - A token you can use in a subsequent request to retrieve the next set of
-- results.
--
-- 'connections', 'listConnectionsResponse_connections' - An array of connections objects that include details about the
-- connections.
--
-- 'httpStatus', 'listConnectionsResponse_httpStatus' - The response's http status code.
newListConnectionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListConnectionsResponse
newListConnectionsResponse pHttpStatus_ =
  ListConnectionsResponse'
    { nextToken = Core.Nothing,
      connections = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token you can use in a subsequent request to retrieve the next set of
-- results.
listConnectionsResponse_nextToken :: Lens.Lens' ListConnectionsResponse (Core.Maybe Core.Text)
listConnectionsResponse_nextToken = Lens.lens (\ListConnectionsResponse' {nextToken} -> nextToken) (\s@ListConnectionsResponse' {} a -> s {nextToken = a} :: ListConnectionsResponse)

-- | An array of connections objects that include details about the
-- connections.
listConnectionsResponse_connections :: Lens.Lens' ListConnectionsResponse (Core.Maybe [Connection])
listConnectionsResponse_connections = Lens.lens (\ListConnectionsResponse' {connections} -> connections) (\s@ListConnectionsResponse' {} a -> s {connections = a} :: ListConnectionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listConnectionsResponse_httpStatus :: Lens.Lens' ListConnectionsResponse Core.Int
listConnectionsResponse_httpStatus = Lens.lens (\ListConnectionsResponse' {httpStatus} -> httpStatus) (\s@ListConnectionsResponse' {} a -> s {httpStatus = a} :: ListConnectionsResponse)

instance Core.NFData ListConnectionsResponse
