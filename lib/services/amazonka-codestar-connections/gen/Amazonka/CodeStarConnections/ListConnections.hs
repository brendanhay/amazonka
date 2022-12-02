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
-- Module      : Amazonka.CodeStarConnections.ListConnections
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the connections associated with your account.
module Amazonka.CodeStarConnections.ListConnections
  ( -- * Creating a Request
    ListConnections (..),
    newListConnections,

    -- * Request Lenses
    listConnections_hostArnFilter,
    listConnections_providerTypeFilter,
    listConnections_nextToken,
    listConnections_maxResults,

    -- * Destructuring the Response
    ListConnectionsResponse (..),
    newListConnectionsResponse,

    -- * Response Lenses
    listConnectionsResponse_nextToken,
    listConnectionsResponse_connections,
    listConnectionsResponse_httpStatus,
  )
where

import Amazonka.CodeStarConnections.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListConnections' smart constructor.
data ListConnections = ListConnections'
  { -- | Filters the list of connections to those associated with a specified
    -- host.
    hostArnFilter :: Prelude.Maybe Prelude.Text,
    -- | Filters the list of connections to those associated with a specified
    -- provider, such as Bitbucket.
    providerTypeFilter :: Prelude.Maybe ProviderType,
    -- | The token that was returned from the previous @ListConnections@ call,
    -- which can be used to return the next set of connections in the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListConnections' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostArnFilter', 'listConnections_hostArnFilter' - Filters the list of connections to those associated with a specified
-- host.
--
-- 'providerTypeFilter', 'listConnections_providerTypeFilter' - Filters the list of connections to those associated with a specified
-- provider, such as Bitbucket.
--
-- 'nextToken', 'listConnections_nextToken' - The token that was returned from the previous @ListConnections@ call,
-- which can be used to return the next set of connections in the list.
--
-- 'maxResults', 'listConnections_maxResults' - The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
newListConnections ::
  ListConnections
newListConnections =
  ListConnections'
    { hostArnFilter = Prelude.Nothing,
      providerTypeFilter = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Filters the list of connections to those associated with a specified
-- host.
listConnections_hostArnFilter :: Lens.Lens' ListConnections (Prelude.Maybe Prelude.Text)
listConnections_hostArnFilter = Lens.lens (\ListConnections' {hostArnFilter} -> hostArnFilter) (\s@ListConnections' {} a -> s {hostArnFilter = a} :: ListConnections)

-- | Filters the list of connections to those associated with a specified
-- provider, such as Bitbucket.
listConnections_providerTypeFilter :: Lens.Lens' ListConnections (Prelude.Maybe ProviderType)
listConnections_providerTypeFilter = Lens.lens (\ListConnections' {providerTypeFilter} -> providerTypeFilter) (\s@ListConnections' {} a -> s {providerTypeFilter = a} :: ListConnections)

-- | The token that was returned from the previous @ListConnections@ call,
-- which can be used to return the next set of connections in the list.
listConnections_nextToken :: Lens.Lens' ListConnections (Prelude.Maybe Prelude.Text)
listConnections_nextToken = Lens.lens (\ListConnections' {nextToken} -> nextToken) (\s@ListConnections' {} a -> s {nextToken = a} :: ListConnections)

-- | The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
listConnections_maxResults :: Lens.Lens' ListConnections (Prelude.Maybe Prelude.Natural)
listConnections_maxResults = Lens.lens (\ListConnections' {maxResults} -> maxResults) (\s@ListConnections' {} a -> s {maxResults = a} :: ListConnections)

instance Core.AWSRequest ListConnections where
  type
    AWSResponse ListConnections =
      ListConnectionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListConnectionsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Connections" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListConnections where
  hashWithSalt _salt ListConnections' {..} =
    _salt `Prelude.hashWithSalt` hostArnFilter
      `Prelude.hashWithSalt` providerTypeFilter
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListConnections where
  rnf ListConnections' {..} =
    Prelude.rnf hostArnFilter
      `Prelude.seq` Prelude.rnf providerTypeFilter
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListConnections where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "com.amazonaws.codestar.connections.CodeStar_connections_20191201.ListConnections" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListConnections where
  toJSON ListConnections' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("HostArnFilter" Data..=) Prelude.<$> hostArnFilter,
            ("ProviderTypeFilter" Data..=)
              Prelude.<$> providerTypeFilter,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("MaxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath ListConnections where
  toPath = Prelude.const "/"

instance Data.ToQuery ListConnections where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListConnectionsResponse' smart constructor.
data ListConnectionsResponse = ListConnectionsResponse'
  { -- | A token that can be used in the next @ListConnections@ call. To view all
    -- items in the list, continue to call this operation with each subsequent
    -- token until no more @nextToken@ values are returned.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of connections and the details for each connection, such as
    -- status, owner, and provider type.
    connections :: Prelude.Maybe [Connection],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListConnectionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listConnectionsResponse_nextToken' - A token that can be used in the next @ListConnections@ call. To view all
-- items in the list, continue to call this operation with each subsequent
-- token until no more @nextToken@ values are returned.
--
-- 'connections', 'listConnectionsResponse_connections' - A list of connections and the details for each connection, such as
-- status, owner, and provider type.
--
-- 'httpStatus', 'listConnectionsResponse_httpStatus' - The response's http status code.
newListConnectionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListConnectionsResponse
newListConnectionsResponse pHttpStatus_ =
  ListConnectionsResponse'
    { nextToken =
        Prelude.Nothing,
      connections = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that can be used in the next @ListConnections@ call. To view all
-- items in the list, continue to call this operation with each subsequent
-- token until no more @nextToken@ values are returned.
listConnectionsResponse_nextToken :: Lens.Lens' ListConnectionsResponse (Prelude.Maybe Prelude.Text)
listConnectionsResponse_nextToken = Lens.lens (\ListConnectionsResponse' {nextToken} -> nextToken) (\s@ListConnectionsResponse' {} a -> s {nextToken = a} :: ListConnectionsResponse)

-- | A list of connections and the details for each connection, such as
-- status, owner, and provider type.
listConnectionsResponse_connections :: Lens.Lens' ListConnectionsResponse (Prelude.Maybe [Connection])
listConnectionsResponse_connections = Lens.lens (\ListConnectionsResponse' {connections} -> connections) (\s@ListConnectionsResponse' {} a -> s {connections = a} :: ListConnectionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listConnectionsResponse_httpStatus :: Lens.Lens' ListConnectionsResponse Prelude.Int
listConnectionsResponse_httpStatus = Lens.lens (\ListConnectionsResponse' {httpStatus} -> httpStatus) (\s@ListConnectionsResponse' {} a -> s {httpStatus = a} :: ListConnectionsResponse)

instance Prelude.NFData ListConnectionsResponse where
  rnf ListConnectionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf connections
      `Prelude.seq` Prelude.rnf httpStatus
