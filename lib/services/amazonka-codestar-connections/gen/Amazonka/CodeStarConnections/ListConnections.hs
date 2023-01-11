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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    listConnections_maxResults,
    listConnections_nextToken,
    listConnections_providerTypeFilter,

    -- * Destructuring the Response
    ListConnectionsResponse (..),
    newListConnectionsResponse,

    -- * Response Lenses
    listConnectionsResponse_connections,
    listConnectionsResponse_nextToken,
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
    -- | The maximum number of results to return in a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token that was returned from the previous @ListConnections@ call,
    -- which can be used to return the next set of connections in the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Filters the list of connections to those associated with a specified
    -- provider, such as Bitbucket.
    providerTypeFilter :: Prelude.Maybe ProviderType
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
-- 'maxResults', 'listConnections_maxResults' - The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'nextToken', 'listConnections_nextToken' - The token that was returned from the previous @ListConnections@ call,
-- which can be used to return the next set of connections in the list.
--
-- 'providerTypeFilter', 'listConnections_providerTypeFilter' - Filters the list of connections to those associated with a specified
-- provider, such as Bitbucket.
newListConnections ::
  ListConnections
newListConnections =
  ListConnections'
    { hostArnFilter = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      providerTypeFilter = Prelude.Nothing
    }

-- | Filters the list of connections to those associated with a specified
-- host.
listConnections_hostArnFilter :: Lens.Lens' ListConnections (Prelude.Maybe Prelude.Text)
listConnections_hostArnFilter = Lens.lens (\ListConnections' {hostArnFilter} -> hostArnFilter) (\s@ListConnections' {} a -> s {hostArnFilter = a} :: ListConnections)

-- | The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
listConnections_maxResults :: Lens.Lens' ListConnections (Prelude.Maybe Prelude.Natural)
listConnections_maxResults = Lens.lens (\ListConnections' {maxResults} -> maxResults) (\s@ListConnections' {} a -> s {maxResults = a} :: ListConnections)

-- | The token that was returned from the previous @ListConnections@ call,
-- which can be used to return the next set of connections in the list.
listConnections_nextToken :: Lens.Lens' ListConnections (Prelude.Maybe Prelude.Text)
listConnections_nextToken = Lens.lens (\ListConnections' {nextToken} -> nextToken) (\s@ListConnections' {} a -> s {nextToken = a} :: ListConnections)

-- | Filters the list of connections to those associated with a specified
-- provider, such as Bitbucket.
listConnections_providerTypeFilter :: Lens.Lens' ListConnections (Prelude.Maybe ProviderType)
listConnections_providerTypeFilter = Lens.lens (\ListConnections' {providerTypeFilter} -> providerTypeFilter) (\s@ListConnections' {} a -> s {providerTypeFilter = a} :: ListConnections)

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
            Prelude.<$> (x Data..?> "Connections" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListConnections where
  hashWithSalt _salt ListConnections' {..} =
    _salt `Prelude.hashWithSalt` hostArnFilter
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` providerTypeFilter

instance Prelude.NFData ListConnections where
  rnf ListConnections' {..} =
    Prelude.rnf hostArnFilter
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf providerTypeFilter

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
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("ProviderTypeFilter" Data..=)
              Prelude.<$> providerTypeFilter
          ]
      )

instance Data.ToPath ListConnections where
  toPath = Prelude.const "/"

instance Data.ToQuery ListConnections where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListConnectionsResponse' smart constructor.
data ListConnectionsResponse = ListConnectionsResponse'
  { -- | A list of connections and the details for each connection, such as
    -- status, owner, and provider type.
    connections :: Prelude.Maybe [Connection],
    -- | A token that can be used in the next @ListConnections@ call. To view all
    -- items in the list, continue to call this operation with each subsequent
    -- token until no more @nextToken@ values are returned.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'connections', 'listConnectionsResponse_connections' - A list of connections and the details for each connection, such as
-- status, owner, and provider type.
--
-- 'nextToken', 'listConnectionsResponse_nextToken' - A token that can be used in the next @ListConnections@ call. To view all
-- items in the list, continue to call this operation with each subsequent
-- token until no more @nextToken@ values are returned.
--
-- 'httpStatus', 'listConnectionsResponse_httpStatus' - The response's http status code.
newListConnectionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListConnectionsResponse
newListConnectionsResponse pHttpStatus_ =
  ListConnectionsResponse'
    { connections =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of connections and the details for each connection, such as
-- status, owner, and provider type.
listConnectionsResponse_connections :: Lens.Lens' ListConnectionsResponse (Prelude.Maybe [Connection])
listConnectionsResponse_connections = Lens.lens (\ListConnectionsResponse' {connections} -> connections) (\s@ListConnectionsResponse' {} a -> s {connections = a} :: ListConnectionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token that can be used in the next @ListConnections@ call. To view all
-- items in the list, continue to call this operation with each subsequent
-- token until no more @nextToken@ values are returned.
listConnectionsResponse_nextToken :: Lens.Lens' ListConnectionsResponse (Prelude.Maybe Prelude.Text)
listConnectionsResponse_nextToken = Lens.lens (\ListConnectionsResponse' {nextToken} -> nextToken) (\s@ListConnectionsResponse' {} a -> s {nextToken = a} :: ListConnectionsResponse)

-- | The response's http status code.
listConnectionsResponse_httpStatus :: Lens.Lens' ListConnectionsResponse Prelude.Int
listConnectionsResponse_httpStatus = Lens.lens (\ListConnectionsResponse' {httpStatus} -> httpStatus) (\s@ListConnectionsResponse' {} a -> s {httpStatus = a} :: ListConnectionsResponse)

instance Prelude.NFData ListConnectionsResponse where
  rnf ListConnectionsResponse' {..} =
    Prelude.rnf connections
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
