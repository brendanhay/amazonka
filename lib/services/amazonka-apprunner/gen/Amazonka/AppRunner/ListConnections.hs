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
-- Module      : Amazonka.AppRunner.ListConnections
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of App Runner connections that are associated with your
-- Amazon Web Services account.
module Amazonka.AppRunner.ListConnections
  ( -- * Creating a Request
    ListConnections (..),
    newListConnections,

    -- * Request Lenses
    listConnections_connectionName,
    listConnections_maxResults,
    listConnections_nextToken,

    -- * Destructuring the Response
    ListConnectionsResponse (..),
    newListConnectionsResponse,

    -- * Response Lenses
    listConnectionsResponse_nextToken,
    listConnectionsResponse_httpStatus,
    listConnectionsResponse_connectionSummaryList,
  )
where

import Amazonka.AppRunner.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListConnections' smart constructor.
data ListConnections = ListConnections'
  { -- | If specified, only this connection is returned. If not specified, the
    -- result isn\'t filtered by name.
    connectionName :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to include in each response (result page).
    -- Used for a paginated request.
    --
    -- If you don\'t specify @MaxResults@, the request retrieves all available
    -- results in a single response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token from a previous result page. Used for a paginated request. The
    -- request retrieves the next result page. All other parameter values must
    -- be identical to the ones specified in the initial request.
    --
    -- If you don\'t specify @NextToken@, the request retrieves the first
    -- result page.
    nextToken :: Prelude.Maybe Prelude.Text
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
-- 'connectionName', 'listConnections_connectionName' - If specified, only this connection is returned. If not specified, the
-- result isn\'t filtered by name.
--
-- 'maxResults', 'listConnections_maxResults' - The maximum number of results to include in each response (result page).
-- Used for a paginated request.
--
-- If you don\'t specify @MaxResults@, the request retrieves all available
-- results in a single response.
--
-- 'nextToken', 'listConnections_nextToken' - A token from a previous result page. Used for a paginated request. The
-- request retrieves the next result page. All other parameter values must
-- be identical to the ones specified in the initial request.
--
-- If you don\'t specify @NextToken@, the request retrieves the first
-- result page.
newListConnections ::
  ListConnections
newListConnections =
  ListConnections'
    { connectionName = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | If specified, only this connection is returned. If not specified, the
-- result isn\'t filtered by name.
listConnections_connectionName :: Lens.Lens' ListConnections (Prelude.Maybe Prelude.Text)
listConnections_connectionName = Lens.lens (\ListConnections' {connectionName} -> connectionName) (\s@ListConnections' {} a -> s {connectionName = a} :: ListConnections)

-- | The maximum number of results to include in each response (result page).
-- Used for a paginated request.
--
-- If you don\'t specify @MaxResults@, the request retrieves all available
-- results in a single response.
listConnections_maxResults :: Lens.Lens' ListConnections (Prelude.Maybe Prelude.Natural)
listConnections_maxResults = Lens.lens (\ListConnections' {maxResults} -> maxResults) (\s@ListConnections' {} a -> s {maxResults = a} :: ListConnections)

-- | A token from a previous result page. Used for a paginated request. The
-- request retrieves the next result page. All other parameter values must
-- be identical to the ones specified in the initial request.
--
-- If you don\'t specify @NextToken@, the request retrieves the first
-- result page.
listConnections_nextToken :: Lens.Lens' ListConnections (Prelude.Maybe Prelude.Text)
listConnections_nextToken = Lens.lens (\ListConnections' {nextToken} -> nextToken) (\s@ListConnections' {} a -> s {nextToken = a} :: ListConnections)

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
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "ConnectionSummaryList"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListConnections where
  hashWithSalt _salt ListConnections' {..} =
    _salt
      `Prelude.hashWithSalt` connectionName
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListConnections where
  rnf ListConnections' {..} =
    Prelude.rnf connectionName `Prelude.seq`
      Prelude.rnf maxResults `Prelude.seq`
        Prelude.rnf nextToken

instance Data.ToHeaders ListConnections where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AppRunner.ListConnections" :: Prelude.ByteString),
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
          [ ("ConnectionName" Data..=)
              Prelude.<$> connectionName,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListConnections where
  toPath = Prelude.const "/"

instance Data.ToQuery ListConnections where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListConnectionsResponse' smart constructor.
data ListConnectionsResponse = ListConnectionsResponse'
  { -- | The token that you can pass in a subsequent request to get the next
    -- result page. Returned in a paginated request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of summary information records for connections. In a paginated
    -- request, the request returns up to @MaxResults@ records for each call.
    connectionSummaryList :: [ConnectionSummary]
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
-- 'nextToken', 'listConnectionsResponse_nextToken' - The token that you can pass in a subsequent request to get the next
-- result page. Returned in a paginated request.
--
-- 'httpStatus', 'listConnectionsResponse_httpStatus' - The response's http status code.
--
-- 'connectionSummaryList', 'listConnectionsResponse_connectionSummaryList' - A list of summary information records for connections. In a paginated
-- request, the request returns up to @MaxResults@ records for each call.
newListConnectionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListConnectionsResponse
newListConnectionsResponse pHttpStatus_ =
  ListConnectionsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      connectionSummaryList = Prelude.mempty
    }

-- | The token that you can pass in a subsequent request to get the next
-- result page. Returned in a paginated request.
listConnectionsResponse_nextToken :: Lens.Lens' ListConnectionsResponse (Prelude.Maybe Prelude.Text)
listConnectionsResponse_nextToken = Lens.lens (\ListConnectionsResponse' {nextToken} -> nextToken) (\s@ListConnectionsResponse' {} a -> s {nextToken = a} :: ListConnectionsResponse)

-- | The response's http status code.
listConnectionsResponse_httpStatus :: Lens.Lens' ListConnectionsResponse Prelude.Int
listConnectionsResponse_httpStatus = Lens.lens (\ListConnectionsResponse' {httpStatus} -> httpStatus) (\s@ListConnectionsResponse' {} a -> s {httpStatus = a} :: ListConnectionsResponse)

-- | A list of summary information records for connections. In a paginated
-- request, the request returns up to @MaxResults@ records for each call.
listConnectionsResponse_connectionSummaryList :: Lens.Lens' ListConnectionsResponse [ConnectionSummary]
listConnectionsResponse_connectionSummaryList = Lens.lens (\ListConnectionsResponse' {connectionSummaryList} -> connectionSummaryList) (\s@ListConnectionsResponse' {} a -> s {connectionSummaryList = a} :: ListConnectionsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListConnectionsResponse where
  rnf ListConnectionsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf connectionSummaryList
