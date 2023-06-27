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
-- Module      : Amazonka.VoiceId.ListWatchlists
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all watchlists in a specified domain.
--
-- This operation returns paginated results.
module Amazonka.VoiceId.ListWatchlists
  ( -- * Creating a Request
    ListWatchlists (..),
    newListWatchlists,

    -- * Request Lenses
    listWatchlists_maxResults,
    listWatchlists_nextToken,
    listWatchlists_domainId,

    -- * Destructuring the Response
    ListWatchlistsResponse (..),
    newListWatchlistsResponse,

    -- * Response Lenses
    listWatchlistsResponse_nextToken,
    listWatchlistsResponse_watchlistSummaries,
    listWatchlistsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VoiceId.Types

-- | /See:/ 'newListWatchlists' smart constructor.
data ListWatchlists = ListWatchlists'
  { -- | The maximum number of results that are returned per call. You can use
    -- @NextToken@ to obtain more pages of results. The default is 100; the
    -- maximum allowed page size is also 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If @NextToken@ is returned, there are more results available. The value
    -- of @NextToken@ is a unique pagination token for each page. Make the call
    -- again using the returned token to retrieve the next page. Keep all other
    -- arguments unchanged. Each pagination token expires after 24 hours.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the domain.
    domainId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWatchlists' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listWatchlists_maxResults' - The maximum number of results that are returned per call. You can use
-- @NextToken@ to obtain more pages of results. The default is 100; the
-- maximum allowed page size is also 100.
--
-- 'nextToken', 'listWatchlists_nextToken' - If @NextToken@ is returned, there are more results available. The value
-- of @NextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours.
--
-- 'domainId', 'listWatchlists_domainId' - The identifier of the domain.
newListWatchlists ::
  -- | 'domainId'
  Prelude.Text ->
  ListWatchlists
newListWatchlists pDomainId_ =
  ListWatchlists'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      domainId = pDomainId_
    }

-- | The maximum number of results that are returned per call. You can use
-- @NextToken@ to obtain more pages of results. The default is 100; the
-- maximum allowed page size is also 100.
listWatchlists_maxResults :: Lens.Lens' ListWatchlists (Prelude.Maybe Prelude.Natural)
listWatchlists_maxResults = Lens.lens (\ListWatchlists' {maxResults} -> maxResults) (\s@ListWatchlists' {} a -> s {maxResults = a} :: ListWatchlists)

-- | If @NextToken@ is returned, there are more results available. The value
-- of @NextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours.
listWatchlists_nextToken :: Lens.Lens' ListWatchlists (Prelude.Maybe Prelude.Text)
listWatchlists_nextToken = Lens.lens (\ListWatchlists' {nextToken} -> nextToken) (\s@ListWatchlists' {} a -> s {nextToken = a} :: ListWatchlists)

-- | The identifier of the domain.
listWatchlists_domainId :: Lens.Lens' ListWatchlists Prelude.Text
listWatchlists_domainId = Lens.lens (\ListWatchlists' {domainId} -> domainId) (\s@ListWatchlists' {} a -> s {domainId = a} :: ListWatchlists)

instance Core.AWSPager ListWatchlists where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listWatchlistsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listWatchlistsResponse_watchlistSummaries
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listWatchlists_nextToken
          Lens..~ rs
          Lens.^? listWatchlistsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListWatchlists where
  type
    AWSResponse ListWatchlists =
      ListWatchlistsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListWatchlistsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "WatchlistSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListWatchlists where
  hashWithSalt _salt ListWatchlists' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` domainId

instance Prelude.NFData ListWatchlists where
  rnf ListWatchlists' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf domainId

instance Data.ToHeaders ListWatchlists where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("VoiceID.ListWatchlists" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListWatchlists where
  toJSON ListWatchlists' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("DomainId" Data..= domainId)
          ]
      )

instance Data.ToPath ListWatchlists where
  toPath = Prelude.const "/"

instance Data.ToQuery ListWatchlists where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListWatchlistsResponse' smart constructor.
data ListWatchlistsResponse = ListWatchlistsResponse'
  { -- | If @NextToken@ is returned, there are more results available. The value
    -- of @NextToken@ is a unique pagination token for each page. Make the call
    -- again using the returned token to retrieve the next page. Keep all other
    -- arguments unchanged. Each pagination token expires after 24 hours.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list that contains details about each watchlist in the Amazon Web
    -- Services account.
    watchlistSummaries :: Prelude.Maybe [WatchlistSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWatchlistsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listWatchlistsResponse_nextToken' - If @NextToken@ is returned, there are more results available. The value
-- of @NextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours.
--
-- 'watchlistSummaries', 'listWatchlistsResponse_watchlistSummaries' - A list that contains details about each watchlist in the Amazon Web
-- Services account.
--
-- 'httpStatus', 'listWatchlistsResponse_httpStatus' - The response's http status code.
newListWatchlistsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListWatchlistsResponse
newListWatchlistsResponse pHttpStatus_ =
  ListWatchlistsResponse'
    { nextToken =
        Prelude.Nothing,
      watchlistSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If @NextToken@ is returned, there are more results available. The value
-- of @NextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours.
listWatchlistsResponse_nextToken :: Lens.Lens' ListWatchlistsResponse (Prelude.Maybe Prelude.Text)
listWatchlistsResponse_nextToken = Lens.lens (\ListWatchlistsResponse' {nextToken} -> nextToken) (\s@ListWatchlistsResponse' {} a -> s {nextToken = a} :: ListWatchlistsResponse)

-- | A list that contains details about each watchlist in the Amazon Web
-- Services account.
listWatchlistsResponse_watchlistSummaries :: Lens.Lens' ListWatchlistsResponse (Prelude.Maybe [WatchlistSummary])
listWatchlistsResponse_watchlistSummaries = Lens.lens (\ListWatchlistsResponse' {watchlistSummaries} -> watchlistSummaries) (\s@ListWatchlistsResponse' {} a -> s {watchlistSummaries = a} :: ListWatchlistsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listWatchlistsResponse_httpStatus :: Lens.Lens' ListWatchlistsResponse Prelude.Int
listWatchlistsResponse_httpStatus = Lens.lens (\ListWatchlistsResponse' {httpStatus} -> httpStatus) (\s@ListWatchlistsResponse' {} a -> s {httpStatus = a} :: ListWatchlistsResponse)

instance Prelude.NFData ListWatchlistsResponse where
  rnf ListWatchlistsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf watchlistSummaries
      `Prelude.seq` Prelude.rnf httpStatus
