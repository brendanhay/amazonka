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
-- Module      : Amazonka.VoiceId.ListFraudsters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all fraudsters in a specified watchlist or domain.
--
-- This operation returns paginated results.
module Amazonka.VoiceId.ListFraudsters
  ( -- * Creating a Request
    ListFraudsters (..),
    newListFraudsters,

    -- * Request Lenses
    listFraudsters_maxResults,
    listFraudsters_nextToken,
    listFraudsters_watchlistId,
    listFraudsters_domainId,

    -- * Destructuring the Response
    ListFraudstersResponse (..),
    newListFraudstersResponse,

    -- * Response Lenses
    listFraudstersResponse_fraudsterSummaries,
    listFraudstersResponse_nextToken,
    listFraudstersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VoiceId.Types

-- | /See:/ 'newListFraudsters' smart constructor.
data ListFraudsters = ListFraudsters'
  { -- | The maximum number of results that are returned per call. You can use
    -- @NextToken@ to obtain more pages of results. The default is 100; the
    -- maximum allowed page size is also 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If @NextToken@ is returned, there are more results available. The value
    -- of @NextToken@ is a unique pagination token for each page. Make the call
    -- again using the returned token to retrieve the next page. Keep all other
    -- arguments unchanged. Each pagination token expires after 24 hours.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the watchlist. If provided, all fraudsters in the
    -- watchlist are listed. If not provided, all fraudsters in the domain are
    -- listed.
    watchlistId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the domain.
    domainId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFraudsters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listFraudsters_maxResults' - The maximum number of results that are returned per call. You can use
-- @NextToken@ to obtain more pages of results. The default is 100; the
-- maximum allowed page size is also 100.
--
-- 'nextToken', 'listFraudsters_nextToken' - If @NextToken@ is returned, there are more results available. The value
-- of @NextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours.
--
-- 'watchlistId', 'listFraudsters_watchlistId' - The identifier of the watchlist. If provided, all fraudsters in the
-- watchlist are listed. If not provided, all fraudsters in the domain are
-- listed.
--
-- 'domainId', 'listFraudsters_domainId' - The identifier of the domain.
newListFraudsters ::
  -- | 'domainId'
  Prelude.Text ->
  ListFraudsters
newListFraudsters pDomainId_ =
  ListFraudsters'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      watchlistId = Prelude.Nothing,
      domainId = pDomainId_
    }

-- | The maximum number of results that are returned per call. You can use
-- @NextToken@ to obtain more pages of results. The default is 100; the
-- maximum allowed page size is also 100.
listFraudsters_maxResults :: Lens.Lens' ListFraudsters (Prelude.Maybe Prelude.Natural)
listFraudsters_maxResults = Lens.lens (\ListFraudsters' {maxResults} -> maxResults) (\s@ListFraudsters' {} a -> s {maxResults = a} :: ListFraudsters)

-- | If @NextToken@ is returned, there are more results available. The value
-- of @NextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours.
listFraudsters_nextToken :: Lens.Lens' ListFraudsters (Prelude.Maybe Prelude.Text)
listFraudsters_nextToken = Lens.lens (\ListFraudsters' {nextToken} -> nextToken) (\s@ListFraudsters' {} a -> s {nextToken = a} :: ListFraudsters)

-- | The identifier of the watchlist. If provided, all fraudsters in the
-- watchlist are listed. If not provided, all fraudsters in the domain are
-- listed.
listFraudsters_watchlistId :: Lens.Lens' ListFraudsters (Prelude.Maybe Prelude.Text)
listFraudsters_watchlistId = Lens.lens (\ListFraudsters' {watchlistId} -> watchlistId) (\s@ListFraudsters' {} a -> s {watchlistId = a} :: ListFraudsters)

-- | The identifier of the domain.
listFraudsters_domainId :: Lens.Lens' ListFraudsters Prelude.Text
listFraudsters_domainId = Lens.lens (\ListFraudsters' {domainId} -> domainId) (\s@ListFraudsters' {} a -> s {domainId = a} :: ListFraudsters)

instance Core.AWSPager ListFraudsters where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listFraudstersResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listFraudstersResponse_fraudsterSummaries
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listFraudsters_nextToken
          Lens..~ rs
          Lens.^? listFraudstersResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListFraudsters where
  type
    AWSResponse ListFraudsters =
      ListFraudstersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFraudstersResponse'
            Prelude.<$> ( x
                            Data..?> "FraudsterSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListFraudsters where
  hashWithSalt _salt ListFraudsters' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` watchlistId
      `Prelude.hashWithSalt` domainId

instance Prelude.NFData ListFraudsters where
  rnf ListFraudsters' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf watchlistId
      `Prelude.seq` Prelude.rnf domainId

instance Data.ToHeaders ListFraudsters where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("VoiceID.ListFraudsters" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListFraudsters where
  toJSON ListFraudsters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("WatchlistId" Data..=) Prelude.<$> watchlistId,
            Prelude.Just ("DomainId" Data..= domainId)
          ]
      )

instance Data.ToPath ListFraudsters where
  toPath = Prelude.const "/"

instance Data.ToQuery ListFraudsters where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListFraudstersResponse' smart constructor.
data ListFraudstersResponse = ListFraudstersResponse'
  { -- | A list that contains details about each fraudster in the Amazon Web
    -- Services account.
    fraudsterSummaries :: Prelude.Maybe [FraudsterSummary],
    -- | If @NextToken@ is returned, there are more results available. The value
    -- of @NextToken@ is a unique pagination token for each page. Make the call
    -- again using the returned token to retrieve the next page. Keep all other
    -- arguments unchanged. Each pagination token expires after 24 hours.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFraudstersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fraudsterSummaries', 'listFraudstersResponse_fraudsterSummaries' - A list that contains details about each fraudster in the Amazon Web
-- Services account.
--
-- 'nextToken', 'listFraudstersResponse_nextToken' - If @NextToken@ is returned, there are more results available. The value
-- of @NextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours.
--
-- 'httpStatus', 'listFraudstersResponse_httpStatus' - The response's http status code.
newListFraudstersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFraudstersResponse
newListFraudstersResponse pHttpStatus_ =
  ListFraudstersResponse'
    { fraudsterSummaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list that contains details about each fraudster in the Amazon Web
-- Services account.
listFraudstersResponse_fraudsterSummaries :: Lens.Lens' ListFraudstersResponse (Prelude.Maybe [FraudsterSummary])
listFraudstersResponse_fraudsterSummaries = Lens.lens (\ListFraudstersResponse' {fraudsterSummaries} -> fraudsterSummaries) (\s@ListFraudstersResponse' {} a -> s {fraudsterSummaries = a} :: ListFraudstersResponse) Prelude.. Lens.mapping Lens.coerced

-- | If @NextToken@ is returned, there are more results available. The value
-- of @NextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours.
listFraudstersResponse_nextToken :: Lens.Lens' ListFraudstersResponse (Prelude.Maybe Prelude.Text)
listFraudstersResponse_nextToken = Lens.lens (\ListFraudstersResponse' {nextToken} -> nextToken) (\s@ListFraudstersResponse' {} a -> s {nextToken = a} :: ListFraudstersResponse)

-- | The response's http status code.
listFraudstersResponse_httpStatus :: Lens.Lens' ListFraudstersResponse Prelude.Int
listFraudstersResponse_httpStatus = Lens.lens (\ListFraudstersResponse' {httpStatus} -> httpStatus) (\s@ListFraudstersResponse' {} a -> s {httpStatus = a} :: ListFraudstersResponse)

instance Prelude.NFData ListFraudstersResponse where
  rnf ListFraudstersResponse' {..} =
    Prelude.rnf fraudsterSummaries
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
