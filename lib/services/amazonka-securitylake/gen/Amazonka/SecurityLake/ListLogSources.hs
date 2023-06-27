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
-- Module      : Amazonka.SecurityLake.ListLogSources
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the log sources in the current Amazon Web Services Region.
--
-- This operation returns paginated results.
module Amazonka.SecurityLake.ListLogSources
  ( -- * Creating a Request
    ListLogSources (..),
    newListLogSources,

    -- * Request Lenses
    listLogSources_accounts,
    listLogSources_maxResults,
    listLogSources_nextToken,
    listLogSources_regions,
    listLogSources_sources,

    -- * Destructuring the Response
    ListLogSourcesResponse (..),
    newListLogSourcesResponse,

    -- * Response Lenses
    listLogSourcesResponse_nextToken,
    listLogSourcesResponse_sources,
    listLogSourcesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityLake.Types

-- | /See:/ 'newListLogSources' smart constructor.
data ListLogSources = ListLogSources'
  { -- | The list of Amazon Web Services accounts for which log sources are
    -- displayed.
    accounts :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of accounts for which the log sources are displayed.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If nextToken is returned, there are more results available. You can
    -- repeat the call using the returned token to retrieve the next page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of regions for which log sources are displayed.
    regions :: Prelude.Maybe [Prelude.Text],
    -- | The list of sources for which log sources are displayed.
    sources :: Prelude.Maybe [LogSourceResource]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLogSources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accounts', 'listLogSources_accounts' - The list of Amazon Web Services accounts for which log sources are
-- displayed.
--
-- 'maxResults', 'listLogSources_maxResults' - The maximum number of accounts for which the log sources are displayed.
--
-- 'nextToken', 'listLogSources_nextToken' - If nextToken is returned, there are more results available. You can
-- repeat the call using the returned token to retrieve the next page.
--
-- 'regions', 'listLogSources_regions' - The list of regions for which log sources are displayed.
--
-- 'sources', 'listLogSources_sources' - The list of sources for which log sources are displayed.
newListLogSources ::
  ListLogSources
newListLogSources =
  ListLogSources'
    { accounts = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      regions = Prelude.Nothing,
      sources = Prelude.Nothing
    }

-- | The list of Amazon Web Services accounts for which log sources are
-- displayed.
listLogSources_accounts :: Lens.Lens' ListLogSources (Prelude.Maybe [Prelude.Text])
listLogSources_accounts = Lens.lens (\ListLogSources' {accounts} -> accounts) (\s@ListLogSources' {} a -> s {accounts = a} :: ListLogSources) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of accounts for which the log sources are displayed.
listLogSources_maxResults :: Lens.Lens' ListLogSources (Prelude.Maybe Prelude.Natural)
listLogSources_maxResults = Lens.lens (\ListLogSources' {maxResults} -> maxResults) (\s@ListLogSources' {} a -> s {maxResults = a} :: ListLogSources)

-- | If nextToken is returned, there are more results available. You can
-- repeat the call using the returned token to retrieve the next page.
listLogSources_nextToken :: Lens.Lens' ListLogSources (Prelude.Maybe Prelude.Text)
listLogSources_nextToken = Lens.lens (\ListLogSources' {nextToken} -> nextToken) (\s@ListLogSources' {} a -> s {nextToken = a} :: ListLogSources)

-- | The list of regions for which log sources are displayed.
listLogSources_regions :: Lens.Lens' ListLogSources (Prelude.Maybe [Prelude.Text])
listLogSources_regions = Lens.lens (\ListLogSources' {regions} -> regions) (\s@ListLogSources' {} a -> s {regions = a} :: ListLogSources) Prelude.. Lens.mapping Lens.coerced

-- | The list of sources for which log sources are displayed.
listLogSources_sources :: Lens.Lens' ListLogSources (Prelude.Maybe [LogSourceResource])
listLogSources_sources = Lens.lens (\ListLogSources' {sources} -> sources) (\s@ListLogSources' {} a -> s {sources = a} :: ListLogSources) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSPager ListLogSources where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listLogSourcesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listLogSourcesResponse_sources
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listLogSources_nextToken
          Lens..~ rs
          Lens.^? listLogSourcesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListLogSources where
  type
    AWSResponse ListLogSources =
      ListLogSourcesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLogSourcesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "sources" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListLogSources where
  hashWithSalt _salt ListLogSources' {..} =
    _salt
      `Prelude.hashWithSalt` accounts
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` regions
      `Prelude.hashWithSalt` sources

instance Prelude.NFData ListLogSources where
  rnf ListLogSources' {..} =
    Prelude.rnf accounts
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf regions
      `Prelude.seq` Prelude.rnf sources

instance Data.ToHeaders ListLogSources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListLogSources where
  toJSON ListLogSources' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("accounts" Data..=) Prelude.<$> accounts,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("regions" Data..=) Prelude.<$> regions,
            ("sources" Data..=) Prelude.<$> sources
          ]
      )

instance Data.ToPath ListLogSources where
  toPath = Prelude.const "/v1/datalake/logsources/list"

instance Data.ToQuery ListLogSources where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListLogSourcesResponse' smart constructor.
data ListLogSourcesResponse = ListLogSourcesResponse'
  { -- | If nextToken is returned, there are more results available. You can
    -- repeat the call using the returned token to retrieve the next page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of log sources in your organization that send data to the data
    -- lake.
    sources :: Prelude.Maybe [LogSource],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLogSourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listLogSourcesResponse_nextToken' - If nextToken is returned, there are more results available. You can
-- repeat the call using the returned token to retrieve the next page.
--
-- 'sources', 'listLogSourcesResponse_sources' - The list of log sources in your organization that send data to the data
-- lake.
--
-- 'httpStatus', 'listLogSourcesResponse_httpStatus' - The response's http status code.
newListLogSourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListLogSourcesResponse
newListLogSourcesResponse pHttpStatus_ =
  ListLogSourcesResponse'
    { nextToken =
        Prelude.Nothing,
      sources = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If nextToken is returned, there are more results available. You can
-- repeat the call using the returned token to retrieve the next page.
listLogSourcesResponse_nextToken :: Lens.Lens' ListLogSourcesResponse (Prelude.Maybe Prelude.Text)
listLogSourcesResponse_nextToken = Lens.lens (\ListLogSourcesResponse' {nextToken} -> nextToken) (\s@ListLogSourcesResponse' {} a -> s {nextToken = a} :: ListLogSourcesResponse)

-- | The list of log sources in your organization that send data to the data
-- lake.
listLogSourcesResponse_sources :: Lens.Lens' ListLogSourcesResponse (Prelude.Maybe [LogSource])
listLogSourcesResponse_sources = Lens.lens (\ListLogSourcesResponse' {sources} -> sources) (\s@ListLogSourcesResponse' {} a -> s {sources = a} :: ListLogSourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listLogSourcesResponse_httpStatus :: Lens.Lens' ListLogSourcesResponse Prelude.Int
listLogSourcesResponse_httpStatus = Lens.lens (\ListLogSourcesResponse' {httpStatus} -> httpStatus) (\s@ListLogSourcesResponse' {} a -> s {httpStatus = a} :: ListLogSourcesResponse)

instance Prelude.NFData ListLogSourcesResponse where
  rnf ListLogSourcesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sources
      `Prelude.seq` Prelude.rnf httpStatus
