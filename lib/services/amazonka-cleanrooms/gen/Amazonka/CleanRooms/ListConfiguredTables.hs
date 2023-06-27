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
-- Module      : Amazonka.CleanRooms.ListConfiguredTables
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists configured tables.
--
-- This operation returns paginated results.
module Amazonka.CleanRooms.ListConfiguredTables
  ( -- * Creating a Request
    ListConfiguredTables (..),
    newListConfiguredTables,

    -- * Request Lenses
    listConfiguredTables_maxResults,
    listConfiguredTables_nextToken,

    -- * Destructuring the Response
    ListConfiguredTablesResponse (..),
    newListConfiguredTablesResponse,

    -- * Response Lenses
    listConfiguredTablesResponse_nextToken,
    listConfiguredTablesResponse_httpStatus,
    listConfiguredTablesResponse_configuredTableSummaries,
  )
where

import Amazonka.CleanRooms.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListConfiguredTables' smart constructor.
data ListConfiguredTables = ListConfiguredTables'
  { -- | The maximum size of the results that is returned per call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token value retrieved from a previous call to access the next page
    -- of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListConfiguredTables' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listConfiguredTables_maxResults' - The maximum size of the results that is returned per call.
--
-- 'nextToken', 'listConfiguredTables_nextToken' - The token value retrieved from a previous call to access the next page
-- of results.
newListConfiguredTables ::
  ListConfiguredTables
newListConfiguredTables =
  ListConfiguredTables'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum size of the results that is returned per call.
listConfiguredTables_maxResults :: Lens.Lens' ListConfiguredTables (Prelude.Maybe Prelude.Natural)
listConfiguredTables_maxResults = Lens.lens (\ListConfiguredTables' {maxResults} -> maxResults) (\s@ListConfiguredTables' {} a -> s {maxResults = a} :: ListConfiguredTables)

-- | The token value retrieved from a previous call to access the next page
-- of results.
listConfiguredTables_nextToken :: Lens.Lens' ListConfiguredTables (Prelude.Maybe Prelude.Text)
listConfiguredTables_nextToken = Lens.lens (\ListConfiguredTables' {nextToken} -> nextToken) (\s@ListConfiguredTables' {} a -> s {nextToken = a} :: ListConfiguredTables)

instance Core.AWSPager ListConfiguredTables where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listConfiguredTablesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listConfiguredTablesResponse_configuredTableSummaries
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listConfiguredTables_nextToken
          Lens..~ rs
          Lens.^? listConfiguredTablesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListConfiguredTables where
  type
    AWSResponse ListConfiguredTables =
      ListConfiguredTablesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListConfiguredTablesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "configuredTableSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListConfiguredTables where
  hashWithSalt _salt ListConfiguredTables' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListConfiguredTables where
  rnf ListConfiguredTables' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListConfiguredTables where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListConfiguredTables where
  toPath = Prelude.const "/configuredTables"

instance Data.ToQuery ListConfiguredTables where
  toQuery ListConfiguredTables' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListConfiguredTablesResponse' smart constructor.
data ListConfiguredTablesResponse = ListConfiguredTablesResponse'
  { -- | The token value retrieved from a previous call to access the next page
    -- of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The configured tables listed by the request.
    configuredTableSummaries :: [ConfiguredTableSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListConfiguredTablesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listConfiguredTablesResponse_nextToken' - The token value retrieved from a previous call to access the next page
-- of results.
--
-- 'httpStatus', 'listConfiguredTablesResponse_httpStatus' - The response's http status code.
--
-- 'configuredTableSummaries', 'listConfiguredTablesResponse_configuredTableSummaries' - The configured tables listed by the request.
newListConfiguredTablesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListConfiguredTablesResponse
newListConfiguredTablesResponse pHttpStatus_ =
  ListConfiguredTablesResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      configuredTableSummaries = Prelude.mempty
    }

-- | The token value retrieved from a previous call to access the next page
-- of results.
listConfiguredTablesResponse_nextToken :: Lens.Lens' ListConfiguredTablesResponse (Prelude.Maybe Prelude.Text)
listConfiguredTablesResponse_nextToken = Lens.lens (\ListConfiguredTablesResponse' {nextToken} -> nextToken) (\s@ListConfiguredTablesResponse' {} a -> s {nextToken = a} :: ListConfiguredTablesResponse)

-- | The response's http status code.
listConfiguredTablesResponse_httpStatus :: Lens.Lens' ListConfiguredTablesResponse Prelude.Int
listConfiguredTablesResponse_httpStatus = Lens.lens (\ListConfiguredTablesResponse' {httpStatus} -> httpStatus) (\s@ListConfiguredTablesResponse' {} a -> s {httpStatus = a} :: ListConfiguredTablesResponse)

-- | The configured tables listed by the request.
listConfiguredTablesResponse_configuredTableSummaries :: Lens.Lens' ListConfiguredTablesResponse [ConfiguredTableSummary]
listConfiguredTablesResponse_configuredTableSummaries = Lens.lens (\ListConfiguredTablesResponse' {configuredTableSummaries} -> configuredTableSummaries) (\s@ListConfiguredTablesResponse' {} a -> s {configuredTableSummaries = a} :: ListConfiguredTablesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListConfiguredTablesResponse where
  rnf ListConfiguredTablesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf configuredTableSummaries
