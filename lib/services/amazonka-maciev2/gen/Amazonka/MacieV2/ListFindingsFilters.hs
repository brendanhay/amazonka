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
-- Module      : Amazonka.MacieV2.ListFindingsFilters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a subset of information about all the findings filters for an
-- account.
--
-- This operation returns paginated results.
module Amazonka.MacieV2.ListFindingsFilters
  ( -- * Creating a Request
    ListFindingsFilters (..),
    newListFindingsFilters,

    -- * Request Lenses
    listFindingsFilters_maxResults,
    listFindingsFilters_nextToken,

    -- * Destructuring the Response
    ListFindingsFiltersResponse (..),
    newListFindingsFiltersResponse,

    -- * Response Lenses
    listFindingsFiltersResponse_findingsFilterListItems,
    listFindingsFiltersResponse_nextToken,
    listFindingsFiltersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListFindingsFilters' smart constructor.
data ListFindingsFilters = ListFindingsFilters'
  { -- | The maximum number of items to include in each page of a paginated
    -- response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The nextToken string that specifies which page of results to return in a
    -- paginated response.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFindingsFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listFindingsFilters_maxResults' - The maximum number of items to include in each page of a paginated
-- response.
--
-- 'nextToken', 'listFindingsFilters_nextToken' - The nextToken string that specifies which page of results to return in a
-- paginated response.
newListFindingsFilters ::
  ListFindingsFilters
newListFindingsFilters =
  ListFindingsFilters'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of items to include in each page of a paginated
-- response.
listFindingsFilters_maxResults :: Lens.Lens' ListFindingsFilters (Prelude.Maybe Prelude.Natural)
listFindingsFilters_maxResults = Lens.lens (\ListFindingsFilters' {maxResults} -> maxResults) (\s@ListFindingsFilters' {} a -> s {maxResults = a} :: ListFindingsFilters)

-- | The nextToken string that specifies which page of results to return in a
-- paginated response.
listFindingsFilters_nextToken :: Lens.Lens' ListFindingsFilters (Prelude.Maybe Prelude.Text)
listFindingsFilters_nextToken = Lens.lens (\ListFindingsFilters' {nextToken} -> nextToken) (\s@ListFindingsFilters' {} a -> s {nextToken = a} :: ListFindingsFilters)

instance Core.AWSPager ListFindingsFilters where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listFindingsFiltersResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listFindingsFiltersResponse_findingsFilterListItems
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listFindingsFilters_nextToken
          Lens..~ rs
          Lens.^? listFindingsFiltersResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListFindingsFilters where
  type
    AWSResponse ListFindingsFilters =
      ListFindingsFiltersResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFindingsFiltersResponse'
            Prelude.<$> ( x
                            Data..?> "findingsFilterListItems"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListFindingsFilters where
  hashWithSalt _salt ListFindingsFilters' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListFindingsFilters where
  rnf ListFindingsFilters' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListFindingsFilters where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListFindingsFilters where
  toPath = Prelude.const "/findingsfilters"

instance Data.ToQuery ListFindingsFilters where
  toQuery ListFindingsFilters' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListFindingsFiltersResponse' smart constructor.
data ListFindingsFiltersResponse = ListFindingsFiltersResponse'
  { -- | An array of objects, one for each filter that\'s associated with the
    -- account.
    findingsFilterListItems :: Prelude.Maybe [FindingsFilterListItem],
    -- | The string to use in a subsequent request to get the next page of
    -- results in a paginated response. This value is null if there are no
    -- additional pages.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFindingsFiltersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'findingsFilterListItems', 'listFindingsFiltersResponse_findingsFilterListItems' - An array of objects, one for each filter that\'s associated with the
-- account.
--
-- 'nextToken', 'listFindingsFiltersResponse_nextToken' - The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
--
-- 'httpStatus', 'listFindingsFiltersResponse_httpStatus' - The response's http status code.
newListFindingsFiltersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFindingsFiltersResponse
newListFindingsFiltersResponse pHttpStatus_ =
  ListFindingsFiltersResponse'
    { findingsFilterListItems =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects, one for each filter that\'s associated with the
-- account.
listFindingsFiltersResponse_findingsFilterListItems :: Lens.Lens' ListFindingsFiltersResponse (Prelude.Maybe [FindingsFilterListItem])
listFindingsFiltersResponse_findingsFilterListItems = Lens.lens (\ListFindingsFiltersResponse' {findingsFilterListItems} -> findingsFilterListItems) (\s@ListFindingsFiltersResponse' {} a -> s {findingsFilterListItems = a} :: ListFindingsFiltersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
listFindingsFiltersResponse_nextToken :: Lens.Lens' ListFindingsFiltersResponse (Prelude.Maybe Prelude.Text)
listFindingsFiltersResponse_nextToken = Lens.lens (\ListFindingsFiltersResponse' {nextToken} -> nextToken) (\s@ListFindingsFiltersResponse' {} a -> s {nextToken = a} :: ListFindingsFiltersResponse)

-- | The response's http status code.
listFindingsFiltersResponse_httpStatus :: Lens.Lens' ListFindingsFiltersResponse Prelude.Int
listFindingsFiltersResponse_httpStatus = Lens.lens (\ListFindingsFiltersResponse' {httpStatus} -> httpStatus) (\s@ListFindingsFiltersResponse' {} a -> s {httpStatus = a} :: ListFindingsFiltersResponse)

instance Prelude.NFData ListFindingsFiltersResponse where
  rnf ListFindingsFiltersResponse' {..} =
    Prelude.rnf findingsFilterListItems
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
