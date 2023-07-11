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
-- Module      : Amazonka.MarketplaceCatalog.ListChangeSets
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of change sets owned by the account being used to make
-- the call. You can filter this list by providing any combination of
-- @entityId@, @ChangeSetName@, and status. If you provide more than one
-- filter, the API operation applies a logical AND between the filters.
--
-- You can describe a change during the 60-day request history retention
-- period for API calls.
module Amazonka.MarketplaceCatalog.ListChangeSets
  ( -- * Creating a Request
    ListChangeSets (..),
    newListChangeSets,

    -- * Request Lenses
    listChangeSets_filterList,
    listChangeSets_maxResults,
    listChangeSets_nextToken,
    listChangeSets_sort,
    listChangeSets_catalog,

    -- * Destructuring the Response
    ListChangeSetsResponse (..),
    newListChangeSetsResponse,

    -- * Response Lenses
    listChangeSetsResponse_changeSetSummaryList,
    listChangeSetsResponse_nextToken,
    listChangeSetsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MarketplaceCatalog.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListChangeSets' smart constructor.
data ListChangeSets = ListChangeSets'
  { -- | An array of filter objects.
    filterList :: Prelude.Maybe (Prelude.NonEmpty Filter),
    -- | The maximum number of results returned by a single call. This value must
    -- be provided in the next call to retrieve the next set of results. By
    -- default, this value is 20.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token value retrieved from a previous call to access the next page
    -- of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An object that contains two attributes, @SortBy@ and @SortOrder@.
    sort :: Prelude.Maybe Sort,
    -- | The catalog related to the request. Fixed value: @AWSMarketplace@
    catalog :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListChangeSets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filterList', 'listChangeSets_filterList' - An array of filter objects.
--
-- 'maxResults', 'listChangeSets_maxResults' - The maximum number of results returned by a single call. This value must
-- be provided in the next call to retrieve the next set of results. By
-- default, this value is 20.
--
-- 'nextToken', 'listChangeSets_nextToken' - The token value retrieved from a previous call to access the next page
-- of results.
--
-- 'sort', 'listChangeSets_sort' - An object that contains two attributes, @SortBy@ and @SortOrder@.
--
-- 'catalog', 'listChangeSets_catalog' - The catalog related to the request. Fixed value: @AWSMarketplace@
newListChangeSets ::
  -- | 'catalog'
  Prelude.Text ->
  ListChangeSets
newListChangeSets pCatalog_ =
  ListChangeSets'
    { filterList = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sort = Prelude.Nothing,
      catalog = pCatalog_
    }

-- | An array of filter objects.
listChangeSets_filterList :: Lens.Lens' ListChangeSets (Prelude.Maybe (Prelude.NonEmpty Filter))
listChangeSets_filterList = Lens.lens (\ListChangeSets' {filterList} -> filterList) (\s@ListChangeSets' {} a -> s {filterList = a} :: ListChangeSets) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results returned by a single call. This value must
-- be provided in the next call to retrieve the next set of results. By
-- default, this value is 20.
listChangeSets_maxResults :: Lens.Lens' ListChangeSets (Prelude.Maybe Prelude.Natural)
listChangeSets_maxResults = Lens.lens (\ListChangeSets' {maxResults} -> maxResults) (\s@ListChangeSets' {} a -> s {maxResults = a} :: ListChangeSets)

-- | The token value retrieved from a previous call to access the next page
-- of results.
listChangeSets_nextToken :: Lens.Lens' ListChangeSets (Prelude.Maybe Prelude.Text)
listChangeSets_nextToken = Lens.lens (\ListChangeSets' {nextToken} -> nextToken) (\s@ListChangeSets' {} a -> s {nextToken = a} :: ListChangeSets)

-- | An object that contains two attributes, @SortBy@ and @SortOrder@.
listChangeSets_sort :: Lens.Lens' ListChangeSets (Prelude.Maybe Sort)
listChangeSets_sort = Lens.lens (\ListChangeSets' {sort} -> sort) (\s@ListChangeSets' {} a -> s {sort = a} :: ListChangeSets)

-- | The catalog related to the request. Fixed value: @AWSMarketplace@
listChangeSets_catalog :: Lens.Lens' ListChangeSets Prelude.Text
listChangeSets_catalog = Lens.lens (\ListChangeSets' {catalog} -> catalog) (\s@ListChangeSets' {} a -> s {catalog = a} :: ListChangeSets)

instance Core.AWSRequest ListChangeSets where
  type
    AWSResponse ListChangeSets =
      ListChangeSetsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListChangeSetsResponse'
            Prelude.<$> ( x
                            Data..?> "ChangeSetSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListChangeSets where
  hashWithSalt _salt ListChangeSets' {..} =
    _salt
      `Prelude.hashWithSalt` filterList
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sort
      `Prelude.hashWithSalt` catalog

instance Prelude.NFData ListChangeSets where
  rnf ListChangeSets' {..} =
    Prelude.rnf filterList
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sort
      `Prelude.seq` Prelude.rnf catalog

instance Data.ToHeaders ListChangeSets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListChangeSets where
  toJSON ListChangeSets' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FilterList" Data..=) Prelude.<$> filterList,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("Sort" Data..=) Prelude.<$> sort,
            Prelude.Just ("Catalog" Data..= catalog)
          ]
      )

instance Data.ToPath ListChangeSets where
  toPath = Prelude.const "/ListChangeSets"

instance Data.ToQuery ListChangeSets where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListChangeSetsResponse' smart constructor.
data ListChangeSetsResponse = ListChangeSetsResponse'
  { -- | Array of @ChangeSetSummaryListItem@ objects.
    changeSetSummaryList :: Prelude.Maybe [ChangeSetSummaryListItem],
    -- | The value of the next token, if it exists. Null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListChangeSetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'changeSetSummaryList', 'listChangeSetsResponse_changeSetSummaryList' - Array of @ChangeSetSummaryListItem@ objects.
--
-- 'nextToken', 'listChangeSetsResponse_nextToken' - The value of the next token, if it exists. Null if there are no more
-- results.
--
-- 'httpStatus', 'listChangeSetsResponse_httpStatus' - The response's http status code.
newListChangeSetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListChangeSetsResponse
newListChangeSetsResponse pHttpStatus_ =
  ListChangeSetsResponse'
    { changeSetSummaryList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Array of @ChangeSetSummaryListItem@ objects.
listChangeSetsResponse_changeSetSummaryList :: Lens.Lens' ListChangeSetsResponse (Prelude.Maybe [ChangeSetSummaryListItem])
listChangeSetsResponse_changeSetSummaryList = Lens.lens (\ListChangeSetsResponse' {changeSetSummaryList} -> changeSetSummaryList) (\s@ListChangeSetsResponse' {} a -> s {changeSetSummaryList = a} :: ListChangeSetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The value of the next token, if it exists. Null if there are no more
-- results.
listChangeSetsResponse_nextToken :: Lens.Lens' ListChangeSetsResponse (Prelude.Maybe Prelude.Text)
listChangeSetsResponse_nextToken = Lens.lens (\ListChangeSetsResponse' {nextToken} -> nextToken) (\s@ListChangeSetsResponse' {} a -> s {nextToken = a} :: ListChangeSetsResponse)

-- | The response's http status code.
listChangeSetsResponse_httpStatus :: Lens.Lens' ListChangeSetsResponse Prelude.Int
listChangeSetsResponse_httpStatus = Lens.lens (\ListChangeSetsResponse' {httpStatus} -> httpStatus) (\s@ListChangeSetsResponse' {} a -> s {httpStatus = a} :: ListChangeSetsResponse)

instance Prelude.NFData ListChangeSetsResponse where
  rnf ListChangeSetsResponse' {..} =
    Prelude.rnf changeSetSummaryList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
