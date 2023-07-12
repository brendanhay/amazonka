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
-- Module      : Amazonka.ConnectCases.SearchRelatedItems
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches for related items that are associated with a case.
--
-- If no filters are provided, this returns all related items associated
-- with a case.
--
-- This operation returns paginated results.
module Amazonka.ConnectCases.SearchRelatedItems
  ( -- * Creating a Request
    SearchRelatedItems (..),
    newSearchRelatedItems,

    -- * Request Lenses
    searchRelatedItems_filters,
    searchRelatedItems_maxResults,
    searchRelatedItems_nextToken,
    searchRelatedItems_caseId,
    searchRelatedItems_domainId,

    -- * Destructuring the Response
    SearchRelatedItemsResponse (..),
    newSearchRelatedItemsResponse,

    -- * Response Lenses
    searchRelatedItemsResponse_nextToken,
    searchRelatedItemsResponse_httpStatus,
    searchRelatedItemsResponse_relatedItems,
  )
where

import Amazonka.ConnectCases.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchRelatedItems' smart constructor.
data SearchRelatedItems = SearchRelatedItems'
  { -- | The list of types of related items and their parameters to use for
    -- filtering.
    filters :: Prelude.Maybe [RelatedItemTypeFilter],
    -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier of the case.
    caseId :: Prelude.Text,
    -- | The unique identifier of the Cases domain.
    domainId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchRelatedItems' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'searchRelatedItems_filters' - The list of types of related items and their parameters to use for
-- filtering.
--
-- 'maxResults', 'searchRelatedItems_maxResults' - The maximum number of results to return per page.
--
-- 'nextToken', 'searchRelatedItems_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'caseId', 'searchRelatedItems_caseId' - A unique identifier of the case.
--
-- 'domainId', 'searchRelatedItems_domainId' - The unique identifier of the Cases domain.
newSearchRelatedItems ::
  -- | 'caseId'
  Prelude.Text ->
  -- | 'domainId'
  Prelude.Text ->
  SearchRelatedItems
newSearchRelatedItems pCaseId_ pDomainId_ =
  SearchRelatedItems'
    { filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      caseId = pCaseId_,
      domainId = pDomainId_
    }

-- | The list of types of related items and their parameters to use for
-- filtering.
searchRelatedItems_filters :: Lens.Lens' SearchRelatedItems (Prelude.Maybe [RelatedItemTypeFilter])
searchRelatedItems_filters = Lens.lens (\SearchRelatedItems' {filters} -> filters) (\s@SearchRelatedItems' {} a -> s {filters = a} :: SearchRelatedItems) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return per page.
searchRelatedItems_maxResults :: Lens.Lens' SearchRelatedItems (Prelude.Maybe Prelude.Natural)
searchRelatedItems_maxResults = Lens.lens (\SearchRelatedItems' {maxResults} -> maxResults) (\s@SearchRelatedItems' {} a -> s {maxResults = a} :: SearchRelatedItems)

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
searchRelatedItems_nextToken :: Lens.Lens' SearchRelatedItems (Prelude.Maybe Prelude.Text)
searchRelatedItems_nextToken = Lens.lens (\SearchRelatedItems' {nextToken} -> nextToken) (\s@SearchRelatedItems' {} a -> s {nextToken = a} :: SearchRelatedItems)

-- | A unique identifier of the case.
searchRelatedItems_caseId :: Lens.Lens' SearchRelatedItems Prelude.Text
searchRelatedItems_caseId = Lens.lens (\SearchRelatedItems' {caseId} -> caseId) (\s@SearchRelatedItems' {} a -> s {caseId = a} :: SearchRelatedItems)

-- | The unique identifier of the Cases domain.
searchRelatedItems_domainId :: Lens.Lens' SearchRelatedItems Prelude.Text
searchRelatedItems_domainId = Lens.lens (\SearchRelatedItems' {domainId} -> domainId) (\s@SearchRelatedItems' {} a -> s {domainId = a} :: SearchRelatedItems)

instance Core.AWSPager SearchRelatedItems where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? searchRelatedItemsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        (rs Lens.^. searchRelatedItemsResponse_relatedItems) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& searchRelatedItems_nextToken
          Lens..~ rs
          Lens.^? searchRelatedItemsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest SearchRelatedItems where
  type
    AWSResponse SearchRelatedItems =
      SearchRelatedItemsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchRelatedItemsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "relatedItems" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable SearchRelatedItems where
  hashWithSalt _salt SearchRelatedItems' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` caseId
      `Prelude.hashWithSalt` domainId

instance Prelude.NFData SearchRelatedItems where
  rnf SearchRelatedItems' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf caseId
      `Prelude.seq` Prelude.rnf domainId

instance Data.ToHeaders SearchRelatedItems where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SearchRelatedItems where
  toJSON SearchRelatedItems' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("filters" Data..=) Prelude.<$> filters,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath SearchRelatedItems where
  toPath SearchRelatedItems' {..} =
    Prelude.mconcat
      [ "/domains/",
        Data.toBS domainId,
        "/cases/",
        Data.toBS caseId,
        "/related-items-search"
      ]

instance Data.ToQuery SearchRelatedItems where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchRelatedItemsResponse' smart constructor.
data SearchRelatedItemsResponse = SearchRelatedItemsResponse'
  { -- | The token for the next set of results. This is null if there are no more
    -- results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of items related to a case.
    relatedItems :: [SearchRelatedItemsResponseItem]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchRelatedItemsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchRelatedItemsResponse_nextToken' - The token for the next set of results. This is null if there are no more
-- results to return.
--
-- 'httpStatus', 'searchRelatedItemsResponse_httpStatus' - The response's http status code.
--
-- 'relatedItems', 'searchRelatedItemsResponse_relatedItems' - A list of items related to a case.
newSearchRelatedItemsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchRelatedItemsResponse
newSearchRelatedItemsResponse pHttpStatus_ =
  SearchRelatedItemsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      relatedItems = Prelude.mempty
    }

-- | The token for the next set of results. This is null if there are no more
-- results to return.
searchRelatedItemsResponse_nextToken :: Lens.Lens' SearchRelatedItemsResponse (Prelude.Maybe Prelude.Text)
searchRelatedItemsResponse_nextToken = Lens.lens (\SearchRelatedItemsResponse' {nextToken} -> nextToken) (\s@SearchRelatedItemsResponse' {} a -> s {nextToken = a} :: SearchRelatedItemsResponse)

-- | The response's http status code.
searchRelatedItemsResponse_httpStatus :: Lens.Lens' SearchRelatedItemsResponse Prelude.Int
searchRelatedItemsResponse_httpStatus = Lens.lens (\SearchRelatedItemsResponse' {httpStatus} -> httpStatus) (\s@SearchRelatedItemsResponse' {} a -> s {httpStatus = a} :: SearchRelatedItemsResponse)

-- | A list of items related to a case.
searchRelatedItemsResponse_relatedItems :: Lens.Lens' SearchRelatedItemsResponse [SearchRelatedItemsResponseItem]
searchRelatedItemsResponse_relatedItems = Lens.lens (\SearchRelatedItemsResponse' {relatedItems} -> relatedItems) (\s@SearchRelatedItemsResponse' {} a -> s {relatedItems = a} :: SearchRelatedItemsResponse) Prelude.. Lens.coerced

instance Prelude.NFData SearchRelatedItemsResponse where
  rnf SearchRelatedItemsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf relatedItems
