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
-- Module      : Amazonka.ConnectCases.SearchCases
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches for cases within their associated Cases domain. Search results
-- are returned as a paginated list of abridged case documents.
--
-- This operation returns paginated results.
module Amazonka.ConnectCases.SearchCases
  ( -- * Creating a Request
    SearchCases (..),
    newSearchCases,

    -- * Request Lenses
    searchCases_fields,
    searchCases_filter,
    searchCases_maxResults,
    searchCases_nextToken,
    searchCases_searchTerm,
    searchCases_sorts,
    searchCases_domainId,

    -- * Destructuring the Response
    SearchCasesResponse (..),
    newSearchCasesResponse,

    -- * Response Lenses
    searchCasesResponse_nextToken,
    searchCasesResponse_httpStatus,
    searchCasesResponse_cases,
  )
where

import Amazonka.ConnectCases.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchCases' smart constructor.
data SearchCases = SearchCases'
  { -- | The list of field identifiers to be returned as part of the response.
    fields :: Prelude.Maybe [FieldIdentifier],
    -- | A list of filter objects.
    filter' :: Prelude.Maybe CaseFilter,
    -- | The maximum number of cases to return. The current maximum supported
    -- value is 25. This is also the default value when no other value is
    -- provided.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A word or phrase used to perform a quick search.
    searchTerm :: Prelude.Maybe Prelude.Text,
    -- | A list of sorts where each sort specifies a field and their sort order
    -- to be applied to the results.
    sorts :: Prelude.Maybe [Sort],
    -- | The unique identifier of the Cases domain.
    domainId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchCases' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fields', 'searchCases_fields' - The list of field identifiers to be returned as part of the response.
--
-- 'filter'', 'searchCases_filter' - A list of filter objects.
--
-- 'maxResults', 'searchCases_maxResults' - The maximum number of cases to return. The current maximum supported
-- value is 25. This is also the default value when no other value is
-- provided.
--
-- 'nextToken', 'searchCases_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'searchTerm', 'searchCases_searchTerm' - A word or phrase used to perform a quick search.
--
-- 'sorts', 'searchCases_sorts' - A list of sorts where each sort specifies a field and their sort order
-- to be applied to the results.
--
-- 'domainId', 'searchCases_domainId' - The unique identifier of the Cases domain.
newSearchCases ::
  -- | 'domainId'
  Prelude.Text ->
  SearchCases
newSearchCases pDomainId_ =
  SearchCases'
    { fields = Prelude.Nothing,
      filter' = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      searchTerm = Prelude.Nothing,
      sorts = Prelude.Nothing,
      domainId = pDomainId_
    }

-- | The list of field identifiers to be returned as part of the response.
searchCases_fields :: Lens.Lens' SearchCases (Prelude.Maybe [FieldIdentifier])
searchCases_fields = Lens.lens (\SearchCases' {fields} -> fields) (\s@SearchCases' {} a -> s {fields = a} :: SearchCases) Prelude.. Lens.mapping Lens.coerced

-- | A list of filter objects.
searchCases_filter :: Lens.Lens' SearchCases (Prelude.Maybe CaseFilter)
searchCases_filter = Lens.lens (\SearchCases' {filter'} -> filter') (\s@SearchCases' {} a -> s {filter' = a} :: SearchCases)

-- | The maximum number of cases to return. The current maximum supported
-- value is 25. This is also the default value when no other value is
-- provided.
searchCases_maxResults :: Lens.Lens' SearchCases (Prelude.Maybe Prelude.Natural)
searchCases_maxResults = Lens.lens (\SearchCases' {maxResults} -> maxResults) (\s@SearchCases' {} a -> s {maxResults = a} :: SearchCases)

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
searchCases_nextToken :: Lens.Lens' SearchCases (Prelude.Maybe Prelude.Text)
searchCases_nextToken = Lens.lens (\SearchCases' {nextToken} -> nextToken) (\s@SearchCases' {} a -> s {nextToken = a} :: SearchCases)

-- | A word or phrase used to perform a quick search.
searchCases_searchTerm :: Lens.Lens' SearchCases (Prelude.Maybe Prelude.Text)
searchCases_searchTerm = Lens.lens (\SearchCases' {searchTerm} -> searchTerm) (\s@SearchCases' {} a -> s {searchTerm = a} :: SearchCases)

-- | A list of sorts where each sort specifies a field and their sort order
-- to be applied to the results.
searchCases_sorts :: Lens.Lens' SearchCases (Prelude.Maybe [Sort])
searchCases_sorts = Lens.lens (\SearchCases' {sorts} -> sorts) (\s@SearchCases' {} a -> s {sorts = a} :: SearchCases) Prelude.. Lens.mapping Lens.coerced

-- | The unique identifier of the Cases domain.
searchCases_domainId :: Lens.Lens' SearchCases Prelude.Text
searchCases_domainId = Lens.lens (\SearchCases' {domainId} -> domainId) (\s@SearchCases' {} a -> s {domainId = a} :: SearchCases)

instance Core.AWSPager SearchCases where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? searchCasesResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop (rs Lens.^. searchCasesResponse_cases) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& searchCases_nextToken
          Lens..~ rs
          Lens.^? searchCasesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest SearchCases where
  type AWSResponse SearchCases = SearchCasesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchCasesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "cases" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable SearchCases where
  hashWithSalt _salt SearchCases' {..} =
    _salt `Prelude.hashWithSalt` fields
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` searchTerm
      `Prelude.hashWithSalt` sorts
      `Prelude.hashWithSalt` domainId

instance Prelude.NFData SearchCases where
  rnf SearchCases' {..} =
    Prelude.rnf fields
      `Prelude.seq` Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf searchTerm
      `Prelude.seq` Prelude.rnf sorts
      `Prelude.seq` Prelude.rnf domainId

instance Data.ToHeaders SearchCases where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SearchCases where
  toJSON SearchCases' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("fields" Data..=) Prelude.<$> fields,
            ("filter" Data..=) Prelude.<$> filter',
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("searchTerm" Data..=) Prelude.<$> searchTerm,
            ("sorts" Data..=) Prelude.<$> sorts
          ]
      )

instance Data.ToPath SearchCases where
  toPath SearchCases' {..} =
    Prelude.mconcat
      ["/domains/", Data.toBS domainId, "/cases-search"]

instance Data.ToQuery SearchCases where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchCasesResponse' smart constructor.
data SearchCasesResponse = SearchCasesResponse'
  { -- | The token for the next set of results. This is null if there are no more
    -- results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of case documents where each case contains the properties
    -- @CaseId@ and @Fields@ where each field is a complex union structure.
    cases :: [SearchCasesResponseItem]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchCasesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchCasesResponse_nextToken' - The token for the next set of results. This is null if there are no more
-- results to return.
--
-- 'httpStatus', 'searchCasesResponse_httpStatus' - The response's http status code.
--
-- 'cases', 'searchCasesResponse_cases' - A list of case documents where each case contains the properties
-- @CaseId@ and @Fields@ where each field is a complex union structure.
newSearchCasesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchCasesResponse
newSearchCasesResponse pHttpStatus_ =
  SearchCasesResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      cases = Prelude.mempty
    }

-- | The token for the next set of results. This is null if there are no more
-- results to return.
searchCasesResponse_nextToken :: Lens.Lens' SearchCasesResponse (Prelude.Maybe Prelude.Text)
searchCasesResponse_nextToken = Lens.lens (\SearchCasesResponse' {nextToken} -> nextToken) (\s@SearchCasesResponse' {} a -> s {nextToken = a} :: SearchCasesResponse)

-- | The response's http status code.
searchCasesResponse_httpStatus :: Lens.Lens' SearchCasesResponse Prelude.Int
searchCasesResponse_httpStatus = Lens.lens (\SearchCasesResponse' {httpStatus} -> httpStatus) (\s@SearchCasesResponse' {} a -> s {httpStatus = a} :: SearchCasesResponse)

-- | A list of case documents where each case contains the properties
-- @CaseId@ and @Fields@ where each field is a complex union structure.
searchCasesResponse_cases :: Lens.Lens' SearchCasesResponse [SearchCasesResponseItem]
searchCasesResponse_cases = Lens.lens (\SearchCasesResponse' {cases} -> cases) (\s@SearchCasesResponse' {} a -> s {cases = a} :: SearchCasesResponse) Prelude.. Lens.coerced

instance Prelude.NFData SearchCasesResponse where
  rnf SearchCasesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf cases
