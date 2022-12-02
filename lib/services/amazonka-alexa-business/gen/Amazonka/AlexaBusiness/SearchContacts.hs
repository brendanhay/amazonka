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
-- Module      : Amazonka.AlexaBusiness.SearchContacts
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches contacts and lists the ones that meet a set of filter and sort
-- criteria.
module Amazonka.AlexaBusiness.SearchContacts
  ( -- * Creating a Request
    SearchContacts (..),
    newSearchContacts,

    -- * Request Lenses
    searchContacts_sortCriteria,
    searchContacts_nextToken,
    searchContacts_filters,
    searchContacts_maxResults,

    -- * Destructuring the Response
    SearchContactsResponse (..),
    newSearchContactsResponse,

    -- * Response Lenses
    searchContactsResponse_nextToken,
    searchContactsResponse_contacts,
    searchContactsResponse_totalCount,
    searchContactsResponse_httpStatus,
  )
where

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchContacts' smart constructor.
data SearchContacts = SearchContacts'
  { -- | The sort order to use in listing the specified set of contacts. The
    -- supported sort keys are DisplayName, FirstName, and LastName.
    sortCriteria :: Prelude.Maybe [Sort],
    -- | An optional token returned from a prior request. Use this token for
    -- pagination of results from this action. If this parameter is specified,
    -- the response only includes results beyond the token, up to the value
    -- specified by MaxResults.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The filters to use to list a specified set of address books. The
    -- supported filter keys are DisplayName, FirstName, LastName, and
    -- AddressBookArns.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of results to include in the response. If more
    -- results exist than the specified MaxResults value, a token is included
    -- in the response so that the remaining results can be retrieved.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchContacts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortCriteria', 'searchContacts_sortCriteria' - The sort order to use in listing the specified set of contacts. The
-- supported sort keys are DisplayName, FirstName, and LastName.
--
-- 'nextToken', 'searchContacts_nextToken' - An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response only includes results beyond the token, up to the value
-- specified by MaxResults.
--
-- 'filters', 'searchContacts_filters' - The filters to use to list a specified set of address books. The
-- supported filter keys are DisplayName, FirstName, LastName, and
-- AddressBookArns.
--
-- 'maxResults', 'searchContacts_maxResults' - The maximum number of results to include in the response. If more
-- results exist than the specified MaxResults value, a token is included
-- in the response so that the remaining results can be retrieved.
newSearchContacts ::
  SearchContacts
newSearchContacts =
  SearchContacts'
    { sortCriteria = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The sort order to use in listing the specified set of contacts. The
-- supported sort keys are DisplayName, FirstName, and LastName.
searchContacts_sortCriteria :: Lens.Lens' SearchContacts (Prelude.Maybe [Sort])
searchContacts_sortCriteria = Lens.lens (\SearchContacts' {sortCriteria} -> sortCriteria) (\s@SearchContacts' {} a -> s {sortCriteria = a} :: SearchContacts) Prelude.. Lens.mapping Lens.coerced

-- | An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response only includes results beyond the token, up to the value
-- specified by MaxResults.
searchContacts_nextToken :: Lens.Lens' SearchContacts (Prelude.Maybe Prelude.Text)
searchContacts_nextToken = Lens.lens (\SearchContacts' {nextToken} -> nextToken) (\s@SearchContacts' {} a -> s {nextToken = a} :: SearchContacts)

-- | The filters to use to list a specified set of address books. The
-- supported filter keys are DisplayName, FirstName, LastName, and
-- AddressBookArns.
searchContacts_filters :: Lens.Lens' SearchContacts (Prelude.Maybe [Filter])
searchContacts_filters = Lens.lens (\SearchContacts' {filters} -> filters) (\s@SearchContacts' {} a -> s {filters = a} :: SearchContacts) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to include in the response. If more
-- results exist than the specified MaxResults value, a token is included
-- in the response so that the remaining results can be retrieved.
searchContacts_maxResults :: Lens.Lens' SearchContacts (Prelude.Maybe Prelude.Natural)
searchContacts_maxResults = Lens.lens (\SearchContacts' {maxResults} -> maxResults) (\s@SearchContacts' {} a -> s {maxResults = a} :: SearchContacts)

instance Core.AWSRequest SearchContacts where
  type
    AWSResponse SearchContacts =
      SearchContactsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchContactsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Contacts" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "TotalCount")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchContacts where
  hashWithSalt _salt SearchContacts' {..} =
    _salt `Prelude.hashWithSalt` sortCriteria
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData SearchContacts where
  rnf SearchContacts' {..} =
    Prelude.rnf sortCriteria
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders SearchContacts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AlexaForBusiness.SearchContacts" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SearchContacts where
  toJSON SearchContacts' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SortCriteria" Data..=) Prelude.<$> sortCriteria,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath SearchContacts where
  toPath = Prelude.const "/"

instance Data.ToQuery SearchContacts where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchContactsResponse' smart constructor.
data SearchContactsResponse = SearchContactsResponse'
  { -- | The token returned to indicate that there is more data available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The contacts that meet the specified set of filter criteria, in sort
    -- order.
    contacts :: Prelude.Maybe [ContactData],
    -- | The total number of contacts returned.
    totalCount :: Prelude.Maybe Prelude.Int,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchContactsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchContactsResponse_nextToken' - The token returned to indicate that there is more data available.
--
-- 'contacts', 'searchContactsResponse_contacts' - The contacts that meet the specified set of filter criteria, in sort
-- order.
--
-- 'totalCount', 'searchContactsResponse_totalCount' - The total number of contacts returned.
--
-- 'httpStatus', 'searchContactsResponse_httpStatus' - The response's http status code.
newSearchContactsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchContactsResponse
newSearchContactsResponse pHttpStatus_ =
  SearchContactsResponse'
    { nextToken =
        Prelude.Nothing,
      contacts = Prelude.Nothing,
      totalCount = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token returned to indicate that there is more data available.
searchContactsResponse_nextToken :: Lens.Lens' SearchContactsResponse (Prelude.Maybe Prelude.Text)
searchContactsResponse_nextToken = Lens.lens (\SearchContactsResponse' {nextToken} -> nextToken) (\s@SearchContactsResponse' {} a -> s {nextToken = a} :: SearchContactsResponse)

-- | The contacts that meet the specified set of filter criteria, in sort
-- order.
searchContactsResponse_contacts :: Lens.Lens' SearchContactsResponse (Prelude.Maybe [ContactData])
searchContactsResponse_contacts = Lens.lens (\SearchContactsResponse' {contacts} -> contacts) (\s@SearchContactsResponse' {} a -> s {contacts = a} :: SearchContactsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The total number of contacts returned.
searchContactsResponse_totalCount :: Lens.Lens' SearchContactsResponse (Prelude.Maybe Prelude.Int)
searchContactsResponse_totalCount = Lens.lens (\SearchContactsResponse' {totalCount} -> totalCount) (\s@SearchContactsResponse' {} a -> s {totalCount = a} :: SearchContactsResponse)

-- | The response's http status code.
searchContactsResponse_httpStatus :: Lens.Lens' SearchContactsResponse Prelude.Int
searchContactsResponse_httpStatus = Lens.lens (\SearchContactsResponse' {httpStatus} -> httpStatus) (\s@SearchContactsResponse' {} a -> s {httpStatus = a} :: SearchContactsResponse)

instance Prelude.NFData SearchContactsResponse where
  rnf SearchContactsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf contacts
      `Prelude.seq` Prelude.rnf totalCount
      `Prelude.seq` Prelude.rnf httpStatus
