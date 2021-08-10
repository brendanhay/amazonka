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
-- Module      : Network.AWS.AlexaBusiness.SearchContacts
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches contacts and lists the ones that meet a set of filter and sort
-- criteria.
module Network.AWS.AlexaBusiness.SearchContacts
  ( -- * Creating a Request
    SearchContacts (..),
    newSearchContacts,

    -- * Request Lenses
    searchContacts_nextToken,
    searchContacts_sortCriteria,
    searchContacts_maxResults,
    searchContacts_filters,

    -- * Destructuring the Response
    SearchContactsResponse (..),
    newSearchContactsResponse,

    -- * Response Lenses
    searchContactsResponse_nextToken,
    searchContactsResponse_totalCount,
    searchContactsResponse_contacts,
    searchContactsResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSearchContacts' smart constructor.
data SearchContacts = SearchContacts'
  { -- | An optional token returned from a prior request. Use this token for
    -- pagination of results from this action. If this parameter is specified,
    -- the response only includes results beyond the token, up to the value
    -- specified by MaxResults.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The sort order to use in listing the specified set of contacts. The
    -- supported sort keys are DisplayName, FirstName, and LastName.
    sortCriteria :: Prelude.Maybe [Sort],
    -- | The maximum number of results to include in the response. If more
    -- results exist than the specified MaxResults value, a token is included
    -- in the response so that the remaining results can be retrieved.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The filters to use to list a specified set of address books. The
    -- supported filter keys are DisplayName, FirstName, LastName, and
    -- AddressBookArns.
    filters :: Prelude.Maybe [Filter]
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
-- 'nextToken', 'searchContacts_nextToken' - An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response only includes results beyond the token, up to the value
-- specified by MaxResults.
--
-- 'sortCriteria', 'searchContacts_sortCriteria' - The sort order to use in listing the specified set of contacts. The
-- supported sort keys are DisplayName, FirstName, and LastName.
--
-- 'maxResults', 'searchContacts_maxResults' - The maximum number of results to include in the response. If more
-- results exist than the specified MaxResults value, a token is included
-- in the response so that the remaining results can be retrieved.
--
-- 'filters', 'searchContacts_filters' - The filters to use to list a specified set of address books. The
-- supported filter keys are DisplayName, FirstName, LastName, and
-- AddressBookArns.
newSearchContacts ::
  SearchContacts
newSearchContacts =
  SearchContacts'
    { nextToken = Prelude.Nothing,
      sortCriteria = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      filters = Prelude.Nothing
    }

-- | An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response only includes results beyond the token, up to the value
-- specified by MaxResults.
searchContacts_nextToken :: Lens.Lens' SearchContacts (Prelude.Maybe Prelude.Text)
searchContacts_nextToken = Lens.lens (\SearchContacts' {nextToken} -> nextToken) (\s@SearchContacts' {} a -> s {nextToken = a} :: SearchContacts)

-- | The sort order to use in listing the specified set of contacts. The
-- supported sort keys are DisplayName, FirstName, and LastName.
searchContacts_sortCriteria :: Lens.Lens' SearchContacts (Prelude.Maybe [Sort])
searchContacts_sortCriteria = Lens.lens (\SearchContacts' {sortCriteria} -> sortCriteria) (\s@SearchContacts' {} a -> s {sortCriteria = a} :: SearchContacts) Prelude.. Lens.mapping Lens._Coerce

-- | The maximum number of results to include in the response. If more
-- results exist than the specified MaxResults value, a token is included
-- in the response so that the remaining results can be retrieved.
searchContacts_maxResults :: Lens.Lens' SearchContacts (Prelude.Maybe Prelude.Natural)
searchContacts_maxResults = Lens.lens (\SearchContacts' {maxResults} -> maxResults) (\s@SearchContacts' {} a -> s {maxResults = a} :: SearchContacts)

-- | The filters to use to list a specified set of address books. The
-- supported filter keys are DisplayName, FirstName, LastName, and
-- AddressBookArns.
searchContacts_filters :: Lens.Lens' SearchContacts (Prelude.Maybe [Filter])
searchContacts_filters = Lens.lens (\SearchContacts' {filters} -> filters) (\s@SearchContacts' {} a -> s {filters = a} :: SearchContacts) Prelude.. Lens.mapping Lens._Coerce

instance Core.AWSRequest SearchContacts where
  type
    AWSResponse SearchContacts =
      SearchContactsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchContactsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "TotalCount")
            Prelude.<*> (x Core..?> "Contacts" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchContacts

instance Prelude.NFData SearchContacts

instance Core.ToHeaders SearchContacts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.SearchContacts" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON SearchContacts where
  toJSON SearchContacts' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("SortCriteria" Core..=) Prelude.<$> sortCriteria,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("Filters" Core..=) Prelude.<$> filters
          ]
      )

instance Core.ToPath SearchContacts where
  toPath = Prelude.const "/"

instance Core.ToQuery SearchContacts where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchContactsResponse' smart constructor.
data SearchContactsResponse = SearchContactsResponse'
  { -- | The token returned to indicate that there is more data available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The total number of contacts returned.
    totalCount :: Prelude.Maybe Prelude.Int,
    -- | The contacts that meet the specified set of filter criteria, in sort
    -- order.
    contacts :: Prelude.Maybe [ContactData],
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
-- 'totalCount', 'searchContactsResponse_totalCount' - The total number of contacts returned.
--
-- 'contacts', 'searchContactsResponse_contacts' - The contacts that meet the specified set of filter criteria, in sort
-- order.
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
      totalCount = Prelude.Nothing,
      contacts = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token returned to indicate that there is more data available.
searchContactsResponse_nextToken :: Lens.Lens' SearchContactsResponse (Prelude.Maybe Prelude.Text)
searchContactsResponse_nextToken = Lens.lens (\SearchContactsResponse' {nextToken} -> nextToken) (\s@SearchContactsResponse' {} a -> s {nextToken = a} :: SearchContactsResponse)

-- | The total number of contacts returned.
searchContactsResponse_totalCount :: Lens.Lens' SearchContactsResponse (Prelude.Maybe Prelude.Int)
searchContactsResponse_totalCount = Lens.lens (\SearchContactsResponse' {totalCount} -> totalCount) (\s@SearchContactsResponse' {} a -> s {totalCount = a} :: SearchContactsResponse)

-- | The contacts that meet the specified set of filter criteria, in sort
-- order.
searchContactsResponse_contacts :: Lens.Lens' SearchContactsResponse (Prelude.Maybe [ContactData])
searchContactsResponse_contacts = Lens.lens (\SearchContactsResponse' {contacts} -> contacts) (\s@SearchContactsResponse' {} a -> s {contacts = a} :: SearchContactsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
searchContactsResponse_httpStatus :: Lens.Lens' SearchContactsResponse Prelude.Int
searchContactsResponse_httpStatus = Lens.lens (\SearchContactsResponse' {httpStatus} -> httpStatus) (\s@SearchContactsResponse' {} a -> s {httpStatus = a} :: SearchContactsResponse)

instance Prelude.NFData SearchContactsResponse
