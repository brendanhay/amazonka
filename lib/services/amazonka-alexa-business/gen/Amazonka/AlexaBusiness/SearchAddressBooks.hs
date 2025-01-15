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
-- Module      : Amazonka.AlexaBusiness.SearchAddressBooks
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches address books and lists the ones that meet a set of filter and
-- sort criteria.
module Amazonka.AlexaBusiness.SearchAddressBooks
  ( -- * Creating a Request
    SearchAddressBooks (..),
    newSearchAddressBooks,

    -- * Request Lenses
    searchAddressBooks_filters,
    searchAddressBooks_maxResults,
    searchAddressBooks_nextToken,
    searchAddressBooks_sortCriteria,

    -- * Destructuring the Response
    SearchAddressBooksResponse (..),
    newSearchAddressBooksResponse,

    -- * Response Lenses
    searchAddressBooksResponse_addressBooks,
    searchAddressBooksResponse_nextToken,
    searchAddressBooksResponse_totalCount,
    searchAddressBooksResponse_httpStatus,
  )
where

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchAddressBooks' smart constructor.
data SearchAddressBooks = SearchAddressBooks'
  { -- | The filters to use to list a specified set of address books. The
    -- supported filter key is AddressBookName.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of results to include in the response. If more
    -- results exist than the specified MaxResults value, a token is included
    -- in the response so that the remaining results can be retrieved.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | An optional token returned from a prior request. Use this token for
    -- pagination of results from this action. If this parameter is specified,
    -- the response only includes results beyond the token, up to the value
    -- specified by MaxResults.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The sort order to use in listing the specified set of address books. The
    -- supported sort key is AddressBookName.
    sortCriteria :: Prelude.Maybe [Sort]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchAddressBooks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'searchAddressBooks_filters' - The filters to use to list a specified set of address books. The
-- supported filter key is AddressBookName.
--
-- 'maxResults', 'searchAddressBooks_maxResults' - The maximum number of results to include in the response. If more
-- results exist than the specified MaxResults value, a token is included
-- in the response so that the remaining results can be retrieved.
--
-- 'nextToken', 'searchAddressBooks_nextToken' - An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response only includes results beyond the token, up to the value
-- specified by MaxResults.
--
-- 'sortCriteria', 'searchAddressBooks_sortCriteria' - The sort order to use in listing the specified set of address books. The
-- supported sort key is AddressBookName.
newSearchAddressBooks ::
  SearchAddressBooks
newSearchAddressBooks =
  SearchAddressBooks'
    { filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortCriteria = Prelude.Nothing
    }

-- | The filters to use to list a specified set of address books. The
-- supported filter key is AddressBookName.
searchAddressBooks_filters :: Lens.Lens' SearchAddressBooks (Prelude.Maybe [Filter])
searchAddressBooks_filters = Lens.lens (\SearchAddressBooks' {filters} -> filters) (\s@SearchAddressBooks' {} a -> s {filters = a} :: SearchAddressBooks) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to include in the response. If more
-- results exist than the specified MaxResults value, a token is included
-- in the response so that the remaining results can be retrieved.
searchAddressBooks_maxResults :: Lens.Lens' SearchAddressBooks (Prelude.Maybe Prelude.Natural)
searchAddressBooks_maxResults = Lens.lens (\SearchAddressBooks' {maxResults} -> maxResults) (\s@SearchAddressBooks' {} a -> s {maxResults = a} :: SearchAddressBooks)

-- | An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response only includes results beyond the token, up to the value
-- specified by MaxResults.
searchAddressBooks_nextToken :: Lens.Lens' SearchAddressBooks (Prelude.Maybe Prelude.Text)
searchAddressBooks_nextToken = Lens.lens (\SearchAddressBooks' {nextToken} -> nextToken) (\s@SearchAddressBooks' {} a -> s {nextToken = a} :: SearchAddressBooks)

-- | The sort order to use in listing the specified set of address books. The
-- supported sort key is AddressBookName.
searchAddressBooks_sortCriteria :: Lens.Lens' SearchAddressBooks (Prelude.Maybe [Sort])
searchAddressBooks_sortCriteria = Lens.lens (\SearchAddressBooks' {sortCriteria} -> sortCriteria) (\s@SearchAddressBooks' {} a -> s {sortCriteria = a} :: SearchAddressBooks) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest SearchAddressBooks where
  type
    AWSResponse SearchAddressBooks =
      SearchAddressBooksResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchAddressBooksResponse'
            Prelude.<$> (x Data..?> "AddressBooks" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "TotalCount")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchAddressBooks where
  hashWithSalt _salt SearchAddressBooks' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortCriteria

instance Prelude.NFData SearchAddressBooks where
  rnf SearchAddressBooks' {..} =
    Prelude.rnf filters `Prelude.seq`
      Prelude.rnf maxResults `Prelude.seq`
        Prelude.rnf nextToken `Prelude.seq`
          Prelude.rnf sortCriteria

instance Data.ToHeaders SearchAddressBooks where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AlexaForBusiness.SearchAddressBooks" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SearchAddressBooks where
  toJSON SearchAddressBooks' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SortCriteria" Data..=) Prelude.<$> sortCriteria
          ]
      )

instance Data.ToPath SearchAddressBooks where
  toPath = Prelude.const "/"

instance Data.ToQuery SearchAddressBooks where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchAddressBooksResponse' smart constructor.
data SearchAddressBooksResponse = SearchAddressBooksResponse'
  { -- | The address books that meet the specified set of filter criteria, in
    -- sort order.
    addressBooks :: Prelude.Maybe [AddressBookData],
    -- | The token returned to indicate that there is more data available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The total number of address books returned.
    totalCount :: Prelude.Maybe Prelude.Int,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchAddressBooksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addressBooks', 'searchAddressBooksResponse_addressBooks' - The address books that meet the specified set of filter criteria, in
-- sort order.
--
-- 'nextToken', 'searchAddressBooksResponse_nextToken' - The token returned to indicate that there is more data available.
--
-- 'totalCount', 'searchAddressBooksResponse_totalCount' - The total number of address books returned.
--
-- 'httpStatus', 'searchAddressBooksResponse_httpStatus' - The response's http status code.
newSearchAddressBooksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchAddressBooksResponse
newSearchAddressBooksResponse pHttpStatus_ =
  SearchAddressBooksResponse'
    { addressBooks =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      totalCount = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The address books that meet the specified set of filter criteria, in
-- sort order.
searchAddressBooksResponse_addressBooks :: Lens.Lens' SearchAddressBooksResponse (Prelude.Maybe [AddressBookData])
searchAddressBooksResponse_addressBooks = Lens.lens (\SearchAddressBooksResponse' {addressBooks} -> addressBooks) (\s@SearchAddressBooksResponse' {} a -> s {addressBooks = a} :: SearchAddressBooksResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token returned to indicate that there is more data available.
searchAddressBooksResponse_nextToken :: Lens.Lens' SearchAddressBooksResponse (Prelude.Maybe Prelude.Text)
searchAddressBooksResponse_nextToken = Lens.lens (\SearchAddressBooksResponse' {nextToken} -> nextToken) (\s@SearchAddressBooksResponse' {} a -> s {nextToken = a} :: SearchAddressBooksResponse)

-- | The total number of address books returned.
searchAddressBooksResponse_totalCount :: Lens.Lens' SearchAddressBooksResponse (Prelude.Maybe Prelude.Int)
searchAddressBooksResponse_totalCount = Lens.lens (\SearchAddressBooksResponse' {totalCount} -> totalCount) (\s@SearchAddressBooksResponse' {} a -> s {totalCount = a} :: SearchAddressBooksResponse)

-- | The response's http status code.
searchAddressBooksResponse_httpStatus :: Lens.Lens' SearchAddressBooksResponse Prelude.Int
searchAddressBooksResponse_httpStatus = Lens.lens (\SearchAddressBooksResponse' {httpStatus} -> httpStatus) (\s@SearchAddressBooksResponse' {} a -> s {httpStatus = a} :: SearchAddressBooksResponse)

instance Prelude.NFData SearchAddressBooksResponse where
  rnf SearchAddressBooksResponse' {..} =
    Prelude.rnf addressBooks `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf totalCount `Prelude.seq`
          Prelude.rnf httpStatus
