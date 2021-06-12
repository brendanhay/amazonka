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
-- Module      : Network.AWS.AlexaBusiness.SearchAddressBooks
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches address books and lists the ones that meet a set of filter and
-- sort criteria.
module Network.AWS.AlexaBusiness.SearchAddressBooks
  ( -- * Creating a Request
    SearchAddressBooks (..),
    newSearchAddressBooks,

    -- * Request Lenses
    searchAddressBooks_nextToken,
    searchAddressBooks_sortCriteria,
    searchAddressBooks_maxResults,
    searchAddressBooks_filters,

    -- * Destructuring the Response
    SearchAddressBooksResponse (..),
    newSearchAddressBooksResponse,

    -- * Response Lenses
    searchAddressBooksResponse_nextToken,
    searchAddressBooksResponse_addressBooks,
    searchAddressBooksResponse_totalCount,
    searchAddressBooksResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSearchAddressBooks' smart constructor.
data SearchAddressBooks = SearchAddressBooks'
  { -- | An optional token returned from a prior request. Use this token for
    -- pagination of results from this action. If this parameter is specified,
    -- the response only includes results beyond the token, up to the value
    -- specified by MaxResults.
    nextToken :: Core.Maybe Core.Text,
    -- | The sort order to use in listing the specified set of address books. The
    -- supported sort key is AddressBookName.
    sortCriteria :: Core.Maybe [Sort],
    -- | The maximum number of results to include in the response. If more
    -- results exist than the specified MaxResults value, a token is included
    -- in the response so that the remaining results can be retrieved.
    maxResults :: Core.Maybe Core.Natural,
    -- | The filters to use to list a specified set of address books. The
    -- supported filter key is AddressBookName.
    filters :: Core.Maybe [Filter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SearchAddressBooks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchAddressBooks_nextToken' - An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response only includes results beyond the token, up to the value
-- specified by MaxResults.
--
-- 'sortCriteria', 'searchAddressBooks_sortCriteria' - The sort order to use in listing the specified set of address books. The
-- supported sort key is AddressBookName.
--
-- 'maxResults', 'searchAddressBooks_maxResults' - The maximum number of results to include in the response. If more
-- results exist than the specified MaxResults value, a token is included
-- in the response so that the remaining results can be retrieved.
--
-- 'filters', 'searchAddressBooks_filters' - The filters to use to list a specified set of address books. The
-- supported filter key is AddressBookName.
newSearchAddressBooks ::
  SearchAddressBooks
newSearchAddressBooks =
  SearchAddressBooks'
    { nextToken = Core.Nothing,
      sortCriteria = Core.Nothing,
      maxResults = Core.Nothing,
      filters = Core.Nothing
    }

-- | An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response only includes results beyond the token, up to the value
-- specified by MaxResults.
searchAddressBooks_nextToken :: Lens.Lens' SearchAddressBooks (Core.Maybe Core.Text)
searchAddressBooks_nextToken = Lens.lens (\SearchAddressBooks' {nextToken} -> nextToken) (\s@SearchAddressBooks' {} a -> s {nextToken = a} :: SearchAddressBooks)

-- | The sort order to use in listing the specified set of address books. The
-- supported sort key is AddressBookName.
searchAddressBooks_sortCriteria :: Lens.Lens' SearchAddressBooks (Core.Maybe [Sort])
searchAddressBooks_sortCriteria = Lens.lens (\SearchAddressBooks' {sortCriteria} -> sortCriteria) (\s@SearchAddressBooks' {} a -> s {sortCriteria = a} :: SearchAddressBooks) Core.. Lens.mapping Lens._Coerce

-- | The maximum number of results to include in the response. If more
-- results exist than the specified MaxResults value, a token is included
-- in the response so that the remaining results can be retrieved.
searchAddressBooks_maxResults :: Lens.Lens' SearchAddressBooks (Core.Maybe Core.Natural)
searchAddressBooks_maxResults = Lens.lens (\SearchAddressBooks' {maxResults} -> maxResults) (\s@SearchAddressBooks' {} a -> s {maxResults = a} :: SearchAddressBooks)

-- | The filters to use to list a specified set of address books. The
-- supported filter key is AddressBookName.
searchAddressBooks_filters :: Lens.Lens' SearchAddressBooks (Core.Maybe [Filter])
searchAddressBooks_filters = Lens.lens (\SearchAddressBooks' {filters} -> filters) (\s@SearchAddressBooks' {} a -> s {filters = a} :: SearchAddressBooks) Core.. Lens.mapping Lens._Coerce

instance Core.AWSRequest SearchAddressBooks where
  type
    AWSResponse SearchAddressBooks =
      SearchAddressBooksResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchAddressBooksResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "AddressBooks" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "TotalCount")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable SearchAddressBooks

instance Core.NFData SearchAddressBooks

instance Core.ToHeaders SearchAddressBooks where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.SearchAddressBooks" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON SearchAddressBooks where
  toJSON SearchAddressBooks' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("SortCriteria" Core..=) Core.<$> sortCriteria,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("Filters" Core..=) Core.<$> filters
          ]
      )

instance Core.ToPath SearchAddressBooks where
  toPath = Core.const "/"

instance Core.ToQuery SearchAddressBooks where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newSearchAddressBooksResponse' smart constructor.
data SearchAddressBooksResponse = SearchAddressBooksResponse'
  { -- | The token returned to indicate that there is more data available.
    nextToken :: Core.Maybe Core.Text,
    -- | The address books that meet the specified set of filter criteria, in
    -- sort order.
    addressBooks :: Core.Maybe [AddressBookData],
    -- | The total number of address books returned.
    totalCount :: Core.Maybe Core.Int,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SearchAddressBooksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchAddressBooksResponse_nextToken' - The token returned to indicate that there is more data available.
--
-- 'addressBooks', 'searchAddressBooksResponse_addressBooks' - The address books that meet the specified set of filter criteria, in
-- sort order.
--
-- 'totalCount', 'searchAddressBooksResponse_totalCount' - The total number of address books returned.
--
-- 'httpStatus', 'searchAddressBooksResponse_httpStatus' - The response's http status code.
newSearchAddressBooksResponse ::
  -- | 'httpStatus'
  Core.Int ->
  SearchAddressBooksResponse
newSearchAddressBooksResponse pHttpStatus_ =
  SearchAddressBooksResponse'
    { nextToken =
        Core.Nothing,
      addressBooks = Core.Nothing,
      totalCount = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token returned to indicate that there is more data available.
searchAddressBooksResponse_nextToken :: Lens.Lens' SearchAddressBooksResponse (Core.Maybe Core.Text)
searchAddressBooksResponse_nextToken = Lens.lens (\SearchAddressBooksResponse' {nextToken} -> nextToken) (\s@SearchAddressBooksResponse' {} a -> s {nextToken = a} :: SearchAddressBooksResponse)

-- | The address books that meet the specified set of filter criteria, in
-- sort order.
searchAddressBooksResponse_addressBooks :: Lens.Lens' SearchAddressBooksResponse (Core.Maybe [AddressBookData])
searchAddressBooksResponse_addressBooks = Lens.lens (\SearchAddressBooksResponse' {addressBooks} -> addressBooks) (\s@SearchAddressBooksResponse' {} a -> s {addressBooks = a} :: SearchAddressBooksResponse) Core.. Lens.mapping Lens._Coerce

-- | The total number of address books returned.
searchAddressBooksResponse_totalCount :: Lens.Lens' SearchAddressBooksResponse (Core.Maybe Core.Int)
searchAddressBooksResponse_totalCount = Lens.lens (\SearchAddressBooksResponse' {totalCount} -> totalCount) (\s@SearchAddressBooksResponse' {} a -> s {totalCount = a} :: SearchAddressBooksResponse)

-- | The response's http status code.
searchAddressBooksResponse_httpStatus :: Lens.Lens' SearchAddressBooksResponse Core.Int
searchAddressBooksResponse_httpStatus = Lens.lens (\SearchAddressBooksResponse' {httpStatus} -> httpStatus) (\s@SearchAddressBooksResponse' {} a -> s {httpStatus = a} :: SearchAddressBooksResponse)

instance Core.NFData SearchAddressBooksResponse
