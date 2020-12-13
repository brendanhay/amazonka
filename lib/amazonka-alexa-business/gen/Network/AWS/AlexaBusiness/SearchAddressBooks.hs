{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.SearchAddressBooks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches address books and lists the ones that meet a set of filter and sort criteria.
module Network.AWS.AlexaBusiness.SearchAddressBooks
  ( -- * Creating a request
    SearchAddressBooks (..),
    mkSearchAddressBooks,

    -- ** Request lenses
    sabFilters,
    sabSortCriteria,
    sabNextToken,
    sabMaxResults,

    -- * Destructuring the response
    SearchAddressBooksResponse (..),
    mkSearchAddressBooksResponse,

    -- ** Response lenses
    sabrsNextToken,
    sabrsAddressBooks,
    sabrsTotalCount,
    sabrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSearchAddressBooks' smart constructor.
data SearchAddressBooks = SearchAddressBooks'
  { -- | The filters to use to list a specified set of address books. The supported filter key is AddressBookName.
    filters :: Lude.Maybe [Filter],
    -- | The sort order to use in listing the specified set of address books. The supported sort key is AddressBookName.
    sortCriteria :: Lude.Maybe [Sort],
    -- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response only includes results beyond the token, up to the value specified by MaxResults.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to include in the response. If more results exist than the specified MaxResults value, a token is included in the response so that the remaining results can be retrieved.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SearchAddressBooks' with the minimum fields required to make a request.
--
-- * 'filters' - The filters to use to list a specified set of address books. The supported filter key is AddressBookName.
-- * 'sortCriteria' - The sort order to use in listing the specified set of address books. The supported sort key is AddressBookName.
-- * 'nextToken' - An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response only includes results beyond the token, up to the value specified by MaxResults.
-- * 'maxResults' - The maximum number of results to include in the response. If more results exist than the specified MaxResults value, a token is included in the response so that the remaining results can be retrieved.
mkSearchAddressBooks ::
  SearchAddressBooks
mkSearchAddressBooks =
  SearchAddressBooks'
    { filters = Lude.Nothing,
      sortCriteria = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The filters to use to list a specified set of address books. The supported filter key is AddressBookName.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sabFilters :: Lens.Lens' SearchAddressBooks (Lude.Maybe [Filter])
sabFilters = Lens.lens (filters :: SearchAddressBooks -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: SearchAddressBooks)
{-# DEPRECATED sabFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The sort order to use in listing the specified set of address books. The supported sort key is AddressBookName.
--
-- /Note:/ Consider using 'sortCriteria' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sabSortCriteria :: Lens.Lens' SearchAddressBooks (Lude.Maybe [Sort])
sabSortCriteria = Lens.lens (sortCriteria :: SearchAddressBooks -> Lude.Maybe [Sort]) (\s a -> s {sortCriteria = a} :: SearchAddressBooks)
{-# DEPRECATED sabSortCriteria "Use generic-lens or generic-optics with 'sortCriteria' instead." #-}

-- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response only includes results beyond the token, up to the value specified by MaxResults.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sabNextToken :: Lens.Lens' SearchAddressBooks (Lude.Maybe Lude.Text)
sabNextToken = Lens.lens (nextToken :: SearchAddressBooks -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: SearchAddressBooks)
{-# DEPRECATED sabNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to include in the response. If more results exist than the specified MaxResults value, a token is included in the response so that the remaining results can be retrieved.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sabMaxResults :: Lens.Lens' SearchAddressBooks (Lude.Maybe Lude.Natural)
sabMaxResults = Lens.lens (maxResults :: SearchAddressBooks -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: SearchAddressBooks)
{-# DEPRECATED sabMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest SearchAddressBooks where
  type Rs SearchAddressBooks = SearchAddressBooksResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveJSON
      ( \s h x ->
          SearchAddressBooksResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "AddressBooks" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "TotalCount")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SearchAddressBooks where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.SearchAddressBooks" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SearchAddressBooks where
  toJSON SearchAddressBooks' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            ("SortCriteria" Lude..=) Lude.<$> sortCriteria,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath SearchAddressBooks where
  toPath = Lude.const "/"

instance Lude.ToQuery SearchAddressBooks where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSearchAddressBooksResponse' smart constructor.
data SearchAddressBooksResponse = SearchAddressBooksResponse'
  { -- | The token returned to indicate that there is more data available.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The address books that meet the specified set of filter criteria, in sort order.
    addressBooks :: Lude.Maybe [AddressBookData],
    -- | The total number of address books returned.
    totalCount :: Lude.Maybe Lude.Int,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SearchAddressBooksResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token returned to indicate that there is more data available.
-- * 'addressBooks' - The address books that meet the specified set of filter criteria, in sort order.
-- * 'totalCount' - The total number of address books returned.
-- * 'responseStatus' - The response status code.
mkSearchAddressBooksResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SearchAddressBooksResponse
mkSearchAddressBooksResponse pResponseStatus_ =
  SearchAddressBooksResponse'
    { nextToken = Lude.Nothing,
      addressBooks = Lude.Nothing,
      totalCount = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token returned to indicate that there is more data available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sabrsNextToken :: Lens.Lens' SearchAddressBooksResponse (Lude.Maybe Lude.Text)
sabrsNextToken = Lens.lens (nextToken :: SearchAddressBooksResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: SearchAddressBooksResponse)
{-# DEPRECATED sabrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The address books that meet the specified set of filter criteria, in sort order.
--
-- /Note:/ Consider using 'addressBooks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sabrsAddressBooks :: Lens.Lens' SearchAddressBooksResponse (Lude.Maybe [AddressBookData])
sabrsAddressBooks = Lens.lens (addressBooks :: SearchAddressBooksResponse -> Lude.Maybe [AddressBookData]) (\s a -> s {addressBooks = a} :: SearchAddressBooksResponse)
{-# DEPRECATED sabrsAddressBooks "Use generic-lens or generic-optics with 'addressBooks' instead." #-}

-- | The total number of address books returned.
--
-- /Note:/ Consider using 'totalCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sabrsTotalCount :: Lens.Lens' SearchAddressBooksResponse (Lude.Maybe Lude.Int)
sabrsTotalCount = Lens.lens (totalCount :: SearchAddressBooksResponse -> Lude.Maybe Lude.Int) (\s a -> s {totalCount = a} :: SearchAddressBooksResponse)
{-# DEPRECATED sabrsTotalCount "Use generic-lens or generic-optics with 'totalCount' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sabrsResponseStatus :: Lens.Lens' SearchAddressBooksResponse Lude.Int
sabrsResponseStatus = Lens.lens (responseStatus :: SearchAddressBooksResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SearchAddressBooksResponse)
{-# DEPRECATED sabrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
