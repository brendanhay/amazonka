{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.SearchContacts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches contacts and lists the ones that meet a set of filter and sort criteria.
module Network.AWS.AlexaBusiness.SearchContacts
  ( -- * Creating a request
    SearchContacts (..),
    mkSearchContacts,

    -- ** Request lenses
    scFilters,
    scSortCriteria,
    scNextToken,
    scMaxResults,

    -- * Destructuring the response
    SearchContactsResponse (..),
    mkSearchContactsResponse,

    -- ** Response lenses
    scrsNextToken,
    scrsContacts,
    scrsTotalCount,
    scrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSearchContacts' smart constructor.
data SearchContacts = SearchContacts'
  { -- | The filters to use to list a specified set of address books. The supported filter keys are DisplayName, FirstName, LastName, and AddressBookArns.
    filters :: Lude.Maybe [Filter],
    -- | The sort order to use in listing the specified set of contacts. The supported sort keys are DisplayName, FirstName, and LastName.
    sortCriteria :: Lude.Maybe [Sort],
    -- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response only includes results beyond the token, up to the value specified by MaxResults.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to include in the response. If more results exist than the specified MaxResults value, a token is included in the response so that the remaining results can be retrieved.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SearchContacts' with the minimum fields required to make a request.
--
-- * 'filters' - The filters to use to list a specified set of address books. The supported filter keys are DisplayName, FirstName, LastName, and AddressBookArns.
-- * 'sortCriteria' - The sort order to use in listing the specified set of contacts. The supported sort keys are DisplayName, FirstName, and LastName.
-- * 'nextToken' - An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response only includes results beyond the token, up to the value specified by MaxResults.
-- * 'maxResults' - The maximum number of results to include in the response. If more results exist than the specified MaxResults value, a token is included in the response so that the remaining results can be retrieved.
mkSearchContacts ::
  SearchContacts
mkSearchContacts =
  SearchContacts'
    { filters = Lude.Nothing,
      sortCriteria = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The filters to use to list a specified set of address books. The supported filter keys are DisplayName, FirstName, LastName, and AddressBookArns.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scFilters :: Lens.Lens' SearchContacts (Lude.Maybe [Filter])
scFilters = Lens.lens (filters :: SearchContacts -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: SearchContacts)
{-# DEPRECATED scFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The sort order to use in listing the specified set of contacts. The supported sort keys are DisplayName, FirstName, and LastName.
--
-- /Note:/ Consider using 'sortCriteria' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scSortCriteria :: Lens.Lens' SearchContacts (Lude.Maybe [Sort])
scSortCriteria = Lens.lens (sortCriteria :: SearchContacts -> Lude.Maybe [Sort]) (\s a -> s {sortCriteria = a} :: SearchContacts)
{-# DEPRECATED scSortCriteria "Use generic-lens or generic-optics with 'sortCriteria' instead." #-}

-- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response only includes results beyond the token, up to the value specified by MaxResults.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scNextToken :: Lens.Lens' SearchContacts (Lude.Maybe Lude.Text)
scNextToken = Lens.lens (nextToken :: SearchContacts -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: SearchContacts)
{-# DEPRECATED scNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to include in the response. If more results exist than the specified MaxResults value, a token is included in the response so that the remaining results can be retrieved.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scMaxResults :: Lens.Lens' SearchContacts (Lude.Maybe Lude.Natural)
scMaxResults = Lens.lens (maxResults :: SearchContacts -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: SearchContacts)
{-# DEPRECATED scMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest SearchContacts where
  type Rs SearchContacts = SearchContactsResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveJSON
      ( \s h x ->
          SearchContactsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Contacts" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "TotalCount")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SearchContacts where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.SearchContacts" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SearchContacts where
  toJSON SearchContacts' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            ("SortCriteria" Lude..=) Lude.<$> sortCriteria,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath SearchContacts where
  toPath = Lude.const "/"

instance Lude.ToQuery SearchContacts where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSearchContactsResponse' smart constructor.
data SearchContactsResponse = SearchContactsResponse'
  { -- | The token returned to indicate that there is more data available.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The contacts that meet the specified set of filter criteria, in sort order.
    contacts :: Lude.Maybe [ContactData],
    -- | The total number of contacts returned.
    totalCount :: Lude.Maybe Lude.Int,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SearchContactsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token returned to indicate that there is more data available.
-- * 'contacts' - The contacts that meet the specified set of filter criteria, in sort order.
-- * 'totalCount' - The total number of contacts returned.
-- * 'responseStatus' - The response status code.
mkSearchContactsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SearchContactsResponse
mkSearchContactsResponse pResponseStatus_ =
  SearchContactsResponse'
    { nextToken = Lude.Nothing,
      contacts = Lude.Nothing,
      totalCount = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token returned to indicate that there is more data available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrsNextToken :: Lens.Lens' SearchContactsResponse (Lude.Maybe Lude.Text)
scrsNextToken = Lens.lens (nextToken :: SearchContactsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: SearchContactsResponse)
{-# DEPRECATED scrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The contacts that meet the specified set of filter criteria, in sort order.
--
-- /Note:/ Consider using 'contacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrsContacts :: Lens.Lens' SearchContactsResponse (Lude.Maybe [ContactData])
scrsContacts = Lens.lens (contacts :: SearchContactsResponse -> Lude.Maybe [ContactData]) (\s a -> s {contacts = a} :: SearchContactsResponse)
{-# DEPRECATED scrsContacts "Use generic-lens or generic-optics with 'contacts' instead." #-}

-- | The total number of contacts returned.
--
-- /Note:/ Consider using 'totalCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrsTotalCount :: Lens.Lens' SearchContactsResponse (Lude.Maybe Lude.Int)
scrsTotalCount = Lens.lens (totalCount :: SearchContactsResponse -> Lude.Maybe Lude.Int) (\s a -> s {totalCount = a} :: SearchContactsResponse)
{-# DEPRECATED scrsTotalCount "Use generic-lens or generic-optics with 'totalCount' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scrsResponseStatus :: Lens.Lens' SearchContactsResponse Lude.Int
scrsResponseStatus = Lens.lens (responseStatus :: SearchContactsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SearchContactsResponse)
{-# DEPRECATED scrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
