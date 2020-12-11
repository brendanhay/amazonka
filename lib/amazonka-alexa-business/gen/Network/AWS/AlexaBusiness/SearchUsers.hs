{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.SearchUsers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches users and lists the ones that meet a set of filter and sort criteria.
--
-- This operation returns paginated results.
module Network.AWS.AlexaBusiness.SearchUsers
  ( -- * Creating a request
    SearchUsers (..),
    mkSearchUsers,

    -- ** Request lenses
    suFilters,
    suSortCriteria,
    suNextToken,
    suMaxResults,

    -- * Destructuring the response
    SearchUsersResponse (..),
    mkSearchUsersResponse,

    -- ** Response lenses
    sursUsers,
    sursNextToken,
    sursTotalCount,
    sursResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSearchUsers' smart constructor.
data SearchUsers = SearchUsers'
  { filters :: Lude.Maybe [Filter],
    sortCriteria :: Lude.Maybe [Sort],
    nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SearchUsers' with the minimum fields required to make a request.
--
-- * 'filters' - The filters to use for listing a specific set of users. Required. Supported filter keys are UserId, FirstName, LastName, Email, and EnrollmentStatus.
-- * 'maxResults' - The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved. Required.
-- * 'nextToken' - An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ . Required.
-- * 'sortCriteria' - The sort order to use in listing the filtered set of users. Required. Supported sort keys are UserId, FirstName, LastName, Email, and EnrollmentStatus.
mkSearchUsers ::
  SearchUsers
mkSearchUsers =
  SearchUsers'
    { filters = Lude.Nothing,
      sortCriteria = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The filters to use for listing a specific set of users. Required. Supported filter keys are UserId, FirstName, LastName, Email, and EnrollmentStatus.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suFilters :: Lens.Lens' SearchUsers (Lude.Maybe [Filter])
suFilters = Lens.lens (filters :: SearchUsers -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: SearchUsers)
{-# DEPRECATED suFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The sort order to use in listing the filtered set of users. Required. Supported sort keys are UserId, FirstName, LastName, Email, and EnrollmentStatus.
--
-- /Note:/ Consider using 'sortCriteria' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suSortCriteria :: Lens.Lens' SearchUsers (Lude.Maybe [Sort])
suSortCriteria = Lens.lens (sortCriteria :: SearchUsers -> Lude.Maybe [Sort]) (\s a -> s {sortCriteria = a} :: SearchUsers)
{-# DEPRECATED suSortCriteria "Use generic-lens or generic-optics with 'sortCriteria' instead." #-}

-- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ . Required.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suNextToken :: Lens.Lens' SearchUsers (Lude.Maybe Lude.Text)
suNextToken = Lens.lens (nextToken :: SearchUsers -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: SearchUsers)
{-# DEPRECATED suNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved. Required.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
suMaxResults :: Lens.Lens' SearchUsers (Lude.Maybe Lude.Natural)
suMaxResults = Lens.lens (maxResults :: SearchUsers -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: SearchUsers)
{-# DEPRECATED suMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager SearchUsers where
  page rq rs
    | Page.stop (rs Lens.^. sursNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. sursUsers) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& suNextToken Lens..~ rs Lens.^. sursNextToken

instance Lude.AWSRequest SearchUsers where
  type Rs SearchUsers = SearchUsersResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveJSON
      ( \s h x ->
          SearchUsersResponse'
            Lude.<$> (x Lude..?> "Users" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "TotalCount")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SearchUsers where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.SearchUsers" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SearchUsers where
  toJSON SearchUsers' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            ("SortCriteria" Lude..=) Lude.<$> sortCriteria,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath SearchUsers where
  toPath = Lude.const "/"

instance Lude.ToQuery SearchUsers where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSearchUsersResponse' smart constructor.
data SearchUsersResponse = SearchUsersResponse'
  { users ::
      Lude.Maybe [UserData],
    nextToken :: Lude.Maybe Lude.Text,
    totalCount :: Lude.Maybe Lude.Int,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SearchUsersResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token returned to indicate that there is more data available.
-- * 'responseStatus' - The response status code.
-- * 'totalCount' - The total number of users returned.
-- * 'users' - The users that meet the specified set of filter criteria, in sort order.
mkSearchUsersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SearchUsersResponse
mkSearchUsersResponse pResponseStatus_ =
  SearchUsersResponse'
    { users = Lude.Nothing,
      nextToken = Lude.Nothing,
      totalCount = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The users that meet the specified set of filter criteria, in sort order.
--
-- /Note:/ Consider using 'users' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sursUsers :: Lens.Lens' SearchUsersResponse (Lude.Maybe [UserData])
sursUsers = Lens.lens (users :: SearchUsersResponse -> Lude.Maybe [UserData]) (\s a -> s {users = a} :: SearchUsersResponse)
{-# DEPRECATED sursUsers "Use generic-lens or generic-optics with 'users' instead." #-}

-- | The token returned to indicate that there is more data available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sursNextToken :: Lens.Lens' SearchUsersResponse (Lude.Maybe Lude.Text)
sursNextToken = Lens.lens (nextToken :: SearchUsersResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: SearchUsersResponse)
{-# DEPRECATED sursNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The total number of users returned.
--
-- /Note:/ Consider using 'totalCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sursTotalCount :: Lens.Lens' SearchUsersResponse (Lude.Maybe Lude.Int)
sursTotalCount = Lens.lens (totalCount :: SearchUsersResponse -> Lude.Maybe Lude.Int) (\s a -> s {totalCount = a} :: SearchUsersResponse)
{-# DEPRECATED sursTotalCount "Use generic-lens or generic-optics with 'totalCount' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sursResponseStatus :: Lens.Lens' SearchUsersResponse Lude.Int
sursResponseStatus = Lens.lens (responseStatus :: SearchUsersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SearchUsersResponse)
{-# DEPRECATED sursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
