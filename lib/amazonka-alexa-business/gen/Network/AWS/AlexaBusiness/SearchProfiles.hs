{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.SearchProfiles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches room profiles and lists the ones that meet a set of filter criteria.
--
-- This operation returns paginated results.
module Network.AWS.AlexaBusiness.SearchProfiles
  ( -- * Creating a request
    SearchProfiles (..),
    mkSearchProfiles,

    -- ** Request lenses
    spFilters,
    spSortCriteria,
    spNextToken,
    spMaxResults,

    -- * Destructuring the response
    SearchProfilesResponse (..),
    mkSearchProfilesResponse,

    -- ** Response lenses
    sprsProfiles,
    sprsNextToken,
    sprsTotalCount,
    sprsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSearchProfiles' smart constructor.
data SearchProfiles = SearchProfiles'
  { filters ::
      Lude.Maybe [Filter],
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

-- | Creates a value of 'SearchProfiles' with the minimum fields required to make a request.
--
-- * 'filters' - The filters to use to list a specified set of room profiles. Supported filter keys are ProfileName and Address. Required.
-- * 'maxResults' - The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
-- * 'nextToken' - An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
-- * 'sortCriteria' - The sort order to use in listing the specified set of room profiles. Supported sort keys are ProfileName and Address.
mkSearchProfiles ::
  SearchProfiles
mkSearchProfiles =
  SearchProfiles'
    { filters = Lude.Nothing,
      sortCriteria = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The filters to use to list a specified set of room profiles. Supported filter keys are ProfileName and Address. Required.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spFilters :: Lens.Lens' SearchProfiles (Lude.Maybe [Filter])
spFilters = Lens.lens (filters :: SearchProfiles -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: SearchProfiles)
{-# DEPRECATED spFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The sort order to use in listing the specified set of room profiles. Supported sort keys are ProfileName and Address.
--
-- /Note:/ Consider using 'sortCriteria' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spSortCriteria :: Lens.Lens' SearchProfiles (Lude.Maybe [Sort])
spSortCriteria = Lens.lens (sortCriteria :: SearchProfiles -> Lude.Maybe [Sort]) (\s a -> s {sortCriteria = a} :: SearchProfiles)
{-# DEPRECATED spSortCriteria "Use generic-lens or generic-optics with 'sortCriteria' instead." #-}

-- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spNextToken :: Lens.Lens' SearchProfiles (Lude.Maybe Lude.Text)
spNextToken = Lens.lens (nextToken :: SearchProfiles -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: SearchProfiles)
{-# DEPRECATED spNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spMaxResults :: Lens.Lens' SearchProfiles (Lude.Maybe Lude.Natural)
spMaxResults = Lens.lens (maxResults :: SearchProfiles -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: SearchProfiles)
{-# DEPRECATED spMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager SearchProfiles where
  page rq rs
    | Page.stop (rs Lens.^. sprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. sprsProfiles) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& spNextToken Lens..~ rs Lens.^. sprsNextToken

instance Lude.AWSRequest SearchProfiles where
  type Rs SearchProfiles = SearchProfilesResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveJSON
      ( \s h x ->
          SearchProfilesResponse'
            Lude.<$> (x Lude..?> "Profiles" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "TotalCount")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SearchProfiles where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.SearchProfiles" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SearchProfiles where
  toJSON SearchProfiles' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            ("SortCriteria" Lude..=) Lude.<$> sortCriteria,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath SearchProfiles where
  toPath = Lude.const "/"

instance Lude.ToQuery SearchProfiles where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSearchProfilesResponse' smart constructor.
data SearchProfilesResponse = SearchProfilesResponse'
  { profiles ::
      Lude.Maybe [ProfileData],
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

-- | Creates a value of 'SearchProfilesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token returned to indicate that there is more data available.
-- * 'profiles' - The profiles that meet the specified set of filter criteria, in sort order.
-- * 'responseStatus' - The response status code.
-- * 'totalCount' - The total number of room profiles returned.
mkSearchProfilesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SearchProfilesResponse
mkSearchProfilesResponse pResponseStatus_ =
  SearchProfilesResponse'
    { profiles = Lude.Nothing,
      nextToken = Lude.Nothing,
      totalCount = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The profiles that meet the specified set of filter criteria, in sort order.
--
-- /Note:/ Consider using 'profiles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sprsProfiles :: Lens.Lens' SearchProfilesResponse (Lude.Maybe [ProfileData])
sprsProfiles = Lens.lens (profiles :: SearchProfilesResponse -> Lude.Maybe [ProfileData]) (\s a -> s {profiles = a} :: SearchProfilesResponse)
{-# DEPRECATED sprsProfiles "Use generic-lens or generic-optics with 'profiles' instead." #-}

-- | The token returned to indicate that there is more data available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sprsNextToken :: Lens.Lens' SearchProfilesResponse (Lude.Maybe Lude.Text)
sprsNextToken = Lens.lens (nextToken :: SearchProfilesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: SearchProfilesResponse)
{-# DEPRECATED sprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The total number of room profiles returned.
--
-- /Note:/ Consider using 'totalCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sprsTotalCount :: Lens.Lens' SearchProfilesResponse (Lude.Maybe Lude.Int)
sprsTotalCount = Lens.lens (totalCount :: SearchProfilesResponse -> Lude.Maybe Lude.Int) (\s a -> s {totalCount = a} :: SearchProfilesResponse)
{-# DEPRECATED sprsTotalCount "Use generic-lens or generic-optics with 'totalCount' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sprsResponseStatus :: Lens.Lens' SearchProfilesResponse Lude.Int
sprsResponseStatus = Lens.lens (responseStatus :: SearchProfilesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SearchProfilesResponse)
{-# DEPRECATED sprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
