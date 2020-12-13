{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.SearchSkillGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches skill groups and lists the ones that meet a set of filter and sort criteria.
--
-- This operation returns paginated results.
module Network.AWS.AlexaBusiness.SearchSkillGroups
  ( -- * Creating a request
    SearchSkillGroups (..),
    mkSearchSkillGroups,

    -- ** Request lenses
    ssgFilters,
    ssgSortCriteria,
    ssgNextToken,
    ssgMaxResults,

    -- * Destructuring the response
    SearchSkillGroupsResponse (..),
    mkSearchSkillGroupsResponse,

    -- ** Response lenses
    ssgrsNextToken,
    ssgrsSkillGroups,
    ssgrsTotalCount,
    ssgrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSearchSkillGroups' smart constructor.
data SearchSkillGroups = SearchSkillGroups'
  { -- | The filters to use to list a specified set of skill groups. The supported filter key is SkillGroupName.
    filters :: Lude.Maybe [Filter],
    -- | The sort order to use in listing the specified set of skill groups. The supported sort key is SkillGroupName.
    sortCriteria :: Lude.Maybe [Sort],
    -- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ . Required.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SearchSkillGroups' with the minimum fields required to make a request.
--
-- * 'filters' - The filters to use to list a specified set of skill groups. The supported filter key is SkillGroupName.
-- * 'sortCriteria' - The sort order to use in listing the specified set of skill groups. The supported sort key is SkillGroupName.
-- * 'nextToken' - An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ . Required.
-- * 'maxResults' - The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
mkSearchSkillGroups ::
  SearchSkillGroups
mkSearchSkillGroups =
  SearchSkillGroups'
    { filters = Lude.Nothing,
      sortCriteria = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The filters to use to list a specified set of skill groups. The supported filter key is SkillGroupName.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssgFilters :: Lens.Lens' SearchSkillGroups (Lude.Maybe [Filter])
ssgFilters = Lens.lens (filters :: SearchSkillGroups -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: SearchSkillGroups)
{-# DEPRECATED ssgFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The sort order to use in listing the specified set of skill groups. The supported sort key is SkillGroupName.
--
-- /Note:/ Consider using 'sortCriteria' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssgSortCriteria :: Lens.Lens' SearchSkillGroups (Lude.Maybe [Sort])
ssgSortCriteria = Lens.lens (sortCriteria :: SearchSkillGroups -> Lude.Maybe [Sort]) (\s a -> s {sortCriteria = a} :: SearchSkillGroups)
{-# DEPRECATED ssgSortCriteria "Use generic-lens or generic-optics with 'sortCriteria' instead." #-}

-- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ . Required.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssgNextToken :: Lens.Lens' SearchSkillGroups (Lude.Maybe Lude.Text)
ssgNextToken = Lens.lens (nextToken :: SearchSkillGroups -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: SearchSkillGroups)
{-# DEPRECATED ssgNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssgMaxResults :: Lens.Lens' SearchSkillGroups (Lude.Maybe Lude.Natural)
ssgMaxResults = Lens.lens (maxResults :: SearchSkillGroups -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: SearchSkillGroups)
{-# DEPRECATED ssgMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager SearchSkillGroups where
  page rq rs
    | Page.stop (rs Lens.^. ssgrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ssgrsSkillGroups) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ssgNextToken Lens..~ rs Lens.^. ssgrsNextToken

instance Lude.AWSRequest SearchSkillGroups where
  type Rs SearchSkillGroups = SearchSkillGroupsResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveJSON
      ( \s h x ->
          SearchSkillGroupsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "SkillGroups" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "TotalCount")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SearchSkillGroups where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.SearchSkillGroups" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SearchSkillGroups where
  toJSON SearchSkillGroups' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            ("SortCriteria" Lude..=) Lude.<$> sortCriteria,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath SearchSkillGroups where
  toPath = Lude.const "/"

instance Lude.ToQuery SearchSkillGroups where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSearchSkillGroupsResponse' smart constructor.
data SearchSkillGroupsResponse = SearchSkillGroupsResponse'
  { -- | The token returned to indicate that there is more data available.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The skill groups that meet the filter criteria, in sort order.
    skillGroups :: Lude.Maybe [SkillGroupData],
    -- | The total number of skill groups returned.
    totalCount :: Lude.Maybe Lude.Int,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SearchSkillGroupsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token returned to indicate that there is more data available.
-- * 'skillGroups' - The skill groups that meet the filter criteria, in sort order.
-- * 'totalCount' - The total number of skill groups returned.
-- * 'responseStatus' - The response status code.
mkSearchSkillGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SearchSkillGroupsResponse
mkSearchSkillGroupsResponse pResponseStatus_ =
  SearchSkillGroupsResponse'
    { nextToken = Lude.Nothing,
      skillGroups = Lude.Nothing,
      totalCount = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token returned to indicate that there is more data available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssgrsNextToken :: Lens.Lens' SearchSkillGroupsResponse (Lude.Maybe Lude.Text)
ssgrsNextToken = Lens.lens (nextToken :: SearchSkillGroupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: SearchSkillGroupsResponse)
{-# DEPRECATED ssgrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The skill groups that meet the filter criteria, in sort order.
--
-- /Note:/ Consider using 'skillGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssgrsSkillGroups :: Lens.Lens' SearchSkillGroupsResponse (Lude.Maybe [SkillGroupData])
ssgrsSkillGroups = Lens.lens (skillGroups :: SearchSkillGroupsResponse -> Lude.Maybe [SkillGroupData]) (\s a -> s {skillGroups = a} :: SearchSkillGroupsResponse)
{-# DEPRECATED ssgrsSkillGroups "Use generic-lens or generic-optics with 'skillGroups' instead." #-}

-- | The total number of skill groups returned.
--
-- /Note:/ Consider using 'totalCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssgrsTotalCount :: Lens.Lens' SearchSkillGroupsResponse (Lude.Maybe Lude.Int)
ssgrsTotalCount = Lens.lens (totalCount :: SearchSkillGroupsResponse -> Lude.Maybe Lude.Int) (\s a -> s {totalCount = a} :: SearchSkillGroupsResponse)
{-# DEPRECATED ssgrsTotalCount "Use generic-lens or generic-optics with 'totalCount' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssgrsResponseStatus :: Lens.Lens' SearchSkillGroupsResponse Lude.Int
ssgrsResponseStatus = Lens.lens (responseStatus :: SearchSkillGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SearchSkillGroupsResponse)
{-# DEPRECATED ssgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
