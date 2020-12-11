{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.SearchNetworkProfiles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches network profiles and lists the ones that meet a set of filter and sort criteria.
module Network.AWS.AlexaBusiness.SearchNetworkProfiles
  ( -- * Creating a request
    SearchNetworkProfiles (..),
    mkSearchNetworkProfiles,

    -- ** Request lenses
    snpFilters,
    snpSortCriteria,
    snpNextToken,
    snpMaxResults,

    -- * Destructuring the response
    SearchNetworkProfilesResponse (..),
    mkSearchNetworkProfilesResponse,

    -- ** Response lenses
    snprsNetworkProfiles,
    snprsNextToken,
    snprsTotalCount,
    snprsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSearchNetworkProfiles' smart constructor.
data SearchNetworkProfiles = SearchNetworkProfiles'
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

-- | Creates a value of 'SearchNetworkProfiles' with the minimum fields required to make a request.
--
-- * 'filters' - The filters to use to list a specified set of network profiles. Valid filters are NetworkProfileName, Ssid, and SecurityType.
-- * 'maxResults' - The maximum number of results to include in the response. If more results exist than the specified MaxResults value, a token is included in the response so that the remaining results can be retrieved.
-- * 'nextToken' - An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by MaxResults.
-- * 'sortCriteria' - The sort order to use to list the specified set of network profiles. Valid sort criteria includes NetworkProfileName, Ssid, and SecurityType.
mkSearchNetworkProfiles ::
  SearchNetworkProfiles
mkSearchNetworkProfiles =
  SearchNetworkProfiles'
    { filters = Lude.Nothing,
      sortCriteria = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The filters to use to list a specified set of network profiles. Valid filters are NetworkProfileName, Ssid, and SecurityType.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
snpFilters :: Lens.Lens' SearchNetworkProfiles (Lude.Maybe [Filter])
snpFilters = Lens.lens (filters :: SearchNetworkProfiles -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: SearchNetworkProfiles)
{-# DEPRECATED snpFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The sort order to use to list the specified set of network profiles. Valid sort criteria includes NetworkProfileName, Ssid, and SecurityType.
--
-- /Note:/ Consider using 'sortCriteria' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
snpSortCriteria :: Lens.Lens' SearchNetworkProfiles (Lude.Maybe [Sort])
snpSortCriteria = Lens.lens (sortCriteria :: SearchNetworkProfiles -> Lude.Maybe [Sort]) (\s a -> s {sortCriteria = a} :: SearchNetworkProfiles)
{-# DEPRECATED snpSortCriteria "Use generic-lens or generic-optics with 'sortCriteria' instead." #-}

-- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by MaxResults.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
snpNextToken :: Lens.Lens' SearchNetworkProfiles (Lude.Maybe Lude.Text)
snpNextToken = Lens.lens (nextToken :: SearchNetworkProfiles -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: SearchNetworkProfiles)
{-# DEPRECATED snpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to include in the response. If more results exist than the specified MaxResults value, a token is included in the response so that the remaining results can be retrieved.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
snpMaxResults :: Lens.Lens' SearchNetworkProfiles (Lude.Maybe Lude.Natural)
snpMaxResults = Lens.lens (maxResults :: SearchNetworkProfiles -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: SearchNetworkProfiles)
{-# DEPRECATED snpMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest SearchNetworkProfiles where
  type Rs SearchNetworkProfiles = SearchNetworkProfilesResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveJSON
      ( \s h x ->
          SearchNetworkProfilesResponse'
            Lude.<$> (x Lude..?> "NetworkProfiles" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "TotalCount")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SearchNetworkProfiles where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.SearchNetworkProfiles" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SearchNetworkProfiles where
  toJSON SearchNetworkProfiles' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            ("SortCriteria" Lude..=) Lude.<$> sortCriteria,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath SearchNetworkProfiles where
  toPath = Lude.const "/"

instance Lude.ToQuery SearchNetworkProfiles where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSearchNetworkProfilesResponse' smart constructor.
data SearchNetworkProfilesResponse = SearchNetworkProfilesResponse'
  { networkProfiles ::
      Lude.Maybe [NetworkProfileData],
    nextToken ::
      Lude.Maybe Lude.Text,
    totalCount ::
      Lude.Maybe Lude.Int,
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

-- | Creates a value of 'SearchNetworkProfilesResponse' with the minimum fields required to make a request.
--
-- * 'networkProfiles' - The network profiles that meet the specified set of filter criteria, in sort order. It is a list of NetworkProfileData objects.
-- * 'nextToken' - An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by MaxResults.
-- * 'responseStatus' - The response status code.
-- * 'totalCount' - The total number of network profiles returned.
mkSearchNetworkProfilesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SearchNetworkProfilesResponse
mkSearchNetworkProfilesResponse pResponseStatus_ =
  SearchNetworkProfilesResponse'
    { networkProfiles = Lude.Nothing,
      nextToken = Lude.Nothing,
      totalCount = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The network profiles that meet the specified set of filter criteria, in sort order. It is a list of NetworkProfileData objects.
--
-- /Note:/ Consider using 'networkProfiles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
snprsNetworkProfiles :: Lens.Lens' SearchNetworkProfilesResponse (Lude.Maybe [NetworkProfileData])
snprsNetworkProfiles = Lens.lens (networkProfiles :: SearchNetworkProfilesResponse -> Lude.Maybe [NetworkProfileData]) (\s a -> s {networkProfiles = a} :: SearchNetworkProfilesResponse)
{-# DEPRECATED snprsNetworkProfiles "Use generic-lens or generic-optics with 'networkProfiles' instead." #-}

-- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by MaxResults.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
snprsNextToken :: Lens.Lens' SearchNetworkProfilesResponse (Lude.Maybe Lude.Text)
snprsNextToken = Lens.lens (nextToken :: SearchNetworkProfilesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: SearchNetworkProfilesResponse)
{-# DEPRECATED snprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The total number of network profiles returned.
--
-- /Note:/ Consider using 'totalCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
snprsTotalCount :: Lens.Lens' SearchNetworkProfilesResponse (Lude.Maybe Lude.Int)
snprsTotalCount = Lens.lens (totalCount :: SearchNetworkProfilesResponse -> Lude.Maybe Lude.Int) (\s a -> s {totalCount = a} :: SearchNetworkProfilesResponse)
{-# DEPRECATED snprsTotalCount "Use generic-lens or generic-optics with 'totalCount' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
snprsResponseStatus :: Lens.Lens' SearchNetworkProfilesResponse Lude.Int
snprsResponseStatus = Lens.lens (responseStatus :: SearchNetworkProfilesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SearchNetworkProfilesResponse)
{-# DEPRECATED snprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
