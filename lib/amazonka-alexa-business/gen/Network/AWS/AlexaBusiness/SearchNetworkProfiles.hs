{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    snpMaxResults,
    snpNextToken,
    snpSortCriteria,

    -- * Destructuring the response
    SearchNetworkProfilesResponse (..),
    mkSearchNetworkProfilesResponse,

    -- ** Response lenses
    snprrsNetworkProfiles,
    snprrsNextToken,
    snprrsTotalCount,
    snprrsResponseStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSearchNetworkProfiles' smart constructor.
data SearchNetworkProfiles = SearchNetworkProfiles'
  { -- | The filters to use to list a specified set of network profiles. Valid filters are NetworkProfileName, Ssid, and SecurityType.
    filters :: Core.Maybe [Types.Filter],
    -- | The maximum number of results to include in the response. If more results exist than the specified MaxResults value, a token is included in the response so that the remaining results can be retrieved.
    maxResults :: Core.Maybe Core.Natural,
    -- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by MaxResults.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The sort order to use to list the specified set of network profiles. Valid sort criteria includes NetworkProfileName, Ssid, and SecurityType.
    sortCriteria :: Core.Maybe [Types.Sort]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SearchNetworkProfiles' value with any optional fields omitted.
mkSearchNetworkProfiles ::
  SearchNetworkProfiles
mkSearchNetworkProfiles =
  SearchNetworkProfiles'
    { filters = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      sortCriteria = Core.Nothing
    }

-- | The filters to use to list a specified set of network profiles. Valid filters are NetworkProfileName, Ssid, and SecurityType.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
snpFilters :: Lens.Lens' SearchNetworkProfiles (Core.Maybe [Types.Filter])
snpFilters = Lens.field @"filters"
{-# DEPRECATED snpFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of results to include in the response. If more results exist than the specified MaxResults value, a token is included in the response so that the remaining results can be retrieved.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
snpMaxResults :: Lens.Lens' SearchNetworkProfiles (Core.Maybe Core.Natural)
snpMaxResults = Lens.field @"maxResults"
{-# DEPRECATED snpMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by MaxResults.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
snpNextToken :: Lens.Lens' SearchNetworkProfiles (Core.Maybe Types.NextToken)
snpNextToken = Lens.field @"nextToken"
{-# DEPRECATED snpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The sort order to use to list the specified set of network profiles. Valid sort criteria includes NetworkProfileName, Ssid, and SecurityType.
--
-- /Note:/ Consider using 'sortCriteria' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
snpSortCriteria :: Lens.Lens' SearchNetworkProfiles (Core.Maybe [Types.Sort])
snpSortCriteria = Lens.field @"sortCriteria"
{-# DEPRECATED snpSortCriteria "Use generic-lens or generic-optics with 'sortCriteria' instead." #-}

instance Core.FromJSON SearchNetworkProfiles where
  toJSON SearchNetworkProfiles {..} =
    Core.object
      ( Core.catMaybes
          [ ("Filters" Core..=) Core.<$> filters,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("SortCriteria" Core..=) Core.<$> sortCriteria
          ]
      )

instance Core.AWSRequest SearchNetworkProfiles where
  type Rs SearchNetworkProfiles = SearchNetworkProfilesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AlexaForBusiness.SearchNetworkProfiles")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchNetworkProfilesResponse'
            Core.<$> (x Core..:? "NetworkProfiles")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "TotalCount")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkSearchNetworkProfilesResponse' smart constructor.
data SearchNetworkProfilesResponse = SearchNetworkProfilesResponse'
  { -- | The network profiles that meet the specified set of filter criteria, in sort order. It is a list of NetworkProfileData objects.
    networkProfiles :: Core.Maybe [Types.NetworkProfileData],
    -- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by MaxResults.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The total number of network profiles returned.
    totalCount :: Core.Maybe Core.Int,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SearchNetworkProfilesResponse' value with any optional fields omitted.
mkSearchNetworkProfilesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  SearchNetworkProfilesResponse
mkSearchNetworkProfilesResponse responseStatus =
  SearchNetworkProfilesResponse'
    { networkProfiles = Core.Nothing,
      nextToken = Core.Nothing,
      totalCount = Core.Nothing,
      responseStatus
    }

-- | The network profiles that meet the specified set of filter criteria, in sort order. It is a list of NetworkProfileData objects.
--
-- /Note:/ Consider using 'networkProfiles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
snprrsNetworkProfiles :: Lens.Lens' SearchNetworkProfilesResponse (Core.Maybe [Types.NetworkProfileData])
snprrsNetworkProfiles = Lens.field @"networkProfiles"
{-# DEPRECATED snprrsNetworkProfiles "Use generic-lens or generic-optics with 'networkProfiles' instead." #-}

-- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by MaxResults.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
snprrsNextToken :: Lens.Lens' SearchNetworkProfilesResponse (Core.Maybe Types.NextToken)
snprrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED snprrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The total number of network profiles returned.
--
-- /Note:/ Consider using 'totalCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
snprrsTotalCount :: Lens.Lens' SearchNetworkProfilesResponse (Core.Maybe Core.Int)
snprrsTotalCount = Lens.field @"totalCount"
{-# DEPRECATED snprrsTotalCount "Use generic-lens or generic-optics with 'totalCount' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
snprrsResponseStatus :: Lens.Lens' SearchNetworkProfilesResponse Core.Int
snprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED snprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
