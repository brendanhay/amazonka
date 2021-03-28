{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.DescribeGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the groups specified by the query. Groups are defined by the underlying Active Directory.
--
-- This operation returns paginated results.
module Network.AWS.WorkDocs.DescribeGroups
    (
    -- * Creating a request
      DescribeGroups (..)
    , mkDescribeGroups
    -- ** Request lenses
    , dgSearchQuery
    , dgAuthenticationToken
    , dgLimit
    , dgMarker
    , dgOrganizationId

    -- * Destructuring the response
    , DescribeGroupsResponse (..)
    , mkDescribeGroupsResponse
    -- ** Response lenses
    , dgrrsGroups
    , dgrrsMarker
    , dgrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkDescribeGroups' smart constructor.
data DescribeGroups = DescribeGroups'
  { searchQuery :: Types.SearchQueryType
    -- ^ A query to describe groups by group name.
  , authenticationToken :: Core.Maybe Types.AuthenticationHeaderType
    -- ^ Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
  , limit :: Core.Maybe Core.Natural
    -- ^ The maximum number of items to return with this call.
  , marker :: Core.Maybe Types.MarkerType
    -- ^ The marker for the next set of results. (You received this marker from a previous call.)
  , organizationId :: Core.Maybe Types.IdType
    -- ^ The ID of the organization.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeGroups' value with any optional fields omitted.
mkDescribeGroups
    :: Types.SearchQueryType -- ^ 'searchQuery'
    -> DescribeGroups
mkDescribeGroups searchQuery
  = DescribeGroups'{searchQuery, authenticationToken = Core.Nothing,
                    limit = Core.Nothing, marker = Core.Nothing,
                    organizationId = Core.Nothing}

-- | A query to describe groups by group name.
--
-- /Note:/ Consider using 'searchQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgSearchQuery :: Lens.Lens' DescribeGroups Types.SearchQueryType
dgSearchQuery = Lens.field @"searchQuery"
{-# INLINEABLE dgSearchQuery #-}
{-# DEPRECATED searchQuery "Use generic-lens or generic-optics with 'searchQuery' instead"  #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgAuthenticationToken :: Lens.Lens' DescribeGroups (Core.Maybe Types.AuthenticationHeaderType)
dgAuthenticationToken = Lens.field @"authenticationToken"
{-# INLINEABLE dgAuthenticationToken #-}
{-# DEPRECATED authenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead"  #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgLimit :: Lens.Lens' DescribeGroups (Core.Maybe Core.Natural)
dgLimit = Lens.field @"limit"
{-# INLINEABLE dgLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The marker for the next set of results. (You received this marker from a previous call.)
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgMarker :: Lens.Lens' DescribeGroups (Core.Maybe Types.MarkerType)
dgMarker = Lens.field @"marker"
{-# INLINEABLE dgMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The ID of the organization.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgOrganizationId :: Lens.Lens' DescribeGroups (Core.Maybe Types.IdType)
dgOrganizationId = Lens.field @"organizationId"
{-# INLINEABLE dgOrganizationId #-}
{-# DEPRECATED organizationId "Use generic-lens or generic-optics with 'organizationId' instead"  #-}

instance Core.ToQuery DescribeGroups where
        toQuery DescribeGroups{..}
          = Core.toQueryPair "searchQuery" searchQuery Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "limit") limit
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "organizationId")
                organizationId

instance Core.ToHeaders DescribeGroups where
        toHeaders DescribeGroups{..}
          = Core.toHeaders "Authentication" authenticationToken Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DescribeGroups where
        type Rs DescribeGroups = DescribeGroupsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET, Core._rqPath = "/api/v1/groups",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeGroupsResponse' Core.<$>
                   (x Core..:? "Groups") Core.<*> x Core..:? "Marker" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeGroups where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"groups" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | /See:/ 'mkDescribeGroupsResponse' smart constructor.
data DescribeGroupsResponse = DescribeGroupsResponse'
  { groups :: Core.Maybe [Types.GroupMetadata]
    -- ^ The list of groups.
  , marker :: Core.Maybe Types.MarkerType
    -- ^ The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeGroupsResponse' value with any optional fields omitted.
mkDescribeGroupsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeGroupsResponse
mkDescribeGroupsResponse responseStatus
  = DescribeGroupsResponse'{groups = Core.Nothing,
                            marker = Core.Nothing, responseStatus}

-- | The list of groups.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrrsGroups :: Lens.Lens' DescribeGroupsResponse (Core.Maybe [Types.GroupMetadata])
dgrrsGroups = Lens.field @"groups"
{-# INLINEABLE dgrrsGroups #-}
{-# DEPRECATED groups "Use generic-lens or generic-optics with 'groups' instead"  #-}

-- | The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrrsMarker :: Lens.Lens' DescribeGroupsResponse (Core.Maybe Types.MarkerType)
dgrrsMarker = Lens.field @"marker"
{-# INLINEABLE dgrrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrrsResponseStatus :: Lens.Lens' DescribeGroupsResponse Core.Int
dgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
