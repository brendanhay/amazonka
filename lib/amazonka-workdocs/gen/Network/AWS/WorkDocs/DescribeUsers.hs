{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.DescribeUsers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified users. You can describe all users or filter the results (for example, by status or organization).
--
-- By default, Amazon WorkDocs returns the first 24 active or pending users. If there are more results, the response includes a marker that you can use to request the next set of results.
--
-- This operation returns paginated results.
module Network.AWS.WorkDocs.DescribeUsers
    (
    -- * Creating a request
      DescribeUsers (..)
    , mkDescribeUsers
    -- ** Request lenses
    , duAuthenticationToken
    , duFields
    , duInclude
    , duLimit
    , duMarker
    , duOrder
    , duOrganizationId
    , duQuery
    , duSort
    , duUserIds

    -- * Destructuring the response
    , DescribeUsersResponse (..)
    , mkDescribeUsersResponse
    -- ** Response lenses
    , durrsMarker
    , durrsTotalNumberOfUsers
    , durrsUsers
    , durrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkDescribeUsers' smart constructor.
data DescribeUsers = DescribeUsers'
  { authenticationToken :: Core.Maybe Types.AuthenticationHeaderType
    -- ^ Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
  , fields :: Core.Maybe Types.Fields
    -- ^ A comma-separated list of values. Specify "STORAGE_METADATA" to include the user storage quota and utilization information.
  , include :: Core.Maybe Types.UserFilterType
    -- ^ The state of the users. Specify "ALL" to include inactive users.
  , limit :: Core.Maybe Core.Natural
    -- ^ The maximum number of items to return.
  , marker :: Core.Maybe Types.PageMarkerType
    -- ^ The marker for the next set of results. (You received this marker from a previous call.)
  , order :: Core.Maybe Types.OrderType
    -- ^ The order for the results.
  , organizationId :: Core.Maybe Types.IdType
    -- ^ The ID of the organization.
  , query :: Core.Maybe Types.SearchQueryType
    -- ^ A query to filter users by user name.
  , sort :: Core.Maybe Types.UserSortType
    -- ^ The sorting criteria.
  , userIds :: Core.Maybe Types.UserIdsType
    -- ^ The IDs of the users.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeUsers' value with any optional fields omitted.
mkDescribeUsers
    :: DescribeUsers
mkDescribeUsers
  = DescribeUsers'{authenticationToken = Core.Nothing,
                   fields = Core.Nothing, include = Core.Nothing,
                   limit = Core.Nothing, marker = Core.Nothing, order = Core.Nothing,
                   organizationId = Core.Nothing, query = Core.Nothing,
                   sort = Core.Nothing, userIds = Core.Nothing}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duAuthenticationToken :: Lens.Lens' DescribeUsers (Core.Maybe Types.AuthenticationHeaderType)
duAuthenticationToken = Lens.field @"authenticationToken"
{-# INLINEABLE duAuthenticationToken #-}
{-# DEPRECATED authenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead"  #-}

-- | A comma-separated list of values. Specify "STORAGE_METADATA" to include the user storage quota and utilization information.
--
-- /Note:/ Consider using 'fields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duFields :: Lens.Lens' DescribeUsers (Core.Maybe Types.Fields)
duFields = Lens.field @"fields"
{-# INLINEABLE duFields #-}
{-# DEPRECATED fields "Use generic-lens or generic-optics with 'fields' instead"  #-}

-- | The state of the users. Specify "ALL" to include inactive users.
--
-- /Note:/ Consider using 'include' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duInclude :: Lens.Lens' DescribeUsers (Core.Maybe Types.UserFilterType)
duInclude = Lens.field @"include"
{-# INLINEABLE duInclude #-}
{-# DEPRECATED include "Use generic-lens or generic-optics with 'include' instead"  #-}

-- | The maximum number of items to return.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duLimit :: Lens.Lens' DescribeUsers (Core.Maybe Core.Natural)
duLimit = Lens.field @"limit"
{-# INLINEABLE duLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The marker for the next set of results. (You received this marker from a previous call.)
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duMarker :: Lens.Lens' DescribeUsers (Core.Maybe Types.PageMarkerType)
duMarker = Lens.field @"marker"
{-# INLINEABLE duMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The order for the results.
--
-- /Note:/ Consider using 'order' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duOrder :: Lens.Lens' DescribeUsers (Core.Maybe Types.OrderType)
duOrder = Lens.field @"order"
{-# INLINEABLE duOrder #-}
{-# DEPRECATED order "Use generic-lens or generic-optics with 'order' instead"  #-}

-- | The ID of the organization.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duOrganizationId :: Lens.Lens' DescribeUsers (Core.Maybe Types.IdType)
duOrganizationId = Lens.field @"organizationId"
{-# INLINEABLE duOrganizationId #-}
{-# DEPRECATED organizationId "Use generic-lens or generic-optics with 'organizationId' instead"  #-}

-- | A query to filter users by user name.
--
-- /Note:/ Consider using 'query' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duQuery :: Lens.Lens' DescribeUsers (Core.Maybe Types.SearchQueryType)
duQuery = Lens.field @"query"
{-# INLINEABLE duQuery #-}
{-# DEPRECATED query "Use generic-lens or generic-optics with 'query' instead"  #-}

-- | The sorting criteria.
--
-- /Note:/ Consider using 'sort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duSort :: Lens.Lens' DescribeUsers (Core.Maybe Types.UserSortType)
duSort = Lens.field @"sort"
{-# INLINEABLE duSort #-}
{-# DEPRECATED sort "Use generic-lens or generic-optics with 'sort' instead"  #-}

-- | The IDs of the users.
--
-- /Note:/ Consider using 'userIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duUserIds :: Lens.Lens' DescribeUsers (Core.Maybe Types.UserIdsType)
duUserIds = Lens.field @"userIds"
{-# INLINEABLE duUserIds #-}
{-# DEPRECATED userIds "Use generic-lens or generic-optics with 'userIds' instead"  #-}

instance Core.ToQuery DescribeUsers where
        toQuery DescribeUsers{..}
          = Core.maybe Core.mempty (Core.toQueryPair "fields") fields Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "include") include
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "limit") limit
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "marker") marker
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "order") order
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "organizationId")
                organizationId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "query") query
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "sort") sort
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "userIds") userIds

instance Core.ToHeaders DescribeUsers where
        toHeaders DescribeUsers{..}
          = Core.toHeaders "Authentication" authenticationToken Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DescribeUsers where
        type Rs DescribeUsers = DescribeUsersResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET, Core._rqPath = "/api/v1/users",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeUsersResponse' Core.<$>
                   (x Core..:? "Marker") Core.<*> x Core..:? "TotalNumberOfUsers"
                     Core.<*> x Core..:? "Users"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeUsers where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"users" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | /See:/ 'mkDescribeUsersResponse' smart constructor.
data DescribeUsersResponse = DescribeUsersResponse'
  { marker :: Core.Maybe Types.Marker
    -- ^ The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
  , totalNumberOfUsers :: Core.Maybe Core.Integer
    -- ^ The total number of users included in the results.
  , users :: Core.Maybe [Types.User]
    -- ^ The users.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeUsersResponse' value with any optional fields omitted.
mkDescribeUsersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeUsersResponse
mkDescribeUsersResponse responseStatus
  = DescribeUsersResponse'{marker = Core.Nothing,
                           totalNumberOfUsers = Core.Nothing, users = Core.Nothing,
                           responseStatus}

-- | The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
durrsMarker :: Lens.Lens' DescribeUsersResponse (Core.Maybe Types.Marker)
durrsMarker = Lens.field @"marker"
{-# INLINEABLE durrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The total number of users included in the results.
--
-- /Note:/ Consider using 'totalNumberOfUsers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
durrsTotalNumberOfUsers :: Lens.Lens' DescribeUsersResponse (Core.Maybe Core.Integer)
durrsTotalNumberOfUsers = Lens.field @"totalNumberOfUsers"
{-# INLINEABLE durrsTotalNumberOfUsers #-}
{-# DEPRECATED totalNumberOfUsers "Use generic-lens or generic-optics with 'totalNumberOfUsers' instead"  #-}

-- | The users.
--
-- /Note:/ Consider using 'users' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
durrsUsers :: Lens.Lens' DescribeUsersResponse (Core.Maybe [Types.User])
durrsUsers = Lens.field @"users"
{-# INLINEABLE durrsUsers #-}
{-# DEPRECATED users "Use generic-lens or generic-optics with 'users' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
durrsResponseStatus :: Lens.Lens' DescribeUsersResponse Core.Int
durrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE durrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
