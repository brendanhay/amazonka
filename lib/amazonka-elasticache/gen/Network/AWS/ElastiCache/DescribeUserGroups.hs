{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DescribeUserGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of user groups.
--
-- This operation returns paginated results.
module Network.AWS.ElastiCache.DescribeUserGroups
    (
    -- * Creating a request
      DescribeUserGroups (..)
    , mkDescribeUserGroups
    -- ** Request lenses
    , dugsMarker
    , dugsMaxRecords
    , dugsUserGroupId

    -- * Destructuring the response
    , DescribeUserGroupsResponse (..)
    , mkDescribeUserGroupsResponse
    -- ** Response lenses
    , dugrrsMarker
    , dugrrsUserGroups
    , dugrrsResponseStatus
    ) where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeUserGroups' smart constructor.
data DescribeUserGroups = DescribeUserGroups'
  { marker :: Core.Maybe Core.Text
    -- ^ An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by MaxRecords. >
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of records to include in the response. If more records exist than the specified MaxRecords value, a marker is included in the response so that the remaining results can be retrieved. 
  , userGroupId :: Core.Maybe Core.Text
    -- ^ The ID of the user group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeUserGroups' value with any optional fields omitted.
mkDescribeUserGroups
    :: DescribeUserGroups
mkDescribeUserGroups
  = DescribeUserGroups'{marker = Core.Nothing,
                        maxRecords = Core.Nothing, userGroupId = Core.Nothing}

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by MaxRecords. >
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dugsMarker :: Lens.Lens' DescribeUserGroups (Core.Maybe Core.Text)
dugsMarker = Lens.field @"marker"
{-# INLINEABLE dugsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of records to include in the response. If more records exist than the specified MaxRecords value, a marker is included in the response so that the remaining results can be retrieved. 
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dugsMaxRecords :: Lens.Lens' DescribeUserGroups (Core.Maybe Core.Int)
dugsMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE dugsMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

-- | The ID of the user group.
--
-- /Note:/ Consider using 'userGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dugsUserGroupId :: Lens.Lens' DescribeUserGroups (Core.Maybe Core.Text)
dugsUserGroupId = Lens.field @"userGroupId"
{-# INLINEABLE dugsUserGroupId #-}
{-# DEPRECATED userGroupId "Use generic-lens or generic-optics with 'userGroupId' instead"  #-}

instance Core.ToQuery DescribeUserGroups where
        toQuery DescribeUserGroups{..}
          = Core.toQueryPair "Action" ("DescribeUserGroups" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2015-02-02" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxRecords") maxRecords
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "UserGroupId") userGroupId

instance Core.ToHeaders DescribeUserGroups where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeUserGroups where
        type Rs DescribeUserGroups = DescribeUserGroupsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "DescribeUserGroupsResult"
              (\ s h x ->
                 DescribeUserGroupsResponse' Core.<$>
                   (x Core..@? "Marker") Core.<*>
                     x Core..@? "UserGroups" Core..<@> Core.parseXMLList "member"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeUserGroups where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"userGroups" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | /See:/ 'mkDescribeUserGroupsResponse' smart constructor.
data DescribeUserGroupsResponse = DescribeUserGroupsResponse'
  { marker :: Core.Maybe Core.Text
    -- ^ An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by MaxRecords. >
  , userGroups :: Core.Maybe [Types.UserGroup]
    -- ^ Returns a list of user groups.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeUserGroupsResponse' value with any optional fields omitted.
mkDescribeUserGroupsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeUserGroupsResponse
mkDescribeUserGroupsResponse responseStatus
  = DescribeUserGroupsResponse'{marker = Core.Nothing,
                                userGroups = Core.Nothing, responseStatus}

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by MaxRecords. >
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dugrrsMarker :: Lens.Lens' DescribeUserGroupsResponse (Core.Maybe Core.Text)
dugrrsMarker = Lens.field @"marker"
{-# INLINEABLE dugrrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | Returns a list of user groups.
--
-- /Note:/ Consider using 'userGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dugrrsUserGroups :: Lens.Lens' DescribeUserGroupsResponse (Core.Maybe [Types.UserGroup])
dugrrsUserGroups = Lens.field @"userGroups"
{-# INLINEABLE dugrrsUserGroups #-}
{-# DEPRECATED userGroups "Use generic-lens or generic-optics with 'userGroups' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dugrrsResponseStatus :: Lens.Lens' DescribeUserGroupsResponse Core.Int
dugrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dugrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
