{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DescribeUsers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of users.
--
-- This operation returns paginated results.
module Network.AWS.ElastiCache.DescribeUsers
    (
    -- * Creating a request
      DescribeUsers (..)
    , mkDescribeUsers
    -- ** Request lenses
    , duEngine
    , duFilters
    , duMarker
    , duMaxRecords
    , duUserId

    -- * Destructuring the response
    , DescribeUsersResponse (..)
    , mkDescribeUsersResponse
    -- ** Response lenses
    , durrsMarker
    , durrsUsers
    , durrsResponseStatus
    ) where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeUsers' smart constructor.
data DescribeUsers = DescribeUsers'
  { engine :: Core.Maybe Types.Engine
    -- ^ The Redis engine. 
  , filters :: Core.Maybe [Types.Filter]
    -- ^ Filter to determine the list of User IDs to return.
  , marker :: Core.Maybe Core.Text
    -- ^ An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by MaxRecords. >
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of records to include in the response. If more records exist than the specified MaxRecords value, a marker is included in the response so that the remaining results can be retrieved. 
  , userId :: Core.Maybe Types.UserId
    -- ^ The ID of the user.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeUsers' value with any optional fields omitted.
mkDescribeUsers
    :: DescribeUsers
mkDescribeUsers
  = DescribeUsers'{engine = Core.Nothing, filters = Core.Nothing,
                   marker = Core.Nothing, maxRecords = Core.Nothing,
                   userId = Core.Nothing}

-- | The Redis engine. 
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duEngine :: Lens.Lens' DescribeUsers (Core.Maybe Types.Engine)
duEngine = Lens.field @"engine"
{-# INLINEABLE duEngine #-}
{-# DEPRECATED engine "Use generic-lens or generic-optics with 'engine' instead"  #-}

-- | Filter to determine the list of User IDs to return.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duFilters :: Lens.Lens' DescribeUsers (Core.Maybe [Types.Filter])
duFilters = Lens.field @"filters"
{-# INLINEABLE duFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by MaxRecords. >
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duMarker :: Lens.Lens' DescribeUsers (Core.Maybe Core.Text)
duMarker = Lens.field @"marker"
{-# INLINEABLE duMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of records to include in the response. If more records exist than the specified MaxRecords value, a marker is included in the response so that the remaining results can be retrieved. 
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duMaxRecords :: Lens.Lens' DescribeUsers (Core.Maybe Core.Int)
duMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE duMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

-- | The ID of the user.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duUserId :: Lens.Lens' DescribeUsers (Core.Maybe Types.UserId)
duUserId = Lens.field @"userId"
{-# INLINEABLE duUserId #-}
{-# DEPRECATED userId "Use generic-lens or generic-optics with 'userId' instead"  #-}

instance Core.ToQuery DescribeUsers where
        toQuery DescribeUsers{..}
          = Core.toQueryPair "Action" ("DescribeUsers" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2015-02-02" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Engine") engine
              Core.<>
              Core.toQueryPair "Filters"
                (Core.maybe Core.mempty (Core.toQueryList "member") filters)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxRecords") maxRecords
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "UserId") userId

instance Core.ToHeaders DescribeUsers where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeUsers where
        type Rs DescribeUsers = DescribeUsersResponse
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
          = Response.receiveXMLWrapper "DescribeUsersResult"
              (\ s h x ->
                 DescribeUsersResponse' Core.<$>
                   (x Core..@? "Marker") Core.<*>
                     x Core..@? "Users" Core..<@> Core.parseXMLList "member"
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
  { marker :: Core.Maybe Core.Text
    -- ^ An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by MaxRecords. >
  , users :: Core.Maybe [Types.User]
    -- ^ A list of users.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeUsersResponse' value with any optional fields omitted.
mkDescribeUsersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeUsersResponse
mkDescribeUsersResponse responseStatus
  = DescribeUsersResponse'{marker = Core.Nothing,
                           users = Core.Nothing, responseStatus}

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by MaxRecords. >
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
durrsMarker :: Lens.Lens' DescribeUsersResponse (Core.Maybe Core.Text)
durrsMarker = Lens.field @"marker"
{-# INLINEABLE durrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | A list of users.
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
