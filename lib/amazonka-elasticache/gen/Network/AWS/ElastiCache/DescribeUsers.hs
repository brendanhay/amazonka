{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeUsers (..),
    mkDescribeUsers,

    -- ** Request lenses
    duEngine,
    duFilters,
    duMarker,
    duMaxRecords,
    duUserId,

    -- * Destructuring the response
    DescribeUsersResponse (..),
    mkDescribeUsersResponse,

    -- ** Response lenses
    durrsMarker,
    durrsUsers,
    durrsResponseStatus,
  )
where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeUsers' smart constructor.
data DescribeUsers = DescribeUsers'
  { -- | The Redis engine.
    engine :: Core.Maybe Types.Engine,
    -- | Filter to determine the list of User IDs to return.
    filters :: Core.Maybe [Types.Filter],
    -- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by MaxRecords. >
    marker :: Core.Maybe Types.Marker,
    -- | The maximum number of records to include in the response. If more records exist than the specified MaxRecords value, a marker is included in the response so that the remaining results can be retrieved.
    maxRecords :: Core.Maybe Core.Int,
    -- | The ID of the user.
    userId :: Core.Maybe Types.UserId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeUsers' value with any optional fields omitted.
mkDescribeUsers ::
  DescribeUsers
mkDescribeUsers =
  DescribeUsers'
    { engine = Core.Nothing,
      filters = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing,
      userId = Core.Nothing
    }

-- | The Redis engine.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duEngine :: Lens.Lens' DescribeUsers (Core.Maybe Types.Engine)
duEngine = Lens.field @"engine"
{-# DEPRECATED duEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | Filter to determine the list of User IDs to return.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duFilters :: Lens.Lens' DescribeUsers (Core.Maybe [Types.Filter])
duFilters = Lens.field @"filters"
{-# DEPRECATED duFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by MaxRecords. >
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duMarker :: Lens.Lens' DescribeUsers (Core.Maybe Types.Marker)
duMarker = Lens.field @"marker"
{-# DEPRECATED duMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified MaxRecords value, a marker is included in the response so that the remaining results can be retrieved.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duMaxRecords :: Lens.Lens' DescribeUsers (Core.Maybe Core.Int)
duMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED duMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The ID of the user.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duUserId :: Lens.Lens' DescribeUsers (Core.Maybe Types.UserId)
duUserId = Lens.field @"userId"
{-# DEPRECATED duUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

instance Core.AWSRequest DescribeUsers where
  type Rs DescribeUsers = DescribeUsersResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DescribeUsers")
                Core.<> (Core.pure ("Version", "2015-02-02"))
                Core.<> (Core.toQueryValue "Engine" Core.<$> engine)
                Core.<> ( Core.toQueryValue
                            "Filters"
                            (Core.toQueryList "member" Core.<$> filters)
                        )
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> (Core.toQueryValue "MaxRecords" Core.<$> maxRecords)
                Core.<> (Core.toQueryValue "UserId" Core.<$> userId)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeUsersResult"
      ( \s h x ->
          DescribeUsersResponse'
            Core.<$> (x Core..@? "Marker")
            Core.<*> (x Core..@? "Users" Core..<@> Core.parseXMLList "member")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeUsers where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"users" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | /See:/ 'mkDescribeUsersResponse' smart constructor.
data DescribeUsersResponse = DescribeUsersResponse'
  { -- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by MaxRecords. >
    marker :: Core.Maybe Types.String,
    -- | A list of users.
    users :: Core.Maybe [Types.User],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeUsersResponse' value with any optional fields omitted.
mkDescribeUsersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeUsersResponse
mkDescribeUsersResponse responseStatus =
  DescribeUsersResponse'
    { marker = Core.Nothing,
      users = Core.Nothing,
      responseStatus
    }

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by MaxRecords. >
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
durrsMarker :: Lens.Lens' DescribeUsersResponse (Core.Maybe Types.String)
durrsMarker = Lens.field @"marker"
{-# DEPRECATED durrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A list of users.
--
-- /Note:/ Consider using 'users' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
durrsUsers :: Lens.Lens' DescribeUsersResponse (Core.Maybe [Types.User])
durrsUsers = Lens.field @"users"
{-# DEPRECATED durrsUsers "Use generic-lens or generic-optics with 'users' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
durrsResponseStatus :: Lens.Lens' DescribeUsersResponse Core.Int
durrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED durrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
