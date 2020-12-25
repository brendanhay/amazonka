{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DescribeGlobalReplicationGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a particular global replication group. If no identifier is specified, returns information about all Global Datastores.
--
-- This operation returns paginated results.
module Network.AWS.ElastiCache.DescribeGlobalReplicationGroups
  ( -- * Creating a request
    DescribeGlobalReplicationGroups (..),
    mkDescribeGlobalReplicationGroups,

    -- ** Request lenses
    dgrgsGlobalReplicationGroupId,
    dgrgsMarker,
    dgrgsMaxRecords,
    dgrgsShowMemberInfo,

    -- * Destructuring the response
    DescribeGlobalReplicationGroupsResponse (..),
    mkDescribeGlobalReplicationGroupsResponse,

    -- ** Response lenses
    dgrgrfrsGlobalReplicationGroups,
    dgrgrfrsMarker,
    dgrgrfrsResponseStatus,
  )
where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeGlobalReplicationGroups' smart constructor.
data DescribeGlobalReplicationGroups = DescribeGlobalReplicationGroups'
  { -- | The name of the Global Datastore
    globalReplicationGroupId :: Core.Maybe Types.GlobalReplicationGroupId,
    -- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Core.Maybe Types.Marker,
    -- | The maximum number of records to include in the response. If more records exist than the specified MaxRecords value, a marker is included in the response so that the remaining results can be retrieved.
    maxRecords :: Core.Maybe Core.Int,
    -- | Returns the list of members that comprise the Global Datastore.
    showMemberInfo :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeGlobalReplicationGroups' value with any optional fields omitted.
mkDescribeGlobalReplicationGroups ::
  DescribeGlobalReplicationGroups
mkDescribeGlobalReplicationGroups =
  DescribeGlobalReplicationGroups'
    { globalReplicationGroupId =
        Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing,
      showMemberInfo = Core.Nothing
    }

-- | The name of the Global Datastore
--
-- /Note:/ Consider using 'globalReplicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrgsGlobalReplicationGroupId :: Lens.Lens' DescribeGlobalReplicationGroups (Core.Maybe Types.GlobalReplicationGroupId)
dgrgsGlobalReplicationGroupId = Lens.field @"globalReplicationGroupId"
{-# DEPRECATED dgrgsGlobalReplicationGroupId "Use generic-lens or generic-optics with 'globalReplicationGroupId' instead." #-}

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrgsMarker :: Lens.Lens' DescribeGlobalReplicationGroups (Core.Maybe Types.Marker)
dgrgsMarker = Lens.field @"marker"
{-# DEPRECATED dgrgsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified MaxRecords value, a marker is included in the response so that the remaining results can be retrieved.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrgsMaxRecords :: Lens.Lens' DescribeGlobalReplicationGroups (Core.Maybe Core.Int)
dgrgsMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED dgrgsMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | Returns the list of members that comprise the Global Datastore.
--
-- /Note:/ Consider using 'showMemberInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrgsShowMemberInfo :: Lens.Lens' DescribeGlobalReplicationGroups (Core.Maybe Core.Bool)
dgrgsShowMemberInfo = Lens.field @"showMemberInfo"
{-# DEPRECATED dgrgsShowMemberInfo "Use generic-lens or generic-optics with 'showMemberInfo' instead." #-}

instance Core.AWSRequest DescribeGlobalReplicationGroups where
  type
    Rs DescribeGlobalReplicationGroups =
      DescribeGlobalReplicationGroupsResponse
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
            ( Core.pure ("Action", "DescribeGlobalReplicationGroups")
                Core.<> (Core.pure ("Version", "2015-02-02"))
                Core.<> ( Core.toQueryValue "GlobalReplicationGroupId"
                            Core.<$> globalReplicationGroupId
                        )
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> (Core.toQueryValue "MaxRecords" Core.<$> maxRecords)
                Core.<> (Core.toQueryValue "ShowMemberInfo" Core.<$> showMemberInfo)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeGlobalReplicationGroupsResult"
      ( \s h x ->
          DescribeGlobalReplicationGroupsResponse'
            Core.<$> ( x Core..@? "GlobalReplicationGroups"
                         Core..<@> Core.parseXMLList "GlobalReplicationGroup"
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeGlobalReplicationGroups where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"globalReplicationGroups" Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | /See:/ 'mkDescribeGlobalReplicationGroupsResponse' smart constructor.
data DescribeGlobalReplicationGroupsResponse = DescribeGlobalReplicationGroupsResponse'
  { -- | Indicates the slot configuration and global identifier for each slice group.
    globalReplicationGroups :: Core.Maybe [Types.GlobalReplicationGroup],
    -- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by MaxRecords. >
    marker :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeGlobalReplicationGroupsResponse' value with any optional fields omitted.
mkDescribeGlobalReplicationGroupsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeGlobalReplicationGroupsResponse
mkDescribeGlobalReplicationGroupsResponse responseStatus =
  DescribeGlobalReplicationGroupsResponse'
    { globalReplicationGroups =
        Core.Nothing,
      marker = Core.Nothing,
      responseStatus
    }

-- | Indicates the slot configuration and global identifier for each slice group.
--
-- /Note:/ Consider using 'globalReplicationGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrgrfrsGlobalReplicationGroups :: Lens.Lens' DescribeGlobalReplicationGroupsResponse (Core.Maybe [Types.GlobalReplicationGroup])
dgrgrfrsGlobalReplicationGroups = Lens.field @"globalReplicationGroups"
{-# DEPRECATED dgrgrfrsGlobalReplicationGroups "Use generic-lens or generic-optics with 'globalReplicationGroups' instead." #-}

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by MaxRecords. >
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrgrfrsMarker :: Lens.Lens' DescribeGlobalReplicationGroupsResponse (Core.Maybe Types.String)
dgrgrfrsMarker = Lens.field @"marker"
{-# DEPRECATED dgrgrfrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrgrfrsResponseStatus :: Lens.Lens' DescribeGlobalReplicationGroupsResponse Core.Int
dgrgrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dgrgrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
