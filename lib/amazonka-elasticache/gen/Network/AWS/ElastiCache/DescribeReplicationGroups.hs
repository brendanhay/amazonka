{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DescribeReplicationGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a particular replication group. If no identifier is specified, @DescribeReplicationGroups@ returns information about all replication groups.
--
-- This operation returns paginated results.
module Network.AWS.ElastiCache.DescribeReplicationGroups
    (
    -- * Creating a request
      DescribeReplicationGroups (..)
    , mkDescribeReplicationGroups
    -- ** Request lenses
    , drgsMarker
    , drgsMaxRecords
    , drgsReplicationGroupId

    -- * Destructuring the response
    , DescribeReplicationGroupsResponse (..)
    , mkDescribeReplicationGroupsResponse
    -- ** Response lenses
    , drgrfrsMarker
    , drgrfrsReplicationGroups
    , drgrfrsResponseStatus
    ) where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DescribeReplicationGroups@ operation.
--
-- /See:/ 'mkDescribeReplicationGroups' smart constructor.
data DescribeReplicationGroups = DescribeReplicationGroups'
  { marker :: Core.Maybe Core.Text
    -- ^ An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: minimum 20; maximum 100.
  , replicationGroupId :: Core.Maybe Core.Text
    -- ^ The identifier for the replication group to be described. This parameter is not case sensitive.
--
-- If you do not specify this parameter, information about all replication groups is returned.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeReplicationGroups' value with any optional fields omitted.
mkDescribeReplicationGroups
    :: DescribeReplicationGroups
mkDescribeReplicationGroups
  = DescribeReplicationGroups'{marker = Core.Nothing,
                               maxRecords = Core.Nothing, replicationGroupId = Core.Nothing}

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drgsMarker :: Lens.Lens' DescribeReplicationGroups (Core.Maybe Core.Text)
drgsMarker = Lens.field @"marker"
{-# INLINEABLE drgsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: minimum 20; maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drgsMaxRecords :: Lens.Lens' DescribeReplicationGroups (Core.Maybe Core.Int)
drgsMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE drgsMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

-- | The identifier for the replication group to be described. This parameter is not case sensitive.
--
-- If you do not specify this parameter, information about all replication groups is returned.
--
-- /Note:/ Consider using 'replicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drgsReplicationGroupId :: Lens.Lens' DescribeReplicationGroups (Core.Maybe Core.Text)
drgsReplicationGroupId = Lens.field @"replicationGroupId"
{-# INLINEABLE drgsReplicationGroupId #-}
{-# DEPRECATED replicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead"  #-}

instance Core.ToQuery DescribeReplicationGroups where
        toQuery DescribeReplicationGroups{..}
          = Core.toQueryPair "Action"
              ("DescribeReplicationGroups" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2015-02-02" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxRecords") maxRecords
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ReplicationGroupId")
                replicationGroupId

instance Core.ToHeaders DescribeReplicationGroups where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeReplicationGroups where
        type Rs DescribeReplicationGroups =
             DescribeReplicationGroupsResponse
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
          = Response.receiveXMLWrapper "DescribeReplicationGroupsResult"
              (\ s h x ->
                 DescribeReplicationGroupsResponse' Core.<$>
                   (x Core..@? "Marker") Core.<*>
                     x Core..@? "ReplicationGroups" Core..<@>
                       Core.parseXMLList "ReplicationGroup"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeReplicationGroups where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"replicationGroups" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | Represents the output of a @DescribeReplicationGroups@ operation.
--
-- /See:/ 'mkDescribeReplicationGroupsResponse' smart constructor.
data DescribeReplicationGroupsResponse = DescribeReplicationGroupsResponse'
  { marker :: Core.Maybe Core.Text
    -- ^ Provides an identifier to allow retrieval of paginated results.
  , replicationGroups :: Core.Maybe [Types.ReplicationGroup]
    -- ^ A list of replication groups. Each item in the list contains detailed information about one replication group.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeReplicationGroupsResponse' value with any optional fields omitted.
mkDescribeReplicationGroupsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeReplicationGroupsResponse
mkDescribeReplicationGroupsResponse responseStatus
  = DescribeReplicationGroupsResponse'{marker = Core.Nothing,
                                       replicationGroups = Core.Nothing, responseStatus}

-- | Provides an identifier to allow retrieval of paginated results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drgrfrsMarker :: Lens.Lens' DescribeReplicationGroupsResponse (Core.Maybe Core.Text)
drgrfrsMarker = Lens.field @"marker"
{-# INLINEABLE drgrfrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | A list of replication groups. Each item in the list contains detailed information about one replication group.
--
-- /Note:/ Consider using 'replicationGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drgrfrsReplicationGroups :: Lens.Lens' DescribeReplicationGroupsResponse (Core.Maybe [Types.ReplicationGroup])
drgrfrsReplicationGroups = Lens.field @"replicationGroups"
{-# INLINEABLE drgrfrsReplicationGroups #-}
{-# DEPRECATED replicationGroups "Use generic-lens or generic-optics with 'replicationGroups' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drgrfrsResponseStatus :: Lens.Lens' DescribeReplicationGroupsResponse Core.Int
drgrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drgrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
