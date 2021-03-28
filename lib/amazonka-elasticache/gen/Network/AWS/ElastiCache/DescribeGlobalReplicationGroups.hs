{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeGlobalReplicationGroups (..)
    , mkDescribeGlobalReplicationGroups
    -- ** Request lenses
    , dgrgsGlobalReplicationGroupId
    , dgrgsMarker
    , dgrgsMaxRecords
    , dgrgsShowMemberInfo

    -- * Destructuring the response
    , DescribeGlobalReplicationGroupsResponse (..)
    , mkDescribeGlobalReplicationGroupsResponse
    -- ** Response lenses
    , dgrgrfrsGlobalReplicationGroups
    , dgrgrfrsMarker
    , dgrgrfrsResponseStatus
    ) where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeGlobalReplicationGroups' smart constructor.
data DescribeGlobalReplicationGroups = DescribeGlobalReplicationGroups'
  { globalReplicationGroupId :: Core.Maybe Core.Text
    -- ^ The name of the Global Datastore
  , marker :: Core.Maybe Core.Text
    -- ^ An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of records to include in the response. If more records exist than the specified MaxRecords value, a marker is included in the response so that the remaining results can be retrieved. 
  , showMemberInfo :: Core.Maybe Core.Bool
    -- ^ Returns the list of members that comprise the Global Datastore.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeGlobalReplicationGroups' value with any optional fields omitted.
mkDescribeGlobalReplicationGroups
    :: DescribeGlobalReplicationGroups
mkDescribeGlobalReplicationGroups
  = DescribeGlobalReplicationGroups'{globalReplicationGroupId =
                                       Core.Nothing,
                                     marker = Core.Nothing, maxRecords = Core.Nothing,
                                     showMemberInfo = Core.Nothing}

-- | The name of the Global Datastore
--
-- /Note:/ Consider using 'globalReplicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrgsGlobalReplicationGroupId :: Lens.Lens' DescribeGlobalReplicationGroups (Core.Maybe Core.Text)
dgrgsGlobalReplicationGroupId = Lens.field @"globalReplicationGroupId"
{-# INLINEABLE dgrgsGlobalReplicationGroupId #-}
{-# DEPRECATED globalReplicationGroupId "Use generic-lens or generic-optics with 'globalReplicationGroupId' instead"  #-}

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrgsMarker :: Lens.Lens' DescribeGlobalReplicationGroups (Core.Maybe Core.Text)
dgrgsMarker = Lens.field @"marker"
{-# INLINEABLE dgrgsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of records to include in the response. If more records exist than the specified MaxRecords value, a marker is included in the response so that the remaining results can be retrieved. 
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrgsMaxRecords :: Lens.Lens' DescribeGlobalReplicationGroups (Core.Maybe Core.Int)
dgrgsMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE dgrgsMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

-- | Returns the list of members that comprise the Global Datastore.
--
-- /Note:/ Consider using 'showMemberInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrgsShowMemberInfo :: Lens.Lens' DescribeGlobalReplicationGroups (Core.Maybe Core.Bool)
dgrgsShowMemberInfo = Lens.field @"showMemberInfo"
{-# INLINEABLE dgrgsShowMemberInfo #-}
{-# DEPRECATED showMemberInfo "Use generic-lens or generic-optics with 'showMemberInfo' instead"  #-}

instance Core.ToQuery DescribeGlobalReplicationGroups where
        toQuery DescribeGlobalReplicationGroups{..}
          = Core.toQueryPair "Action"
              ("DescribeGlobalReplicationGroups" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2015-02-02" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "GlobalReplicationGroupId")
                globalReplicationGroupId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxRecords") maxRecords
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ShowMemberInfo")
                showMemberInfo

instance Core.ToHeaders DescribeGlobalReplicationGroups where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeGlobalReplicationGroups where
        type Rs DescribeGlobalReplicationGroups =
             DescribeGlobalReplicationGroupsResponse
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
          = Response.receiveXMLWrapper
              "DescribeGlobalReplicationGroupsResult"
              (\ s h x ->
                 DescribeGlobalReplicationGroupsResponse' Core.<$>
                   (x Core..@? "GlobalReplicationGroups" Core..<@>
                      Core.parseXMLList "GlobalReplicationGroup")
                     Core.<*> x Core..@? "Marker"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeGlobalReplicationGroups where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"globalReplicationGroups" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | /See:/ 'mkDescribeGlobalReplicationGroupsResponse' smart constructor.
data DescribeGlobalReplicationGroupsResponse = DescribeGlobalReplicationGroupsResponse'
  { globalReplicationGroups :: Core.Maybe [Types.GlobalReplicationGroup]
    -- ^ Indicates the slot configuration and global identifier for each slice group.
  , marker :: Core.Maybe Core.Text
    -- ^ An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by MaxRecords. >
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeGlobalReplicationGroupsResponse' value with any optional fields omitted.
mkDescribeGlobalReplicationGroupsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeGlobalReplicationGroupsResponse
mkDescribeGlobalReplicationGroupsResponse responseStatus
  = DescribeGlobalReplicationGroupsResponse'{globalReplicationGroups
                                               = Core.Nothing,
                                             marker = Core.Nothing, responseStatus}

-- | Indicates the slot configuration and global identifier for each slice group.
--
-- /Note:/ Consider using 'globalReplicationGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrgrfrsGlobalReplicationGroups :: Lens.Lens' DescribeGlobalReplicationGroupsResponse (Core.Maybe [Types.GlobalReplicationGroup])
dgrgrfrsGlobalReplicationGroups = Lens.field @"globalReplicationGroups"
{-# INLINEABLE dgrgrfrsGlobalReplicationGroups #-}
{-# DEPRECATED globalReplicationGroups "Use generic-lens or generic-optics with 'globalReplicationGroups' instead"  #-}

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by MaxRecords. >
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrgrfrsMarker :: Lens.Lens' DescribeGlobalReplicationGroupsResponse (Core.Maybe Core.Text)
dgrgrfrsMarker = Lens.field @"marker"
{-# INLINEABLE dgrgrfrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrgrfrsResponseStatus :: Lens.Lens' DescribeGlobalReplicationGroupsResponse Core.Int
dgrgrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dgrgrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
