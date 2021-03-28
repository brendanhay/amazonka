{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DescribeCacheParameterGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of cache parameter group descriptions. If a cache parameter group name is specified, the list contains only the descriptions for that group.
--
-- This operation returns paginated results.
module Network.AWS.ElastiCache.DescribeCacheParameterGroups
    (
    -- * Creating a request
      DescribeCacheParameterGroups (..)
    , mkDescribeCacheParameterGroups
    -- ** Request lenses
    , dcpgCacheParameterGroupName
    , dcpgMarker
    , dcpgMaxRecords

    -- * Destructuring the response
    , DescribeCacheParameterGroupsResponse (..)
    , mkDescribeCacheParameterGroupsResponse
    -- ** Response lenses
    , dcpgrrsCacheParameterGroups
    , dcpgrrsMarker
    , dcpgrrsResponseStatus
    ) where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DescribeCacheParameterGroups@ operation.
--
-- /See:/ 'mkDescribeCacheParameterGroups' smart constructor.
data DescribeCacheParameterGroups = DescribeCacheParameterGroups'
  { cacheParameterGroupName :: Core.Maybe Core.Text
    -- ^ The name of a specific cache parameter group to return details for.
  , marker :: Core.Maybe Core.Text
    -- ^ An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: minimum 20; maximum 100.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCacheParameterGroups' value with any optional fields omitted.
mkDescribeCacheParameterGroups
    :: DescribeCacheParameterGroups
mkDescribeCacheParameterGroups
  = DescribeCacheParameterGroups'{cacheParameterGroupName =
                                    Core.Nothing,
                                  marker = Core.Nothing, maxRecords = Core.Nothing}

-- | The name of a specific cache parameter group to return details for.
--
-- /Note:/ Consider using 'cacheParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpgCacheParameterGroupName :: Lens.Lens' DescribeCacheParameterGroups (Core.Maybe Core.Text)
dcpgCacheParameterGroupName = Lens.field @"cacheParameterGroupName"
{-# INLINEABLE dcpgCacheParameterGroupName #-}
{-# DEPRECATED cacheParameterGroupName "Use generic-lens or generic-optics with 'cacheParameterGroupName' instead"  #-}

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpgMarker :: Lens.Lens' DescribeCacheParameterGroups (Core.Maybe Core.Text)
dcpgMarker = Lens.field @"marker"
{-# INLINEABLE dcpgMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: minimum 20; maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpgMaxRecords :: Lens.Lens' DescribeCacheParameterGroups (Core.Maybe Core.Int)
dcpgMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE dcpgMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

instance Core.ToQuery DescribeCacheParameterGroups where
        toQuery DescribeCacheParameterGroups{..}
          = Core.toQueryPair "Action"
              ("DescribeCacheParameterGroups" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2015-02-02" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "CacheParameterGroupName")
                cacheParameterGroupName
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxRecords") maxRecords

instance Core.ToHeaders DescribeCacheParameterGroups where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeCacheParameterGroups where
        type Rs DescribeCacheParameterGroups =
             DescribeCacheParameterGroupsResponse
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
          = Response.receiveXMLWrapper "DescribeCacheParameterGroupsResult"
              (\ s h x ->
                 DescribeCacheParameterGroupsResponse' Core.<$>
                   (x Core..@? "CacheParameterGroups" Core..<@>
                      Core.parseXMLList "CacheParameterGroup")
                     Core.<*> x Core..@? "Marker"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeCacheParameterGroups where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"cacheParameterGroups" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | Represents the output of a @DescribeCacheParameterGroups@ operation.
--
-- /See:/ 'mkDescribeCacheParameterGroupsResponse' smart constructor.
data DescribeCacheParameterGroupsResponse = DescribeCacheParameterGroupsResponse'
  { cacheParameterGroups :: Core.Maybe [Types.CacheParameterGroup]
    -- ^ A list of cache parameter groups. Each element in the list contains detailed information about one cache parameter group.
  , marker :: Core.Maybe Core.Text
    -- ^ Provides an identifier to allow retrieval of paginated results.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCacheParameterGroupsResponse' value with any optional fields omitted.
mkDescribeCacheParameterGroupsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeCacheParameterGroupsResponse
mkDescribeCacheParameterGroupsResponse responseStatus
  = DescribeCacheParameterGroupsResponse'{cacheParameterGroups =
                                            Core.Nothing,
                                          marker = Core.Nothing, responseStatus}

-- | A list of cache parameter groups. Each element in the list contains detailed information about one cache parameter group.
--
-- /Note:/ Consider using 'cacheParameterGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpgrrsCacheParameterGroups :: Lens.Lens' DescribeCacheParameterGroupsResponse (Core.Maybe [Types.CacheParameterGroup])
dcpgrrsCacheParameterGroups = Lens.field @"cacheParameterGroups"
{-# INLINEABLE dcpgrrsCacheParameterGroups #-}
{-# DEPRECATED cacheParameterGroups "Use generic-lens or generic-optics with 'cacheParameterGroups' instead"  #-}

-- | Provides an identifier to allow retrieval of paginated results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpgrrsMarker :: Lens.Lens' DescribeCacheParameterGroupsResponse (Core.Maybe Core.Text)
dcpgrrsMarker = Lens.field @"marker"
{-# INLINEABLE dcpgrrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpgrrsResponseStatus :: Lens.Lens' DescribeCacheParameterGroupsResponse Core.Int
dcpgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcpgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
