{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DescribeCacheSubnetGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of cache subnet group descriptions. If a subnet group name is specified, the list contains only the description of that group. This is applicable only when you have ElastiCache in VPC setup. All ElastiCache clusters now launch in VPC by default. 
--
-- This operation returns paginated results.
module Network.AWS.ElastiCache.DescribeCacheSubnetGroups
    (
    -- * Creating a request
      DescribeCacheSubnetGroups (..)
    , mkDescribeCacheSubnetGroups
    -- ** Request lenses
    , dcsgCacheSubnetGroupName
    , dcsgMarker
    , dcsgMaxRecords

    -- * Destructuring the response
    , DescribeCacheSubnetGroupsResponse (..)
    , mkDescribeCacheSubnetGroupsResponse
    -- ** Response lenses
    , dcsgrrsCacheSubnetGroups
    , dcsgrrsMarker
    , dcsgrrsResponseStatus
    ) where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DescribeCacheSubnetGroups@ operation.
--
-- /See:/ 'mkDescribeCacheSubnetGroups' smart constructor.
data DescribeCacheSubnetGroups = DescribeCacheSubnetGroups'
  { cacheSubnetGroupName :: Core.Maybe Core.Text
    -- ^ The name of the cache subnet group to return details for.
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

-- | Creates a 'DescribeCacheSubnetGroups' value with any optional fields omitted.
mkDescribeCacheSubnetGroups
    :: DescribeCacheSubnetGroups
mkDescribeCacheSubnetGroups
  = DescribeCacheSubnetGroups'{cacheSubnetGroupName = Core.Nothing,
                               marker = Core.Nothing, maxRecords = Core.Nothing}

-- | The name of the cache subnet group to return details for.
--
-- /Note:/ Consider using 'cacheSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgCacheSubnetGroupName :: Lens.Lens' DescribeCacheSubnetGroups (Core.Maybe Core.Text)
dcsgCacheSubnetGroupName = Lens.field @"cacheSubnetGroupName"
{-# INLINEABLE dcsgCacheSubnetGroupName #-}
{-# DEPRECATED cacheSubnetGroupName "Use generic-lens or generic-optics with 'cacheSubnetGroupName' instead"  #-}

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgMarker :: Lens.Lens' DescribeCacheSubnetGroups (Core.Maybe Core.Text)
dcsgMarker = Lens.field @"marker"
{-# INLINEABLE dcsgMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: minimum 20; maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgMaxRecords :: Lens.Lens' DescribeCacheSubnetGroups (Core.Maybe Core.Int)
dcsgMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE dcsgMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

instance Core.ToQuery DescribeCacheSubnetGroups where
        toQuery DescribeCacheSubnetGroups{..}
          = Core.toQueryPair "Action"
              ("DescribeCacheSubnetGroups" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2015-02-02" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "CacheSubnetGroupName")
                cacheSubnetGroupName
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxRecords") maxRecords

instance Core.ToHeaders DescribeCacheSubnetGroups where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeCacheSubnetGroups where
        type Rs DescribeCacheSubnetGroups =
             DescribeCacheSubnetGroupsResponse
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
          = Response.receiveXMLWrapper "DescribeCacheSubnetGroupsResult"
              (\ s h x ->
                 DescribeCacheSubnetGroupsResponse' Core.<$>
                   (x Core..@? "CacheSubnetGroups" Core..<@>
                      Core.parseXMLList "CacheSubnetGroup")
                     Core.<*> x Core..@? "Marker"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeCacheSubnetGroups where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"cacheSubnetGroups" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | Represents the output of a @DescribeCacheSubnetGroups@ operation.
--
-- /See:/ 'mkDescribeCacheSubnetGroupsResponse' smart constructor.
data DescribeCacheSubnetGroupsResponse = DescribeCacheSubnetGroupsResponse'
  { cacheSubnetGroups :: Core.Maybe [Types.CacheSubnetGroup]
    -- ^ A list of cache subnet groups. Each element in the list contains detailed information about one group.
  , marker :: Core.Maybe Core.Text
    -- ^ Provides an identifier to allow retrieval of paginated results.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCacheSubnetGroupsResponse' value with any optional fields omitted.
mkDescribeCacheSubnetGroupsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeCacheSubnetGroupsResponse
mkDescribeCacheSubnetGroupsResponse responseStatus
  = DescribeCacheSubnetGroupsResponse'{cacheSubnetGroups =
                                         Core.Nothing,
                                       marker = Core.Nothing, responseStatus}

-- | A list of cache subnet groups. Each element in the list contains detailed information about one group.
--
-- /Note:/ Consider using 'cacheSubnetGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgrrsCacheSubnetGroups :: Lens.Lens' DescribeCacheSubnetGroupsResponse (Core.Maybe [Types.CacheSubnetGroup])
dcsgrrsCacheSubnetGroups = Lens.field @"cacheSubnetGroups"
{-# INLINEABLE dcsgrrsCacheSubnetGroups #-}
{-# DEPRECATED cacheSubnetGroups "Use generic-lens or generic-optics with 'cacheSubnetGroups' instead"  #-}

-- | Provides an identifier to allow retrieval of paginated results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgrrsMarker :: Lens.Lens' DescribeCacheSubnetGroupsResponse (Core.Maybe Core.Text)
dcsgrrsMarker = Lens.field @"marker"
{-# INLINEABLE dcsgrrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgrrsResponseStatus :: Lens.Lens' DescribeCacheSubnetGroupsResponse Core.Int
dcsgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcsgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
