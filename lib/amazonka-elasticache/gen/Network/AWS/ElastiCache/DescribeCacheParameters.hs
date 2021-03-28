{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DescribeCacheParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the detailed parameter list for a particular cache parameter group.
--
-- This operation returns paginated results.
module Network.AWS.ElastiCache.DescribeCacheParameters
    (
    -- * Creating a request
      DescribeCacheParameters (..)
    , mkDescribeCacheParameters
    -- ** Request lenses
    , dcpCacheParameterGroupName
    , dcpMarker
    , dcpMaxRecords
    , dcpSource

    -- * Destructuring the response
    , DescribeCacheParametersResponse (..)
    , mkDescribeCacheParametersResponse
    -- ** Response lenses
    , dcprrsCacheNodeTypeSpecificParameters
    , dcprrsMarker
    , dcprrsParameters
    , dcprrsResponseStatus
    ) where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DescribeCacheParameters@ operation.
--
-- /See:/ 'mkDescribeCacheParameters' smart constructor.
data DescribeCacheParameters = DescribeCacheParameters'
  { cacheParameterGroupName :: Core.Text
    -- ^ The name of a specific cache parameter group to return details for.
  , marker :: Core.Maybe Core.Text
    -- ^ An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: minimum 20; maximum 100.
  , source :: Core.Maybe Core.Text
    -- ^ The parameter types to return.
--
-- Valid values: @user@ | @system@ | @engine-default@ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCacheParameters' value with any optional fields omitted.
mkDescribeCacheParameters
    :: Core.Text -- ^ 'cacheParameterGroupName'
    -> DescribeCacheParameters
mkDescribeCacheParameters cacheParameterGroupName
  = DescribeCacheParameters'{cacheParameterGroupName,
                             marker = Core.Nothing, maxRecords = Core.Nothing,
                             source = Core.Nothing}

-- | The name of a specific cache parameter group to return details for.
--
-- /Note:/ Consider using 'cacheParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpCacheParameterGroupName :: Lens.Lens' DescribeCacheParameters Core.Text
dcpCacheParameterGroupName = Lens.field @"cacheParameterGroupName"
{-# INLINEABLE dcpCacheParameterGroupName #-}
{-# DEPRECATED cacheParameterGroupName "Use generic-lens or generic-optics with 'cacheParameterGroupName' instead"  #-}

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpMarker :: Lens.Lens' DescribeCacheParameters (Core.Maybe Core.Text)
dcpMarker = Lens.field @"marker"
{-# INLINEABLE dcpMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: minimum 20; maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpMaxRecords :: Lens.Lens' DescribeCacheParameters (Core.Maybe Core.Int)
dcpMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE dcpMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

-- | The parameter types to return.
--
-- Valid values: @user@ | @system@ | @engine-default@ 
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpSource :: Lens.Lens' DescribeCacheParameters (Core.Maybe Core.Text)
dcpSource = Lens.field @"source"
{-# INLINEABLE dcpSource #-}
{-# DEPRECATED source "Use generic-lens or generic-optics with 'source' instead"  #-}

instance Core.ToQuery DescribeCacheParameters where
        toQuery DescribeCacheParameters{..}
          = Core.toQueryPair "Action"
              ("DescribeCacheParameters" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2015-02-02" :: Core.Text)
              Core.<>
              Core.toQueryPair "CacheParameterGroupName" cacheParameterGroupName
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxRecords") maxRecords
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Source") source

instance Core.ToHeaders DescribeCacheParameters where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeCacheParameters where
        type Rs DescribeCacheParameters = DescribeCacheParametersResponse
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
          = Response.receiveXMLWrapper "DescribeCacheParametersResult"
              (\ s h x ->
                 DescribeCacheParametersResponse' Core.<$>
                   (x Core..@? "CacheNodeTypeSpecificParameters" Core..<@>
                      Core.parseXMLList "CacheNodeTypeSpecificParameter")
                     Core.<*> x Core..@? "Marker"
                     Core.<*>
                     x Core..@? "Parameters" Core..<@> Core.parseXMLList "Parameter"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeCacheParameters where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"parameters" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | Represents the output of a @DescribeCacheParameters@ operation.
--
-- /See:/ 'mkDescribeCacheParametersResponse' smart constructor.
data DescribeCacheParametersResponse = DescribeCacheParametersResponse'
  { cacheNodeTypeSpecificParameters :: Core.Maybe [Types.CacheNodeTypeSpecificParameter]
    -- ^ A list of parameters specific to a particular cache node type. Each element in the list contains detailed information about one parameter.
  , marker :: Core.Maybe Core.Text
    -- ^ Provides an identifier to allow retrieval of paginated results.
  , parameters :: Core.Maybe [Types.Parameter]
    -- ^ A list of 'Parameter' instances.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCacheParametersResponse' value with any optional fields omitted.
mkDescribeCacheParametersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeCacheParametersResponse
mkDescribeCacheParametersResponse responseStatus
  = DescribeCacheParametersResponse'{cacheNodeTypeSpecificParameters
                                       = Core.Nothing,
                                     marker = Core.Nothing, parameters = Core.Nothing,
                                     responseStatus}

-- | A list of parameters specific to a particular cache node type. Each element in the list contains detailed information about one parameter.
--
-- /Note:/ Consider using 'cacheNodeTypeSpecificParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprrsCacheNodeTypeSpecificParameters :: Lens.Lens' DescribeCacheParametersResponse (Core.Maybe [Types.CacheNodeTypeSpecificParameter])
dcprrsCacheNodeTypeSpecificParameters = Lens.field @"cacheNodeTypeSpecificParameters"
{-# INLINEABLE dcprrsCacheNodeTypeSpecificParameters #-}
{-# DEPRECATED cacheNodeTypeSpecificParameters "Use generic-lens or generic-optics with 'cacheNodeTypeSpecificParameters' instead"  #-}

-- | Provides an identifier to allow retrieval of paginated results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprrsMarker :: Lens.Lens' DescribeCacheParametersResponse (Core.Maybe Core.Text)
dcprrsMarker = Lens.field @"marker"
{-# INLINEABLE dcprrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | A list of 'Parameter' instances.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprrsParameters :: Lens.Lens' DescribeCacheParametersResponse (Core.Maybe [Types.Parameter])
dcprrsParameters = Lens.field @"parameters"
{-# INLINEABLE dcprrsParameters #-}
{-# DEPRECATED parameters "Use generic-lens or generic-optics with 'parameters' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprrsResponseStatus :: Lens.Lens' DescribeCacheParametersResponse Core.Int
dcprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
