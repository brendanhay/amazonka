{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DescribeCacheEngineVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the available cache engines and their versions.
--
-- This operation returns paginated results.
module Network.AWS.ElastiCache.DescribeCacheEngineVersions
  ( -- * Creating a request
    DescribeCacheEngineVersions (..),
    mkDescribeCacheEngineVersions,

    -- ** Request lenses
    dcevCacheParameterGroupFamily,
    dcevDefaultOnly,
    dcevEngine,
    dcevEngineVersion,
    dcevMarker,
    dcevMaxRecords,

    -- * Destructuring the response
    DescribeCacheEngineVersionsResponse (..),
    mkDescribeCacheEngineVersionsResponse,

    -- ** Response lenses
    dcevrrsCacheEngineVersions,
    dcevrrsMarker,
    dcevrrsResponseStatus,
  )
where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DescribeCacheEngineVersions@ operation.
--
-- /See:/ 'mkDescribeCacheEngineVersions' smart constructor.
data DescribeCacheEngineVersions = DescribeCacheEngineVersions'
  { -- | The name of a specific cache parameter group family to return details for.
    --
    -- Valid values are: @memcached1.4@ | @memcached1.5@ | @memcached1.6@ | @redis2.6@ | @redis2.8@ | @redis3.2@ | @redis4.0@ | @redis5.0@ | @redis6.x@ |
    -- Constraints:
    --
    --     * Must be 1 to 255 alphanumeric characters
    --
    --
    --     * First character must be a letter
    --
    --
    --     * Cannot end with a hyphen or contain two consecutive hyphens
    cacheParameterGroupFamily :: Core.Maybe Types.String,
    -- | If @true@ , specifies that only the default version of the specified engine or engine and major version combination is to be returned.
    defaultOnly :: Core.Maybe Core.Bool,
    -- | The cache engine to return. Valid values: @memcached@ | @redis@
    engine :: Core.Maybe Types.String,
    -- | The cache engine version to return.
    --
    -- Example: @1.4.14@
    engineVersion :: Core.Maybe Types.String,
    -- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Core.Maybe Types.String,
    -- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved.
    --
    -- Default: 100
    -- Constraints: minimum 20; maximum 100.
    maxRecords :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCacheEngineVersions' value with any optional fields omitted.
mkDescribeCacheEngineVersions ::
  DescribeCacheEngineVersions
mkDescribeCacheEngineVersions =
  DescribeCacheEngineVersions'
    { cacheParameterGroupFamily =
        Core.Nothing,
      defaultOnly = Core.Nothing,
      engine = Core.Nothing,
      engineVersion = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | The name of a specific cache parameter group family to return details for.
--
-- Valid values are: @memcached1.4@ | @memcached1.5@ | @memcached1.6@ | @redis2.6@ | @redis2.8@ | @redis3.2@ | @redis4.0@ | @redis5.0@ | @redis6.x@ |
-- Constraints:
--
--     * Must be 1 to 255 alphanumeric characters
--
--
--     * First character must be a letter
--
--
--     * Cannot end with a hyphen or contain two consecutive hyphens
--
--
--
-- /Note:/ Consider using 'cacheParameterGroupFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcevCacheParameterGroupFamily :: Lens.Lens' DescribeCacheEngineVersions (Core.Maybe Types.String)
dcevCacheParameterGroupFamily = Lens.field @"cacheParameterGroupFamily"
{-# DEPRECATED dcevCacheParameterGroupFamily "Use generic-lens or generic-optics with 'cacheParameterGroupFamily' instead." #-}

-- | If @true@ , specifies that only the default version of the specified engine or engine and major version combination is to be returned.
--
-- /Note:/ Consider using 'defaultOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcevDefaultOnly :: Lens.Lens' DescribeCacheEngineVersions (Core.Maybe Core.Bool)
dcevDefaultOnly = Lens.field @"defaultOnly"
{-# DEPRECATED dcevDefaultOnly "Use generic-lens or generic-optics with 'defaultOnly' instead." #-}

-- | The cache engine to return. Valid values: @memcached@ | @redis@
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcevEngine :: Lens.Lens' DescribeCacheEngineVersions (Core.Maybe Types.String)
dcevEngine = Lens.field @"engine"
{-# DEPRECATED dcevEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | The cache engine version to return.
--
-- Example: @1.4.14@
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcevEngineVersion :: Lens.Lens' DescribeCacheEngineVersions (Core.Maybe Types.String)
dcevEngineVersion = Lens.field @"engineVersion"
{-# DEPRECATED dcevEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcevMarker :: Lens.Lens' DescribeCacheEngineVersions (Core.Maybe Types.String)
dcevMarker = Lens.field @"marker"
{-# DEPRECATED dcevMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: minimum 20; maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcevMaxRecords :: Lens.Lens' DescribeCacheEngineVersions (Core.Maybe Core.Int)
dcevMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED dcevMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Core.AWSRequest DescribeCacheEngineVersions where
  type
    Rs DescribeCacheEngineVersions =
      DescribeCacheEngineVersionsResponse
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
            ( Core.pure ("Action", "DescribeCacheEngineVersions")
                Core.<> (Core.pure ("Version", "2015-02-02"))
                Core.<> ( Core.toQueryValue "CacheParameterGroupFamily"
                            Core.<$> cacheParameterGroupFamily
                        )
                Core.<> (Core.toQueryValue "DefaultOnly" Core.<$> defaultOnly)
                Core.<> (Core.toQueryValue "Engine" Core.<$> engine)
                Core.<> (Core.toQueryValue "EngineVersion" Core.<$> engineVersion)
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> (Core.toQueryValue "MaxRecords" Core.<$> maxRecords)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeCacheEngineVersionsResult"
      ( \s h x ->
          DescribeCacheEngineVersionsResponse'
            Core.<$> ( x Core..@? "CacheEngineVersions"
                         Core..<@> Core.parseXMLList "CacheEngineVersion"
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeCacheEngineVersions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"cacheEngineVersions" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | Represents the output of a 'DescribeCacheEngineVersions' operation.
--
-- /See:/ 'mkDescribeCacheEngineVersionsResponse' smart constructor.
data DescribeCacheEngineVersionsResponse = DescribeCacheEngineVersionsResponse'
  { -- | A list of cache engine version details. Each element in the list contains detailed information about one cache engine version.
    cacheEngineVersions :: Core.Maybe [Types.CacheEngineVersion],
    -- | Provides an identifier to allow retrieval of paginated results.
    marker :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCacheEngineVersionsResponse' value with any optional fields omitted.
mkDescribeCacheEngineVersionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeCacheEngineVersionsResponse
mkDescribeCacheEngineVersionsResponse responseStatus =
  DescribeCacheEngineVersionsResponse'
    { cacheEngineVersions =
        Core.Nothing,
      marker = Core.Nothing,
      responseStatus
    }

-- | A list of cache engine version details. Each element in the list contains detailed information about one cache engine version.
--
-- /Note:/ Consider using 'cacheEngineVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcevrrsCacheEngineVersions :: Lens.Lens' DescribeCacheEngineVersionsResponse (Core.Maybe [Types.CacheEngineVersion])
dcevrrsCacheEngineVersions = Lens.field @"cacheEngineVersions"
{-# DEPRECATED dcevrrsCacheEngineVersions "Use generic-lens or generic-optics with 'cacheEngineVersions' instead." #-}

-- | Provides an identifier to allow retrieval of paginated results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcevrrsMarker :: Lens.Lens' DescribeCacheEngineVersionsResponse (Core.Maybe Types.String)
dcevrrsMarker = Lens.field @"marker"
{-# DEPRECATED dcevrrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcevrrsResponseStatus :: Lens.Lens' DescribeCacheEngineVersionsResponse Core.Int
dcevrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcevrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
