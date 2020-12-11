{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    dcevEngineVersion,
    dcevCacheParameterGroupFamily,
    dcevDefaultOnly,
    dcevEngine,
    dcevMarker,
    dcevMaxRecords,

    -- * Destructuring the response
    DescribeCacheEngineVersionsResponse (..),
    mkDescribeCacheEngineVersionsResponse,

    -- ** Response lenses
    dcevrsCacheEngineVersions,
    dcevrsMarker,
    dcevrsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @DescribeCacheEngineVersions@ operation.
--
-- /See:/ 'mkDescribeCacheEngineVersions' smart constructor.
data DescribeCacheEngineVersions = DescribeCacheEngineVersions'
  { engineVersion ::
      Lude.Maybe Lude.Text,
    cacheParameterGroupFamily ::
      Lude.Maybe Lude.Text,
    defaultOnly :: Lude.Maybe Lude.Bool,
    engine :: Lude.Maybe Lude.Text,
    marker :: Lude.Maybe Lude.Text,
    maxRecords :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCacheEngineVersions' with the minimum fields required to make a request.
--
-- * 'cacheParameterGroupFamily' - The name of a specific cache parameter group family to return details for.
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
-- * 'defaultOnly' - If @true@ , specifies that only the default version of the specified engine or engine and major version combination is to be returned.
-- * 'engine' - The cache engine to return. Valid values: @memcached@ | @redis@
-- * 'engineVersion' - The cache engine version to return.
--
-- Example: @1.4.14@
-- * 'marker' - An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: minimum 20; maximum 100.
mkDescribeCacheEngineVersions ::
  DescribeCacheEngineVersions
mkDescribeCacheEngineVersions =
  DescribeCacheEngineVersions'
    { engineVersion = Lude.Nothing,
      cacheParameterGroupFamily = Lude.Nothing,
      defaultOnly = Lude.Nothing,
      engine = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing
    }

-- | The cache engine version to return.
--
-- Example: @1.4.14@
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcevEngineVersion :: Lens.Lens' DescribeCacheEngineVersions (Lude.Maybe Lude.Text)
dcevEngineVersion = Lens.lens (engineVersion :: DescribeCacheEngineVersions -> Lude.Maybe Lude.Text) (\s a -> s {engineVersion = a} :: DescribeCacheEngineVersions)
{-# DEPRECATED dcevEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

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
dcevCacheParameterGroupFamily :: Lens.Lens' DescribeCacheEngineVersions (Lude.Maybe Lude.Text)
dcevCacheParameterGroupFamily = Lens.lens (cacheParameterGroupFamily :: DescribeCacheEngineVersions -> Lude.Maybe Lude.Text) (\s a -> s {cacheParameterGroupFamily = a} :: DescribeCacheEngineVersions)
{-# DEPRECATED dcevCacheParameterGroupFamily "Use generic-lens or generic-optics with 'cacheParameterGroupFamily' instead." #-}

-- | If @true@ , specifies that only the default version of the specified engine or engine and major version combination is to be returned.
--
-- /Note:/ Consider using 'defaultOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcevDefaultOnly :: Lens.Lens' DescribeCacheEngineVersions (Lude.Maybe Lude.Bool)
dcevDefaultOnly = Lens.lens (defaultOnly :: DescribeCacheEngineVersions -> Lude.Maybe Lude.Bool) (\s a -> s {defaultOnly = a} :: DescribeCacheEngineVersions)
{-# DEPRECATED dcevDefaultOnly "Use generic-lens or generic-optics with 'defaultOnly' instead." #-}

-- | The cache engine to return. Valid values: @memcached@ | @redis@
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcevEngine :: Lens.Lens' DescribeCacheEngineVersions (Lude.Maybe Lude.Text)
dcevEngine = Lens.lens (engine :: DescribeCacheEngineVersions -> Lude.Maybe Lude.Text) (\s a -> s {engine = a} :: DescribeCacheEngineVersions)
{-# DEPRECATED dcevEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcevMarker :: Lens.Lens' DescribeCacheEngineVersions (Lude.Maybe Lude.Text)
dcevMarker = Lens.lens (marker :: DescribeCacheEngineVersions -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeCacheEngineVersions)
{-# DEPRECATED dcevMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: minimum 20; maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcevMaxRecords :: Lens.Lens' DescribeCacheEngineVersions (Lude.Maybe Lude.Int)
dcevMaxRecords = Lens.lens (maxRecords :: DescribeCacheEngineVersions -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeCacheEngineVersions)
{-# DEPRECATED dcevMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Page.AWSPager DescribeCacheEngineVersions where
  page rq rs
    | Page.stop (rs Lens.^. dcevrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dcevrsCacheEngineVersions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dcevMarker Lens..~ rs Lens.^. dcevrsMarker

instance Lude.AWSRequest DescribeCacheEngineVersions where
  type
    Rs DescribeCacheEngineVersions =
      DescribeCacheEngineVersionsResponse
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "DescribeCacheEngineVersionsResult"
      ( \s h x ->
          DescribeCacheEngineVersionsResponse'
            Lude.<$> ( x Lude..@? "CacheEngineVersions" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "CacheEngineVersion")
                     )
            Lude.<*> (x Lude..@? "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeCacheEngineVersions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeCacheEngineVersions where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeCacheEngineVersions where
  toQuery DescribeCacheEngineVersions' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeCacheEngineVersions" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "EngineVersion" Lude.=: engineVersion,
        "CacheParameterGroupFamily" Lude.=: cacheParameterGroupFamily,
        "DefaultOnly" Lude.=: defaultOnly,
        "Engine" Lude.=: engine,
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords
      ]

-- | Represents the output of a 'DescribeCacheEngineVersions' operation.
--
-- /See:/ 'mkDescribeCacheEngineVersionsResponse' smart constructor.
data DescribeCacheEngineVersionsResponse = DescribeCacheEngineVersionsResponse'
  { cacheEngineVersions ::
      Lude.Maybe
        [CacheEngineVersion],
    marker ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCacheEngineVersionsResponse' with the minimum fields required to make a request.
--
-- * 'cacheEngineVersions' - A list of cache engine version details. Each element in the list contains detailed information about one cache engine version.
-- * 'marker' - Provides an identifier to allow retrieval of paginated results.
-- * 'responseStatus' - The response status code.
mkDescribeCacheEngineVersionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeCacheEngineVersionsResponse
mkDescribeCacheEngineVersionsResponse pResponseStatus_ =
  DescribeCacheEngineVersionsResponse'
    { cacheEngineVersions =
        Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of cache engine version details. Each element in the list contains detailed information about one cache engine version.
--
-- /Note:/ Consider using 'cacheEngineVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcevrsCacheEngineVersions :: Lens.Lens' DescribeCacheEngineVersionsResponse (Lude.Maybe [CacheEngineVersion])
dcevrsCacheEngineVersions = Lens.lens (cacheEngineVersions :: DescribeCacheEngineVersionsResponse -> Lude.Maybe [CacheEngineVersion]) (\s a -> s {cacheEngineVersions = a} :: DescribeCacheEngineVersionsResponse)
{-# DEPRECATED dcevrsCacheEngineVersions "Use generic-lens or generic-optics with 'cacheEngineVersions' instead." #-}

-- | Provides an identifier to allow retrieval of paginated results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcevrsMarker :: Lens.Lens' DescribeCacheEngineVersionsResponse (Lude.Maybe Lude.Text)
dcevrsMarker = Lens.lens (marker :: DescribeCacheEngineVersionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeCacheEngineVersionsResponse)
{-# DEPRECATED dcevrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcevrsResponseStatus :: Lens.Lens' DescribeCacheEngineVersionsResponse Lude.Int
dcevrsResponseStatus = Lens.lens (responseStatus :: DescribeCacheEngineVersionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeCacheEngineVersionsResponse)
{-# DEPRECATED dcevrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
