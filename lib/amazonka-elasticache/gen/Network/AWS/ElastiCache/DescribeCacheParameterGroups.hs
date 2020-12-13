{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeCacheParameterGroups (..),
    mkDescribeCacheParameterGroups,

    -- ** Request lenses
    dcpgCacheParameterGroupName,
    dcpgMarker,
    dcpgMaxRecords,

    -- * Destructuring the response
    DescribeCacheParameterGroupsResponse (..),
    mkDescribeCacheParameterGroupsResponse,

    -- ** Response lenses
    dcpgrsCacheParameterGroups,
    dcpgrsMarker,
    dcpgrsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @DescribeCacheParameterGroups@ operation.
--
-- /See:/ 'mkDescribeCacheParameterGroups' smart constructor.
data DescribeCacheParameterGroups = DescribeCacheParameterGroups'
  { -- | The name of a specific cache parameter group to return details for.
    cacheParameterGroupName :: Lude.Maybe Lude.Text,
    -- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved.
    --
    -- Default: 100
    -- Constraints: minimum 20; maximum 100.
    maxRecords :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCacheParameterGroups' with the minimum fields required to make a request.
--
-- * 'cacheParameterGroupName' - The name of a specific cache parameter group to return details for.
-- * 'marker' - An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: minimum 20; maximum 100.
mkDescribeCacheParameterGroups ::
  DescribeCacheParameterGroups
mkDescribeCacheParameterGroups =
  DescribeCacheParameterGroups'
    { cacheParameterGroupName =
        Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing
    }

-- | The name of a specific cache parameter group to return details for.
--
-- /Note:/ Consider using 'cacheParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpgCacheParameterGroupName :: Lens.Lens' DescribeCacheParameterGroups (Lude.Maybe Lude.Text)
dcpgCacheParameterGroupName = Lens.lens (cacheParameterGroupName :: DescribeCacheParameterGroups -> Lude.Maybe Lude.Text) (\s a -> s {cacheParameterGroupName = a} :: DescribeCacheParameterGroups)
{-# DEPRECATED dcpgCacheParameterGroupName "Use generic-lens or generic-optics with 'cacheParameterGroupName' instead." #-}

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpgMarker :: Lens.Lens' DescribeCacheParameterGroups (Lude.Maybe Lude.Text)
dcpgMarker = Lens.lens (marker :: DescribeCacheParameterGroups -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeCacheParameterGroups)
{-# DEPRECATED dcpgMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: minimum 20; maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpgMaxRecords :: Lens.Lens' DescribeCacheParameterGroups (Lude.Maybe Lude.Int)
dcpgMaxRecords = Lens.lens (maxRecords :: DescribeCacheParameterGroups -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeCacheParameterGroups)
{-# DEPRECATED dcpgMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Page.AWSPager DescribeCacheParameterGroups where
  page rq rs
    | Page.stop (rs Lens.^. dcpgrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dcpgrsCacheParameterGroups) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dcpgMarker Lens..~ rs Lens.^. dcpgrsMarker

instance Lude.AWSRequest DescribeCacheParameterGroups where
  type
    Rs DescribeCacheParameterGroups =
      DescribeCacheParameterGroupsResponse
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "DescribeCacheParameterGroupsResult"
      ( \s h x ->
          DescribeCacheParameterGroupsResponse'
            Lude.<$> ( x Lude..@? "CacheParameterGroups" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "CacheParameterGroup")
                     )
            Lude.<*> (x Lude..@? "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeCacheParameterGroups where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeCacheParameterGroups where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeCacheParameterGroups where
  toQuery DescribeCacheParameterGroups' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeCacheParameterGroups" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "CacheParameterGroupName" Lude.=: cacheParameterGroupName,
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords
      ]

-- | Represents the output of a @DescribeCacheParameterGroups@ operation.
--
-- /See:/ 'mkDescribeCacheParameterGroupsResponse' smart constructor.
data DescribeCacheParameterGroupsResponse = DescribeCacheParameterGroupsResponse'
  { -- | A list of cache parameter groups. Each element in the list contains detailed information about one cache parameter group.
    cacheParameterGroups :: Lude.Maybe [CacheParameterGroup],
    -- | Provides an identifier to allow retrieval of paginated results.
    marker :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCacheParameterGroupsResponse' with the minimum fields required to make a request.
--
-- * 'cacheParameterGroups' - A list of cache parameter groups. Each element in the list contains detailed information about one cache parameter group.
-- * 'marker' - Provides an identifier to allow retrieval of paginated results.
-- * 'responseStatus' - The response status code.
mkDescribeCacheParameterGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeCacheParameterGroupsResponse
mkDescribeCacheParameterGroupsResponse pResponseStatus_ =
  DescribeCacheParameterGroupsResponse'
    { cacheParameterGroups =
        Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of cache parameter groups. Each element in the list contains detailed information about one cache parameter group.
--
-- /Note:/ Consider using 'cacheParameterGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpgrsCacheParameterGroups :: Lens.Lens' DescribeCacheParameterGroupsResponse (Lude.Maybe [CacheParameterGroup])
dcpgrsCacheParameterGroups = Lens.lens (cacheParameterGroups :: DescribeCacheParameterGroupsResponse -> Lude.Maybe [CacheParameterGroup]) (\s a -> s {cacheParameterGroups = a} :: DescribeCacheParameterGroupsResponse)
{-# DEPRECATED dcpgrsCacheParameterGroups "Use generic-lens or generic-optics with 'cacheParameterGroups' instead." #-}

-- | Provides an identifier to allow retrieval of paginated results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpgrsMarker :: Lens.Lens' DescribeCacheParameterGroupsResponse (Lude.Maybe Lude.Text)
dcpgrsMarker = Lens.lens (marker :: DescribeCacheParameterGroupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeCacheParameterGroupsResponse)
{-# DEPRECATED dcpgrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpgrsResponseStatus :: Lens.Lens' DescribeCacheParameterGroupsResponse Lude.Int
dcpgrsResponseStatus = Lens.lens (responseStatus :: DescribeCacheParameterGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeCacheParameterGroupsResponse)
{-# DEPRECATED dcpgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
