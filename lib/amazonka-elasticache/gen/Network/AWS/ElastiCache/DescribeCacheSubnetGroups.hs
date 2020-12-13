{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeCacheSubnetGroups (..),
    mkDescribeCacheSubnetGroups,

    -- ** Request lenses
    dcsgCacheSubnetGroupName,
    dcsgMarker,
    dcsgMaxRecords,

    -- * Destructuring the response
    DescribeCacheSubnetGroupsResponse (..),
    mkDescribeCacheSubnetGroupsResponse,

    -- ** Response lenses
    dcsgsrsMarker,
    dcsgsrsCacheSubnetGroups,
    dcsgsrsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @DescribeCacheSubnetGroups@ operation.
--
-- /See:/ 'mkDescribeCacheSubnetGroups' smart constructor.
data DescribeCacheSubnetGroups = DescribeCacheSubnetGroups'
  { -- | The name of the cache subnet group to return details for.
    cacheSubnetGroupName :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DescribeCacheSubnetGroups' with the minimum fields required to make a request.
--
-- * 'cacheSubnetGroupName' - The name of the cache subnet group to return details for.
-- * 'marker' - An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: minimum 20; maximum 100.
mkDescribeCacheSubnetGroups ::
  DescribeCacheSubnetGroups
mkDescribeCacheSubnetGroups =
  DescribeCacheSubnetGroups'
    { cacheSubnetGroupName = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing
    }

-- | The name of the cache subnet group to return details for.
--
-- /Note:/ Consider using 'cacheSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgCacheSubnetGroupName :: Lens.Lens' DescribeCacheSubnetGroups (Lude.Maybe Lude.Text)
dcsgCacheSubnetGroupName = Lens.lens (cacheSubnetGroupName :: DescribeCacheSubnetGroups -> Lude.Maybe Lude.Text) (\s a -> s {cacheSubnetGroupName = a} :: DescribeCacheSubnetGroups)
{-# DEPRECATED dcsgCacheSubnetGroupName "Use generic-lens or generic-optics with 'cacheSubnetGroupName' instead." #-}

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgMarker :: Lens.Lens' DescribeCacheSubnetGroups (Lude.Maybe Lude.Text)
dcsgMarker = Lens.lens (marker :: DescribeCacheSubnetGroups -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeCacheSubnetGroups)
{-# DEPRECATED dcsgMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: minimum 20; maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgMaxRecords :: Lens.Lens' DescribeCacheSubnetGroups (Lude.Maybe Lude.Int)
dcsgMaxRecords = Lens.lens (maxRecords :: DescribeCacheSubnetGroups -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeCacheSubnetGroups)
{-# DEPRECATED dcsgMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Page.AWSPager DescribeCacheSubnetGroups where
  page rq rs
    | Page.stop (rs Lens.^. dcsgsrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dcsgsrsCacheSubnetGroups) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dcsgMarker Lens..~ rs Lens.^. dcsgsrsMarker

instance Lude.AWSRequest DescribeCacheSubnetGroups where
  type
    Rs DescribeCacheSubnetGroups =
      DescribeCacheSubnetGroupsResponse
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "DescribeCacheSubnetGroupsResult"
      ( \s h x ->
          DescribeCacheSubnetGroupsResponse'
            Lude.<$> (x Lude..@? "Marker")
            Lude.<*> ( x Lude..@? "CacheSubnetGroups" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "CacheSubnetGroup")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeCacheSubnetGroups where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeCacheSubnetGroups where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeCacheSubnetGroups where
  toQuery DescribeCacheSubnetGroups' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeCacheSubnetGroups" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "CacheSubnetGroupName" Lude.=: cacheSubnetGroupName,
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords
      ]

-- | Represents the output of a @DescribeCacheSubnetGroups@ operation.
--
-- /See:/ 'mkDescribeCacheSubnetGroupsResponse' smart constructor.
data DescribeCacheSubnetGroupsResponse = DescribeCacheSubnetGroupsResponse'
  { -- | Provides an identifier to allow retrieval of paginated results.
    marker :: Lude.Maybe Lude.Text,
    -- | A list of cache subnet groups. Each element in the list contains detailed information about one group.
    cacheSubnetGroups :: Lude.Maybe [CacheSubnetGroup],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCacheSubnetGroupsResponse' with the minimum fields required to make a request.
--
-- * 'marker' - Provides an identifier to allow retrieval of paginated results.
-- * 'cacheSubnetGroups' - A list of cache subnet groups. Each element in the list contains detailed information about one group.
-- * 'responseStatus' - The response status code.
mkDescribeCacheSubnetGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeCacheSubnetGroupsResponse
mkDescribeCacheSubnetGroupsResponse pResponseStatus_ =
  DescribeCacheSubnetGroupsResponse'
    { marker = Lude.Nothing,
      cacheSubnetGroups = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Provides an identifier to allow retrieval of paginated results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgsrsMarker :: Lens.Lens' DescribeCacheSubnetGroupsResponse (Lude.Maybe Lude.Text)
dcsgsrsMarker = Lens.lens (marker :: DescribeCacheSubnetGroupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeCacheSubnetGroupsResponse)
{-# DEPRECATED dcsgsrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A list of cache subnet groups. Each element in the list contains detailed information about one group.
--
-- /Note:/ Consider using 'cacheSubnetGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgsrsCacheSubnetGroups :: Lens.Lens' DescribeCacheSubnetGroupsResponse (Lude.Maybe [CacheSubnetGroup])
dcsgsrsCacheSubnetGroups = Lens.lens (cacheSubnetGroups :: DescribeCacheSubnetGroupsResponse -> Lude.Maybe [CacheSubnetGroup]) (\s a -> s {cacheSubnetGroups = a} :: DescribeCacheSubnetGroupsResponse)
{-# DEPRECATED dcsgsrsCacheSubnetGroups "Use generic-lens or generic-optics with 'cacheSubnetGroups' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgsrsResponseStatus :: Lens.Lens' DescribeCacheSubnetGroupsResponse Lude.Int
dcsgsrsResponseStatus = Lens.lens (responseStatus :: DescribeCacheSubnetGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeCacheSubnetGroupsResponse)
{-# DEPRECATED dcsgsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
