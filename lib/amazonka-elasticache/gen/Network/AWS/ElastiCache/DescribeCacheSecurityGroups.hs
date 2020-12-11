{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DescribeCacheSecurityGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of cache security group descriptions. If a cache security group name is specified, the list contains only the description of that group. This applicable only when you have ElastiCache in Classic setup
--
-- This operation returns paginated results.
module Network.AWS.ElastiCache.DescribeCacheSecurityGroups
  ( -- * Creating a request
    DescribeCacheSecurityGroups (..),
    mkDescribeCacheSecurityGroups,

    -- ** Request lenses
    dcsgsCacheSecurityGroupName,
    dcsgsMarker,
    dcsgsMaxRecords,

    -- * Destructuring the response
    DescribeCacheSecurityGroupsResponse (..),
    mkDescribeCacheSecurityGroupsResponse,

    -- ** Response lenses
    dcsgsrsCacheSecurityGroups,
    dcsgsrsMarker,
    dcsgsrsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @DescribeCacheSecurityGroups@ operation.
--
-- /See:/ 'mkDescribeCacheSecurityGroups' smart constructor.
data DescribeCacheSecurityGroups = DescribeCacheSecurityGroups'
  { cacheSecurityGroupName ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DescribeCacheSecurityGroups' with the minimum fields required to make a request.
--
-- * 'cacheSecurityGroupName' - The name of the cache security group to return details for.
-- * 'marker' - An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: minimum 20; maximum 100.
mkDescribeCacheSecurityGroups ::
  DescribeCacheSecurityGroups
mkDescribeCacheSecurityGroups =
  DescribeCacheSecurityGroups'
    { cacheSecurityGroupName =
        Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing
    }

-- | The name of the cache security group to return details for.
--
-- /Note:/ Consider using 'cacheSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgsCacheSecurityGroupName :: Lens.Lens' DescribeCacheSecurityGroups (Lude.Maybe Lude.Text)
dcsgsCacheSecurityGroupName = Lens.lens (cacheSecurityGroupName :: DescribeCacheSecurityGroups -> Lude.Maybe Lude.Text) (\s a -> s {cacheSecurityGroupName = a} :: DescribeCacheSecurityGroups)
{-# DEPRECATED dcsgsCacheSecurityGroupName "Use generic-lens or generic-optics with 'cacheSecurityGroupName' instead." #-}

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgsMarker :: Lens.Lens' DescribeCacheSecurityGroups (Lude.Maybe Lude.Text)
dcsgsMarker = Lens.lens (marker :: DescribeCacheSecurityGroups -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeCacheSecurityGroups)
{-# DEPRECATED dcsgsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: minimum 20; maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgsMaxRecords :: Lens.Lens' DescribeCacheSecurityGroups (Lude.Maybe Lude.Int)
dcsgsMaxRecords = Lens.lens (maxRecords :: DescribeCacheSecurityGroups -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeCacheSecurityGroups)
{-# DEPRECATED dcsgsMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Page.AWSPager DescribeCacheSecurityGroups where
  page rq rs
    | Page.stop (rs Lens.^. dcsgsrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dcsgsrsCacheSecurityGroups) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dcsgsMarker Lens..~ rs Lens.^. dcsgsrsMarker

instance Lude.AWSRequest DescribeCacheSecurityGroups where
  type
    Rs DescribeCacheSecurityGroups =
      DescribeCacheSecurityGroupsResponse
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "DescribeCacheSecurityGroupsResult"
      ( \s h x ->
          DescribeCacheSecurityGroupsResponse'
            Lude.<$> ( x Lude..@? "CacheSecurityGroups" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "CacheSecurityGroup")
                     )
            Lude.<*> (x Lude..@? "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeCacheSecurityGroups where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeCacheSecurityGroups where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeCacheSecurityGroups where
  toQuery DescribeCacheSecurityGroups' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeCacheSecurityGroups" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "CacheSecurityGroupName" Lude.=: cacheSecurityGroupName,
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords
      ]

-- | Represents the output of a @DescribeCacheSecurityGroups@ operation.
--
-- /See:/ 'mkDescribeCacheSecurityGroupsResponse' smart constructor.
data DescribeCacheSecurityGroupsResponse = DescribeCacheSecurityGroupsResponse'
  { cacheSecurityGroups ::
      Lude.Maybe
        [CacheSecurityGroup],
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

-- | Creates a value of 'DescribeCacheSecurityGroupsResponse' with the minimum fields required to make a request.
--
-- * 'cacheSecurityGroups' - A list of cache security groups. Each element in the list contains detailed information about one group.
-- * 'marker' - Provides an identifier to allow retrieval of paginated results.
-- * 'responseStatus' - The response status code.
mkDescribeCacheSecurityGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeCacheSecurityGroupsResponse
mkDescribeCacheSecurityGroupsResponse pResponseStatus_ =
  DescribeCacheSecurityGroupsResponse'
    { cacheSecurityGroups =
        Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of cache security groups. Each element in the list contains detailed information about one group.
--
-- /Note:/ Consider using 'cacheSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgsrsCacheSecurityGroups :: Lens.Lens' DescribeCacheSecurityGroupsResponse (Lude.Maybe [CacheSecurityGroup])
dcsgsrsCacheSecurityGroups = Lens.lens (cacheSecurityGroups :: DescribeCacheSecurityGroupsResponse -> Lude.Maybe [CacheSecurityGroup]) (\s a -> s {cacheSecurityGroups = a} :: DescribeCacheSecurityGroupsResponse)
{-# DEPRECATED dcsgsrsCacheSecurityGroups "Use generic-lens or generic-optics with 'cacheSecurityGroups' instead." #-}

-- | Provides an identifier to allow retrieval of paginated results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgsrsMarker :: Lens.Lens' DescribeCacheSecurityGroupsResponse (Lude.Maybe Lude.Text)
dcsgsrsMarker = Lens.lens (marker :: DescribeCacheSecurityGroupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeCacheSecurityGroupsResponse)
{-# DEPRECATED dcsgsrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgsrsResponseStatus :: Lens.Lens' DescribeCacheSecurityGroupsResponse Lude.Int
dcsgsrsResponseStatus = Lens.lens (responseStatus :: DescribeCacheSecurityGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeCacheSecurityGroupsResponse)
{-# DEPRECATED dcsgsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
