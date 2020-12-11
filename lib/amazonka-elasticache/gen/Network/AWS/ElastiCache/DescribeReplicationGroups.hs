{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeReplicationGroups (..),
    mkDescribeReplicationGroups,

    -- ** Request lenses
    drgsMarker,
    drgsMaxRecords,
    drgsReplicationGroupId,

    -- * Destructuring the response
    DescribeReplicationGroupsResponse (..),
    mkDescribeReplicationGroupsResponse,

    -- ** Response lenses
    drgrsMarker,
    drgrsReplicationGroups,
    drgrsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @DescribeReplicationGroups@ operation.
--
-- /See:/ 'mkDescribeReplicationGroups' smart constructor.
data DescribeReplicationGroups = DescribeReplicationGroups'
  { marker ::
      Lude.Maybe Lude.Text,
    maxRecords :: Lude.Maybe Lude.Int,
    replicationGroupId ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeReplicationGroups' with the minimum fields required to make a request.
--
-- * 'marker' - An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: minimum 20; maximum 100.
-- * 'replicationGroupId' - The identifier for the replication group to be described. This parameter is not case sensitive.
--
-- If you do not specify this parameter, information about all replication groups is returned.
mkDescribeReplicationGroups ::
  DescribeReplicationGroups
mkDescribeReplicationGroups =
  DescribeReplicationGroups'
    { marker = Lude.Nothing,
      maxRecords = Lude.Nothing,
      replicationGroupId = Lude.Nothing
    }

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drgsMarker :: Lens.Lens' DescribeReplicationGroups (Lude.Maybe Lude.Text)
drgsMarker = Lens.lens (marker :: DescribeReplicationGroups -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeReplicationGroups)
{-# DEPRECATED drgsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: minimum 20; maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drgsMaxRecords :: Lens.Lens' DescribeReplicationGroups (Lude.Maybe Lude.Int)
drgsMaxRecords = Lens.lens (maxRecords :: DescribeReplicationGroups -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeReplicationGroups)
{-# DEPRECATED drgsMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The identifier for the replication group to be described. This parameter is not case sensitive.
--
-- If you do not specify this parameter, information about all replication groups is returned.
--
-- /Note:/ Consider using 'replicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drgsReplicationGroupId :: Lens.Lens' DescribeReplicationGroups (Lude.Maybe Lude.Text)
drgsReplicationGroupId = Lens.lens (replicationGroupId :: DescribeReplicationGroups -> Lude.Maybe Lude.Text) (\s a -> s {replicationGroupId = a} :: DescribeReplicationGroups)
{-# DEPRECATED drgsReplicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead." #-}

instance Page.AWSPager DescribeReplicationGroups where
  page rq rs
    | Page.stop (rs Lens.^. drgrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. drgrsReplicationGroups) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& drgsMarker Lens..~ rs Lens.^. drgrsMarker

instance Lude.AWSRequest DescribeReplicationGroups where
  type
    Rs DescribeReplicationGroups =
      DescribeReplicationGroupsResponse
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "DescribeReplicationGroupsResult"
      ( \s h x ->
          DescribeReplicationGroupsResponse'
            Lude.<$> (x Lude..@? "Marker")
            Lude.<*> ( x Lude..@? "ReplicationGroups" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "ReplicationGroup")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeReplicationGroups where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeReplicationGroups where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeReplicationGroups where
  toQuery DescribeReplicationGroups' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeReplicationGroups" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords,
        "ReplicationGroupId" Lude.=: replicationGroupId
      ]

-- | Represents the output of a @DescribeReplicationGroups@ operation.
--
-- /See:/ 'mkDescribeReplicationGroupsResponse' smart constructor.
data DescribeReplicationGroupsResponse = DescribeReplicationGroupsResponse'
  { marker ::
      Lude.Maybe Lude.Text,
    replicationGroups ::
      Lude.Maybe
        [ReplicationGroup],
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

-- | Creates a value of 'DescribeReplicationGroupsResponse' with the minimum fields required to make a request.
--
-- * 'marker' - Provides an identifier to allow retrieval of paginated results.
-- * 'replicationGroups' - A list of replication groups. Each item in the list contains detailed information about one replication group.
-- * 'responseStatus' - The response status code.
mkDescribeReplicationGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeReplicationGroupsResponse
mkDescribeReplicationGroupsResponse pResponseStatus_ =
  DescribeReplicationGroupsResponse'
    { marker = Lude.Nothing,
      replicationGroups = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Provides an identifier to allow retrieval of paginated results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drgrsMarker :: Lens.Lens' DescribeReplicationGroupsResponse (Lude.Maybe Lude.Text)
drgrsMarker = Lens.lens (marker :: DescribeReplicationGroupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeReplicationGroupsResponse)
{-# DEPRECATED drgrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A list of replication groups. Each item in the list contains detailed information about one replication group.
--
-- /Note:/ Consider using 'replicationGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drgrsReplicationGroups :: Lens.Lens' DescribeReplicationGroupsResponse (Lude.Maybe [ReplicationGroup])
drgrsReplicationGroups = Lens.lens (replicationGroups :: DescribeReplicationGroupsResponse -> Lude.Maybe [ReplicationGroup]) (\s a -> s {replicationGroups = a} :: DescribeReplicationGroupsResponse)
{-# DEPRECATED drgrsReplicationGroups "Use generic-lens or generic-optics with 'replicationGroups' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drgrsResponseStatus :: Lens.Lens' DescribeReplicationGroupsResponse Lude.Int
drgrsResponseStatus = Lens.lens (responseStatus :: DescribeReplicationGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeReplicationGroupsResponse)
{-# DEPRECATED drgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
