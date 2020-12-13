{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    drgMarker,
    drgMaxRecords,
    drgReplicationGroupId,

    -- * Destructuring the response
    DescribeReplicationGroupsResponse (..),
    mkDescribeReplicationGroupsResponse,

    -- ** Response lenses
    drgsrsMarker,
    drgsrsReplicationGroups,
    drgsrsResponseStatus,
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
  { -- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved.
    --
    -- Default: 100
    -- Constraints: minimum 20; maximum 100.
    maxRecords :: Lude.Maybe Lude.Int,
    -- | The identifier for the replication group to be described. This parameter is not case sensitive.
    --
    -- If you do not specify this parameter, information about all replication groups is returned.
    replicationGroupId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
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
drgMarker :: Lens.Lens' DescribeReplicationGroups (Lude.Maybe Lude.Text)
drgMarker = Lens.lens (marker :: DescribeReplicationGroups -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeReplicationGroups)
{-# DEPRECATED drgMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: minimum 20; maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drgMaxRecords :: Lens.Lens' DescribeReplicationGroups (Lude.Maybe Lude.Int)
drgMaxRecords = Lens.lens (maxRecords :: DescribeReplicationGroups -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeReplicationGroups)
{-# DEPRECATED drgMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The identifier for the replication group to be described. This parameter is not case sensitive.
--
-- If you do not specify this parameter, information about all replication groups is returned.
--
-- /Note:/ Consider using 'replicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drgReplicationGroupId :: Lens.Lens' DescribeReplicationGroups (Lude.Maybe Lude.Text)
drgReplicationGroupId = Lens.lens (replicationGroupId :: DescribeReplicationGroups -> Lude.Maybe Lude.Text) (\s a -> s {replicationGroupId = a} :: DescribeReplicationGroups)
{-# DEPRECATED drgReplicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead." #-}

instance Page.AWSPager DescribeReplicationGroups where
  page rq rs
    | Page.stop (rs Lens.^. drgsrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. drgsrsReplicationGroups) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& drgMarker Lens..~ rs Lens.^. drgsrsMarker

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
  { -- | Provides an identifier to allow retrieval of paginated results.
    marker :: Lude.Maybe Lude.Text,
    -- | A list of replication groups. Each item in the list contains detailed information about one replication group.
    replicationGroups :: Lude.Maybe [ReplicationGroup],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
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
drgsrsMarker :: Lens.Lens' DescribeReplicationGroupsResponse (Lude.Maybe Lude.Text)
drgsrsMarker = Lens.lens (marker :: DescribeReplicationGroupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeReplicationGroupsResponse)
{-# DEPRECATED drgsrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A list of replication groups. Each item in the list contains detailed information about one replication group.
--
-- /Note:/ Consider using 'replicationGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drgsrsReplicationGroups :: Lens.Lens' DescribeReplicationGroupsResponse (Lude.Maybe [ReplicationGroup])
drgsrsReplicationGroups = Lens.lens (replicationGroups :: DescribeReplicationGroupsResponse -> Lude.Maybe [ReplicationGroup]) (\s a -> s {replicationGroups = a} :: DescribeReplicationGroupsResponse)
{-# DEPRECATED drgsrsReplicationGroups "Use generic-lens or generic-optics with 'replicationGroups' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drgsrsResponseStatus :: Lens.Lens' DescribeReplicationGroupsResponse Lude.Int
drgsrsResponseStatus = Lens.lens (responseStatus :: DescribeReplicationGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeReplicationGroupsResponse)
{-# DEPRECATED drgsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
