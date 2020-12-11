{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DescribeReplicationInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about replication instances for your account in the current region.
--
-- This operation returns paginated results.
module Network.AWS.DMS.DescribeReplicationInstances
  ( -- * Creating a request
    DescribeReplicationInstances (..),
    mkDescribeReplicationInstances,

    -- ** Request lenses
    driFilters,
    driMarker,
    driMaxRecords,

    -- * Destructuring the response
    DescribeReplicationInstancesResponse (..),
    mkDescribeReplicationInstancesResponse,

    -- ** Response lenses
    drisrsMarker,
    drisrsReplicationInstances,
    drisrsResponseStatus,
  )
where

import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDescribeReplicationInstances' smart constructor.
data DescribeReplicationInstances = DescribeReplicationInstances'
  { filters ::
      Lude.Maybe [Filter],
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

-- | Creates a value of 'DescribeReplicationInstances' with the minimum fields required to make a request.
--
-- * 'filters' - Filters applied to replication instances.
--
-- Valid filter names: replication-instance-arn | replication-instance-id | replication-instance-class | engine-version
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
mkDescribeReplicationInstances ::
  DescribeReplicationInstances
mkDescribeReplicationInstances =
  DescribeReplicationInstances'
    { filters = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing
    }

-- | Filters applied to replication instances.
--
-- Valid filter names: replication-instance-arn | replication-instance-id | replication-instance-class | engine-version
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
driFilters :: Lens.Lens' DescribeReplicationInstances (Lude.Maybe [Filter])
driFilters = Lens.lens (filters :: DescribeReplicationInstances -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeReplicationInstances)
{-# DEPRECATED driFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
driMarker :: Lens.Lens' DescribeReplicationInstances (Lude.Maybe Lude.Text)
driMarker = Lens.lens (marker :: DescribeReplicationInstances -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeReplicationInstances)
{-# DEPRECATED driMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
driMaxRecords :: Lens.Lens' DescribeReplicationInstances (Lude.Maybe Lude.Int)
driMaxRecords = Lens.lens (maxRecords :: DescribeReplicationInstances -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeReplicationInstances)
{-# DEPRECATED driMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Page.AWSPager DescribeReplicationInstances where
  page rq rs
    | Page.stop (rs Lens.^. drisrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. drisrsReplicationInstances) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& driMarker Lens..~ rs Lens.^. drisrsMarker

instance Lude.AWSRequest DescribeReplicationInstances where
  type
    Rs DescribeReplicationInstances =
      DescribeReplicationInstancesResponse
  request = Req.postJSON dmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeReplicationInstancesResponse'
            Lude.<$> (x Lude..?> "Marker")
            Lude.<*> (x Lude..?> "ReplicationInstances" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeReplicationInstances where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonDMSv20160101.DescribeReplicationInstances" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeReplicationInstances where
  toJSON DescribeReplicationInstances' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            ("Marker" Lude..=) Lude.<$> marker,
            ("MaxRecords" Lude..=) Lude.<$> maxRecords
          ]
      )

instance Lude.ToPath DescribeReplicationInstances where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeReplicationInstances where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkDescribeReplicationInstancesResponse' smart constructor.
data DescribeReplicationInstancesResponse = DescribeReplicationInstancesResponse'
  { marker ::
      Lude.Maybe
        Lude.Text,
    replicationInstances ::
      Lude.Maybe
        [ReplicationInstance],
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

-- | Creates a value of 'DescribeReplicationInstancesResponse' with the minimum fields required to make a request.
--
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'replicationInstances' - The replication instances described.
-- * 'responseStatus' - The response status code.
mkDescribeReplicationInstancesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeReplicationInstancesResponse
mkDescribeReplicationInstancesResponse pResponseStatus_ =
  DescribeReplicationInstancesResponse'
    { marker = Lude.Nothing,
      replicationInstances = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drisrsMarker :: Lens.Lens' DescribeReplicationInstancesResponse (Lude.Maybe Lude.Text)
drisrsMarker = Lens.lens (marker :: DescribeReplicationInstancesResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeReplicationInstancesResponse)
{-# DEPRECATED drisrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The replication instances described.
--
-- /Note:/ Consider using 'replicationInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drisrsReplicationInstances :: Lens.Lens' DescribeReplicationInstancesResponse (Lude.Maybe [ReplicationInstance])
drisrsReplicationInstances = Lens.lens (replicationInstances :: DescribeReplicationInstancesResponse -> Lude.Maybe [ReplicationInstance]) (\s a -> s {replicationInstances = a} :: DescribeReplicationInstancesResponse)
{-# DEPRECATED drisrsReplicationInstances "Use generic-lens or generic-optics with 'replicationInstances' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drisrsResponseStatus :: Lens.Lens' DescribeReplicationInstancesResponse Lude.Int
drisrsResponseStatus = Lens.lens (responseStatus :: DescribeReplicationInstancesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeReplicationInstancesResponse)
{-# DEPRECATED drisrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
