{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DescribeReplicationTasks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about replication tasks for your account in the current region.
--
-- This operation returns paginated results.
module Network.AWS.DMS.DescribeReplicationTasks
  ( -- * Creating a request
    DescribeReplicationTasks (..),
    mkDescribeReplicationTasks,

    -- ** Request lenses
    drtFilters,
    drtWithoutSettings,
    drtMarker,
    drtMaxRecords,

    -- * Destructuring the response
    DescribeReplicationTasksResponse (..),
    mkDescribeReplicationTasksResponse,

    -- ** Response lenses
    drtsrsReplicationTasks,
    drtsrsMarker,
    drtsrsResponseStatus,
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
-- /See:/ 'mkDescribeReplicationTasks' smart constructor.
data DescribeReplicationTasks = DescribeReplicationTasks'
  { filters ::
      Lude.Maybe [Filter],
    withoutSettings :: Lude.Maybe Lude.Bool,
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

-- | Creates a value of 'DescribeReplicationTasks' with the minimum fields required to make a request.
--
-- * 'filters' - Filters applied to replication tasks.
--
-- Valid filter names: replication-task-arn | replication-task-id | migration-type | endpoint-arn | replication-instance-arn
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
-- * 'withoutSettings' - An option to set to avoid returning information about settings. Use this to reduce overhead when setting information is too large. To use this option, choose @true@ ; otherwise, choose @false@ (the default).
mkDescribeReplicationTasks ::
  DescribeReplicationTasks
mkDescribeReplicationTasks =
  DescribeReplicationTasks'
    { filters = Lude.Nothing,
      withoutSettings = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing
    }

-- | Filters applied to replication tasks.
--
-- Valid filter names: replication-task-arn | replication-task-id | migration-type | endpoint-arn | replication-instance-arn
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtFilters :: Lens.Lens' DescribeReplicationTasks (Lude.Maybe [Filter])
drtFilters = Lens.lens (filters :: DescribeReplicationTasks -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeReplicationTasks)
{-# DEPRECATED drtFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | An option to set to avoid returning information about settings. Use this to reduce overhead when setting information is too large. To use this option, choose @true@ ; otherwise, choose @false@ (the default).
--
-- /Note:/ Consider using 'withoutSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtWithoutSettings :: Lens.Lens' DescribeReplicationTasks (Lude.Maybe Lude.Bool)
drtWithoutSettings = Lens.lens (withoutSettings :: DescribeReplicationTasks -> Lude.Maybe Lude.Bool) (\s a -> s {withoutSettings = a} :: DescribeReplicationTasks)
{-# DEPRECATED drtWithoutSettings "Use generic-lens or generic-optics with 'withoutSettings' instead." #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtMarker :: Lens.Lens' DescribeReplicationTasks (Lude.Maybe Lude.Text)
drtMarker = Lens.lens (marker :: DescribeReplicationTasks -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeReplicationTasks)
{-# DEPRECATED drtMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtMaxRecords :: Lens.Lens' DescribeReplicationTasks (Lude.Maybe Lude.Int)
drtMaxRecords = Lens.lens (maxRecords :: DescribeReplicationTasks -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeReplicationTasks)
{-# DEPRECATED drtMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Page.AWSPager DescribeReplicationTasks where
  page rq rs
    | Page.stop (rs Lens.^. drtsrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. drtsrsReplicationTasks) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& drtMarker Lens..~ rs Lens.^. drtsrsMarker

instance Lude.AWSRequest DescribeReplicationTasks where
  type Rs DescribeReplicationTasks = DescribeReplicationTasksResponse
  request = Req.postJSON dmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeReplicationTasksResponse'
            Lude.<$> (x Lude..?> "ReplicationTasks" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeReplicationTasks where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonDMSv20160101.DescribeReplicationTasks" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeReplicationTasks where
  toJSON DescribeReplicationTasks' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            ("WithoutSettings" Lude..=) Lude.<$> withoutSettings,
            ("Marker" Lude..=) Lude.<$> marker,
            ("MaxRecords" Lude..=) Lude.<$> maxRecords
          ]
      )

instance Lude.ToPath DescribeReplicationTasks where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeReplicationTasks where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkDescribeReplicationTasksResponse' smart constructor.
data DescribeReplicationTasksResponse = DescribeReplicationTasksResponse'
  { replicationTasks ::
      Lude.Maybe
        [ReplicationTask],
    marker ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DescribeReplicationTasksResponse' with the minimum fields required to make a request.
--
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'replicationTasks' - A description of the replication tasks.
-- * 'responseStatus' - The response status code.
mkDescribeReplicationTasksResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeReplicationTasksResponse
mkDescribeReplicationTasksResponse pResponseStatus_ =
  DescribeReplicationTasksResponse'
    { replicationTasks =
        Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A description of the replication tasks.
--
-- /Note:/ Consider using 'replicationTasks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtsrsReplicationTasks :: Lens.Lens' DescribeReplicationTasksResponse (Lude.Maybe [ReplicationTask])
drtsrsReplicationTasks = Lens.lens (replicationTasks :: DescribeReplicationTasksResponse -> Lude.Maybe [ReplicationTask]) (\s a -> s {replicationTasks = a} :: DescribeReplicationTasksResponse)
{-# DEPRECATED drtsrsReplicationTasks "Use generic-lens or generic-optics with 'replicationTasks' instead." #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtsrsMarker :: Lens.Lens' DescribeReplicationTasksResponse (Lude.Maybe Lude.Text)
drtsrsMarker = Lens.lens (marker :: DescribeReplicationTasksResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeReplicationTasksResponse)
{-# DEPRECATED drtsrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtsrsResponseStatus :: Lens.Lens' DescribeReplicationTasksResponse Lude.Int
drtsrsResponseStatus = Lens.lens (responseStatus :: DescribeReplicationTasksResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeReplicationTasksResponse)
{-# DEPRECATED drtsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
