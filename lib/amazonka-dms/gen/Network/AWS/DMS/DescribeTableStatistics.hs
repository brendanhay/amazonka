{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DescribeTableStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns table statistics on the database migration task, including table name, rows inserted, rows updated, and rows deleted.
--
-- Note that the "last updated" column the DMS console only indicates the time that AWS DMS last updated the table statistics record for a table. It does not indicate the time of the last update to the table.
--
-- This operation returns paginated results.
module Network.AWS.DMS.DescribeTableStatistics
  ( -- * Creating a request
    DescribeTableStatistics (..),
    mkDescribeTableStatistics,

    -- ** Request lenses
    dtsFilters,
    dtsMarker,
    dtsMaxRecords,
    dtsReplicationTaskARN,

    -- * Destructuring the response
    DescribeTableStatisticsResponse (..),
    mkDescribeTableStatisticsResponse,

    -- ** Response lenses
    dtsrsReplicationTaskARN,
    dtsrsMarker,
    dtsrsTableStatistics,
    dtsrsResponseStatus,
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
-- /See:/ 'mkDescribeTableStatistics' smart constructor.
data DescribeTableStatistics = DescribeTableStatistics'
  { filters ::
      Lude.Maybe [Filter],
    marker :: Lude.Maybe Lude.Text,
    maxRecords :: Lude.Maybe Lude.Int,
    replicationTaskARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTableStatistics' with the minimum fields required to make a request.
--
-- * 'filters' - Filters applied to table statistics.
--
-- Valid filter names: schema-name | table-name | table-state
-- A combination of filters creates an AND condition where each record matches all specified filters.
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 500.
-- * 'replicationTaskARN' - The Amazon Resource Name (ARN) of the replication task.
mkDescribeTableStatistics ::
  -- | 'replicationTaskARN'
  Lude.Text ->
  DescribeTableStatistics
mkDescribeTableStatistics pReplicationTaskARN_ =
  DescribeTableStatistics'
    { filters = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing,
      replicationTaskARN = pReplicationTaskARN_
    }

-- | Filters applied to table statistics.
--
-- Valid filter names: schema-name | table-name | table-state
-- A combination of filters creates an AND condition where each record matches all specified filters.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsFilters :: Lens.Lens' DescribeTableStatistics (Lude.Maybe [Filter])
dtsFilters = Lens.lens (filters :: DescribeTableStatistics -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeTableStatistics)
{-# DEPRECATED dtsFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsMarker :: Lens.Lens' DescribeTableStatistics (Lude.Maybe Lude.Text)
dtsMarker = Lens.lens (marker :: DescribeTableStatistics -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeTableStatistics)
{-# DEPRECATED dtsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 500.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsMaxRecords :: Lens.Lens' DescribeTableStatistics (Lude.Maybe Lude.Int)
dtsMaxRecords = Lens.lens (maxRecords :: DescribeTableStatistics -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeTableStatistics)
{-# DEPRECATED dtsMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The Amazon Resource Name (ARN) of the replication task.
--
-- /Note:/ Consider using 'replicationTaskARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsReplicationTaskARN :: Lens.Lens' DescribeTableStatistics Lude.Text
dtsReplicationTaskARN = Lens.lens (replicationTaskARN :: DescribeTableStatistics -> Lude.Text) (\s a -> s {replicationTaskARN = a} :: DescribeTableStatistics)
{-# DEPRECATED dtsReplicationTaskARN "Use generic-lens or generic-optics with 'replicationTaskARN' instead." #-}

instance Page.AWSPager DescribeTableStatistics where
  page rq rs
    | Page.stop (rs Lens.^. dtsrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dtsrsTableStatistics) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& dtsMarker Lens..~ rs Lens.^. dtsrsMarker

instance Lude.AWSRequest DescribeTableStatistics where
  type Rs DescribeTableStatistics = DescribeTableStatisticsResponse
  request = Req.postJSON dmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeTableStatisticsResponse'
            Lude.<$> (x Lude..?> "ReplicationTaskArn")
            Lude.<*> (x Lude..?> "Marker")
            Lude.<*> (x Lude..?> "TableStatistics" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeTableStatistics where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonDMSv20160101.DescribeTableStatistics" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeTableStatistics where
  toJSON DescribeTableStatistics' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            ("Marker" Lude..=) Lude.<$> marker,
            ("MaxRecords" Lude..=) Lude.<$> maxRecords,
            Lude.Just ("ReplicationTaskArn" Lude..= replicationTaskARN)
          ]
      )

instance Lude.ToPath DescribeTableStatistics where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeTableStatistics where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkDescribeTableStatisticsResponse' smart constructor.
data DescribeTableStatisticsResponse = DescribeTableStatisticsResponse'
  { replicationTaskARN ::
      Lude.Maybe Lude.Text,
    marker ::
      Lude.Maybe Lude.Text,
    tableStatistics ::
      Lude.Maybe
        [TableStatistics],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTableStatisticsResponse' with the minimum fields required to make a request.
--
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'replicationTaskARN' - The Amazon Resource Name (ARN) of the replication task.
-- * 'responseStatus' - The response status code.
-- * 'tableStatistics' - The table statistics.
mkDescribeTableStatisticsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeTableStatisticsResponse
mkDescribeTableStatisticsResponse pResponseStatus_ =
  DescribeTableStatisticsResponse'
    { replicationTaskARN =
        Lude.Nothing,
      marker = Lude.Nothing,
      tableStatistics = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the replication task.
--
-- /Note:/ Consider using 'replicationTaskARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsrsReplicationTaskARN :: Lens.Lens' DescribeTableStatisticsResponse (Lude.Maybe Lude.Text)
dtsrsReplicationTaskARN = Lens.lens (replicationTaskARN :: DescribeTableStatisticsResponse -> Lude.Maybe Lude.Text) (\s a -> s {replicationTaskARN = a} :: DescribeTableStatisticsResponse)
{-# DEPRECATED dtsrsReplicationTaskARN "Use generic-lens or generic-optics with 'replicationTaskARN' instead." #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsrsMarker :: Lens.Lens' DescribeTableStatisticsResponse (Lude.Maybe Lude.Text)
dtsrsMarker = Lens.lens (marker :: DescribeTableStatisticsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeTableStatisticsResponse)
{-# DEPRECATED dtsrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The table statistics.
--
-- /Note:/ Consider using 'tableStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsrsTableStatistics :: Lens.Lens' DescribeTableStatisticsResponse (Lude.Maybe [TableStatistics])
dtsrsTableStatistics = Lens.lens (tableStatistics :: DescribeTableStatisticsResponse -> Lude.Maybe [TableStatistics]) (\s a -> s {tableStatistics = a} :: DescribeTableStatisticsResponse)
{-# DEPRECATED dtsrsTableStatistics "Use generic-lens or generic-optics with 'tableStatistics' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsrsResponseStatus :: Lens.Lens' DescribeTableStatisticsResponse Lude.Int
dtsrsResponseStatus = Lens.lens (responseStatus :: DescribeTableStatisticsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeTableStatisticsResponse)
{-# DEPRECATED dtsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
