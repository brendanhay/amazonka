{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeExportTasks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a snapshot export to Amazon S3. This API operation supports pagination.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeExportTasks
  ( -- * Creating a request
    DescribeExportTasks (..),
    mkDescribeExportTasks,

    -- ** Request lenses
    detSourceARN,
    detFilters,
    detMarker,
    detExportTaskIdentifier,
    detMaxRecords,

    -- * Destructuring the response
    DescribeExportTasksResponse (..),
    mkDescribeExportTasksResponse,

    -- ** Response lenses
    detrsMarker,
    detrsExportTasks,
    detrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeExportTasks' smart constructor.
data DescribeExportTasks = DescribeExportTasks'
  { sourceARN ::
      Lude.Maybe Lude.Text,
    filters :: Lude.Maybe [Filter],
    marker :: Lude.Maybe Lude.Text,
    exportTaskIdentifier :: Lude.Maybe Lude.Text,
    maxRecords :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeExportTasks' with the minimum fields required to make a request.
--
-- * 'exportTaskIdentifier' - The identifier of the snapshot export task to be described.
-- * 'filters' - Filters specify one or more snapshot exports to describe. The filters are specified as name-value pairs that define what to include in the output. Filter names and values are case-sensitive.
--
-- Supported filters include the following:
--
--     * @export-task-identifier@ - An identifier for the snapshot export task.
--
--
--     * @s3-bucket@ - The Amazon S3 bucket the snapshot is exported to.
--
--
--     * @source-arn@ - The Amazon Resource Name (ARN) of the snapshot exported to Amazon S3
--
--
--     * @status@ - The status of the export task. Must be lowercase, for example, @complete@ .
--
--
-- * 'marker' - An optional pagination token provided by a previous @DescribeExportTasks@ request. If you specify this parameter, the response includes only records beyond the marker, up to the value specified by the @MaxRecords@ parameter.
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified value, a pagination token called a marker is included in the response. You can use the marker in a later @DescribeExportTasks@ request to retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
-- * 'sourceARN' - The Amazon Resource Name (ARN) of the snapshot exported to Amazon S3.
mkDescribeExportTasks ::
  DescribeExportTasks
mkDescribeExportTasks =
  DescribeExportTasks'
    { sourceARN = Lude.Nothing,
      filters = Lude.Nothing,
      marker = Lude.Nothing,
      exportTaskIdentifier = Lude.Nothing,
      maxRecords = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the snapshot exported to Amazon S3.
--
-- /Note:/ Consider using 'sourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detSourceARN :: Lens.Lens' DescribeExportTasks (Lude.Maybe Lude.Text)
detSourceARN = Lens.lens (sourceARN :: DescribeExportTasks -> Lude.Maybe Lude.Text) (\s a -> s {sourceARN = a} :: DescribeExportTasks)
{-# DEPRECATED detSourceARN "Use generic-lens or generic-optics with 'sourceARN' instead." #-}

-- | Filters specify one or more snapshot exports to describe. The filters are specified as name-value pairs that define what to include in the output. Filter names and values are case-sensitive.
--
-- Supported filters include the following:
--
--     * @export-task-identifier@ - An identifier for the snapshot export task.
--
--
--     * @s3-bucket@ - The Amazon S3 bucket the snapshot is exported to.
--
--
--     * @source-arn@ - The Amazon Resource Name (ARN) of the snapshot exported to Amazon S3
--
--
--     * @status@ - The status of the export task. Must be lowercase, for example, @complete@ .
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detFilters :: Lens.Lens' DescribeExportTasks (Lude.Maybe [Filter])
detFilters = Lens.lens (filters :: DescribeExportTasks -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeExportTasks)
{-# DEPRECATED detFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | An optional pagination token provided by a previous @DescribeExportTasks@ request. If you specify this parameter, the response includes only records beyond the marker, up to the value specified by the @MaxRecords@ parameter.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detMarker :: Lens.Lens' DescribeExportTasks (Lude.Maybe Lude.Text)
detMarker = Lens.lens (marker :: DescribeExportTasks -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeExportTasks)
{-# DEPRECATED detMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The identifier of the snapshot export task to be described.
--
-- /Note:/ Consider using 'exportTaskIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detExportTaskIdentifier :: Lens.Lens' DescribeExportTasks (Lude.Maybe Lude.Text)
detExportTaskIdentifier = Lens.lens (exportTaskIdentifier :: DescribeExportTasks -> Lude.Maybe Lude.Text) (\s a -> s {exportTaskIdentifier = a} :: DescribeExportTasks)
{-# DEPRECATED detExportTaskIdentifier "Use generic-lens or generic-optics with 'exportTaskIdentifier' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified value, a pagination token called a marker is included in the response. You can use the marker in a later @DescribeExportTasks@ request to retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detMaxRecords :: Lens.Lens' DescribeExportTasks (Lude.Maybe Lude.Natural)
detMaxRecords = Lens.lens (maxRecords :: DescribeExportTasks -> Lude.Maybe Lude.Natural) (\s a -> s {maxRecords = a} :: DescribeExportTasks)
{-# DEPRECATED detMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Page.AWSPager DescribeExportTasks where
  page rq rs
    | Page.stop (rs Lens.^. detrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. detrsExportTasks) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& detMarker Lens..~ rs Lens.^. detrsMarker

instance Lude.AWSRequest DescribeExportTasks where
  type Rs DescribeExportTasks = DescribeExportTasksResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "DescribeExportTasksResult"
      ( \s h x ->
          DescribeExportTasksResponse'
            Lude.<$> (x Lude..@? "Marker")
            Lude.<*> ( x Lude..@? "ExportTasks" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "ExportTask")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeExportTasks where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeExportTasks where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeExportTasks where
  toQuery DescribeExportTasks' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeExportTasks" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "SourceArn" Lude.=: sourceARN,
        "Filters"
          Lude.=: Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "Marker" Lude.=: marker,
        "ExportTaskIdentifier" Lude.=: exportTaskIdentifier,
        "MaxRecords" Lude.=: maxRecords
      ]

-- | /See:/ 'mkDescribeExportTasksResponse' smart constructor.
data DescribeExportTasksResponse = DescribeExportTasksResponse'
  { marker ::
      Lude.Maybe Lude.Text,
    exportTasks ::
      Lude.Maybe [ExportTask],
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

-- | Creates a value of 'DescribeExportTasksResponse' with the minimum fields required to make a request.
--
-- * 'exportTasks' - Information about an export of a snapshot to Amazon S3.
-- * 'marker' - A pagination token that can be used in a later @DescribeExportTasks@ request. A marker is used for pagination to identify the location to begin output for the next response of @DescribeExportTasks@ .
-- * 'responseStatus' - The response status code.
mkDescribeExportTasksResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeExportTasksResponse
mkDescribeExportTasksResponse pResponseStatus_ =
  DescribeExportTasksResponse'
    { marker = Lude.Nothing,
      exportTasks = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A pagination token that can be used in a later @DescribeExportTasks@ request. A marker is used for pagination to identify the location to begin output for the next response of @DescribeExportTasks@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detrsMarker :: Lens.Lens' DescribeExportTasksResponse (Lude.Maybe Lude.Text)
detrsMarker = Lens.lens (marker :: DescribeExportTasksResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeExportTasksResponse)
{-# DEPRECATED detrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Information about an export of a snapshot to Amazon S3.
--
-- /Note:/ Consider using 'exportTasks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detrsExportTasks :: Lens.Lens' DescribeExportTasksResponse (Lude.Maybe [ExportTask])
detrsExportTasks = Lens.lens (exportTasks :: DescribeExportTasksResponse -> Lude.Maybe [ExportTask]) (\s a -> s {exportTasks = a} :: DescribeExportTasksResponse)
{-# DEPRECATED detrsExportTasks "Use generic-lens or generic-optics with 'exportTasks' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detrsResponseStatus :: Lens.Lens' DescribeExportTasksResponse Lude.Int
detrsResponseStatus = Lens.lens (responseStatus :: DescribeExportTasksResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeExportTasksResponse)
{-# DEPRECATED detrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
