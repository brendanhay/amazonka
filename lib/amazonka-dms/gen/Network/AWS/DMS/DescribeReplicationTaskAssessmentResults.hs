{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DescribeReplicationTaskAssessmentResults
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the task assessment results from Amazon S3. This action always returns the latest results.
--
-- This operation returns paginated results.
module Network.AWS.DMS.DescribeReplicationTaskAssessmentResults
  ( -- * Creating a request
    DescribeReplicationTaskAssessmentResults (..),
    mkDescribeReplicationTaskAssessmentResults,

    -- ** Request lenses
    drtarReplicationTaskARN,
    drtarMarker,
    drtarMaxRecords,

    -- * Destructuring the response
    DescribeReplicationTaskAssessmentResultsResponse (..),
    mkDescribeReplicationTaskAssessmentResultsResponse,

    -- ** Response lenses
    drtarfrsBucketName,
    drtarfrsMarker,
    drtarfrsReplicationTaskAssessmentResults,
    drtarfrsResponseStatus,
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
-- /See:/ 'mkDescribeReplicationTaskAssessmentResults' smart constructor.
data DescribeReplicationTaskAssessmentResults = DescribeReplicationTaskAssessmentResults'
  { -- | The Amazon Resource Name (ARN) string that uniquely identifies the task. When this input parameter is specified, the API returns only one result and ignore the values of the @MaxRecords@ and @Marker@ parameters.
    replicationTaskARN :: Lude.Maybe Lude.Text,
    -- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
    --
    -- Default: 100
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeReplicationTaskAssessmentResults' with the minimum fields required to make a request.
--
-- * 'replicationTaskARN' - The Amazon Resource Name (ARN) string that uniquely identifies the task. When this input parameter is specified, the API returns only one result and ignore the values of the @MaxRecords@ and @Marker@ parameters.
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
mkDescribeReplicationTaskAssessmentResults ::
  DescribeReplicationTaskAssessmentResults
mkDescribeReplicationTaskAssessmentResults =
  DescribeReplicationTaskAssessmentResults'
    { replicationTaskARN =
        Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) string that uniquely identifies the task. When this input parameter is specified, the API returns only one result and ignore the values of the @MaxRecords@ and @Marker@ parameters.
--
-- /Note:/ Consider using 'replicationTaskARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtarReplicationTaskARN :: Lens.Lens' DescribeReplicationTaskAssessmentResults (Lude.Maybe Lude.Text)
drtarReplicationTaskARN = Lens.lens (replicationTaskARN :: DescribeReplicationTaskAssessmentResults -> Lude.Maybe Lude.Text) (\s a -> s {replicationTaskARN = a} :: DescribeReplicationTaskAssessmentResults)
{-# DEPRECATED drtarReplicationTaskARN "Use generic-lens or generic-optics with 'replicationTaskARN' instead." #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtarMarker :: Lens.Lens' DescribeReplicationTaskAssessmentResults (Lude.Maybe Lude.Text)
drtarMarker = Lens.lens (marker :: DescribeReplicationTaskAssessmentResults -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeReplicationTaskAssessmentResults)
{-# DEPRECATED drtarMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtarMaxRecords :: Lens.Lens' DescribeReplicationTaskAssessmentResults (Lude.Maybe Lude.Int)
drtarMaxRecords = Lens.lens (maxRecords :: DescribeReplicationTaskAssessmentResults -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeReplicationTaskAssessmentResults)
{-# DEPRECATED drtarMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Page.AWSPager DescribeReplicationTaskAssessmentResults where
  page rq rs
    | Page.stop (rs Lens.^. drtarfrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. drtarfrsReplicationTaskAssessmentResults) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& drtarMarker Lens..~ rs Lens.^. drtarfrsMarker

instance Lude.AWSRequest DescribeReplicationTaskAssessmentResults where
  type
    Rs DescribeReplicationTaskAssessmentResults =
      DescribeReplicationTaskAssessmentResultsResponse
  request = Req.postJSON dmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeReplicationTaskAssessmentResultsResponse'
            Lude.<$> (x Lude..?> "BucketName")
            Lude.<*> (x Lude..?> "Marker")
            Lude.<*> ( x Lude..?> "ReplicationTaskAssessmentResults"
                         Lude..!@ Lude.mempty
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeReplicationTaskAssessmentResults where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonDMSv20160101.DescribeReplicationTaskAssessmentResults" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeReplicationTaskAssessmentResults where
  toJSON DescribeReplicationTaskAssessmentResults' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ReplicationTaskArn" Lude..=) Lude.<$> replicationTaskARN,
            ("Marker" Lude..=) Lude.<$> marker,
            ("MaxRecords" Lude..=) Lude.<$> maxRecords
          ]
      )

instance Lude.ToPath DescribeReplicationTaskAssessmentResults where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeReplicationTaskAssessmentResults where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkDescribeReplicationTaskAssessmentResultsResponse' smart constructor.
data DescribeReplicationTaskAssessmentResultsResponse = DescribeReplicationTaskAssessmentResultsResponse'
  { -- | - The Amazon S3 bucket where the task assessment report is located.
    bucketName :: Lude.Maybe Lude.Text,
    -- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Lude.Maybe Lude.Text,
    -- | The task assessment report.
    replicationTaskAssessmentResults :: Lude.Maybe [ReplicationTaskAssessmentResult],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeReplicationTaskAssessmentResultsResponse' with the minimum fields required to make a request.
--
-- * 'bucketName' - - The Amazon S3 bucket where the task assessment report is located.
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'replicationTaskAssessmentResults' - The task assessment report.
-- * 'responseStatus' - The response status code.
mkDescribeReplicationTaskAssessmentResultsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeReplicationTaskAssessmentResultsResponse
mkDescribeReplicationTaskAssessmentResultsResponse pResponseStatus_ =
  DescribeReplicationTaskAssessmentResultsResponse'
    { bucketName =
        Lude.Nothing,
      marker = Lude.Nothing,
      replicationTaskAssessmentResults =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | - The Amazon S3 bucket where the task assessment report is located.
--
-- /Note:/ Consider using 'bucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtarfrsBucketName :: Lens.Lens' DescribeReplicationTaskAssessmentResultsResponse (Lude.Maybe Lude.Text)
drtarfrsBucketName = Lens.lens (bucketName :: DescribeReplicationTaskAssessmentResultsResponse -> Lude.Maybe Lude.Text) (\s a -> s {bucketName = a} :: DescribeReplicationTaskAssessmentResultsResponse)
{-# DEPRECATED drtarfrsBucketName "Use generic-lens or generic-optics with 'bucketName' instead." #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtarfrsMarker :: Lens.Lens' DescribeReplicationTaskAssessmentResultsResponse (Lude.Maybe Lude.Text)
drtarfrsMarker = Lens.lens (marker :: DescribeReplicationTaskAssessmentResultsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeReplicationTaskAssessmentResultsResponse)
{-# DEPRECATED drtarfrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The task assessment report.
--
-- /Note:/ Consider using 'replicationTaskAssessmentResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtarfrsReplicationTaskAssessmentResults :: Lens.Lens' DescribeReplicationTaskAssessmentResultsResponse (Lude.Maybe [ReplicationTaskAssessmentResult])
drtarfrsReplicationTaskAssessmentResults = Lens.lens (replicationTaskAssessmentResults :: DescribeReplicationTaskAssessmentResultsResponse -> Lude.Maybe [ReplicationTaskAssessmentResult]) (\s a -> s {replicationTaskAssessmentResults = a} :: DescribeReplicationTaskAssessmentResultsResponse)
{-# DEPRECATED drtarfrsReplicationTaskAssessmentResults "Use generic-lens or generic-optics with 'replicationTaskAssessmentResults' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtarfrsResponseStatus :: Lens.Lens' DescribeReplicationTaskAssessmentResultsResponse Lude.Int
drtarfrsResponseStatus = Lens.lens (responseStatus :: DescribeReplicationTaskAssessmentResultsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeReplicationTaskAssessmentResultsResponse)
{-# DEPRECATED drtarfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
