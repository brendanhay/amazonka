{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeGlobalClusters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about Aurora global database clusters. This API supports pagination.
--
-- For more information on Amazon Aurora, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?> in the /Amazon Aurora User Guide./
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeGlobalClusters
  ( -- * Creating a request
    DescribeGlobalClusters (..),
    mkDescribeGlobalClusters,

    -- ** Request lenses
    dgcsGlobalClusterIdentifier,
    dgcsFilters,
    dgcsMarker,
    dgcsMaxRecords,

    -- * Destructuring the response
    DescribeGlobalClustersResponse (..),
    mkDescribeGlobalClustersResponse,

    -- ** Response lenses
    drsGlobalClusters,
    drsMarker,
    drsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeGlobalClusters' smart constructor.
data DescribeGlobalClusters = DescribeGlobalClusters'
  { globalClusterIdentifier ::
      Lude.Maybe Lude.Text,
    filters :: Lude.Maybe [Filter],
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

-- | Creates a value of 'DescribeGlobalClusters' with the minimum fields required to make a request.
--
-- * 'filters' - A filter that specifies one or more global DB clusters to describe.
--
-- Supported filters:
--
--     * @db-cluster-id@ - Accepts DB cluster identifiers and DB cluster Amazon Resource Names (ARNs). The results list will only include information about the DB clusters identified by these ARNs.
--
--
-- * 'globalClusterIdentifier' - The user-supplied DB cluster identifier. If this parameter is specified, information from only the specific DB cluster is returned. This parameter isn't case-sensitive.
--
-- Constraints:
--
--     * If supplied, must match an existing DBClusterIdentifier.
--
--
-- * 'marker' - An optional pagination token provided by a previous @DescribeGlobalClusters@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
mkDescribeGlobalClusters ::
  DescribeGlobalClusters
mkDescribeGlobalClusters =
  DescribeGlobalClusters'
    { globalClusterIdentifier = Lude.Nothing,
      filters = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing
    }

-- | The user-supplied DB cluster identifier. If this parameter is specified, information from only the specific DB cluster is returned. This parameter isn't case-sensitive.
--
-- Constraints:
--
--     * If supplied, must match an existing DBClusterIdentifier.
--
--
--
-- /Note:/ Consider using 'globalClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgcsGlobalClusterIdentifier :: Lens.Lens' DescribeGlobalClusters (Lude.Maybe Lude.Text)
dgcsGlobalClusterIdentifier = Lens.lens (globalClusterIdentifier :: DescribeGlobalClusters -> Lude.Maybe Lude.Text) (\s a -> s {globalClusterIdentifier = a} :: DescribeGlobalClusters)
{-# DEPRECATED dgcsGlobalClusterIdentifier "Use generic-lens or generic-optics with 'globalClusterIdentifier' instead." #-}

-- | A filter that specifies one or more global DB clusters to describe.
--
-- Supported filters:
--
--     * @db-cluster-id@ - Accepts DB cluster identifiers and DB cluster Amazon Resource Names (ARNs). The results list will only include information about the DB clusters identified by these ARNs.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgcsFilters :: Lens.Lens' DescribeGlobalClusters (Lude.Maybe [Filter])
dgcsFilters = Lens.lens (filters :: DescribeGlobalClusters -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeGlobalClusters)
{-# DEPRECATED dgcsFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | An optional pagination token provided by a previous @DescribeGlobalClusters@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgcsMarker :: Lens.Lens' DescribeGlobalClusters (Lude.Maybe Lude.Text)
dgcsMarker = Lens.lens (marker :: DescribeGlobalClusters -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeGlobalClusters)
{-# DEPRECATED dgcsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgcsMaxRecords :: Lens.Lens' DescribeGlobalClusters (Lude.Maybe Lude.Int)
dgcsMaxRecords = Lens.lens (maxRecords :: DescribeGlobalClusters -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeGlobalClusters)
{-# DEPRECATED dgcsMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Page.AWSPager DescribeGlobalClusters where
  page rq rs
    | Page.stop (rs Lens.^. drsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. drsGlobalClusters) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& dgcsMarker Lens..~ rs Lens.^. drsMarker

instance Lude.AWSRequest DescribeGlobalClusters where
  type Rs DescribeGlobalClusters = DescribeGlobalClustersResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "DescribeGlobalClustersResult"
      ( \s h x ->
          DescribeGlobalClustersResponse'
            Lude.<$> ( x Lude..@? "GlobalClusters" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "GlobalClusterMember")
                     )
            Lude.<*> (x Lude..@? "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeGlobalClusters where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeGlobalClusters where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeGlobalClusters where
  toQuery DescribeGlobalClusters' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeGlobalClusters" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "GlobalClusterIdentifier" Lude.=: globalClusterIdentifier,
        "Filters"
          Lude.=: Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords
      ]

-- | /See:/ 'mkDescribeGlobalClustersResponse' smart constructor.
data DescribeGlobalClustersResponse = DescribeGlobalClustersResponse'
  { globalClusters ::
      Lude.Maybe [GlobalCluster],
    marker ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DescribeGlobalClustersResponse' with the minimum fields required to make a request.
--
-- * 'globalClusters' - The list of global clusters returned by this request.
-- * 'marker' - An optional pagination token provided by a previous @DescribeGlobalClusters@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'responseStatus' - The response status code.
mkDescribeGlobalClustersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeGlobalClustersResponse
mkDescribeGlobalClustersResponse pResponseStatus_ =
  DescribeGlobalClustersResponse'
    { globalClusters = Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of global clusters returned by this request.
--
-- /Note:/ Consider using 'globalClusters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsGlobalClusters :: Lens.Lens' DescribeGlobalClustersResponse (Lude.Maybe [GlobalCluster])
drsGlobalClusters = Lens.lens (globalClusters :: DescribeGlobalClustersResponse -> Lude.Maybe [GlobalCluster]) (\s a -> s {globalClusters = a} :: DescribeGlobalClustersResponse)
{-# DEPRECATED drsGlobalClusters "Use generic-lens or generic-optics with 'globalClusters' instead." #-}

-- | An optional pagination token provided by a previous @DescribeGlobalClusters@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsMarker :: Lens.Lens' DescribeGlobalClustersResponse (Lude.Maybe Lude.Text)
drsMarker = Lens.lens (marker :: DescribeGlobalClustersResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeGlobalClustersResponse)
{-# DEPRECATED drsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeGlobalClustersResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DescribeGlobalClustersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeGlobalClustersResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
