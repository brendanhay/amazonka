{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeDBClusters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about provisioned Aurora DB clusters. This API supports pagination.
--
-- For more information on Amazon Aurora, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?> in the /Amazon Aurora User Guide./
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBClusters
  ( -- * Creating a request
    DescribeDBClusters (..),
    mkDescribeDBClusters,

    -- ** Request lenses
    ddbcDBClusterIdentifier,
    ddbcIncludeShared,
    ddbcFilters,
    ddbcMarker,
    ddbcMaxRecords,

    -- * Destructuring the response
    DescribeDBClustersResponse (..),
    mkDescribeDBClustersResponse,

    -- ** Response lenses
    ddbcrsDBClusters,
    ddbcrsMarker,
    ddbcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDescribeDBClusters' smart constructor.
data DescribeDBClusters = DescribeDBClusters'
  { -- | The user-supplied DB cluster identifier. If this parameter is specified, information from only the specific DB cluster is returned. This parameter isn't case-sensitive.
    --
    -- Constraints:
    --
    --     * If supplied, must match an existing DBClusterIdentifier.
    dbClusterIdentifier :: Lude.Maybe Lude.Text,
    -- | Optional Boolean parameter that specifies whether the output includes information about clusters shared from other AWS accounts.
    includeShared :: Lude.Maybe Lude.Bool,
    -- | A filter that specifies one or more DB clusters to describe.
    --
    -- Supported filters:
    --
    --     * @db-cluster-id@ - Accepts DB cluster identifiers and DB cluster Amazon Resource Names (ARNs). The results list will only include information about the DB clusters identified by these ARNs.
    filters :: Lude.Maybe [Filter],
    -- | An optional pagination token provided by a previous @DescribeDBClusters@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results.
    --
    -- Default: 100
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDBClusters' with the minimum fields required to make a request.
--
-- * 'dbClusterIdentifier' - The user-supplied DB cluster identifier. If this parameter is specified, information from only the specific DB cluster is returned. This parameter isn't case-sensitive.
--
-- Constraints:
--
--     * If supplied, must match an existing DBClusterIdentifier.
--
--
-- * 'includeShared' - Optional Boolean parameter that specifies whether the output includes information about clusters shared from other AWS accounts.
-- * 'filters' - A filter that specifies one or more DB clusters to describe.
--
-- Supported filters:
--
--     * @db-cluster-id@ - Accepts DB cluster identifiers and DB cluster Amazon Resource Names (ARNs). The results list will only include information about the DB clusters identified by these ARNs.
--
--
-- * 'marker' - An optional pagination token provided by a previous @DescribeDBClusters@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
mkDescribeDBClusters ::
  DescribeDBClusters
mkDescribeDBClusters =
  DescribeDBClusters'
    { dbClusterIdentifier = Lude.Nothing,
      includeShared = Lude.Nothing,
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
-- /Note:/ Consider using 'dbClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcDBClusterIdentifier :: Lens.Lens' DescribeDBClusters (Lude.Maybe Lude.Text)
ddbcDBClusterIdentifier = Lens.lens (dbClusterIdentifier :: DescribeDBClusters -> Lude.Maybe Lude.Text) (\s a -> s {dbClusterIdentifier = a} :: DescribeDBClusters)
{-# DEPRECATED ddbcDBClusterIdentifier "Use generic-lens or generic-optics with 'dbClusterIdentifier' instead." #-}

-- | Optional Boolean parameter that specifies whether the output includes information about clusters shared from other AWS accounts.
--
-- /Note:/ Consider using 'includeShared' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcIncludeShared :: Lens.Lens' DescribeDBClusters (Lude.Maybe Lude.Bool)
ddbcIncludeShared = Lens.lens (includeShared :: DescribeDBClusters -> Lude.Maybe Lude.Bool) (\s a -> s {includeShared = a} :: DescribeDBClusters)
{-# DEPRECATED ddbcIncludeShared "Use generic-lens or generic-optics with 'includeShared' instead." #-}

-- | A filter that specifies one or more DB clusters to describe.
--
-- Supported filters:
--
--     * @db-cluster-id@ - Accepts DB cluster identifiers and DB cluster Amazon Resource Names (ARNs). The results list will only include information about the DB clusters identified by these ARNs.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcFilters :: Lens.Lens' DescribeDBClusters (Lude.Maybe [Filter])
ddbcFilters = Lens.lens (filters :: DescribeDBClusters -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeDBClusters)
{-# DEPRECATED ddbcFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | An optional pagination token provided by a previous @DescribeDBClusters@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcMarker :: Lens.Lens' DescribeDBClusters (Lude.Maybe Lude.Text)
ddbcMarker = Lens.lens (marker :: DescribeDBClusters -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeDBClusters)
{-# DEPRECATED ddbcMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcMaxRecords :: Lens.Lens' DescribeDBClusters (Lude.Maybe Lude.Int)
ddbcMaxRecords = Lens.lens (maxRecords :: DescribeDBClusters -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeDBClusters)
{-# DEPRECATED ddbcMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Page.AWSPager DescribeDBClusters where
  page rq rs
    | Page.stop (rs Lens.^. ddbcrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. ddbcrsDBClusters) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ddbcMarker Lens..~ rs Lens.^. ddbcrsMarker

instance Lude.AWSRequest DescribeDBClusters where
  type Rs DescribeDBClusters = DescribeDBClustersResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "DescribeDBClustersResult"
      ( \s h x ->
          DescribeDBClustersResponse'
            Lude.<$> ( x Lude..@? "DBClusters" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "DBCluster")
                     )
            Lude.<*> (x Lude..@? "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeDBClusters where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeDBClusters where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeDBClusters where
  toQuery DescribeDBClusters' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeDBClusters" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "DBClusterIdentifier" Lude.=: dbClusterIdentifier,
        "IncludeShared" Lude.=: includeShared,
        "Filters"
          Lude.=: Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords
      ]

-- | Contains the result of a successful invocation of the @DescribeDBClusters@ action.
--
-- /See:/ 'mkDescribeDBClustersResponse' smart constructor.
data DescribeDBClustersResponse = DescribeDBClustersResponse'
  { -- | Contains a list of DB clusters for the user.
    dbClusters :: Lude.Maybe [DBCluster],
    -- | A pagination token that can be used in a later DescribeDBClusters request.
    marker :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDBClustersResponse' with the minimum fields required to make a request.
--
-- * 'dbClusters' - Contains a list of DB clusters for the user.
-- * 'marker' - A pagination token that can be used in a later DescribeDBClusters request.
-- * 'responseStatus' - The response status code.
mkDescribeDBClustersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeDBClustersResponse
mkDescribeDBClustersResponse pResponseStatus_ =
  DescribeDBClustersResponse'
    { dbClusters = Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Contains a list of DB clusters for the user.
--
-- /Note:/ Consider using 'dbClusters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcrsDBClusters :: Lens.Lens' DescribeDBClustersResponse (Lude.Maybe [DBCluster])
ddbcrsDBClusters = Lens.lens (dbClusters :: DescribeDBClustersResponse -> Lude.Maybe [DBCluster]) (\s a -> s {dbClusters = a} :: DescribeDBClustersResponse)
{-# DEPRECATED ddbcrsDBClusters "Use generic-lens or generic-optics with 'dbClusters' instead." #-}

-- | A pagination token that can be used in a later DescribeDBClusters request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcrsMarker :: Lens.Lens' DescribeDBClustersResponse (Lude.Maybe Lude.Text)
ddbcrsMarker = Lens.lens (marker :: DescribeDBClustersResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeDBClustersResponse)
{-# DEPRECATED ddbcrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcrsResponseStatus :: Lens.Lens' DescribeDBClustersResponse Lude.Int
ddbcrsResponseStatus = Lens.lens (responseStatus :: DescribeDBClustersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeDBClustersResponse)
{-# DEPRECATED ddbcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
