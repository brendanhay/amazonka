{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeClusters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns properties of provisioned clusters including general cluster properties, cluster database properties, maintenance and backup properties, and security and access properties. This operation supports pagination. For more information about managing clusters, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html Amazon Redshift Clusters> in the /Amazon Redshift Cluster Management Guide/ .
--
-- If you specify both tag keys and tag values in the same request, Amazon Redshift returns all clusters that match any combination of the specified keys and values. For example, if you have @owner@ and @environment@ for tag keys, and @admin@ and @test@ for tag values, all clusters that have any combination of those values are returned.
-- If both tag keys and values are omitted from the request, clusters are returned regardless of whether they have tag keys or values associated with them.
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeClusters
  ( -- * Creating a request
    DescribeClusters (..),
    mkDescribeClusters,

    -- ** Request lenses
    dcsTagValues,
    dcsTagKeys,
    dcsClusterIdentifier,
    dcsMarker,
    dcsMaxRecords,

    -- * Destructuring the response
    DescribeClustersResponse (..),
    mkDescribeClustersResponse,

    -- ** Response lenses
    dcfrsMarker,
    dcfrsClusters,
    dcfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDescribeClusters' smart constructor.
data DescribeClusters = DescribeClusters'
  { -- | A tag value or values for which you want to return all matching clusters that are associated with the specified tag value or values. For example, suppose that you have clusters that are tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with the clusters that have either or both of these tag values associated with them.
    tagValues :: Lude.Maybe [Lude.Text],
    -- | A tag key or keys for which you want to return all matching clusters that are associated with the specified key or keys. For example, suppose that you have clusters that are tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with the clusters that have either or both of these tag keys associated with them.
    tagKeys :: Lude.Maybe [Lude.Text],
    -- | The unique identifier of a cluster whose properties you are requesting. This parameter is case sensitive.
    --
    -- The default is that all clusters defined for an account are returned.
    clusterIdentifier :: Lude.Maybe Lude.Text,
    -- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeClusters' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
    --
    -- Constraints: You can specify either the __ClusterIdentifier__ parameter or the __Marker__ parameter, but not both.
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
    --
    -- Default: @100@
    -- Constraints: minimum 20, maximum 100.
    maxRecords :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeClusters' with the minimum fields required to make a request.
--
-- * 'tagValues' - A tag value or values for which you want to return all matching clusters that are associated with the specified tag value or values. For example, suppose that you have clusters that are tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with the clusters that have either or both of these tag values associated with them.
-- * 'tagKeys' - A tag key or keys for which you want to return all matching clusters that are associated with the specified key or keys. For example, suppose that you have clusters that are tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with the clusters that have either or both of these tag keys associated with them.
-- * 'clusterIdentifier' - The unique identifier of a cluster whose properties you are requesting. This parameter is case sensitive.
--
-- The default is that all clusters defined for an account are returned.
-- * 'marker' - An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeClusters' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
--
-- Constraints: You can specify either the __ClusterIdentifier__ parameter or the __Marker__ parameter, but not both.
-- * 'maxRecords' - The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
--
-- Default: @100@
-- Constraints: minimum 20, maximum 100.
mkDescribeClusters ::
  DescribeClusters
mkDescribeClusters =
  DescribeClusters'
    { tagValues = Lude.Nothing,
      tagKeys = Lude.Nothing,
      clusterIdentifier = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing
    }

-- | A tag value or values for which you want to return all matching clusters that are associated with the specified tag value or values. For example, suppose that you have clusters that are tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with the clusters that have either or both of these tag values associated with them.
--
-- /Note:/ Consider using 'tagValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsTagValues :: Lens.Lens' DescribeClusters (Lude.Maybe [Lude.Text])
dcsTagValues = Lens.lens (tagValues :: DescribeClusters -> Lude.Maybe [Lude.Text]) (\s a -> s {tagValues = a} :: DescribeClusters)
{-# DEPRECATED dcsTagValues "Use generic-lens or generic-optics with 'tagValues' instead." #-}

-- | A tag key or keys for which you want to return all matching clusters that are associated with the specified key or keys. For example, suppose that you have clusters that are tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with the clusters that have either or both of these tag keys associated with them.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsTagKeys :: Lens.Lens' DescribeClusters (Lude.Maybe [Lude.Text])
dcsTagKeys = Lens.lens (tagKeys :: DescribeClusters -> Lude.Maybe [Lude.Text]) (\s a -> s {tagKeys = a} :: DescribeClusters)
{-# DEPRECATED dcsTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

-- | The unique identifier of a cluster whose properties you are requesting. This parameter is case sensitive.
--
-- The default is that all clusters defined for an account are returned.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsClusterIdentifier :: Lens.Lens' DescribeClusters (Lude.Maybe Lude.Text)
dcsClusterIdentifier = Lens.lens (clusterIdentifier :: DescribeClusters -> Lude.Maybe Lude.Text) (\s a -> s {clusterIdentifier = a} :: DescribeClusters)
{-# DEPRECATED dcsClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeClusters' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
--
-- Constraints: You can specify either the __ClusterIdentifier__ parameter or the __Marker__ parameter, but not both.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsMarker :: Lens.Lens' DescribeClusters (Lude.Maybe Lude.Text)
dcsMarker = Lens.lens (marker :: DescribeClusters -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeClusters)
{-# DEPRECATED dcsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
--
-- Default: @100@
-- Constraints: minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsMaxRecords :: Lens.Lens' DescribeClusters (Lude.Maybe Lude.Int)
dcsMaxRecords = Lens.lens (maxRecords :: DescribeClusters -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeClusters)
{-# DEPRECATED dcsMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Page.AWSPager DescribeClusters where
  page rq rs
    | Page.stop (rs Lens.^. dcfrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dcfrsClusters) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& dcsMarker Lens..~ rs Lens.^. dcfrsMarker

instance Lude.AWSRequest DescribeClusters where
  type Rs DescribeClusters = DescribeClustersResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "DescribeClustersResult"
      ( \s h x ->
          DescribeClustersResponse'
            Lude.<$> (x Lude..@? "Marker")
            Lude.<*> ( x Lude..@? "Clusters" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "Cluster")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeClusters where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeClusters where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeClusters where
  toQuery DescribeClusters' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeClusters" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "TagValues"
          Lude.=: Lude.toQuery (Lude.toQueryList "TagValue" Lude.<$> tagValues),
        "TagKeys"
          Lude.=: Lude.toQuery (Lude.toQueryList "TagKey" Lude.<$> tagKeys),
        "ClusterIdentifier" Lude.=: clusterIdentifier,
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords
      ]

-- | Contains the output from the 'DescribeClusters' action.
--
-- /See:/ 'mkDescribeClustersResponse' smart constructor.
data DescribeClustersResponse = DescribeClustersResponse'
  { -- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
    marker :: Lude.Maybe Lude.Text,
    -- | A list of @Cluster@ objects, where each object describes one cluster.
    clusters :: Lude.Maybe [Cluster],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeClustersResponse' with the minimum fields required to make a request.
--
-- * 'marker' - A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
-- * 'clusters' - A list of @Cluster@ objects, where each object describes one cluster.
-- * 'responseStatus' - The response status code.
mkDescribeClustersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeClustersResponse
mkDescribeClustersResponse pResponseStatus_ =
  DescribeClustersResponse'
    { marker = Lude.Nothing,
      clusters = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfrsMarker :: Lens.Lens' DescribeClustersResponse (Lude.Maybe Lude.Text)
dcfrsMarker = Lens.lens (marker :: DescribeClustersResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeClustersResponse)
{-# DEPRECATED dcfrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A list of @Cluster@ objects, where each object describes one cluster.
--
-- /Note:/ Consider using 'clusters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfrsClusters :: Lens.Lens' DescribeClustersResponse (Lude.Maybe [Cluster])
dcfrsClusters = Lens.lens (clusters :: DescribeClustersResponse -> Lude.Maybe [Cluster]) (\s a -> s {clusters = a} :: DescribeClustersResponse)
{-# DEPRECATED dcfrsClusters "Use generic-lens or generic-optics with 'clusters' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfrsResponseStatus :: Lens.Lens' DescribeClustersResponse Lude.Int
dcfrsResponseStatus = Lens.lens (responseStatus :: DescribeClustersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeClustersResponse)
{-# DEPRECATED dcfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
