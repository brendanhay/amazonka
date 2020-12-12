{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeClusterSnapshots
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns one or more snapshot objects, which contain metadata about your cluster snapshots. By default, this operation returns information about all snapshots of all clusters that are owned by you AWS customer account. No information is returned for snapshots owned by inactive AWS customer accounts.
--
-- If you specify both tag keys and tag values in the same request, Amazon Redshift returns all snapshots that match any combination of the specified keys and values. For example, if you have @owner@ and @environment@ for tag keys, and @admin@ and @test@ for tag values, all snapshots that have any combination of those values are returned. Only snapshots that you own are returned in the response; shared snapshots are not returned with the tag key and tag value request parameters.
-- If both tag keys and values are omitted from the request, snapshots are returned regardless of whether they have tag keys or values associated with them.
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeClusterSnapshots
  ( -- * Creating a request
    DescribeClusterSnapshots (..),
    mkDescribeClusterSnapshots,

    -- ** Request lenses
    dSnapshotIdentifier,
    dTagValues,
    dClusterExists,
    dStartTime,
    dTagKeys,
    dClusterIdentifier,
    dSnapshotType,
    dSortingEntities,
    dMarker,
    dMaxRecords,
    dEndTime,
    dOwnerAccount,

    -- * Destructuring the response
    DescribeClusterSnapshotsResponse (..),
    mkDescribeClusterSnapshotsResponse,

    -- ** Response lenses
    dcssrsSnapshots,
    dcssrsMarker,
    dcssrsResponseStatus,
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
-- /See:/ 'mkDescribeClusterSnapshots' smart constructor.
data DescribeClusterSnapshots = DescribeClusterSnapshots'
  { snapshotIdentifier ::
      Lude.Maybe Lude.Text,
    tagValues :: Lude.Maybe [Lude.Text],
    clusterExists :: Lude.Maybe Lude.Bool,
    startTime :: Lude.Maybe Lude.DateTime,
    tagKeys :: Lude.Maybe [Lude.Text],
    clusterIdentifier :: Lude.Maybe Lude.Text,
    snapshotType :: Lude.Maybe Lude.Text,
    sortingEntities ::
      Lude.Maybe [SnapshotSortingEntity],
    marker :: Lude.Maybe Lude.Text,
    maxRecords :: Lude.Maybe Lude.Int,
    endTime :: Lude.Maybe Lude.DateTime,
    ownerAccount :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeClusterSnapshots' with the minimum fields required to make a request.
--
-- * 'clusterExists' - A value that indicates whether to return snapshots only for an existing cluster. You can perform table-level restore only by using a snapshot of an existing cluster, that is, a cluster that has not been deleted. Values for this parameter work as follows:
--
--
--     * If @ClusterExists@ is set to @true@ , @ClusterIdentifier@ is required.
--
--
--     * If @ClusterExists@ is set to @false@ and @ClusterIdentifier@ isn't specified, all snapshots associated with deleted clusters (orphaned snapshots) are returned.
--
--
--     * If @ClusterExists@ is set to @false@ and @ClusterIdentifier@ is specified for a deleted cluster, snapshots associated with that cluster are returned.
--
--
--     * If @ClusterExists@ is set to @false@ and @ClusterIdentifier@ is specified for an existing cluster, no snapshots are returned.
--
--
-- * 'clusterIdentifier' - The identifier of the cluster which generated the requested snapshots.
-- * 'endTime' - A time value that requests only snapshots created at or before the specified time. The time value is specified in ISO 8601 format. For more information about ISO 8601, go to the <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.>
--
-- Example: @2012-07-16T18:00:00Z@
-- * 'marker' - An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeClusterSnapshots' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
-- * 'maxRecords' - The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
--
-- Default: @100@
-- Constraints: minimum 20, maximum 100.
-- * 'ownerAccount' - The AWS customer account used to create or copy the snapshot. Use this field to filter the results to snapshots owned by a particular account. To describe snapshots you own, either specify your AWS customer account, or do not specify the parameter.
-- * 'snapshotIdentifier' - The snapshot identifier of the snapshot about which to return information.
-- * 'snapshotType' - The type of snapshots for which you are requesting information. By default, snapshots of all types are returned.
--
-- Valid Values: @automated@ | @manual@
-- * 'sortingEntities' -
-- * 'startTime' - A value that requests only snapshots created at or after the specified time. The time value is specified in ISO 8601 format. For more information about ISO 8601, go to the <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.>
--
-- Example: @2012-07-16T18:00:00Z@
-- * 'tagKeys' - A tag key or keys for which you want to return all matching cluster snapshots that are associated with the specified key or keys. For example, suppose that you have snapshots that are tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with the snapshots that have either or both of these tag keys associated with them.
-- * 'tagValues' - A tag value or values for which you want to return all matching cluster snapshots that are associated with the specified tag value or values. For example, suppose that you have snapshots that are tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with the snapshots that have either or both of these tag values associated with them.
mkDescribeClusterSnapshots ::
  DescribeClusterSnapshots
mkDescribeClusterSnapshots =
  DescribeClusterSnapshots'
    { snapshotIdentifier = Lude.Nothing,
      tagValues = Lude.Nothing,
      clusterExists = Lude.Nothing,
      startTime = Lude.Nothing,
      tagKeys = Lude.Nothing,
      clusterIdentifier = Lude.Nothing,
      snapshotType = Lude.Nothing,
      sortingEntities = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing,
      endTime = Lude.Nothing,
      ownerAccount = Lude.Nothing
    }

-- | The snapshot identifier of the snapshot about which to return information.
--
-- /Note:/ Consider using 'snapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dSnapshotIdentifier :: Lens.Lens' DescribeClusterSnapshots (Lude.Maybe Lude.Text)
dSnapshotIdentifier = Lens.lens (snapshotIdentifier :: DescribeClusterSnapshots -> Lude.Maybe Lude.Text) (\s a -> s {snapshotIdentifier = a} :: DescribeClusterSnapshots)
{-# DEPRECATED dSnapshotIdentifier "Use generic-lens or generic-optics with 'snapshotIdentifier' instead." #-}

-- | A tag value or values for which you want to return all matching cluster snapshots that are associated with the specified tag value or values. For example, suppose that you have snapshots that are tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with the snapshots that have either or both of these tag values associated with them.
--
-- /Note:/ Consider using 'tagValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dTagValues :: Lens.Lens' DescribeClusterSnapshots (Lude.Maybe [Lude.Text])
dTagValues = Lens.lens (tagValues :: DescribeClusterSnapshots -> Lude.Maybe [Lude.Text]) (\s a -> s {tagValues = a} :: DescribeClusterSnapshots)
{-# DEPRECATED dTagValues "Use generic-lens or generic-optics with 'tagValues' instead." #-}

-- | A value that indicates whether to return snapshots only for an existing cluster. You can perform table-level restore only by using a snapshot of an existing cluster, that is, a cluster that has not been deleted. Values for this parameter work as follows:
--
--
--     * If @ClusterExists@ is set to @true@ , @ClusterIdentifier@ is required.
--
--
--     * If @ClusterExists@ is set to @false@ and @ClusterIdentifier@ isn't specified, all snapshots associated with deleted clusters (orphaned snapshots) are returned.
--
--
--     * If @ClusterExists@ is set to @false@ and @ClusterIdentifier@ is specified for a deleted cluster, snapshots associated with that cluster are returned.
--
--
--     * If @ClusterExists@ is set to @false@ and @ClusterIdentifier@ is specified for an existing cluster, no snapshots are returned.
--
--
--
-- /Note:/ Consider using 'clusterExists' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dClusterExists :: Lens.Lens' DescribeClusterSnapshots (Lude.Maybe Lude.Bool)
dClusterExists = Lens.lens (clusterExists :: DescribeClusterSnapshots -> Lude.Maybe Lude.Bool) (\s a -> s {clusterExists = a} :: DescribeClusterSnapshots)
{-# DEPRECATED dClusterExists "Use generic-lens or generic-optics with 'clusterExists' instead." #-}

-- | A value that requests only snapshots created at or after the specified time. The time value is specified in ISO 8601 format. For more information about ISO 8601, go to the <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.>
--
-- Example: @2012-07-16T18:00:00Z@
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dStartTime :: Lens.Lens' DescribeClusterSnapshots (Lude.Maybe Lude.DateTime)
dStartTime = Lens.lens (startTime :: DescribeClusterSnapshots -> Lude.Maybe Lude.DateTime) (\s a -> s {startTime = a} :: DescribeClusterSnapshots)
{-# DEPRECATED dStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | A tag key or keys for which you want to return all matching cluster snapshots that are associated with the specified key or keys. For example, suppose that you have snapshots that are tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with the snapshots that have either or both of these tag keys associated with them.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dTagKeys :: Lens.Lens' DescribeClusterSnapshots (Lude.Maybe [Lude.Text])
dTagKeys = Lens.lens (tagKeys :: DescribeClusterSnapshots -> Lude.Maybe [Lude.Text]) (\s a -> s {tagKeys = a} :: DescribeClusterSnapshots)
{-# DEPRECATED dTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

-- | The identifier of the cluster which generated the requested snapshots.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dClusterIdentifier :: Lens.Lens' DescribeClusterSnapshots (Lude.Maybe Lude.Text)
dClusterIdentifier = Lens.lens (clusterIdentifier :: DescribeClusterSnapshots -> Lude.Maybe Lude.Text) (\s a -> s {clusterIdentifier = a} :: DescribeClusterSnapshots)
{-# DEPRECATED dClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

-- | The type of snapshots for which you are requesting information. By default, snapshots of all types are returned.
--
-- Valid Values: @automated@ | @manual@
--
-- /Note:/ Consider using 'snapshotType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dSnapshotType :: Lens.Lens' DescribeClusterSnapshots (Lude.Maybe Lude.Text)
dSnapshotType = Lens.lens (snapshotType :: DescribeClusterSnapshots -> Lude.Maybe Lude.Text) (\s a -> s {snapshotType = a} :: DescribeClusterSnapshots)
{-# DEPRECATED dSnapshotType "Use generic-lens or generic-optics with 'snapshotType' instead." #-}

-- |
--
-- /Note:/ Consider using 'sortingEntities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dSortingEntities :: Lens.Lens' DescribeClusterSnapshots (Lude.Maybe [SnapshotSortingEntity])
dSortingEntities = Lens.lens (sortingEntities :: DescribeClusterSnapshots -> Lude.Maybe [SnapshotSortingEntity]) (\s a -> s {sortingEntities = a} :: DescribeClusterSnapshots)
{-# DEPRECATED dSortingEntities "Use generic-lens or generic-optics with 'sortingEntities' instead." #-}

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeClusterSnapshots' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMarker :: Lens.Lens' DescribeClusterSnapshots (Lude.Maybe Lude.Text)
dMarker = Lens.lens (marker :: DescribeClusterSnapshots -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeClusterSnapshots)
{-# DEPRECATED dMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
--
-- Default: @100@
-- Constraints: minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMaxRecords :: Lens.Lens' DescribeClusterSnapshots (Lude.Maybe Lude.Int)
dMaxRecords = Lens.lens (maxRecords :: DescribeClusterSnapshots -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeClusterSnapshots)
{-# DEPRECATED dMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | A time value that requests only snapshots created at or before the specified time. The time value is specified in ISO 8601 format. For more information about ISO 8601, go to the <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.>
--
-- Example: @2012-07-16T18:00:00Z@
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dEndTime :: Lens.Lens' DescribeClusterSnapshots (Lude.Maybe Lude.DateTime)
dEndTime = Lens.lens (endTime :: DescribeClusterSnapshots -> Lude.Maybe Lude.DateTime) (\s a -> s {endTime = a} :: DescribeClusterSnapshots)
{-# DEPRECATED dEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The AWS customer account used to create or copy the snapshot. Use this field to filter the results to snapshots owned by a particular account. To describe snapshots you own, either specify your AWS customer account, or do not specify the parameter.
--
-- /Note:/ Consider using 'ownerAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dOwnerAccount :: Lens.Lens' DescribeClusterSnapshots (Lude.Maybe Lude.Text)
dOwnerAccount = Lens.lens (ownerAccount :: DescribeClusterSnapshots -> Lude.Maybe Lude.Text) (\s a -> s {ownerAccount = a} :: DescribeClusterSnapshots)
{-# DEPRECATED dOwnerAccount "Use generic-lens or generic-optics with 'ownerAccount' instead." #-}

instance Page.AWSPager DescribeClusterSnapshots where
  page rq rs
    | Page.stop (rs Lens.^. dcssrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dcssrsSnapshots) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& dMarker Lens..~ rs Lens.^. dcssrsMarker

instance Lude.AWSRequest DescribeClusterSnapshots where
  type Rs DescribeClusterSnapshots = DescribeClusterSnapshotsResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "DescribeClusterSnapshotsResult"
      ( \s h x ->
          DescribeClusterSnapshotsResponse'
            Lude.<$> ( x Lude..@? "Snapshots" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "Snapshot")
                     )
            Lude.<*> (x Lude..@? "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeClusterSnapshots where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeClusterSnapshots where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeClusterSnapshots where
  toQuery DescribeClusterSnapshots' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeClusterSnapshots" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "SnapshotIdentifier" Lude.=: snapshotIdentifier,
        "TagValues"
          Lude.=: Lude.toQuery (Lude.toQueryList "TagValue" Lude.<$> tagValues),
        "ClusterExists" Lude.=: clusterExists,
        "StartTime" Lude.=: startTime,
        "TagKeys"
          Lude.=: Lude.toQuery (Lude.toQueryList "TagKey" Lude.<$> tagKeys),
        "ClusterIdentifier" Lude.=: clusterIdentifier,
        "SnapshotType" Lude.=: snapshotType,
        "SortingEntities"
          Lude.=: Lude.toQuery
            ( Lude.toQueryList "SnapshotSortingEntity"
                Lude.<$> sortingEntities
            ),
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords,
        "EndTime" Lude.=: endTime,
        "OwnerAccount" Lude.=: ownerAccount
      ]

-- | Contains the output from the 'DescribeClusterSnapshots' action.
--
-- /See:/ 'mkDescribeClusterSnapshotsResponse' smart constructor.
data DescribeClusterSnapshotsResponse = DescribeClusterSnapshotsResponse'
  { snapshots ::
      Lude.Maybe [Snapshot],
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

-- | Creates a value of 'DescribeClusterSnapshotsResponse' with the minimum fields required to make a request.
--
-- * 'marker' - A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
-- * 'responseStatus' - The response status code.
-- * 'snapshots' - A list of 'Snapshot' instances.
mkDescribeClusterSnapshotsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeClusterSnapshotsResponse
mkDescribeClusterSnapshotsResponse pResponseStatus_ =
  DescribeClusterSnapshotsResponse'
    { snapshots = Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of 'Snapshot' instances.
--
-- /Note:/ Consider using 'snapshots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcssrsSnapshots :: Lens.Lens' DescribeClusterSnapshotsResponse (Lude.Maybe [Snapshot])
dcssrsSnapshots = Lens.lens (snapshots :: DescribeClusterSnapshotsResponse -> Lude.Maybe [Snapshot]) (\s a -> s {snapshots = a} :: DescribeClusterSnapshotsResponse)
{-# DEPRECATED dcssrsSnapshots "Use generic-lens or generic-optics with 'snapshots' instead." #-}

-- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcssrsMarker :: Lens.Lens' DescribeClusterSnapshotsResponse (Lude.Maybe Lude.Text)
dcssrsMarker = Lens.lens (marker :: DescribeClusterSnapshotsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeClusterSnapshotsResponse)
{-# DEPRECATED dcssrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcssrsResponseStatus :: Lens.Lens' DescribeClusterSnapshotsResponse Lude.Int
dcssrsResponseStatus = Lens.lens (responseStatus :: DescribeClusterSnapshotsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeClusterSnapshotsResponse)
{-# DEPRECATED dcssrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
