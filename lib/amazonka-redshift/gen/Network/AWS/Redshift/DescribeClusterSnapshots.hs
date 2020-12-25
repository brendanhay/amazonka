{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    dClusterExists,
    dClusterIdentifier,
    dEndTime,
    dMarker,
    dMaxRecords,
    dOwnerAccount,
    dSnapshotIdentifier,
    dSnapshotType,
    dSortingEntities,
    dStartTime,
    dTagKeys,
    dTagValues,

    -- * Destructuring the response
    DescribeClusterSnapshotsResponse (..),
    mkDescribeClusterSnapshotsResponse,

    -- ** Response lenses
    drsMarker,
    drsSnapshots,
    drsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkDescribeClusterSnapshots' smart constructor.
data DescribeClusterSnapshots = DescribeClusterSnapshots'
  { -- | A value that indicates whether to return snapshots only for an existing cluster. You can perform table-level restore only by using a snapshot of an existing cluster, that is, a cluster that has not been deleted. Values for this parameter work as follows:
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
    clusterExists :: Core.Maybe Core.Bool,
    -- | The identifier of the cluster which generated the requested snapshots.
    clusterIdentifier :: Core.Maybe Types.String,
    -- | A time value that requests only snapshots created at or before the specified time. The time value is specified in ISO 8601 format. For more information about ISO 8601, go to the <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.>
    --
    -- Example: @2012-07-16T18:00:00Z@
    endTime :: Core.Maybe Core.UTCTime,
    -- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeClusterSnapshots' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
    marker :: Core.Maybe Types.String,
    -- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
    --
    -- Default: @100@
    -- Constraints: minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Int,
    -- | The AWS customer account used to create or copy the snapshot. Use this field to filter the results to snapshots owned by a particular account. To describe snapshots you own, either specify your AWS customer account, or do not specify the parameter.
    ownerAccount :: Core.Maybe Types.String,
    -- | The snapshot identifier of the snapshot about which to return information.
    snapshotIdentifier :: Core.Maybe Types.String,
    -- | The type of snapshots for which you are requesting information. By default, snapshots of all types are returned.
    --
    -- Valid Values: @automated@ | @manual@
    snapshotType :: Core.Maybe Types.String,
    -- |
    sortingEntities :: Core.Maybe [Types.SnapshotSortingEntity],
    -- | A value that requests only snapshots created at or after the specified time. The time value is specified in ISO 8601 format. For more information about ISO 8601, go to the <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.>
    --
    -- Example: @2012-07-16T18:00:00Z@
    startTime :: Core.Maybe Core.UTCTime,
    -- | A tag key or keys for which you want to return all matching cluster snapshots that are associated with the specified key or keys. For example, suppose that you have snapshots that are tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with the snapshots that have either or both of these tag keys associated with them.
    tagKeys :: Core.Maybe [Types.String],
    -- | A tag value or values for which you want to return all matching cluster snapshots that are associated with the specified tag value or values. For example, suppose that you have snapshots that are tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with the snapshots that have either or both of these tag values associated with them.
    tagValues :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeClusterSnapshots' value with any optional fields omitted.
mkDescribeClusterSnapshots ::
  DescribeClusterSnapshots
mkDescribeClusterSnapshots =
  DescribeClusterSnapshots'
    { clusterExists = Core.Nothing,
      clusterIdentifier = Core.Nothing,
      endTime = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing,
      ownerAccount = Core.Nothing,
      snapshotIdentifier = Core.Nothing,
      snapshotType = Core.Nothing,
      sortingEntities = Core.Nothing,
      startTime = Core.Nothing,
      tagKeys = Core.Nothing,
      tagValues = Core.Nothing
    }

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
dClusterExists :: Lens.Lens' DescribeClusterSnapshots (Core.Maybe Core.Bool)
dClusterExists = Lens.field @"clusterExists"
{-# DEPRECATED dClusterExists "Use generic-lens or generic-optics with 'clusterExists' instead." #-}

-- | The identifier of the cluster which generated the requested snapshots.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dClusterIdentifier :: Lens.Lens' DescribeClusterSnapshots (Core.Maybe Types.String)
dClusterIdentifier = Lens.field @"clusterIdentifier"
{-# DEPRECATED dClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

-- | A time value that requests only snapshots created at or before the specified time. The time value is specified in ISO 8601 format. For more information about ISO 8601, go to the <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.>
--
-- Example: @2012-07-16T18:00:00Z@
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dEndTime :: Lens.Lens' DescribeClusterSnapshots (Core.Maybe Core.UTCTime)
dEndTime = Lens.field @"endTime"
{-# DEPRECATED dEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeClusterSnapshots' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMarker :: Lens.Lens' DescribeClusterSnapshots (Core.Maybe Types.String)
dMarker = Lens.field @"marker"
{-# DEPRECATED dMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
--
-- Default: @100@
-- Constraints: minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMaxRecords :: Lens.Lens' DescribeClusterSnapshots (Core.Maybe Core.Int)
dMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED dMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The AWS customer account used to create or copy the snapshot. Use this field to filter the results to snapshots owned by a particular account. To describe snapshots you own, either specify your AWS customer account, or do not specify the parameter.
--
-- /Note:/ Consider using 'ownerAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dOwnerAccount :: Lens.Lens' DescribeClusterSnapshots (Core.Maybe Types.String)
dOwnerAccount = Lens.field @"ownerAccount"
{-# DEPRECATED dOwnerAccount "Use generic-lens or generic-optics with 'ownerAccount' instead." #-}

-- | The snapshot identifier of the snapshot about which to return information.
--
-- /Note:/ Consider using 'snapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dSnapshotIdentifier :: Lens.Lens' DescribeClusterSnapshots (Core.Maybe Types.String)
dSnapshotIdentifier = Lens.field @"snapshotIdentifier"
{-# DEPRECATED dSnapshotIdentifier "Use generic-lens or generic-optics with 'snapshotIdentifier' instead." #-}

-- | The type of snapshots for which you are requesting information. By default, snapshots of all types are returned.
--
-- Valid Values: @automated@ | @manual@
--
-- /Note:/ Consider using 'snapshotType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dSnapshotType :: Lens.Lens' DescribeClusterSnapshots (Core.Maybe Types.String)
dSnapshotType = Lens.field @"snapshotType"
{-# DEPRECATED dSnapshotType "Use generic-lens or generic-optics with 'snapshotType' instead." #-}

-- |
--
-- /Note:/ Consider using 'sortingEntities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dSortingEntities :: Lens.Lens' DescribeClusterSnapshots (Core.Maybe [Types.SnapshotSortingEntity])
dSortingEntities = Lens.field @"sortingEntities"
{-# DEPRECATED dSortingEntities "Use generic-lens or generic-optics with 'sortingEntities' instead." #-}

-- | A value that requests only snapshots created at or after the specified time. The time value is specified in ISO 8601 format. For more information about ISO 8601, go to the <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.>
--
-- Example: @2012-07-16T18:00:00Z@
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dStartTime :: Lens.Lens' DescribeClusterSnapshots (Core.Maybe Core.UTCTime)
dStartTime = Lens.field @"startTime"
{-# DEPRECATED dStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | A tag key or keys for which you want to return all matching cluster snapshots that are associated with the specified key or keys. For example, suppose that you have snapshots that are tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with the snapshots that have either or both of these tag keys associated with them.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dTagKeys :: Lens.Lens' DescribeClusterSnapshots (Core.Maybe [Types.String])
dTagKeys = Lens.field @"tagKeys"
{-# DEPRECATED dTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

-- | A tag value or values for which you want to return all matching cluster snapshots that are associated with the specified tag value or values. For example, suppose that you have snapshots that are tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with the snapshots that have either or both of these tag values associated with them.
--
-- /Note:/ Consider using 'tagValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dTagValues :: Lens.Lens' DescribeClusterSnapshots (Core.Maybe [Types.String])
dTagValues = Lens.field @"tagValues"
{-# DEPRECATED dTagValues "Use generic-lens or generic-optics with 'tagValues' instead." #-}

instance Core.AWSRequest DescribeClusterSnapshots where
  type Rs DescribeClusterSnapshots = DescribeClusterSnapshotsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DescribeClusterSnapshots")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> (Core.toQueryValue "ClusterExists" Core.<$> clusterExists)
                Core.<> (Core.toQueryValue "ClusterIdentifier" Core.<$> clusterIdentifier)
                Core.<> (Core.toQueryValue "EndTime" Core.<$> endTime)
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> (Core.toQueryValue "MaxRecords" Core.<$> maxRecords)
                Core.<> (Core.toQueryValue "OwnerAccount" Core.<$> ownerAccount)
                Core.<> ( Core.toQueryValue "SnapshotIdentifier"
                            Core.<$> snapshotIdentifier
                        )
                Core.<> (Core.toQueryValue "SnapshotType" Core.<$> snapshotType)
                Core.<> ( Core.toQueryValue
                            "SortingEntities"
                            ( Core.toQueryList "SnapshotSortingEntity"
                                Core.<$> sortingEntities
                            )
                        )
                Core.<> (Core.toQueryValue "StartTime" Core.<$> startTime)
                Core.<> ( Core.toQueryValue
                            "TagKeys"
                            (Core.toQueryList "TagKey" Core.<$> tagKeys)
                        )
                Core.<> ( Core.toQueryValue
                            "TagValues"
                            (Core.toQueryList "TagValue" Core.<$> tagValues)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeClusterSnapshotsResult"
      ( \s h x ->
          DescribeClusterSnapshotsResponse'
            Core.<$> (x Core..@? "Marker")
            Core.<*> (x Core..@? "Snapshots" Core..<@> Core.parseXMLList "Snapshot")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeClusterSnapshots where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"snapshots" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | Contains the output from the 'DescribeClusterSnapshots' action.
--
-- /See:/ 'mkDescribeClusterSnapshotsResponse' smart constructor.
data DescribeClusterSnapshotsResponse = DescribeClusterSnapshotsResponse'
  { -- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
    marker :: Core.Maybe Types.String,
    -- | A list of 'Snapshot' instances.
    snapshots :: Core.Maybe [Types.Snapshot],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeClusterSnapshotsResponse' value with any optional fields omitted.
mkDescribeClusterSnapshotsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeClusterSnapshotsResponse
mkDescribeClusterSnapshotsResponse responseStatus =
  DescribeClusterSnapshotsResponse'
    { marker = Core.Nothing,
      snapshots = Core.Nothing,
      responseStatus
    }

-- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsMarker :: Lens.Lens' DescribeClusterSnapshotsResponse (Core.Maybe Types.String)
drsMarker = Lens.field @"marker"
{-# DEPRECATED drsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A list of 'Snapshot' instances.
--
-- /Note:/ Consider using 'snapshots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsSnapshots :: Lens.Lens' DescribeClusterSnapshotsResponse (Core.Maybe [Types.Snapshot])
drsSnapshots = Lens.field @"snapshots"
{-# DEPRECATED drsSnapshots "Use generic-lens or generic-optics with 'snapshots' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeClusterSnapshotsResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
