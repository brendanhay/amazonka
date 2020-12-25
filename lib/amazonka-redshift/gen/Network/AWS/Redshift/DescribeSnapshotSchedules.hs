{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeSnapshotSchedules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of snapshot schedules.
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeSnapshotSchedules
  ( -- * Creating a request
    DescribeSnapshotSchedules (..),
    mkDescribeSnapshotSchedules,

    -- ** Request lenses
    dssClusterIdentifier,
    dssMarker,
    dssMaxRecords,
    dssScheduleIdentifier,
    dssTagKeys,
    dssTagValues,

    -- * Destructuring the response
    DescribeSnapshotSchedulesResponse (..),
    mkDescribeSnapshotSchedulesResponse,

    -- ** Response lenses
    dssrrsMarker,
    dssrrsSnapshotSchedules,
    dssrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeSnapshotSchedules' smart constructor.
data DescribeSnapshotSchedules = DescribeSnapshotSchedules'
  { -- | The unique identifier for the cluster whose snapshot schedules you want to view.
    clusterIdentifier :: Core.Maybe Types.ClusterIdentifier,
    -- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @marker@ parameter and retrying the command. If the @marker@ field is empty, all response records have been retrieved for the request.
    marker :: Core.Maybe Types.Marker,
    -- | The maximum number or response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned @marker@ value.
    maxRecords :: Core.Maybe Core.Int,
    -- | A unique identifier for a snapshot schedule.
    scheduleIdentifier :: Core.Maybe Types.ScheduleIdentifier,
    -- | The key value for a snapshot schedule tag.
    tagKeys :: Core.Maybe [Types.String],
    -- | The value corresponding to the key of the snapshot schedule tag.
    tagValues :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSnapshotSchedules' value with any optional fields omitted.
mkDescribeSnapshotSchedules ::
  DescribeSnapshotSchedules
mkDescribeSnapshotSchedules =
  DescribeSnapshotSchedules'
    { clusterIdentifier = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing,
      scheduleIdentifier = Core.Nothing,
      tagKeys = Core.Nothing,
      tagValues = Core.Nothing
    }

-- | The unique identifier for the cluster whose snapshot schedules you want to view.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssClusterIdentifier :: Lens.Lens' DescribeSnapshotSchedules (Core.Maybe Types.ClusterIdentifier)
dssClusterIdentifier = Lens.field @"clusterIdentifier"
{-# DEPRECATED dssClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

-- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @marker@ parameter and retrying the command. If the @marker@ field is empty, all response records have been retrieved for the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssMarker :: Lens.Lens' DescribeSnapshotSchedules (Core.Maybe Types.Marker)
dssMarker = Lens.field @"marker"
{-# DEPRECATED dssMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number or response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned @marker@ value.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssMaxRecords :: Lens.Lens' DescribeSnapshotSchedules (Core.Maybe Core.Int)
dssMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED dssMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | A unique identifier for a snapshot schedule.
--
-- /Note:/ Consider using 'scheduleIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssScheduleIdentifier :: Lens.Lens' DescribeSnapshotSchedules (Core.Maybe Types.ScheduleIdentifier)
dssScheduleIdentifier = Lens.field @"scheduleIdentifier"
{-# DEPRECATED dssScheduleIdentifier "Use generic-lens or generic-optics with 'scheduleIdentifier' instead." #-}

-- | The key value for a snapshot schedule tag.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssTagKeys :: Lens.Lens' DescribeSnapshotSchedules (Core.Maybe [Types.String])
dssTagKeys = Lens.field @"tagKeys"
{-# DEPRECATED dssTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

-- | The value corresponding to the key of the snapshot schedule tag.
--
-- /Note:/ Consider using 'tagValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssTagValues :: Lens.Lens' DescribeSnapshotSchedules (Core.Maybe [Types.String])
dssTagValues = Lens.field @"tagValues"
{-# DEPRECATED dssTagValues "Use generic-lens or generic-optics with 'tagValues' instead." #-}

instance Core.AWSRequest DescribeSnapshotSchedules where
  type
    Rs DescribeSnapshotSchedules =
      DescribeSnapshotSchedulesResponse
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
            ( Core.pure ("Action", "DescribeSnapshotSchedules")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> (Core.toQueryValue "ClusterIdentifier" Core.<$> clusterIdentifier)
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> (Core.toQueryValue "MaxRecords" Core.<$> maxRecords)
                Core.<> ( Core.toQueryValue "ScheduleIdentifier"
                            Core.<$> scheduleIdentifier
                        )
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
      "DescribeSnapshotSchedulesResult"
      ( \s h x ->
          DescribeSnapshotSchedulesResponse'
            Core.<$> (x Core..@? "Marker")
            Core.<*> ( x Core..@? "SnapshotSchedules"
                         Core..<@> Core.parseXMLList "SnapshotSchedule"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeSnapshotSchedules where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"snapshotSchedules" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | /See:/ 'mkDescribeSnapshotSchedulesResponse' smart constructor.
data DescribeSnapshotSchedulesResponse = DescribeSnapshotSchedulesResponse'
  { -- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @marker@ parameter and retrying the command. If the @marker@ field is empty, all response records have been retrieved for the request.
    marker :: Core.Maybe Types.String,
    -- | A list of SnapshotSchedules.
    snapshotSchedules :: Core.Maybe [Types.SnapshotSchedule],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeSnapshotSchedulesResponse' value with any optional fields omitted.
mkDescribeSnapshotSchedulesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeSnapshotSchedulesResponse
mkDescribeSnapshotSchedulesResponse responseStatus =
  DescribeSnapshotSchedulesResponse'
    { marker = Core.Nothing,
      snapshotSchedules = Core.Nothing,
      responseStatus
    }

-- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @marker@ parameter and retrying the command. If the @marker@ field is empty, all response records have been retrieved for the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrrsMarker :: Lens.Lens' DescribeSnapshotSchedulesResponse (Core.Maybe Types.String)
dssrrsMarker = Lens.field @"marker"
{-# DEPRECATED dssrrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A list of SnapshotSchedules.
--
-- /Note:/ Consider using 'snapshotSchedules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrrsSnapshotSchedules :: Lens.Lens' DescribeSnapshotSchedulesResponse (Core.Maybe [Types.SnapshotSchedule])
dssrrsSnapshotSchedules = Lens.field @"snapshotSchedules"
{-# DEPRECATED dssrrsSnapshotSchedules "Use generic-lens or generic-optics with 'snapshotSchedules' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrrsResponseStatus :: Lens.Lens' DescribeSnapshotSchedulesResponse Core.Int
dssrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dssrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
