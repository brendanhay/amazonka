{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeClusterTracks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all the available maintenance tracks.
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeClusterTracks
  ( -- * Creating a request
    DescribeClusterTracks (..),
    mkDescribeClusterTracks,

    -- ** Request lenses
    dctMaintenanceTrackName,
    dctMarker,
    dctMaxRecords,

    -- * Destructuring the response
    DescribeClusterTracksResponse (..),
    mkDescribeClusterTracksResponse,

    -- ** Response lenses
    dctrrsMaintenanceTracks,
    dctrrsMarker,
    dctrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeClusterTracks' smart constructor.
data DescribeClusterTracks = DescribeClusterTracks'
  { -- | The name of the maintenance track.
    maintenanceTrackName :: Core.Maybe Types.String,
    -- | An optional parameter that specifies the starting point to return a set of response records. When the results of a @DescribeClusterTracks@ request exceed the value specified in @MaxRecords@ , Amazon Redshift returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
    marker :: Core.Maybe Types.String,
    -- | An integer value for the maximum number of maintenance tracks to return.
    maxRecords :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeClusterTracks' value with any optional fields omitted.
mkDescribeClusterTracks ::
  DescribeClusterTracks
mkDescribeClusterTracks =
  DescribeClusterTracks'
    { maintenanceTrackName = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | The name of the maintenance track.
--
-- /Note:/ Consider using 'maintenanceTrackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dctMaintenanceTrackName :: Lens.Lens' DescribeClusterTracks (Core.Maybe Types.String)
dctMaintenanceTrackName = Lens.field @"maintenanceTrackName"
{-# DEPRECATED dctMaintenanceTrackName "Use generic-lens or generic-optics with 'maintenanceTrackName' instead." #-}

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a @DescribeClusterTracks@ request exceed the value specified in @MaxRecords@ , Amazon Redshift returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dctMarker :: Lens.Lens' DescribeClusterTracks (Core.Maybe Types.String)
dctMarker = Lens.field @"marker"
{-# DEPRECATED dctMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | An integer value for the maximum number of maintenance tracks to return.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dctMaxRecords :: Lens.Lens' DescribeClusterTracks (Core.Maybe Core.Int)
dctMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED dctMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Core.AWSRequest DescribeClusterTracks where
  type Rs DescribeClusterTracks = DescribeClusterTracksResponse
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
            ( Core.pure ("Action", "DescribeClusterTracks")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> ( Core.toQueryValue "MaintenanceTrackName"
                            Core.<$> maintenanceTrackName
                        )
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> (Core.toQueryValue "MaxRecords" Core.<$> maxRecords)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeClusterTracksResult"
      ( \s h x ->
          DescribeClusterTracksResponse'
            Core.<$> ( x Core..@? "MaintenanceTracks"
                         Core..<@> Core.parseXMLList "MaintenanceTrack"
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeClusterTracks where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"maintenanceTracks" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | /See:/ 'mkDescribeClusterTracksResponse' smart constructor.
data DescribeClusterTracksResponse = DescribeClusterTracksResponse'
  { -- | A list of maintenance tracks output by the @DescribeClusterTracks@ operation.
    maintenanceTracks :: Core.Maybe [Types.MaintenanceTrack],
    -- | The starting point to return a set of response tracklist records. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
    marker :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeClusterTracksResponse' value with any optional fields omitted.
mkDescribeClusterTracksResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeClusterTracksResponse
mkDescribeClusterTracksResponse responseStatus =
  DescribeClusterTracksResponse'
    { maintenanceTracks = Core.Nothing,
      marker = Core.Nothing,
      responseStatus
    }

-- | A list of maintenance tracks output by the @DescribeClusterTracks@ operation.
--
-- /Note:/ Consider using 'maintenanceTracks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dctrrsMaintenanceTracks :: Lens.Lens' DescribeClusterTracksResponse (Core.Maybe [Types.MaintenanceTrack])
dctrrsMaintenanceTracks = Lens.field @"maintenanceTracks"
{-# DEPRECATED dctrrsMaintenanceTracks "Use generic-lens or generic-optics with 'maintenanceTracks' instead." #-}

-- | The starting point to return a set of response tracklist records. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dctrrsMarker :: Lens.Lens' DescribeClusterTracksResponse (Core.Maybe Types.String)
dctrrsMarker = Lens.field @"marker"
{-# DEPRECATED dctrrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dctrrsResponseStatus :: Lens.Lens' DescribeClusterTracksResponse Core.Int
dctrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dctrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
