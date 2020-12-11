{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    dctrsMaintenanceTracks,
    dctrsMarker,
    dctrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeClusterTracks' smart constructor.
data DescribeClusterTracks = DescribeClusterTracks'
  { maintenanceTrackName ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DescribeClusterTracks' with the minimum fields required to make a request.
--
-- * 'maintenanceTrackName' - The name of the maintenance track.
-- * 'marker' - An optional parameter that specifies the starting point to return a set of response records. When the results of a @DescribeClusterTracks@ request exceed the value specified in @MaxRecords@ , Amazon Redshift returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
-- * 'maxRecords' - An integer value for the maximum number of maintenance tracks to return.
mkDescribeClusterTracks ::
  DescribeClusterTracks
mkDescribeClusterTracks =
  DescribeClusterTracks'
    { maintenanceTrackName = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing
    }

-- | The name of the maintenance track.
--
-- /Note:/ Consider using 'maintenanceTrackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dctMaintenanceTrackName :: Lens.Lens' DescribeClusterTracks (Lude.Maybe Lude.Text)
dctMaintenanceTrackName = Lens.lens (maintenanceTrackName :: DescribeClusterTracks -> Lude.Maybe Lude.Text) (\s a -> s {maintenanceTrackName = a} :: DescribeClusterTracks)
{-# DEPRECATED dctMaintenanceTrackName "Use generic-lens or generic-optics with 'maintenanceTrackName' instead." #-}

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a @DescribeClusterTracks@ request exceed the value specified in @MaxRecords@ , Amazon Redshift returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dctMarker :: Lens.Lens' DescribeClusterTracks (Lude.Maybe Lude.Text)
dctMarker = Lens.lens (marker :: DescribeClusterTracks -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeClusterTracks)
{-# DEPRECATED dctMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | An integer value for the maximum number of maintenance tracks to return.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dctMaxRecords :: Lens.Lens' DescribeClusterTracks (Lude.Maybe Lude.Int)
dctMaxRecords = Lens.lens (maxRecords :: DescribeClusterTracks -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeClusterTracks)
{-# DEPRECATED dctMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Page.AWSPager DescribeClusterTracks where
  page rq rs
    | Page.stop (rs Lens.^. dctrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dctrsMaintenanceTracks) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& dctMarker Lens..~ rs Lens.^. dctrsMarker

instance Lude.AWSRequest DescribeClusterTracks where
  type Rs DescribeClusterTracks = DescribeClusterTracksResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "DescribeClusterTracksResult"
      ( \s h x ->
          DescribeClusterTracksResponse'
            Lude.<$> ( x Lude..@? "MaintenanceTracks" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "MaintenanceTrack")
                     )
            Lude.<*> (x Lude..@? "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeClusterTracks where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeClusterTracks where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeClusterTracks where
  toQuery DescribeClusterTracks' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeClusterTracks" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "MaintenanceTrackName" Lude.=: maintenanceTrackName,
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords
      ]

-- | /See:/ 'mkDescribeClusterTracksResponse' smart constructor.
data DescribeClusterTracksResponse = DescribeClusterTracksResponse'
  { maintenanceTracks ::
      Lude.Maybe [MaintenanceTrack],
    marker :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DescribeClusterTracksResponse' with the minimum fields required to make a request.
--
-- * 'maintenanceTracks' - A list of maintenance tracks output by the @DescribeClusterTracks@ operation.
-- * 'marker' - The starting point to return a set of response tracklist records. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
-- * 'responseStatus' - The response status code.
mkDescribeClusterTracksResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeClusterTracksResponse
mkDescribeClusterTracksResponse pResponseStatus_ =
  DescribeClusterTracksResponse'
    { maintenanceTracks = Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of maintenance tracks output by the @DescribeClusterTracks@ operation.
--
-- /Note:/ Consider using 'maintenanceTracks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dctrsMaintenanceTracks :: Lens.Lens' DescribeClusterTracksResponse (Lude.Maybe [MaintenanceTrack])
dctrsMaintenanceTracks = Lens.lens (maintenanceTracks :: DescribeClusterTracksResponse -> Lude.Maybe [MaintenanceTrack]) (\s a -> s {maintenanceTracks = a} :: DescribeClusterTracksResponse)
{-# DEPRECATED dctrsMaintenanceTracks "Use generic-lens or generic-optics with 'maintenanceTracks' instead." #-}

-- | The starting point to return a set of response tracklist records. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dctrsMarker :: Lens.Lens' DescribeClusterTracksResponse (Lude.Maybe Lude.Text)
dctrsMarker = Lens.lens (marker :: DescribeClusterTracksResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeClusterTracksResponse)
{-# DEPRECATED dctrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dctrsResponseStatus :: Lens.Lens' DescribeClusterTracksResponse Lude.Int
dctrsResponseStatus = Lens.lens (responseStatus :: DescribeClusterTracksResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeClusterTracksResponse)
{-# DEPRECATED dctrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
