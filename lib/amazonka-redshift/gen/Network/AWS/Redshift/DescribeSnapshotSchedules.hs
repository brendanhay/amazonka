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
    dssTagValues,
    dssTagKeys,
    dssClusterIdentifier,
    dssMarker,
    dssMaxRecords,
    dssScheduleIdentifier,

    -- * Destructuring the response
    DescribeSnapshotSchedulesResponse (..),
    mkDescribeSnapshotSchedulesResponse,

    -- ** Response lenses
    dssrsSnapshotSchedules,
    dssrsMarker,
    dssrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeSnapshotSchedules' smart constructor.
data DescribeSnapshotSchedules = DescribeSnapshotSchedules'
  { -- | The value corresponding to the key of the snapshot schedule tag.
    tagValues :: Lude.Maybe [Lude.Text],
    -- | The key value for a snapshot schedule tag.
    tagKeys :: Lude.Maybe [Lude.Text],
    -- | The unique identifier for the cluster whose snapshot schedules you want to view.
    clusterIdentifier :: Lude.Maybe Lude.Text,
    -- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @marker@ parameter and retrying the command. If the @marker@ field is empty, all response records have been retrieved for the request.
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number or response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned @marker@ value.
    maxRecords :: Lude.Maybe Lude.Int,
    -- | A unique identifier for a snapshot schedule.
    scheduleIdentifier :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSnapshotSchedules' with the minimum fields required to make a request.
--
-- * 'tagValues' - The value corresponding to the key of the snapshot schedule tag.
-- * 'tagKeys' - The key value for a snapshot schedule tag.
-- * 'clusterIdentifier' - The unique identifier for the cluster whose snapshot schedules you want to view.
-- * 'marker' - A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @marker@ parameter and retrying the command. If the @marker@ field is empty, all response records have been retrieved for the request.
-- * 'maxRecords' - The maximum number or response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned @marker@ value.
-- * 'scheduleIdentifier' - A unique identifier for a snapshot schedule.
mkDescribeSnapshotSchedules ::
  DescribeSnapshotSchedules
mkDescribeSnapshotSchedules =
  DescribeSnapshotSchedules'
    { tagValues = Lude.Nothing,
      tagKeys = Lude.Nothing,
      clusterIdentifier = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing,
      scheduleIdentifier = Lude.Nothing
    }

-- | The value corresponding to the key of the snapshot schedule tag.
--
-- /Note:/ Consider using 'tagValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssTagValues :: Lens.Lens' DescribeSnapshotSchedules (Lude.Maybe [Lude.Text])
dssTagValues = Lens.lens (tagValues :: DescribeSnapshotSchedules -> Lude.Maybe [Lude.Text]) (\s a -> s {tagValues = a} :: DescribeSnapshotSchedules)
{-# DEPRECATED dssTagValues "Use generic-lens or generic-optics with 'tagValues' instead." #-}

-- | The key value for a snapshot schedule tag.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssTagKeys :: Lens.Lens' DescribeSnapshotSchedules (Lude.Maybe [Lude.Text])
dssTagKeys = Lens.lens (tagKeys :: DescribeSnapshotSchedules -> Lude.Maybe [Lude.Text]) (\s a -> s {tagKeys = a} :: DescribeSnapshotSchedules)
{-# DEPRECATED dssTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

-- | The unique identifier for the cluster whose snapshot schedules you want to view.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssClusterIdentifier :: Lens.Lens' DescribeSnapshotSchedules (Lude.Maybe Lude.Text)
dssClusterIdentifier = Lens.lens (clusterIdentifier :: DescribeSnapshotSchedules -> Lude.Maybe Lude.Text) (\s a -> s {clusterIdentifier = a} :: DescribeSnapshotSchedules)
{-# DEPRECATED dssClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

-- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @marker@ parameter and retrying the command. If the @marker@ field is empty, all response records have been retrieved for the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssMarker :: Lens.Lens' DescribeSnapshotSchedules (Lude.Maybe Lude.Text)
dssMarker = Lens.lens (marker :: DescribeSnapshotSchedules -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeSnapshotSchedules)
{-# DEPRECATED dssMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number or response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned @marker@ value.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssMaxRecords :: Lens.Lens' DescribeSnapshotSchedules (Lude.Maybe Lude.Int)
dssMaxRecords = Lens.lens (maxRecords :: DescribeSnapshotSchedules -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeSnapshotSchedules)
{-# DEPRECATED dssMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | A unique identifier for a snapshot schedule.
--
-- /Note:/ Consider using 'scheduleIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssScheduleIdentifier :: Lens.Lens' DescribeSnapshotSchedules (Lude.Maybe Lude.Text)
dssScheduleIdentifier = Lens.lens (scheduleIdentifier :: DescribeSnapshotSchedules -> Lude.Maybe Lude.Text) (\s a -> s {scheduleIdentifier = a} :: DescribeSnapshotSchedules)
{-# DEPRECATED dssScheduleIdentifier "Use generic-lens or generic-optics with 'scheduleIdentifier' instead." #-}

instance Page.AWSPager DescribeSnapshotSchedules where
  page rq rs
    | Page.stop (rs Lens.^. dssrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dssrsSnapshotSchedules) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& dssMarker Lens..~ rs Lens.^. dssrsMarker

instance Lude.AWSRequest DescribeSnapshotSchedules where
  type
    Rs DescribeSnapshotSchedules =
      DescribeSnapshotSchedulesResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "DescribeSnapshotSchedulesResult"
      ( \s h x ->
          DescribeSnapshotSchedulesResponse'
            Lude.<$> ( x Lude..@? "SnapshotSchedules" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "SnapshotSchedule")
                     )
            Lude.<*> (x Lude..@? "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeSnapshotSchedules where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeSnapshotSchedules where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeSnapshotSchedules where
  toQuery DescribeSnapshotSchedules' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeSnapshotSchedules" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "TagValues"
          Lude.=: Lude.toQuery (Lude.toQueryList "TagValue" Lude.<$> tagValues),
        "TagKeys"
          Lude.=: Lude.toQuery (Lude.toQueryList "TagKey" Lude.<$> tagKeys),
        "ClusterIdentifier" Lude.=: clusterIdentifier,
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords,
        "ScheduleIdentifier" Lude.=: scheduleIdentifier
      ]

-- | /See:/ 'mkDescribeSnapshotSchedulesResponse' smart constructor.
data DescribeSnapshotSchedulesResponse = DescribeSnapshotSchedulesResponse'
  { -- | A list of SnapshotSchedules.
    snapshotSchedules :: Lude.Maybe [SnapshotSchedule],
    -- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @marker@ parameter and retrying the command. If the @marker@ field is empty, all response records have been retrieved for the request.
    marker :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSnapshotSchedulesResponse' with the minimum fields required to make a request.
--
-- * 'snapshotSchedules' - A list of SnapshotSchedules.
-- * 'marker' - A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @marker@ parameter and retrying the command. If the @marker@ field is empty, all response records have been retrieved for the request.
-- * 'responseStatus' - The response status code.
mkDescribeSnapshotSchedulesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeSnapshotSchedulesResponse
mkDescribeSnapshotSchedulesResponse pResponseStatus_ =
  DescribeSnapshotSchedulesResponse'
    { snapshotSchedules =
        Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of SnapshotSchedules.
--
-- /Note:/ Consider using 'snapshotSchedules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrsSnapshotSchedules :: Lens.Lens' DescribeSnapshotSchedulesResponse (Lude.Maybe [SnapshotSchedule])
dssrsSnapshotSchedules = Lens.lens (snapshotSchedules :: DescribeSnapshotSchedulesResponse -> Lude.Maybe [SnapshotSchedule]) (\s a -> s {snapshotSchedules = a} :: DescribeSnapshotSchedulesResponse)
{-# DEPRECATED dssrsSnapshotSchedules "Use generic-lens or generic-optics with 'snapshotSchedules' instead." #-}

-- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @marker@ parameter and retrying the command. If the @marker@ field is empty, all response records have been retrieved for the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrsMarker :: Lens.Lens' DescribeSnapshotSchedulesResponse (Lude.Maybe Lude.Text)
dssrsMarker = Lens.lens (marker :: DescribeSnapshotSchedulesResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeSnapshotSchedulesResponse)
{-# DEPRECATED dssrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrsResponseStatus :: Lens.Lens' DescribeSnapshotSchedulesResponse Lude.Int
dssrsResponseStatus = Lens.lens (responseStatus :: DescribeSnapshotSchedulesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeSnapshotSchedulesResponse)
{-# DEPRECATED dssrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
