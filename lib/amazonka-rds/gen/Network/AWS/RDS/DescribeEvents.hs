{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeEvents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns events related to DB instances, DB clusters, DB parameter groups, DB security groups, DB snapshots, and DB cluster snapshots for the past 14 days. Events specific to a particular DB instances, DB clusters, DB parameter groups, DB security groups, DB snapshots, and DB cluster snapshots group can be obtained by providing the name as a parameter.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeEvents
  ( -- * Creating a request
    DescribeEvents (..),
    mkDescribeEvents,

    -- ** Request lenses
    deStartTime,
    deSourceType,
    deFilters,
    deSourceIdentifier,
    deEventCategories,
    deMarker,
    deMaxRecords,
    deEndTime,
    deDuration,

    -- * Destructuring the response
    DescribeEventsResponse (..),
    mkDescribeEventsResponse,

    -- ** Response lenses
    dersEvents,
    dersMarker,
    dersResponseStatus,
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
-- /See:/ 'mkDescribeEvents' smart constructor.
data DescribeEvents = DescribeEvents'
  { startTime ::
      Lude.Maybe Lude.DateTime,
    sourceType :: Lude.Maybe SourceType,
    filters :: Lude.Maybe [Filter],
    sourceIdentifier :: Lude.Maybe Lude.Text,
    eventCategories :: Lude.Maybe [Lude.Text],
    marker :: Lude.Maybe Lude.Text,
    maxRecords :: Lude.Maybe Lude.Int,
    endTime :: Lude.Maybe Lude.DateTime,
    duration :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEvents' with the minimum fields required to make a request.
--
-- * 'duration' - The number of minutes to retrieve events for.
--
-- Default: 60
-- * 'endTime' - The end of the time interval for which to retrieve events, specified in ISO 8601 format. For more information about ISO 8601, go to the <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.>
--
-- Example: 2009-07-08T18:00Z
-- * 'eventCategories' - A list of event categories that trigger notifications for a event notification subscription.
-- * 'filters' - This parameter isn't currently supported.
-- * 'marker' - An optional pagination token provided by a previous DescribeEvents request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
-- * 'sourceIdentifier' - The identifier of the event source for which events are returned. If not specified, then all sources are included in the response.
--
-- Constraints:
--
--     * If @SourceIdentifier@ is supplied, @SourceType@ must also be provided.
--
--
--     * If the source type is a DB instance, a @DBInstanceIdentifier@ value must be supplied.
--
--
--     * If the source type is a DB cluster, a @DBClusterIdentifier@ value must be supplied.
--
--
--     * If the source type is a DB parameter group, a @DBParameterGroupName@ value must be supplied.
--
--
--     * If the source type is a DB security group, a @DBSecurityGroupName@ value must be supplied.
--
--
--     * If the source type is a DB snapshot, a @DBSnapshotIdentifier@ value must be supplied.
--
--
--     * If the source type is a DB cluster snapshot, a @DBClusterSnapshotIdentifier@ value must be supplied.
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens.
--
--
-- * 'sourceType' - The event source to retrieve events for. If no value is specified, all events are returned.
-- * 'startTime' - The beginning of the time interval to retrieve events for, specified in ISO 8601 format. For more information about ISO 8601, go to the <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.>
--
-- Example: 2009-07-08T18:00Z
mkDescribeEvents ::
  DescribeEvents
mkDescribeEvents =
  DescribeEvents'
    { startTime = Lude.Nothing,
      sourceType = Lude.Nothing,
      filters = Lude.Nothing,
      sourceIdentifier = Lude.Nothing,
      eventCategories = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing,
      endTime = Lude.Nothing,
      duration = Lude.Nothing
    }

-- | The beginning of the time interval to retrieve events for, specified in ISO 8601 format. For more information about ISO 8601, go to the <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.>
--
-- Example: 2009-07-08T18:00Z
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deStartTime :: Lens.Lens' DescribeEvents (Lude.Maybe Lude.DateTime)
deStartTime = Lens.lens (startTime :: DescribeEvents -> Lude.Maybe Lude.DateTime) (\s a -> s {startTime = a} :: DescribeEvents)
{-# DEPRECATED deStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The event source to retrieve events for. If no value is specified, all events are returned.
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deSourceType :: Lens.Lens' DescribeEvents (Lude.Maybe SourceType)
deSourceType = Lens.lens (sourceType :: DescribeEvents -> Lude.Maybe SourceType) (\s a -> s {sourceType = a} :: DescribeEvents)
{-# DEPRECATED deSourceType "Use generic-lens or generic-optics with 'sourceType' instead." #-}

-- | This parameter isn't currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deFilters :: Lens.Lens' DescribeEvents (Lude.Maybe [Filter])
deFilters = Lens.lens (filters :: DescribeEvents -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeEvents)
{-# DEPRECATED deFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The identifier of the event source for which events are returned. If not specified, then all sources are included in the response.
--
-- Constraints:
--
--     * If @SourceIdentifier@ is supplied, @SourceType@ must also be provided.
--
--
--     * If the source type is a DB instance, a @DBInstanceIdentifier@ value must be supplied.
--
--
--     * If the source type is a DB cluster, a @DBClusterIdentifier@ value must be supplied.
--
--
--     * If the source type is a DB parameter group, a @DBParameterGroupName@ value must be supplied.
--
--
--     * If the source type is a DB security group, a @DBSecurityGroupName@ value must be supplied.
--
--
--     * If the source type is a DB snapshot, a @DBSnapshotIdentifier@ value must be supplied.
--
--
--     * If the source type is a DB cluster snapshot, a @DBClusterSnapshotIdentifier@ value must be supplied.
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens.
--
--
--
-- /Note:/ Consider using 'sourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deSourceIdentifier :: Lens.Lens' DescribeEvents (Lude.Maybe Lude.Text)
deSourceIdentifier = Lens.lens (sourceIdentifier :: DescribeEvents -> Lude.Maybe Lude.Text) (\s a -> s {sourceIdentifier = a} :: DescribeEvents)
{-# DEPRECATED deSourceIdentifier "Use generic-lens or generic-optics with 'sourceIdentifier' instead." #-}

-- | A list of event categories that trigger notifications for a event notification subscription.
--
-- /Note:/ Consider using 'eventCategories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deEventCategories :: Lens.Lens' DescribeEvents (Lude.Maybe [Lude.Text])
deEventCategories = Lens.lens (eventCategories :: DescribeEvents -> Lude.Maybe [Lude.Text]) (\s a -> s {eventCategories = a} :: DescribeEvents)
{-# DEPRECATED deEventCategories "Use generic-lens or generic-optics with 'eventCategories' instead." #-}

-- | An optional pagination token provided by a previous DescribeEvents request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deMarker :: Lens.Lens' DescribeEvents (Lude.Maybe Lude.Text)
deMarker = Lens.lens (marker :: DescribeEvents -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeEvents)
{-# DEPRECATED deMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deMaxRecords :: Lens.Lens' DescribeEvents (Lude.Maybe Lude.Int)
deMaxRecords = Lens.lens (maxRecords :: DescribeEvents -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeEvents)
{-# DEPRECATED deMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The end of the time interval for which to retrieve events, specified in ISO 8601 format. For more information about ISO 8601, go to the <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.>
--
-- Example: 2009-07-08T18:00Z
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deEndTime :: Lens.Lens' DescribeEvents (Lude.Maybe Lude.DateTime)
deEndTime = Lens.lens (endTime :: DescribeEvents -> Lude.Maybe Lude.DateTime) (\s a -> s {endTime = a} :: DescribeEvents)
{-# DEPRECATED deEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The number of minutes to retrieve events for.
--
-- Default: 60
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deDuration :: Lens.Lens' DescribeEvents (Lude.Maybe Lude.Int)
deDuration = Lens.lens (duration :: DescribeEvents -> Lude.Maybe Lude.Int) (\s a -> s {duration = a} :: DescribeEvents)
{-# DEPRECATED deDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

instance Page.AWSPager DescribeEvents where
  page rq rs
    | Page.stop (rs Lens.^. dersMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dersEvents) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& deMarker Lens..~ rs Lens.^. dersMarker

instance Lude.AWSRequest DescribeEvents where
  type Rs DescribeEvents = DescribeEventsResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "DescribeEventsResult"
      ( \s h x ->
          DescribeEventsResponse'
            Lude.<$> ( x Lude..@? "Events" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "Event")
                     )
            Lude.<*> (x Lude..@? "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeEvents where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeEvents where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeEvents where
  toQuery DescribeEvents' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeEvents" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "StartTime" Lude.=: startTime,
        "SourceType" Lude.=: sourceType,
        "Filters"
          Lude.=: Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "SourceIdentifier" Lude.=: sourceIdentifier,
        "EventCategories"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "EventCategory" Lude.<$> eventCategories),
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords,
        "EndTime" Lude.=: endTime,
        "Duration" Lude.=: duration
      ]

-- | Contains the result of a successful invocation of the @DescribeEvents@ action.
--
-- /See:/ 'mkDescribeEventsResponse' smart constructor.
data DescribeEventsResponse = DescribeEventsResponse'
  { events ::
      Lude.Maybe [Event],
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

-- | Creates a value of 'DescribeEventsResponse' with the minimum fields required to make a request.
--
-- * 'events' - A list of @Event@ instances.
-- * 'marker' - An optional pagination token provided by a previous Events request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'responseStatus' - The response status code.
mkDescribeEventsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeEventsResponse
mkDescribeEventsResponse pResponseStatus_ =
  DescribeEventsResponse'
    { events = Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of @Event@ instances.
--
-- /Note:/ Consider using 'events' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersEvents :: Lens.Lens' DescribeEventsResponse (Lude.Maybe [Event])
dersEvents = Lens.lens (events :: DescribeEventsResponse -> Lude.Maybe [Event]) (\s a -> s {events = a} :: DescribeEventsResponse)
{-# DEPRECATED dersEvents "Use generic-lens or generic-optics with 'events' instead." #-}

-- | An optional pagination token provided by a previous Events request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersMarker :: Lens.Lens' DescribeEventsResponse (Lude.Maybe Lude.Text)
dersMarker = Lens.lens (marker :: DescribeEventsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeEventsResponse)
{-# DEPRECATED dersMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersResponseStatus :: Lens.Lens' DescribeEventsResponse Lude.Int
dersResponseStatus = Lens.lens (responseStatus :: DescribeEventsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeEventsResponse)
{-# DEPRECATED dersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
