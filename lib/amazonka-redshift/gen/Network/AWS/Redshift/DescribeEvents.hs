{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeEvents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns events related to clusters, security groups, snapshots, and parameter groups for the past 14 days. Events specific to a particular cluster, security group, snapshot or parameter group can be obtained by providing the name as a parameter. By default, the past hour of events are returned.
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeEvents
  ( -- * Creating a request
    DescribeEvents (..),
    mkDescribeEvents,

    -- ** Request lenses
    deStartTime,
    deSourceType,
    deSourceIdentifier,
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
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDescribeEvents' smart constructor.
data DescribeEvents = DescribeEvents'
  { startTime ::
      Lude.Maybe Lude.DateTime,
    sourceType :: Lude.Maybe SourceType,
    sourceIdentifier :: Lude.Maybe Lude.Text,
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
-- * 'duration' - The number of minutes prior to the time of the request for which to retrieve events. For example, if the request is sent at 18:00 and you specify a duration of 60, then only events which have occurred after 17:00 will be returned.
--
-- Default: @60@
-- * 'endTime' - The end of the time interval for which to retrieve events, specified in ISO 8601 format. For more information about ISO 8601, go to the <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.>
--
-- Example: @2009-07-08T18:00Z@
-- * 'marker' - An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeEvents' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
-- * 'maxRecords' - The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
--
-- Default: @100@
-- Constraints: minimum 20, maximum 100.
-- * 'sourceIdentifier' - The identifier of the event source for which events will be returned. If this parameter is not specified, then all sources are included in the response.
--
-- Constraints:
-- If /SourceIdentifier/ is supplied, /SourceType/ must also be provided.
--
--     * Specify a cluster identifier when /SourceType/ is @cluster@ .
--
--
--     * Specify a cluster security group name when /SourceType/ is @cluster-security-group@ .
--
--
--     * Specify a cluster parameter group name when /SourceType/ is @cluster-parameter-group@ .
--
--
--     * Specify a cluster snapshot identifier when /SourceType/ is @cluster-snapshot@ .
--
--
-- * 'sourceType' - The event source to retrieve events for. If no value is specified, all events are returned.
--
-- Constraints:
-- If /SourceType/ is supplied, /SourceIdentifier/ must also be provided.
--
--     * Specify @cluster@ when /SourceIdentifier/ is a cluster identifier.
--
--
--     * Specify @cluster-security-group@ when /SourceIdentifier/ is a cluster security group name.
--
--
--     * Specify @cluster-parameter-group@ when /SourceIdentifier/ is a cluster parameter group name.
--
--
--     * Specify @cluster-snapshot@ when /SourceIdentifier/ is a cluster snapshot identifier.
--
--
-- * 'startTime' - The beginning of the time interval to retrieve events for, specified in ISO 8601 format. For more information about ISO 8601, go to the <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.>
--
-- Example: @2009-07-08T18:00Z@
mkDescribeEvents ::
  DescribeEvents
mkDescribeEvents =
  DescribeEvents'
    { startTime = Lude.Nothing,
      sourceType = Lude.Nothing,
      sourceIdentifier = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing,
      endTime = Lude.Nothing,
      duration = Lude.Nothing
    }

-- | The beginning of the time interval to retrieve events for, specified in ISO 8601 format. For more information about ISO 8601, go to the <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.>
--
-- Example: @2009-07-08T18:00Z@
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deStartTime :: Lens.Lens' DescribeEvents (Lude.Maybe Lude.DateTime)
deStartTime = Lens.lens (startTime :: DescribeEvents -> Lude.Maybe Lude.DateTime) (\s a -> s {startTime = a} :: DescribeEvents)
{-# DEPRECATED deStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The event source to retrieve events for. If no value is specified, all events are returned.
--
-- Constraints:
-- If /SourceType/ is supplied, /SourceIdentifier/ must also be provided.
--
--     * Specify @cluster@ when /SourceIdentifier/ is a cluster identifier.
--
--
--     * Specify @cluster-security-group@ when /SourceIdentifier/ is a cluster security group name.
--
--
--     * Specify @cluster-parameter-group@ when /SourceIdentifier/ is a cluster parameter group name.
--
--
--     * Specify @cluster-snapshot@ when /SourceIdentifier/ is a cluster snapshot identifier.
--
--
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deSourceType :: Lens.Lens' DescribeEvents (Lude.Maybe SourceType)
deSourceType = Lens.lens (sourceType :: DescribeEvents -> Lude.Maybe SourceType) (\s a -> s {sourceType = a} :: DescribeEvents)
{-# DEPRECATED deSourceType "Use generic-lens or generic-optics with 'sourceType' instead." #-}

-- | The identifier of the event source for which events will be returned. If this parameter is not specified, then all sources are included in the response.
--
-- Constraints:
-- If /SourceIdentifier/ is supplied, /SourceType/ must also be provided.
--
--     * Specify a cluster identifier when /SourceType/ is @cluster@ .
--
--
--     * Specify a cluster security group name when /SourceType/ is @cluster-security-group@ .
--
--
--     * Specify a cluster parameter group name when /SourceType/ is @cluster-parameter-group@ .
--
--
--     * Specify a cluster snapshot identifier when /SourceType/ is @cluster-snapshot@ .
--
--
--
-- /Note:/ Consider using 'sourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deSourceIdentifier :: Lens.Lens' DescribeEvents (Lude.Maybe Lude.Text)
deSourceIdentifier = Lens.lens (sourceIdentifier :: DescribeEvents -> Lude.Maybe Lude.Text) (\s a -> s {sourceIdentifier = a} :: DescribeEvents)
{-# DEPRECATED deSourceIdentifier "Use generic-lens or generic-optics with 'sourceIdentifier' instead." #-}

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeEvents' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deMarker :: Lens.Lens' DescribeEvents (Lude.Maybe Lude.Text)
deMarker = Lens.lens (marker :: DescribeEvents -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeEvents)
{-# DEPRECATED deMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
--
-- Default: @100@
-- Constraints: minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deMaxRecords :: Lens.Lens' DescribeEvents (Lude.Maybe Lude.Int)
deMaxRecords = Lens.lens (maxRecords :: DescribeEvents -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeEvents)
{-# DEPRECATED deMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The end of the time interval for which to retrieve events, specified in ISO 8601 format. For more information about ISO 8601, go to the <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.>
--
-- Example: @2009-07-08T18:00Z@
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deEndTime :: Lens.Lens' DescribeEvents (Lude.Maybe Lude.DateTime)
deEndTime = Lens.lens (endTime :: DescribeEvents -> Lude.Maybe Lude.DateTime) (\s a -> s {endTime = a} :: DescribeEvents)
{-# DEPRECATED deEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The number of minutes prior to the time of the request for which to retrieve events. For example, if the request is sent at 18:00 and you specify a duration of 60, then only events which have occurred after 17:00 will be returned.
--
-- Default: @60@
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
  request = Req.postQuery redshiftService
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
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "StartTime" Lude.=: startTime,
        "SourceType" Lude.=: sourceType,
        "SourceIdentifier" Lude.=: sourceIdentifier,
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords,
        "EndTime" Lude.=: endTime,
        "Duration" Lude.=: duration
      ]

-- |
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
-- * 'marker' - A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
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

-- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
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
