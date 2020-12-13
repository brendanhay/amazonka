{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DescribeEvents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists events for a given source identifier and source type. You can also specify a start and end time. For more information on AWS DMS events, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Events.html Working with Events and Notifications> in the /AWS Database Migration User Guide./
--
-- This operation returns paginated results.
module Network.AWS.DMS.DescribeEvents
  ( -- * Creating a request
    DescribeEvents (..),
    mkDescribeEvents,

    -- ** Request lenses
    defStartTime,
    defSourceType,
    defFilters,
    defSourceIdentifier,
    defEventCategories,
    defMarker,
    defMaxRecords,
    defEndTime,
    defDuration,

    -- * Destructuring the response
    DescribeEventsResponse (..),
    mkDescribeEventsResponse,

    -- ** Response lenses
    degrsEvents,
    degrsMarker,
    degrsResponseStatus,
  )
where

import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDescribeEvents' smart constructor.
data DescribeEvents = DescribeEvents'
  { -- | The start time for the events to be listed.
    startTime :: Lude.Maybe Lude.Timestamp,
    -- | The type of AWS DMS resource that generates events.
    --
    -- Valid values: replication-instance | replication-task
    sourceType :: Lude.Maybe SourceType,
    -- | Filters applied to events.
    filters :: Lude.Maybe [Filter],
    -- | The identifier of an event source.
    sourceIdentifier :: Lude.Maybe Lude.Text,
    -- | A list of event categories for the source type that you've chosen.
    eventCategories :: Lude.Maybe [Lude.Text],
    -- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
    --
    -- Default: 100
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Lude.Maybe Lude.Int,
    -- | The end time for the events to be listed.
    endTime :: Lude.Maybe Lude.Timestamp,
    -- | The duration of the events to be listed.
    duration :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEvents' with the minimum fields required to make a request.
--
-- * 'startTime' - The start time for the events to be listed.
-- * 'sourceType' - The type of AWS DMS resource that generates events.
--
-- Valid values: replication-instance | replication-task
-- * 'filters' - Filters applied to events.
-- * 'sourceIdentifier' - The identifier of an event source.
-- * 'eventCategories' - A list of event categories for the source type that you've chosen.
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
-- * 'endTime' - The end time for the events to be listed.
-- * 'duration' - The duration of the events to be listed.
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

-- | The start time for the events to be listed.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
defStartTime :: Lens.Lens' DescribeEvents (Lude.Maybe Lude.Timestamp)
defStartTime = Lens.lens (startTime :: DescribeEvents -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: DescribeEvents)
{-# DEPRECATED defStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The type of AWS DMS resource that generates events.
--
-- Valid values: replication-instance | replication-task
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
defSourceType :: Lens.Lens' DescribeEvents (Lude.Maybe SourceType)
defSourceType = Lens.lens (sourceType :: DescribeEvents -> Lude.Maybe SourceType) (\s a -> s {sourceType = a} :: DescribeEvents)
{-# DEPRECATED defSourceType "Use generic-lens or generic-optics with 'sourceType' instead." #-}

-- | Filters applied to events.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
defFilters :: Lens.Lens' DescribeEvents (Lude.Maybe [Filter])
defFilters = Lens.lens (filters :: DescribeEvents -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeEvents)
{-# DEPRECATED defFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The identifier of an event source.
--
-- /Note:/ Consider using 'sourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
defSourceIdentifier :: Lens.Lens' DescribeEvents (Lude.Maybe Lude.Text)
defSourceIdentifier = Lens.lens (sourceIdentifier :: DescribeEvents -> Lude.Maybe Lude.Text) (\s a -> s {sourceIdentifier = a} :: DescribeEvents)
{-# DEPRECATED defSourceIdentifier "Use generic-lens or generic-optics with 'sourceIdentifier' instead." #-}

-- | A list of event categories for the source type that you've chosen.
--
-- /Note:/ Consider using 'eventCategories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
defEventCategories :: Lens.Lens' DescribeEvents (Lude.Maybe [Lude.Text])
defEventCategories = Lens.lens (eventCategories :: DescribeEvents -> Lude.Maybe [Lude.Text]) (\s a -> s {eventCategories = a} :: DescribeEvents)
{-# DEPRECATED defEventCategories "Use generic-lens or generic-optics with 'eventCategories' instead." #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
defMarker :: Lens.Lens' DescribeEvents (Lude.Maybe Lude.Text)
defMarker = Lens.lens (marker :: DescribeEvents -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeEvents)
{-# DEPRECATED defMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
defMaxRecords :: Lens.Lens' DescribeEvents (Lude.Maybe Lude.Int)
defMaxRecords = Lens.lens (maxRecords :: DescribeEvents -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeEvents)
{-# DEPRECATED defMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The end time for the events to be listed.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
defEndTime :: Lens.Lens' DescribeEvents (Lude.Maybe Lude.Timestamp)
defEndTime = Lens.lens (endTime :: DescribeEvents -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: DescribeEvents)
{-# DEPRECATED defEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The duration of the events to be listed.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
defDuration :: Lens.Lens' DescribeEvents (Lude.Maybe Lude.Int)
defDuration = Lens.lens (duration :: DescribeEvents -> Lude.Maybe Lude.Int) (\s a -> s {duration = a} :: DescribeEvents)
{-# DEPRECATED defDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

instance Page.AWSPager DescribeEvents where
  page rq rs
    | Page.stop (rs Lens.^. degrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. degrsEvents) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& defMarker Lens..~ rs Lens.^. degrsMarker

instance Lude.AWSRequest DescribeEvents where
  type Rs DescribeEvents = DescribeEventsResponse
  request = Req.postJSON dmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeEventsResponse'
            Lude.<$> (x Lude..?> "Events" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeEvents where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonDMSv20160101.DescribeEvents" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeEvents where
  toJSON DescribeEvents' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("StartTime" Lude..=) Lude.<$> startTime,
            ("SourceType" Lude..=) Lude.<$> sourceType,
            ("Filters" Lude..=) Lude.<$> filters,
            ("SourceIdentifier" Lude..=) Lude.<$> sourceIdentifier,
            ("EventCategories" Lude..=) Lude.<$> eventCategories,
            ("Marker" Lude..=) Lude.<$> marker,
            ("MaxRecords" Lude..=) Lude.<$> maxRecords,
            ("EndTime" Lude..=) Lude.<$> endTime,
            ("Duration" Lude..=) Lude.<$> duration
          ]
      )

instance Lude.ToPath DescribeEvents where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeEvents where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkDescribeEventsResponse' smart constructor.
data DescribeEventsResponse = DescribeEventsResponse'
  { -- | The events described.
    events :: Lude.Maybe [Event],
    -- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEventsResponse' with the minimum fields required to make a request.
--
-- * 'events' - The events described.
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
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

-- | The events described.
--
-- /Note:/ Consider using 'events' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
degrsEvents :: Lens.Lens' DescribeEventsResponse (Lude.Maybe [Event])
degrsEvents = Lens.lens (events :: DescribeEventsResponse -> Lude.Maybe [Event]) (\s a -> s {events = a} :: DescribeEventsResponse)
{-# DEPRECATED degrsEvents "Use generic-lens or generic-optics with 'events' instead." #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
degrsMarker :: Lens.Lens' DescribeEventsResponse (Lude.Maybe Lude.Text)
degrsMarker = Lens.lens (marker :: DescribeEventsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeEventsResponse)
{-# DEPRECATED degrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
degrsResponseStatus :: Lens.Lens' DescribeEventsResponse Lude.Int
degrsResponseStatus = Lens.lens (responseStatus :: DescribeEventsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeEventsResponse)
{-# DEPRECATED degrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
