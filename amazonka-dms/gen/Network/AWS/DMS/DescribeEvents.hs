{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DescribeEvents
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists events for a given source identifier and source type. You can also
-- specify a start and end time. For more information on AWS DMS events,
-- see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Events.html Working with Events and Notifications>
-- in the /AWS Database Migration User Guide./
--
-- This operation returns paginated results.
module Network.AWS.DMS.DescribeEvents
  ( -- * Creating a Request
    DescribeEvents (..),
    newDescribeEvents,

    -- * Request Lenses
    describeEvents_duration,
    describeEvents_startTime,
    describeEvents_eventCategories,
    describeEvents_endTime,
    describeEvents_sourceIdentifier,
    describeEvents_filters,
    describeEvents_sourceType,
    describeEvents_marker,
    describeEvents_maxRecords,

    -- * Destructuring the Response
    DescribeEventsResponse (..),
    newDescribeEventsResponse,

    -- * Response Lenses
    describeEventsResponse_events,
    describeEventsResponse_marker,
    describeEventsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeEvents' smart constructor.
data DescribeEvents = DescribeEvents'
  { -- | The duration of the events to be listed.
    duration :: Prelude.Maybe Prelude.Int,
    -- | The start time for the events to be listed.
    startTime :: Prelude.Maybe Core.POSIX,
    -- | A list of event categories for the source type that you\'ve chosen.
    eventCategories :: Prelude.Maybe [Prelude.Text],
    -- | The end time for the events to be listed.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | The identifier of an event source.
    sourceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | Filters applied to events.
    filters :: Prelude.Maybe [Filter],
    -- | The type of AWS DMS resource that generates events.
    --
    -- Valid values: replication-instance | replication-task
    sourceType :: Prelude.Maybe SourceType,
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so that the remaining
    -- results can be retrieved.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEvents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'duration', 'describeEvents_duration' - The duration of the events to be listed.
--
-- 'startTime', 'describeEvents_startTime' - The start time for the events to be listed.
--
-- 'eventCategories', 'describeEvents_eventCategories' - A list of event categories for the source type that you\'ve chosen.
--
-- 'endTime', 'describeEvents_endTime' - The end time for the events to be listed.
--
-- 'sourceIdentifier', 'describeEvents_sourceIdentifier' - The identifier of an event source.
--
-- 'filters', 'describeEvents_filters' - Filters applied to events.
--
-- 'sourceType', 'describeEvents_sourceType' - The type of AWS DMS resource that generates events.
--
-- Valid values: replication-instance | replication-task
--
-- 'marker', 'describeEvents_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'maxRecords', 'describeEvents_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
newDescribeEvents ::
  DescribeEvents
newDescribeEvents =
  DescribeEvents'
    { duration = Prelude.Nothing,
      startTime = Prelude.Nothing,
      eventCategories = Prelude.Nothing,
      endTime = Prelude.Nothing,
      sourceIdentifier = Prelude.Nothing,
      filters = Prelude.Nothing,
      sourceType = Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | The duration of the events to be listed.
describeEvents_duration :: Lens.Lens' DescribeEvents (Prelude.Maybe Prelude.Int)
describeEvents_duration = Lens.lens (\DescribeEvents' {duration} -> duration) (\s@DescribeEvents' {} a -> s {duration = a} :: DescribeEvents)

-- | The start time for the events to be listed.
describeEvents_startTime :: Lens.Lens' DescribeEvents (Prelude.Maybe Prelude.UTCTime)
describeEvents_startTime = Lens.lens (\DescribeEvents' {startTime} -> startTime) (\s@DescribeEvents' {} a -> s {startTime = a} :: DescribeEvents) Prelude.. Lens.mapping Core._Time

-- | A list of event categories for the source type that you\'ve chosen.
describeEvents_eventCategories :: Lens.Lens' DescribeEvents (Prelude.Maybe [Prelude.Text])
describeEvents_eventCategories = Lens.lens (\DescribeEvents' {eventCategories} -> eventCategories) (\s@DescribeEvents' {} a -> s {eventCategories = a} :: DescribeEvents) Prelude.. Lens.mapping Lens._Coerce

-- | The end time for the events to be listed.
describeEvents_endTime :: Lens.Lens' DescribeEvents (Prelude.Maybe Prelude.UTCTime)
describeEvents_endTime = Lens.lens (\DescribeEvents' {endTime} -> endTime) (\s@DescribeEvents' {} a -> s {endTime = a} :: DescribeEvents) Prelude.. Lens.mapping Core._Time

-- | The identifier of an event source.
describeEvents_sourceIdentifier :: Lens.Lens' DescribeEvents (Prelude.Maybe Prelude.Text)
describeEvents_sourceIdentifier = Lens.lens (\DescribeEvents' {sourceIdentifier} -> sourceIdentifier) (\s@DescribeEvents' {} a -> s {sourceIdentifier = a} :: DescribeEvents)

-- | Filters applied to events.
describeEvents_filters :: Lens.Lens' DescribeEvents (Prelude.Maybe [Filter])
describeEvents_filters = Lens.lens (\DescribeEvents' {filters} -> filters) (\s@DescribeEvents' {} a -> s {filters = a} :: DescribeEvents) Prelude.. Lens.mapping Lens._Coerce

-- | The type of AWS DMS resource that generates events.
--
-- Valid values: replication-instance | replication-task
describeEvents_sourceType :: Lens.Lens' DescribeEvents (Prelude.Maybe SourceType)
describeEvents_sourceType = Lens.lens (\DescribeEvents' {sourceType} -> sourceType) (\s@DescribeEvents' {} a -> s {sourceType = a} :: DescribeEvents)

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeEvents_marker :: Lens.Lens' DescribeEvents (Prelude.Maybe Prelude.Text)
describeEvents_marker = Lens.lens (\DescribeEvents' {marker} -> marker) (\s@DescribeEvents' {} a -> s {marker = a} :: DescribeEvents)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeEvents_maxRecords :: Lens.Lens' DescribeEvents (Prelude.Maybe Prelude.Int)
describeEvents_maxRecords = Lens.lens (\DescribeEvents' {maxRecords} -> maxRecords) (\s@DescribeEvents' {} a -> s {maxRecords = a} :: DescribeEvents)

instance Core.AWSPager DescribeEvents where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeEventsResponse_marker Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeEventsResponse_events Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeEvents_marker
          Lens..~ rs
          Lens.^? describeEventsResponse_marker Prelude.. Lens._Just

instance Core.AWSRequest DescribeEvents where
  type
    AWSResponse DescribeEvents =
      DescribeEventsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEventsResponse'
            Prelude.<$> (x Core..?> "Events" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeEvents

instance Prelude.NFData DescribeEvents

instance Core.ToHeaders DescribeEvents where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.DescribeEvents" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeEvents where
  toJSON DescribeEvents' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Duration" Core..=) Prelude.<$> duration,
            ("StartTime" Core..=) Prelude.<$> startTime,
            ("EventCategories" Core..=)
              Prelude.<$> eventCategories,
            ("EndTime" Core..=) Prelude.<$> endTime,
            ("SourceIdentifier" Core..=)
              Prelude.<$> sourceIdentifier,
            ("Filters" Core..=) Prelude.<$> filters,
            ("SourceType" Core..=) Prelude.<$> sourceType,
            ("Marker" Core..=) Prelude.<$> marker,
            ("MaxRecords" Core..=) Prelude.<$> maxRecords
          ]
      )

instance Core.ToPath DescribeEvents where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeEvents where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newDescribeEventsResponse' smart constructor.
data DescribeEventsResponse = DescribeEventsResponse'
  { -- | The events described.
    events :: Prelude.Maybe [Event],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEventsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'events', 'describeEventsResponse_events' - The events described.
--
-- 'marker', 'describeEventsResponse_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'httpStatus', 'describeEventsResponse_httpStatus' - The response's http status code.
newDescribeEventsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeEventsResponse
newDescribeEventsResponse pHttpStatus_ =
  DescribeEventsResponse'
    { events = Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The events described.
describeEventsResponse_events :: Lens.Lens' DescribeEventsResponse (Prelude.Maybe [Event])
describeEventsResponse_events = Lens.lens (\DescribeEventsResponse' {events} -> events) (\s@DescribeEventsResponse' {} a -> s {events = a} :: DescribeEventsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeEventsResponse_marker :: Lens.Lens' DescribeEventsResponse (Prelude.Maybe Prelude.Text)
describeEventsResponse_marker = Lens.lens (\DescribeEventsResponse' {marker} -> marker) (\s@DescribeEventsResponse' {} a -> s {marker = a} :: DescribeEventsResponse)

-- | The response's http status code.
describeEventsResponse_httpStatus :: Lens.Lens' DescribeEventsResponse Prelude.Int
describeEventsResponse_httpStatus = Lens.lens (\DescribeEventsResponse' {httpStatus} -> httpStatus) (\s@DescribeEventsResponse' {} a -> s {httpStatus = a} :: DescribeEventsResponse)

instance Prelude.NFData DescribeEventsResponse
