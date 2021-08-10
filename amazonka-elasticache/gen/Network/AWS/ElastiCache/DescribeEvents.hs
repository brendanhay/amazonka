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
-- Module      : Network.AWS.ElastiCache.DescribeEvents
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns events related to clusters, cache security groups, and cache
-- parameter groups. You can obtain events specific to a particular
-- cluster, cache security group, or cache parameter group by providing the
-- name as a parameter.
--
-- By default, only the events occurring within the last hour are returned;
-- however, you can retrieve up to 14 days\' worth of events if necessary.
--
-- This operation returns paginated results.
module Network.AWS.ElastiCache.DescribeEvents
  ( -- * Creating a Request
    DescribeEvents (..),
    newDescribeEvents,

    -- * Request Lenses
    describeEvents_duration,
    describeEvents_startTime,
    describeEvents_endTime,
    describeEvents_sourceIdentifier,
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
import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DescribeEvents@ operation.
--
-- /See:/ 'newDescribeEvents' smart constructor.
data DescribeEvents = DescribeEvents'
  { -- | The number of minutes worth of events to retrieve.
    duration :: Prelude.Maybe Prelude.Int,
    -- | The beginning of the time interval to retrieve events for, specified in
    -- ISO 8601 format.
    --
    -- __Example:__ 2017-03-30T07:03:49.555Z
    startTime :: Prelude.Maybe Core.ISO8601,
    -- | The end of the time interval for which to retrieve events, specified in
    -- ISO 8601 format.
    --
    -- __Example:__ 2017-03-30T07:03:49.555Z
    endTime :: Prelude.Maybe Core.ISO8601,
    -- | The identifier of the event source for which events are returned. If not
    -- specified, all sources are included in the response.
    sourceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The event source to retrieve events for. If no value is specified, all
    -- events are returned.
    sourceType :: Prelude.Maybe SourceType,
    -- | An optional marker returned from a prior request. Use this marker for
    -- pagination of results from this operation. If this parameter is
    -- specified, the response includes only records beyond the marker, up to
    -- the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a marker is
    -- included in the response so that the remaining results can be retrieved.
    --
    -- Default: 100
    --
    -- Constraints: minimum 20; maximum 100.
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
-- 'duration', 'describeEvents_duration' - The number of minutes worth of events to retrieve.
--
-- 'startTime', 'describeEvents_startTime' - The beginning of the time interval to retrieve events for, specified in
-- ISO 8601 format.
--
-- __Example:__ 2017-03-30T07:03:49.555Z
--
-- 'endTime', 'describeEvents_endTime' - The end of the time interval for which to retrieve events, specified in
-- ISO 8601 format.
--
-- __Example:__ 2017-03-30T07:03:49.555Z
--
-- 'sourceIdentifier', 'describeEvents_sourceIdentifier' - The identifier of the event source for which events are returned. If not
-- specified, all sources are included in the response.
--
-- 'sourceType', 'describeEvents_sourceType' - The event source to retrieve events for. If no value is specified, all
-- events are returned.
--
-- 'marker', 'describeEvents_marker' - An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
--
-- 'maxRecords', 'describeEvents_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a marker is
-- included in the response so that the remaining results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20; maximum 100.
newDescribeEvents ::
  DescribeEvents
newDescribeEvents =
  DescribeEvents'
    { duration = Prelude.Nothing,
      startTime = Prelude.Nothing,
      endTime = Prelude.Nothing,
      sourceIdentifier = Prelude.Nothing,
      sourceType = Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | The number of minutes worth of events to retrieve.
describeEvents_duration :: Lens.Lens' DescribeEvents (Prelude.Maybe Prelude.Int)
describeEvents_duration = Lens.lens (\DescribeEvents' {duration} -> duration) (\s@DescribeEvents' {} a -> s {duration = a} :: DescribeEvents)

-- | The beginning of the time interval to retrieve events for, specified in
-- ISO 8601 format.
--
-- __Example:__ 2017-03-30T07:03:49.555Z
describeEvents_startTime :: Lens.Lens' DescribeEvents (Prelude.Maybe Prelude.UTCTime)
describeEvents_startTime = Lens.lens (\DescribeEvents' {startTime} -> startTime) (\s@DescribeEvents' {} a -> s {startTime = a} :: DescribeEvents) Prelude.. Lens.mapping Core._Time

-- | The end of the time interval for which to retrieve events, specified in
-- ISO 8601 format.
--
-- __Example:__ 2017-03-30T07:03:49.555Z
describeEvents_endTime :: Lens.Lens' DescribeEvents (Prelude.Maybe Prelude.UTCTime)
describeEvents_endTime = Lens.lens (\DescribeEvents' {endTime} -> endTime) (\s@DescribeEvents' {} a -> s {endTime = a} :: DescribeEvents) Prelude.. Lens.mapping Core._Time

-- | The identifier of the event source for which events are returned. If not
-- specified, all sources are included in the response.
describeEvents_sourceIdentifier :: Lens.Lens' DescribeEvents (Prelude.Maybe Prelude.Text)
describeEvents_sourceIdentifier = Lens.lens (\DescribeEvents' {sourceIdentifier} -> sourceIdentifier) (\s@DescribeEvents' {} a -> s {sourceIdentifier = a} :: DescribeEvents)

-- | The event source to retrieve events for. If no value is specified, all
-- events are returned.
describeEvents_sourceType :: Lens.Lens' DescribeEvents (Prelude.Maybe SourceType)
describeEvents_sourceType = Lens.lens (\DescribeEvents' {sourceType} -> sourceType) (\s@DescribeEvents' {} a -> s {sourceType = a} :: DescribeEvents)

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
describeEvents_marker :: Lens.Lens' DescribeEvents (Prelude.Maybe Prelude.Text)
describeEvents_marker = Lens.lens (\DescribeEvents' {marker} -> marker) (\s@DescribeEvents' {} a -> s {marker = a} :: DescribeEvents)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a marker is
-- included in the response so that the remaining results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20; maximum 100.
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
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeEventsResult"
      ( \s h x ->
          DescribeEventsResponse'
            Prelude.<$> ( x Core..@? "Events" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "Event")
                        )
            Prelude.<*> (x Core..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeEvents

instance Prelude.NFData DescribeEvents

instance Core.ToHeaders DescribeEvents where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeEvents where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeEvents where
  toQuery DescribeEvents' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeEvents" :: Prelude.ByteString),
        "Version"
          Core.=: ("2015-02-02" :: Prelude.ByteString),
        "Duration" Core.=: duration,
        "StartTime" Core.=: startTime,
        "EndTime" Core.=: endTime,
        "SourceIdentifier" Core.=: sourceIdentifier,
        "SourceType" Core.=: sourceType,
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords
      ]

-- | Represents the output of a @DescribeEvents@ operation.
--
-- /See:/ 'newDescribeEventsResponse' smart constructor.
data DescribeEventsResponse = DescribeEventsResponse'
  { -- | A list of events. Each element in the list contains detailed information
    -- about one event.
    events :: Prelude.Maybe [Event],
    -- | Provides an identifier to allow retrieval of paginated results.
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
-- 'events', 'describeEventsResponse_events' - A list of events. Each element in the list contains detailed information
-- about one event.
--
-- 'marker', 'describeEventsResponse_marker' - Provides an identifier to allow retrieval of paginated results.
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

-- | A list of events. Each element in the list contains detailed information
-- about one event.
describeEventsResponse_events :: Lens.Lens' DescribeEventsResponse (Prelude.Maybe [Event])
describeEventsResponse_events = Lens.lens (\DescribeEventsResponse' {events} -> events) (\s@DescribeEventsResponse' {} a -> s {events = a} :: DescribeEventsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | Provides an identifier to allow retrieval of paginated results.
describeEventsResponse_marker :: Lens.Lens' DescribeEventsResponse (Prelude.Maybe Prelude.Text)
describeEventsResponse_marker = Lens.lens (\DescribeEventsResponse' {marker} -> marker) (\s@DescribeEventsResponse' {} a -> s {marker = a} :: DescribeEventsResponse)

-- | The response's http status code.
describeEventsResponse_httpStatus :: Lens.Lens' DescribeEventsResponse Prelude.Int
describeEventsResponse_httpStatus = Lens.lens (\DescribeEventsResponse' {httpStatus} -> httpStatus) (\s@DescribeEventsResponse' {} a -> s {httpStatus = a} :: DescribeEventsResponse)

instance Prelude.NFData DescribeEventsResponse
