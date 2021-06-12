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
-- Module      : Network.AWS.Redshift.DescribeEvents
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns events related to clusters, security groups, snapshots, and
-- parameter groups for the past 14 days. Events specific to a particular
-- cluster, security group, snapshot or parameter group can be obtained by
-- providing the name as a parameter. By default, the past hour of events
-- are returned.
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeEvents
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
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeEvents' smart constructor.
data DescribeEvents = DescribeEvents'
  { -- | The number of minutes prior to the time of the request for which to
    -- retrieve events. For example, if the request is sent at 18:00 and you
    -- specify a duration of 60, then only events which have occurred after
    -- 17:00 will be returned.
    --
    -- Default: @60@
    duration :: Core.Maybe Core.Int,
    -- | The beginning of the time interval to retrieve events for, specified in
    -- ISO 8601 format. For more information about ISO 8601, go to the
    -- <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.>
    --
    -- Example: @2009-07-08T18:00Z@
    startTime :: Core.Maybe Core.ISO8601,
    -- | The end of the time interval for which to retrieve events, specified in
    -- ISO 8601 format. For more information about ISO 8601, go to the
    -- <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.>
    --
    -- Example: @2009-07-08T18:00Z@
    endTime :: Core.Maybe Core.ISO8601,
    -- | The identifier of the event source for which events will be returned. If
    -- this parameter is not specified, then all sources are included in the
    -- response.
    --
    -- Constraints:
    --
    -- If /SourceIdentifier/ is supplied, /SourceType/ must also be provided.
    --
    -- -   Specify a cluster identifier when /SourceType/ is @cluster@.
    --
    -- -   Specify a cluster security group name when /SourceType/ is
    --     @cluster-security-group@.
    --
    -- -   Specify a cluster parameter group name when /SourceType/ is
    --     @cluster-parameter-group@.
    --
    -- -   Specify a cluster snapshot identifier when /SourceType/ is
    --     @cluster-snapshot@.
    sourceIdentifier :: Core.Maybe Core.Text,
    -- | The event source to retrieve events for. If no value is specified, all
    -- events are returned.
    --
    -- Constraints:
    --
    -- If /SourceType/ is supplied, /SourceIdentifier/ must also be provided.
    --
    -- -   Specify @cluster@ when /SourceIdentifier/ is a cluster identifier.
    --
    -- -   Specify @cluster-security-group@ when /SourceIdentifier/ is a
    --     cluster security group name.
    --
    -- -   Specify @cluster-parameter-group@ when /SourceIdentifier/ is a
    --     cluster parameter group name.
    --
    -- -   Specify @cluster-snapshot@ when /SourceIdentifier/ is a cluster
    --     snapshot identifier.
    sourceType :: Core.Maybe SourceType,
    -- | An optional parameter that specifies the starting point to return a set
    -- of response records. When the results of a DescribeEvents request exceed
    -- the value specified in @MaxRecords@, AWS returns a value in the @Marker@
    -- field of the response. You can retrieve the next set of response records
    -- by providing the returned marker value in the @Marker@ parameter and
    -- retrying the request.
    marker :: Core.Maybe Core.Text,
    -- | The maximum number of response records to return in each call. If the
    -- number of remaining response records exceeds the specified @MaxRecords@
    -- value, a value is returned in a @marker@ field of the response. You can
    -- retrieve the next set of records by retrying the command with the
    -- returned marker value.
    --
    -- Default: @100@
    --
    -- Constraints: minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeEvents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'duration', 'describeEvents_duration' - The number of minutes prior to the time of the request for which to
-- retrieve events. For example, if the request is sent at 18:00 and you
-- specify a duration of 60, then only events which have occurred after
-- 17:00 will be returned.
--
-- Default: @60@
--
-- 'startTime', 'describeEvents_startTime' - The beginning of the time interval to retrieve events for, specified in
-- ISO 8601 format. For more information about ISO 8601, go to the
-- <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.>
--
-- Example: @2009-07-08T18:00Z@
--
-- 'endTime', 'describeEvents_endTime' - The end of the time interval for which to retrieve events, specified in
-- ISO 8601 format. For more information about ISO 8601, go to the
-- <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.>
--
-- Example: @2009-07-08T18:00Z@
--
-- 'sourceIdentifier', 'describeEvents_sourceIdentifier' - The identifier of the event source for which events will be returned. If
-- this parameter is not specified, then all sources are included in the
-- response.
--
-- Constraints:
--
-- If /SourceIdentifier/ is supplied, /SourceType/ must also be provided.
--
-- -   Specify a cluster identifier when /SourceType/ is @cluster@.
--
-- -   Specify a cluster security group name when /SourceType/ is
--     @cluster-security-group@.
--
-- -   Specify a cluster parameter group name when /SourceType/ is
--     @cluster-parameter-group@.
--
-- -   Specify a cluster snapshot identifier when /SourceType/ is
--     @cluster-snapshot@.
--
-- 'sourceType', 'describeEvents_sourceType' - The event source to retrieve events for. If no value is specified, all
-- events are returned.
--
-- Constraints:
--
-- If /SourceType/ is supplied, /SourceIdentifier/ must also be provided.
--
-- -   Specify @cluster@ when /SourceIdentifier/ is a cluster identifier.
--
-- -   Specify @cluster-security-group@ when /SourceIdentifier/ is a
--     cluster security group name.
--
-- -   Specify @cluster-parameter-group@ when /SourceIdentifier/ is a
--     cluster parameter group name.
--
-- -   Specify @cluster-snapshot@ when /SourceIdentifier/ is a cluster
--     snapshot identifier.
--
-- 'marker', 'describeEvents_marker' - An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeEvents request exceed
-- the value specified in @MaxRecords@, AWS returns a value in the @Marker@
-- field of the response. You can retrieve the next set of response records
-- by providing the returned marker value in the @Marker@ parameter and
-- retrying the request.
--
-- 'maxRecords', 'describeEvents_maxRecords' - The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
newDescribeEvents ::
  DescribeEvents
newDescribeEvents =
  DescribeEvents'
    { duration = Core.Nothing,
      startTime = Core.Nothing,
      endTime = Core.Nothing,
      sourceIdentifier = Core.Nothing,
      sourceType = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | The number of minutes prior to the time of the request for which to
-- retrieve events. For example, if the request is sent at 18:00 and you
-- specify a duration of 60, then only events which have occurred after
-- 17:00 will be returned.
--
-- Default: @60@
describeEvents_duration :: Lens.Lens' DescribeEvents (Core.Maybe Core.Int)
describeEvents_duration = Lens.lens (\DescribeEvents' {duration} -> duration) (\s@DescribeEvents' {} a -> s {duration = a} :: DescribeEvents)

-- | The beginning of the time interval to retrieve events for, specified in
-- ISO 8601 format. For more information about ISO 8601, go to the
-- <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.>
--
-- Example: @2009-07-08T18:00Z@
describeEvents_startTime :: Lens.Lens' DescribeEvents (Core.Maybe Core.UTCTime)
describeEvents_startTime = Lens.lens (\DescribeEvents' {startTime} -> startTime) (\s@DescribeEvents' {} a -> s {startTime = a} :: DescribeEvents) Core.. Lens.mapping Core._Time

-- | The end of the time interval for which to retrieve events, specified in
-- ISO 8601 format. For more information about ISO 8601, go to the
-- <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.>
--
-- Example: @2009-07-08T18:00Z@
describeEvents_endTime :: Lens.Lens' DescribeEvents (Core.Maybe Core.UTCTime)
describeEvents_endTime = Lens.lens (\DescribeEvents' {endTime} -> endTime) (\s@DescribeEvents' {} a -> s {endTime = a} :: DescribeEvents) Core.. Lens.mapping Core._Time

-- | The identifier of the event source for which events will be returned. If
-- this parameter is not specified, then all sources are included in the
-- response.
--
-- Constraints:
--
-- If /SourceIdentifier/ is supplied, /SourceType/ must also be provided.
--
-- -   Specify a cluster identifier when /SourceType/ is @cluster@.
--
-- -   Specify a cluster security group name when /SourceType/ is
--     @cluster-security-group@.
--
-- -   Specify a cluster parameter group name when /SourceType/ is
--     @cluster-parameter-group@.
--
-- -   Specify a cluster snapshot identifier when /SourceType/ is
--     @cluster-snapshot@.
describeEvents_sourceIdentifier :: Lens.Lens' DescribeEvents (Core.Maybe Core.Text)
describeEvents_sourceIdentifier = Lens.lens (\DescribeEvents' {sourceIdentifier} -> sourceIdentifier) (\s@DescribeEvents' {} a -> s {sourceIdentifier = a} :: DescribeEvents)

-- | The event source to retrieve events for. If no value is specified, all
-- events are returned.
--
-- Constraints:
--
-- If /SourceType/ is supplied, /SourceIdentifier/ must also be provided.
--
-- -   Specify @cluster@ when /SourceIdentifier/ is a cluster identifier.
--
-- -   Specify @cluster-security-group@ when /SourceIdentifier/ is a
--     cluster security group name.
--
-- -   Specify @cluster-parameter-group@ when /SourceIdentifier/ is a
--     cluster parameter group name.
--
-- -   Specify @cluster-snapshot@ when /SourceIdentifier/ is a cluster
--     snapshot identifier.
describeEvents_sourceType :: Lens.Lens' DescribeEvents (Core.Maybe SourceType)
describeEvents_sourceType = Lens.lens (\DescribeEvents' {sourceType} -> sourceType) (\s@DescribeEvents' {} a -> s {sourceType = a} :: DescribeEvents)

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeEvents request exceed
-- the value specified in @MaxRecords@, AWS returns a value in the @Marker@
-- field of the response. You can retrieve the next set of response records
-- by providing the returned marker value in the @Marker@ parameter and
-- retrying the request.
describeEvents_marker :: Lens.Lens' DescribeEvents (Core.Maybe Core.Text)
describeEvents_marker = Lens.lens (\DescribeEvents' {marker} -> marker) (\s@DescribeEvents' {} a -> s {marker = a} :: DescribeEvents)

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
describeEvents_maxRecords :: Lens.Lens' DescribeEvents (Core.Maybe Core.Int)
describeEvents_maxRecords = Lens.lens (\DescribeEvents' {maxRecords} -> maxRecords) (\s@DescribeEvents' {} a -> s {maxRecords = a} :: DescribeEvents)

instance Core.AWSPager DescribeEvents where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeEventsResponse_marker Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeEventsResponse_events Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeEvents_marker
          Lens..~ rs
          Lens.^? describeEventsResponse_marker Core.. Lens._Just

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
            Core.<$> ( x Core..@? "Events" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "Event")
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeEvents

instance Core.NFData DescribeEvents

instance Core.ToHeaders DescribeEvents where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeEvents where
  toPath = Core.const "/"

instance Core.ToQuery DescribeEvents where
  toQuery DescribeEvents' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeEvents" :: Core.ByteString),
        "Version" Core.=: ("2012-12-01" :: Core.ByteString),
        "Duration" Core.=: duration,
        "StartTime" Core.=: startTime,
        "EndTime" Core.=: endTime,
        "SourceIdentifier" Core.=: sourceIdentifier,
        "SourceType" Core.=: sourceType,
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords
      ]

-- |
--
-- /See:/ 'newDescribeEventsResponse' smart constructor.
data DescribeEventsResponse = DescribeEventsResponse'
  { -- | A list of @Event@ instances.
    events :: Core.Maybe [Event],
    -- | A value that indicates the starting point for the next set of response
    -- records in a subsequent request. If a value is returned in a response,
    -- you can retrieve the next set of records by providing this returned
    -- marker value in the @Marker@ parameter and retrying the command. If the
    -- @Marker@ field is empty, all response records have been retrieved for
    -- the request.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeEventsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'events', 'describeEventsResponse_events' - A list of @Event@ instances.
--
-- 'marker', 'describeEventsResponse_marker' - A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
--
-- 'httpStatus', 'describeEventsResponse_httpStatus' - The response's http status code.
newDescribeEventsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeEventsResponse
newDescribeEventsResponse pHttpStatus_ =
  DescribeEventsResponse'
    { events = Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of @Event@ instances.
describeEventsResponse_events :: Lens.Lens' DescribeEventsResponse (Core.Maybe [Event])
describeEventsResponse_events = Lens.lens (\DescribeEventsResponse' {events} -> events) (\s@DescribeEventsResponse' {} a -> s {events = a} :: DescribeEventsResponse) Core.. Lens.mapping Lens._Coerce

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
describeEventsResponse_marker :: Lens.Lens' DescribeEventsResponse (Core.Maybe Core.Text)
describeEventsResponse_marker = Lens.lens (\DescribeEventsResponse' {marker} -> marker) (\s@DescribeEventsResponse' {} a -> s {marker = a} :: DescribeEventsResponse)

-- | The response's http status code.
describeEventsResponse_httpStatus :: Lens.Lens' DescribeEventsResponse Core.Int
describeEventsResponse_httpStatus = Lens.lens (\DescribeEventsResponse' {httpStatus} -> httpStatus) (\s@DescribeEventsResponse' {} a -> s {httpStatus = a} :: DescribeEventsResponse)

instance Core.NFData DescribeEventsResponse
