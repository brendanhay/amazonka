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
-- Module      : Network.AWS.DAX.DescribeEvents
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns events related to DAX clusters and parameter groups. You can
-- obtain events specific to a particular DAX cluster or parameter group by
-- providing the name as a parameter.
--
-- By default, only the events occurring within the last 24 hours are
-- returned; however, you can retrieve up to 14 days\' worth of events if
-- necessary.
--
-- This operation returns paginated results.
module Network.AWS.DAX.DescribeEvents
  ( -- * Creating a Request
    DescribeEvents (..),
    newDescribeEvents,

    -- * Request Lenses
    describeEvents_nextToken,
    describeEvents_duration,
    describeEvents_maxResults,
    describeEvents_startTime,
    describeEvents_sourceName,
    describeEvents_endTime,
    describeEvents_sourceType,

    -- * Destructuring the Response
    DescribeEventsResponse (..),
    newDescribeEventsResponse,

    -- * Response Lenses
    describeEventsResponse_nextToken,
    describeEventsResponse_events,
    describeEventsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DAX.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeEvents' smart constructor.
data DescribeEvents = DescribeEvents'
  { -- | An optional token returned from a prior request. Use this token for
    -- pagination of results from this action. If this parameter is specified,
    -- the response includes only results beyond the token, up to the value
    -- specified by @MaxResults@.
    nextToken :: Core.Maybe Core.Text,
    -- | The number of minutes\' worth of events to retrieve.
    duration :: Core.Maybe Core.Int,
    -- | The maximum number of results to include in the response. If more
    -- results exist than the specified @MaxResults@ value, a token is included
    -- in the response so that the remaining results can be retrieved.
    --
    -- The value for @MaxResults@ must be between 20 and 100.
    maxResults :: Core.Maybe Core.Int,
    -- | The beginning of the time interval to retrieve events for, specified in
    -- ISO 8601 format.
    startTime :: Core.Maybe Core.POSIX,
    -- | The identifier of the event source for which events will be returned. If
    -- not specified, then all sources are included in the response.
    sourceName :: Core.Maybe Core.Text,
    -- | The end of the time interval for which to retrieve events, specified in
    -- ISO 8601 format.
    endTime :: Core.Maybe Core.POSIX,
    -- | The event source to retrieve events for. If no value is specified, all
    -- events are returned.
    sourceType :: Core.Maybe SourceType
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
-- 'nextToken', 'describeEvents_nextToken' - An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only results beyond the token, up to the value
-- specified by @MaxResults@.
--
-- 'duration', 'describeEvents_duration' - The number of minutes\' worth of events to retrieve.
--
-- 'maxResults', 'describeEvents_maxResults' - The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
--
-- The value for @MaxResults@ must be between 20 and 100.
--
-- 'startTime', 'describeEvents_startTime' - The beginning of the time interval to retrieve events for, specified in
-- ISO 8601 format.
--
-- 'sourceName', 'describeEvents_sourceName' - The identifier of the event source for which events will be returned. If
-- not specified, then all sources are included in the response.
--
-- 'endTime', 'describeEvents_endTime' - The end of the time interval for which to retrieve events, specified in
-- ISO 8601 format.
--
-- 'sourceType', 'describeEvents_sourceType' - The event source to retrieve events for. If no value is specified, all
-- events are returned.
newDescribeEvents ::
  DescribeEvents
newDescribeEvents =
  DescribeEvents'
    { nextToken = Core.Nothing,
      duration = Core.Nothing,
      maxResults = Core.Nothing,
      startTime = Core.Nothing,
      sourceName = Core.Nothing,
      endTime = Core.Nothing,
      sourceType = Core.Nothing
    }

-- | An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only results beyond the token, up to the value
-- specified by @MaxResults@.
describeEvents_nextToken :: Lens.Lens' DescribeEvents (Core.Maybe Core.Text)
describeEvents_nextToken = Lens.lens (\DescribeEvents' {nextToken} -> nextToken) (\s@DescribeEvents' {} a -> s {nextToken = a} :: DescribeEvents)

-- | The number of minutes\' worth of events to retrieve.
describeEvents_duration :: Lens.Lens' DescribeEvents (Core.Maybe Core.Int)
describeEvents_duration = Lens.lens (\DescribeEvents' {duration} -> duration) (\s@DescribeEvents' {} a -> s {duration = a} :: DescribeEvents)

-- | The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
--
-- The value for @MaxResults@ must be between 20 and 100.
describeEvents_maxResults :: Lens.Lens' DescribeEvents (Core.Maybe Core.Int)
describeEvents_maxResults = Lens.lens (\DescribeEvents' {maxResults} -> maxResults) (\s@DescribeEvents' {} a -> s {maxResults = a} :: DescribeEvents)

-- | The beginning of the time interval to retrieve events for, specified in
-- ISO 8601 format.
describeEvents_startTime :: Lens.Lens' DescribeEvents (Core.Maybe Core.UTCTime)
describeEvents_startTime = Lens.lens (\DescribeEvents' {startTime} -> startTime) (\s@DescribeEvents' {} a -> s {startTime = a} :: DescribeEvents) Core.. Lens.mapping Core._Time

-- | The identifier of the event source for which events will be returned. If
-- not specified, then all sources are included in the response.
describeEvents_sourceName :: Lens.Lens' DescribeEvents (Core.Maybe Core.Text)
describeEvents_sourceName = Lens.lens (\DescribeEvents' {sourceName} -> sourceName) (\s@DescribeEvents' {} a -> s {sourceName = a} :: DescribeEvents)

-- | The end of the time interval for which to retrieve events, specified in
-- ISO 8601 format.
describeEvents_endTime :: Lens.Lens' DescribeEvents (Core.Maybe Core.UTCTime)
describeEvents_endTime = Lens.lens (\DescribeEvents' {endTime} -> endTime) (\s@DescribeEvents' {} a -> s {endTime = a} :: DescribeEvents) Core.. Lens.mapping Core._Time

-- | The event source to retrieve events for. If no value is specified, all
-- events are returned.
describeEvents_sourceType :: Lens.Lens' DescribeEvents (Core.Maybe SourceType)
describeEvents_sourceType = Lens.lens (\DescribeEvents' {sourceType} -> sourceType) (\s@DescribeEvents' {} a -> s {sourceType = a} :: DescribeEvents)

instance Core.AWSPager DescribeEvents where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeEventsResponse_nextToken Core.. Lens._Just
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
          Lens.& describeEvents_nextToken
          Lens..~ rs
          Lens.^? describeEventsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest DescribeEvents where
  type
    AWSResponse DescribeEvents =
      DescribeEventsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEventsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Events" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeEvents

instance Core.NFData DescribeEvents

instance Core.ToHeaders DescribeEvents where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonDAXV3.DescribeEvents" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeEvents where
  toJSON DescribeEvents' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("Duration" Core..=) Core.<$> duration,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("StartTime" Core..=) Core.<$> startTime,
            ("SourceName" Core..=) Core.<$> sourceName,
            ("EndTime" Core..=) Core.<$> endTime,
            ("SourceType" Core..=) Core.<$> sourceType
          ]
      )

instance Core.ToPath DescribeEvents where
  toPath = Core.const "/"

instance Core.ToQuery DescribeEvents where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeEventsResponse' smart constructor.
data DescribeEventsResponse = DescribeEventsResponse'
  { -- | Provides an identifier to allow retrieval of paginated results.
    nextToken :: Core.Maybe Core.Text,
    -- | An array of events. Each element in the array represents one event.
    events :: Core.Maybe [Event],
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
-- 'nextToken', 'describeEventsResponse_nextToken' - Provides an identifier to allow retrieval of paginated results.
--
-- 'events', 'describeEventsResponse_events' - An array of events. Each element in the array represents one event.
--
-- 'httpStatus', 'describeEventsResponse_httpStatus' - The response's http status code.
newDescribeEventsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeEventsResponse
newDescribeEventsResponse pHttpStatus_ =
  DescribeEventsResponse'
    { nextToken = Core.Nothing,
      events = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Provides an identifier to allow retrieval of paginated results.
describeEventsResponse_nextToken :: Lens.Lens' DescribeEventsResponse (Core.Maybe Core.Text)
describeEventsResponse_nextToken = Lens.lens (\DescribeEventsResponse' {nextToken} -> nextToken) (\s@DescribeEventsResponse' {} a -> s {nextToken = a} :: DescribeEventsResponse)

-- | An array of events. Each element in the array represents one event.
describeEventsResponse_events :: Lens.Lens' DescribeEventsResponse (Core.Maybe [Event])
describeEventsResponse_events = Lens.lens (\DescribeEventsResponse' {events} -> events) (\s@DescribeEventsResponse' {} a -> s {events = a} :: DescribeEventsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeEventsResponse_httpStatus :: Lens.Lens' DescribeEventsResponse Core.Int
describeEventsResponse_httpStatus = Lens.lens (\DescribeEventsResponse' {httpStatus} -> httpStatus) (\s@DescribeEventsResponse' {} a -> s {httpStatus = a} :: DescribeEventsResponse)

instance Core.NFData DescribeEventsResponse
