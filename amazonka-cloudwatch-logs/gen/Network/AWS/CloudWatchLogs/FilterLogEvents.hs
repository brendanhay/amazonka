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
-- Module      : Network.AWS.CloudWatchLogs.FilterLogEvents
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists log events from the specified log group. You can list all the log
-- events or filter the results using a filter pattern, a time range, and
-- the name of the log stream.
--
-- By default, this operation returns as many log events as can fit in 1 MB
-- (up to 10,000 log events) or all the events found within the time range
-- that you specify. If the results include a token, then there are more
-- log events available, and you can get additional results by specifying
-- the token in a subsequent call. This operation can return empty results
-- while there are more log events available through the token.
--
-- The returned log events are sorted by event timestamp, the timestamp
-- when the event was ingested by CloudWatch Logs, and the ID of the
-- @PutLogEvents@ request.
--
-- This operation returns paginated results.
module Network.AWS.CloudWatchLogs.FilterLogEvents
  ( -- * Creating a Request
    FilterLogEvents (..),
    newFilterLogEvents,

    -- * Request Lenses
    filterLogEvents_logStreamNamePrefix,
    filterLogEvents_nextToken,
    filterLogEvents_interleaved,
    filterLogEvents_filterPattern,
    filterLogEvents_startTime,
    filterLogEvents_endTime,
    filterLogEvents_logStreamNames,
    filterLogEvents_limit,
    filterLogEvents_logGroupName,

    -- * Destructuring the Response
    FilterLogEventsResponse (..),
    newFilterLogEventsResponse,

    -- * Response Lenses
    filterLogEventsResponse_nextToken,
    filterLogEventsResponse_searchedLogStreams,
    filterLogEventsResponse_events,
    filterLogEventsResponse_httpStatus,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newFilterLogEvents' smart constructor.
data FilterLogEvents = FilterLogEvents'
  { -- | Filters the results to include only events from log streams that have
    -- names starting with this prefix.
    --
    -- If you specify a value for both @logStreamNamePrefix@ and
    -- @logStreamNames@, but the value for @logStreamNamePrefix@ does not match
    -- any log stream names specified in @logStreamNames@, the action returns
    -- an @InvalidParameterException@ error.
    logStreamNamePrefix :: Prelude.Maybe Prelude.Text,
    -- | The token for the next set of events to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | If the value is true, the operation makes a best effort to provide
    -- responses that contain events from multiple log streams within the log
    -- group, interleaved in a single response. If the value is false, all the
    -- matched log events in the first log stream are searched first, then
    -- those in the next log stream, and so on. The default is false.
    --
    -- __Important:__ Starting on June 17, 2019, this parameter is ignored and
    -- the value is assumed to be true. The response from this operation always
    -- interleaves events from multiple log streams within a log group.
    interleaved :: Prelude.Maybe Prelude.Bool,
    -- | The filter pattern to use. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/FilterAndPatternSyntax.html Filter and Pattern Syntax>.
    --
    -- If not provided, all the events are matched.
    filterPattern :: Prelude.Maybe Prelude.Text,
    -- | The start of the time range, expressed as the number of milliseconds
    -- after Jan 1, 1970 00:00:00 UTC. Events with a timestamp before this time
    -- are not returned.
    --
    -- If you omit @startTime@ and @endTime@ the most recent log events are
    -- retrieved, to up 1 MB or 10,000 log events.
    startTime :: Prelude.Maybe Prelude.Natural,
    -- | The end of the time range, expressed as the number of milliseconds after
    -- Jan 1, 1970 00:00:00 UTC. Events with a timestamp later than this time
    -- are not returned.
    endTime :: Prelude.Maybe Prelude.Natural,
    -- | Filters the results to only logs from the log streams in this list.
    --
    -- If you specify a value for both @logStreamNamePrefix@ and
    -- @logStreamNames@, the action returns an @InvalidParameterException@
    -- error.
    logStreamNames :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The maximum number of events to return. The default is 10,000 events.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The name of the log group to search.
    logGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FilterLogEvents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logStreamNamePrefix', 'filterLogEvents_logStreamNamePrefix' - Filters the results to include only events from log streams that have
-- names starting with this prefix.
--
-- If you specify a value for both @logStreamNamePrefix@ and
-- @logStreamNames@, but the value for @logStreamNamePrefix@ does not match
-- any log stream names specified in @logStreamNames@, the action returns
-- an @InvalidParameterException@ error.
--
-- 'nextToken', 'filterLogEvents_nextToken' - The token for the next set of events to return. (You received this token
-- from a previous call.)
--
-- 'interleaved', 'filterLogEvents_interleaved' - If the value is true, the operation makes a best effort to provide
-- responses that contain events from multiple log streams within the log
-- group, interleaved in a single response. If the value is false, all the
-- matched log events in the first log stream are searched first, then
-- those in the next log stream, and so on. The default is false.
--
-- __Important:__ Starting on June 17, 2019, this parameter is ignored and
-- the value is assumed to be true. The response from this operation always
-- interleaves events from multiple log streams within a log group.
--
-- 'filterPattern', 'filterLogEvents_filterPattern' - The filter pattern to use. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/FilterAndPatternSyntax.html Filter and Pattern Syntax>.
--
-- If not provided, all the events are matched.
--
-- 'startTime', 'filterLogEvents_startTime' - The start of the time range, expressed as the number of milliseconds
-- after Jan 1, 1970 00:00:00 UTC. Events with a timestamp before this time
-- are not returned.
--
-- If you omit @startTime@ and @endTime@ the most recent log events are
-- retrieved, to up 1 MB or 10,000 log events.
--
-- 'endTime', 'filterLogEvents_endTime' - The end of the time range, expressed as the number of milliseconds after
-- Jan 1, 1970 00:00:00 UTC. Events with a timestamp later than this time
-- are not returned.
--
-- 'logStreamNames', 'filterLogEvents_logStreamNames' - Filters the results to only logs from the log streams in this list.
--
-- If you specify a value for both @logStreamNamePrefix@ and
-- @logStreamNames@, the action returns an @InvalidParameterException@
-- error.
--
-- 'limit', 'filterLogEvents_limit' - The maximum number of events to return. The default is 10,000 events.
--
-- 'logGroupName', 'filterLogEvents_logGroupName' - The name of the log group to search.
newFilterLogEvents ::
  -- | 'logGroupName'
  Prelude.Text ->
  FilterLogEvents
newFilterLogEvents pLogGroupName_ =
  FilterLogEvents'
    { logStreamNamePrefix =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      interleaved = Prelude.Nothing,
      filterPattern = Prelude.Nothing,
      startTime = Prelude.Nothing,
      endTime = Prelude.Nothing,
      logStreamNames = Prelude.Nothing,
      limit = Prelude.Nothing,
      logGroupName = pLogGroupName_
    }

-- | Filters the results to include only events from log streams that have
-- names starting with this prefix.
--
-- If you specify a value for both @logStreamNamePrefix@ and
-- @logStreamNames@, but the value for @logStreamNamePrefix@ does not match
-- any log stream names specified in @logStreamNames@, the action returns
-- an @InvalidParameterException@ error.
filterLogEvents_logStreamNamePrefix :: Lens.Lens' FilterLogEvents (Prelude.Maybe Prelude.Text)
filterLogEvents_logStreamNamePrefix = Lens.lens (\FilterLogEvents' {logStreamNamePrefix} -> logStreamNamePrefix) (\s@FilterLogEvents' {} a -> s {logStreamNamePrefix = a} :: FilterLogEvents)

-- | The token for the next set of events to return. (You received this token
-- from a previous call.)
filterLogEvents_nextToken :: Lens.Lens' FilterLogEvents (Prelude.Maybe Prelude.Text)
filterLogEvents_nextToken = Lens.lens (\FilterLogEvents' {nextToken} -> nextToken) (\s@FilterLogEvents' {} a -> s {nextToken = a} :: FilterLogEvents)

-- | If the value is true, the operation makes a best effort to provide
-- responses that contain events from multiple log streams within the log
-- group, interleaved in a single response. If the value is false, all the
-- matched log events in the first log stream are searched first, then
-- those in the next log stream, and so on. The default is false.
--
-- __Important:__ Starting on June 17, 2019, this parameter is ignored and
-- the value is assumed to be true. The response from this operation always
-- interleaves events from multiple log streams within a log group.
filterLogEvents_interleaved :: Lens.Lens' FilterLogEvents (Prelude.Maybe Prelude.Bool)
filterLogEvents_interleaved = Lens.lens (\FilterLogEvents' {interleaved} -> interleaved) (\s@FilterLogEvents' {} a -> s {interleaved = a} :: FilterLogEvents)

-- | The filter pattern to use. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/FilterAndPatternSyntax.html Filter and Pattern Syntax>.
--
-- If not provided, all the events are matched.
filterLogEvents_filterPattern :: Lens.Lens' FilterLogEvents (Prelude.Maybe Prelude.Text)
filterLogEvents_filterPattern = Lens.lens (\FilterLogEvents' {filterPattern} -> filterPattern) (\s@FilterLogEvents' {} a -> s {filterPattern = a} :: FilterLogEvents)

-- | The start of the time range, expressed as the number of milliseconds
-- after Jan 1, 1970 00:00:00 UTC. Events with a timestamp before this time
-- are not returned.
--
-- If you omit @startTime@ and @endTime@ the most recent log events are
-- retrieved, to up 1 MB or 10,000 log events.
filterLogEvents_startTime :: Lens.Lens' FilterLogEvents (Prelude.Maybe Prelude.Natural)
filterLogEvents_startTime = Lens.lens (\FilterLogEvents' {startTime} -> startTime) (\s@FilterLogEvents' {} a -> s {startTime = a} :: FilterLogEvents)

-- | The end of the time range, expressed as the number of milliseconds after
-- Jan 1, 1970 00:00:00 UTC. Events with a timestamp later than this time
-- are not returned.
filterLogEvents_endTime :: Lens.Lens' FilterLogEvents (Prelude.Maybe Prelude.Natural)
filterLogEvents_endTime = Lens.lens (\FilterLogEvents' {endTime} -> endTime) (\s@FilterLogEvents' {} a -> s {endTime = a} :: FilterLogEvents)

-- | Filters the results to only logs from the log streams in this list.
--
-- If you specify a value for both @logStreamNamePrefix@ and
-- @logStreamNames@, the action returns an @InvalidParameterException@
-- error.
filterLogEvents_logStreamNames :: Lens.Lens' FilterLogEvents (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
filterLogEvents_logStreamNames = Lens.lens (\FilterLogEvents' {logStreamNames} -> logStreamNames) (\s@FilterLogEvents' {} a -> s {logStreamNames = a} :: FilterLogEvents) Prelude.. Lens.mapping Lens._Coerce

-- | The maximum number of events to return. The default is 10,000 events.
filterLogEvents_limit :: Lens.Lens' FilterLogEvents (Prelude.Maybe Prelude.Natural)
filterLogEvents_limit = Lens.lens (\FilterLogEvents' {limit} -> limit) (\s@FilterLogEvents' {} a -> s {limit = a} :: FilterLogEvents)

-- | The name of the log group to search.
filterLogEvents_logGroupName :: Lens.Lens' FilterLogEvents Prelude.Text
filterLogEvents_logGroupName = Lens.lens (\FilterLogEvents' {logGroupName} -> logGroupName) (\s@FilterLogEvents' {} a -> s {logGroupName = a} :: FilterLogEvents)

instance Core.AWSPager FilterLogEvents where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? filterLogEventsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& filterLogEvents_nextToken
          Lens..~ rs
          Lens.^? filterLogEventsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest FilterLogEvents where
  type
    AWSResponse FilterLogEvents =
      FilterLogEventsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          FilterLogEventsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> ( x Core..?> "searchedLogStreams"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "events" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable FilterLogEvents

instance Prelude.NFData FilterLogEvents

instance Core.ToHeaders FilterLogEvents where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Logs_20140328.FilterLogEvents" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON FilterLogEvents where
  toJSON FilterLogEvents' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("logStreamNamePrefix" Core..=)
              Prelude.<$> logStreamNamePrefix,
            ("nextToken" Core..=) Prelude.<$> nextToken,
            ("interleaved" Core..=) Prelude.<$> interleaved,
            ("filterPattern" Core..=) Prelude.<$> filterPattern,
            ("startTime" Core..=) Prelude.<$> startTime,
            ("endTime" Core..=) Prelude.<$> endTime,
            ("logStreamNames" Core..=)
              Prelude.<$> logStreamNames,
            ("limit" Core..=) Prelude.<$> limit,
            Prelude.Just ("logGroupName" Core..= logGroupName)
          ]
      )

instance Core.ToPath FilterLogEvents where
  toPath = Prelude.const "/"

instance Core.ToQuery FilterLogEvents where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newFilterLogEventsResponse' smart constructor.
data FilterLogEventsResponse = FilterLogEventsResponse'
  { -- | The token to use when requesting the next set of items. The token
    -- expires after 24 hours.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | __IMPORTANT__ Starting on May 15, 2020, this parameter will be
    -- deprecated. This parameter will be an empty list after the deprecation
    -- occurs.
    --
    -- Indicates which log streams have been searched and whether each has been
    -- searched completely.
    searchedLogStreams :: Prelude.Maybe [SearchedLogStream],
    -- | The matched events.
    events :: Prelude.Maybe [FilteredLogEvent],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FilterLogEventsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'filterLogEventsResponse_nextToken' - The token to use when requesting the next set of items. The token
-- expires after 24 hours.
--
-- 'searchedLogStreams', 'filterLogEventsResponse_searchedLogStreams' - __IMPORTANT__ Starting on May 15, 2020, this parameter will be
-- deprecated. This parameter will be an empty list after the deprecation
-- occurs.
--
-- Indicates which log streams have been searched and whether each has been
-- searched completely.
--
-- 'events', 'filterLogEventsResponse_events' - The matched events.
--
-- 'httpStatus', 'filterLogEventsResponse_httpStatus' - The response's http status code.
newFilterLogEventsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  FilterLogEventsResponse
newFilterLogEventsResponse pHttpStatus_ =
  FilterLogEventsResponse'
    { nextToken =
        Prelude.Nothing,
      searchedLogStreams = Prelude.Nothing,
      events = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use when requesting the next set of items. The token
-- expires after 24 hours.
filterLogEventsResponse_nextToken :: Lens.Lens' FilterLogEventsResponse (Prelude.Maybe Prelude.Text)
filterLogEventsResponse_nextToken = Lens.lens (\FilterLogEventsResponse' {nextToken} -> nextToken) (\s@FilterLogEventsResponse' {} a -> s {nextToken = a} :: FilterLogEventsResponse)

-- | __IMPORTANT__ Starting on May 15, 2020, this parameter will be
-- deprecated. This parameter will be an empty list after the deprecation
-- occurs.
--
-- Indicates which log streams have been searched and whether each has been
-- searched completely.
filterLogEventsResponse_searchedLogStreams :: Lens.Lens' FilterLogEventsResponse (Prelude.Maybe [SearchedLogStream])
filterLogEventsResponse_searchedLogStreams = Lens.lens (\FilterLogEventsResponse' {searchedLogStreams} -> searchedLogStreams) (\s@FilterLogEventsResponse' {} a -> s {searchedLogStreams = a} :: FilterLogEventsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The matched events.
filterLogEventsResponse_events :: Lens.Lens' FilterLogEventsResponse (Prelude.Maybe [FilteredLogEvent])
filterLogEventsResponse_events = Lens.lens (\FilterLogEventsResponse' {events} -> events) (\s@FilterLogEventsResponse' {} a -> s {events = a} :: FilterLogEventsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
filterLogEventsResponse_httpStatus :: Lens.Lens' FilterLogEventsResponse Prelude.Int
filterLogEventsResponse_httpStatus = Lens.lens (\FilterLogEventsResponse' {httpStatus} -> httpStatus) (\s@FilterLogEventsResponse' {} a -> s {httpStatus = a} :: FilterLogEventsResponse)

instance Prelude.NFData FilterLogEventsResponse
