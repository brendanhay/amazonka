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
-- Module      : Amazonka.CloudWatchLogs.FilterLogEvents
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists log events from the specified log group. You can list all the log
-- events or filter the results using a filter pattern, a time range, and
-- the name of the log stream.
--
-- You must have the @logs;FilterLogEvents@ permission to perform this
-- operation.
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
module Amazonka.CloudWatchLogs.FilterLogEvents
  ( -- * Creating a Request
    FilterLogEvents (..),
    newFilterLogEvents,

    -- * Request Lenses
    filterLogEvents_nextToken,
    filterLogEvents_interleaved,
    filterLogEvents_endTime,
    filterLogEvents_limit,
    filterLogEvents_logStreamNamePrefix,
    filterLogEvents_filterPattern,
    filterLogEvents_startTime,
    filterLogEvents_logStreamNames,
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

import Amazonka.CloudWatchLogs.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newFilterLogEvents' smart constructor.
data FilterLogEvents = FilterLogEvents'
  { -- | The token for the next set of events to return. (You received this token
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
    -- | The end of the time range, expressed as the number of milliseconds after
    -- Jan 1, 1970 00:00:00 UTC. Events with a timestamp later than this time
    -- are not returned.
    endTime :: Prelude.Maybe Prelude.Natural,
    -- | The maximum number of events to return. The default is 10,000 events.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | Filters the results to include only events from log streams that have
    -- names starting with this prefix.
    --
    -- If you specify a value for both @logStreamNamePrefix@ and
    -- @logStreamNames@, but the value for @logStreamNamePrefix@ does not match
    -- any log stream names specified in @logStreamNames@, the action returns
    -- an @InvalidParameterException@ error.
    logStreamNamePrefix :: Prelude.Maybe Prelude.Text,
    -- | The filter pattern to use. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/FilterAndPatternSyntax.html Filter and Pattern Syntax>.
    --
    -- If not provided, all the events are matched.
    filterPattern :: Prelude.Maybe Prelude.Text,
    -- | The start of the time range, expressed as the number of milliseconds
    -- after Jan 1, 1970 00:00:00 UTC. Events with a timestamp before this time
    -- are not returned.
    startTime :: Prelude.Maybe Prelude.Natural,
    -- | Filters the results to only logs from the log streams in this list.
    --
    -- If you specify a value for both @logStreamNamePrefix@ and
    -- @logStreamNames@, the action returns an @InvalidParameterException@
    -- error.
    logStreamNames :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
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
-- 'endTime', 'filterLogEvents_endTime' - The end of the time range, expressed as the number of milliseconds after
-- Jan 1, 1970 00:00:00 UTC. Events with a timestamp later than this time
-- are not returned.
--
-- 'limit', 'filterLogEvents_limit' - The maximum number of events to return. The default is 10,000 events.
--
-- 'logStreamNamePrefix', 'filterLogEvents_logStreamNamePrefix' - Filters the results to include only events from log streams that have
-- names starting with this prefix.
--
-- If you specify a value for both @logStreamNamePrefix@ and
-- @logStreamNames@, but the value for @logStreamNamePrefix@ does not match
-- any log stream names specified in @logStreamNames@, the action returns
-- an @InvalidParameterException@ error.
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
-- 'logStreamNames', 'filterLogEvents_logStreamNames' - Filters the results to only logs from the log streams in this list.
--
-- If you specify a value for both @logStreamNamePrefix@ and
-- @logStreamNames@, the action returns an @InvalidParameterException@
-- error.
--
-- 'logGroupName', 'filterLogEvents_logGroupName' - The name of the log group to search.
newFilterLogEvents ::
  -- | 'logGroupName'
  Prelude.Text ->
  FilterLogEvents
newFilterLogEvents pLogGroupName_ =
  FilterLogEvents'
    { nextToken = Prelude.Nothing,
      interleaved = Prelude.Nothing,
      endTime = Prelude.Nothing,
      limit = Prelude.Nothing,
      logStreamNamePrefix = Prelude.Nothing,
      filterPattern = Prelude.Nothing,
      startTime = Prelude.Nothing,
      logStreamNames = Prelude.Nothing,
      logGroupName = pLogGroupName_
    }

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

-- | The end of the time range, expressed as the number of milliseconds after
-- Jan 1, 1970 00:00:00 UTC. Events with a timestamp later than this time
-- are not returned.
filterLogEvents_endTime :: Lens.Lens' FilterLogEvents (Prelude.Maybe Prelude.Natural)
filterLogEvents_endTime = Lens.lens (\FilterLogEvents' {endTime} -> endTime) (\s@FilterLogEvents' {} a -> s {endTime = a} :: FilterLogEvents)

-- | The maximum number of events to return. The default is 10,000 events.
filterLogEvents_limit :: Lens.Lens' FilterLogEvents (Prelude.Maybe Prelude.Natural)
filterLogEvents_limit = Lens.lens (\FilterLogEvents' {limit} -> limit) (\s@FilterLogEvents' {} a -> s {limit = a} :: FilterLogEvents)

-- | Filters the results to include only events from log streams that have
-- names starting with this prefix.
--
-- If you specify a value for both @logStreamNamePrefix@ and
-- @logStreamNames@, but the value for @logStreamNamePrefix@ does not match
-- any log stream names specified in @logStreamNames@, the action returns
-- an @InvalidParameterException@ error.
filterLogEvents_logStreamNamePrefix :: Lens.Lens' FilterLogEvents (Prelude.Maybe Prelude.Text)
filterLogEvents_logStreamNamePrefix = Lens.lens (\FilterLogEvents' {logStreamNamePrefix} -> logStreamNamePrefix) (\s@FilterLogEvents' {} a -> s {logStreamNamePrefix = a} :: FilterLogEvents)

-- | The filter pattern to use. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/FilterAndPatternSyntax.html Filter and Pattern Syntax>.
--
-- If not provided, all the events are matched.
filterLogEvents_filterPattern :: Lens.Lens' FilterLogEvents (Prelude.Maybe Prelude.Text)
filterLogEvents_filterPattern = Lens.lens (\FilterLogEvents' {filterPattern} -> filterPattern) (\s@FilterLogEvents' {} a -> s {filterPattern = a} :: FilterLogEvents)

-- | The start of the time range, expressed as the number of milliseconds
-- after Jan 1, 1970 00:00:00 UTC. Events with a timestamp before this time
-- are not returned.
filterLogEvents_startTime :: Lens.Lens' FilterLogEvents (Prelude.Maybe Prelude.Natural)
filterLogEvents_startTime = Lens.lens (\FilterLogEvents' {startTime} -> startTime) (\s@FilterLogEvents' {} a -> s {startTime = a} :: FilterLogEvents)

-- | Filters the results to only logs from the log streams in this list.
--
-- If you specify a value for both @logStreamNamePrefix@ and
-- @logStreamNames@, the action returns an @InvalidParameterException@
-- error.
filterLogEvents_logStreamNames :: Lens.Lens' FilterLogEvents (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
filterLogEvents_logStreamNames = Lens.lens (\FilterLogEvents' {logStreamNames} -> logStreamNames) (\s@FilterLogEvents' {} a -> s {logStreamNames = a} :: FilterLogEvents) Prelude.. Lens.mapping Lens.coerced

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          FilterLogEventsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x Data..?> "searchedLogStreams"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "events" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable FilterLogEvents where
  hashWithSalt _salt FilterLogEvents' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` interleaved
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` logStreamNamePrefix
      `Prelude.hashWithSalt` filterPattern
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` logStreamNames
      `Prelude.hashWithSalt` logGroupName

instance Prelude.NFData FilterLogEvents where
  rnf FilterLogEvents' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf interleaved
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf logStreamNamePrefix
      `Prelude.seq` Prelude.rnf filterPattern
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf logStreamNames
      `Prelude.seq` Prelude.rnf logGroupName

instance Data.ToHeaders FilterLogEvents where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Logs_20140328.FilterLogEvents" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON FilterLogEvents where
  toJSON FilterLogEvents' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nextToken" Data..=) Prelude.<$> nextToken,
            ("interleaved" Data..=) Prelude.<$> interleaved,
            ("endTime" Data..=) Prelude.<$> endTime,
            ("limit" Data..=) Prelude.<$> limit,
            ("logStreamNamePrefix" Data..=)
              Prelude.<$> logStreamNamePrefix,
            ("filterPattern" Data..=) Prelude.<$> filterPattern,
            ("startTime" Data..=) Prelude.<$> startTime,
            ("logStreamNames" Data..=)
              Prelude.<$> logStreamNames,
            Prelude.Just ("logGroupName" Data..= logGroupName)
          ]
      )

instance Data.ToPath FilterLogEvents where
  toPath = Prelude.const "/"

instance Data.ToQuery FilterLogEvents where
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
filterLogEventsResponse_searchedLogStreams = Lens.lens (\FilterLogEventsResponse' {searchedLogStreams} -> searchedLogStreams) (\s@FilterLogEventsResponse' {} a -> s {searchedLogStreams = a} :: FilterLogEventsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The matched events.
filterLogEventsResponse_events :: Lens.Lens' FilterLogEventsResponse (Prelude.Maybe [FilteredLogEvent])
filterLogEventsResponse_events = Lens.lens (\FilterLogEventsResponse' {events} -> events) (\s@FilterLogEventsResponse' {} a -> s {events = a} :: FilterLogEventsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
filterLogEventsResponse_httpStatus :: Lens.Lens' FilterLogEventsResponse Prelude.Int
filterLogEventsResponse_httpStatus = Lens.lens (\FilterLogEventsResponse' {httpStatus} -> httpStatus) (\s@FilterLogEventsResponse' {} a -> s {httpStatus = a} :: FilterLogEventsResponse)

instance Prelude.NFData FilterLogEventsResponse where
  rnf FilterLogEventsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf searchedLogStreams
      `Prelude.seq` Prelude.rnf events
      `Prelude.seq` Prelude.rnf httpStatus
