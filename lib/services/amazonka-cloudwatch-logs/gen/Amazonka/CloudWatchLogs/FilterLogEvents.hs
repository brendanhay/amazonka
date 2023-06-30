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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
-- (up to 10,000 log events) or all the events found within the specified
-- time range. If the results include a token, that means there are more
-- log events available. You can get additional results by specifying the
-- token in a subsequent call. This operation can return empty results
-- while there are more log events available through the token.
--
-- The returned log events are sorted by event timestamp, the timestamp
-- when the event was ingested by CloudWatch Logs, and the ID of the
-- @PutLogEvents@ request.
--
-- If you are using CloudWatch cross-account observability, you can use
-- this operation in a monitoring account and view data from the linked
-- source accounts. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-Unified-Cross-Account.html CloudWatch cross-account observability>.
--
-- This operation returns paginated results.
module Amazonka.CloudWatchLogs.FilterLogEvents
  ( -- * Creating a Request
    FilterLogEvents (..),
    newFilterLogEvents,

    -- * Request Lenses
    filterLogEvents_endTime,
    filterLogEvents_filterPattern,
    filterLogEvents_interleaved,
    filterLogEvents_limit,
    filterLogEvents_logGroupIdentifier,
    filterLogEvents_logStreamNamePrefix,
    filterLogEvents_logStreamNames,
    filterLogEvents_nextToken,
    filterLogEvents_startTime,
    filterLogEvents_unmask,
    filterLogEvents_logGroupName,

    -- * Destructuring the Response
    FilterLogEventsResponse (..),
    newFilterLogEventsResponse,

    -- * Response Lenses
    filterLogEventsResponse_events,
    filterLogEventsResponse_nextToken,
    filterLogEventsResponse_searchedLogStreams,
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
  { -- | The end of the time range, expressed as the number of milliseconds after
    -- @Jan 1, 1970 00:00:00 UTC@. Events with a timestamp later than this time
    -- are not returned.
    endTime :: Prelude.Maybe Prelude.Natural,
    -- | The filter pattern to use. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/FilterAndPatternSyntax.html Filter and Pattern Syntax>.
    --
    -- If not provided, all the events are matched.
    filterPattern :: Prelude.Maybe Prelude.Text,
    -- | If the value is true, the operation attempts to provide responses that
    -- contain events from multiple log streams within the log group,
    -- interleaved in a single response. If the value is false, all the matched
    -- log events in the first log stream are searched first, then those in the
    -- next log stream, and so on.
    --
    -- __Important__ As of June 17, 2019, this parameter is ignored and the
    -- value is assumed to be true. The response from this operation always
    -- interleaves events from multiple log streams within a log group.
    interleaved :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of events to return. The default is 10,000 events.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | Specify either the name or ARN of the log group to view log events from.
    -- If the log group is in a source account and you are using a monitoring
    -- account, you must use the log group ARN.
    --
    -- If you specify values for both @logGroupName@ and @logGroupIdentifier@,
    -- the action returns an @InvalidParameterException@ error.
    logGroupIdentifier :: Prelude.Maybe Prelude.Text,
    -- | Filters the results to include only events from log streams that have
    -- names starting with this prefix.
    --
    -- If you specify a value for both @logStreamNamePrefix@ and
    -- @logStreamNames@, but the value for @logStreamNamePrefix@ does not match
    -- any log stream names specified in @logStreamNames@, the action returns
    -- an @InvalidParameterException@ error.
    logStreamNamePrefix :: Prelude.Maybe Prelude.Text,
    -- | Filters the results to only logs from the log streams in this list.
    --
    -- If you specify a value for both @logStreamNamePrefix@ and
    -- @logStreamNames@, the action returns an @InvalidParameterException@
    -- error.
    logStreamNames :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The token for the next set of events to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The start of the time range, expressed as the number of milliseconds
    -- after @Jan 1, 1970 00:00:00 UTC@. Events with a timestamp before this
    -- time are not returned.
    startTime :: Prelude.Maybe Prelude.Natural,
    -- | Specify @true@ to display the log event fields with all sensitive data
    -- unmasked and visible. The default is @false@.
    --
    -- To use this operation with this parameter, you must be signed into an
    -- account with the @logs:Unmask@ permission.
    unmask :: Prelude.Maybe Prelude.Bool,
    -- | The name of the log group to search.
    --
    -- If you specify values for both @logGroupName@ and @logGroupIdentifier@,
    -- the action returns an @InvalidParameterException@ error.
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
-- 'endTime', 'filterLogEvents_endTime' - The end of the time range, expressed as the number of milliseconds after
-- @Jan 1, 1970 00:00:00 UTC@. Events with a timestamp later than this time
-- are not returned.
--
-- 'filterPattern', 'filterLogEvents_filterPattern' - The filter pattern to use. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/FilterAndPatternSyntax.html Filter and Pattern Syntax>.
--
-- If not provided, all the events are matched.
--
-- 'interleaved', 'filterLogEvents_interleaved' - If the value is true, the operation attempts to provide responses that
-- contain events from multiple log streams within the log group,
-- interleaved in a single response. If the value is false, all the matched
-- log events in the first log stream are searched first, then those in the
-- next log stream, and so on.
--
-- __Important__ As of June 17, 2019, this parameter is ignored and the
-- value is assumed to be true. The response from this operation always
-- interleaves events from multiple log streams within a log group.
--
-- 'limit', 'filterLogEvents_limit' - The maximum number of events to return. The default is 10,000 events.
--
-- 'logGroupIdentifier', 'filterLogEvents_logGroupIdentifier' - Specify either the name or ARN of the log group to view log events from.
-- If the log group is in a source account and you are using a monitoring
-- account, you must use the log group ARN.
--
-- If you specify values for both @logGroupName@ and @logGroupIdentifier@,
-- the action returns an @InvalidParameterException@ error.
--
-- 'logStreamNamePrefix', 'filterLogEvents_logStreamNamePrefix' - Filters the results to include only events from log streams that have
-- names starting with this prefix.
--
-- If you specify a value for both @logStreamNamePrefix@ and
-- @logStreamNames@, but the value for @logStreamNamePrefix@ does not match
-- any log stream names specified in @logStreamNames@, the action returns
-- an @InvalidParameterException@ error.
--
-- 'logStreamNames', 'filterLogEvents_logStreamNames' - Filters the results to only logs from the log streams in this list.
--
-- If you specify a value for both @logStreamNamePrefix@ and
-- @logStreamNames@, the action returns an @InvalidParameterException@
-- error.
--
-- 'nextToken', 'filterLogEvents_nextToken' - The token for the next set of events to return. (You received this token
-- from a previous call.)
--
-- 'startTime', 'filterLogEvents_startTime' - The start of the time range, expressed as the number of milliseconds
-- after @Jan 1, 1970 00:00:00 UTC@. Events with a timestamp before this
-- time are not returned.
--
-- 'unmask', 'filterLogEvents_unmask' - Specify @true@ to display the log event fields with all sensitive data
-- unmasked and visible. The default is @false@.
--
-- To use this operation with this parameter, you must be signed into an
-- account with the @logs:Unmask@ permission.
--
-- 'logGroupName', 'filterLogEvents_logGroupName' - The name of the log group to search.
--
-- If you specify values for both @logGroupName@ and @logGroupIdentifier@,
-- the action returns an @InvalidParameterException@ error.
newFilterLogEvents ::
  -- | 'logGroupName'
  Prelude.Text ->
  FilterLogEvents
newFilterLogEvents pLogGroupName_ =
  FilterLogEvents'
    { endTime = Prelude.Nothing,
      filterPattern = Prelude.Nothing,
      interleaved = Prelude.Nothing,
      limit = Prelude.Nothing,
      logGroupIdentifier = Prelude.Nothing,
      logStreamNamePrefix = Prelude.Nothing,
      logStreamNames = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      startTime = Prelude.Nothing,
      unmask = Prelude.Nothing,
      logGroupName = pLogGroupName_
    }

-- | The end of the time range, expressed as the number of milliseconds after
-- @Jan 1, 1970 00:00:00 UTC@. Events with a timestamp later than this time
-- are not returned.
filterLogEvents_endTime :: Lens.Lens' FilterLogEvents (Prelude.Maybe Prelude.Natural)
filterLogEvents_endTime = Lens.lens (\FilterLogEvents' {endTime} -> endTime) (\s@FilterLogEvents' {} a -> s {endTime = a} :: FilterLogEvents)

-- | The filter pattern to use. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/FilterAndPatternSyntax.html Filter and Pattern Syntax>.
--
-- If not provided, all the events are matched.
filterLogEvents_filterPattern :: Lens.Lens' FilterLogEvents (Prelude.Maybe Prelude.Text)
filterLogEvents_filterPattern = Lens.lens (\FilterLogEvents' {filterPattern} -> filterPattern) (\s@FilterLogEvents' {} a -> s {filterPattern = a} :: FilterLogEvents)

-- | If the value is true, the operation attempts to provide responses that
-- contain events from multiple log streams within the log group,
-- interleaved in a single response. If the value is false, all the matched
-- log events in the first log stream are searched first, then those in the
-- next log stream, and so on.
--
-- __Important__ As of June 17, 2019, this parameter is ignored and the
-- value is assumed to be true. The response from this operation always
-- interleaves events from multiple log streams within a log group.
filterLogEvents_interleaved :: Lens.Lens' FilterLogEvents (Prelude.Maybe Prelude.Bool)
filterLogEvents_interleaved = Lens.lens (\FilterLogEvents' {interleaved} -> interleaved) (\s@FilterLogEvents' {} a -> s {interleaved = a} :: FilterLogEvents)

-- | The maximum number of events to return. The default is 10,000 events.
filterLogEvents_limit :: Lens.Lens' FilterLogEvents (Prelude.Maybe Prelude.Natural)
filterLogEvents_limit = Lens.lens (\FilterLogEvents' {limit} -> limit) (\s@FilterLogEvents' {} a -> s {limit = a} :: FilterLogEvents)

-- | Specify either the name or ARN of the log group to view log events from.
-- If the log group is in a source account and you are using a monitoring
-- account, you must use the log group ARN.
--
-- If you specify values for both @logGroupName@ and @logGroupIdentifier@,
-- the action returns an @InvalidParameterException@ error.
filterLogEvents_logGroupIdentifier :: Lens.Lens' FilterLogEvents (Prelude.Maybe Prelude.Text)
filterLogEvents_logGroupIdentifier = Lens.lens (\FilterLogEvents' {logGroupIdentifier} -> logGroupIdentifier) (\s@FilterLogEvents' {} a -> s {logGroupIdentifier = a} :: FilterLogEvents)

-- | Filters the results to include only events from log streams that have
-- names starting with this prefix.
--
-- If you specify a value for both @logStreamNamePrefix@ and
-- @logStreamNames@, but the value for @logStreamNamePrefix@ does not match
-- any log stream names specified in @logStreamNames@, the action returns
-- an @InvalidParameterException@ error.
filterLogEvents_logStreamNamePrefix :: Lens.Lens' FilterLogEvents (Prelude.Maybe Prelude.Text)
filterLogEvents_logStreamNamePrefix = Lens.lens (\FilterLogEvents' {logStreamNamePrefix} -> logStreamNamePrefix) (\s@FilterLogEvents' {} a -> s {logStreamNamePrefix = a} :: FilterLogEvents)

-- | Filters the results to only logs from the log streams in this list.
--
-- If you specify a value for both @logStreamNamePrefix@ and
-- @logStreamNames@, the action returns an @InvalidParameterException@
-- error.
filterLogEvents_logStreamNames :: Lens.Lens' FilterLogEvents (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
filterLogEvents_logStreamNames = Lens.lens (\FilterLogEvents' {logStreamNames} -> logStreamNames) (\s@FilterLogEvents' {} a -> s {logStreamNames = a} :: FilterLogEvents) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of events to return. (You received this token
-- from a previous call.)
filterLogEvents_nextToken :: Lens.Lens' FilterLogEvents (Prelude.Maybe Prelude.Text)
filterLogEvents_nextToken = Lens.lens (\FilterLogEvents' {nextToken} -> nextToken) (\s@FilterLogEvents' {} a -> s {nextToken = a} :: FilterLogEvents)

-- | The start of the time range, expressed as the number of milliseconds
-- after @Jan 1, 1970 00:00:00 UTC@. Events with a timestamp before this
-- time are not returned.
filterLogEvents_startTime :: Lens.Lens' FilterLogEvents (Prelude.Maybe Prelude.Natural)
filterLogEvents_startTime = Lens.lens (\FilterLogEvents' {startTime} -> startTime) (\s@FilterLogEvents' {} a -> s {startTime = a} :: FilterLogEvents)

-- | Specify @true@ to display the log event fields with all sensitive data
-- unmasked and visible. The default is @false@.
--
-- To use this operation with this parameter, you must be signed into an
-- account with the @logs:Unmask@ permission.
filterLogEvents_unmask :: Lens.Lens' FilterLogEvents (Prelude.Maybe Prelude.Bool)
filterLogEvents_unmask = Lens.lens (\FilterLogEvents' {unmask} -> unmask) (\s@FilterLogEvents' {} a -> s {unmask = a} :: FilterLogEvents)

-- | The name of the log group to search.
--
-- If you specify values for both @logGroupName@ and @logGroupIdentifier@,
-- the action returns an @InvalidParameterException@ error.
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
        Prelude.Just
          Prelude.$ rq
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
            Prelude.<$> (x Data..?> "events" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> ( x
                            Data..?> "searchedLogStreams"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable FilterLogEvents where
  hashWithSalt _salt FilterLogEvents' {..} =
    _salt
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` filterPattern
      `Prelude.hashWithSalt` interleaved
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` logGroupIdentifier
      `Prelude.hashWithSalt` logStreamNamePrefix
      `Prelude.hashWithSalt` logStreamNames
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` unmask
      `Prelude.hashWithSalt` logGroupName

instance Prelude.NFData FilterLogEvents where
  rnf FilterLogEvents' {..} =
    Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf filterPattern
      `Prelude.seq` Prelude.rnf interleaved
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf logGroupIdentifier
      `Prelude.seq` Prelude.rnf logStreamNamePrefix
      `Prelude.seq` Prelude.rnf logStreamNames
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf unmask
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
          [ ("endTime" Data..=) Prelude.<$> endTime,
            ("filterPattern" Data..=) Prelude.<$> filterPattern,
            ("interleaved" Data..=) Prelude.<$> interleaved,
            ("limit" Data..=) Prelude.<$> limit,
            ("logGroupIdentifier" Data..=)
              Prelude.<$> logGroupIdentifier,
            ("logStreamNamePrefix" Data..=)
              Prelude.<$> logStreamNamePrefix,
            ("logStreamNames" Data..=)
              Prelude.<$> logStreamNames,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("startTime" Data..=) Prelude.<$> startTime,
            ("unmask" Data..=) Prelude.<$> unmask,
            Prelude.Just ("logGroupName" Data..= logGroupName)
          ]
      )

instance Data.ToPath FilterLogEvents where
  toPath = Prelude.const "/"

instance Data.ToQuery FilterLogEvents where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newFilterLogEventsResponse' smart constructor.
data FilterLogEventsResponse = FilterLogEventsResponse'
  { -- | The matched events.
    events :: Prelude.Maybe [FilteredLogEvent],
    -- | The token to use when requesting the next set of items. The token
    -- expires after 24 hours.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | __Important__ As of May 15, 2020, this parameter is no longer supported.
    -- This parameter returns an empty list.
    --
    -- Indicates which log streams have been searched and whether each has been
    -- searched completely.
    searchedLogStreams :: Prelude.Maybe [SearchedLogStream],
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
-- 'events', 'filterLogEventsResponse_events' - The matched events.
--
-- 'nextToken', 'filterLogEventsResponse_nextToken' - The token to use when requesting the next set of items. The token
-- expires after 24 hours.
--
-- 'searchedLogStreams', 'filterLogEventsResponse_searchedLogStreams' - __Important__ As of May 15, 2020, this parameter is no longer supported.
-- This parameter returns an empty list.
--
-- Indicates which log streams have been searched and whether each has been
-- searched completely.
--
-- 'httpStatus', 'filterLogEventsResponse_httpStatus' - The response's http status code.
newFilterLogEventsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  FilterLogEventsResponse
newFilterLogEventsResponse pHttpStatus_ =
  FilterLogEventsResponse'
    { events = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      searchedLogStreams = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The matched events.
filterLogEventsResponse_events :: Lens.Lens' FilterLogEventsResponse (Prelude.Maybe [FilteredLogEvent])
filterLogEventsResponse_events = Lens.lens (\FilterLogEventsResponse' {events} -> events) (\s@FilterLogEventsResponse' {} a -> s {events = a} :: FilterLogEventsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use when requesting the next set of items. The token
-- expires after 24 hours.
filterLogEventsResponse_nextToken :: Lens.Lens' FilterLogEventsResponse (Prelude.Maybe Prelude.Text)
filterLogEventsResponse_nextToken = Lens.lens (\FilterLogEventsResponse' {nextToken} -> nextToken) (\s@FilterLogEventsResponse' {} a -> s {nextToken = a} :: FilterLogEventsResponse)

-- | __Important__ As of May 15, 2020, this parameter is no longer supported.
-- This parameter returns an empty list.
--
-- Indicates which log streams have been searched and whether each has been
-- searched completely.
filterLogEventsResponse_searchedLogStreams :: Lens.Lens' FilterLogEventsResponse (Prelude.Maybe [SearchedLogStream])
filterLogEventsResponse_searchedLogStreams = Lens.lens (\FilterLogEventsResponse' {searchedLogStreams} -> searchedLogStreams) (\s@FilterLogEventsResponse' {} a -> s {searchedLogStreams = a} :: FilterLogEventsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
filterLogEventsResponse_httpStatus :: Lens.Lens' FilterLogEventsResponse Prelude.Int
filterLogEventsResponse_httpStatus = Lens.lens (\FilterLogEventsResponse' {httpStatus} -> httpStatus) (\s@FilterLogEventsResponse' {} a -> s {httpStatus = a} :: FilterLogEventsResponse)

instance Prelude.NFData FilterLogEventsResponse where
  rnf FilterLogEventsResponse' {..} =
    Prelude.rnf events
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf searchedLogStreams
      `Prelude.seq` Prelude.rnf httpStatus
