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
-- Module      : Amazonka.CloudWatchLogs.GetLogEvents
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists log events from the specified log stream. You can list all of the
-- log events or filter using a time range.
--
-- By default, this operation returns as many log events as can fit in a
-- response size of 1MB (up to 10,000 log events). You can get additional
-- log events by specifying one of the tokens in a subsequent call. This
-- operation can return empty results while there are more log events
-- available through the token.
--
-- If you are using CloudWatch cross-account observability, you can use
-- this operation in a monitoring account and view data from the linked
-- source accounts. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-Unified-Cross-Account.html CloudWatch cross-account observability>.
module Amazonka.CloudWatchLogs.GetLogEvents
  ( -- * Creating a Request
    GetLogEvents (..),
    newGetLogEvents,

    -- * Request Lenses
    getLogEvents_endTime,
    getLogEvents_limit,
    getLogEvents_logGroupIdentifier,
    getLogEvents_nextToken,
    getLogEvents_startFromHead,
    getLogEvents_startTime,
    getLogEvents_unmask,
    getLogEvents_logGroupName,
    getLogEvents_logStreamName,

    -- * Destructuring the Response
    GetLogEventsResponse (..),
    newGetLogEventsResponse,

    -- * Response Lenses
    getLogEventsResponse_events,
    getLogEventsResponse_nextBackwardToken,
    getLogEventsResponse_nextForwardToken,
    getLogEventsResponse_httpStatus,
  )
where

import Amazonka.CloudWatchLogs.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetLogEvents' smart constructor.
data GetLogEvents = GetLogEvents'
  { -- | The end of the time range, expressed as the number of milliseconds after
    -- @Jan 1, 1970 00:00:00 UTC@. Events with a timestamp equal to or later
    -- than this time are not included.
    endTime :: Prelude.Maybe Prelude.Natural,
    -- | The maximum number of log events returned. If you don\'t specify a
    -- limit, the default is as many log events as can fit in a response size
    -- of 1 MB (up to 10,000 log events).
    limit :: Prelude.Maybe Prelude.Natural,
    -- | Specify either the name or ARN of the log group to view events from. If
    -- the log group is in a source account and you are using a monitoring
    -- account, you must use the log group ARN.
    --
    -- If you specify values for both @logGroupName@ and @logGroupIdentifier@,
    -- the action returns an @InvalidParameterException@ error.
    logGroupIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | If the value is true, the earliest log events are returned first. If the
    -- value is false, the latest log events are returned first. The default
    -- value is false.
    --
    -- If you are using a previous @nextForwardToken@ value as the @nextToken@
    -- in this operation, you must specify @true@ for @startFromHead@.
    startFromHead :: Prelude.Maybe Prelude.Bool,
    -- | The start of the time range, expressed as the number of milliseconds
    -- after @Jan 1, 1970 00:00:00 UTC@. Events with a timestamp equal to this
    -- time or later than this time are included. Events with a timestamp
    -- earlier than this time are not included.
    startTime :: Prelude.Maybe Prelude.Natural,
    -- | Specify @true@ to display the log event fields with all sensitive data
    -- unmasked and visible. The default is @false@.
    --
    -- To use this operation with this parameter, you must be signed into an
    -- account with the @logs:Unmask@ permission.
    unmask :: Prelude.Maybe Prelude.Bool,
    -- | The name of the log group.
    --
    -- If you specify values for both @logGroupName@ and @logGroupIdentifier@,
    -- the action returns an @InvalidParameterException@ error.
    logGroupName :: Prelude.Text,
    -- | The name of the log stream.
    logStreamName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLogEvents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endTime', 'getLogEvents_endTime' - The end of the time range, expressed as the number of milliseconds after
-- @Jan 1, 1970 00:00:00 UTC@. Events with a timestamp equal to or later
-- than this time are not included.
--
-- 'limit', 'getLogEvents_limit' - The maximum number of log events returned. If you don\'t specify a
-- limit, the default is as many log events as can fit in a response size
-- of 1 MB (up to 10,000 log events).
--
-- 'logGroupIdentifier', 'getLogEvents_logGroupIdentifier' - Specify either the name or ARN of the log group to view events from. If
-- the log group is in a source account and you are using a monitoring
-- account, you must use the log group ARN.
--
-- If you specify values for both @logGroupName@ and @logGroupIdentifier@,
-- the action returns an @InvalidParameterException@ error.
--
-- 'nextToken', 'getLogEvents_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'startFromHead', 'getLogEvents_startFromHead' - If the value is true, the earliest log events are returned first. If the
-- value is false, the latest log events are returned first. The default
-- value is false.
--
-- If you are using a previous @nextForwardToken@ value as the @nextToken@
-- in this operation, you must specify @true@ for @startFromHead@.
--
-- 'startTime', 'getLogEvents_startTime' - The start of the time range, expressed as the number of milliseconds
-- after @Jan 1, 1970 00:00:00 UTC@. Events with a timestamp equal to this
-- time or later than this time are included. Events with a timestamp
-- earlier than this time are not included.
--
-- 'unmask', 'getLogEvents_unmask' - Specify @true@ to display the log event fields with all sensitive data
-- unmasked and visible. The default is @false@.
--
-- To use this operation with this parameter, you must be signed into an
-- account with the @logs:Unmask@ permission.
--
-- 'logGroupName', 'getLogEvents_logGroupName' - The name of the log group.
--
-- If you specify values for both @logGroupName@ and @logGroupIdentifier@,
-- the action returns an @InvalidParameterException@ error.
--
-- 'logStreamName', 'getLogEvents_logStreamName' - The name of the log stream.
newGetLogEvents ::
  -- | 'logGroupName'
  Prelude.Text ->
  -- | 'logStreamName'
  Prelude.Text ->
  GetLogEvents
newGetLogEvents pLogGroupName_ pLogStreamName_ =
  GetLogEvents'
    { endTime = Prelude.Nothing,
      limit = Prelude.Nothing,
      logGroupIdentifier = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      startFromHead = Prelude.Nothing,
      startTime = Prelude.Nothing,
      unmask = Prelude.Nothing,
      logGroupName = pLogGroupName_,
      logStreamName = pLogStreamName_
    }

-- | The end of the time range, expressed as the number of milliseconds after
-- @Jan 1, 1970 00:00:00 UTC@. Events with a timestamp equal to or later
-- than this time are not included.
getLogEvents_endTime :: Lens.Lens' GetLogEvents (Prelude.Maybe Prelude.Natural)
getLogEvents_endTime = Lens.lens (\GetLogEvents' {endTime} -> endTime) (\s@GetLogEvents' {} a -> s {endTime = a} :: GetLogEvents)

-- | The maximum number of log events returned. If you don\'t specify a
-- limit, the default is as many log events as can fit in a response size
-- of 1 MB (up to 10,000 log events).
getLogEvents_limit :: Lens.Lens' GetLogEvents (Prelude.Maybe Prelude.Natural)
getLogEvents_limit = Lens.lens (\GetLogEvents' {limit} -> limit) (\s@GetLogEvents' {} a -> s {limit = a} :: GetLogEvents)

-- | Specify either the name or ARN of the log group to view events from. If
-- the log group is in a source account and you are using a monitoring
-- account, you must use the log group ARN.
--
-- If you specify values for both @logGroupName@ and @logGroupIdentifier@,
-- the action returns an @InvalidParameterException@ error.
getLogEvents_logGroupIdentifier :: Lens.Lens' GetLogEvents (Prelude.Maybe Prelude.Text)
getLogEvents_logGroupIdentifier = Lens.lens (\GetLogEvents' {logGroupIdentifier} -> logGroupIdentifier) (\s@GetLogEvents' {} a -> s {logGroupIdentifier = a} :: GetLogEvents)

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
getLogEvents_nextToken :: Lens.Lens' GetLogEvents (Prelude.Maybe Prelude.Text)
getLogEvents_nextToken = Lens.lens (\GetLogEvents' {nextToken} -> nextToken) (\s@GetLogEvents' {} a -> s {nextToken = a} :: GetLogEvents)

-- | If the value is true, the earliest log events are returned first. If the
-- value is false, the latest log events are returned first. The default
-- value is false.
--
-- If you are using a previous @nextForwardToken@ value as the @nextToken@
-- in this operation, you must specify @true@ for @startFromHead@.
getLogEvents_startFromHead :: Lens.Lens' GetLogEvents (Prelude.Maybe Prelude.Bool)
getLogEvents_startFromHead = Lens.lens (\GetLogEvents' {startFromHead} -> startFromHead) (\s@GetLogEvents' {} a -> s {startFromHead = a} :: GetLogEvents)

-- | The start of the time range, expressed as the number of milliseconds
-- after @Jan 1, 1970 00:00:00 UTC@. Events with a timestamp equal to this
-- time or later than this time are included. Events with a timestamp
-- earlier than this time are not included.
getLogEvents_startTime :: Lens.Lens' GetLogEvents (Prelude.Maybe Prelude.Natural)
getLogEvents_startTime = Lens.lens (\GetLogEvents' {startTime} -> startTime) (\s@GetLogEvents' {} a -> s {startTime = a} :: GetLogEvents)

-- | Specify @true@ to display the log event fields with all sensitive data
-- unmasked and visible. The default is @false@.
--
-- To use this operation with this parameter, you must be signed into an
-- account with the @logs:Unmask@ permission.
getLogEvents_unmask :: Lens.Lens' GetLogEvents (Prelude.Maybe Prelude.Bool)
getLogEvents_unmask = Lens.lens (\GetLogEvents' {unmask} -> unmask) (\s@GetLogEvents' {} a -> s {unmask = a} :: GetLogEvents)

-- | The name of the log group.
--
-- If you specify values for both @logGroupName@ and @logGroupIdentifier@,
-- the action returns an @InvalidParameterException@ error.
getLogEvents_logGroupName :: Lens.Lens' GetLogEvents Prelude.Text
getLogEvents_logGroupName = Lens.lens (\GetLogEvents' {logGroupName} -> logGroupName) (\s@GetLogEvents' {} a -> s {logGroupName = a} :: GetLogEvents)

-- | The name of the log stream.
getLogEvents_logStreamName :: Lens.Lens' GetLogEvents Prelude.Text
getLogEvents_logStreamName = Lens.lens (\GetLogEvents' {logStreamName} -> logStreamName) (\s@GetLogEvents' {} a -> s {logStreamName = a} :: GetLogEvents)

instance Core.AWSRequest GetLogEvents where
  type AWSResponse GetLogEvents = GetLogEventsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLogEventsResponse'
            Prelude.<$> (x Data..?> "events" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextBackwardToken")
            Prelude.<*> (x Data..?> "nextForwardToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLogEvents where
  hashWithSalt _salt GetLogEvents' {..} =
    _salt `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` logGroupIdentifier
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` startFromHead
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` unmask
      `Prelude.hashWithSalt` logGroupName
      `Prelude.hashWithSalt` logStreamName

instance Prelude.NFData GetLogEvents where
  rnf GetLogEvents' {..} =
    Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf logGroupIdentifier
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf startFromHead
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf unmask
      `Prelude.seq` Prelude.rnf logGroupName
      `Prelude.seq` Prelude.rnf logStreamName

instance Data.ToHeaders GetLogEvents where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("Logs_20140328.GetLogEvents" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetLogEvents where
  toJSON GetLogEvents' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("endTime" Data..=) Prelude.<$> endTime,
            ("limit" Data..=) Prelude.<$> limit,
            ("logGroupIdentifier" Data..=)
              Prelude.<$> logGroupIdentifier,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("startFromHead" Data..=) Prelude.<$> startFromHead,
            ("startTime" Data..=) Prelude.<$> startTime,
            ("unmask" Data..=) Prelude.<$> unmask,
            Prelude.Just ("logGroupName" Data..= logGroupName),
            Prelude.Just
              ("logStreamName" Data..= logStreamName)
          ]
      )

instance Data.ToPath GetLogEvents where
  toPath = Prelude.const "/"

instance Data.ToQuery GetLogEvents where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetLogEventsResponse' smart constructor.
data GetLogEventsResponse = GetLogEventsResponse'
  { -- | The events.
    events :: Prelude.Maybe [OutputLogEvent],
    -- | The token for the next set of items in the backward direction. The token
    -- expires after 24 hours. This token is not null. If you have reached the
    -- end of the stream, it returns the same token you passed in.
    nextBackwardToken :: Prelude.Maybe Prelude.Text,
    -- | The token for the next set of items in the forward direction. The token
    -- expires after 24 hours. If you have reached the end of the stream, it
    -- returns the same token you passed in.
    nextForwardToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLogEventsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'events', 'getLogEventsResponse_events' - The events.
--
-- 'nextBackwardToken', 'getLogEventsResponse_nextBackwardToken' - The token for the next set of items in the backward direction. The token
-- expires after 24 hours. This token is not null. If you have reached the
-- end of the stream, it returns the same token you passed in.
--
-- 'nextForwardToken', 'getLogEventsResponse_nextForwardToken' - The token for the next set of items in the forward direction. The token
-- expires after 24 hours. If you have reached the end of the stream, it
-- returns the same token you passed in.
--
-- 'httpStatus', 'getLogEventsResponse_httpStatus' - The response's http status code.
newGetLogEventsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetLogEventsResponse
newGetLogEventsResponse pHttpStatus_ =
  GetLogEventsResponse'
    { events = Prelude.Nothing,
      nextBackwardToken = Prelude.Nothing,
      nextForwardToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The events.
getLogEventsResponse_events :: Lens.Lens' GetLogEventsResponse (Prelude.Maybe [OutputLogEvent])
getLogEventsResponse_events = Lens.lens (\GetLogEventsResponse' {events} -> events) (\s@GetLogEventsResponse' {} a -> s {events = a} :: GetLogEventsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of items in the backward direction. The token
-- expires after 24 hours. This token is not null. If you have reached the
-- end of the stream, it returns the same token you passed in.
getLogEventsResponse_nextBackwardToken :: Lens.Lens' GetLogEventsResponse (Prelude.Maybe Prelude.Text)
getLogEventsResponse_nextBackwardToken = Lens.lens (\GetLogEventsResponse' {nextBackwardToken} -> nextBackwardToken) (\s@GetLogEventsResponse' {} a -> s {nextBackwardToken = a} :: GetLogEventsResponse)

-- | The token for the next set of items in the forward direction. The token
-- expires after 24 hours. If you have reached the end of the stream, it
-- returns the same token you passed in.
getLogEventsResponse_nextForwardToken :: Lens.Lens' GetLogEventsResponse (Prelude.Maybe Prelude.Text)
getLogEventsResponse_nextForwardToken = Lens.lens (\GetLogEventsResponse' {nextForwardToken} -> nextForwardToken) (\s@GetLogEventsResponse' {} a -> s {nextForwardToken = a} :: GetLogEventsResponse)

-- | The response's http status code.
getLogEventsResponse_httpStatus :: Lens.Lens' GetLogEventsResponse Prelude.Int
getLogEventsResponse_httpStatus = Lens.lens (\GetLogEventsResponse' {httpStatus} -> httpStatus) (\s@GetLogEventsResponse' {} a -> s {httpStatus = a} :: GetLogEventsResponse)

instance Prelude.NFData GetLogEventsResponse where
  rnf GetLogEventsResponse' {..} =
    Prelude.rnf events
      `Prelude.seq` Prelude.rnf nextBackwardToken
      `Prelude.seq` Prelude.rnf nextForwardToken
      `Prelude.seq` Prelude.rnf httpStatus
