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
-- Module      : Network.AWS.CloudWatchLogs.GetLogEvents
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.CloudWatchLogs.GetLogEvents
  ( -- * Creating a Request
    GetLogEvents (..),
    newGetLogEvents,

    -- * Request Lenses
    getLogEvents_nextToken,
    getLogEvents_startFromHead,
    getLogEvents_startTime,
    getLogEvents_endTime,
    getLogEvents_limit,
    getLogEvents_logGroupName,
    getLogEvents_logStreamName,

    -- * Destructuring the Response
    GetLogEventsResponse (..),
    newGetLogEventsResponse,

    -- * Response Lenses
    getLogEventsResponse_nextBackwardToken,
    getLogEventsResponse_nextForwardToken,
    getLogEventsResponse_events,
    getLogEventsResponse_httpStatus,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetLogEvents' smart constructor.
data GetLogEvents = GetLogEvents'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    --
    -- Using this token works only when you specify @true@ for @startFromHead@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | If the value is true, the earliest log events are returned first. If the
    -- value is false, the latest log events are returned first. The default
    -- value is false.
    --
    -- If you are using @nextToken@ in this operation, you must specify @true@
    -- for @startFromHead@.
    startFromHead :: Prelude.Maybe Prelude.Bool,
    -- | The start of the time range, expressed as the number of milliseconds
    -- after Jan 1, 1970 00:00:00 UTC. Events with a timestamp equal to this
    -- time or later than this time are included. Events with a timestamp
    -- earlier than this time are not included.
    startTime :: Prelude.Maybe Prelude.Natural,
    -- | The end of the time range, expressed as the number of milliseconds after
    -- Jan 1, 1970 00:00:00 UTC. Events with a timestamp equal to or later than
    -- this time are not included.
    endTime :: Prelude.Maybe Prelude.Natural,
    -- | The maximum number of log events returned. If you don\'t specify a
    -- value, the maximum is as many log events as can fit in a response size
    -- of 1 MB, up to 10,000 log events.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The name of the log group.
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
-- 'nextToken', 'getLogEvents_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- Using this token works only when you specify @true@ for @startFromHead@.
--
-- 'startFromHead', 'getLogEvents_startFromHead' - If the value is true, the earliest log events are returned first. If the
-- value is false, the latest log events are returned first. The default
-- value is false.
--
-- If you are using @nextToken@ in this operation, you must specify @true@
-- for @startFromHead@.
--
-- 'startTime', 'getLogEvents_startTime' - The start of the time range, expressed as the number of milliseconds
-- after Jan 1, 1970 00:00:00 UTC. Events with a timestamp equal to this
-- time or later than this time are included. Events with a timestamp
-- earlier than this time are not included.
--
-- 'endTime', 'getLogEvents_endTime' - The end of the time range, expressed as the number of milliseconds after
-- Jan 1, 1970 00:00:00 UTC. Events with a timestamp equal to or later than
-- this time are not included.
--
-- 'limit', 'getLogEvents_limit' - The maximum number of log events returned. If you don\'t specify a
-- value, the maximum is as many log events as can fit in a response size
-- of 1 MB, up to 10,000 log events.
--
-- 'logGroupName', 'getLogEvents_logGroupName' - The name of the log group.
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
    { nextToken = Prelude.Nothing,
      startFromHead = Prelude.Nothing,
      startTime = Prelude.Nothing,
      endTime = Prelude.Nothing,
      limit = Prelude.Nothing,
      logGroupName = pLogGroupName_,
      logStreamName = pLogStreamName_
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- Using this token works only when you specify @true@ for @startFromHead@.
getLogEvents_nextToken :: Lens.Lens' GetLogEvents (Prelude.Maybe Prelude.Text)
getLogEvents_nextToken = Lens.lens (\GetLogEvents' {nextToken} -> nextToken) (\s@GetLogEvents' {} a -> s {nextToken = a} :: GetLogEvents)

-- | If the value is true, the earliest log events are returned first. If the
-- value is false, the latest log events are returned first. The default
-- value is false.
--
-- If you are using @nextToken@ in this operation, you must specify @true@
-- for @startFromHead@.
getLogEvents_startFromHead :: Lens.Lens' GetLogEvents (Prelude.Maybe Prelude.Bool)
getLogEvents_startFromHead = Lens.lens (\GetLogEvents' {startFromHead} -> startFromHead) (\s@GetLogEvents' {} a -> s {startFromHead = a} :: GetLogEvents)

-- | The start of the time range, expressed as the number of milliseconds
-- after Jan 1, 1970 00:00:00 UTC. Events with a timestamp equal to this
-- time or later than this time are included. Events with a timestamp
-- earlier than this time are not included.
getLogEvents_startTime :: Lens.Lens' GetLogEvents (Prelude.Maybe Prelude.Natural)
getLogEvents_startTime = Lens.lens (\GetLogEvents' {startTime} -> startTime) (\s@GetLogEvents' {} a -> s {startTime = a} :: GetLogEvents)

-- | The end of the time range, expressed as the number of milliseconds after
-- Jan 1, 1970 00:00:00 UTC. Events with a timestamp equal to or later than
-- this time are not included.
getLogEvents_endTime :: Lens.Lens' GetLogEvents (Prelude.Maybe Prelude.Natural)
getLogEvents_endTime = Lens.lens (\GetLogEvents' {endTime} -> endTime) (\s@GetLogEvents' {} a -> s {endTime = a} :: GetLogEvents)

-- | The maximum number of log events returned. If you don\'t specify a
-- value, the maximum is as many log events as can fit in a response size
-- of 1 MB, up to 10,000 log events.
getLogEvents_limit :: Lens.Lens' GetLogEvents (Prelude.Maybe Prelude.Natural)
getLogEvents_limit = Lens.lens (\GetLogEvents' {limit} -> limit) (\s@GetLogEvents' {} a -> s {limit = a} :: GetLogEvents)

-- | The name of the log group.
getLogEvents_logGroupName :: Lens.Lens' GetLogEvents Prelude.Text
getLogEvents_logGroupName = Lens.lens (\GetLogEvents' {logGroupName} -> logGroupName) (\s@GetLogEvents' {} a -> s {logGroupName = a} :: GetLogEvents)

-- | The name of the log stream.
getLogEvents_logStreamName :: Lens.Lens' GetLogEvents Prelude.Text
getLogEvents_logStreamName = Lens.lens (\GetLogEvents' {logStreamName} -> logStreamName) (\s@GetLogEvents' {} a -> s {logStreamName = a} :: GetLogEvents)

instance Core.AWSRequest GetLogEvents where
  type AWSResponse GetLogEvents = GetLogEventsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLogEventsResponse'
            Prelude.<$> (x Core..?> "nextBackwardToken")
            Prelude.<*> (x Core..?> "nextForwardToken")
            Prelude.<*> (x Core..?> "events" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLogEvents

instance Prelude.NFData GetLogEvents

instance Core.ToHeaders GetLogEvents where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("Logs_20140328.GetLogEvents" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetLogEvents where
  toJSON GetLogEvents' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("startFromHead" Core..=) Prelude.<$> startFromHead,
            ("startTime" Core..=) Prelude.<$> startTime,
            ("endTime" Core..=) Prelude.<$> endTime,
            ("limit" Core..=) Prelude.<$> limit,
            Prelude.Just ("logGroupName" Core..= logGroupName),
            Prelude.Just
              ("logStreamName" Core..= logStreamName)
          ]
      )

instance Core.ToPath GetLogEvents where
  toPath = Prelude.const "/"

instance Core.ToQuery GetLogEvents where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetLogEventsResponse' smart constructor.
data GetLogEventsResponse = GetLogEventsResponse'
  { -- | The token for the next set of items in the backward direction. The token
    -- expires after 24 hours. This token is never null. If you have reached
    -- the end of the stream, it returns the same token you passed in.
    nextBackwardToken :: Prelude.Maybe Prelude.Text,
    -- | The token for the next set of items in the forward direction. The token
    -- expires after 24 hours. If you have reached the end of the stream, it
    -- returns the same token you passed in.
    nextForwardToken :: Prelude.Maybe Prelude.Text,
    -- | The events.
    events :: Prelude.Maybe [OutputLogEvent],
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
-- 'nextBackwardToken', 'getLogEventsResponse_nextBackwardToken' - The token for the next set of items in the backward direction. The token
-- expires after 24 hours. This token is never null. If you have reached
-- the end of the stream, it returns the same token you passed in.
--
-- 'nextForwardToken', 'getLogEventsResponse_nextForwardToken' - The token for the next set of items in the forward direction. The token
-- expires after 24 hours. If you have reached the end of the stream, it
-- returns the same token you passed in.
--
-- 'events', 'getLogEventsResponse_events' - The events.
--
-- 'httpStatus', 'getLogEventsResponse_httpStatus' - The response's http status code.
newGetLogEventsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetLogEventsResponse
newGetLogEventsResponse pHttpStatus_ =
  GetLogEventsResponse'
    { nextBackwardToken =
        Prelude.Nothing,
      nextForwardToken = Prelude.Nothing,
      events = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of items in the backward direction. The token
-- expires after 24 hours. This token is never null. If you have reached
-- the end of the stream, it returns the same token you passed in.
getLogEventsResponse_nextBackwardToken :: Lens.Lens' GetLogEventsResponse (Prelude.Maybe Prelude.Text)
getLogEventsResponse_nextBackwardToken = Lens.lens (\GetLogEventsResponse' {nextBackwardToken} -> nextBackwardToken) (\s@GetLogEventsResponse' {} a -> s {nextBackwardToken = a} :: GetLogEventsResponse)

-- | The token for the next set of items in the forward direction. The token
-- expires after 24 hours. If you have reached the end of the stream, it
-- returns the same token you passed in.
getLogEventsResponse_nextForwardToken :: Lens.Lens' GetLogEventsResponse (Prelude.Maybe Prelude.Text)
getLogEventsResponse_nextForwardToken = Lens.lens (\GetLogEventsResponse' {nextForwardToken} -> nextForwardToken) (\s@GetLogEventsResponse' {} a -> s {nextForwardToken = a} :: GetLogEventsResponse)

-- | The events.
getLogEventsResponse_events :: Lens.Lens' GetLogEventsResponse (Prelude.Maybe [OutputLogEvent])
getLogEventsResponse_events = Lens.lens (\GetLogEventsResponse' {events} -> events) (\s@GetLogEventsResponse' {} a -> s {events = a} :: GetLogEventsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getLogEventsResponse_httpStatus :: Lens.Lens' GetLogEventsResponse Prelude.Int
getLogEventsResponse_httpStatus = Lens.lens (\GetLogEventsResponse' {httpStatus} -> httpStatus) (\s@GetLogEventsResponse' {} a -> s {httpStatus = a} :: GetLogEventsResponse)

instance Prelude.NFData GetLogEventsResponse
