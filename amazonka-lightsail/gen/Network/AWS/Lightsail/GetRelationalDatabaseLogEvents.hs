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
-- Module      : Network.AWS.Lightsail.GetRelationalDatabaseLogEvents
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of log events for a database in Amazon Lightsail.
module Network.AWS.Lightsail.GetRelationalDatabaseLogEvents
  ( -- * Creating a Request
    GetRelationalDatabaseLogEvents (..),
    newGetRelationalDatabaseLogEvents,

    -- * Request Lenses
    getRelationalDatabaseLogEvents_startFromHead,
    getRelationalDatabaseLogEvents_pageToken,
    getRelationalDatabaseLogEvents_startTime,
    getRelationalDatabaseLogEvents_endTime,
    getRelationalDatabaseLogEvents_relationalDatabaseName,
    getRelationalDatabaseLogEvents_logStreamName,

    -- * Destructuring the Response
    GetRelationalDatabaseLogEventsResponse (..),
    newGetRelationalDatabaseLogEventsResponse,

    -- * Response Lenses
    getRelationalDatabaseLogEventsResponse_nextBackwardToken,
    getRelationalDatabaseLogEventsResponse_nextForwardToken,
    getRelationalDatabaseLogEventsResponse_resourceLogEvents,
    getRelationalDatabaseLogEventsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetRelationalDatabaseLogEvents' smart constructor.
data GetRelationalDatabaseLogEvents = GetRelationalDatabaseLogEvents'
  { -- | Parameter to specify if the log should start from head or tail. If
    -- @true@ is specified, the log event starts from the head of the log. If
    -- @false@ is specified, the log event starts from the tail of the log.
    --
    -- For PostgreSQL, the default value of @false@ is the only option
    -- available.
    startFromHead :: Prelude.Maybe Prelude.Bool,
    -- | The token to advance to the next or previous page of results from your
    -- request.
    --
    -- To get a page token, perform an initial @GetRelationalDatabaseLogEvents@
    -- request. If your results are paginated, the response will return a next
    -- forward token and\/or next backward token that you can specify as the
    -- page token in a subsequent request.
    pageToken :: Prelude.Maybe Prelude.Text,
    -- | The start of the time interval from which to get log events.
    --
    -- Constraints:
    --
    -- -   Specified in Coordinated Universal Time (UTC).
    --
    -- -   Specified in the Unix time format.
    --
    --     For example, if you wish to use a start time of October 1, 2018, at
    --     8 PM UTC, then you input @1538424000@ as the start time.
    startTime :: Prelude.Maybe Core.POSIX,
    -- | The end of the time interval from which to get log events.
    --
    -- Constraints:
    --
    -- -   Specified in Coordinated Universal Time (UTC).
    --
    -- -   Specified in the Unix time format.
    --
    --     For example, if you wish to use an end time of October 1, 2018, at 8
    --     PM UTC, then you input @1538424000@ as the end time.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | The name of your database for which to get log events.
    relationalDatabaseName :: Prelude.Text,
    -- | The name of the log stream.
    --
    -- Use the @get relational database log streams@ operation to get a list of
    -- available log streams.
    logStreamName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRelationalDatabaseLogEvents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startFromHead', 'getRelationalDatabaseLogEvents_startFromHead' - Parameter to specify if the log should start from head or tail. If
-- @true@ is specified, the log event starts from the head of the log. If
-- @false@ is specified, the log event starts from the tail of the log.
--
-- For PostgreSQL, the default value of @false@ is the only option
-- available.
--
-- 'pageToken', 'getRelationalDatabaseLogEvents_pageToken' - The token to advance to the next or previous page of results from your
-- request.
--
-- To get a page token, perform an initial @GetRelationalDatabaseLogEvents@
-- request. If your results are paginated, the response will return a next
-- forward token and\/or next backward token that you can specify as the
-- page token in a subsequent request.
--
-- 'startTime', 'getRelationalDatabaseLogEvents_startTime' - The start of the time interval from which to get log events.
--
-- Constraints:
--
-- -   Specified in Coordinated Universal Time (UTC).
--
-- -   Specified in the Unix time format.
--
--     For example, if you wish to use a start time of October 1, 2018, at
--     8 PM UTC, then you input @1538424000@ as the start time.
--
-- 'endTime', 'getRelationalDatabaseLogEvents_endTime' - The end of the time interval from which to get log events.
--
-- Constraints:
--
-- -   Specified in Coordinated Universal Time (UTC).
--
-- -   Specified in the Unix time format.
--
--     For example, if you wish to use an end time of October 1, 2018, at 8
--     PM UTC, then you input @1538424000@ as the end time.
--
-- 'relationalDatabaseName', 'getRelationalDatabaseLogEvents_relationalDatabaseName' - The name of your database for which to get log events.
--
-- 'logStreamName', 'getRelationalDatabaseLogEvents_logStreamName' - The name of the log stream.
--
-- Use the @get relational database log streams@ operation to get a list of
-- available log streams.
newGetRelationalDatabaseLogEvents ::
  -- | 'relationalDatabaseName'
  Prelude.Text ->
  -- | 'logStreamName'
  Prelude.Text ->
  GetRelationalDatabaseLogEvents
newGetRelationalDatabaseLogEvents
  pRelationalDatabaseName_
  pLogStreamName_ =
    GetRelationalDatabaseLogEvents'
      { startFromHead =
          Prelude.Nothing,
        pageToken = Prelude.Nothing,
        startTime = Prelude.Nothing,
        endTime = Prelude.Nothing,
        relationalDatabaseName =
          pRelationalDatabaseName_,
        logStreamName = pLogStreamName_
      }

-- | Parameter to specify if the log should start from head or tail. If
-- @true@ is specified, the log event starts from the head of the log. If
-- @false@ is specified, the log event starts from the tail of the log.
--
-- For PostgreSQL, the default value of @false@ is the only option
-- available.
getRelationalDatabaseLogEvents_startFromHead :: Lens.Lens' GetRelationalDatabaseLogEvents (Prelude.Maybe Prelude.Bool)
getRelationalDatabaseLogEvents_startFromHead = Lens.lens (\GetRelationalDatabaseLogEvents' {startFromHead} -> startFromHead) (\s@GetRelationalDatabaseLogEvents' {} a -> s {startFromHead = a} :: GetRelationalDatabaseLogEvents)

-- | The token to advance to the next or previous page of results from your
-- request.
--
-- To get a page token, perform an initial @GetRelationalDatabaseLogEvents@
-- request. If your results are paginated, the response will return a next
-- forward token and\/or next backward token that you can specify as the
-- page token in a subsequent request.
getRelationalDatabaseLogEvents_pageToken :: Lens.Lens' GetRelationalDatabaseLogEvents (Prelude.Maybe Prelude.Text)
getRelationalDatabaseLogEvents_pageToken = Lens.lens (\GetRelationalDatabaseLogEvents' {pageToken} -> pageToken) (\s@GetRelationalDatabaseLogEvents' {} a -> s {pageToken = a} :: GetRelationalDatabaseLogEvents)

-- | The start of the time interval from which to get log events.
--
-- Constraints:
--
-- -   Specified in Coordinated Universal Time (UTC).
--
-- -   Specified in the Unix time format.
--
--     For example, if you wish to use a start time of October 1, 2018, at
--     8 PM UTC, then you input @1538424000@ as the start time.
getRelationalDatabaseLogEvents_startTime :: Lens.Lens' GetRelationalDatabaseLogEvents (Prelude.Maybe Prelude.UTCTime)
getRelationalDatabaseLogEvents_startTime = Lens.lens (\GetRelationalDatabaseLogEvents' {startTime} -> startTime) (\s@GetRelationalDatabaseLogEvents' {} a -> s {startTime = a} :: GetRelationalDatabaseLogEvents) Prelude.. Lens.mapping Core._Time

-- | The end of the time interval from which to get log events.
--
-- Constraints:
--
-- -   Specified in Coordinated Universal Time (UTC).
--
-- -   Specified in the Unix time format.
--
--     For example, if you wish to use an end time of October 1, 2018, at 8
--     PM UTC, then you input @1538424000@ as the end time.
getRelationalDatabaseLogEvents_endTime :: Lens.Lens' GetRelationalDatabaseLogEvents (Prelude.Maybe Prelude.UTCTime)
getRelationalDatabaseLogEvents_endTime = Lens.lens (\GetRelationalDatabaseLogEvents' {endTime} -> endTime) (\s@GetRelationalDatabaseLogEvents' {} a -> s {endTime = a} :: GetRelationalDatabaseLogEvents) Prelude.. Lens.mapping Core._Time

-- | The name of your database for which to get log events.
getRelationalDatabaseLogEvents_relationalDatabaseName :: Lens.Lens' GetRelationalDatabaseLogEvents Prelude.Text
getRelationalDatabaseLogEvents_relationalDatabaseName = Lens.lens (\GetRelationalDatabaseLogEvents' {relationalDatabaseName} -> relationalDatabaseName) (\s@GetRelationalDatabaseLogEvents' {} a -> s {relationalDatabaseName = a} :: GetRelationalDatabaseLogEvents)

-- | The name of the log stream.
--
-- Use the @get relational database log streams@ operation to get a list of
-- available log streams.
getRelationalDatabaseLogEvents_logStreamName :: Lens.Lens' GetRelationalDatabaseLogEvents Prelude.Text
getRelationalDatabaseLogEvents_logStreamName = Lens.lens (\GetRelationalDatabaseLogEvents' {logStreamName} -> logStreamName) (\s@GetRelationalDatabaseLogEvents' {} a -> s {logStreamName = a} :: GetRelationalDatabaseLogEvents)

instance
  Core.AWSRequest
    GetRelationalDatabaseLogEvents
  where
  type
    AWSResponse GetRelationalDatabaseLogEvents =
      GetRelationalDatabaseLogEventsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRelationalDatabaseLogEventsResponse'
            Prelude.<$> (x Core..?> "nextBackwardToken")
            Prelude.<*> (x Core..?> "nextForwardToken")
            Prelude.<*> ( x Core..?> "resourceLogEvents"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetRelationalDatabaseLogEvents

instance
  Prelude.NFData
    GetRelationalDatabaseLogEvents

instance
  Core.ToHeaders
    GetRelationalDatabaseLogEvents
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.GetRelationalDatabaseLogEvents" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetRelationalDatabaseLogEvents where
  toJSON GetRelationalDatabaseLogEvents' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("startFromHead" Core..=) Prelude.<$> startFromHead,
            ("pageToken" Core..=) Prelude.<$> pageToken,
            ("startTime" Core..=) Prelude.<$> startTime,
            ("endTime" Core..=) Prelude.<$> endTime,
            Prelude.Just
              ( "relationalDatabaseName"
                  Core..= relationalDatabaseName
              ),
            Prelude.Just
              ("logStreamName" Core..= logStreamName)
          ]
      )

instance Core.ToPath GetRelationalDatabaseLogEvents where
  toPath = Prelude.const "/"

instance Core.ToQuery GetRelationalDatabaseLogEvents where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRelationalDatabaseLogEventsResponse' smart constructor.
data GetRelationalDatabaseLogEventsResponse = GetRelationalDatabaseLogEventsResponse'
  { -- | A token used for advancing to the previous page of results from your get
    -- relational database log events request.
    nextBackwardToken :: Prelude.Maybe Prelude.Text,
    -- | A token used for advancing to the next page of results from your get
    -- relational database log events request.
    nextForwardToken :: Prelude.Maybe Prelude.Text,
    -- | An object describing the result of your get relational database log
    -- events request.
    resourceLogEvents :: Prelude.Maybe [LogEvent],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRelationalDatabaseLogEventsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextBackwardToken', 'getRelationalDatabaseLogEventsResponse_nextBackwardToken' - A token used for advancing to the previous page of results from your get
-- relational database log events request.
--
-- 'nextForwardToken', 'getRelationalDatabaseLogEventsResponse_nextForwardToken' - A token used for advancing to the next page of results from your get
-- relational database log events request.
--
-- 'resourceLogEvents', 'getRelationalDatabaseLogEventsResponse_resourceLogEvents' - An object describing the result of your get relational database log
-- events request.
--
-- 'httpStatus', 'getRelationalDatabaseLogEventsResponse_httpStatus' - The response's http status code.
newGetRelationalDatabaseLogEventsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRelationalDatabaseLogEventsResponse
newGetRelationalDatabaseLogEventsResponse
  pHttpStatus_ =
    GetRelationalDatabaseLogEventsResponse'
      { nextBackwardToken =
          Prelude.Nothing,
        nextForwardToken = Prelude.Nothing,
        resourceLogEvents = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A token used for advancing to the previous page of results from your get
-- relational database log events request.
getRelationalDatabaseLogEventsResponse_nextBackwardToken :: Lens.Lens' GetRelationalDatabaseLogEventsResponse (Prelude.Maybe Prelude.Text)
getRelationalDatabaseLogEventsResponse_nextBackwardToken = Lens.lens (\GetRelationalDatabaseLogEventsResponse' {nextBackwardToken} -> nextBackwardToken) (\s@GetRelationalDatabaseLogEventsResponse' {} a -> s {nextBackwardToken = a} :: GetRelationalDatabaseLogEventsResponse)

-- | A token used for advancing to the next page of results from your get
-- relational database log events request.
getRelationalDatabaseLogEventsResponse_nextForwardToken :: Lens.Lens' GetRelationalDatabaseLogEventsResponse (Prelude.Maybe Prelude.Text)
getRelationalDatabaseLogEventsResponse_nextForwardToken = Lens.lens (\GetRelationalDatabaseLogEventsResponse' {nextForwardToken} -> nextForwardToken) (\s@GetRelationalDatabaseLogEventsResponse' {} a -> s {nextForwardToken = a} :: GetRelationalDatabaseLogEventsResponse)

-- | An object describing the result of your get relational database log
-- events request.
getRelationalDatabaseLogEventsResponse_resourceLogEvents :: Lens.Lens' GetRelationalDatabaseLogEventsResponse (Prelude.Maybe [LogEvent])
getRelationalDatabaseLogEventsResponse_resourceLogEvents = Lens.lens (\GetRelationalDatabaseLogEventsResponse' {resourceLogEvents} -> resourceLogEvents) (\s@GetRelationalDatabaseLogEventsResponse' {} a -> s {resourceLogEvents = a} :: GetRelationalDatabaseLogEventsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getRelationalDatabaseLogEventsResponse_httpStatus :: Lens.Lens' GetRelationalDatabaseLogEventsResponse Prelude.Int
getRelationalDatabaseLogEventsResponse_httpStatus = Lens.lens (\GetRelationalDatabaseLogEventsResponse' {httpStatus} -> httpStatus) (\s@GetRelationalDatabaseLogEventsResponse' {} a -> s {httpStatus = a} :: GetRelationalDatabaseLogEventsResponse)

instance
  Prelude.NFData
    GetRelationalDatabaseLogEventsResponse
