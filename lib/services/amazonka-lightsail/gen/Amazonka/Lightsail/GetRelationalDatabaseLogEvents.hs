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
-- Module      : Amazonka.Lightsail.GetRelationalDatabaseLogEvents
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of log events for a database in Amazon Lightsail.
module Amazonka.Lightsail.GetRelationalDatabaseLogEvents
  ( -- * Creating a Request
    GetRelationalDatabaseLogEvents (..),
    newGetRelationalDatabaseLogEvents,

    -- * Request Lenses
    getRelationalDatabaseLogEvents_endTime,
    getRelationalDatabaseLogEvents_pageToken,
    getRelationalDatabaseLogEvents_startFromHead,
    getRelationalDatabaseLogEvents_startTime,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetRelationalDatabaseLogEvents' smart constructor.
data GetRelationalDatabaseLogEvents = GetRelationalDatabaseLogEvents'
  { -- | The end of the time interval from which to get log events.
    --
    -- Constraints:
    --
    -- -   Specified in Coordinated Universal Time (UTC).
    --
    -- -   Specified in the Unix time format.
    --
    --     For example, if you wish to use an end time of October 1, 2018, at 8
    --     PM UTC, then you input @1538424000@ as the end time.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The token to advance to the next or previous page of results from your
    -- request.
    --
    -- To get a page token, perform an initial @GetRelationalDatabaseLogEvents@
    -- request. If your results are paginated, the response will return a next
    -- forward token and\/or next backward token that you can specify as the
    -- page token in a subsequent request.
    pageToken :: Prelude.Maybe Prelude.Text,
    -- | Parameter to specify if the log should start from head or tail. If
    -- @true@ is specified, the log event starts from the head of the log. If
    -- @false@ is specified, the log event starts from the tail of the log.
    --
    -- For PostgreSQL, the default value of @false@ is the only option
    -- available.
    startFromHead :: Prelude.Maybe Prelude.Bool,
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
    startTime :: Prelude.Maybe Data.POSIX,
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
-- 'pageToken', 'getRelationalDatabaseLogEvents_pageToken' - The token to advance to the next or previous page of results from your
-- request.
--
-- To get a page token, perform an initial @GetRelationalDatabaseLogEvents@
-- request. If your results are paginated, the response will return a next
-- forward token and\/or next backward token that you can specify as the
-- page token in a subsequent request.
--
-- 'startFromHead', 'getRelationalDatabaseLogEvents_startFromHead' - Parameter to specify if the log should start from head or tail. If
-- @true@ is specified, the log event starts from the head of the log. If
-- @false@ is specified, the log event starts from the tail of the log.
--
-- For PostgreSQL, the default value of @false@ is the only option
-- available.
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
      { endTime =
          Prelude.Nothing,
        pageToken = Prelude.Nothing,
        startFromHead = Prelude.Nothing,
        startTime = Prelude.Nothing,
        relationalDatabaseName =
          pRelationalDatabaseName_,
        logStreamName = pLogStreamName_
      }

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
getRelationalDatabaseLogEvents_endTime = Lens.lens (\GetRelationalDatabaseLogEvents' {endTime} -> endTime) (\s@GetRelationalDatabaseLogEvents' {} a -> s {endTime = a} :: GetRelationalDatabaseLogEvents) Prelude.. Lens.mapping Data._Time

-- | The token to advance to the next or previous page of results from your
-- request.
--
-- To get a page token, perform an initial @GetRelationalDatabaseLogEvents@
-- request. If your results are paginated, the response will return a next
-- forward token and\/or next backward token that you can specify as the
-- page token in a subsequent request.
getRelationalDatabaseLogEvents_pageToken :: Lens.Lens' GetRelationalDatabaseLogEvents (Prelude.Maybe Prelude.Text)
getRelationalDatabaseLogEvents_pageToken = Lens.lens (\GetRelationalDatabaseLogEvents' {pageToken} -> pageToken) (\s@GetRelationalDatabaseLogEvents' {} a -> s {pageToken = a} :: GetRelationalDatabaseLogEvents)

-- | Parameter to specify if the log should start from head or tail. If
-- @true@ is specified, the log event starts from the head of the log. If
-- @false@ is specified, the log event starts from the tail of the log.
--
-- For PostgreSQL, the default value of @false@ is the only option
-- available.
getRelationalDatabaseLogEvents_startFromHead :: Lens.Lens' GetRelationalDatabaseLogEvents (Prelude.Maybe Prelude.Bool)
getRelationalDatabaseLogEvents_startFromHead = Lens.lens (\GetRelationalDatabaseLogEvents' {startFromHead} -> startFromHead) (\s@GetRelationalDatabaseLogEvents' {} a -> s {startFromHead = a} :: GetRelationalDatabaseLogEvents)

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
getRelationalDatabaseLogEvents_startTime = Lens.lens (\GetRelationalDatabaseLogEvents' {startTime} -> startTime) (\s@GetRelationalDatabaseLogEvents' {} a -> s {startTime = a} :: GetRelationalDatabaseLogEvents) Prelude.. Lens.mapping Data._Time

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRelationalDatabaseLogEventsResponse'
            Prelude.<$> (x Data..?> "nextBackwardToken")
            Prelude.<*> (x Data..?> "nextForwardToken")
            Prelude.<*> ( x
                            Data..?> "resourceLogEvents"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetRelationalDatabaseLogEvents
  where
  hashWithSalt
    _salt
    GetRelationalDatabaseLogEvents' {..} =
      _salt
        `Prelude.hashWithSalt` endTime
        `Prelude.hashWithSalt` pageToken
        `Prelude.hashWithSalt` startFromHead
        `Prelude.hashWithSalt` startTime
        `Prelude.hashWithSalt` relationalDatabaseName
        `Prelude.hashWithSalt` logStreamName

instance
  Prelude.NFData
    GetRelationalDatabaseLogEvents
  where
  rnf GetRelationalDatabaseLogEvents' {..} =
    Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf pageToken
      `Prelude.seq` Prelude.rnf startFromHead
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf relationalDatabaseName
      `Prelude.seq` Prelude.rnf logStreamName

instance
  Data.ToHeaders
    GetRelationalDatabaseLogEvents
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.GetRelationalDatabaseLogEvents" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetRelationalDatabaseLogEvents where
  toJSON GetRelationalDatabaseLogEvents' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("endTime" Data..=) Prelude.<$> endTime,
            ("pageToken" Data..=) Prelude.<$> pageToken,
            ("startFromHead" Data..=) Prelude.<$> startFromHead,
            ("startTime" Data..=) Prelude.<$> startTime,
            Prelude.Just
              ( "relationalDatabaseName"
                  Data..= relationalDatabaseName
              ),
            Prelude.Just
              ("logStreamName" Data..= logStreamName)
          ]
      )

instance Data.ToPath GetRelationalDatabaseLogEvents where
  toPath = Prelude.const "/"

instance Data.ToQuery GetRelationalDatabaseLogEvents where
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
getRelationalDatabaseLogEventsResponse_resourceLogEvents = Lens.lens (\GetRelationalDatabaseLogEventsResponse' {resourceLogEvents} -> resourceLogEvents) (\s@GetRelationalDatabaseLogEventsResponse' {} a -> s {resourceLogEvents = a} :: GetRelationalDatabaseLogEventsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getRelationalDatabaseLogEventsResponse_httpStatus :: Lens.Lens' GetRelationalDatabaseLogEventsResponse Prelude.Int
getRelationalDatabaseLogEventsResponse_httpStatus = Lens.lens (\GetRelationalDatabaseLogEventsResponse' {httpStatus} -> httpStatus) (\s@GetRelationalDatabaseLogEventsResponse' {} a -> s {httpStatus = a} :: GetRelationalDatabaseLogEventsResponse)

instance
  Prelude.NFData
    GetRelationalDatabaseLogEventsResponse
  where
  rnf GetRelationalDatabaseLogEventsResponse' {..} =
    Prelude.rnf nextBackwardToken
      `Prelude.seq` Prelude.rnf nextForwardToken
      `Prelude.seq` Prelude.rnf resourceLogEvents
      `Prelude.seq` Prelude.rnf httpStatus
