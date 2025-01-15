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
-- Module      : Amazonka.Lightsail.GetContainerLog
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the log events of a container of your Amazon Lightsail container
-- service.
--
-- If your container service has more than one node (i.e., a scale greater
-- than 1), then the log events that are returned for the specified
-- container are merged from all nodes on your container service.
--
-- Container logs are retained for a certain amount of time. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/lightsail.html Amazon Lightsail endpoints and quotas>
-- in the /Amazon Web Services General Reference/.
module Amazonka.Lightsail.GetContainerLog
  ( -- * Creating a Request
    GetContainerLog (..),
    newGetContainerLog,

    -- * Request Lenses
    getContainerLog_endTime,
    getContainerLog_filterPattern,
    getContainerLog_pageToken,
    getContainerLog_startTime,
    getContainerLog_serviceName,
    getContainerLog_containerName,

    -- * Destructuring the Response
    GetContainerLogResponse (..),
    newGetContainerLogResponse,

    -- * Response Lenses
    getContainerLogResponse_logEvents,
    getContainerLogResponse_nextPageToken,
    getContainerLogResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetContainerLog' smart constructor.
data GetContainerLog = GetContainerLog'
  { -- | The end of the time interval for which to get log data.
    --
    -- Constraints:
    --
    -- -   Specified in Coordinated Universal Time (UTC).
    --
    -- -   Specified in the Unix time format.
    --
    --     For example, if you wish to use an end time of October 1, 2018, at 9
    --     PM UTC, specify @1538427600@ as the end time.
    --
    -- You can convert a human-friendly time to Unix time format using a
    -- converter like <https://www.epochconverter.com/ Epoch converter>.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The pattern to use to filter the returned log events to a specific term.
    --
    -- The following are a few examples of filter patterns that you can
    -- specify:
    --
    -- -   To return all log events, specify a filter pattern of @\"\"@.
    --
    -- -   To exclude log events that contain the @ERROR@ term, and return all
    --     other log events, specify a filter pattern of @\"-ERROR\"@.
    --
    -- -   To return log events that contain the @ERROR@ term, specify a filter
    --     pattern of @\"ERROR\"@.
    --
    -- -   To return log events that contain both the @ERROR@ and @Exception@
    --     terms, specify a filter pattern of @\"ERROR Exception\"@.
    --
    -- -   To return log events that contain the @ERROR@ /or/ the @Exception@
    --     term, specify a filter pattern of @\"?ERROR ?Exception\"@.
    filterPattern :: Prelude.Maybe Prelude.Text,
    -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetContainerLog@ request. If
    -- your results are paginated, the response will return a next page token
    -- that you can specify as the page token in a subsequent request.
    pageToken :: Prelude.Maybe Prelude.Text,
    -- | The start of the time interval for which to get log data.
    --
    -- Constraints:
    --
    -- -   Specified in Coordinated Universal Time (UTC).
    --
    -- -   Specified in the Unix time format.
    --
    --     For example, if you wish to use a start time of October 1, 2018, at
    --     8 PM UTC, specify @1538424000@ as the start time.
    --
    -- You can convert a human-friendly time to Unix time format using a
    -- converter like <https://www.epochconverter.com/ Epoch converter>.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the container service for which to get a container log.
    serviceName :: Prelude.Text,
    -- | The name of the container that is either running or previously ran on
    -- the container service for which to return a log.
    containerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetContainerLog' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endTime', 'getContainerLog_endTime' - The end of the time interval for which to get log data.
--
-- Constraints:
--
-- -   Specified in Coordinated Universal Time (UTC).
--
-- -   Specified in the Unix time format.
--
--     For example, if you wish to use an end time of October 1, 2018, at 9
--     PM UTC, specify @1538427600@ as the end time.
--
-- You can convert a human-friendly time to Unix time format using a
-- converter like <https://www.epochconverter.com/ Epoch converter>.
--
-- 'filterPattern', 'getContainerLog_filterPattern' - The pattern to use to filter the returned log events to a specific term.
--
-- The following are a few examples of filter patterns that you can
-- specify:
--
-- -   To return all log events, specify a filter pattern of @\"\"@.
--
-- -   To exclude log events that contain the @ERROR@ term, and return all
--     other log events, specify a filter pattern of @\"-ERROR\"@.
--
-- -   To return log events that contain the @ERROR@ term, specify a filter
--     pattern of @\"ERROR\"@.
--
-- -   To return log events that contain both the @ERROR@ and @Exception@
--     terms, specify a filter pattern of @\"ERROR Exception\"@.
--
-- -   To return log events that contain the @ERROR@ /or/ the @Exception@
--     term, specify a filter pattern of @\"?ERROR ?Exception\"@.
--
-- 'pageToken', 'getContainerLog_pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetContainerLog@ request. If
-- your results are paginated, the response will return a next page token
-- that you can specify as the page token in a subsequent request.
--
-- 'startTime', 'getContainerLog_startTime' - The start of the time interval for which to get log data.
--
-- Constraints:
--
-- -   Specified in Coordinated Universal Time (UTC).
--
-- -   Specified in the Unix time format.
--
--     For example, if you wish to use a start time of October 1, 2018, at
--     8 PM UTC, specify @1538424000@ as the start time.
--
-- You can convert a human-friendly time to Unix time format using a
-- converter like <https://www.epochconverter.com/ Epoch converter>.
--
-- 'serviceName', 'getContainerLog_serviceName' - The name of the container service for which to get a container log.
--
-- 'containerName', 'getContainerLog_containerName' - The name of the container that is either running or previously ran on
-- the container service for which to return a log.
newGetContainerLog ::
  -- | 'serviceName'
  Prelude.Text ->
  -- | 'containerName'
  Prelude.Text ->
  GetContainerLog
newGetContainerLog pServiceName_ pContainerName_ =
  GetContainerLog'
    { endTime = Prelude.Nothing,
      filterPattern = Prelude.Nothing,
      pageToken = Prelude.Nothing,
      startTime = Prelude.Nothing,
      serviceName = pServiceName_,
      containerName = pContainerName_
    }

-- | The end of the time interval for which to get log data.
--
-- Constraints:
--
-- -   Specified in Coordinated Universal Time (UTC).
--
-- -   Specified in the Unix time format.
--
--     For example, if you wish to use an end time of October 1, 2018, at 9
--     PM UTC, specify @1538427600@ as the end time.
--
-- You can convert a human-friendly time to Unix time format using a
-- converter like <https://www.epochconverter.com/ Epoch converter>.
getContainerLog_endTime :: Lens.Lens' GetContainerLog (Prelude.Maybe Prelude.UTCTime)
getContainerLog_endTime = Lens.lens (\GetContainerLog' {endTime} -> endTime) (\s@GetContainerLog' {} a -> s {endTime = a} :: GetContainerLog) Prelude.. Lens.mapping Data._Time

-- | The pattern to use to filter the returned log events to a specific term.
--
-- The following are a few examples of filter patterns that you can
-- specify:
--
-- -   To return all log events, specify a filter pattern of @\"\"@.
--
-- -   To exclude log events that contain the @ERROR@ term, and return all
--     other log events, specify a filter pattern of @\"-ERROR\"@.
--
-- -   To return log events that contain the @ERROR@ term, specify a filter
--     pattern of @\"ERROR\"@.
--
-- -   To return log events that contain both the @ERROR@ and @Exception@
--     terms, specify a filter pattern of @\"ERROR Exception\"@.
--
-- -   To return log events that contain the @ERROR@ /or/ the @Exception@
--     term, specify a filter pattern of @\"?ERROR ?Exception\"@.
getContainerLog_filterPattern :: Lens.Lens' GetContainerLog (Prelude.Maybe Prelude.Text)
getContainerLog_filterPattern = Lens.lens (\GetContainerLog' {filterPattern} -> filterPattern) (\s@GetContainerLog' {} a -> s {filterPattern = a} :: GetContainerLog)

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetContainerLog@ request. If
-- your results are paginated, the response will return a next page token
-- that you can specify as the page token in a subsequent request.
getContainerLog_pageToken :: Lens.Lens' GetContainerLog (Prelude.Maybe Prelude.Text)
getContainerLog_pageToken = Lens.lens (\GetContainerLog' {pageToken} -> pageToken) (\s@GetContainerLog' {} a -> s {pageToken = a} :: GetContainerLog)

-- | The start of the time interval for which to get log data.
--
-- Constraints:
--
-- -   Specified in Coordinated Universal Time (UTC).
--
-- -   Specified in the Unix time format.
--
--     For example, if you wish to use a start time of October 1, 2018, at
--     8 PM UTC, specify @1538424000@ as the start time.
--
-- You can convert a human-friendly time to Unix time format using a
-- converter like <https://www.epochconverter.com/ Epoch converter>.
getContainerLog_startTime :: Lens.Lens' GetContainerLog (Prelude.Maybe Prelude.UTCTime)
getContainerLog_startTime = Lens.lens (\GetContainerLog' {startTime} -> startTime) (\s@GetContainerLog' {} a -> s {startTime = a} :: GetContainerLog) Prelude.. Lens.mapping Data._Time

-- | The name of the container service for which to get a container log.
getContainerLog_serviceName :: Lens.Lens' GetContainerLog Prelude.Text
getContainerLog_serviceName = Lens.lens (\GetContainerLog' {serviceName} -> serviceName) (\s@GetContainerLog' {} a -> s {serviceName = a} :: GetContainerLog)

-- | The name of the container that is either running or previously ran on
-- the container service for which to return a log.
getContainerLog_containerName :: Lens.Lens' GetContainerLog Prelude.Text
getContainerLog_containerName = Lens.lens (\GetContainerLog' {containerName} -> containerName) (\s@GetContainerLog' {} a -> s {containerName = a} :: GetContainerLog)

instance Core.AWSRequest GetContainerLog where
  type
    AWSResponse GetContainerLog =
      GetContainerLogResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetContainerLogResponse'
            Prelude.<$> (x Data..?> "logEvents" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextPageToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetContainerLog where
  hashWithSalt _salt GetContainerLog' {..} =
    _salt
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` filterPattern
      `Prelude.hashWithSalt` pageToken
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` serviceName
      `Prelude.hashWithSalt` containerName

instance Prelude.NFData GetContainerLog where
  rnf GetContainerLog' {..} =
    Prelude.rnf endTime `Prelude.seq`
      Prelude.rnf filterPattern `Prelude.seq`
        Prelude.rnf pageToken `Prelude.seq`
          Prelude.rnf startTime `Prelude.seq`
            Prelude.rnf serviceName `Prelude.seq`
              Prelude.rnf containerName

instance Data.ToHeaders GetContainerLog where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.GetContainerLog" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetContainerLog where
  toJSON GetContainerLog' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("endTime" Data..=) Prelude.<$> endTime,
            ("filterPattern" Data..=) Prelude.<$> filterPattern,
            ("pageToken" Data..=) Prelude.<$> pageToken,
            ("startTime" Data..=) Prelude.<$> startTime,
            Prelude.Just ("serviceName" Data..= serviceName),
            Prelude.Just
              ("containerName" Data..= containerName)
          ]
      )

instance Data.ToPath GetContainerLog where
  toPath = Prelude.const "/"

instance Data.ToQuery GetContainerLog where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetContainerLogResponse' smart constructor.
data GetContainerLogResponse = GetContainerLogResponse'
  { -- | An array of objects that describe the log events of a container.
    logEvents :: Prelude.Maybe [ContainerServiceLogEvent],
    -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to
    -- display.
    --
    -- To get the next page of results, perform another @GetContainerLog@
    -- request and specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetContainerLogResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logEvents', 'getContainerLogResponse_logEvents' - An array of objects that describe the log events of a container.
--
-- 'nextPageToken', 'getContainerLogResponse_nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetContainerLog@
-- request and specify the next page token using the @pageToken@ parameter.
--
-- 'httpStatus', 'getContainerLogResponse_httpStatus' - The response's http status code.
newGetContainerLogResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetContainerLogResponse
newGetContainerLogResponse pHttpStatus_ =
  GetContainerLogResponse'
    { logEvents =
        Prelude.Nothing,
      nextPageToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the log events of a container.
getContainerLogResponse_logEvents :: Lens.Lens' GetContainerLogResponse (Prelude.Maybe [ContainerServiceLogEvent])
getContainerLogResponse_logEvents = Lens.lens (\GetContainerLogResponse' {logEvents} -> logEvents) (\s@GetContainerLogResponse' {} a -> s {logEvents = a} :: GetContainerLogResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetContainerLog@
-- request and specify the next page token using the @pageToken@ parameter.
getContainerLogResponse_nextPageToken :: Lens.Lens' GetContainerLogResponse (Prelude.Maybe Prelude.Text)
getContainerLogResponse_nextPageToken = Lens.lens (\GetContainerLogResponse' {nextPageToken} -> nextPageToken) (\s@GetContainerLogResponse' {} a -> s {nextPageToken = a} :: GetContainerLogResponse)

-- | The response's http status code.
getContainerLogResponse_httpStatus :: Lens.Lens' GetContainerLogResponse Prelude.Int
getContainerLogResponse_httpStatus = Lens.lens (\GetContainerLogResponse' {httpStatus} -> httpStatus) (\s@GetContainerLogResponse' {} a -> s {httpStatus = a} :: GetContainerLogResponse)

instance Prelude.NFData GetContainerLogResponse where
  rnf GetContainerLogResponse' {..} =
    Prelude.rnf logEvents `Prelude.seq`
      Prelude.rnf nextPageToken `Prelude.seq`
        Prelude.rnf httpStatus
