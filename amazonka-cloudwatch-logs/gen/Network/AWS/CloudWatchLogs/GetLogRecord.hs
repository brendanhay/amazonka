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
-- Module      : Network.AWS.CloudWatchLogs.GetLogRecord
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all of the fields and values of a single log event. All fields
-- are retrieved, even if the original query that produced the
-- @logRecordPointer@ retrieved only a subset of fields. Fields are
-- returned as field name\/field value pairs.
--
-- The full unparsed log event is returned within @\@message@.
module Network.AWS.CloudWatchLogs.GetLogRecord
  ( -- * Creating a Request
    GetLogRecord (..),
    newGetLogRecord,

    -- * Request Lenses
    getLogRecord_logRecordPointer,

    -- * Destructuring the Response
    GetLogRecordResponse (..),
    newGetLogRecordResponse,

    -- * Response Lenses
    getLogRecordResponse_logRecord,
    getLogRecordResponse_httpStatus,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetLogRecord' smart constructor.
data GetLogRecord = GetLogRecord'
  { -- | The pointer corresponding to the log event record you want to retrieve.
    -- You get this from the response of a @GetQueryResults@ operation. In that
    -- response, the value of the @\@ptr@ field for a log event is the value to
    -- use as @logRecordPointer@ to retrieve that complete log event record.
    logRecordPointer :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetLogRecord' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logRecordPointer', 'getLogRecord_logRecordPointer' - The pointer corresponding to the log event record you want to retrieve.
-- You get this from the response of a @GetQueryResults@ operation. In that
-- response, the value of the @\@ptr@ field for a log event is the value to
-- use as @logRecordPointer@ to retrieve that complete log event record.
newGetLogRecord ::
  -- | 'logRecordPointer'
  Core.Text ->
  GetLogRecord
newGetLogRecord pLogRecordPointer_ =
  GetLogRecord'
    { logRecordPointer =
        pLogRecordPointer_
    }

-- | The pointer corresponding to the log event record you want to retrieve.
-- You get this from the response of a @GetQueryResults@ operation. In that
-- response, the value of the @\@ptr@ field for a log event is the value to
-- use as @logRecordPointer@ to retrieve that complete log event record.
getLogRecord_logRecordPointer :: Lens.Lens' GetLogRecord Core.Text
getLogRecord_logRecordPointer = Lens.lens (\GetLogRecord' {logRecordPointer} -> logRecordPointer) (\s@GetLogRecord' {} a -> s {logRecordPointer = a} :: GetLogRecord)

instance Core.AWSRequest GetLogRecord where
  type AWSResponse GetLogRecord = GetLogRecordResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLogRecordResponse'
            Core.<$> (x Core..?> "logRecord" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetLogRecord

instance Core.NFData GetLogRecord

instance Core.ToHeaders GetLogRecord where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("Logs_20140328.GetLogRecord" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetLogRecord where
  toJSON GetLogRecord' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("logRecordPointer" Core..= logRecordPointer)
          ]
      )

instance Core.ToPath GetLogRecord where
  toPath = Core.const "/"

instance Core.ToQuery GetLogRecord where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetLogRecordResponse' smart constructor.
data GetLogRecordResponse = GetLogRecordResponse'
  { -- | The requested log event, as a JSON string.
    logRecord :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetLogRecordResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logRecord', 'getLogRecordResponse_logRecord' - The requested log event, as a JSON string.
--
-- 'httpStatus', 'getLogRecordResponse_httpStatus' - The response's http status code.
newGetLogRecordResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetLogRecordResponse
newGetLogRecordResponse pHttpStatus_ =
  GetLogRecordResponse'
    { logRecord = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The requested log event, as a JSON string.
getLogRecordResponse_logRecord :: Lens.Lens' GetLogRecordResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
getLogRecordResponse_logRecord = Lens.lens (\GetLogRecordResponse' {logRecord} -> logRecord) (\s@GetLogRecordResponse' {} a -> s {logRecord = a} :: GetLogRecordResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getLogRecordResponse_httpStatus :: Lens.Lens' GetLogRecordResponse Core.Int
getLogRecordResponse_httpStatus = Lens.lens (\GetLogRecordResponse' {httpStatus} -> httpStatus) (\s@GetLogRecordResponse' {} a -> s {httpStatus = a} :: GetLogRecordResponse)

instance Core.NFData GetLogRecordResponse
