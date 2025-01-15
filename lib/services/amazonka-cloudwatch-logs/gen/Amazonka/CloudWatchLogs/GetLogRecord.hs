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
-- Module      : Amazonka.CloudWatchLogs.GetLogRecord
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all of the fields and values of a single log event. All fields
-- are retrieved, even if the original query that produced the
-- @logRecordPointer@ retrieved only a subset of fields. Fields are
-- returned as field name\/field value pairs.
--
-- The full unparsed log event is returned within @\@message@.
module Amazonka.CloudWatchLogs.GetLogRecord
  ( -- * Creating a Request
    GetLogRecord (..),
    newGetLogRecord,

    -- * Request Lenses
    getLogRecord_unmask,
    getLogRecord_logRecordPointer,

    -- * Destructuring the Response
    GetLogRecordResponse (..),
    newGetLogRecordResponse,

    -- * Response Lenses
    getLogRecordResponse_logRecord,
    getLogRecordResponse_httpStatus,
  )
where

import Amazonka.CloudWatchLogs.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetLogRecord' smart constructor.
data GetLogRecord = GetLogRecord'
  { -- | Specify @true@ to display the log event fields with all sensitive data
    -- unmasked and visible. The default is @false@.
    --
    -- To use this operation with this parameter, you must be signed into an
    -- account with the @logs:Unmask@ permission.
    unmask :: Prelude.Maybe Prelude.Bool,
    -- | The pointer corresponding to the log event record you want to retrieve.
    -- You get this from the response of a @GetQueryResults@ operation. In that
    -- response, the value of the @\@ptr@ field for a log event is the value to
    -- use as @logRecordPointer@ to retrieve that complete log event record.
    logRecordPointer :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLogRecord' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unmask', 'getLogRecord_unmask' - Specify @true@ to display the log event fields with all sensitive data
-- unmasked and visible. The default is @false@.
--
-- To use this operation with this parameter, you must be signed into an
-- account with the @logs:Unmask@ permission.
--
-- 'logRecordPointer', 'getLogRecord_logRecordPointer' - The pointer corresponding to the log event record you want to retrieve.
-- You get this from the response of a @GetQueryResults@ operation. In that
-- response, the value of the @\@ptr@ field for a log event is the value to
-- use as @logRecordPointer@ to retrieve that complete log event record.
newGetLogRecord ::
  -- | 'logRecordPointer'
  Prelude.Text ->
  GetLogRecord
newGetLogRecord pLogRecordPointer_ =
  GetLogRecord'
    { unmask = Prelude.Nothing,
      logRecordPointer = pLogRecordPointer_
    }

-- | Specify @true@ to display the log event fields with all sensitive data
-- unmasked and visible. The default is @false@.
--
-- To use this operation with this parameter, you must be signed into an
-- account with the @logs:Unmask@ permission.
getLogRecord_unmask :: Lens.Lens' GetLogRecord (Prelude.Maybe Prelude.Bool)
getLogRecord_unmask = Lens.lens (\GetLogRecord' {unmask} -> unmask) (\s@GetLogRecord' {} a -> s {unmask = a} :: GetLogRecord)

-- | The pointer corresponding to the log event record you want to retrieve.
-- You get this from the response of a @GetQueryResults@ operation. In that
-- response, the value of the @\@ptr@ field for a log event is the value to
-- use as @logRecordPointer@ to retrieve that complete log event record.
getLogRecord_logRecordPointer :: Lens.Lens' GetLogRecord Prelude.Text
getLogRecord_logRecordPointer = Lens.lens (\GetLogRecord' {logRecordPointer} -> logRecordPointer) (\s@GetLogRecord' {} a -> s {logRecordPointer = a} :: GetLogRecord)

instance Core.AWSRequest GetLogRecord where
  type AWSResponse GetLogRecord = GetLogRecordResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLogRecordResponse'
            Prelude.<$> (x Data..?> "logRecord" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLogRecord where
  hashWithSalt _salt GetLogRecord' {..} =
    _salt
      `Prelude.hashWithSalt` unmask
      `Prelude.hashWithSalt` logRecordPointer

instance Prelude.NFData GetLogRecord where
  rnf GetLogRecord' {..} =
    Prelude.rnf unmask `Prelude.seq`
      Prelude.rnf logRecordPointer

instance Data.ToHeaders GetLogRecord where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("Logs_20140328.GetLogRecord" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetLogRecord where
  toJSON GetLogRecord' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("unmask" Data..=) Prelude.<$> unmask,
            Prelude.Just
              ("logRecordPointer" Data..= logRecordPointer)
          ]
      )

instance Data.ToPath GetLogRecord where
  toPath = Prelude.const "/"

instance Data.ToQuery GetLogRecord where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetLogRecordResponse' smart constructor.
data GetLogRecordResponse = GetLogRecordResponse'
  { -- | The requested log event, as a JSON string.
    logRecord :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetLogRecordResponse
newGetLogRecordResponse pHttpStatus_ =
  GetLogRecordResponse'
    { logRecord = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The requested log event, as a JSON string.
getLogRecordResponse_logRecord :: Lens.Lens' GetLogRecordResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getLogRecordResponse_logRecord = Lens.lens (\GetLogRecordResponse' {logRecord} -> logRecord) (\s@GetLogRecordResponse' {} a -> s {logRecord = a} :: GetLogRecordResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getLogRecordResponse_httpStatus :: Lens.Lens' GetLogRecordResponse Prelude.Int
getLogRecordResponse_httpStatus = Lens.lens (\GetLogRecordResponse' {httpStatus} -> httpStatus) (\s@GetLogRecordResponse' {} a -> s {httpStatus = a} :: GetLogRecordResponse)

instance Prelude.NFData GetLogRecordResponse where
  rnf GetLogRecordResponse' {..} =
    Prelude.rnf logRecord `Prelude.seq`
      Prelude.rnf httpStatus
