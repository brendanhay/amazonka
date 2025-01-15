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
-- Module      : Amazonka.CloudWatchLogs.PutLogEvents
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads a batch of log events to the specified log stream.
--
-- The sequence token is now ignored in @PutLogEvents@ actions.
-- @PutLogEvents@ actions are always accepted and never return
-- @InvalidSequenceTokenException@ or @DataAlreadyAcceptedException@ even
-- if the sequence token is not valid. You can use parallel @PutLogEvents@
-- actions on the same log stream.
--
-- The batch of events must satisfy the following constraints:
--
-- -   The maximum batch size is 1,048,576 bytes. This size is calculated
--     as the sum of all event messages in UTF-8, plus 26 bytes for each
--     log event.
--
-- -   None of the log events in the batch can be more than 2 hours in the
--     future.
--
-- -   None of the log events in the batch can be more than 14 days in the
--     past. Also, none of the log events can be from earlier than the
--     retention period of the log group.
--
-- -   The log events in the batch must be in chronological order by their
--     timestamp. The timestamp is the time that the event occurred,
--     expressed as the number of milliseconds after
--     @Jan 1, 1970 00:00:00 UTC@. (In Amazon Web Services Tools for
--     PowerShell and the Amazon Web Services SDK for .NET, the timestamp
--     is specified in .NET format: @yyyy-mm-ddThh:mm:ss@. For example,
--     @2017-09-15T13:45:30@.)
--
-- -   A batch of log events in a single request cannot span more than 24
--     hours. Otherwise, the operation fails.
--
-- -   The maximum number of log events in a batch is 10,000.
--
-- -   The quota of five requests per second per log stream has been
--     removed. Instead, @PutLogEvents@ actions are throttled based on a
--     per-second per-account quota. You can request an increase to the
--     per-second throttling quota by using the Service Quotas service.
--
-- If a call to @PutLogEvents@ returns \"UnrecognizedClientException\" the
-- most likely cause is a non-valid Amazon Web Services access key ID or
-- secret key.
module Amazonka.CloudWatchLogs.PutLogEvents
  ( -- * Creating a Request
    PutLogEvents (..),
    newPutLogEvents,

    -- * Request Lenses
    putLogEvents_sequenceToken,
    putLogEvents_logGroupName,
    putLogEvents_logStreamName,
    putLogEvents_logEvents,

    -- * Destructuring the Response
    PutLogEventsResponse (..),
    newPutLogEventsResponse,

    -- * Response Lenses
    putLogEventsResponse_nextSequenceToken,
    putLogEventsResponse_rejectedLogEventsInfo,
    putLogEventsResponse_httpStatus,
  )
where

import Amazonka.CloudWatchLogs.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutLogEvents' smart constructor.
data PutLogEvents = PutLogEvents'
  { -- | The sequence token obtained from the response of the previous
    -- @PutLogEvents@ call.
    --
    -- The @sequenceToken@ parameter is now ignored in @PutLogEvents@ actions.
    -- @PutLogEvents@ actions are now accepted and never return
    -- @InvalidSequenceTokenException@ or @DataAlreadyAcceptedException@ even
    -- if the sequence token is not valid.
    sequenceToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the log group.
    logGroupName :: Prelude.Text,
    -- | The name of the log stream.
    logStreamName :: Prelude.Text,
    -- | The log events.
    logEvents :: Prelude.NonEmpty InputLogEvent
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutLogEvents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sequenceToken', 'putLogEvents_sequenceToken' - The sequence token obtained from the response of the previous
-- @PutLogEvents@ call.
--
-- The @sequenceToken@ parameter is now ignored in @PutLogEvents@ actions.
-- @PutLogEvents@ actions are now accepted and never return
-- @InvalidSequenceTokenException@ or @DataAlreadyAcceptedException@ even
-- if the sequence token is not valid.
--
-- 'logGroupName', 'putLogEvents_logGroupName' - The name of the log group.
--
-- 'logStreamName', 'putLogEvents_logStreamName' - The name of the log stream.
--
-- 'logEvents', 'putLogEvents_logEvents' - The log events.
newPutLogEvents ::
  -- | 'logGroupName'
  Prelude.Text ->
  -- | 'logStreamName'
  Prelude.Text ->
  -- | 'logEvents'
  Prelude.NonEmpty InputLogEvent ->
  PutLogEvents
newPutLogEvents
  pLogGroupName_
  pLogStreamName_
  pLogEvents_ =
    PutLogEvents'
      { sequenceToken = Prelude.Nothing,
        logGroupName = pLogGroupName_,
        logStreamName = pLogStreamName_,
        logEvents = Lens.coerced Lens.# pLogEvents_
      }

-- | The sequence token obtained from the response of the previous
-- @PutLogEvents@ call.
--
-- The @sequenceToken@ parameter is now ignored in @PutLogEvents@ actions.
-- @PutLogEvents@ actions are now accepted and never return
-- @InvalidSequenceTokenException@ or @DataAlreadyAcceptedException@ even
-- if the sequence token is not valid.
putLogEvents_sequenceToken :: Lens.Lens' PutLogEvents (Prelude.Maybe Prelude.Text)
putLogEvents_sequenceToken = Lens.lens (\PutLogEvents' {sequenceToken} -> sequenceToken) (\s@PutLogEvents' {} a -> s {sequenceToken = a} :: PutLogEvents)

-- | The name of the log group.
putLogEvents_logGroupName :: Lens.Lens' PutLogEvents Prelude.Text
putLogEvents_logGroupName = Lens.lens (\PutLogEvents' {logGroupName} -> logGroupName) (\s@PutLogEvents' {} a -> s {logGroupName = a} :: PutLogEvents)

-- | The name of the log stream.
putLogEvents_logStreamName :: Lens.Lens' PutLogEvents Prelude.Text
putLogEvents_logStreamName = Lens.lens (\PutLogEvents' {logStreamName} -> logStreamName) (\s@PutLogEvents' {} a -> s {logStreamName = a} :: PutLogEvents)

-- | The log events.
putLogEvents_logEvents :: Lens.Lens' PutLogEvents (Prelude.NonEmpty InputLogEvent)
putLogEvents_logEvents = Lens.lens (\PutLogEvents' {logEvents} -> logEvents) (\s@PutLogEvents' {} a -> s {logEvents = a} :: PutLogEvents) Prelude.. Lens.coerced

instance Core.AWSRequest PutLogEvents where
  type AWSResponse PutLogEvents = PutLogEventsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutLogEventsResponse'
            Prelude.<$> (x Data..?> "nextSequenceToken")
            Prelude.<*> (x Data..?> "rejectedLogEventsInfo")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutLogEvents where
  hashWithSalt _salt PutLogEvents' {..} =
    _salt
      `Prelude.hashWithSalt` sequenceToken
      `Prelude.hashWithSalt` logGroupName
      `Prelude.hashWithSalt` logStreamName
      `Prelude.hashWithSalt` logEvents

instance Prelude.NFData PutLogEvents where
  rnf PutLogEvents' {..} =
    Prelude.rnf sequenceToken `Prelude.seq`
      Prelude.rnf logGroupName `Prelude.seq`
        Prelude.rnf logStreamName `Prelude.seq`
          Prelude.rnf logEvents

instance Data.ToHeaders PutLogEvents where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("Logs_20140328.PutLogEvents" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutLogEvents where
  toJSON PutLogEvents' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("sequenceToken" Data..=) Prelude.<$> sequenceToken,
            Prelude.Just ("logGroupName" Data..= logGroupName),
            Prelude.Just ("logStreamName" Data..= logStreamName),
            Prelude.Just ("logEvents" Data..= logEvents)
          ]
      )

instance Data.ToPath PutLogEvents where
  toPath = Prelude.const "/"

instance Data.ToQuery PutLogEvents where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutLogEventsResponse' smart constructor.
data PutLogEventsResponse = PutLogEventsResponse'
  { -- | The next sequence token.
    --
    -- This field has been deprecated.
    --
    -- The sequence token is now ignored in @PutLogEvents@ actions.
    -- @PutLogEvents@ actions are always accepted even if the sequence token is
    -- not valid. You can use parallel @PutLogEvents@ actions on the same log
    -- stream and you do not need to wait for the response of a previous
    -- @PutLogEvents@ action to obtain the @nextSequenceToken@ value.
    nextSequenceToken :: Prelude.Maybe Prelude.Text,
    -- | The rejected events.
    rejectedLogEventsInfo :: Prelude.Maybe RejectedLogEventsInfo,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutLogEventsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextSequenceToken', 'putLogEventsResponse_nextSequenceToken' - The next sequence token.
--
-- This field has been deprecated.
--
-- The sequence token is now ignored in @PutLogEvents@ actions.
-- @PutLogEvents@ actions are always accepted even if the sequence token is
-- not valid. You can use parallel @PutLogEvents@ actions on the same log
-- stream and you do not need to wait for the response of a previous
-- @PutLogEvents@ action to obtain the @nextSequenceToken@ value.
--
-- 'rejectedLogEventsInfo', 'putLogEventsResponse_rejectedLogEventsInfo' - The rejected events.
--
-- 'httpStatus', 'putLogEventsResponse_httpStatus' - The response's http status code.
newPutLogEventsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutLogEventsResponse
newPutLogEventsResponse pHttpStatus_ =
  PutLogEventsResponse'
    { nextSequenceToken =
        Prelude.Nothing,
      rejectedLogEventsInfo = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The next sequence token.
--
-- This field has been deprecated.
--
-- The sequence token is now ignored in @PutLogEvents@ actions.
-- @PutLogEvents@ actions are always accepted even if the sequence token is
-- not valid. You can use parallel @PutLogEvents@ actions on the same log
-- stream and you do not need to wait for the response of a previous
-- @PutLogEvents@ action to obtain the @nextSequenceToken@ value.
putLogEventsResponse_nextSequenceToken :: Lens.Lens' PutLogEventsResponse (Prelude.Maybe Prelude.Text)
putLogEventsResponse_nextSequenceToken = Lens.lens (\PutLogEventsResponse' {nextSequenceToken} -> nextSequenceToken) (\s@PutLogEventsResponse' {} a -> s {nextSequenceToken = a} :: PutLogEventsResponse)

-- | The rejected events.
putLogEventsResponse_rejectedLogEventsInfo :: Lens.Lens' PutLogEventsResponse (Prelude.Maybe RejectedLogEventsInfo)
putLogEventsResponse_rejectedLogEventsInfo = Lens.lens (\PutLogEventsResponse' {rejectedLogEventsInfo} -> rejectedLogEventsInfo) (\s@PutLogEventsResponse' {} a -> s {rejectedLogEventsInfo = a} :: PutLogEventsResponse)

-- | The response's http status code.
putLogEventsResponse_httpStatus :: Lens.Lens' PutLogEventsResponse Prelude.Int
putLogEventsResponse_httpStatus = Lens.lens (\PutLogEventsResponse' {httpStatus} -> httpStatus) (\s@PutLogEventsResponse' {} a -> s {httpStatus = a} :: PutLogEventsResponse)

instance Prelude.NFData PutLogEventsResponse where
  rnf PutLogEventsResponse' {..} =
    Prelude.rnf nextSequenceToken `Prelude.seq`
      Prelude.rnf rejectedLogEventsInfo `Prelude.seq`
        Prelude.rnf httpStatus
