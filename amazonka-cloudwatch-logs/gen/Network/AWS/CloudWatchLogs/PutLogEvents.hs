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
-- Module      : Network.AWS.CloudWatchLogs.PutLogEvents
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads a batch of log events to the specified log stream.
--
-- You must include the sequence token obtained from the response of the
-- previous call. An upload in a newly created log stream does not require
-- a sequence token. You can also get the sequence token in the
-- @expectedSequenceToken@ field from @InvalidSequenceTokenException@. If
-- you call @PutLogEvents@ twice within a narrow time period using the same
-- value for @sequenceToken@, both calls might be successful or one might
-- be rejected.
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
-- -   None of the log events in the batch can be older than 14 days or
--     older than the retention period of the log group.
--
-- -   The log events in the batch must be in chronological order by their
--     timestamp. The timestamp is the time the event occurred, expressed
--     as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. (In
--     AWS Tools for PowerShell and the AWS SDK for .NET, the timestamp is
--     specified in .NET format: yyyy-mm-ddThh:mm:ss. For example,
--     2017-09-15T13:45:30.)
--
-- -   A batch of log events in a single request cannot span more than 24
--     hours. Otherwise, the operation fails.
--
-- -   The maximum number of log events in a batch is 10,000.
--
-- -   There is a quota of 5 requests per second per log stream. Additional
--     requests are throttled. This quota can\'t be changed.
--
-- If a call to @PutLogEvents@ returns \"UnrecognizedClientException\" the
-- most likely cause is an invalid AWS access key ID or secret key.
module Network.AWS.CloudWatchLogs.PutLogEvents
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

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutLogEvents' smart constructor.
data PutLogEvents = PutLogEvents'
  { -- | The sequence token obtained from the response of the previous
    -- @PutLogEvents@ call. An upload in a newly created log stream does not
    -- require a sequence token. You can also get the sequence token using
    -- <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeLogStreams.html DescribeLogStreams>.
    -- If you call @PutLogEvents@ twice within a narrow time period using the
    -- same value for @sequenceToken@, both calls might be successful or one
    -- might be rejected.
    sequenceToken :: Core.Maybe Core.Text,
    -- | The name of the log group.
    logGroupName :: Core.Text,
    -- | The name of the log stream.
    logStreamName :: Core.Text,
    -- | The log events.
    logEvents :: Core.NonEmpty InputLogEvent
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutLogEvents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sequenceToken', 'putLogEvents_sequenceToken' - The sequence token obtained from the response of the previous
-- @PutLogEvents@ call. An upload in a newly created log stream does not
-- require a sequence token. You can also get the sequence token using
-- <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeLogStreams.html DescribeLogStreams>.
-- If you call @PutLogEvents@ twice within a narrow time period using the
-- same value for @sequenceToken@, both calls might be successful or one
-- might be rejected.
--
-- 'logGroupName', 'putLogEvents_logGroupName' - The name of the log group.
--
-- 'logStreamName', 'putLogEvents_logStreamName' - The name of the log stream.
--
-- 'logEvents', 'putLogEvents_logEvents' - The log events.
newPutLogEvents ::
  -- | 'logGroupName'
  Core.Text ->
  -- | 'logStreamName'
  Core.Text ->
  -- | 'logEvents'
  Core.NonEmpty InputLogEvent ->
  PutLogEvents
newPutLogEvents
  pLogGroupName_
  pLogStreamName_
  pLogEvents_ =
    PutLogEvents'
      { sequenceToken = Core.Nothing,
        logGroupName = pLogGroupName_,
        logStreamName = pLogStreamName_,
        logEvents = Lens._Coerce Lens.# pLogEvents_
      }

-- | The sequence token obtained from the response of the previous
-- @PutLogEvents@ call. An upload in a newly created log stream does not
-- require a sequence token. You can also get the sequence token using
-- <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeLogStreams.html DescribeLogStreams>.
-- If you call @PutLogEvents@ twice within a narrow time period using the
-- same value for @sequenceToken@, both calls might be successful or one
-- might be rejected.
putLogEvents_sequenceToken :: Lens.Lens' PutLogEvents (Core.Maybe Core.Text)
putLogEvents_sequenceToken = Lens.lens (\PutLogEvents' {sequenceToken} -> sequenceToken) (\s@PutLogEvents' {} a -> s {sequenceToken = a} :: PutLogEvents)

-- | The name of the log group.
putLogEvents_logGroupName :: Lens.Lens' PutLogEvents Core.Text
putLogEvents_logGroupName = Lens.lens (\PutLogEvents' {logGroupName} -> logGroupName) (\s@PutLogEvents' {} a -> s {logGroupName = a} :: PutLogEvents)

-- | The name of the log stream.
putLogEvents_logStreamName :: Lens.Lens' PutLogEvents Core.Text
putLogEvents_logStreamName = Lens.lens (\PutLogEvents' {logStreamName} -> logStreamName) (\s@PutLogEvents' {} a -> s {logStreamName = a} :: PutLogEvents)

-- | The log events.
putLogEvents_logEvents :: Lens.Lens' PutLogEvents (Core.NonEmpty InputLogEvent)
putLogEvents_logEvents = Lens.lens (\PutLogEvents' {logEvents} -> logEvents) (\s@PutLogEvents' {} a -> s {logEvents = a} :: PutLogEvents) Core.. Lens._Coerce

instance Core.AWSRequest PutLogEvents where
  type AWSResponse PutLogEvents = PutLogEventsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutLogEventsResponse'
            Core.<$> (x Core..?> "nextSequenceToken")
            Core.<*> (x Core..?> "rejectedLogEventsInfo")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PutLogEvents

instance Core.NFData PutLogEvents

instance Core.ToHeaders PutLogEvents where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("Logs_20140328.PutLogEvents" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutLogEvents where
  toJSON PutLogEvents' {..} =
    Core.object
      ( Core.catMaybes
          [ ("sequenceToken" Core..=) Core.<$> sequenceToken,
            Core.Just ("logGroupName" Core..= logGroupName),
            Core.Just ("logStreamName" Core..= logStreamName),
            Core.Just ("logEvents" Core..= logEvents)
          ]
      )

instance Core.ToPath PutLogEvents where
  toPath = Core.const "/"

instance Core.ToQuery PutLogEvents where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutLogEventsResponse' smart constructor.
data PutLogEventsResponse = PutLogEventsResponse'
  { -- | The next sequence token.
    nextSequenceToken :: Core.Maybe Core.Text,
    -- | The rejected events.
    rejectedLogEventsInfo :: Core.Maybe RejectedLogEventsInfo,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'rejectedLogEventsInfo', 'putLogEventsResponse_rejectedLogEventsInfo' - The rejected events.
--
-- 'httpStatus', 'putLogEventsResponse_httpStatus' - The response's http status code.
newPutLogEventsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PutLogEventsResponse
newPutLogEventsResponse pHttpStatus_ =
  PutLogEventsResponse'
    { nextSequenceToken =
        Core.Nothing,
      rejectedLogEventsInfo = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The next sequence token.
putLogEventsResponse_nextSequenceToken :: Lens.Lens' PutLogEventsResponse (Core.Maybe Core.Text)
putLogEventsResponse_nextSequenceToken = Lens.lens (\PutLogEventsResponse' {nextSequenceToken} -> nextSequenceToken) (\s@PutLogEventsResponse' {} a -> s {nextSequenceToken = a} :: PutLogEventsResponse)

-- | The rejected events.
putLogEventsResponse_rejectedLogEventsInfo :: Lens.Lens' PutLogEventsResponse (Core.Maybe RejectedLogEventsInfo)
putLogEventsResponse_rejectedLogEventsInfo = Lens.lens (\PutLogEventsResponse' {rejectedLogEventsInfo} -> rejectedLogEventsInfo) (\s@PutLogEventsResponse' {} a -> s {rejectedLogEventsInfo = a} :: PutLogEventsResponse)

-- | The response's http status code.
putLogEventsResponse_httpStatus :: Lens.Lens' PutLogEventsResponse Core.Int
putLogEventsResponse_httpStatus = Lens.lens (\PutLogEventsResponse' {httpStatus} -> httpStatus) (\s@PutLogEventsResponse' {} a -> s {httpStatus = a} :: PutLogEventsResponse)

instance Core.NFData PutLogEventsResponse
