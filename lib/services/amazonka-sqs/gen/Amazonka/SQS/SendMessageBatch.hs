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
-- Module      : Amazonka.SQS.SendMessageBatch
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delivers up to ten messages to the specified queue. This is a batch
-- version of @ @@SendMessage@@.@ For a FIFO queue, multiple messages
-- within a single batch are enqueued in the order they are sent.
--
-- The result of sending each message is reported individually in the
-- response. Because the batch request can result in a combination of
-- successful and unsuccessful actions, you should check for batch errors
-- even when the call returns an HTTP status code of @200@.
--
-- The maximum allowed individual message size and the maximum total
-- payload size (the sum of the individual lengths of all of the batched
-- messages) are both 256 KB (262,144 bytes).
--
-- A message can include only XML, JSON, and unformatted text. The
-- following Unicode characters are allowed:
--
-- @#x9@ | @#xA@ | @#xD@ | @#x20@ to @#xD7FF@ | @#xE000@ to @#xFFFD@ |
-- @#x10000@ to @#x10FFFF@
--
-- Any characters not included in this list will be rejected. For more
-- information, see the
-- <http://www.w3.org/TR/REC-xml/#charsets W3C specification for characters>.
--
-- If you don\'t specify the @DelaySeconds@ parameter for an entry, Amazon
-- SQS uses the default value for the queue.
--
-- Some actions take lists of parameters. These lists are specified using
-- the @param.n@ notation. Values of @n@ are integers starting from 1. For
-- example, a parameter list with two elements looks like this:
--
-- @&AttributeName.1=first@
--
-- @&AttributeName.2=second@
module Amazonka.SQS.SendMessageBatch
  ( -- * Creating a Request
    SendMessageBatch (..),
    newSendMessageBatch,

    -- * Request Lenses
    sendMessageBatch_queueUrl,
    sendMessageBatch_entries,

    -- * Destructuring the Response
    SendMessageBatchResponse (..),
    newSendMessageBatchResponse,

    -- * Response Lenses
    sendMessageBatchResponse_httpStatus,
    sendMessageBatchResponse_successful,
    sendMessageBatchResponse_failed,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SQS.Types

-- |
--
-- /See:/ 'newSendMessageBatch' smart constructor.
data SendMessageBatch = SendMessageBatch'
  { -- | The URL of the Amazon SQS queue to which batched messages are sent.
    --
    -- Queue URLs and names are case-sensitive.
    queueUrl :: Prelude.Text,
    -- | A list of @ @@SendMessageBatchRequestEntry@@ @ items.
    entries :: [SendMessageBatchRequestEntry]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendMessageBatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queueUrl', 'sendMessageBatch_queueUrl' - The URL of the Amazon SQS queue to which batched messages are sent.
--
-- Queue URLs and names are case-sensitive.
--
-- 'entries', 'sendMessageBatch_entries' - A list of @ @@SendMessageBatchRequestEntry@@ @ items.
newSendMessageBatch ::
  -- | 'queueUrl'
  Prelude.Text ->
  SendMessageBatch
newSendMessageBatch pQueueUrl_ =
  SendMessageBatch'
    { queueUrl = pQueueUrl_,
      entries = Prelude.mempty
    }

-- | The URL of the Amazon SQS queue to which batched messages are sent.
--
-- Queue URLs and names are case-sensitive.
sendMessageBatch_queueUrl :: Lens.Lens' SendMessageBatch Prelude.Text
sendMessageBatch_queueUrl = Lens.lens (\SendMessageBatch' {queueUrl} -> queueUrl) (\s@SendMessageBatch' {} a -> s {queueUrl = a} :: SendMessageBatch)

-- | A list of @ @@SendMessageBatchRequestEntry@@ @ items.
sendMessageBatch_entries :: Lens.Lens' SendMessageBatch [SendMessageBatchRequestEntry]
sendMessageBatch_entries = Lens.lens (\SendMessageBatch' {entries} -> entries) (\s@SendMessageBatch' {} a -> s {entries = a} :: SendMessageBatch) Prelude.. Lens.coerced

instance Core.AWSRequest SendMessageBatch where
  type
    AWSResponse SendMessageBatch =
      SendMessageBatchResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "SendMessageBatchResult"
      ( \s h x ->
          SendMessageBatchResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.parseXMLList "SendMessageBatchResultEntry" x)
            Prelude.<*> (Data.parseXMLList "BatchResultErrorEntry" x)
      )

instance Prelude.Hashable SendMessageBatch where
  hashWithSalt _salt SendMessageBatch' {..} =
    _salt
      `Prelude.hashWithSalt` queueUrl
      `Prelude.hashWithSalt` entries

instance Prelude.NFData SendMessageBatch where
  rnf SendMessageBatch' {..} =
    Prelude.rnf queueUrl
      `Prelude.seq` Prelude.rnf entries

instance Data.ToHeaders SendMessageBatch where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath SendMessageBatch where
  toPath = Prelude.const "/"

instance Data.ToQuery SendMessageBatch where
  toQuery SendMessageBatch' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("SendMessageBatch" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-11-05" :: Prelude.ByteString),
        "QueueUrl" Data.=: queueUrl,
        Data.toQueryList
          "SendMessageBatchRequestEntry"
          entries
      ]

-- | For each message in the batch, the response contains a
-- @ @@SendMessageBatchResultEntry@@ @ tag if the message succeeds or a
-- @ @@BatchResultErrorEntry@@ @ tag if the message fails.
--
-- /See:/ 'newSendMessageBatchResponse' smart constructor.
data SendMessageBatchResponse = SendMessageBatchResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of @ @@SendMessageBatchResultEntry@@ @ items.
    successful :: [SendMessageBatchResultEntry],
    -- | A list of @ @@BatchResultErrorEntry@@ @ items with error details about
    -- each message that can\'t be enqueued.
    failed :: [BatchResultErrorEntry]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendMessageBatchResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'sendMessageBatchResponse_httpStatus' - The response's http status code.
--
-- 'successful', 'sendMessageBatchResponse_successful' - A list of @ @@SendMessageBatchResultEntry@@ @ items.
--
-- 'failed', 'sendMessageBatchResponse_failed' - A list of @ @@BatchResultErrorEntry@@ @ items with error details about
-- each message that can\'t be enqueued.
newSendMessageBatchResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SendMessageBatchResponse
newSendMessageBatchResponse pHttpStatus_ =
  SendMessageBatchResponse'
    { httpStatus =
        pHttpStatus_,
      successful = Prelude.mempty,
      failed = Prelude.mempty
    }

-- | The response's http status code.
sendMessageBatchResponse_httpStatus :: Lens.Lens' SendMessageBatchResponse Prelude.Int
sendMessageBatchResponse_httpStatus = Lens.lens (\SendMessageBatchResponse' {httpStatus} -> httpStatus) (\s@SendMessageBatchResponse' {} a -> s {httpStatus = a} :: SendMessageBatchResponse)

-- | A list of @ @@SendMessageBatchResultEntry@@ @ items.
sendMessageBatchResponse_successful :: Lens.Lens' SendMessageBatchResponse [SendMessageBatchResultEntry]
sendMessageBatchResponse_successful = Lens.lens (\SendMessageBatchResponse' {successful} -> successful) (\s@SendMessageBatchResponse' {} a -> s {successful = a} :: SendMessageBatchResponse) Prelude.. Lens.coerced

-- | A list of @ @@BatchResultErrorEntry@@ @ items with error details about
-- each message that can\'t be enqueued.
sendMessageBatchResponse_failed :: Lens.Lens' SendMessageBatchResponse [BatchResultErrorEntry]
sendMessageBatchResponse_failed = Lens.lens (\SendMessageBatchResponse' {failed} -> failed) (\s@SendMessageBatchResponse' {} a -> s {failed = a} :: SendMessageBatchResponse) Prelude.. Lens.coerced

instance Prelude.NFData SendMessageBatchResponse where
  rnf SendMessageBatchResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf successful
      `Prelude.seq` Prelude.rnf failed
