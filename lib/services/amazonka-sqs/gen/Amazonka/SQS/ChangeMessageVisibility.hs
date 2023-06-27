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
-- Module      : Amazonka.SQS.ChangeMessageVisibility
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the visibility timeout of a specified message in a queue to a
-- new value. The default visibility timeout for a message is 30 seconds.
-- The minimum is 0 seconds. The maximum is 12 hours. For more information,
-- see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-visibility-timeout.html Visibility Timeout>
-- in the /Amazon SQS Developer Guide/.
--
-- For example, if the default timeout for a queue is 60 seconds, 15
-- seconds have elapsed since you received the message, and you send a
-- ChangeMessageVisibility call with @VisibilityTimeout@ set to 10 seconds,
-- the 10 seconds begin to count from the time that you make the
-- @ChangeMessageVisibility@ call. Thus, any attempt to change the
-- visibility timeout or to delete that message 10 seconds after you
-- initially change the visibility timeout (a total of 25 seconds) might
-- result in an error.
--
-- An Amazon SQS message has three basic states:
--
-- 1.  Sent to a queue by a producer.
--
-- 2.  Received from the queue by a consumer.
--
-- 3.  Deleted from the queue.
--
-- A message is considered to be /stored/ after it is sent to a queue by a
-- producer, but not yet received from the queue by a consumer (that is,
-- between states 1 and 2). There is no limit to the number of stored
-- messages. A message is considered to be /in flight/ after it is received
-- from a queue by a consumer, but not yet deleted from the queue (that is,
-- between states 2 and 3). There is a limit to the number of in flight
-- messages.
--
-- Limits that apply to in flight messages are unrelated to the /unlimited/
-- number of stored messages.
--
-- For most standard queues (depending on queue traffic and message
-- backlog), there can be a maximum of approximately 120,000 in flight
-- messages (received from a queue by a consumer, but not yet deleted from
-- the queue). If you reach this limit, Amazon SQS returns the @OverLimit@
-- error message. To avoid reaching the limit, you should delete messages
-- from the queue after they\'re processed. You can also increase the
-- number of queues you use to process your messages. To request a limit
-- increase,
-- <https://console.aws.amazon.com/support/home#/case/create?issueType=service-limit-increase&limitType=service-code-sqs file a support request>.
--
-- For FIFO queues, there can be a maximum of 20,000 in flight messages
-- (received from a queue by a consumer, but not yet deleted from the
-- queue). If you reach this limit, Amazon SQS returns no error messages.
--
-- If you attempt to set the @VisibilityTimeout@ to a value greater than
-- the maximum time left, Amazon SQS returns an error. Amazon SQS doesn\'t
-- automatically recalculate and increase the timeout to the maximum
-- remaining time.
--
-- Unlike with a queue, when you change the visibility timeout for a
-- specific message the timeout value is applied immediately but isn\'t
-- saved in memory for that message. If you don\'t delete a message after
-- it is received, the visibility timeout for the message reverts to the
-- original timeout value (not to the value you set using the
-- @ChangeMessageVisibility@ action) the next time the message is received.
module Amazonka.SQS.ChangeMessageVisibility
  ( -- * Creating a Request
    ChangeMessageVisibility (..),
    newChangeMessageVisibility,

    -- * Request Lenses
    changeMessageVisibility_queueUrl,
    changeMessageVisibility_receiptHandle,
    changeMessageVisibility_visibilityTimeout,

    -- * Destructuring the Response
    ChangeMessageVisibilityResponse (..),
    newChangeMessageVisibilityResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SQS.Types

-- | /See:/ 'newChangeMessageVisibility' smart constructor.
data ChangeMessageVisibility = ChangeMessageVisibility'
  { -- | The URL of the Amazon SQS queue whose message\'s visibility is changed.
    --
    -- Queue URLs and names are case-sensitive.
    queueUrl :: Prelude.Text,
    -- | The receipt handle associated with the message, whose visibility timeout
    -- is changed. This parameter is returned by the @ @@ReceiveMessage@@ @
    -- action.
    receiptHandle :: Prelude.Text,
    -- | The new value for the message\'s visibility timeout (in seconds). Values
    -- range: @0@ to @43200@. Maximum: 12 hours.
    visibilityTimeout :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChangeMessageVisibility' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queueUrl', 'changeMessageVisibility_queueUrl' - The URL of the Amazon SQS queue whose message\'s visibility is changed.
--
-- Queue URLs and names are case-sensitive.
--
-- 'receiptHandle', 'changeMessageVisibility_receiptHandle' - The receipt handle associated with the message, whose visibility timeout
-- is changed. This parameter is returned by the @ @@ReceiveMessage@@ @
-- action.
--
-- 'visibilityTimeout', 'changeMessageVisibility_visibilityTimeout' - The new value for the message\'s visibility timeout (in seconds). Values
-- range: @0@ to @43200@. Maximum: 12 hours.
newChangeMessageVisibility ::
  -- | 'queueUrl'
  Prelude.Text ->
  -- | 'receiptHandle'
  Prelude.Text ->
  -- | 'visibilityTimeout'
  Prelude.Int ->
  ChangeMessageVisibility
newChangeMessageVisibility
  pQueueUrl_
  pReceiptHandle_
  pVisibilityTimeout_ =
    ChangeMessageVisibility'
      { queueUrl = pQueueUrl_,
        receiptHandle = pReceiptHandle_,
        visibilityTimeout = pVisibilityTimeout_
      }

-- | The URL of the Amazon SQS queue whose message\'s visibility is changed.
--
-- Queue URLs and names are case-sensitive.
changeMessageVisibility_queueUrl :: Lens.Lens' ChangeMessageVisibility Prelude.Text
changeMessageVisibility_queueUrl = Lens.lens (\ChangeMessageVisibility' {queueUrl} -> queueUrl) (\s@ChangeMessageVisibility' {} a -> s {queueUrl = a} :: ChangeMessageVisibility)

-- | The receipt handle associated with the message, whose visibility timeout
-- is changed. This parameter is returned by the @ @@ReceiveMessage@@ @
-- action.
changeMessageVisibility_receiptHandle :: Lens.Lens' ChangeMessageVisibility Prelude.Text
changeMessageVisibility_receiptHandle = Lens.lens (\ChangeMessageVisibility' {receiptHandle} -> receiptHandle) (\s@ChangeMessageVisibility' {} a -> s {receiptHandle = a} :: ChangeMessageVisibility)

-- | The new value for the message\'s visibility timeout (in seconds). Values
-- range: @0@ to @43200@. Maximum: 12 hours.
changeMessageVisibility_visibilityTimeout :: Lens.Lens' ChangeMessageVisibility Prelude.Int
changeMessageVisibility_visibilityTimeout = Lens.lens (\ChangeMessageVisibility' {visibilityTimeout} -> visibilityTimeout) (\s@ChangeMessageVisibility' {} a -> s {visibilityTimeout = a} :: ChangeMessageVisibility)

instance Core.AWSRequest ChangeMessageVisibility where
  type
    AWSResponse ChangeMessageVisibility =
      ChangeMessageVisibilityResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      ChangeMessageVisibilityResponse'

instance Prelude.Hashable ChangeMessageVisibility where
  hashWithSalt _salt ChangeMessageVisibility' {..} =
    _salt
      `Prelude.hashWithSalt` queueUrl
      `Prelude.hashWithSalt` receiptHandle
      `Prelude.hashWithSalt` visibilityTimeout

instance Prelude.NFData ChangeMessageVisibility where
  rnf ChangeMessageVisibility' {..} =
    Prelude.rnf queueUrl
      `Prelude.seq` Prelude.rnf receiptHandle
      `Prelude.seq` Prelude.rnf visibilityTimeout

instance Data.ToHeaders ChangeMessageVisibility where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ChangeMessageVisibility where
  toPath = Prelude.const "/"

instance Data.ToQuery ChangeMessageVisibility where
  toQuery ChangeMessageVisibility' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ChangeMessageVisibility" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-11-05" :: Prelude.ByteString),
        "QueueUrl" Data.=: queueUrl,
        "ReceiptHandle" Data.=: receiptHandle,
        "VisibilityTimeout" Data.=: visibilityTimeout
      ]

-- | /See:/ 'newChangeMessageVisibilityResponse' smart constructor.
data ChangeMessageVisibilityResponse = ChangeMessageVisibilityResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChangeMessageVisibilityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newChangeMessageVisibilityResponse ::
  ChangeMessageVisibilityResponse
newChangeMessageVisibilityResponse =
  ChangeMessageVisibilityResponse'

instance
  Prelude.NFData
    ChangeMessageVisibilityResponse
  where
  rnf _ = ()
