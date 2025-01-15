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
-- Module      : Amazonka.SNS.PublishBatch
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Publishes up to ten messages to the specified topic. This is a batch
-- version of @Publish@. For FIFO topics, multiple messages within a single
-- batch are published in the order they are sent, and messages are
-- deduplicated within the batch and across batches for 5 minutes.
--
-- The result of publishing each message is reported individually in the
-- response. Because the batch request can result in a combination of
-- successful and unsuccessful actions, you should check for batch errors
-- even when the call returns an HTTP status code of @200@.
--
-- The maximum allowed individual message size and the maximum total
-- payload size (the sum of the individual lengths of all of the batched
-- messages) are both 256 KB (262,144 bytes).
--
-- Some actions take lists of parameters. These lists are specified using
-- the @param.n@ notation. Values of @n@ are integers starting from 1. For
-- example, a parameter list with two elements looks like this:
--
-- &AttributeName.1=first
--
-- &AttributeName.2=second
--
-- If you send a batch message to a topic, Amazon SNS publishes the batch
-- message to each endpoint that is subscribed to the topic. The format of
-- the batch message depends on the notification protocol for each
-- subscribed endpoint.
--
-- When a @messageId@ is returned, the batch message is saved and Amazon
-- SNS immediately delivers the message to subscribers.
module Amazonka.SNS.PublishBatch
  ( -- * Creating a Request
    PublishBatch (..),
    newPublishBatch,

    -- * Request Lenses
    publishBatch_topicArn,
    publishBatch_publishBatchRequestEntries,

    -- * Destructuring the Response
    PublishBatchResponse (..),
    newPublishBatchResponse,

    -- * Response Lenses
    publishBatchResponse_failed,
    publishBatchResponse_successful,
    publishBatchResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SNS.Types

-- | /See:/ 'newPublishBatch' smart constructor.
data PublishBatch = PublishBatch'
  { -- | The Amazon resource name (ARN) of the topic you want to batch publish
    -- to.
    topicArn :: Prelude.Text,
    -- | A list of @PublishBatch@ request entries to be sent to the SNS topic.
    publishBatchRequestEntries :: [PublishBatchRequestEntry]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PublishBatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'topicArn', 'publishBatch_topicArn' - The Amazon resource name (ARN) of the topic you want to batch publish
-- to.
--
-- 'publishBatchRequestEntries', 'publishBatch_publishBatchRequestEntries' - A list of @PublishBatch@ request entries to be sent to the SNS topic.
newPublishBatch ::
  -- | 'topicArn'
  Prelude.Text ->
  PublishBatch
newPublishBatch pTopicArn_ =
  PublishBatch'
    { topicArn = pTopicArn_,
      publishBatchRequestEntries = Prelude.mempty
    }

-- | The Amazon resource name (ARN) of the topic you want to batch publish
-- to.
publishBatch_topicArn :: Lens.Lens' PublishBatch Prelude.Text
publishBatch_topicArn = Lens.lens (\PublishBatch' {topicArn} -> topicArn) (\s@PublishBatch' {} a -> s {topicArn = a} :: PublishBatch)

-- | A list of @PublishBatch@ request entries to be sent to the SNS topic.
publishBatch_publishBatchRequestEntries :: Lens.Lens' PublishBatch [PublishBatchRequestEntry]
publishBatch_publishBatchRequestEntries = Lens.lens (\PublishBatch' {publishBatchRequestEntries} -> publishBatchRequestEntries) (\s@PublishBatch' {} a -> s {publishBatchRequestEntries = a} :: PublishBatch) Prelude.. Lens.coerced

instance Core.AWSRequest PublishBatch where
  type AWSResponse PublishBatch = PublishBatchResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "PublishBatchResult"
      ( \s h x ->
          PublishBatchResponse'
            Prelude.<$> ( x Data..@? "Failed" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> ( x Data..@? "Successful" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PublishBatch where
  hashWithSalt _salt PublishBatch' {..} =
    _salt
      `Prelude.hashWithSalt` topicArn
      `Prelude.hashWithSalt` publishBatchRequestEntries

instance Prelude.NFData PublishBatch where
  rnf PublishBatch' {..} =
    Prelude.rnf topicArn `Prelude.seq`
      Prelude.rnf publishBatchRequestEntries

instance Data.ToHeaders PublishBatch where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath PublishBatch where
  toPath = Prelude.const "/"

instance Data.ToQuery PublishBatch where
  toQuery PublishBatch' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("PublishBatch" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-03-31" :: Prelude.ByteString),
        "TopicArn" Data.=: topicArn,
        "PublishBatchRequestEntries"
          Data.=: Data.toQueryList "member" publishBatchRequestEntries
      ]

-- | /See:/ 'newPublishBatchResponse' smart constructor.
data PublishBatchResponse = PublishBatchResponse'
  { -- | A list of failed @PublishBatch@ responses.
    failed :: Prelude.Maybe [BatchResultErrorEntry],
    -- | A list of successful @PublishBatch@ responses.
    successful :: Prelude.Maybe [PublishBatchResultEntry],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PublishBatchResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failed', 'publishBatchResponse_failed' - A list of failed @PublishBatch@ responses.
--
-- 'successful', 'publishBatchResponse_successful' - A list of successful @PublishBatch@ responses.
--
-- 'httpStatus', 'publishBatchResponse_httpStatus' - The response's http status code.
newPublishBatchResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PublishBatchResponse
newPublishBatchResponse pHttpStatus_ =
  PublishBatchResponse'
    { failed = Prelude.Nothing,
      successful = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of failed @PublishBatch@ responses.
publishBatchResponse_failed :: Lens.Lens' PublishBatchResponse (Prelude.Maybe [BatchResultErrorEntry])
publishBatchResponse_failed = Lens.lens (\PublishBatchResponse' {failed} -> failed) (\s@PublishBatchResponse' {} a -> s {failed = a} :: PublishBatchResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of successful @PublishBatch@ responses.
publishBatchResponse_successful :: Lens.Lens' PublishBatchResponse (Prelude.Maybe [PublishBatchResultEntry])
publishBatchResponse_successful = Lens.lens (\PublishBatchResponse' {successful} -> successful) (\s@PublishBatchResponse' {} a -> s {successful = a} :: PublishBatchResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
publishBatchResponse_httpStatus :: Lens.Lens' PublishBatchResponse Prelude.Int
publishBatchResponse_httpStatus = Lens.lens (\PublishBatchResponse' {httpStatus} -> httpStatus) (\s@PublishBatchResponse' {} a -> s {httpStatus = a} :: PublishBatchResponse)

instance Prelude.NFData PublishBatchResponse where
  rnf PublishBatchResponse' {..} =
    Prelude.rnf failed `Prelude.seq`
      Prelude.rnf successful `Prelude.seq`
        Prelude.rnf httpStatus
