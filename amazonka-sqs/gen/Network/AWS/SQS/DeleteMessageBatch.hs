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
-- Module      : Network.AWS.SQS.DeleteMessageBatch
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes up to ten messages from the specified queue. This is a batch
-- version of @ DeleteMessage.@ The result of the action on each message is
-- reported individually in the response.
--
-- Because the batch request can result in a combination of successful and
-- unsuccessful actions, you should check for batch errors even when the
-- call returns an HTTP status code of @200@.
--
-- Some actions take lists of parameters. These lists are specified using
-- the @param.n@ notation. Values of @n@ are integers starting from 1. For
-- example, a parameter list with two elements looks like this:
--
-- @&AttributeName.1=first@
--
-- @&AttributeName.2=second@
module Network.AWS.SQS.DeleteMessageBatch
  ( -- * Creating a Request
    DeleteMessageBatch (..),
    newDeleteMessageBatch,

    -- * Request Lenses
    deleteMessageBatch_queueUrl,
    deleteMessageBatch_entries,

    -- * Destructuring the Response
    DeleteMessageBatchResponse (..),
    newDeleteMessageBatchResponse,

    -- * Response Lenses
    deleteMessageBatchResponse_httpStatus,
    deleteMessageBatchResponse_successful,
    deleteMessageBatchResponse_failed,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SQS.Types

-- |
--
-- /See:/ 'newDeleteMessageBatch' smart constructor.
data DeleteMessageBatch = DeleteMessageBatch'
  { -- | The URL of the Amazon SQS queue from which messages are deleted.
    --
    -- Queue URLs and names are case-sensitive.
    queueUrl :: Prelude.Text,
    -- | A list of receipt handles for the messages to be deleted.
    entries :: [DeleteMessageBatchRequestEntry]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMessageBatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queueUrl', 'deleteMessageBatch_queueUrl' - The URL of the Amazon SQS queue from which messages are deleted.
--
-- Queue URLs and names are case-sensitive.
--
-- 'entries', 'deleteMessageBatch_entries' - A list of receipt handles for the messages to be deleted.
newDeleteMessageBatch ::
  -- | 'queueUrl'
  Prelude.Text ->
  DeleteMessageBatch
newDeleteMessageBatch pQueueUrl_ =
  DeleteMessageBatch'
    { queueUrl = pQueueUrl_,
      entries = Prelude.mempty
    }

-- | The URL of the Amazon SQS queue from which messages are deleted.
--
-- Queue URLs and names are case-sensitive.
deleteMessageBatch_queueUrl :: Lens.Lens' DeleteMessageBatch Prelude.Text
deleteMessageBatch_queueUrl = Lens.lens (\DeleteMessageBatch' {queueUrl} -> queueUrl) (\s@DeleteMessageBatch' {} a -> s {queueUrl = a} :: DeleteMessageBatch)

-- | A list of receipt handles for the messages to be deleted.
deleteMessageBatch_entries :: Lens.Lens' DeleteMessageBatch [DeleteMessageBatchRequestEntry]
deleteMessageBatch_entries = Lens.lens (\DeleteMessageBatch' {entries} -> entries) (\s@DeleteMessageBatch' {} a -> s {entries = a} :: DeleteMessageBatch) Prelude.. Lens._Coerce

instance Core.AWSRequest DeleteMessageBatch where
  type
    AWSResponse DeleteMessageBatch =
      DeleteMessageBatchResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DeleteMessageBatchResult"
      ( \s h x ->
          DeleteMessageBatchResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.parseXMLList "DeleteMessageBatchResultEntry" x)
            Prelude.<*> (Core.parseXMLList "BatchResultErrorEntry" x)
      )

instance Prelude.Hashable DeleteMessageBatch

instance Prelude.NFData DeleteMessageBatch

instance Core.ToHeaders DeleteMessageBatch where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteMessageBatch where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteMessageBatch where
  toQuery DeleteMessageBatch' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DeleteMessageBatch" :: Prelude.ByteString),
        "Version"
          Core.=: ("2012-11-05" :: Prelude.ByteString),
        "QueueUrl" Core.=: queueUrl,
        Core.toQueryList
          "DeleteMessageBatchRequestEntry"
          entries
      ]

-- | For each message in the batch, the response contains a
-- @ DeleteMessageBatchResultEntry @ tag if the message is deleted or a
-- @ BatchResultErrorEntry @ tag if the message can\'t be deleted.
--
-- /See:/ 'newDeleteMessageBatchResponse' smart constructor.
data DeleteMessageBatchResponse = DeleteMessageBatchResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of @ DeleteMessageBatchResultEntry @ items.
    successful :: [DeleteMessageBatchResultEntry],
    -- | A list of @ BatchResultErrorEntry @ items.
    failed :: [BatchResultErrorEntry]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMessageBatchResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteMessageBatchResponse_httpStatus' - The response's http status code.
--
-- 'successful', 'deleteMessageBatchResponse_successful' - A list of @ DeleteMessageBatchResultEntry @ items.
--
-- 'failed', 'deleteMessageBatchResponse_failed' - A list of @ BatchResultErrorEntry @ items.
newDeleteMessageBatchResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteMessageBatchResponse
newDeleteMessageBatchResponse pHttpStatus_ =
  DeleteMessageBatchResponse'
    { httpStatus =
        pHttpStatus_,
      successful = Prelude.mempty,
      failed = Prelude.mempty
    }

-- | The response's http status code.
deleteMessageBatchResponse_httpStatus :: Lens.Lens' DeleteMessageBatchResponse Prelude.Int
deleteMessageBatchResponse_httpStatus = Lens.lens (\DeleteMessageBatchResponse' {httpStatus} -> httpStatus) (\s@DeleteMessageBatchResponse' {} a -> s {httpStatus = a} :: DeleteMessageBatchResponse)

-- | A list of @ DeleteMessageBatchResultEntry @ items.
deleteMessageBatchResponse_successful :: Lens.Lens' DeleteMessageBatchResponse [DeleteMessageBatchResultEntry]
deleteMessageBatchResponse_successful = Lens.lens (\DeleteMessageBatchResponse' {successful} -> successful) (\s@DeleteMessageBatchResponse' {} a -> s {successful = a} :: DeleteMessageBatchResponse) Prelude.. Lens._Coerce

-- | A list of @ BatchResultErrorEntry @ items.
deleteMessageBatchResponse_failed :: Lens.Lens' DeleteMessageBatchResponse [BatchResultErrorEntry]
deleteMessageBatchResponse_failed = Lens.lens (\DeleteMessageBatchResponse' {failed} -> failed) (\s@DeleteMessageBatchResponse' {} a -> s {failed = a} :: DeleteMessageBatchResponse) Prelude.. Lens._Coerce

instance Prelude.NFData DeleteMessageBatchResponse
