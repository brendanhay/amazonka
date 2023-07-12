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
-- Module      : Amazonka.SQS.DeleteMessageBatch
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes up to ten messages from the specified queue. This is a batch
-- version of @ @@DeleteMessage@@.@ The result of the action on each
-- message is reported individually in the response.
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
module Amazonka.SQS.DeleteMessageBatch
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SQS.Types

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
deleteMessageBatch_entries = Lens.lens (\DeleteMessageBatch' {entries} -> entries) (\s@DeleteMessageBatch' {} a -> s {entries = a} :: DeleteMessageBatch) Prelude.. Lens.coerced

instance Core.AWSRequest DeleteMessageBatch where
  type
    AWSResponse DeleteMessageBatch =
      DeleteMessageBatchResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DeleteMessageBatchResult"
      ( \s h x ->
          DeleteMessageBatchResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.parseXMLList "DeleteMessageBatchResultEntry" x)
            Prelude.<*> (Data.parseXMLList "BatchResultErrorEntry" x)
      )

instance Prelude.Hashable DeleteMessageBatch where
  hashWithSalt _salt DeleteMessageBatch' {..} =
    _salt
      `Prelude.hashWithSalt` queueUrl
      `Prelude.hashWithSalt` entries

instance Prelude.NFData DeleteMessageBatch where
  rnf DeleteMessageBatch' {..} =
    Prelude.rnf queueUrl
      `Prelude.seq` Prelude.rnf entries

instance Data.ToHeaders DeleteMessageBatch where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteMessageBatch where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteMessageBatch where
  toQuery DeleteMessageBatch' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteMessageBatch" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-11-05" :: Prelude.ByteString),
        "QueueUrl" Data.=: queueUrl,
        Data.toQueryList
          "DeleteMessageBatchRequestEntry"
          entries
      ]

-- | For each message in the batch, the response contains a
-- @ @@DeleteMessageBatchResultEntry@@ @ tag if the message is deleted or a
-- @ @@BatchResultErrorEntry@@ @ tag if the message can\'t be deleted.
--
-- /See:/ 'newDeleteMessageBatchResponse' smart constructor.
data DeleteMessageBatchResponse = DeleteMessageBatchResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of @ @@DeleteMessageBatchResultEntry@@ @ items.
    successful :: [DeleteMessageBatchResultEntry],
    -- | A list of @ @@BatchResultErrorEntry@@ @ items.
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
-- 'successful', 'deleteMessageBatchResponse_successful' - A list of @ @@DeleteMessageBatchResultEntry@@ @ items.
--
-- 'failed', 'deleteMessageBatchResponse_failed' - A list of @ @@BatchResultErrorEntry@@ @ items.
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

-- | A list of @ @@DeleteMessageBatchResultEntry@@ @ items.
deleteMessageBatchResponse_successful :: Lens.Lens' DeleteMessageBatchResponse [DeleteMessageBatchResultEntry]
deleteMessageBatchResponse_successful = Lens.lens (\DeleteMessageBatchResponse' {successful} -> successful) (\s@DeleteMessageBatchResponse' {} a -> s {successful = a} :: DeleteMessageBatchResponse) Prelude.. Lens.coerced

-- | A list of @ @@BatchResultErrorEntry@@ @ items.
deleteMessageBatchResponse_failed :: Lens.Lens' DeleteMessageBatchResponse [BatchResultErrorEntry]
deleteMessageBatchResponse_failed = Lens.lens (\DeleteMessageBatchResponse' {failed} -> failed) (\s@DeleteMessageBatchResponse' {} a -> s {failed = a} :: DeleteMessageBatchResponse) Prelude.. Lens.coerced

instance Prelude.NFData DeleteMessageBatchResponse where
  rnf DeleteMessageBatchResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf successful
      `Prelude.seq` Prelude.rnf failed
