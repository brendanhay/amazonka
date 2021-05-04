{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SQS.DeleteMessage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified message from the specified queue. To select the
-- message to delete, use the @ReceiptHandle@ of the message (/not/ the
-- @MessageId@ which you receive when you send the message). Amazon SQS can
-- delete a message from a queue even if a visibility timeout setting
-- causes the message to be locked by another consumer. Amazon SQS
-- automatically deletes messages left in a queue longer than the retention
-- period configured for the queue.
--
-- The @ReceiptHandle@ is associated with a /specific instance/ of
-- receiving a message. If you receive a message more than once, the
-- @ReceiptHandle@ is different each time you receive a message. When you
-- use the @DeleteMessage@ action, you must provide the most recently
-- received @ReceiptHandle@ for the message (otherwise, the request
-- succeeds, but the message might not be deleted).
--
-- For standard queues, it is possible to receive a message even after you
-- delete it. This might happen on rare occasions if one of the servers
-- which stores a copy of the message is unavailable when you send the
-- request to delete the message. The copy remains on the server and might
-- be returned to you during a subsequent receive request. You should
-- ensure that your application is idempotent, so that receiving a message
-- more than once does not cause issues.
module Network.AWS.SQS.DeleteMessage
  ( -- * Creating a Request
    DeleteMessage (..),
    newDeleteMessage,

    -- * Request Lenses
    deleteMessage_queueUrl,
    deleteMessage_receiptHandle,

    -- * Destructuring the Response
    DeleteMessageResponse (..),
    newDeleteMessageResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SQS.Types

-- |
--
-- /See:/ 'newDeleteMessage' smart constructor.
data DeleteMessage = DeleteMessage'
  { -- | The URL of the Amazon SQS queue from which messages are deleted.
    --
    -- Queue URLs and names are case-sensitive.
    queueUrl :: Prelude.Text,
    -- | The receipt handle associated with the message to delete.
    receiptHandle :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queueUrl', 'deleteMessage_queueUrl' - The URL of the Amazon SQS queue from which messages are deleted.
--
-- Queue URLs and names are case-sensitive.
--
-- 'receiptHandle', 'deleteMessage_receiptHandle' - The receipt handle associated with the message to delete.
newDeleteMessage ::
  -- | 'queueUrl'
  Prelude.Text ->
  -- | 'receiptHandle'
  Prelude.Text ->
  DeleteMessage
newDeleteMessage pQueueUrl_ pReceiptHandle_ =
  DeleteMessage'
    { queueUrl = pQueueUrl_,
      receiptHandle = pReceiptHandle_
    }

-- | The URL of the Amazon SQS queue from which messages are deleted.
--
-- Queue URLs and names are case-sensitive.
deleteMessage_queueUrl :: Lens.Lens' DeleteMessage Prelude.Text
deleteMessage_queueUrl = Lens.lens (\DeleteMessage' {queueUrl} -> queueUrl) (\s@DeleteMessage' {} a -> s {queueUrl = a} :: DeleteMessage)

-- | The receipt handle associated with the message to delete.
deleteMessage_receiptHandle :: Lens.Lens' DeleteMessage Prelude.Text
deleteMessage_receiptHandle = Lens.lens (\DeleteMessage' {receiptHandle} -> receiptHandle) (\s@DeleteMessage' {} a -> s {receiptHandle = a} :: DeleteMessage)

instance Prelude.AWSRequest DeleteMessage where
  type Rs DeleteMessage = DeleteMessageResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull DeleteMessageResponse'

instance Prelude.Hashable DeleteMessage

instance Prelude.NFData DeleteMessage

instance Prelude.ToHeaders DeleteMessage where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteMessage where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteMessage where
  toQuery DeleteMessage' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeleteMessage" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2012-11-05" :: Prelude.ByteString),
        "QueueUrl" Prelude.=: queueUrl,
        "ReceiptHandle" Prelude.=: receiptHandle
      ]

-- | /See:/ 'newDeleteMessageResponse' smart constructor.
data DeleteMessageResponse = DeleteMessageResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteMessageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteMessageResponse ::
  DeleteMessageResponse
newDeleteMessageResponse = DeleteMessageResponse'

instance Prelude.NFData DeleteMessageResponse
