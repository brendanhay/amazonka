{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.DeleteMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified message from the specified queue. To select the message to delete, use the @ReceiptHandle@ of the message (/not/ the @MessageId@ which you receive when you send the message). Amazon SQS can delete a message from a queue even if a visibility timeout setting causes the message to be locked by another consumer. Amazon SQS automatically deletes messages left in a queue longer than the retention period configured for the queue.
module Network.AWS.SQS.DeleteMessage
  ( -- * Creating a request
    DeleteMessage (..),
    mkDeleteMessage,

    -- ** Request lenses
    dmQueueURL,
    dmReceiptHandle,

    -- * Destructuring the response
    DeleteMessageResponse (..),
    mkDeleteMessageResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SQS.Types

-- |
--
-- /See:/ 'mkDeleteMessage' smart constructor.
data DeleteMessage = DeleteMessage'
  { queueURL :: Lude.Text,
    receiptHandle :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteMessage' with the minimum fields required to make a request.
--
-- * 'queueURL' - The URL of the Amazon SQS queue from which messages are deleted.
--
-- Queue URLs and names are case-sensitive.
-- * 'receiptHandle' - The receipt handle associated with the message to delete.
mkDeleteMessage ::
  -- | 'queueURL'
  Lude.Text ->
  -- | 'receiptHandle'
  Lude.Text ->
  DeleteMessage
mkDeleteMessage pQueueURL_ pReceiptHandle_ =
  DeleteMessage'
    { queueURL = pQueueURL_,
      receiptHandle = pReceiptHandle_
    }

-- | The URL of the Amazon SQS queue from which messages are deleted.
--
-- Queue URLs and names are case-sensitive.
--
-- /Note:/ Consider using 'queueURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmQueueURL :: Lens.Lens' DeleteMessage Lude.Text
dmQueueURL = Lens.lens (queueURL :: DeleteMessage -> Lude.Text) (\s a -> s {queueURL = a} :: DeleteMessage)
{-# DEPRECATED dmQueueURL "Use generic-lens or generic-optics with 'queueURL' instead." #-}

-- | The receipt handle associated with the message to delete.
--
-- /Note:/ Consider using 'receiptHandle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmReceiptHandle :: Lens.Lens' DeleteMessage Lude.Text
dmReceiptHandle = Lens.lens (receiptHandle :: DeleteMessage -> Lude.Text) (\s a -> s {receiptHandle = a} :: DeleteMessage)
{-# DEPRECATED dmReceiptHandle "Use generic-lens or generic-optics with 'receiptHandle' instead." #-}

instance Lude.AWSRequest DeleteMessage where
  type Rs DeleteMessage = DeleteMessageResponse
  request = Req.postQuery sqsService
  response = Res.receiveNull DeleteMessageResponse'

instance Lude.ToHeaders DeleteMessage where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteMessage where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteMessage where
  toQuery DeleteMessage' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteMessage" :: Lude.ByteString),
        "Version" Lude.=: ("2012-11-05" :: Lude.ByteString),
        "QueueUrl" Lude.=: queueURL,
        "ReceiptHandle" Lude.=: receiptHandle
      ]

-- | /See:/ 'mkDeleteMessageResponse' smart constructor.
data DeleteMessageResponse = DeleteMessageResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteMessageResponse' with the minimum fields required to make a request.
mkDeleteMessageResponse ::
  DeleteMessageResponse
mkDeleteMessageResponse = DeleteMessageResponse'
