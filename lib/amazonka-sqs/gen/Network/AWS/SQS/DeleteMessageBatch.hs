{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.DeleteMessageBatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes up to ten messages from the specified queue. This is a batch version of @'DeleteMessage' .@ The result of the action on each message is reported individually in the response.
--
-- /Important:/ Because the batch request can result in a combination of successful and unsuccessful actions, you should check for batch errors even when the call returns an HTTP status code of @200@ .
-- Some actions take lists of parameters. These lists are specified using the @param.n@ notation. Values of @n@ are integers starting from 1. For example, a parameter list with two elements looks like this:
-- @&AttributeName.1=first@
-- @&AttributeName.2=second@
module Network.AWS.SQS.DeleteMessageBatch
  ( -- * Creating a request
    DeleteMessageBatch (..),
    mkDeleteMessageBatch,

    -- ** Request lenses
    dmbQueueURL,
    dmbEntries,

    -- * Destructuring the response
    DeleteMessageBatchResponse (..),
    mkDeleteMessageBatchResponse,

    -- ** Response lenses
    dmbrsResponseStatus,
    dmbrsSuccessful,
    dmbrsFailed,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SQS.Types

-- |
--
-- /See:/ 'mkDeleteMessageBatch' smart constructor.
data DeleteMessageBatch = DeleteMessageBatch'
  { queueURL ::
      Lude.Text,
    entries :: [DeleteMessageBatchRequestEntry]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteMessageBatch' with the minimum fields required to make a request.
--
-- * 'entries' - A list of receipt handles for the messages to be deleted.
-- * 'queueURL' - The URL of the Amazon SQS queue from which messages are deleted.
--
-- Queue URLs and names are case-sensitive.
mkDeleteMessageBatch ::
  -- | 'queueURL'
  Lude.Text ->
  DeleteMessageBatch
mkDeleteMessageBatch pQueueURL_ =
  DeleteMessageBatch' {queueURL = pQueueURL_, entries = Lude.mempty}

-- | The URL of the Amazon SQS queue from which messages are deleted.
--
-- Queue URLs and names are case-sensitive.
--
-- /Note:/ Consider using 'queueURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmbQueueURL :: Lens.Lens' DeleteMessageBatch Lude.Text
dmbQueueURL = Lens.lens (queueURL :: DeleteMessageBatch -> Lude.Text) (\s a -> s {queueURL = a} :: DeleteMessageBatch)
{-# DEPRECATED dmbQueueURL "Use generic-lens or generic-optics with 'queueURL' instead." #-}

-- | A list of receipt handles for the messages to be deleted.
--
-- /Note:/ Consider using 'entries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmbEntries :: Lens.Lens' DeleteMessageBatch [DeleteMessageBatchRequestEntry]
dmbEntries = Lens.lens (entries :: DeleteMessageBatch -> [DeleteMessageBatchRequestEntry]) (\s a -> s {entries = a} :: DeleteMessageBatch)
{-# DEPRECATED dmbEntries "Use generic-lens or generic-optics with 'entries' instead." #-}

instance Lude.AWSRequest DeleteMessageBatch where
  type Rs DeleteMessageBatch = DeleteMessageBatchResponse
  request = Req.postQuery sqsService
  response =
    Res.receiveXMLWrapper
      "DeleteMessageBatchResult"
      ( \s h x ->
          DeleteMessageBatchResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (Lude.parseXMLList "DeleteMessageBatchResultEntry" x)
            Lude.<*> (Lude.parseXMLList "BatchResultErrorEntry" x)
      )

instance Lude.ToHeaders DeleteMessageBatch where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteMessageBatch where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteMessageBatch where
  toQuery DeleteMessageBatch' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteMessageBatch" :: Lude.ByteString),
        "Version" Lude.=: ("2012-11-05" :: Lude.ByteString),
        "QueueUrl" Lude.=: queueURL,
        Lude.toQueryList "DeleteMessageBatchRequestEntry" entries
      ]

-- | For each message in the batch, the response contains a @'DeleteMessageBatchResultEntry' @ tag if the message is deleted or a @'BatchResultErrorEntry' @ tag if the message can't be deleted.
--
-- /See:/ 'mkDeleteMessageBatchResponse' smart constructor.
data DeleteMessageBatchResponse = DeleteMessageBatchResponse'
  { responseStatus ::
      Lude.Int,
    successful ::
      [DeleteMessageBatchResultEntry],
    failed :: [BatchResultErrorEntry]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteMessageBatchResponse' with the minimum fields required to make a request.
--
-- * 'failed' - A list of @'BatchResultErrorEntry' @ items.
-- * 'responseStatus' - The response status code.
-- * 'successful' - A list of @'DeleteMessageBatchResultEntry' @ items.
mkDeleteMessageBatchResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteMessageBatchResponse
mkDeleteMessageBatchResponse pResponseStatus_ =
  DeleteMessageBatchResponse'
    { responseStatus = pResponseStatus_,
      successful = Lude.mempty,
      failed = Lude.mempty
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmbrsResponseStatus :: Lens.Lens' DeleteMessageBatchResponse Lude.Int
dmbrsResponseStatus = Lens.lens (responseStatus :: DeleteMessageBatchResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteMessageBatchResponse)
{-# DEPRECATED dmbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A list of @'DeleteMessageBatchResultEntry' @ items.
--
-- /Note:/ Consider using 'successful' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmbrsSuccessful :: Lens.Lens' DeleteMessageBatchResponse [DeleteMessageBatchResultEntry]
dmbrsSuccessful = Lens.lens (successful :: DeleteMessageBatchResponse -> [DeleteMessageBatchResultEntry]) (\s a -> s {successful = a} :: DeleteMessageBatchResponse)
{-# DEPRECATED dmbrsSuccessful "Use generic-lens or generic-optics with 'successful' instead." #-}

-- | A list of @'BatchResultErrorEntry' @ items.
--
-- /Note:/ Consider using 'failed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmbrsFailed :: Lens.Lens' DeleteMessageBatchResponse [BatchResultErrorEntry]
dmbrsFailed = Lens.lens (failed :: DeleteMessageBatchResponse -> [BatchResultErrorEntry]) (\s a -> s {failed = a} :: DeleteMessageBatchResponse)
{-# DEPRECATED dmbrsFailed "Use generic-lens or generic-optics with 'failed' instead." #-}
