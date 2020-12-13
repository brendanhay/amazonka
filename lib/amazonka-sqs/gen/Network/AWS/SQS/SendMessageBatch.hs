{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.SendMessageBatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delivers up to ten messages to the specified queue. This is a batch version of @'SendMessage' .@ For a FIFO queue, multiple messages within a single batch are enqueued in the order they are sent.
--
-- The result of sending each message is reported individually in the response. Because the batch request can result in a combination of successful and unsuccessful actions, you should check for batch errors even when the call returns an HTTP status code of @200@ .
-- The maximum allowed individual message size and the maximum total payload size (the sum of the individual lengths of all of the batched messages) are both 256 KB (262,144 bytes).
-- /Important:/ A message can include only XML, JSON, and unformatted text. The following Unicode characters are allowed:
-- @#x9@ | @#xA@ | @#xD@ | @#x20@ to @#xD7FF@ | @#xE000@ to @#xFFFD@ | @#x10000@ to @#x10FFFF@
-- Any characters not included in this list will be rejected. For more information, see the <http://www.w3.org/TR/REC-xml/#charsets W3C specification for characters> .
-- If you don't specify the @DelaySeconds@ parameter for an entry, Amazon SQS uses the default value for the queue.
-- Some actions take lists of parameters. These lists are specified using the @param.n@ notation. Values of @n@ are integers starting from 1. For example, a parameter list with two elements looks like this:
-- @&AttributeName.1=first@
-- @&AttributeName.2=second@
module Network.AWS.SQS.SendMessageBatch
  ( -- * Creating a request
    SendMessageBatch (..),
    mkSendMessageBatch,

    -- ** Request lenses
    smbEntries,
    smbQueueURL,

    -- * Destructuring the response
    SendMessageBatchResponse (..),
    mkSendMessageBatchResponse,

    -- ** Response lenses
    smbrsSuccessful,
    smbrsFailed,
    smbrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SQS.Types

-- |
--
-- /See:/ 'mkSendMessageBatch' smart constructor.
data SendMessageBatch = SendMessageBatch'
  { -- | A list of @'SendMessageBatchRequestEntry' @ items.
    entries :: [SendMessageBatchRequestEntry],
    -- | The URL of the Amazon SQS queue to which batched messages are sent.
    --
    -- Queue URLs and names are case-sensitive.
    queueURL :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SendMessageBatch' with the minimum fields required to make a request.
--
-- * 'entries' - A list of @'SendMessageBatchRequestEntry' @ items.
-- * 'queueURL' - The URL of the Amazon SQS queue to which batched messages are sent.
--
-- Queue URLs and names are case-sensitive.
mkSendMessageBatch ::
  -- | 'queueURL'
  Lude.Text ->
  SendMessageBatch
mkSendMessageBatch pQueueURL_ =
  SendMessageBatch' {entries = Lude.mempty, queueURL = pQueueURL_}

-- | A list of @'SendMessageBatchRequestEntry' @ items.
--
-- /Note:/ Consider using 'entries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbEntries :: Lens.Lens' SendMessageBatch [SendMessageBatchRequestEntry]
smbEntries = Lens.lens (entries :: SendMessageBatch -> [SendMessageBatchRequestEntry]) (\s a -> s {entries = a} :: SendMessageBatch)
{-# DEPRECATED smbEntries "Use generic-lens or generic-optics with 'entries' instead." #-}

-- | The URL of the Amazon SQS queue to which batched messages are sent.
--
-- Queue URLs and names are case-sensitive.
--
-- /Note:/ Consider using 'queueURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbQueueURL :: Lens.Lens' SendMessageBatch Lude.Text
smbQueueURL = Lens.lens (queueURL :: SendMessageBatch -> Lude.Text) (\s a -> s {queueURL = a} :: SendMessageBatch)
{-# DEPRECATED smbQueueURL "Use generic-lens or generic-optics with 'queueURL' instead." #-}

instance Lude.AWSRequest SendMessageBatch where
  type Rs SendMessageBatch = SendMessageBatchResponse
  request = Req.postQuery sqsService
  response =
    Res.receiveXMLWrapper
      "SendMessageBatchResult"
      ( \s h x ->
          SendMessageBatchResponse'
            Lude.<$> (Lude.parseXMLList "SendMessageBatchResultEntry" x)
            Lude.<*> (Lude.parseXMLList "BatchResultErrorEntry" x)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SendMessageBatch where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath SendMessageBatch where
  toPath = Lude.const "/"

instance Lude.ToQuery SendMessageBatch where
  toQuery SendMessageBatch' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("SendMessageBatch" :: Lude.ByteString),
        "Version" Lude.=: ("2012-11-05" :: Lude.ByteString),
        Lude.toQueryList "SendMessageBatchRequestEntry" entries,
        "QueueUrl" Lude.=: queueURL
      ]

-- | For each message in the batch, the response contains a @'SendMessageBatchResultEntry' @ tag if the message succeeds or a @'BatchResultErrorEntry' @ tag if the message fails.
--
-- /See:/ 'mkSendMessageBatchResponse' smart constructor.
data SendMessageBatchResponse = SendMessageBatchResponse'
  { -- | A list of @'SendMessageBatchResultEntry' @ items.
    successful :: [SendMessageBatchResultEntry],
    -- | A list of @'BatchResultErrorEntry' @ items with error details about each message that can't be enqueued.
    failed :: [BatchResultErrorEntry],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SendMessageBatchResponse' with the minimum fields required to make a request.
--
-- * 'successful' - A list of @'SendMessageBatchResultEntry' @ items.
-- * 'failed' - A list of @'BatchResultErrorEntry' @ items with error details about each message that can't be enqueued.
-- * 'responseStatus' - The response status code.
mkSendMessageBatchResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SendMessageBatchResponse
mkSendMessageBatchResponse pResponseStatus_ =
  SendMessageBatchResponse'
    { successful = Lude.mempty,
      failed = Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | A list of @'SendMessageBatchResultEntry' @ items.
--
-- /Note:/ Consider using 'successful' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbrsSuccessful :: Lens.Lens' SendMessageBatchResponse [SendMessageBatchResultEntry]
smbrsSuccessful = Lens.lens (successful :: SendMessageBatchResponse -> [SendMessageBatchResultEntry]) (\s a -> s {successful = a} :: SendMessageBatchResponse)
{-# DEPRECATED smbrsSuccessful "Use generic-lens or generic-optics with 'successful' instead." #-}

-- | A list of @'BatchResultErrorEntry' @ items with error details about each message that can't be enqueued.
--
-- /Note:/ Consider using 'failed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbrsFailed :: Lens.Lens' SendMessageBatchResponse [BatchResultErrorEntry]
smbrsFailed = Lens.lens (failed :: SendMessageBatchResponse -> [BatchResultErrorEntry]) (\s a -> s {failed = a} :: SendMessageBatchResponse)
{-# DEPRECATED smbrsFailed "Use generic-lens or generic-optics with 'failed' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbrsResponseStatus :: Lens.Lens' SendMessageBatchResponse Lude.Int
smbrsResponseStatus = Lens.lens (responseStatus :: SendMessageBatchResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SendMessageBatchResponse)
{-# DEPRECATED smbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
