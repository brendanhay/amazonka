{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.ChangeMessageVisibilityBatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the visibility timeout of multiple messages. This is a batch version of @'ChangeMessageVisibility' .@ The result of the action on each message is reported individually in the response. You can send up to 10 @'ChangeMessageVisibility' @ requests with each @ChangeMessageVisibilityBatch@ action.
--
-- /Important:/ Because the batch request can result in a combination of successful and unsuccessful actions, you should check for batch errors even when the call returns an HTTP status code of @200@ .
-- Some actions take lists of parameters. These lists are specified using the @param.n@ notation. Values of @n@ are integers starting from 1. For example, a parameter list with two elements looks like this:
-- @&AttributeName.1=first@
-- @&AttributeName.2=second@
module Network.AWS.SQS.ChangeMessageVisibilityBatch
  ( -- * Creating a request
    ChangeMessageVisibilityBatch (..),
    mkChangeMessageVisibilityBatch,

    -- ** Request lenses
    cmvbEntries,
    cmvbQueueURL,

    -- * Destructuring the response
    ChangeMessageVisibilityBatchResponse (..),
    mkChangeMessageVisibilityBatchResponse,

    -- ** Response lenses
    cmvbrsSuccessful,
    cmvbrsFailed,
    cmvbrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SQS.Types

-- |
--
-- /See:/ 'mkChangeMessageVisibilityBatch' smart constructor.
data ChangeMessageVisibilityBatch = ChangeMessageVisibilityBatch'
  { -- | A list of receipt handles of the messages for which the visibility timeout must be changed.
    entries :: [ChangeMessageVisibilityBatchRequestEntry],
    -- | The URL of the Amazon SQS queue whose messages' visibility is changed.
    --
    -- Queue URLs and names are case-sensitive.
    queueURL :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ChangeMessageVisibilityBatch' with the minimum fields required to make a request.
--
-- * 'entries' - A list of receipt handles of the messages for which the visibility timeout must be changed.
-- * 'queueURL' - The URL of the Amazon SQS queue whose messages' visibility is changed.
--
-- Queue URLs and names are case-sensitive.
mkChangeMessageVisibilityBatch ::
  -- | 'queueURL'
  Lude.Text ->
  ChangeMessageVisibilityBatch
mkChangeMessageVisibilityBatch pQueueURL_ =
  ChangeMessageVisibilityBatch'
    { entries = Lude.mempty,
      queueURL = pQueueURL_
    }

-- | A list of receipt handles of the messages for which the visibility timeout must be changed.
--
-- /Note:/ Consider using 'entries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmvbEntries :: Lens.Lens' ChangeMessageVisibilityBatch [ChangeMessageVisibilityBatchRequestEntry]
cmvbEntries = Lens.lens (entries :: ChangeMessageVisibilityBatch -> [ChangeMessageVisibilityBatchRequestEntry]) (\s a -> s {entries = a} :: ChangeMessageVisibilityBatch)
{-# DEPRECATED cmvbEntries "Use generic-lens or generic-optics with 'entries' instead." #-}

-- | The URL of the Amazon SQS queue whose messages' visibility is changed.
--
-- Queue URLs and names are case-sensitive.
--
-- /Note:/ Consider using 'queueURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmvbQueueURL :: Lens.Lens' ChangeMessageVisibilityBatch Lude.Text
cmvbQueueURL = Lens.lens (queueURL :: ChangeMessageVisibilityBatch -> Lude.Text) (\s a -> s {queueURL = a} :: ChangeMessageVisibilityBatch)
{-# DEPRECATED cmvbQueueURL "Use generic-lens or generic-optics with 'queueURL' instead." #-}

instance Lude.AWSRequest ChangeMessageVisibilityBatch where
  type
    Rs ChangeMessageVisibilityBatch =
      ChangeMessageVisibilityBatchResponse
  request = Req.postQuery sqsService
  response =
    Res.receiveXMLWrapper
      "ChangeMessageVisibilityBatchResult"
      ( \s h x ->
          ChangeMessageVisibilityBatchResponse'
            Lude.<$> (Lude.parseXMLList "ChangeMessageVisibilityBatchResultEntry" x)
            Lude.<*> (Lude.parseXMLList "BatchResultErrorEntry" x)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ChangeMessageVisibilityBatch where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ChangeMessageVisibilityBatch where
  toPath = Lude.const "/"

instance Lude.ToQuery ChangeMessageVisibilityBatch where
  toQuery ChangeMessageVisibilityBatch' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("ChangeMessageVisibilityBatch" :: Lude.ByteString),
        "Version" Lude.=: ("2012-11-05" :: Lude.ByteString),
        Lude.toQueryList
          "ChangeMessageVisibilityBatchRequestEntry"
          entries,
        "QueueUrl" Lude.=: queueURL
      ]

-- | For each message in the batch, the response contains a @'ChangeMessageVisibilityBatchResultEntry' @ tag if the message succeeds or a @'BatchResultErrorEntry' @ tag if the message fails.
--
-- /See:/ 'mkChangeMessageVisibilityBatchResponse' smart constructor.
data ChangeMessageVisibilityBatchResponse = ChangeMessageVisibilityBatchResponse'
  { -- | A list of @'ChangeMessageVisibilityBatchResultEntry' @ items.
    successful :: [ChangeMessageVisibilityBatchResultEntry],
    -- | A list of @'BatchResultErrorEntry' @ items.
    failed :: [BatchResultErrorEntry],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ChangeMessageVisibilityBatchResponse' with the minimum fields required to make a request.
--
-- * 'successful' - A list of @'ChangeMessageVisibilityBatchResultEntry' @ items.
-- * 'failed' - A list of @'BatchResultErrorEntry' @ items.
-- * 'responseStatus' - The response status code.
mkChangeMessageVisibilityBatchResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ChangeMessageVisibilityBatchResponse
mkChangeMessageVisibilityBatchResponse pResponseStatus_ =
  ChangeMessageVisibilityBatchResponse'
    { successful = Lude.mempty,
      failed = Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | A list of @'ChangeMessageVisibilityBatchResultEntry' @ items.
--
-- /Note:/ Consider using 'successful' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmvbrsSuccessful :: Lens.Lens' ChangeMessageVisibilityBatchResponse [ChangeMessageVisibilityBatchResultEntry]
cmvbrsSuccessful = Lens.lens (successful :: ChangeMessageVisibilityBatchResponse -> [ChangeMessageVisibilityBatchResultEntry]) (\s a -> s {successful = a} :: ChangeMessageVisibilityBatchResponse)
{-# DEPRECATED cmvbrsSuccessful "Use generic-lens or generic-optics with 'successful' instead." #-}

-- | A list of @'BatchResultErrorEntry' @ items.
--
-- /Note:/ Consider using 'failed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmvbrsFailed :: Lens.Lens' ChangeMessageVisibilityBatchResponse [BatchResultErrorEntry]
cmvbrsFailed = Lens.lens (failed :: ChangeMessageVisibilityBatchResponse -> [BatchResultErrorEntry]) (\s a -> s {failed = a} :: ChangeMessageVisibilityBatchResponse)
{-# DEPRECATED cmvbrsFailed "Use generic-lens or generic-optics with 'failed' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmvbrsResponseStatus :: Lens.Lens' ChangeMessageVisibilityBatchResponse Lude.Int
cmvbrsResponseStatus = Lens.lens (responseStatus :: ChangeMessageVisibilityBatchResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ChangeMessageVisibilityBatchResponse)
{-# DEPRECATED cmvbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
