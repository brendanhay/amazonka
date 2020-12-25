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
    smbQueueUrl,
    smbEntries,

    -- * Destructuring the response
    SendMessageBatchResponse (..),
    mkSendMessageBatchResponse,

    -- ** Response lenses
    smbrrsSuccessful,
    smbrrsFailed,
    smbrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SQS.Types as Types

-- |
--
-- /See:/ 'mkSendMessageBatch' smart constructor.
data SendMessageBatch = SendMessageBatch'
  { -- | The URL of the Amazon SQS queue to which batched messages are sent.
    --
    -- Queue URLs and names are case-sensitive.
    queueUrl :: Types.String,
    -- | A list of @'SendMessageBatchRequestEntry' @ items.
    entries :: [Types.SendMessageBatchRequestEntry]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SendMessageBatch' value with any optional fields omitted.
mkSendMessageBatch ::
  -- | 'queueUrl'
  Types.String ->
  SendMessageBatch
mkSendMessageBatch queueUrl =
  SendMessageBatch' {queueUrl, entries = Core.mempty}

-- | The URL of the Amazon SQS queue to which batched messages are sent.
--
-- Queue URLs and names are case-sensitive.
--
-- /Note:/ Consider using 'queueUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbQueueUrl :: Lens.Lens' SendMessageBatch Types.String
smbQueueUrl = Lens.field @"queueUrl"
{-# DEPRECATED smbQueueUrl "Use generic-lens or generic-optics with 'queueUrl' instead." #-}

-- | A list of @'SendMessageBatchRequestEntry' @ items.
--
-- /Note:/ Consider using 'entries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbEntries :: Lens.Lens' SendMessageBatch [Types.SendMessageBatchRequestEntry]
smbEntries = Lens.field @"entries"
{-# DEPRECATED smbEntries "Use generic-lens or generic-optics with 'entries' instead." #-}

instance Core.AWSRequest SendMessageBatch where
  type Rs SendMessageBatch = SendMessageBatchResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "SendMessageBatch")
                Core.<> (Core.pure ("Version", "2012-11-05"))
                Core.<> (Core.toQueryValue "QueueUrl" queueUrl)
                Core.<> (Core.toQueryList "SendMessageBatchRequestEntry" entries)
            )
      }
  response =
    Response.receiveXMLWrapper
      "SendMessageBatchResult"
      ( \s h x ->
          SendMessageBatchResponse'
            Core.<$> (x Core..@? "SendMessageBatchResultEntry" Core..@! Core.mempty)
            Core.<*> (x Core..@? "BatchResultErrorEntry" Core..@! Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | For each message in the batch, the response contains a @'SendMessageBatchResultEntry' @ tag if the message succeeds or a @'BatchResultErrorEntry' @ tag if the message fails.
--
-- /See:/ 'mkSendMessageBatchResponse' smart constructor.
data SendMessageBatchResponse = SendMessageBatchResponse'
  { -- | A list of @'SendMessageBatchResultEntry' @ items.
    successful :: [Types.SendMessageBatchResultEntry],
    -- | A list of @'BatchResultErrorEntry' @ items with error details about each message that can't be enqueued.
    failed :: [Types.BatchResultErrorEntry],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SendMessageBatchResponse' value with any optional fields omitted.
mkSendMessageBatchResponse ::
  -- | 'responseStatus'
  Core.Int ->
  SendMessageBatchResponse
mkSendMessageBatchResponse responseStatus =
  SendMessageBatchResponse'
    { successful = Core.mempty,
      failed = Core.mempty,
      responseStatus
    }

-- | A list of @'SendMessageBatchResultEntry' @ items.
--
-- /Note:/ Consider using 'successful' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbrrsSuccessful :: Lens.Lens' SendMessageBatchResponse [Types.SendMessageBatchResultEntry]
smbrrsSuccessful = Lens.field @"successful"
{-# DEPRECATED smbrrsSuccessful "Use generic-lens or generic-optics with 'successful' instead." #-}

-- | A list of @'BatchResultErrorEntry' @ items with error details about each message that can't be enqueued.
--
-- /Note:/ Consider using 'failed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbrrsFailed :: Lens.Lens' SendMessageBatchResponse [Types.BatchResultErrorEntry]
smbrrsFailed = Lens.field @"failed"
{-# DEPRECATED smbrrsFailed "Use generic-lens or generic-optics with 'failed' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbrrsResponseStatus :: Lens.Lens' SendMessageBatchResponse Core.Int
smbrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED smbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
