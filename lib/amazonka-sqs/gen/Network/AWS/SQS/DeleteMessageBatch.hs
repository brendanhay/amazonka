{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    dmbQueueUrl,
    dmbEntries,

    -- * Destructuring the response
    DeleteMessageBatchResponse (..),
    mkDeleteMessageBatchResponse,

    -- ** Response lenses
    dmbrrsSuccessful,
    dmbrrsFailed,
    dmbrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SQS.Types as Types

-- |
--
-- /See:/ 'mkDeleteMessageBatch' smart constructor.
data DeleteMessageBatch = DeleteMessageBatch'
  { -- | The URL of the Amazon SQS queue from which messages are deleted.
    --
    -- Queue URLs and names are case-sensitive.
    queueUrl :: Types.QueueUrl,
    -- | A list of receipt handles for the messages to be deleted.
    entries :: [Types.DeleteMessageBatchRequestEntry]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteMessageBatch' value with any optional fields omitted.
mkDeleteMessageBatch ::
  -- | 'queueUrl'
  Types.QueueUrl ->
  DeleteMessageBatch
mkDeleteMessageBatch queueUrl =
  DeleteMessageBatch' {queueUrl, entries = Core.mempty}

-- | The URL of the Amazon SQS queue from which messages are deleted.
--
-- Queue URLs and names are case-sensitive.
--
-- /Note:/ Consider using 'queueUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmbQueueUrl :: Lens.Lens' DeleteMessageBatch Types.QueueUrl
dmbQueueUrl = Lens.field @"queueUrl"
{-# DEPRECATED dmbQueueUrl "Use generic-lens or generic-optics with 'queueUrl' instead." #-}

-- | A list of receipt handles for the messages to be deleted.
--
-- /Note:/ Consider using 'entries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmbEntries :: Lens.Lens' DeleteMessageBatch [Types.DeleteMessageBatchRequestEntry]
dmbEntries = Lens.field @"entries"
{-# DEPRECATED dmbEntries "Use generic-lens or generic-optics with 'entries' instead." #-}

instance Core.AWSRequest DeleteMessageBatch where
  type Rs DeleteMessageBatch = DeleteMessageBatchResponse
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
            ( Core.pure ("Action", "DeleteMessageBatch")
                Core.<> (Core.pure ("Version", "2012-11-05"))
                Core.<> (Core.toQueryValue "QueueUrl" queueUrl)
                Core.<> (Core.toQueryList "DeleteMessageBatchRequestEntry" entries)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DeleteMessageBatchResult"
      ( \s h x ->
          DeleteMessageBatchResponse'
            Core.<$> (x Core..@? "DeleteMessageBatchResultEntry" Core..@! Core.mempty)
            Core.<*> (x Core..@? "BatchResultErrorEntry" Core..@! Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | For each message in the batch, the response contains a @'DeleteMessageBatchResultEntry' @ tag if the message is deleted or a @'BatchResultErrorEntry' @ tag if the message can't be deleted.
--
-- /See:/ 'mkDeleteMessageBatchResponse' smart constructor.
data DeleteMessageBatchResponse = DeleteMessageBatchResponse'
  { -- | A list of @'DeleteMessageBatchResultEntry' @ items.
    successful :: [Types.DeleteMessageBatchResultEntry],
    -- | A list of @'BatchResultErrorEntry' @ items.
    failed :: [Types.BatchResultErrorEntry],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteMessageBatchResponse' value with any optional fields omitted.
mkDeleteMessageBatchResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteMessageBatchResponse
mkDeleteMessageBatchResponse responseStatus =
  DeleteMessageBatchResponse'
    { successful = Core.mempty,
      failed = Core.mempty,
      responseStatus
    }

-- | A list of @'DeleteMessageBatchResultEntry' @ items.
--
-- /Note:/ Consider using 'successful' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmbrrsSuccessful :: Lens.Lens' DeleteMessageBatchResponse [Types.DeleteMessageBatchResultEntry]
dmbrrsSuccessful = Lens.field @"successful"
{-# DEPRECATED dmbrrsSuccessful "Use generic-lens or generic-optics with 'successful' instead." #-}

-- | A list of @'BatchResultErrorEntry' @ items.
--
-- /Note:/ Consider using 'failed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmbrrsFailed :: Lens.Lens' DeleteMessageBatchResponse [Types.BatchResultErrorEntry]
dmbrrsFailed = Lens.field @"failed"
{-# DEPRECATED dmbrrsFailed "Use generic-lens or generic-optics with 'failed' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmbrrsResponseStatus :: Lens.Lens' DeleteMessageBatchResponse Core.Int
dmbrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dmbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
