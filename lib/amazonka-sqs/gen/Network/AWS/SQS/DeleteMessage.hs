{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    dmQueueUrl,
    dmReceiptHandle,

    -- * Destructuring the response
    DeleteMessageResponse (..),
    mkDeleteMessageResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SQS.Types as Types

-- |
--
-- /See:/ 'mkDeleteMessage' smart constructor.
data DeleteMessage = DeleteMessage'
  { -- | The URL of the Amazon SQS queue from which messages are deleted.
    --
    -- Queue URLs and names are case-sensitive.
    queueUrl :: Types.String,
    -- | The receipt handle associated with the message to delete.
    receiptHandle :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteMessage' value with any optional fields omitted.
mkDeleteMessage ::
  -- | 'queueUrl'
  Types.String ->
  -- | 'receiptHandle'
  Types.String ->
  DeleteMessage
mkDeleteMessage queueUrl receiptHandle =
  DeleteMessage' {queueUrl, receiptHandle}

-- | The URL of the Amazon SQS queue from which messages are deleted.
--
-- Queue URLs and names are case-sensitive.
--
-- /Note:/ Consider using 'queueUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmQueueUrl :: Lens.Lens' DeleteMessage Types.String
dmQueueUrl = Lens.field @"queueUrl"
{-# DEPRECATED dmQueueUrl "Use generic-lens or generic-optics with 'queueUrl' instead." #-}

-- | The receipt handle associated with the message to delete.
--
-- /Note:/ Consider using 'receiptHandle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmReceiptHandle :: Lens.Lens' DeleteMessage Types.String
dmReceiptHandle = Lens.field @"receiptHandle"
{-# DEPRECATED dmReceiptHandle "Use generic-lens or generic-optics with 'receiptHandle' instead." #-}

instance Core.AWSRequest DeleteMessage where
  type Rs DeleteMessage = DeleteMessageResponse
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
            ( Core.pure ("Action", "DeleteMessage")
                Core.<> (Core.pure ("Version", "2012-11-05"))
                Core.<> (Core.toQueryValue "QueueUrl" queueUrl)
                Core.<> (Core.toQueryValue "ReceiptHandle" receiptHandle)
            )
      }
  response = Response.receiveNull DeleteMessageResponse'

-- | /See:/ 'mkDeleteMessageResponse' smart constructor.
data DeleteMessageResponse = DeleteMessageResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteMessageResponse' value with any optional fields omitted.
mkDeleteMessageResponse ::
  DeleteMessageResponse
mkDeleteMessageResponse = DeleteMessageResponse'
