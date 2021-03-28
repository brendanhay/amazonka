{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DeleteMessage (..)
    , mkDeleteMessage
    -- ** Request lenses
    , dmQueueUrl
    , dmReceiptHandle

    -- * Destructuring the response
    , DeleteMessageResponse (..)
    , mkDeleteMessageResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SQS.Types as Types

-- | 
--
-- /See:/ 'mkDeleteMessage' smart constructor.
data DeleteMessage = DeleteMessage'
  { queueUrl :: Core.Text
    -- ^ The URL of the Amazon SQS queue from which messages are deleted.
--
-- Queue URLs and names are case-sensitive.
  , receiptHandle :: Core.Text
    -- ^ The receipt handle associated with the message to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteMessage' value with any optional fields omitted.
mkDeleteMessage
    :: Core.Text -- ^ 'queueUrl'
    -> Core.Text -- ^ 'receiptHandle'
    -> DeleteMessage
mkDeleteMessage queueUrl receiptHandle
  = DeleteMessage'{queueUrl, receiptHandle}

-- | The URL of the Amazon SQS queue from which messages are deleted.
--
-- Queue URLs and names are case-sensitive.
--
-- /Note:/ Consider using 'queueUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmQueueUrl :: Lens.Lens' DeleteMessage Core.Text
dmQueueUrl = Lens.field @"queueUrl"
{-# INLINEABLE dmQueueUrl #-}
{-# DEPRECATED queueUrl "Use generic-lens or generic-optics with 'queueUrl' instead"  #-}

-- | The receipt handle associated with the message to delete.
--
-- /Note:/ Consider using 'receiptHandle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmReceiptHandle :: Lens.Lens' DeleteMessage Core.Text
dmReceiptHandle = Lens.field @"receiptHandle"
{-# INLINEABLE dmReceiptHandle #-}
{-# DEPRECATED receiptHandle "Use generic-lens or generic-optics with 'receiptHandle' instead"  #-}

instance Core.ToQuery DeleteMessage where
        toQuery DeleteMessage{..}
          = Core.toQueryPair "Action" ("DeleteMessage" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2012-11-05" :: Core.Text)
              Core.<> Core.toQueryPair "QueueUrl" queueUrl
              Core.<> Core.toQueryPair "ReceiptHandle" receiptHandle

instance Core.ToHeaders DeleteMessage where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteMessage where
        type Rs DeleteMessage = DeleteMessageResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteMessageResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteMessageResponse' smart constructor.
data DeleteMessageResponse = DeleteMessageResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteMessageResponse' value with any optional fields omitted.
mkDeleteMessageResponse
    :: DeleteMessageResponse
mkDeleteMessageResponse = DeleteMessageResponse'
