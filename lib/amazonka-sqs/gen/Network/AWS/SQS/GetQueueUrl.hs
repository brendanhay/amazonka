{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.GetQueueUrl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the URL of an existing Amazon SQS queue.
--
-- To access a queue that belongs to another AWS account, use the @QueueOwnerAWSAccountId@ parameter to specify the account ID of the queue's owner. The queue's owner must grant you permission to access the queue. For more information about shared queue access, see @'AddPermission' @ or see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-writing-an-sqs-policy.html#write-messages-to-shared-queue Allow Developers to Write Messages to a Shared Queue> in the /Amazon Simple Queue Service Developer Guide/ . 
module Network.AWS.SQS.GetQueueUrl
    (
    -- * Creating a request
      GetQueueUrl (..)
    , mkGetQueueUrl
    -- ** Request lenses
    , gquQueueName
    , gquQueueOwnerAWSAccountId

    -- * Destructuring the response
    , GetQueueUrlResponse (..)
    , mkGetQueueUrlResponse
    -- ** Response lenses
    , gqurrsQueueUrl
    , gqurrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SQS.Types as Types

-- | 
--
-- /See:/ 'mkGetQueueUrl' smart constructor.
data GetQueueUrl = GetQueueUrl'
  { queueName :: Core.Text
    -- ^ The name of the queue whose URL must be fetched. Maximum 80 characters. Valid values: alphanumeric characters, hyphens (@-@ ), and underscores (@_@ ).
--
-- Queue URLs and names are case-sensitive.
  , queueOwnerAWSAccountId :: Core.Maybe Core.Text
    -- ^ The AWS account ID of the account that created the queue.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetQueueUrl' value with any optional fields omitted.
mkGetQueueUrl
    :: Core.Text -- ^ 'queueName'
    -> GetQueueUrl
mkGetQueueUrl queueName
  = GetQueueUrl'{queueName, queueOwnerAWSAccountId = Core.Nothing}

-- | The name of the queue whose URL must be fetched. Maximum 80 characters. Valid values: alphanumeric characters, hyphens (@-@ ), and underscores (@_@ ).
--
-- Queue URLs and names are case-sensitive.
--
-- /Note:/ Consider using 'queueName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gquQueueName :: Lens.Lens' GetQueueUrl Core.Text
gquQueueName = Lens.field @"queueName"
{-# INLINEABLE gquQueueName #-}
{-# DEPRECATED queueName "Use generic-lens or generic-optics with 'queueName' instead"  #-}

-- | The AWS account ID of the account that created the queue.
--
-- /Note:/ Consider using 'queueOwnerAWSAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gquQueueOwnerAWSAccountId :: Lens.Lens' GetQueueUrl (Core.Maybe Core.Text)
gquQueueOwnerAWSAccountId = Lens.field @"queueOwnerAWSAccountId"
{-# INLINEABLE gquQueueOwnerAWSAccountId #-}
{-# DEPRECATED queueOwnerAWSAccountId "Use generic-lens or generic-optics with 'queueOwnerAWSAccountId' instead"  #-}

instance Core.ToQuery GetQueueUrl where
        toQuery GetQueueUrl{..}
          = Core.toQueryPair "Action" ("GetQueueUrl" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2012-11-05" :: Core.Text)
              Core.<> Core.toQueryPair "QueueName" queueName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "QueueOwnerAWSAccountId")
                queueOwnerAWSAccountId

instance Core.ToHeaders GetQueueUrl where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetQueueUrl where
        type Rs GetQueueUrl = GetQueueUrlResponse
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
        parseResponse
          = Response.receiveXMLWrapper "GetQueueUrlResult"
              (\ s h x ->
                 GetQueueUrlResponse' Core.<$>
                   (x Core..@ "QueueUrl") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-api-responses.html Interpreting Responses> in the /Amazon Simple Queue Service Developer Guide/ .
--
-- /See:/ 'mkGetQueueUrlResponse' smart constructor.
data GetQueueUrlResponse = GetQueueUrlResponse'
  { queueUrl :: Core.Text
    -- ^ The URL of the queue.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetQueueUrlResponse' value with any optional fields omitted.
mkGetQueueUrlResponse
    :: Core.Text -- ^ 'queueUrl'
    -> Core.Int -- ^ 'responseStatus'
    -> GetQueueUrlResponse
mkGetQueueUrlResponse queueUrl responseStatus
  = GetQueueUrlResponse'{queueUrl, responseStatus}

-- | The URL of the queue.
--
-- /Note:/ Consider using 'queueUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqurrsQueueUrl :: Lens.Lens' GetQueueUrlResponse Core.Text
gqurrsQueueUrl = Lens.field @"queueUrl"
{-# INLINEABLE gqurrsQueueUrl #-}
{-# DEPRECATED queueUrl "Use generic-lens or generic-optics with 'queueUrl' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqurrsResponseStatus :: Lens.Lens' GetQueueUrlResponse Core.Int
gqurrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gqurrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
