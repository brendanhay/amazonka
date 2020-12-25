{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.ChangeMessageVisibility
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the visibility timeout of a specified message in a queue to a new value. The default visibility timeout for a message is 30 seconds. The minimum is 0 seconds. The maximum is 12 hours. For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-visibility-timeout.html Visibility Timeout> in the /Amazon Simple Queue Service Developer Guide/ .
--
-- For example, you have a message with a visibility timeout of 5 minutes. After 3 minutes, you call @ChangeMessageVisibility@ with a timeout of 10 minutes. You can continue to call @ChangeMessageVisibility@ to extend the visibility timeout to the maximum allowed time. If you try to extend the visibility timeout beyond the maximum, your request is rejected.
-- An Amazon SQS message has three basic states:
--
--     * Sent to a queue by a producer.
--
--
--     * Received from the queue by a consumer.
--
--
--     * Deleted from the queue.
--
--
-- A message is considered to be /stored/ after it is sent to a queue by a producer, but not yet received from the queue by a consumer (that is, between states 1 and 2). There is no limit to the number of stored messages. A message is considered to be /in flight/ after it is received from a queue by a consumer, but not yet deleted from the queue (that is, between states 2 and 3). There is a limit to the number of inflight messages.
-- Limits that apply to inflight messages are unrelated to the /unlimited/ number of stored messages.
-- For most standard queues (depending on queue traffic and message backlog), there can be a maximum of approximately 120,000 inflight messages (received from a queue by a consumer, but not yet deleted from the queue). If you reach this limit, Amazon SQS returns the @OverLimit@ error message. To avoid reaching the limit, you should delete messages from the queue after they're processed. You can also increase the number of queues you use to process your messages. To request a limit increase, <https://console.aws.amazon.com/support/home#/case/create?issueType=service-limit-increase&limitType=service-code-sqs file a support request> .
-- For FIFO queues, there can be a maximum of 20,000 inflight messages (received from a queue by a consumer, but not yet deleted from the queue). If you reach this limit, Amazon SQS returns no error messages.
-- /Important:/ If you attempt to set the @VisibilityTimeout@ to a value greater than the maximum time left, Amazon SQS returns an error. Amazon SQS doesn't automatically recalculate and increase the timeout to the maximum remaining time.
-- Unlike with a queue, when you change the visibility timeout for a specific message the timeout value is applied immediately but isn't saved in memory for that message. If you don't delete a message after it is received, the visibility timeout for the message reverts to the original timeout value (not to the value you set using the @ChangeMessageVisibility@ action) the next time the message is received.
module Network.AWS.SQS.ChangeMessageVisibility
  ( -- * Creating a request
    ChangeMessageVisibility (..),
    mkChangeMessageVisibility,

    -- ** Request lenses
    cmvQueueUrl,
    cmvReceiptHandle,
    cmvVisibilityTimeout,

    -- * Destructuring the response
    ChangeMessageVisibilityResponse (..),
    mkChangeMessageVisibilityResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SQS.Types as Types

-- | /See:/ 'mkChangeMessageVisibility' smart constructor.
data ChangeMessageVisibility = ChangeMessageVisibility'
  { -- | The URL of the Amazon SQS queue whose message's visibility is changed.
    --
    -- Queue URLs and names are case-sensitive.
    queueUrl :: Types.String,
    -- | The receipt handle associated with the message whose visibility timeout is changed. This parameter is returned by the @'ReceiveMessage' @ action.
    receiptHandle :: Types.String,
    -- | The new value for the message's visibility timeout (in seconds). Values range: @0@ to @43200@ . Maximum: 12 hours.
    visibilityTimeout :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ChangeMessageVisibility' value with any optional fields omitted.
mkChangeMessageVisibility ::
  -- | 'queueUrl'
  Types.String ->
  -- | 'receiptHandle'
  Types.String ->
  -- | 'visibilityTimeout'
  Core.Int ->
  ChangeMessageVisibility
mkChangeMessageVisibility queueUrl receiptHandle visibilityTimeout =
  ChangeMessageVisibility'
    { queueUrl,
      receiptHandle,
      visibilityTimeout
    }

-- | The URL of the Amazon SQS queue whose message's visibility is changed.
--
-- Queue URLs and names are case-sensitive.
--
-- /Note:/ Consider using 'queueUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmvQueueUrl :: Lens.Lens' ChangeMessageVisibility Types.String
cmvQueueUrl = Lens.field @"queueUrl"
{-# DEPRECATED cmvQueueUrl "Use generic-lens or generic-optics with 'queueUrl' instead." #-}

-- | The receipt handle associated with the message whose visibility timeout is changed. This parameter is returned by the @'ReceiveMessage' @ action.
--
-- /Note:/ Consider using 'receiptHandle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmvReceiptHandle :: Lens.Lens' ChangeMessageVisibility Types.String
cmvReceiptHandle = Lens.field @"receiptHandle"
{-# DEPRECATED cmvReceiptHandle "Use generic-lens or generic-optics with 'receiptHandle' instead." #-}

-- | The new value for the message's visibility timeout (in seconds). Values range: @0@ to @43200@ . Maximum: 12 hours.
--
-- /Note:/ Consider using 'visibilityTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmvVisibilityTimeout :: Lens.Lens' ChangeMessageVisibility Core.Int
cmvVisibilityTimeout = Lens.field @"visibilityTimeout"
{-# DEPRECATED cmvVisibilityTimeout "Use generic-lens or generic-optics with 'visibilityTimeout' instead." #-}

instance Core.AWSRequest ChangeMessageVisibility where
  type Rs ChangeMessageVisibility = ChangeMessageVisibilityResponse
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
            ( Core.pure ("Action", "ChangeMessageVisibility")
                Core.<> (Core.pure ("Version", "2012-11-05"))
                Core.<> (Core.toQueryValue "QueueUrl" queueUrl)
                Core.<> (Core.toQueryValue "ReceiptHandle" receiptHandle)
                Core.<> (Core.toQueryValue "VisibilityTimeout" visibilityTimeout)
            )
      }
  response = Response.receiveNull ChangeMessageVisibilityResponse'

-- | /See:/ 'mkChangeMessageVisibilityResponse' smart constructor.
data ChangeMessageVisibilityResponse = ChangeMessageVisibilityResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ChangeMessageVisibilityResponse' value with any optional fields omitted.
mkChangeMessageVisibilityResponse ::
  ChangeMessageVisibilityResponse
mkChangeMessageVisibilityResponse =
  ChangeMessageVisibilityResponse'
