{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.ReceiveMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves one or more messages (up to 10), from the specified queue. Using the @WaitTimeSeconds@ parameter enables long-poll support. For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-long-polling.html Amazon SQS Long Polling> in the /Amazon Simple Queue Service Developer Guide/ . 
--
-- Short poll is the default behavior where a weighted random set of machines is sampled on a @ReceiveMessage@ call. Thus, only the messages on the sampled machines are returned. If the number of messages in the queue is small (fewer than 1,000), you most likely get fewer messages than you requested per @ReceiveMessage@ call. If the number of messages in the queue is extremely small, you might not receive any messages in a particular @ReceiveMessage@ response. If this happens, repeat the request. 
-- For each message returned, the response includes the following:
--
--     * The message body.
--
--
--     * An MD5 digest of the message body. For information about MD5, see <https://www.ietf.org/rfc/rfc1321.txt RFC1321> .
--
--
--     * The @MessageId@ you received when you sent the message to the queue.
--
--
--     * The receipt handle.
--
--
--     * The message attributes.
--
--
--     * An MD5 digest of the message attributes.
--
--
-- The receipt handle is the identifier you must provide when deleting the message. For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-queue-message-identifiers.html Queue and Message Identifiers> in the /Amazon Simple Queue Service Developer Guide/ .
-- You can provide the @VisibilityTimeout@ parameter in your request. The parameter is applied to the messages that Amazon SQS returns in the response. If you don't include the parameter, the overall visibility timeout for the queue is used for the returned messages. For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-visibility-timeout.html Visibility Timeout> in the /Amazon Simple Queue Service Developer Guide/ .
-- A message that isn't deleted or a message whose visibility isn't extended before the visibility timeout expires counts as a failed receive. Depending on the configuration of the queue, the message might be sent to the dead-letter queue.
module Network.AWS.SQS.ReceiveMessage
    (
    -- * Creating a request
      ReceiveMessage (..)
    , mkReceiveMessage
    -- ** Request lenses
    , rmQueueUrl
    , rmAttributeNames
    , rmMaxNumberOfMessages
    , rmMessageAttributeNames
    , rmReceiveRequestAttemptId
    , rmVisibilityTimeout
    , rmWaitTimeSeconds

    -- * Destructuring the response
    , ReceiveMessageResponse (..)
    , mkReceiveMessageResponse
    -- ** Response lenses
    , rmrrsMessages
    , rmrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SQS.Types as Types

-- | 
--
-- /See:/ 'mkReceiveMessage' smart constructor.
data ReceiveMessage = ReceiveMessage'
  { queueUrl :: Core.Text
    -- ^ The URL of the Amazon SQS queue from which messages are received.
--
-- Queue URLs and names are case-sensitive.
  , attributeNames :: Core.Maybe [Types.MessageAttribute]
    -- ^ A list of attributes that need to be returned along with each message. These attributes include:
--
--
--     * @All@ – Returns all values.
--
--
--     * @ApproximateFirstReceiveTimestamp@ – Returns the time the message was first received from the queue (<http://en.wikipedia.org/wiki/Unix_time epoch time> in milliseconds).
--
--
--     * @ApproximateReceiveCount@ – Returns the number of times a message has been received across all queues but not deleted.
--
--
--     * @AWSTraceHeader@ – Returns the AWS X-Ray trace header string. 
--
--
--     * @SenderId@ 
--
--     * For an IAM user, returns the IAM user ID, for example @ABCDEFGHI1JKLMNOPQ23R@ .
--
--
--     * For an IAM role, returns the IAM role ID, for example @ABCDE1F2GH3I4JK5LMNOP:i-a123b456@ .
--
--
--
--
--     * @SentTimestamp@ – Returns the time the message was sent to the queue (<http://en.wikipedia.org/wiki/Unix_time epoch time> in milliseconds).
--
--
--     * @MessageDeduplicationId@ – Returns the value provided by the producer that calls the @'SendMessage' @ action.
--
--
--     * @MessageGroupId@ – Returns the value provided by the producer that calls the @'SendMessage' @ action. Messages with the same @MessageGroupId@ are returned in sequence.
--
--
--     * @SequenceNumber@ – Returns the value provided by Amazon SQS.
--
--
  , maxNumberOfMessages :: Core.Maybe Core.Int
    -- ^ The maximum number of messages to return. Amazon SQS never returns more messages than this value (however, fewer messages might be returned). Valid values: 1 to 10. Default: 1.
  , messageAttributeNames :: Core.Maybe [Types.MessageAttributeName]
    -- ^ The name of the message attribute, where /N/ is the index.
--
--
--     * The name can contain alphanumeric characters and the underscore (@_@ ), hyphen (@-@ ), and period (@.@ ).
--
--
--     * The name is case-sensitive and must be unique among all attribute names for the message.
--
--
--     * The name must not start with AWS-reserved prefixes such as @AWS.@ or @Amazon.@ (or any casing variants).
--
--
--     * The name must not start or end with a period (@.@ ), and it should not have periods in succession (@..@ ).
--
--
--     * The name can be up to 256 characters long.
--
--
-- When using @ReceiveMessage@ , you can send a list of attribute names to receive, or you can return all of the attributes by specifying @All@ or @.*@ in your request. You can also use all message attributes starting with a prefix, for example @bar.*@ .
  , receiveRequestAttemptId :: Core.Maybe Core.Text
    -- ^ This parameter applies only to FIFO (first-in-first-out) queues.
--
-- The token used for deduplication of @ReceiveMessage@ calls. If a networking issue occurs after a @ReceiveMessage@ action, and instead of a response you receive a generic error, it is possible to retry the same action with an identical @ReceiveRequestAttemptId@ to retrieve the same set of messages, even if their visibility timeout has not yet expired.
--
--     * You can use @ReceiveRequestAttemptId@ only for 5 minutes after a @ReceiveMessage@ action.
--
--
--     * When you set @FifoQueue@ , a caller of the @ReceiveMessage@ action can provide a @ReceiveRequestAttemptId@ explicitly.
--
--
--     * If a caller of the @ReceiveMessage@ action doesn't provide a @ReceiveRequestAttemptId@ , Amazon SQS generates a @ReceiveRequestAttemptId@ .
--
--
--     * It is possible to retry the @ReceiveMessage@ action with the same @ReceiveRequestAttemptId@ if none of the messages have been modified (deleted or had their visibility changes).
--
--
--     * During a visibility timeout, subsequent calls with the same @ReceiveRequestAttemptId@ return the same messages and receipt handles. If a retry occurs within the deduplication interval, it resets the visibility timeout. For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-visibility-timeout.html Visibility Timeout> in the /Amazon Simple Queue Service Developer Guide/ .
-- /Important:/ If a caller of the @ReceiveMessage@ action still processes messages when the visibility timeout expires and messages become visible, another worker consuming from the same queue can receive the same messages and therefore process duplicates. Also, if a consumer whose message processing time is longer than the visibility timeout tries to delete the processed messages, the action fails with an error.
-- To mitigate this effect, ensure that your application observes a safe threshold before the visibility timeout expires and extend the visibility timeout as necessary.
--
--
--     * While messages with a particular @MessageGroupId@ are invisible, no more messages belonging to the same @MessageGroupId@ are returned until the visibility timeout expires. You can still receive messages with another @MessageGroupId@ as long as it is also visible.
--
--
--     * If a caller of @ReceiveMessage@ can't track the @ReceiveRequestAttemptId@ , no retries work until the original visibility timeout expires. As a result, delays might occur but the messages in the queue remain in a strict order.
--
--
-- The maximum length of @ReceiveRequestAttemptId@ is 128 characters. @ReceiveRequestAttemptId@ can contain alphanumeric characters (@a-z@ , @A-Z@ , @0-9@ ) and punctuation (@!"#$%&'()*+,-./:;<=>?@[\]^_`{|}~@ ).
-- For best practices of using @ReceiveRequestAttemptId@ , see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/using-receiverequestattemptid-request-parameter.html Using the ReceiveRequestAttemptId Request Parameter> in the /Amazon Simple Queue Service Developer Guide/ .
  , visibilityTimeout :: Core.Maybe Core.Int
    -- ^ The duration (in seconds) that the received messages are hidden from subsequent retrieve requests after being retrieved by a @ReceiveMessage@ request.
  , waitTimeSeconds :: Core.Maybe Core.Int
    -- ^ The duration (in seconds) for which the call waits for a message to arrive in the queue before returning. If a message is available, the call returns sooner than @WaitTimeSeconds@ . If no messages are available and the wait time expires, the call returns successfully with an empty list of messages.
--
-- /Important:/ To avoid HTTP errors, ensure that the HTTP response timeout for @ReceiveMessage@ requests is longer than the @WaitTimeSeconds@ parameter. For example, with the Java SDK, you can set HTTP transport settings using the <https://sdk.amazonaws.com/java/api/latest/software/amazon/awssdk/http/nio/netty/NettyNioAsyncHttpClient.html NettyNioAsyncHttpClient> for asynchronous clients, or the <https://sdk.amazonaws.com/java/api/latest/software/amazon/awssdk/http/apache/ApacheHttpClient.html ApacheHttpClient> for synchronous clients. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReceiveMessage' value with any optional fields omitted.
mkReceiveMessage
    :: Core.Text -- ^ 'queueUrl'
    -> ReceiveMessage
mkReceiveMessage queueUrl
  = ReceiveMessage'{queueUrl, attributeNames = Core.Nothing,
                    maxNumberOfMessages = Core.Nothing,
                    messageAttributeNames = Core.Nothing,
                    receiveRequestAttemptId = Core.Nothing,
                    visibilityTimeout = Core.Nothing, waitTimeSeconds = Core.Nothing}

-- | The URL of the Amazon SQS queue from which messages are received.
--
-- Queue URLs and names are case-sensitive.
--
-- /Note:/ Consider using 'queueUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmQueueUrl :: Lens.Lens' ReceiveMessage Core.Text
rmQueueUrl = Lens.field @"queueUrl"
{-# INLINEABLE rmQueueUrl #-}
{-# DEPRECATED queueUrl "Use generic-lens or generic-optics with 'queueUrl' instead"  #-}

-- | A list of attributes that need to be returned along with each message. These attributes include:
--
--
--     * @All@ – Returns all values.
--
--
--     * @ApproximateFirstReceiveTimestamp@ – Returns the time the message was first received from the queue (<http://en.wikipedia.org/wiki/Unix_time epoch time> in milliseconds).
--
--
--     * @ApproximateReceiveCount@ – Returns the number of times a message has been received across all queues but not deleted.
--
--
--     * @AWSTraceHeader@ – Returns the AWS X-Ray trace header string. 
--
--
--     * @SenderId@ 
--
--     * For an IAM user, returns the IAM user ID, for example @ABCDEFGHI1JKLMNOPQ23R@ .
--
--
--     * For an IAM role, returns the IAM role ID, for example @ABCDE1F2GH3I4JK5LMNOP:i-a123b456@ .
--
--
--
--
--     * @SentTimestamp@ – Returns the time the message was sent to the queue (<http://en.wikipedia.org/wiki/Unix_time epoch time> in milliseconds).
--
--
--     * @MessageDeduplicationId@ – Returns the value provided by the producer that calls the @'SendMessage' @ action.
--
--
--     * @MessageGroupId@ – Returns the value provided by the producer that calls the @'SendMessage' @ action. Messages with the same @MessageGroupId@ are returned in sequence.
--
--
--     * @SequenceNumber@ – Returns the value provided by Amazon SQS.
--
--
--
-- /Note:/ Consider using 'attributeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmAttributeNames :: Lens.Lens' ReceiveMessage (Core.Maybe [Types.MessageAttribute])
rmAttributeNames = Lens.field @"attributeNames"
{-# INLINEABLE rmAttributeNames #-}
{-# DEPRECATED attributeNames "Use generic-lens or generic-optics with 'attributeNames' instead"  #-}

-- | The maximum number of messages to return. Amazon SQS never returns more messages than this value (however, fewer messages might be returned). Valid values: 1 to 10. Default: 1.
--
-- /Note:/ Consider using 'maxNumberOfMessages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmMaxNumberOfMessages :: Lens.Lens' ReceiveMessage (Core.Maybe Core.Int)
rmMaxNumberOfMessages = Lens.field @"maxNumberOfMessages"
{-# INLINEABLE rmMaxNumberOfMessages #-}
{-# DEPRECATED maxNumberOfMessages "Use generic-lens or generic-optics with 'maxNumberOfMessages' instead"  #-}

-- | The name of the message attribute, where /N/ is the index.
--
--
--     * The name can contain alphanumeric characters and the underscore (@_@ ), hyphen (@-@ ), and period (@.@ ).
--
--
--     * The name is case-sensitive and must be unique among all attribute names for the message.
--
--
--     * The name must not start with AWS-reserved prefixes such as @AWS.@ or @Amazon.@ (or any casing variants).
--
--
--     * The name must not start or end with a period (@.@ ), and it should not have periods in succession (@..@ ).
--
--
--     * The name can be up to 256 characters long.
--
--
-- When using @ReceiveMessage@ , you can send a list of attribute names to receive, or you can return all of the attributes by specifying @All@ or @.*@ in your request. You can also use all message attributes starting with a prefix, for example @bar.*@ .
--
-- /Note:/ Consider using 'messageAttributeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmMessageAttributeNames :: Lens.Lens' ReceiveMessage (Core.Maybe [Types.MessageAttributeName])
rmMessageAttributeNames = Lens.field @"messageAttributeNames"
{-# INLINEABLE rmMessageAttributeNames #-}
{-# DEPRECATED messageAttributeNames "Use generic-lens or generic-optics with 'messageAttributeNames' instead"  #-}

-- | This parameter applies only to FIFO (first-in-first-out) queues.
--
-- The token used for deduplication of @ReceiveMessage@ calls. If a networking issue occurs after a @ReceiveMessage@ action, and instead of a response you receive a generic error, it is possible to retry the same action with an identical @ReceiveRequestAttemptId@ to retrieve the same set of messages, even if their visibility timeout has not yet expired.
--
--     * You can use @ReceiveRequestAttemptId@ only for 5 minutes after a @ReceiveMessage@ action.
--
--
--     * When you set @FifoQueue@ , a caller of the @ReceiveMessage@ action can provide a @ReceiveRequestAttemptId@ explicitly.
--
--
--     * If a caller of the @ReceiveMessage@ action doesn't provide a @ReceiveRequestAttemptId@ , Amazon SQS generates a @ReceiveRequestAttemptId@ .
--
--
--     * It is possible to retry the @ReceiveMessage@ action with the same @ReceiveRequestAttemptId@ if none of the messages have been modified (deleted or had their visibility changes).
--
--
--     * During a visibility timeout, subsequent calls with the same @ReceiveRequestAttemptId@ return the same messages and receipt handles. If a retry occurs within the deduplication interval, it resets the visibility timeout. For more information, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-visibility-timeout.html Visibility Timeout> in the /Amazon Simple Queue Service Developer Guide/ .
-- /Important:/ If a caller of the @ReceiveMessage@ action still processes messages when the visibility timeout expires and messages become visible, another worker consuming from the same queue can receive the same messages and therefore process duplicates. Also, if a consumer whose message processing time is longer than the visibility timeout tries to delete the processed messages, the action fails with an error.
-- To mitigate this effect, ensure that your application observes a safe threshold before the visibility timeout expires and extend the visibility timeout as necessary.
--
--
--     * While messages with a particular @MessageGroupId@ are invisible, no more messages belonging to the same @MessageGroupId@ are returned until the visibility timeout expires. You can still receive messages with another @MessageGroupId@ as long as it is also visible.
--
--
--     * If a caller of @ReceiveMessage@ can't track the @ReceiveRequestAttemptId@ , no retries work until the original visibility timeout expires. As a result, delays might occur but the messages in the queue remain in a strict order.
--
--
-- The maximum length of @ReceiveRequestAttemptId@ is 128 characters. @ReceiveRequestAttemptId@ can contain alphanumeric characters (@a-z@ , @A-Z@ , @0-9@ ) and punctuation (@!"#$%&'()*+,-./:;<=>?@[\]^_`{|}~@ ).
-- For best practices of using @ReceiveRequestAttemptId@ , see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/using-receiverequestattemptid-request-parameter.html Using the ReceiveRequestAttemptId Request Parameter> in the /Amazon Simple Queue Service Developer Guide/ .
--
-- /Note:/ Consider using 'receiveRequestAttemptId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmReceiveRequestAttemptId :: Lens.Lens' ReceiveMessage (Core.Maybe Core.Text)
rmReceiveRequestAttemptId = Lens.field @"receiveRequestAttemptId"
{-# INLINEABLE rmReceiveRequestAttemptId #-}
{-# DEPRECATED receiveRequestAttemptId "Use generic-lens or generic-optics with 'receiveRequestAttemptId' instead"  #-}

-- | The duration (in seconds) that the received messages are hidden from subsequent retrieve requests after being retrieved by a @ReceiveMessage@ request.
--
-- /Note:/ Consider using 'visibilityTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmVisibilityTimeout :: Lens.Lens' ReceiveMessage (Core.Maybe Core.Int)
rmVisibilityTimeout = Lens.field @"visibilityTimeout"
{-# INLINEABLE rmVisibilityTimeout #-}
{-# DEPRECATED visibilityTimeout "Use generic-lens or generic-optics with 'visibilityTimeout' instead"  #-}

-- | The duration (in seconds) for which the call waits for a message to arrive in the queue before returning. If a message is available, the call returns sooner than @WaitTimeSeconds@ . If no messages are available and the wait time expires, the call returns successfully with an empty list of messages.
--
-- /Important:/ To avoid HTTP errors, ensure that the HTTP response timeout for @ReceiveMessage@ requests is longer than the @WaitTimeSeconds@ parameter. For example, with the Java SDK, you can set HTTP transport settings using the <https://sdk.amazonaws.com/java/api/latest/software/amazon/awssdk/http/nio/netty/NettyNioAsyncHttpClient.html NettyNioAsyncHttpClient> for asynchronous clients, or the <https://sdk.amazonaws.com/java/api/latest/software/amazon/awssdk/http/apache/ApacheHttpClient.html ApacheHttpClient> for synchronous clients. 
--
-- /Note:/ Consider using 'waitTimeSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmWaitTimeSeconds :: Lens.Lens' ReceiveMessage (Core.Maybe Core.Int)
rmWaitTimeSeconds = Lens.field @"waitTimeSeconds"
{-# INLINEABLE rmWaitTimeSeconds #-}
{-# DEPRECATED waitTimeSeconds "Use generic-lens or generic-optics with 'waitTimeSeconds' instead"  #-}

instance Core.ToQuery ReceiveMessage where
        toQuery ReceiveMessage{..}
          = Core.toQueryPair "Action" ("ReceiveMessage" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2012-11-05" :: Core.Text)
              Core.<> Core.toQueryPair "QueueUrl" queueUrl
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "AttributeName")
                attributeNames
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxNumberOfMessages")
                maxNumberOfMessages
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "MessageAttributeName")
                messageAttributeNames
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ReceiveRequestAttemptId")
                receiveRequestAttemptId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "VisibilityTimeout")
                visibilityTimeout
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "WaitTimeSeconds")
                waitTimeSeconds

instance Core.ToHeaders ReceiveMessage where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ReceiveMessage where
        type Rs ReceiveMessage = ReceiveMessageResponse
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
          = Response.receiveXMLWrapper "ReceiveMessageResult"
              (\ s h x ->
                 ReceiveMessageResponse' Core.<$>
                   (x Core..@? "Message") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | A list of received messages.
--
-- /See:/ 'mkReceiveMessageResponse' smart constructor.
data ReceiveMessageResponse = ReceiveMessageResponse'
  { messages :: Core.Maybe [Types.Message]
    -- ^ A list of messages.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReceiveMessageResponse' value with any optional fields omitted.
mkReceiveMessageResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ReceiveMessageResponse
mkReceiveMessageResponse responseStatus
  = ReceiveMessageResponse'{messages = Core.Nothing, responseStatus}

-- | A list of messages.
--
-- /Note:/ Consider using 'messages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmrrsMessages :: Lens.Lens' ReceiveMessageResponse (Core.Maybe [Types.Message])
rmrrsMessages = Lens.field @"messages"
{-# INLINEABLE rmrrsMessages #-}
{-# DEPRECATED messages "Use generic-lens or generic-optics with 'messages' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmrrsResponseStatus :: Lens.Lens' ReceiveMessageResponse Core.Int
rmrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rmrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
