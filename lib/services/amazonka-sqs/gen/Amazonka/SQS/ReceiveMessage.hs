{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SQS.ReceiveMessage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves one or more messages (up to 10), from the specified queue.
-- Using the @WaitTimeSeconds@ parameter enables long-poll support. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-long-polling.html Amazon SQS Long Polling>
-- in the /Amazon SQS Developer Guide/.
--
-- Short poll is the default behavior where a weighted random set of
-- machines is sampled on a @ReceiveMessage@ call. Thus, only the messages
-- on the sampled machines are returned. If the number of messages in the
-- queue is small (fewer than 1,000), you most likely get fewer messages
-- than you requested per @ReceiveMessage@ call. If the number of messages
-- in the queue is extremely small, you might not receive any messages in a
-- particular @ReceiveMessage@ response. If this happens, repeat the
-- request.
--
-- For each message returned, the response includes the following:
--
-- -   The message body.
--
-- -   An MD5 digest of the message body. For information about MD5, see
--     <https://www.ietf.org/rfc/rfc1321.txt RFC1321>.
--
-- -   The @MessageId@ you received when you sent the message to the queue.
--
-- -   The receipt handle.
--
-- -   The message attributes.
--
-- -   An MD5 digest of the message attributes.
--
-- The receipt handle is the identifier you must provide when deleting the
-- message. For more information, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-queue-message-identifiers.html Queue and Message Identifiers>
-- in the /Amazon SQS Developer Guide/.
--
-- You can provide the @VisibilityTimeout@ parameter in your request. The
-- parameter is applied to the messages that Amazon SQS returns in the
-- response. If you don\'t include the parameter, the overall visibility
-- timeout for the queue is used for the returned messages. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-visibility-timeout.html Visibility Timeout>
-- in the /Amazon SQS Developer Guide/.
--
-- A message that isn\'t deleted or a message whose visibility isn\'t
-- extended before the visibility timeout expires counts as a failed
-- receive. Depending on the configuration of the queue, the message might
-- be sent to the dead-letter queue.
--
-- In the future, new attributes might be added. If you write code that
-- calls this action, we recommend that you structure your code so that it
-- can handle new attributes gracefully.
module Amazonka.SQS.ReceiveMessage
  ( -- * Creating a Request
    ReceiveMessage (..),
    newReceiveMessage,

    -- * Request Lenses
    receiveMessage_attributeNames,
    receiveMessage_maxNumberOfMessages,
    receiveMessage_messageAttributeNames,
    receiveMessage_receiveRequestAttemptId,
    receiveMessage_visibilityTimeout,
    receiveMessage_waitTimeSeconds,
    receiveMessage_queueUrl,

    -- * Destructuring the Response
    ReceiveMessageResponse (..),
    newReceiveMessageResponse,

    -- * Response Lenses
    receiveMessageResponse_messages,
    receiveMessageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SQS.Types

-- |
--
-- /See:/ 'newReceiveMessage' smart constructor.
data ReceiveMessage = ReceiveMessage'
  { -- | A list of attributes that need to be returned along with each message.
    -- These attributes include:
    --
    -- -   @All@ – Returns all values.
    --
    -- -   @ApproximateFirstReceiveTimestamp@ – Returns the time the message
    --     was first received from the queue
    --     (<http://en.wikipedia.org/wiki/Unix_time epoch time> in
    --     milliseconds).
    --
    -- -   @ApproximateReceiveCount@ – Returns the number of times a message
    --     has been received across all queues but not deleted.
    --
    -- -   @AWSTraceHeader@ – Returns the X-Ray trace header string.
    --
    -- -   @SenderId@
    --
    --     -   For an IAM user, returns the IAM user ID, for example
    --         @ABCDEFGHI1JKLMNOPQ23R@.
    --
    --     -   For an IAM role, returns the IAM role ID, for example
    --         @ABCDE1F2GH3I4JK5LMNOP:i-a123b456@.
    --
    -- -   @SentTimestamp@ – Returns the time the message was sent to the queue
    --     (<http://en.wikipedia.org/wiki/Unix_time epoch time> in
    --     milliseconds).
    --
    -- -   @SqsManagedSseEnabled@ – Enables server-side queue encryption using
    --     SQS owned encryption keys. Only one server-side encryption option is
    --     supported per queue (e.g.
    --     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-configure-sse-existing-queue.html SSE-KMS>
    --     or
    --     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-configure-sqs-sse-queue.html SSE-SQS>).
    --
    -- -   @MessageDeduplicationId@ – Returns the value provided by the
    --     producer that calls the @ @@SendMessage@@ @ action.
    --
    -- -   @MessageGroupId@ – Returns the value provided by the producer that
    --     calls the @ @@SendMessage@@ @ action. Messages with the same
    --     @MessageGroupId@ are returned in sequence.
    --
    -- -   @SequenceNumber@ – Returns the value provided by Amazon SQS.
    attributeNames :: Prelude.Maybe [MessageAttribute],
    -- | The maximum number of messages to return. Amazon SQS never returns more
    -- messages than this value (however, fewer messages might be returned).
    -- Valid values: 1 to 10. Default: 1.
    maxNumberOfMessages :: Prelude.Maybe Prelude.Int,
    -- | The name of the message attribute, where /N/ is the index.
    --
    -- -   The name can contain alphanumeric characters and the underscore
    --     (@_@), hyphen (@-@), and period (@.@).
    --
    -- -   The name is case-sensitive and must be unique among all attribute
    --     names for the message.
    --
    -- -   The name must not start with AWS-reserved prefixes such as @AWS.@ or
    --     @Amazon.@ (or any casing variants).
    --
    -- -   The name must not start or end with a period (@.@), and it should
    --     not have periods in succession (@..@).
    --
    -- -   The name can be up to 256 characters long.
    --
    -- When using @ReceiveMessage@, you can send a list of attribute names to
    -- receive, or you can return all of the attributes by specifying @All@ or
    -- @.*@ in your request. You can also use all message attributes starting
    -- with a prefix, for example @bar.*@.
    messageAttributeNames :: Prelude.Maybe [Prelude.Text],
    -- | This parameter applies only to FIFO (first-in-first-out) queues.
    --
    -- The token used for deduplication of @ReceiveMessage@ calls. If a
    -- networking issue occurs after a @ReceiveMessage@ action, and instead of
    -- a response you receive a generic error, it is possible to retry the same
    -- action with an identical @ReceiveRequestAttemptId@ to retrieve the same
    -- set of messages, even if their visibility timeout has not yet expired.
    --
    -- -   You can use @ReceiveRequestAttemptId@ only for 5 minutes after a
    --     @ReceiveMessage@ action.
    --
    -- -   When you set @FifoQueue@, a caller of the @ReceiveMessage@ action
    --     can provide a @ReceiveRequestAttemptId@ explicitly.
    --
    -- -   If a caller of the @ReceiveMessage@ action doesn\'t provide a
    --     @ReceiveRequestAttemptId@, Amazon SQS generates a
    --     @ReceiveRequestAttemptId@.
    --
    -- -   It is possible to retry the @ReceiveMessage@ action with the same
    --     @ReceiveRequestAttemptId@ if none of the messages have been modified
    --     (deleted or had their visibility changes).
    --
    -- -   During a visibility timeout, subsequent calls with the same
    --     @ReceiveRequestAttemptId@ return the same messages and receipt
    --     handles. If a retry occurs within the deduplication interval, it
    --     resets the visibility timeout. For more information, see
    --     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-visibility-timeout.html Visibility Timeout>
    --     in the /Amazon SQS Developer Guide/.
    --
    --     If a caller of the @ReceiveMessage@ action still processes messages
    --     when the visibility timeout expires and messages become visible,
    --     another worker consuming from the same queue can receive the same
    --     messages and therefore process duplicates. Also, if a consumer whose
    --     message processing time is longer than the visibility timeout tries
    --     to delete the processed messages, the action fails with an error.
    --
    --     To mitigate this effect, ensure that your application observes a
    --     safe threshold before the visibility timeout expires and extend the
    --     visibility timeout as necessary.
    --
    -- -   While messages with a particular @MessageGroupId@ are invisible, no
    --     more messages belonging to the same @MessageGroupId@ are returned
    --     until the visibility timeout expires. You can still receive messages
    --     with another @MessageGroupId@ as long as it is also visible.
    --
    -- -   If a caller of @ReceiveMessage@ can\'t track the
    --     @ReceiveRequestAttemptId@, no retries work until the original
    --     visibility timeout expires. As a result, delays might occur but the
    --     messages in the queue remain in a strict order.
    --
    -- The maximum length of @ReceiveRequestAttemptId@ is 128 characters.
    -- @ReceiveRequestAttemptId@ can contain alphanumeric characters (@a-z@,
    -- @A-Z@, @0-9@) and punctuation
    -- (@!\"#$%&\'()*+,-.\/:;\<=>?\@[\\]^_\`{|}~@).
    --
    -- For best practices of using @ReceiveRequestAttemptId@, see
    -- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/using-receiverequestattemptid-request-parameter.html Using the ReceiveRequestAttemptId Request Parameter>
    -- in the /Amazon SQS Developer Guide/.
    receiveRequestAttemptId :: Prelude.Maybe Prelude.Text,
    -- | The duration (in seconds) that the received messages are hidden from
    -- subsequent retrieve requests after being retrieved by a @ReceiveMessage@
    -- request.
    visibilityTimeout :: Prelude.Maybe Prelude.Int,
    -- | The duration (in seconds) for which the call waits for a message to
    -- arrive in the queue before returning. If a message is available, the
    -- call returns sooner than @WaitTimeSeconds@. If no messages are available
    -- and the wait time expires, the call returns successfully with an empty
    -- list of messages.
    --
    -- To avoid HTTP errors, ensure that the HTTP response timeout for
    -- @ReceiveMessage@ requests is longer than the @WaitTimeSeconds@
    -- parameter. For example, with the Java SDK, you can set HTTP transport
    -- settings using the
    -- <https://sdk.amazonaws.com/java/api/latest/software/amazon/awssdk/http/nio/netty/NettyNioAsyncHttpClient.html NettyNioAsyncHttpClient>
    -- for asynchronous clients, or the
    -- <https://sdk.amazonaws.com/java/api/latest/software/amazon/awssdk/http/apache/ApacheHttpClient.html ApacheHttpClient>
    -- for synchronous clients.
    waitTimeSeconds :: Prelude.Maybe Prelude.Int,
    -- | The URL of the Amazon SQS queue from which messages are received.
    --
    -- Queue URLs and names are case-sensitive.
    queueUrl :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReceiveMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeNames', 'receiveMessage_attributeNames' - A list of attributes that need to be returned along with each message.
-- These attributes include:
--
-- -   @All@ – Returns all values.
--
-- -   @ApproximateFirstReceiveTimestamp@ – Returns the time the message
--     was first received from the queue
--     (<http://en.wikipedia.org/wiki/Unix_time epoch time> in
--     milliseconds).
--
-- -   @ApproximateReceiveCount@ – Returns the number of times a message
--     has been received across all queues but not deleted.
--
-- -   @AWSTraceHeader@ – Returns the X-Ray trace header string.
--
-- -   @SenderId@
--
--     -   For an IAM user, returns the IAM user ID, for example
--         @ABCDEFGHI1JKLMNOPQ23R@.
--
--     -   For an IAM role, returns the IAM role ID, for example
--         @ABCDE1F2GH3I4JK5LMNOP:i-a123b456@.
--
-- -   @SentTimestamp@ – Returns the time the message was sent to the queue
--     (<http://en.wikipedia.org/wiki/Unix_time epoch time> in
--     milliseconds).
--
-- -   @SqsManagedSseEnabled@ – Enables server-side queue encryption using
--     SQS owned encryption keys. Only one server-side encryption option is
--     supported per queue (e.g.
--     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-configure-sse-existing-queue.html SSE-KMS>
--     or
--     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-configure-sqs-sse-queue.html SSE-SQS>).
--
-- -   @MessageDeduplicationId@ – Returns the value provided by the
--     producer that calls the @ @@SendMessage@@ @ action.
--
-- -   @MessageGroupId@ – Returns the value provided by the producer that
--     calls the @ @@SendMessage@@ @ action. Messages with the same
--     @MessageGroupId@ are returned in sequence.
--
-- -   @SequenceNumber@ – Returns the value provided by Amazon SQS.
--
-- 'maxNumberOfMessages', 'receiveMessage_maxNumberOfMessages' - The maximum number of messages to return. Amazon SQS never returns more
-- messages than this value (however, fewer messages might be returned).
-- Valid values: 1 to 10. Default: 1.
--
-- 'messageAttributeNames', 'receiveMessage_messageAttributeNames' - The name of the message attribute, where /N/ is the index.
--
-- -   The name can contain alphanumeric characters and the underscore
--     (@_@), hyphen (@-@), and period (@.@).
--
-- -   The name is case-sensitive and must be unique among all attribute
--     names for the message.
--
-- -   The name must not start with AWS-reserved prefixes such as @AWS.@ or
--     @Amazon.@ (or any casing variants).
--
-- -   The name must not start or end with a period (@.@), and it should
--     not have periods in succession (@..@).
--
-- -   The name can be up to 256 characters long.
--
-- When using @ReceiveMessage@, you can send a list of attribute names to
-- receive, or you can return all of the attributes by specifying @All@ or
-- @.*@ in your request. You can also use all message attributes starting
-- with a prefix, for example @bar.*@.
--
-- 'receiveRequestAttemptId', 'receiveMessage_receiveRequestAttemptId' - This parameter applies only to FIFO (first-in-first-out) queues.
--
-- The token used for deduplication of @ReceiveMessage@ calls. If a
-- networking issue occurs after a @ReceiveMessage@ action, and instead of
-- a response you receive a generic error, it is possible to retry the same
-- action with an identical @ReceiveRequestAttemptId@ to retrieve the same
-- set of messages, even if their visibility timeout has not yet expired.
--
-- -   You can use @ReceiveRequestAttemptId@ only for 5 minutes after a
--     @ReceiveMessage@ action.
--
-- -   When you set @FifoQueue@, a caller of the @ReceiveMessage@ action
--     can provide a @ReceiveRequestAttemptId@ explicitly.
--
-- -   If a caller of the @ReceiveMessage@ action doesn\'t provide a
--     @ReceiveRequestAttemptId@, Amazon SQS generates a
--     @ReceiveRequestAttemptId@.
--
-- -   It is possible to retry the @ReceiveMessage@ action with the same
--     @ReceiveRequestAttemptId@ if none of the messages have been modified
--     (deleted or had their visibility changes).
--
-- -   During a visibility timeout, subsequent calls with the same
--     @ReceiveRequestAttemptId@ return the same messages and receipt
--     handles. If a retry occurs within the deduplication interval, it
--     resets the visibility timeout. For more information, see
--     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-visibility-timeout.html Visibility Timeout>
--     in the /Amazon SQS Developer Guide/.
--
--     If a caller of the @ReceiveMessage@ action still processes messages
--     when the visibility timeout expires and messages become visible,
--     another worker consuming from the same queue can receive the same
--     messages and therefore process duplicates. Also, if a consumer whose
--     message processing time is longer than the visibility timeout tries
--     to delete the processed messages, the action fails with an error.
--
--     To mitigate this effect, ensure that your application observes a
--     safe threshold before the visibility timeout expires and extend the
--     visibility timeout as necessary.
--
-- -   While messages with a particular @MessageGroupId@ are invisible, no
--     more messages belonging to the same @MessageGroupId@ are returned
--     until the visibility timeout expires. You can still receive messages
--     with another @MessageGroupId@ as long as it is also visible.
--
-- -   If a caller of @ReceiveMessage@ can\'t track the
--     @ReceiveRequestAttemptId@, no retries work until the original
--     visibility timeout expires. As a result, delays might occur but the
--     messages in the queue remain in a strict order.
--
-- The maximum length of @ReceiveRequestAttemptId@ is 128 characters.
-- @ReceiveRequestAttemptId@ can contain alphanumeric characters (@a-z@,
-- @A-Z@, @0-9@) and punctuation
-- (@!\"#$%&\'()*+,-.\/:;\<=>?\@[\\]^_\`{|}~@).
--
-- For best practices of using @ReceiveRequestAttemptId@, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/using-receiverequestattemptid-request-parameter.html Using the ReceiveRequestAttemptId Request Parameter>
-- in the /Amazon SQS Developer Guide/.
--
-- 'visibilityTimeout', 'receiveMessage_visibilityTimeout' - The duration (in seconds) that the received messages are hidden from
-- subsequent retrieve requests after being retrieved by a @ReceiveMessage@
-- request.
--
-- 'waitTimeSeconds', 'receiveMessage_waitTimeSeconds' - The duration (in seconds) for which the call waits for a message to
-- arrive in the queue before returning. If a message is available, the
-- call returns sooner than @WaitTimeSeconds@. If no messages are available
-- and the wait time expires, the call returns successfully with an empty
-- list of messages.
--
-- To avoid HTTP errors, ensure that the HTTP response timeout for
-- @ReceiveMessage@ requests is longer than the @WaitTimeSeconds@
-- parameter. For example, with the Java SDK, you can set HTTP transport
-- settings using the
-- <https://sdk.amazonaws.com/java/api/latest/software/amazon/awssdk/http/nio/netty/NettyNioAsyncHttpClient.html NettyNioAsyncHttpClient>
-- for asynchronous clients, or the
-- <https://sdk.amazonaws.com/java/api/latest/software/amazon/awssdk/http/apache/ApacheHttpClient.html ApacheHttpClient>
-- for synchronous clients.
--
-- 'queueUrl', 'receiveMessage_queueUrl' - The URL of the Amazon SQS queue from which messages are received.
--
-- Queue URLs and names are case-sensitive.
newReceiveMessage ::
  -- | 'queueUrl'
  Prelude.Text ->
  ReceiveMessage
newReceiveMessage pQueueUrl_ =
  ReceiveMessage'
    { attributeNames = Prelude.Nothing,
      maxNumberOfMessages = Prelude.Nothing,
      messageAttributeNames = Prelude.Nothing,
      receiveRequestAttemptId = Prelude.Nothing,
      visibilityTimeout = Prelude.Nothing,
      waitTimeSeconds = Prelude.Nothing,
      queueUrl = pQueueUrl_
    }

-- | A list of attributes that need to be returned along with each message.
-- These attributes include:
--
-- -   @All@ – Returns all values.
--
-- -   @ApproximateFirstReceiveTimestamp@ – Returns the time the message
--     was first received from the queue
--     (<http://en.wikipedia.org/wiki/Unix_time epoch time> in
--     milliseconds).
--
-- -   @ApproximateReceiveCount@ – Returns the number of times a message
--     has been received across all queues but not deleted.
--
-- -   @AWSTraceHeader@ – Returns the X-Ray trace header string.
--
-- -   @SenderId@
--
--     -   For an IAM user, returns the IAM user ID, for example
--         @ABCDEFGHI1JKLMNOPQ23R@.
--
--     -   For an IAM role, returns the IAM role ID, for example
--         @ABCDE1F2GH3I4JK5LMNOP:i-a123b456@.
--
-- -   @SentTimestamp@ – Returns the time the message was sent to the queue
--     (<http://en.wikipedia.org/wiki/Unix_time epoch time> in
--     milliseconds).
--
-- -   @SqsManagedSseEnabled@ – Enables server-side queue encryption using
--     SQS owned encryption keys. Only one server-side encryption option is
--     supported per queue (e.g.
--     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-configure-sse-existing-queue.html SSE-KMS>
--     or
--     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-configure-sqs-sse-queue.html SSE-SQS>).
--
-- -   @MessageDeduplicationId@ – Returns the value provided by the
--     producer that calls the @ @@SendMessage@@ @ action.
--
-- -   @MessageGroupId@ – Returns the value provided by the producer that
--     calls the @ @@SendMessage@@ @ action. Messages with the same
--     @MessageGroupId@ are returned in sequence.
--
-- -   @SequenceNumber@ – Returns the value provided by Amazon SQS.
receiveMessage_attributeNames :: Lens.Lens' ReceiveMessage (Prelude.Maybe [MessageAttribute])
receiveMessage_attributeNames = Lens.lens (\ReceiveMessage' {attributeNames} -> attributeNames) (\s@ReceiveMessage' {} a -> s {attributeNames = a} :: ReceiveMessage) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of messages to return. Amazon SQS never returns more
-- messages than this value (however, fewer messages might be returned).
-- Valid values: 1 to 10. Default: 1.
receiveMessage_maxNumberOfMessages :: Lens.Lens' ReceiveMessage (Prelude.Maybe Prelude.Int)
receiveMessage_maxNumberOfMessages = Lens.lens (\ReceiveMessage' {maxNumberOfMessages} -> maxNumberOfMessages) (\s@ReceiveMessage' {} a -> s {maxNumberOfMessages = a} :: ReceiveMessage)

-- | The name of the message attribute, where /N/ is the index.
--
-- -   The name can contain alphanumeric characters and the underscore
--     (@_@), hyphen (@-@), and period (@.@).
--
-- -   The name is case-sensitive and must be unique among all attribute
--     names for the message.
--
-- -   The name must not start with AWS-reserved prefixes such as @AWS.@ or
--     @Amazon.@ (or any casing variants).
--
-- -   The name must not start or end with a period (@.@), and it should
--     not have periods in succession (@..@).
--
-- -   The name can be up to 256 characters long.
--
-- When using @ReceiveMessage@, you can send a list of attribute names to
-- receive, or you can return all of the attributes by specifying @All@ or
-- @.*@ in your request. You can also use all message attributes starting
-- with a prefix, for example @bar.*@.
receiveMessage_messageAttributeNames :: Lens.Lens' ReceiveMessage (Prelude.Maybe [Prelude.Text])
receiveMessage_messageAttributeNames = Lens.lens (\ReceiveMessage' {messageAttributeNames} -> messageAttributeNames) (\s@ReceiveMessage' {} a -> s {messageAttributeNames = a} :: ReceiveMessage) Prelude.. Lens.mapping Lens.coerced

-- | This parameter applies only to FIFO (first-in-first-out) queues.
--
-- The token used for deduplication of @ReceiveMessage@ calls. If a
-- networking issue occurs after a @ReceiveMessage@ action, and instead of
-- a response you receive a generic error, it is possible to retry the same
-- action with an identical @ReceiveRequestAttemptId@ to retrieve the same
-- set of messages, even if their visibility timeout has not yet expired.
--
-- -   You can use @ReceiveRequestAttemptId@ only for 5 minutes after a
--     @ReceiveMessage@ action.
--
-- -   When you set @FifoQueue@, a caller of the @ReceiveMessage@ action
--     can provide a @ReceiveRequestAttemptId@ explicitly.
--
-- -   If a caller of the @ReceiveMessage@ action doesn\'t provide a
--     @ReceiveRequestAttemptId@, Amazon SQS generates a
--     @ReceiveRequestAttemptId@.
--
-- -   It is possible to retry the @ReceiveMessage@ action with the same
--     @ReceiveRequestAttemptId@ if none of the messages have been modified
--     (deleted or had their visibility changes).
--
-- -   During a visibility timeout, subsequent calls with the same
--     @ReceiveRequestAttemptId@ return the same messages and receipt
--     handles. If a retry occurs within the deduplication interval, it
--     resets the visibility timeout. For more information, see
--     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-visibility-timeout.html Visibility Timeout>
--     in the /Amazon SQS Developer Guide/.
--
--     If a caller of the @ReceiveMessage@ action still processes messages
--     when the visibility timeout expires and messages become visible,
--     another worker consuming from the same queue can receive the same
--     messages and therefore process duplicates. Also, if a consumer whose
--     message processing time is longer than the visibility timeout tries
--     to delete the processed messages, the action fails with an error.
--
--     To mitigate this effect, ensure that your application observes a
--     safe threshold before the visibility timeout expires and extend the
--     visibility timeout as necessary.
--
-- -   While messages with a particular @MessageGroupId@ are invisible, no
--     more messages belonging to the same @MessageGroupId@ are returned
--     until the visibility timeout expires. You can still receive messages
--     with another @MessageGroupId@ as long as it is also visible.
--
-- -   If a caller of @ReceiveMessage@ can\'t track the
--     @ReceiveRequestAttemptId@, no retries work until the original
--     visibility timeout expires. As a result, delays might occur but the
--     messages in the queue remain in a strict order.
--
-- The maximum length of @ReceiveRequestAttemptId@ is 128 characters.
-- @ReceiveRequestAttemptId@ can contain alphanumeric characters (@a-z@,
-- @A-Z@, @0-9@) and punctuation
-- (@!\"#$%&\'()*+,-.\/:;\<=>?\@[\\]^_\`{|}~@).
--
-- For best practices of using @ReceiveRequestAttemptId@, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/using-receiverequestattemptid-request-parameter.html Using the ReceiveRequestAttemptId Request Parameter>
-- in the /Amazon SQS Developer Guide/.
receiveMessage_receiveRequestAttemptId :: Lens.Lens' ReceiveMessage (Prelude.Maybe Prelude.Text)
receiveMessage_receiveRequestAttemptId = Lens.lens (\ReceiveMessage' {receiveRequestAttemptId} -> receiveRequestAttemptId) (\s@ReceiveMessage' {} a -> s {receiveRequestAttemptId = a} :: ReceiveMessage)

-- | The duration (in seconds) that the received messages are hidden from
-- subsequent retrieve requests after being retrieved by a @ReceiveMessage@
-- request.
receiveMessage_visibilityTimeout :: Lens.Lens' ReceiveMessage (Prelude.Maybe Prelude.Int)
receiveMessage_visibilityTimeout = Lens.lens (\ReceiveMessage' {visibilityTimeout} -> visibilityTimeout) (\s@ReceiveMessage' {} a -> s {visibilityTimeout = a} :: ReceiveMessage)

-- | The duration (in seconds) for which the call waits for a message to
-- arrive in the queue before returning. If a message is available, the
-- call returns sooner than @WaitTimeSeconds@. If no messages are available
-- and the wait time expires, the call returns successfully with an empty
-- list of messages.
--
-- To avoid HTTP errors, ensure that the HTTP response timeout for
-- @ReceiveMessage@ requests is longer than the @WaitTimeSeconds@
-- parameter. For example, with the Java SDK, you can set HTTP transport
-- settings using the
-- <https://sdk.amazonaws.com/java/api/latest/software/amazon/awssdk/http/nio/netty/NettyNioAsyncHttpClient.html NettyNioAsyncHttpClient>
-- for asynchronous clients, or the
-- <https://sdk.amazonaws.com/java/api/latest/software/amazon/awssdk/http/apache/ApacheHttpClient.html ApacheHttpClient>
-- for synchronous clients.
receiveMessage_waitTimeSeconds :: Lens.Lens' ReceiveMessage (Prelude.Maybe Prelude.Int)
receiveMessage_waitTimeSeconds = Lens.lens (\ReceiveMessage' {waitTimeSeconds} -> waitTimeSeconds) (\s@ReceiveMessage' {} a -> s {waitTimeSeconds = a} :: ReceiveMessage)

-- | The URL of the Amazon SQS queue from which messages are received.
--
-- Queue URLs and names are case-sensitive.
receiveMessage_queueUrl :: Lens.Lens' ReceiveMessage Prelude.Text
receiveMessage_queueUrl = Lens.lens (\ReceiveMessage' {queueUrl} -> queueUrl) (\s@ReceiveMessage' {} a -> s {queueUrl = a} :: ReceiveMessage)

instance Core.AWSRequest ReceiveMessage where
  type
    AWSResponse ReceiveMessage =
      ReceiveMessageResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ReceiveMessageResult"
      ( \s h x ->
          ReceiveMessageResponse'
            Prelude.<$> (Core.may (Data.parseXMLList "Message") x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ReceiveMessage where
  hashWithSalt _salt ReceiveMessage' {..} =
    _salt
      `Prelude.hashWithSalt` attributeNames
      `Prelude.hashWithSalt` maxNumberOfMessages
      `Prelude.hashWithSalt` messageAttributeNames
      `Prelude.hashWithSalt` receiveRequestAttemptId
      `Prelude.hashWithSalt` visibilityTimeout
      `Prelude.hashWithSalt` waitTimeSeconds
      `Prelude.hashWithSalt` queueUrl

instance Prelude.NFData ReceiveMessage where
  rnf ReceiveMessage' {..} =
    Prelude.rnf attributeNames
      `Prelude.seq` Prelude.rnf maxNumberOfMessages
      `Prelude.seq` Prelude.rnf messageAttributeNames
      `Prelude.seq` Prelude.rnf receiveRequestAttemptId
      `Prelude.seq` Prelude.rnf visibilityTimeout
      `Prelude.seq` Prelude.rnf waitTimeSeconds
      `Prelude.seq` Prelude.rnf queueUrl

instance Data.ToHeaders ReceiveMessage where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ReceiveMessage where
  toPath = Prelude.const "/"

instance Data.ToQuery ReceiveMessage where
  toQuery ReceiveMessage' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ReceiveMessage" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-11-05" :: Prelude.ByteString),
        Data.toQuery
          ( Data.toQueryList "AttributeName"
              Prelude.<$> attributeNames
          ),
        "MaxNumberOfMessages" Data.=: maxNumberOfMessages,
        Data.toQuery
          ( Data.toQueryList "MessageAttributeName"
              Prelude.<$> messageAttributeNames
          ),
        "ReceiveRequestAttemptId"
          Data.=: receiveRequestAttemptId,
        "VisibilityTimeout" Data.=: visibilityTimeout,
        "WaitTimeSeconds" Data.=: waitTimeSeconds,
        "QueueUrl" Data.=: queueUrl
      ]

-- | A list of received messages.
--
-- /See:/ 'newReceiveMessageResponse' smart constructor.
data ReceiveMessageResponse = ReceiveMessageResponse'
  { -- | A list of messages.
    messages :: Prelude.Maybe [Message],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReceiveMessageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'messages', 'receiveMessageResponse_messages' - A list of messages.
--
-- 'httpStatus', 'receiveMessageResponse_httpStatus' - The response's http status code.
newReceiveMessageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ReceiveMessageResponse
newReceiveMessageResponse pHttpStatus_ =
  ReceiveMessageResponse'
    { messages = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of messages.
receiveMessageResponse_messages :: Lens.Lens' ReceiveMessageResponse (Prelude.Maybe [Message])
receiveMessageResponse_messages = Lens.lens (\ReceiveMessageResponse' {messages} -> messages) (\s@ReceiveMessageResponse' {} a -> s {messages = a} :: ReceiveMessageResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
receiveMessageResponse_httpStatus :: Lens.Lens' ReceiveMessageResponse Prelude.Int
receiveMessageResponse_httpStatus = Lens.lens (\ReceiveMessageResponse' {httpStatus} -> httpStatus) (\s@ReceiveMessageResponse' {} a -> s {httpStatus = a} :: ReceiveMessageResponse)

instance Prelude.NFData ReceiveMessageResponse where
  rnf ReceiveMessageResponse' {..} =
    Prelude.rnf messages
      `Prelude.seq` Prelude.rnf httpStatus
