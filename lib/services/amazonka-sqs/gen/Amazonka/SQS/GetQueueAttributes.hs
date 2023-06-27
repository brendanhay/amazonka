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
-- Module      : Amazonka.SQS.GetQueueAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets attributes for the specified queue.
--
-- To determine whether a queue is
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html FIFO>,
-- you can check whether @QueueName@ ends with the @.fifo@ suffix.
module Amazonka.SQS.GetQueueAttributes
  ( -- * Creating a Request
    GetQueueAttributes (..),
    newGetQueueAttributes,

    -- * Request Lenses
    getQueueAttributes_attributeNames,
    getQueueAttributes_queueUrl,

    -- * Destructuring the Response
    GetQueueAttributesResponse (..),
    newGetQueueAttributesResponse,

    -- * Response Lenses
    getQueueAttributesResponse_attributes,
    getQueueAttributesResponse_httpStatus,
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
-- /See:/ 'newGetQueueAttributes' smart constructor.
data GetQueueAttributes = GetQueueAttributes'
  { -- | A list of attributes for which to retrieve information.
    --
    -- The @AttributeNames@ parameter is optional, but if you don\'t specify
    -- values for this parameter, the request returns empty results.
    --
    -- In the future, new attributes might be added. If you write code that
    -- calls this action, we recommend that you structure your code so that it
    -- can handle new attributes gracefully.
    --
    -- The following attributes are supported:
    --
    -- The @ApproximateNumberOfMessagesDelayed@,
    -- @ApproximateNumberOfMessagesNotVisible@, and
    -- @ApproximateNumberOfMessages@ metrics may not achieve consistency until
    -- at least 1 minute after the producers stop sending messages. This period
    -- is required for the queue metadata to reach eventual consistency.
    --
    -- -   @All@ – Returns all values.
    --
    -- -   @ApproximateNumberOfMessages@ – Returns the approximate number of
    --     messages available for retrieval from the queue.
    --
    -- -   @ApproximateNumberOfMessagesDelayed@ – Returns the approximate
    --     number of messages in the queue that are delayed and not available
    --     for reading immediately. This can happen when the queue is
    --     configured as a delay queue or when a message has been sent with a
    --     delay parameter.
    --
    -- -   @ApproximateNumberOfMessagesNotVisible@ – Returns the approximate
    --     number of messages that are in flight. Messages are considered to be
    --     /in flight/ if they have been sent to a client but have not yet been
    --     deleted or have not yet reached the end of their visibility window.
    --
    -- -   @CreatedTimestamp@ – Returns the time when the queue was created in
    --     seconds (<http://en.wikipedia.org/wiki/Unix_time epoch time>).
    --
    -- -   @DelaySeconds@ – Returns the default delay on the queue in seconds.
    --
    -- -   @LastModifiedTimestamp@ – Returns the time when the queue was last
    --     changed in seconds
    --     (<http://en.wikipedia.org/wiki/Unix_time epoch time>).
    --
    -- -   @MaximumMessageSize@ – Returns the limit of how many bytes a message
    --     can contain before Amazon SQS rejects it.
    --
    -- -   @MessageRetentionPeriod@ – Returns the length of time, in seconds,
    --     for which Amazon SQS retains a message. When you change a queue\'s
    --     attributes, the change can take up to 60 seconds for most of the
    --     attributes to propagate throughout the Amazon SQS system. Changes
    --     made to the @MessageRetentionPeriod@ attribute can take up to 15
    --     minutes and will impact existing messages in the queue potentially
    --     causing them to be expired and deleted if the
    --     @MessageRetentionPeriod@ is reduced below the age of existing
    --     messages.
    --
    -- -   @Policy@ – Returns the policy of the queue.
    --
    -- -   @QueueArn@ – Returns the Amazon resource name (ARN) of the queue.
    --
    -- -   @ReceiveMessageWaitTimeSeconds@ – Returns the length of time, in
    --     seconds, for which the @ReceiveMessage@ action waits for a message
    --     to arrive.
    --
    -- -   @VisibilityTimeout@ – Returns the visibility timeout for the queue.
    --     For more information about the visibility timeout, see
    --     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-visibility-timeout.html Visibility Timeout>
    --     in the /Amazon SQS Developer Guide/.
    --
    -- The following attributes apply only to
    -- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-dead-letter-queues.html dead-letter queues:>
    --
    -- -   @RedrivePolicy@ – The string that includes the parameters for the
    --     dead-letter queue functionality of the source queue as a JSON
    --     object. The parameters are as follows:
    --
    --     -   @deadLetterTargetArn@ – The Amazon Resource Name (ARN) of the
    --         dead-letter queue to which Amazon SQS moves messages after the
    --         value of @maxReceiveCount@ is exceeded.
    --
    --     -   @maxReceiveCount@ – The number of times a message is delivered
    --         to the source queue before being moved to the dead-letter queue.
    --         Default: 10. When the @ReceiveCount@ for a message exceeds the
    --         @maxReceiveCount@ for a queue, Amazon SQS moves the message to
    --         the dead-letter-queue.
    --
    -- -   @RedriveAllowPolicy@ – The string that includes the parameters for
    --     the permissions for the dead-letter queue redrive permission and
    --     which source queues can specify dead-letter queues as a JSON object.
    --     The parameters are as follows:
    --
    --     -   @redrivePermission@ – The permission type that defines which
    --         source queues can specify the current queue as the dead-letter
    --         queue. Valid values are:
    --
    --         -   @allowAll@ – (Default) Any source queues in this Amazon Web
    --             Services account in the same Region can specify this queue
    --             as the dead-letter queue.
    --
    --         -   @denyAll@ – No source queues can specify this queue as the
    --             dead-letter queue.
    --
    --         -   @byQueue@ – Only queues specified by the @sourceQueueArns@
    --             parameter can specify this queue as the dead-letter queue.
    --
    --     -   @sourceQueueArns@ – The Amazon Resource Names (ARN)s of the
    --         source queues that can specify this queue as the dead-letter
    --         queue and redrive messages. You can specify this parameter only
    --         when the @redrivePermission@ parameter is set to @byQueue@. You
    --         can specify up to 10 source queue ARNs. To allow more than 10
    --         source queues to specify dead-letter queues, set the
    --         @redrivePermission@ parameter to @allowAll@.
    --
    -- The dead-letter queue of a FIFO queue must also be a FIFO queue.
    -- Similarly, the dead-letter queue of a standard queue must also be a
    -- standard queue.
    --
    -- The following attributes apply only to
    -- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-server-side-encryption.html server-side-encryption>:
    --
    -- -   @KmsMasterKeyId@ – Returns the ID of an Amazon Web Services managed
    --     customer master key (CMK) for Amazon SQS or a custom CMK. For more
    --     information, see
    --     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-server-side-encryption.html#sqs-sse-key-terms Key Terms>.
    --
    -- -   @KmsDataKeyReusePeriodSeconds@ – Returns the length of time, in
    --     seconds, for which Amazon SQS can reuse a data key to encrypt or
    --     decrypt messages before calling KMS again. For more information, see
    --     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-server-side-encryption.html#sqs-how-does-the-data-key-reuse-period-work How Does the Data Key Reuse Period Work?>.
    --
    -- -   @SqsManagedSseEnabled@ – Returns information about whether the queue
    --     is using SSE-SQS encryption using SQS owned encryption keys. Only
    --     one server-side encryption option is supported per queue (for
    --     example,
    --     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-configure-sse-existing-queue.html SSE-KMS>
    --     or
    --     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-configure-sqs-sse-queue.html SSE-SQS>).
    --
    -- The following attributes apply only to
    -- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html FIFO (first-in-first-out) queues>:
    --
    -- -   @FifoQueue@ – Returns information about whether the queue is FIFO.
    --     For more information, see
    --     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues-understanding-logic.html FIFO queue logic>
    --     in the /Amazon SQS Developer Guide/.
    --
    --     To determine whether a queue is
    --     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html FIFO>,
    --     you can check whether @QueueName@ ends with the @.fifo@ suffix.
    --
    -- -   @ContentBasedDeduplication@ – Returns whether content-based
    --     deduplication is enabled for the queue. For more information, see
    --     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues-exactly-once-processing.html Exactly-once processing>
    --     in the /Amazon SQS Developer Guide/.
    --
    -- The following attributes apply only to
    -- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/high-throughput-fifo.html high throughput for FIFO queues>:
    --
    -- -   @DeduplicationScope@ – Specifies whether message deduplication
    --     occurs at the message group or queue level. Valid values are
    --     @messageGroup@ and @queue@.
    --
    -- -   @FifoThroughputLimit@ – Specifies whether the FIFO queue throughput
    --     quota applies to the entire queue or per message group. Valid values
    --     are @perQueue@ and @perMessageGroupId@. The @perMessageGroupId@
    --     value is allowed only when the value for @DeduplicationScope@ is
    --     @messageGroup@.
    --
    -- To enable high throughput for FIFO queues, do the following:
    --
    -- -   Set @DeduplicationScope@ to @messageGroup@.
    --
    -- -   Set @FifoThroughputLimit@ to @perMessageGroupId@.
    --
    -- If you set these attributes to anything other than the values shown for
    -- enabling high throughput, normal throughput is in effect and
    -- deduplication occurs as specified.
    --
    -- For information on throughput quotas, see
    -- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/quotas-messages.html Quotas related to messages>
    -- in the /Amazon SQS Developer Guide/.
    attributeNames :: Prelude.Maybe [QueueAttributeName],
    -- | The URL of the Amazon SQS queue whose attribute information is
    -- retrieved.
    --
    -- Queue URLs and names are case-sensitive.
    queueUrl :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetQueueAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeNames', 'getQueueAttributes_attributeNames' - A list of attributes for which to retrieve information.
--
-- The @AttributeNames@ parameter is optional, but if you don\'t specify
-- values for this parameter, the request returns empty results.
--
-- In the future, new attributes might be added. If you write code that
-- calls this action, we recommend that you structure your code so that it
-- can handle new attributes gracefully.
--
-- The following attributes are supported:
--
-- The @ApproximateNumberOfMessagesDelayed@,
-- @ApproximateNumberOfMessagesNotVisible@, and
-- @ApproximateNumberOfMessages@ metrics may not achieve consistency until
-- at least 1 minute after the producers stop sending messages. This period
-- is required for the queue metadata to reach eventual consistency.
--
-- -   @All@ – Returns all values.
--
-- -   @ApproximateNumberOfMessages@ – Returns the approximate number of
--     messages available for retrieval from the queue.
--
-- -   @ApproximateNumberOfMessagesDelayed@ – Returns the approximate
--     number of messages in the queue that are delayed and not available
--     for reading immediately. This can happen when the queue is
--     configured as a delay queue or when a message has been sent with a
--     delay parameter.
--
-- -   @ApproximateNumberOfMessagesNotVisible@ – Returns the approximate
--     number of messages that are in flight. Messages are considered to be
--     /in flight/ if they have been sent to a client but have not yet been
--     deleted or have not yet reached the end of their visibility window.
--
-- -   @CreatedTimestamp@ – Returns the time when the queue was created in
--     seconds (<http://en.wikipedia.org/wiki/Unix_time epoch time>).
--
-- -   @DelaySeconds@ – Returns the default delay on the queue in seconds.
--
-- -   @LastModifiedTimestamp@ – Returns the time when the queue was last
--     changed in seconds
--     (<http://en.wikipedia.org/wiki/Unix_time epoch time>).
--
-- -   @MaximumMessageSize@ – Returns the limit of how many bytes a message
--     can contain before Amazon SQS rejects it.
--
-- -   @MessageRetentionPeriod@ – Returns the length of time, in seconds,
--     for which Amazon SQS retains a message. When you change a queue\'s
--     attributes, the change can take up to 60 seconds for most of the
--     attributes to propagate throughout the Amazon SQS system. Changes
--     made to the @MessageRetentionPeriod@ attribute can take up to 15
--     minutes and will impact existing messages in the queue potentially
--     causing them to be expired and deleted if the
--     @MessageRetentionPeriod@ is reduced below the age of existing
--     messages.
--
-- -   @Policy@ – Returns the policy of the queue.
--
-- -   @QueueArn@ – Returns the Amazon resource name (ARN) of the queue.
--
-- -   @ReceiveMessageWaitTimeSeconds@ – Returns the length of time, in
--     seconds, for which the @ReceiveMessage@ action waits for a message
--     to arrive.
--
-- -   @VisibilityTimeout@ – Returns the visibility timeout for the queue.
--     For more information about the visibility timeout, see
--     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-visibility-timeout.html Visibility Timeout>
--     in the /Amazon SQS Developer Guide/.
--
-- The following attributes apply only to
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-dead-letter-queues.html dead-letter queues:>
--
-- -   @RedrivePolicy@ – The string that includes the parameters for the
--     dead-letter queue functionality of the source queue as a JSON
--     object. The parameters are as follows:
--
--     -   @deadLetterTargetArn@ – The Amazon Resource Name (ARN) of the
--         dead-letter queue to which Amazon SQS moves messages after the
--         value of @maxReceiveCount@ is exceeded.
--
--     -   @maxReceiveCount@ – The number of times a message is delivered
--         to the source queue before being moved to the dead-letter queue.
--         Default: 10. When the @ReceiveCount@ for a message exceeds the
--         @maxReceiveCount@ for a queue, Amazon SQS moves the message to
--         the dead-letter-queue.
--
-- -   @RedriveAllowPolicy@ – The string that includes the parameters for
--     the permissions for the dead-letter queue redrive permission and
--     which source queues can specify dead-letter queues as a JSON object.
--     The parameters are as follows:
--
--     -   @redrivePermission@ – The permission type that defines which
--         source queues can specify the current queue as the dead-letter
--         queue. Valid values are:
--
--         -   @allowAll@ – (Default) Any source queues in this Amazon Web
--             Services account in the same Region can specify this queue
--             as the dead-letter queue.
--
--         -   @denyAll@ – No source queues can specify this queue as the
--             dead-letter queue.
--
--         -   @byQueue@ – Only queues specified by the @sourceQueueArns@
--             parameter can specify this queue as the dead-letter queue.
--
--     -   @sourceQueueArns@ – The Amazon Resource Names (ARN)s of the
--         source queues that can specify this queue as the dead-letter
--         queue and redrive messages. You can specify this parameter only
--         when the @redrivePermission@ parameter is set to @byQueue@. You
--         can specify up to 10 source queue ARNs. To allow more than 10
--         source queues to specify dead-letter queues, set the
--         @redrivePermission@ parameter to @allowAll@.
--
-- The dead-letter queue of a FIFO queue must also be a FIFO queue.
-- Similarly, the dead-letter queue of a standard queue must also be a
-- standard queue.
--
-- The following attributes apply only to
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-server-side-encryption.html server-side-encryption>:
--
-- -   @KmsMasterKeyId@ – Returns the ID of an Amazon Web Services managed
--     customer master key (CMK) for Amazon SQS or a custom CMK. For more
--     information, see
--     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-server-side-encryption.html#sqs-sse-key-terms Key Terms>.
--
-- -   @KmsDataKeyReusePeriodSeconds@ – Returns the length of time, in
--     seconds, for which Amazon SQS can reuse a data key to encrypt or
--     decrypt messages before calling KMS again. For more information, see
--     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-server-side-encryption.html#sqs-how-does-the-data-key-reuse-period-work How Does the Data Key Reuse Period Work?>.
--
-- -   @SqsManagedSseEnabled@ – Returns information about whether the queue
--     is using SSE-SQS encryption using SQS owned encryption keys. Only
--     one server-side encryption option is supported per queue (for
--     example,
--     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-configure-sse-existing-queue.html SSE-KMS>
--     or
--     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-configure-sqs-sse-queue.html SSE-SQS>).
--
-- The following attributes apply only to
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html FIFO (first-in-first-out) queues>:
--
-- -   @FifoQueue@ – Returns information about whether the queue is FIFO.
--     For more information, see
--     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues-understanding-logic.html FIFO queue logic>
--     in the /Amazon SQS Developer Guide/.
--
--     To determine whether a queue is
--     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html FIFO>,
--     you can check whether @QueueName@ ends with the @.fifo@ suffix.
--
-- -   @ContentBasedDeduplication@ – Returns whether content-based
--     deduplication is enabled for the queue. For more information, see
--     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues-exactly-once-processing.html Exactly-once processing>
--     in the /Amazon SQS Developer Guide/.
--
-- The following attributes apply only to
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/high-throughput-fifo.html high throughput for FIFO queues>:
--
-- -   @DeduplicationScope@ – Specifies whether message deduplication
--     occurs at the message group or queue level. Valid values are
--     @messageGroup@ and @queue@.
--
-- -   @FifoThroughputLimit@ – Specifies whether the FIFO queue throughput
--     quota applies to the entire queue or per message group. Valid values
--     are @perQueue@ and @perMessageGroupId@. The @perMessageGroupId@
--     value is allowed only when the value for @DeduplicationScope@ is
--     @messageGroup@.
--
-- To enable high throughput for FIFO queues, do the following:
--
-- -   Set @DeduplicationScope@ to @messageGroup@.
--
-- -   Set @FifoThroughputLimit@ to @perMessageGroupId@.
--
-- If you set these attributes to anything other than the values shown for
-- enabling high throughput, normal throughput is in effect and
-- deduplication occurs as specified.
--
-- For information on throughput quotas, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/quotas-messages.html Quotas related to messages>
-- in the /Amazon SQS Developer Guide/.
--
-- 'queueUrl', 'getQueueAttributes_queueUrl' - The URL of the Amazon SQS queue whose attribute information is
-- retrieved.
--
-- Queue URLs and names are case-sensitive.
newGetQueueAttributes ::
  -- | 'queueUrl'
  Prelude.Text ->
  GetQueueAttributes
newGetQueueAttributes pQueueUrl_ =
  GetQueueAttributes'
    { attributeNames =
        Prelude.Nothing,
      queueUrl = pQueueUrl_
    }

-- | A list of attributes for which to retrieve information.
--
-- The @AttributeNames@ parameter is optional, but if you don\'t specify
-- values for this parameter, the request returns empty results.
--
-- In the future, new attributes might be added. If you write code that
-- calls this action, we recommend that you structure your code so that it
-- can handle new attributes gracefully.
--
-- The following attributes are supported:
--
-- The @ApproximateNumberOfMessagesDelayed@,
-- @ApproximateNumberOfMessagesNotVisible@, and
-- @ApproximateNumberOfMessages@ metrics may not achieve consistency until
-- at least 1 minute after the producers stop sending messages. This period
-- is required for the queue metadata to reach eventual consistency.
--
-- -   @All@ – Returns all values.
--
-- -   @ApproximateNumberOfMessages@ – Returns the approximate number of
--     messages available for retrieval from the queue.
--
-- -   @ApproximateNumberOfMessagesDelayed@ – Returns the approximate
--     number of messages in the queue that are delayed and not available
--     for reading immediately. This can happen when the queue is
--     configured as a delay queue or when a message has been sent with a
--     delay parameter.
--
-- -   @ApproximateNumberOfMessagesNotVisible@ – Returns the approximate
--     number of messages that are in flight. Messages are considered to be
--     /in flight/ if they have been sent to a client but have not yet been
--     deleted or have not yet reached the end of their visibility window.
--
-- -   @CreatedTimestamp@ – Returns the time when the queue was created in
--     seconds (<http://en.wikipedia.org/wiki/Unix_time epoch time>).
--
-- -   @DelaySeconds@ – Returns the default delay on the queue in seconds.
--
-- -   @LastModifiedTimestamp@ – Returns the time when the queue was last
--     changed in seconds
--     (<http://en.wikipedia.org/wiki/Unix_time epoch time>).
--
-- -   @MaximumMessageSize@ – Returns the limit of how many bytes a message
--     can contain before Amazon SQS rejects it.
--
-- -   @MessageRetentionPeriod@ – Returns the length of time, in seconds,
--     for which Amazon SQS retains a message. When you change a queue\'s
--     attributes, the change can take up to 60 seconds for most of the
--     attributes to propagate throughout the Amazon SQS system. Changes
--     made to the @MessageRetentionPeriod@ attribute can take up to 15
--     minutes and will impact existing messages in the queue potentially
--     causing them to be expired and deleted if the
--     @MessageRetentionPeriod@ is reduced below the age of existing
--     messages.
--
-- -   @Policy@ – Returns the policy of the queue.
--
-- -   @QueueArn@ – Returns the Amazon resource name (ARN) of the queue.
--
-- -   @ReceiveMessageWaitTimeSeconds@ – Returns the length of time, in
--     seconds, for which the @ReceiveMessage@ action waits for a message
--     to arrive.
--
-- -   @VisibilityTimeout@ – Returns the visibility timeout for the queue.
--     For more information about the visibility timeout, see
--     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-visibility-timeout.html Visibility Timeout>
--     in the /Amazon SQS Developer Guide/.
--
-- The following attributes apply only to
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-dead-letter-queues.html dead-letter queues:>
--
-- -   @RedrivePolicy@ – The string that includes the parameters for the
--     dead-letter queue functionality of the source queue as a JSON
--     object. The parameters are as follows:
--
--     -   @deadLetterTargetArn@ – The Amazon Resource Name (ARN) of the
--         dead-letter queue to which Amazon SQS moves messages after the
--         value of @maxReceiveCount@ is exceeded.
--
--     -   @maxReceiveCount@ – The number of times a message is delivered
--         to the source queue before being moved to the dead-letter queue.
--         Default: 10. When the @ReceiveCount@ for a message exceeds the
--         @maxReceiveCount@ for a queue, Amazon SQS moves the message to
--         the dead-letter-queue.
--
-- -   @RedriveAllowPolicy@ – The string that includes the parameters for
--     the permissions for the dead-letter queue redrive permission and
--     which source queues can specify dead-letter queues as a JSON object.
--     The parameters are as follows:
--
--     -   @redrivePermission@ – The permission type that defines which
--         source queues can specify the current queue as the dead-letter
--         queue. Valid values are:
--
--         -   @allowAll@ – (Default) Any source queues in this Amazon Web
--             Services account in the same Region can specify this queue
--             as the dead-letter queue.
--
--         -   @denyAll@ – No source queues can specify this queue as the
--             dead-letter queue.
--
--         -   @byQueue@ – Only queues specified by the @sourceQueueArns@
--             parameter can specify this queue as the dead-letter queue.
--
--     -   @sourceQueueArns@ – The Amazon Resource Names (ARN)s of the
--         source queues that can specify this queue as the dead-letter
--         queue and redrive messages. You can specify this parameter only
--         when the @redrivePermission@ parameter is set to @byQueue@. You
--         can specify up to 10 source queue ARNs. To allow more than 10
--         source queues to specify dead-letter queues, set the
--         @redrivePermission@ parameter to @allowAll@.
--
-- The dead-letter queue of a FIFO queue must also be a FIFO queue.
-- Similarly, the dead-letter queue of a standard queue must also be a
-- standard queue.
--
-- The following attributes apply only to
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-server-side-encryption.html server-side-encryption>:
--
-- -   @KmsMasterKeyId@ – Returns the ID of an Amazon Web Services managed
--     customer master key (CMK) for Amazon SQS or a custom CMK. For more
--     information, see
--     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-server-side-encryption.html#sqs-sse-key-terms Key Terms>.
--
-- -   @KmsDataKeyReusePeriodSeconds@ – Returns the length of time, in
--     seconds, for which Amazon SQS can reuse a data key to encrypt or
--     decrypt messages before calling KMS again. For more information, see
--     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-server-side-encryption.html#sqs-how-does-the-data-key-reuse-period-work How Does the Data Key Reuse Period Work?>.
--
-- -   @SqsManagedSseEnabled@ – Returns information about whether the queue
--     is using SSE-SQS encryption using SQS owned encryption keys. Only
--     one server-side encryption option is supported per queue (for
--     example,
--     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-configure-sse-existing-queue.html SSE-KMS>
--     or
--     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-configure-sqs-sse-queue.html SSE-SQS>).
--
-- The following attributes apply only to
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html FIFO (first-in-first-out) queues>:
--
-- -   @FifoQueue@ – Returns information about whether the queue is FIFO.
--     For more information, see
--     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues-understanding-logic.html FIFO queue logic>
--     in the /Amazon SQS Developer Guide/.
--
--     To determine whether a queue is
--     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html FIFO>,
--     you can check whether @QueueName@ ends with the @.fifo@ suffix.
--
-- -   @ContentBasedDeduplication@ – Returns whether content-based
--     deduplication is enabled for the queue. For more information, see
--     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues-exactly-once-processing.html Exactly-once processing>
--     in the /Amazon SQS Developer Guide/.
--
-- The following attributes apply only to
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/high-throughput-fifo.html high throughput for FIFO queues>:
--
-- -   @DeduplicationScope@ – Specifies whether message deduplication
--     occurs at the message group or queue level. Valid values are
--     @messageGroup@ and @queue@.
--
-- -   @FifoThroughputLimit@ – Specifies whether the FIFO queue throughput
--     quota applies to the entire queue or per message group. Valid values
--     are @perQueue@ and @perMessageGroupId@. The @perMessageGroupId@
--     value is allowed only when the value for @DeduplicationScope@ is
--     @messageGroup@.
--
-- To enable high throughput for FIFO queues, do the following:
--
-- -   Set @DeduplicationScope@ to @messageGroup@.
--
-- -   Set @FifoThroughputLimit@ to @perMessageGroupId@.
--
-- If you set these attributes to anything other than the values shown for
-- enabling high throughput, normal throughput is in effect and
-- deduplication occurs as specified.
--
-- For information on throughput quotas, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/quotas-messages.html Quotas related to messages>
-- in the /Amazon SQS Developer Guide/.
getQueueAttributes_attributeNames :: Lens.Lens' GetQueueAttributes (Prelude.Maybe [QueueAttributeName])
getQueueAttributes_attributeNames = Lens.lens (\GetQueueAttributes' {attributeNames} -> attributeNames) (\s@GetQueueAttributes' {} a -> s {attributeNames = a} :: GetQueueAttributes) Prelude.. Lens.mapping Lens.coerced

-- | The URL of the Amazon SQS queue whose attribute information is
-- retrieved.
--
-- Queue URLs and names are case-sensitive.
getQueueAttributes_queueUrl :: Lens.Lens' GetQueueAttributes Prelude.Text
getQueueAttributes_queueUrl = Lens.lens (\GetQueueAttributes' {queueUrl} -> queueUrl) (\s@GetQueueAttributes' {} a -> s {queueUrl = a} :: GetQueueAttributes)

instance Core.AWSRequest GetQueueAttributes where
  type
    AWSResponse GetQueueAttributes =
      GetQueueAttributesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "GetQueueAttributesResult"
      ( \s h x ->
          GetQueueAttributesResponse'
            Prelude.<$> ( Core.may
                            (Data.parseXMLMap "Attribute" "Name" "Value")
                            x
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetQueueAttributes where
  hashWithSalt _salt GetQueueAttributes' {..} =
    _salt
      `Prelude.hashWithSalt` attributeNames
      `Prelude.hashWithSalt` queueUrl

instance Prelude.NFData GetQueueAttributes where
  rnf GetQueueAttributes' {..} =
    Prelude.rnf attributeNames
      `Prelude.seq` Prelude.rnf queueUrl

instance Data.ToHeaders GetQueueAttributes where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetQueueAttributes where
  toPath = Prelude.const "/"

instance Data.ToQuery GetQueueAttributes where
  toQuery GetQueueAttributes' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("GetQueueAttributes" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-11-05" :: Prelude.ByteString),
        Data.toQuery
          ( Data.toQueryList "AttributeName"
              Prelude.<$> attributeNames
          ),
        "QueueUrl" Data.=: queueUrl
      ]

-- | A list of returned queue attributes.
--
-- /See:/ 'newGetQueueAttributesResponse' smart constructor.
data GetQueueAttributesResponse = GetQueueAttributesResponse'
  { -- | A map of attributes to their respective values.
    attributes :: Prelude.Maybe (Prelude.HashMap QueueAttributeName Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetQueueAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'getQueueAttributesResponse_attributes' - A map of attributes to their respective values.
--
-- 'httpStatus', 'getQueueAttributesResponse_httpStatus' - The response's http status code.
newGetQueueAttributesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetQueueAttributesResponse
newGetQueueAttributesResponse pHttpStatus_ =
  GetQueueAttributesResponse'
    { attributes =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A map of attributes to their respective values.
getQueueAttributesResponse_attributes :: Lens.Lens' GetQueueAttributesResponse (Prelude.Maybe (Prelude.HashMap QueueAttributeName Prelude.Text))
getQueueAttributesResponse_attributes = Lens.lens (\GetQueueAttributesResponse' {attributes} -> attributes) (\s@GetQueueAttributesResponse' {} a -> s {attributes = a} :: GetQueueAttributesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getQueueAttributesResponse_httpStatus :: Lens.Lens' GetQueueAttributesResponse Prelude.Int
getQueueAttributesResponse_httpStatus = Lens.lens (\GetQueueAttributesResponse' {httpStatus} -> httpStatus) (\s@GetQueueAttributesResponse' {} a -> s {httpStatus = a} :: GetQueueAttributesResponse)

instance Prelude.NFData GetQueueAttributesResponse where
  rnf GetQueueAttributesResponse' {..} =
    Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf httpStatus
