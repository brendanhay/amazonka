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
-- Module      : Amazonka.SQS.CreateQueue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new standard or FIFO queue. You can pass one or more
-- attributes in the request. Keep the following in mind:
--
-- -   If you don\'t specify the @FifoQueue@ attribute, Amazon SQS creates
--     a standard queue.
--
--     You can\'t change the queue type after you create it and you can\'t
--     convert an existing standard queue into a FIFO queue. You must
--     either create a new FIFO queue for your application or delete your
--     existing standard queue and recreate it as a FIFO queue. For more
--     information, see
--     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html#FIFO-queues-moving Moving From a Standard Queue to a FIFO Queue>
--     in the /Amazon SQS Developer Guide/.
--
-- -   If you don\'t provide a value for an attribute, the queue is created
--     with the default value for the attribute.
--
-- -   If you delete a queue, you must wait at least 60 seconds before
--     creating a queue with the same name.
--
-- To successfully create a new queue, you must provide a queue name that
-- adheres to the
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/limits-queues.html limits related to queues>
-- and is unique within the scope of your queues.
--
-- After you create a queue, you must wait at least one second after the
-- queue is created to be able to use the queue.
--
-- To get the queue URL, use the @ @@GetQueueUrl@@ @ action.
-- @ @@GetQueueUrl@@ @ requires only the @QueueName@ parameter. be aware of
-- existing queue names:
--
-- -   If you provide the name of an existing queue along with the exact
--     names and values of all the queue\'s attributes, @CreateQueue@
--     returns the queue URL for the existing queue.
--
-- -   If the queue name, attribute names, or attribute values don\'t match
--     an existing queue, @CreateQueue@ returns an error.
--
-- Some actions take lists of parameters. These lists are specified using
-- the @param.n@ notation. Values of @n@ are integers starting from 1. For
-- example, a parameter list with two elements looks like this:
--
-- @&AttributeName.1=first@
--
-- @&AttributeName.2=second@
--
-- Cross-account permissions don\'t apply to this action. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-customer-managed-policy-examples.html#grant-cross-account-permissions-to-role-and-user-name Grant cross-account permissions to a role and a user name>
-- in the /Amazon SQS Developer Guide/.
module Amazonka.SQS.CreateQueue
  ( -- * Creating a Request
    CreateQueue (..),
    newCreateQueue,

    -- * Request Lenses
    createQueue_attributes,
    createQueue_tags,
    createQueue_queueName,

    -- * Destructuring the Response
    CreateQueueResponse (..),
    newCreateQueueResponse,

    -- * Response Lenses
    createQueueResponse_queueUrl,
    createQueueResponse_httpStatus,
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
-- /See:/ 'newCreateQueue' smart constructor.
data CreateQueue = CreateQueue'
  { -- | A map of attributes with their corresponding values.
    --
    -- The following lists the names, descriptions, and values of the special
    -- request parameters that the @CreateQueue@ action uses:
    --
    -- -   @DelaySeconds@ – The length of time, in seconds, for which the
    --     delivery of all messages in the queue is delayed. Valid values: An
    --     integer from 0 to 900 seconds (15 minutes). Default: 0.
    --
    -- -   @MaximumMessageSize@ – The limit of how many bytes a message can
    --     contain before Amazon SQS rejects it. Valid values: An integer from
    --     1,024 bytes (1 KiB) to 262,144 bytes (256 KiB). Default: 262,144
    --     (256 KiB).
    --
    -- -   @MessageRetentionPeriod@ – The length of time, in seconds, for which
    --     Amazon SQS retains a message. Valid values: An integer from 60
    --     seconds (1 minute) to 1,209,600 seconds (14 days). Default: 345,600
    --     (4 days).
    --
    -- -   @Policy@ – The queue\'s policy. A valid Amazon Web Services policy.
    --     For more information about policy structure, see
    --     <https://docs.aws.amazon.com/IAM/latest/UserGuide/PoliciesOverview.html Overview of Amazon Web Services IAM Policies>
    --     in the /Amazon IAM User Guide/.
    --
    -- -   @ReceiveMessageWaitTimeSeconds@ – The length of time, in seconds,
    --     for which a @ @@ReceiveMessage@@ @ action waits for a message to
    --     arrive. Valid values: An integer from 0 to 20 (seconds). Default: 0.
    --
    -- -   @RedrivePolicy@ – The string that includes the parameters for the
    --     dead-letter queue functionality of the source queue as a JSON
    --     object. For more information about the redrive policy and
    --     dead-letter queues, see
    --     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-dead-letter-queues.html Using Amazon SQS Dead-Letter Queues>
    --     in the /Amazon SQS Developer Guide/.
    --
    --     -   @deadLetterTargetArn@ – The Amazon Resource Name (ARN) of the
    --         dead-letter queue to which Amazon SQS moves messages after the
    --         value of @maxReceiveCount@ is exceeded.
    --
    --     -   @maxReceiveCount@ – The number of times a message is delivered
    --         to the source queue before being moved to the dead-letter queue.
    --         When the @ReceiveCount@ for a message exceeds the
    --         @maxReceiveCount@ for a queue, Amazon SQS moves the message to
    --         the dead-letter-queue.
    --
    --     The dead-letter queue of a FIFO queue must also be a FIFO queue.
    --     Similarly, the dead-letter queue of a standard queue must also be a
    --     standard queue.
    --
    -- -   @VisibilityTimeout@ – The visibility timeout for the queue, in
    --     seconds. Valid values: An integer from 0 to 43,200 (12 hours).
    --     Default: 30. For more information about the visibility timeout, see
    --     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-visibility-timeout.html Visibility Timeout>
    --     in the /Amazon SQS Developer Guide/.
    --
    -- The following attributes apply only to
    -- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-server-side-encryption.html server-side-encryption>:
    --
    -- -   @KmsMasterKeyId@ – The ID of an Amazon Web Services managed customer
    --     master key (CMK) for Amazon SQS or a custom CMK. For more
    --     information, see
    --     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-server-side-encryption.html#sqs-sse-key-terms Key Terms>.
    --     While the alias of the Amazon Web Services managed CMK for Amazon
    --     SQS is always @alias\/aws\/sqs@, the alias of a custom CMK can, for
    --     example, be @alias\/@/@MyAlias@/@ @. For more examples, see
    --     <https://docs.aws.amazon.com/kms/latest/APIReference/API_DescribeKey.html#API_DescribeKey_RequestParameters KeyId>
    --     in the /Key Management Service API Reference/.
    --
    -- -   @KmsDataKeyReusePeriodSeconds@ – The length of time, in seconds, for
    --     which Amazon SQS can reuse a
    --     <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#data-keys data key>
    --     to encrypt or decrypt messages before calling KMS again. An integer
    --     representing seconds, between 60 seconds (1 minute) and 86,400
    --     seconds (24 hours). Default: 300 (5 minutes). A shorter time period
    --     provides better security but results in more calls to KMS which
    --     might incur charges after Free Tier. For more information, see
    --     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-server-side-encryption.html#sqs-how-does-the-data-key-reuse-period-work How Does the Data Key Reuse Period Work?>.
    --
    -- -   @SqsManagedSseEnabled@ – Enables server-side queue encryption using
    --     SQS owned encryption keys. Only one server-side encryption option is
    --     supported per queue (e.g.
    --     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-configure-sse-existing-queue.html SSE-KMS>
    --     or
    --     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-configure-sqs-sse-queue.html SSE-SQS>).
    --
    -- The following attributes apply only to
    -- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html FIFO (first-in-first-out) queues>:
    --
    -- -   @FifoQueue@ – Designates a queue as FIFO. Valid values are @true@
    --     and @false@. If you don\'t specify the @FifoQueue@ attribute, Amazon
    --     SQS creates a standard queue. You can provide this attribute only
    --     during queue creation. You can\'t change it for an existing queue.
    --     When you set this attribute, you must also provide the
    --     @MessageGroupId@ for your messages explicitly.
    --
    --     For more information, see
    --     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues-understanding-logic.html FIFO queue logic>
    --     in the /Amazon SQS Developer Guide/.
    --
    -- -   @ContentBasedDeduplication@ – Enables content-based deduplication.
    --     Valid values are @true@ and @false@. For more information, see
    --     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues-exactly-once-processing.html Exactly-once processing>
    --     in the /Amazon SQS Developer Guide/. Note the following:
    --
    --     -   Every message must have a unique @MessageDeduplicationId@.
    --
    --         -   You may provide a @MessageDeduplicationId@ explicitly.
    --
    --         -   If you aren\'t able to provide a @MessageDeduplicationId@
    --             and you enable @ContentBasedDeduplication@ for your queue,
    --             Amazon SQS uses a SHA-256 hash to generate the
    --             @MessageDeduplicationId@ using the body of the message (but
    --             not the attributes of the message).
    --
    --         -   If you don\'t provide a @MessageDeduplicationId@ and the
    --             queue doesn\'t have @ContentBasedDeduplication@ set, the
    --             action fails with an error.
    --
    --         -   If the queue has @ContentBasedDeduplication@ set, your
    --             @MessageDeduplicationId@ overrides the generated one.
    --
    --     -   When @ContentBasedDeduplication@ is in effect, messages with
    --         identical content sent within the deduplication interval are
    --         treated as duplicates and only one copy of the message is
    --         delivered.
    --
    --     -   If you send one message with @ContentBasedDeduplication@ enabled
    --         and then another message with a @MessageDeduplicationId@ that is
    --         the same as the one generated for the first
    --         @MessageDeduplicationId@, the two messages are treated as
    --         duplicates and only one copy of the message is delivered.
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
    attributes :: Prelude.Maybe (Prelude.HashMap QueueAttributeName Prelude.Text),
    -- | Add cost allocation tags to the specified Amazon SQS queue. For an
    -- overview, see
    -- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-queue-tags.html Tagging Your Amazon SQS Queues>
    -- in the /Amazon SQS Developer Guide/.
    --
    -- When you use queue tags, keep the following guidelines in mind:
    --
    -- -   Adding more than 50 tags to a queue isn\'t recommended.
    --
    -- -   Tags don\'t have any semantic meaning. Amazon SQS interprets tags as
    --     character strings.
    --
    -- -   Tags are case-sensitive.
    --
    -- -   A new tag with a key identical to that of an existing tag overwrites
    --     the existing tag.
    --
    -- For a full list of tag restrictions, see
    -- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-limits.html#limits-queues Quotas related to queues>
    -- in the /Amazon SQS Developer Guide/.
    --
    -- To be able to tag a queue on creation, you must have the
    -- @sqs:CreateQueue@ and @sqs:TagQueue@ permissions.
    --
    -- Cross-account permissions don\'t apply to this action. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-customer-managed-policy-examples.html#grant-cross-account-permissions-to-role-and-user-name Grant cross-account permissions to a role and a user name>
    -- in the /Amazon SQS Developer Guide/.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the new queue. The following limits apply to this name:
    --
    -- -   A queue name can have up to 80 characters.
    --
    -- -   Valid values: alphanumeric characters, hyphens (@-@), and
    --     underscores (@_@).
    --
    -- -   A FIFO queue name must end with the @.fifo@ suffix.
    --
    -- Queue URLs and names are case-sensitive.
    queueName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateQueue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'createQueue_attributes' - A map of attributes with their corresponding values.
--
-- The following lists the names, descriptions, and values of the special
-- request parameters that the @CreateQueue@ action uses:
--
-- -   @DelaySeconds@ – The length of time, in seconds, for which the
--     delivery of all messages in the queue is delayed. Valid values: An
--     integer from 0 to 900 seconds (15 minutes). Default: 0.
--
-- -   @MaximumMessageSize@ – The limit of how many bytes a message can
--     contain before Amazon SQS rejects it. Valid values: An integer from
--     1,024 bytes (1 KiB) to 262,144 bytes (256 KiB). Default: 262,144
--     (256 KiB).
--
-- -   @MessageRetentionPeriod@ – The length of time, in seconds, for which
--     Amazon SQS retains a message. Valid values: An integer from 60
--     seconds (1 minute) to 1,209,600 seconds (14 days). Default: 345,600
--     (4 days).
--
-- -   @Policy@ – The queue\'s policy. A valid Amazon Web Services policy.
--     For more information about policy structure, see
--     <https://docs.aws.amazon.com/IAM/latest/UserGuide/PoliciesOverview.html Overview of Amazon Web Services IAM Policies>
--     in the /Amazon IAM User Guide/.
--
-- -   @ReceiveMessageWaitTimeSeconds@ – The length of time, in seconds,
--     for which a @ @@ReceiveMessage@@ @ action waits for a message to
--     arrive. Valid values: An integer from 0 to 20 (seconds). Default: 0.
--
-- -   @RedrivePolicy@ – The string that includes the parameters for the
--     dead-letter queue functionality of the source queue as a JSON
--     object. For more information about the redrive policy and
--     dead-letter queues, see
--     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-dead-letter-queues.html Using Amazon SQS Dead-Letter Queues>
--     in the /Amazon SQS Developer Guide/.
--
--     -   @deadLetterTargetArn@ – The Amazon Resource Name (ARN) of the
--         dead-letter queue to which Amazon SQS moves messages after the
--         value of @maxReceiveCount@ is exceeded.
--
--     -   @maxReceiveCount@ – The number of times a message is delivered
--         to the source queue before being moved to the dead-letter queue.
--         When the @ReceiveCount@ for a message exceeds the
--         @maxReceiveCount@ for a queue, Amazon SQS moves the message to
--         the dead-letter-queue.
--
--     The dead-letter queue of a FIFO queue must also be a FIFO queue.
--     Similarly, the dead-letter queue of a standard queue must also be a
--     standard queue.
--
-- -   @VisibilityTimeout@ – The visibility timeout for the queue, in
--     seconds. Valid values: An integer from 0 to 43,200 (12 hours).
--     Default: 30. For more information about the visibility timeout, see
--     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-visibility-timeout.html Visibility Timeout>
--     in the /Amazon SQS Developer Guide/.
--
-- The following attributes apply only to
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-server-side-encryption.html server-side-encryption>:
--
-- -   @KmsMasterKeyId@ – The ID of an Amazon Web Services managed customer
--     master key (CMK) for Amazon SQS or a custom CMK. For more
--     information, see
--     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-server-side-encryption.html#sqs-sse-key-terms Key Terms>.
--     While the alias of the Amazon Web Services managed CMK for Amazon
--     SQS is always @alias\/aws\/sqs@, the alias of a custom CMK can, for
--     example, be @alias\/@/@MyAlias@/@ @. For more examples, see
--     <https://docs.aws.amazon.com/kms/latest/APIReference/API_DescribeKey.html#API_DescribeKey_RequestParameters KeyId>
--     in the /Key Management Service API Reference/.
--
-- -   @KmsDataKeyReusePeriodSeconds@ – The length of time, in seconds, for
--     which Amazon SQS can reuse a
--     <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#data-keys data key>
--     to encrypt or decrypt messages before calling KMS again. An integer
--     representing seconds, between 60 seconds (1 minute) and 86,400
--     seconds (24 hours). Default: 300 (5 minutes). A shorter time period
--     provides better security but results in more calls to KMS which
--     might incur charges after Free Tier. For more information, see
--     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-server-side-encryption.html#sqs-how-does-the-data-key-reuse-period-work How Does the Data Key Reuse Period Work?>.
--
-- -   @SqsManagedSseEnabled@ – Enables server-side queue encryption using
--     SQS owned encryption keys. Only one server-side encryption option is
--     supported per queue (e.g.
--     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-configure-sse-existing-queue.html SSE-KMS>
--     or
--     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-configure-sqs-sse-queue.html SSE-SQS>).
--
-- The following attributes apply only to
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html FIFO (first-in-first-out) queues>:
--
-- -   @FifoQueue@ – Designates a queue as FIFO. Valid values are @true@
--     and @false@. If you don\'t specify the @FifoQueue@ attribute, Amazon
--     SQS creates a standard queue. You can provide this attribute only
--     during queue creation. You can\'t change it for an existing queue.
--     When you set this attribute, you must also provide the
--     @MessageGroupId@ for your messages explicitly.
--
--     For more information, see
--     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues-understanding-logic.html FIFO queue logic>
--     in the /Amazon SQS Developer Guide/.
--
-- -   @ContentBasedDeduplication@ – Enables content-based deduplication.
--     Valid values are @true@ and @false@. For more information, see
--     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues-exactly-once-processing.html Exactly-once processing>
--     in the /Amazon SQS Developer Guide/. Note the following:
--
--     -   Every message must have a unique @MessageDeduplicationId@.
--
--         -   You may provide a @MessageDeduplicationId@ explicitly.
--
--         -   If you aren\'t able to provide a @MessageDeduplicationId@
--             and you enable @ContentBasedDeduplication@ for your queue,
--             Amazon SQS uses a SHA-256 hash to generate the
--             @MessageDeduplicationId@ using the body of the message (but
--             not the attributes of the message).
--
--         -   If you don\'t provide a @MessageDeduplicationId@ and the
--             queue doesn\'t have @ContentBasedDeduplication@ set, the
--             action fails with an error.
--
--         -   If the queue has @ContentBasedDeduplication@ set, your
--             @MessageDeduplicationId@ overrides the generated one.
--
--     -   When @ContentBasedDeduplication@ is in effect, messages with
--         identical content sent within the deduplication interval are
--         treated as duplicates and only one copy of the message is
--         delivered.
--
--     -   If you send one message with @ContentBasedDeduplication@ enabled
--         and then another message with a @MessageDeduplicationId@ that is
--         the same as the one generated for the first
--         @MessageDeduplicationId@, the two messages are treated as
--         duplicates and only one copy of the message is delivered.
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
-- 'tags', 'createQueue_tags' - Add cost allocation tags to the specified Amazon SQS queue. For an
-- overview, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-queue-tags.html Tagging Your Amazon SQS Queues>
-- in the /Amazon SQS Developer Guide/.
--
-- When you use queue tags, keep the following guidelines in mind:
--
-- -   Adding more than 50 tags to a queue isn\'t recommended.
--
-- -   Tags don\'t have any semantic meaning. Amazon SQS interprets tags as
--     character strings.
--
-- -   Tags are case-sensitive.
--
-- -   A new tag with a key identical to that of an existing tag overwrites
--     the existing tag.
--
-- For a full list of tag restrictions, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-limits.html#limits-queues Quotas related to queues>
-- in the /Amazon SQS Developer Guide/.
--
-- To be able to tag a queue on creation, you must have the
-- @sqs:CreateQueue@ and @sqs:TagQueue@ permissions.
--
-- Cross-account permissions don\'t apply to this action. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-customer-managed-policy-examples.html#grant-cross-account-permissions-to-role-and-user-name Grant cross-account permissions to a role and a user name>
-- in the /Amazon SQS Developer Guide/.
--
-- 'queueName', 'createQueue_queueName' - The name of the new queue. The following limits apply to this name:
--
-- -   A queue name can have up to 80 characters.
--
-- -   Valid values: alphanumeric characters, hyphens (@-@), and
--     underscores (@_@).
--
-- -   A FIFO queue name must end with the @.fifo@ suffix.
--
-- Queue URLs and names are case-sensitive.
newCreateQueue ::
  -- | 'queueName'
  Prelude.Text ->
  CreateQueue
newCreateQueue pQueueName_ =
  CreateQueue'
    { attributes = Prelude.Nothing,
      tags = Prelude.Nothing,
      queueName = pQueueName_
    }

-- | A map of attributes with their corresponding values.
--
-- The following lists the names, descriptions, and values of the special
-- request parameters that the @CreateQueue@ action uses:
--
-- -   @DelaySeconds@ – The length of time, in seconds, for which the
--     delivery of all messages in the queue is delayed. Valid values: An
--     integer from 0 to 900 seconds (15 minutes). Default: 0.
--
-- -   @MaximumMessageSize@ – The limit of how many bytes a message can
--     contain before Amazon SQS rejects it. Valid values: An integer from
--     1,024 bytes (1 KiB) to 262,144 bytes (256 KiB). Default: 262,144
--     (256 KiB).
--
-- -   @MessageRetentionPeriod@ – The length of time, in seconds, for which
--     Amazon SQS retains a message. Valid values: An integer from 60
--     seconds (1 minute) to 1,209,600 seconds (14 days). Default: 345,600
--     (4 days).
--
-- -   @Policy@ – The queue\'s policy. A valid Amazon Web Services policy.
--     For more information about policy structure, see
--     <https://docs.aws.amazon.com/IAM/latest/UserGuide/PoliciesOverview.html Overview of Amazon Web Services IAM Policies>
--     in the /Amazon IAM User Guide/.
--
-- -   @ReceiveMessageWaitTimeSeconds@ – The length of time, in seconds,
--     for which a @ @@ReceiveMessage@@ @ action waits for a message to
--     arrive. Valid values: An integer from 0 to 20 (seconds). Default: 0.
--
-- -   @RedrivePolicy@ – The string that includes the parameters for the
--     dead-letter queue functionality of the source queue as a JSON
--     object. For more information about the redrive policy and
--     dead-letter queues, see
--     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-dead-letter-queues.html Using Amazon SQS Dead-Letter Queues>
--     in the /Amazon SQS Developer Guide/.
--
--     -   @deadLetterTargetArn@ – The Amazon Resource Name (ARN) of the
--         dead-letter queue to which Amazon SQS moves messages after the
--         value of @maxReceiveCount@ is exceeded.
--
--     -   @maxReceiveCount@ – The number of times a message is delivered
--         to the source queue before being moved to the dead-letter queue.
--         When the @ReceiveCount@ for a message exceeds the
--         @maxReceiveCount@ for a queue, Amazon SQS moves the message to
--         the dead-letter-queue.
--
--     The dead-letter queue of a FIFO queue must also be a FIFO queue.
--     Similarly, the dead-letter queue of a standard queue must also be a
--     standard queue.
--
-- -   @VisibilityTimeout@ – The visibility timeout for the queue, in
--     seconds. Valid values: An integer from 0 to 43,200 (12 hours).
--     Default: 30. For more information about the visibility timeout, see
--     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-visibility-timeout.html Visibility Timeout>
--     in the /Amazon SQS Developer Guide/.
--
-- The following attributes apply only to
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-server-side-encryption.html server-side-encryption>:
--
-- -   @KmsMasterKeyId@ – The ID of an Amazon Web Services managed customer
--     master key (CMK) for Amazon SQS or a custom CMK. For more
--     information, see
--     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-server-side-encryption.html#sqs-sse-key-terms Key Terms>.
--     While the alias of the Amazon Web Services managed CMK for Amazon
--     SQS is always @alias\/aws\/sqs@, the alias of a custom CMK can, for
--     example, be @alias\/@/@MyAlias@/@ @. For more examples, see
--     <https://docs.aws.amazon.com/kms/latest/APIReference/API_DescribeKey.html#API_DescribeKey_RequestParameters KeyId>
--     in the /Key Management Service API Reference/.
--
-- -   @KmsDataKeyReusePeriodSeconds@ – The length of time, in seconds, for
--     which Amazon SQS can reuse a
--     <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#data-keys data key>
--     to encrypt or decrypt messages before calling KMS again. An integer
--     representing seconds, between 60 seconds (1 minute) and 86,400
--     seconds (24 hours). Default: 300 (5 minutes). A shorter time period
--     provides better security but results in more calls to KMS which
--     might incur charges after Free Tier. For more information, see
--     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-server-side-encryption.html#sqs-how-does-the-data-key-reuse-period-work How Does the Data Key Reuse Period Work?>.
--
-- -   @SqsManagedSseEnabled@ – Enables server-side queue encryption using
--     SQS owned encryption keys. Only one server-side encryption option is
--     supported per queue (e.g.
--     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-configure-sse-existing-queue.html SSE-KMS>
--     or
--     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-configure-sqs-sse-queue.html SSE-SQS>).
--
-- The following attributes apply only to
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html FIFO (first-in-first-out) queues>:
--
-- -   @FifoQueue@ – Designates a queue as FIFO. Valid values are @true@
--     and @false@. If you don\'t specify the @FifoQueue@ attribute, Amazon
--     SQS creates a standard queue. You can provide this attribute only
--     during queue creation. You can\'t change it for an existing queue.
--     When you set this attribute, you must also provide the
--     @MessageGroupId@ for your messages explicitly.
--
--     For more information, see
--     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues-understanding-logic.html FIFO queue logic>
--     in the /Amazon SQS Developer Guide/.
--
-- -   @ContentBasedDeduplication@ – Enables content-based deduplication.
--     Valid values are @true@ and @false@. For more information, see
--     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues-exactly-once-processing.html Exactly-once processing>
--     in the /Amazon SQS Developer Guide/. Note the following:
--
--     -   Every message must have a unique @MessageDeduplicationId@.
--
--         -   You may provide a @MessageDeduplicationId@ explicitly.
--
--         -   If you aren\'t able to provide a @MessageDeduplicationId@
--             and you enable @ContentBasedDeduplication@ for your queue,
--             Amazon SQS uses a SHA-256 hash to generate the
--             @MessageDeduplicationId@ using the body of the message (but
--             not the attributes of the message).
--
--         -   If you don\'t provide a @MessageDeduplicationId@ and the
--             queue doesn\'t have @ContentBasedDeduplication@ set, the
--             action fails with an error.
--
--         -   If the queue has @ContentBasedDeduplication@ set, your
--             @MessageDeduplicationId@ overrides the generated one.
--
--     -   When @ContentBasedDeduplication@ is in effect, messages with
--         identical content sent within the deduplication interval are
--         treated as duplicates and only one copy of the message is
--         delivered.
--
--     -   If you send one message with @ContentBasedDeduplication@ enabled
--         and then another message with a @MessageDeduplicationId@ that is
--         the same as the one generated for the first
--         @MessageDeduplicationId@, the two messages are treated as
--         duplicates and only one copy of the message is delivered.
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
createQueue_attributes :: Lens.Lens' CreateQueue (Prelude.Maybe (Prelude.HashMap QueueAttributeName Prelude.Text))
createQueue_attributes = Lens.lens (\CreateQueue' {attributes} -> attributes) (\s@CreateQueue' {} a -> s {attributes = a} :: CreateQueue) Prelude.. Lens.mapping Lens.coerced

-- | Add cost allocation tags to the specified Amazon SQS queue. For an
-- overview, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-queue-tags.html Tagging Your Amazon SQS Queues>
-- in the /Amazon SQS Developer Guide/.
--
-- When you use queue tags, keep the following guidelines in mind:
--
-- -   Adding more than 50 tags to a queue isn\'t recommended.
--
-- -   Tags don\'t have any semantic meaning. Amazon SQS interprets tags as
--     character strings.
--
-- -   Tags are case-sensitive.
--
-- -   A new tag with a key identical to that of an existing tag overwrites
--     the existing tag.
--
-- For a full list of tag restrictions, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-limits.html#limits-queues Quotas related to queues>
-- in the /Amazon SQS Developer Guide/.
--
-- To be able to tag a queue on creation, you must have the
-- @sqs:CreateQueue@ and @sqs:TagQueue@ permissions.
--
-- Cross-account permissions don\'t apply to this action. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-customer-managed-policy-examples.html#grant-cross-account-permissions-to-role-and-user-name Grant cross-account permissions to a role and a user name>
-- in the /Amazon SQS Developer Guide/.
createQueue_tags :: Lens.Lens' CreateQueue (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createQueue_tags = Lens.lens (\CreateQueue' {tags} -> tags) (\s@CreateQueue' {} a -> s {tags = a} :: CreateQueue) Prelude.. Lens.mapping Lens.coerced

-- | The name of the new queue. The following limits apply to this name:
--
-- -   A queue name can have up to 80 characters.
--
-- -   Valid values: alphanumeric characters, hyphens (@-@), and
--     underscores (@_@).
--
-- -   A FIFO queue name must end with the @.fifo@ suffix.
--
-- Queue URLs and names are case-sensitive.
createQueue_queueName :: Lens.Lens' CreateQueue Prelude.Text
createQueue_queueName = Lens.lens (\CreateQueue' {queueName} -> queueName) (\s@CreateQueue' {} a -> s {queueName = a} :: CreateQueue)

instance Core.AWSRequest CreateQueue where
  type AWSResponse CreateQueue = CreateQueueResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreateQueueResult"
      ( \s h x ->
          CreateQueueResponse'
            Prelude.<$> (x Data..@? "QueueUrl")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateQueue where
  hashWithSalt _salt CreateQueue' {..} =
    _salt
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` queueName

instance Prelude.NFData CreateQueue where
  rnf CreateQueue' {..} =
    Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf queueName

instance Data.ToHeaders CreateQueue where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateQueue where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateQueue where
  toQuery CreateQueue' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateQueue" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-11-05" :: Prelude.ByteString),
        Data.toQuery
          ( Data.toQueryMap "Attribute" "Name" "Value"
              Prelude.<$> attributes
          ),
        Data.toQuery
          ( Data.toQueryMap "Tag" "Key" "Value"
              Prelude.<$> tags
          ),
        "QueueName" Data.=: queueName
      ]

-- | Returns the @QueueUrl@ attribute of the created queue.
--
-- /See:/ 'newCreateQueueResponse' smart constructor.
data CreateQueueResponse = CreateQueueResponse'
  { -- | The URL of the created Amazon SQS queue.
    queueUrl :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateQueueResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queueUrl', 'createQueueResponse_queueUrl' - The URL of the created Amazon SQS queue.
--
-- 'httpStatus', 'createQueueResponse_httpStatus' - The response's http status code.
newCreateQueueResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateQueueResponse
newCreateQueueResponse pHttpStatus_ =
  CreateQueueResponse'
    { queueUrl = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The URL of the created Amazon SQS queue.
createQueueResponse_queueUrl :: Lens.Lens' CreateQueueResponse (Prelude.Maybe Prelude.Text)
createQueueResponse_queueUrl = Lens.lens (\CreateQueueResponse' {queueUrl} -> queueUrl) (\s@CreateQueueResponse' {} a -> s {queueUrl = a} :: CreateQueueResponse)

-- | The response's http status code.
createQueueResponse_httpStatus :: Lens.Lens' CreateQueueResponse Prelude.Int
createQueueResponse_httpStatus = Lens.lens (\CreateQueueResponse' {httpStatus} -> httpStatus) (\s@CreateQueueResponse' {} a -> s {httpStatus = a} :: CreateQueueResponse)

instance Prelude.NFData CreateQueueResponse where
  rnf CreateQueueResponse' {..} =
    Prelude.rnf queueUrl
      `Prelude.seq` Prelude.rnf httpStatus
