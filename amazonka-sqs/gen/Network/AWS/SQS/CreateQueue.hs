{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.CreateQueue
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new standard or FIFO queue. You can pass one or more attributes in the request. Keep the following caveats in mind:
--
--
--     * If you don't specify the @FifoQueue@ attribute, Amazon SQS creates a standard queue.
--
--     * If you don't provide a value for an attribute, the queue is created with the default value for the attribute.
--
--     * If you delete a queue, you must wait at least 60 seconds before creating a queue with the same name.
--
--
--
-- To successfully create a new queue, you must provide a queue name that adheres to the <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/limits-queues.html limits related to queues> and is unique within the scope of your queues.
--
-- To get the queue URL, use the @'GetQueueUrl' @ action. @'GetQueueUrl' @ requires only the @QueueName@ parameter. be aware of existing queue names:
--
--     * If you provide the name of an existing queue along with the exact names and values of all the queue's attributes, @CreateQueue@ returns the queue URL for the existing queue.
--
--     * If the queue name, attribute names, or attribute values don't match an existing queue, @CreateQueue@ returns an error.
--
--
--
module Network.AWS.SQS.CreateQueue
    (
    -- * Creating a Request
      createQueue
    , CreateQueue
    -- * Request Lenses
    , cqAttributes
    , cqQueueName

    -- * Destructuring the Response
    , createQueueResponse
    , CreateQueueResponse
    -- * Response Lenses
    , cqrsQueueURL
    , cqrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SQS.Types
import Network.AWS.SQS.Types.Product

-- |
--
--
--
-- /See:/ 'createQueue' smart constructor.
data CreateQueue = CreateQueue'
  { _cqAttributes :: !(Maybe (Map QueueAttributeName Text))
  , _cqQueueName  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateQueue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cqAttributes' - A map of attributes with their corresponding values. The following lists the names, descriptions, and values of the special request parameters that the @CreateQueue@ action uses:     * @DelaySeconds@ - The length of time, in seconds, for which the delivery of all messages in the queue is delayed. Valid values: An integer from 0 to 900 seconds (15 minutes). The default is 0 (zero).      * @MaximumMessageSize@ - The limit of how many bytes a message can contain before Amazon SQS rejects it. Valid values: An integer from 1,024 bytes (1 KiB) to 262,144 bytes (256 KiB). The default is 262,144 (256 KiB).      * @MessageRetentionPeriod@ - The length of time, in seconds, for which Amazon SQS retains a message. Valid values: An integer from 60 seconds (1 minute) to 1,209,600 seconds (14 days). The default is 345,600 (4 days).      * @Policy@ - The queue's policy. A valid AWS policy. For more information about policy structure, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/PoliciesOverview.html Overview of AWS IAM Policies> in the /Amazon IAM User Guide/ .      * @ReceiveMessageWaitTimeSeconds@ - The length of time, in seconds, for which a @'ReceiveMessage' @ action waits for a message to arrive. Valid values: An integer from 0 to 20 (seconds). The default is 0 (zero).      * @RedrivePolicy@ - The string that includes the parameters for the dead-letter queue functionality of the source queue. For more information about the redrive policy and dead-letter queues, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-dead-letter-queues.html Using Amazon SQS Dead-Letter Queues> in the /Amazon Simple Queue Service Developer Guide/ .      * @deadLetterTargetArn@ - The Amazon Resource Name (ARN) of the dead-letter queue to which Amazon SQS moves messages after the value of @maxReceiveCount@ is exceeded.     * @maxReceiveCount@ - The number of times a message is delivered to the source queue before being moved to the dead-letter queue.     * @VisibilityTimeout@ - The visibility timeout for the queue. Valid values: An integer from 0 to 43,200 (12 hours). The default is 30. For more information about the visibility timeout, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-visibility-timeout.html Visibility Timeout> in the /Amazon Simple Queue Service Developer Guide/ . The following attributes apply only to <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-server-side-encryption.html server-side-encryption> :     * @KmsMasterKeyId@ - The ID of an AWS-managed customer master key (CMK) for Amazon SQS or a custom CMK. For more information, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-server-side-encryption.html#sqs-sse-key-terms Key Terms> . While the alias of the AWS-managed CMK for Amazon SQS is always @alias/aws/sqs@ , the alias of a custom CMK can, for example, be @alias//MyAlias/ @ . For more examples, see <http://docs.aws.amazon.com/kms/latest/APIReference/API_DescribeKey.html#API_DescribeKey_RequestParameters KeyId> in the /AWS Key Management Service API Reference/ .      * @KmsDataKeyReusePeriodSeconds@ - The length of time, in seconds, for which Amazon SQS can reuse a <http://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#data-keys data key> to encrypt or decrypt messages before calling AWS KMS again. An integer representing seconds, between 60 seconds (1 minute) and 86,400 seconds (24 hours). The default is 300 (5 minutes). A shorter time period provides better security but results in more calls to KMS which might incur charges after Free Tier. For more information, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-server-side-encryption.html#sqs-how-does-the-data-key-reuse-period-work How Does the Data Key Reuse Period Work?> .  The following attributes apply only to <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html FIFO (first-in-first-out) queues> :     * @FifoQueue@ - Designates a queue as FIFO. Valid values: @true@ , @false@ . You can provide this attribute only during queue creation. You can't change it for an existing queue. When you set this attribute, you must also provide the @MessageGroupId@ for your messages explicitly. For more information, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html#FIFO-queues-understanding-logic FIFO Queue Logic> in the /Amazon Simple Queue Service Developer Guide/ .     * @ContentBasedDeduplication@ - Enables content-based deduplication. Valid values: @true@ , @false@ . For more information, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html#FIFO-queues-exactly-once-processing Exactly-Once Processing> in the /Amazon Simple Queue Service Developer Guide/ .      * Every message must have a unique @MessageDeduplicationId@ ,     * You may provide a @MessageDeduplicationId@ explicitly.     * If you aren't able to provide a @MessageDeduplicationId@ and you enable @ContentBasedDeduplication@ for your queue, Amazon SQS uses a SHA-256 hash to generate the @MessageDeduplicationId@ using the body of the message (but not the attributes of the message).      * If you don't provide a @MessageDeduplicationId@ and the queue doesn't have @ContentBasedDeduplication@ set, the action fails with an error.     * If the queue has @ContentBasedDeduplication@ set, your @MessageDeduplicationId@ overrides the generated one.     * When @ContentBasedDeduplication@ is in effect, messages with identical content sent within the deduplication interval are treated as duplicates and only one copy of the message is delivered.     * If you send one message with @ContentBasedDeduplication@ enabled and then another message with a @MessageDeduplicationId@ that is the same as the one generated for the first @MessageDeduplicationId@ , the two messages are treated as duplicates and only one copy of the message is delivered.  Any other valid special request parameters (such as the following) are ignored:     * @ApproximateNumberOfMessages@      * @ApproximateNumberOfMessagesDelayed@      * @ApproximateNumberOfMessagesNotVisible@      * @CreatedTimestamp@      * @LastModifiedTimestamp@      * @QueueArn@
--
-- * 'cqQueueName' - The name of the new queue. The following limits apply to this name:     * A queue name can have up to 80 characters.     * Valid values: alphanumeric characters, hyphens (@-@ ), and underscores (@_@ ).     * A FIFO queue name must end with the @.fifo@ suffix. Queue names are case-sensitive.
createQueue
    :: Text -- ^ 'cqQueueName'
    -> CreateQueue
createQueue pQueueName_ =
  CreateQueue' {_cqAttributes = Nothing, _cqQueueName = pQueueName_}


-- | A map of attributes with their corresponding values. The following lists the names, descriptions, and values of the special request parameters that the @CreateQueue@ action uses:     * @DelaySeconds@ - The length of time, in seconds, for which the delivery of all messages in the queue is delayed. Valid values: An integer from 0 to 900 seconds (15 minutes). The default is 0 (zero).      * @MaximumMessageSize@ - The limit of how many bytes a message can contain before Amazon SQS rejects it. Valid values: An integer from 1,024 bytes (1 KiB) to 262,144 bytes (256 KiB). The default is 262,144 (256 KiB).      * @MessageRetentionPeriod@ - The length of time, in seconds, for which Amazon SQS retains a message. Valid values: An integer from 60 seconds (1 minute) to 1,209,600 seconds (14 days). The default is 345,600 (4 days).      * @Policy@ - The queue's policy. A valid AWS policy. For more information about policy structure, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/PoliciesOverview.html Overview of AWS IAM Policies> in the /Amazon IAM User Guide/ .      * @ReceiveMessageWaitTimeSeconds@ - The length of time, in seconds, for which a @'ReceiveMessage' @ action waits for a message to arrive. Valid values: An integer from 0 to 20 (seconds). The default is 0 (zero).      * @RedrivePolicy@ - The string that includes the parameters for the dead-letter queue functionality of the source queue. For more information about the redrive policy and dead-letter queues, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-dead-letter-queues.html Using Amazon SQS Dead-Letter Queues> in the /Amazon Simple Queue Service Developer Guide/ .      * @deadLetterTargetArn@ - The Amazon Resource Name (ARN) of the dead-letter queue to which Amazon SQS moves messages after the value of @maxReceiveCount@ is exceeded.     * @maxReceiveCount@ - The number of times a message is delivered to the source queue before being moved to the dead-letter queue.     * @VisibilityTimeout@ - The visibility timeout for the queue. Valid values: An integer from 0 to 43,200 (12 hours). The default is 30. For more information about the visibility timeout, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-visibility-timeout.html Visibility Timeout> in the /Amazon Simple Queue Service Developer Guide/ . The following attributes apply only to <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-server-side-encryption.html server-side-encryption> :     * @KmsMasterKeyId@ - The ID of an AWS-managed customer master key (CMK) for Amazon SQS or a custom CMK. For more information, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-server-side-encryption.html#sqs-sse-key-terms Key Terms> . While the alias of the AWS-managed CMK for Amazon SQS is always @alias/aws/sqs@ , the alias of a custom CMK can, for example, be @alias//MyAlias/ @ . For more examples, see <http://docs.aws.amazon.com/kms/latest/APIReference/API_DescribeKey.html#API_DescribeKey_RequestParameters KeyId> in the /AWS Key Management Service API Reference/ .      * @KmsDataKeyReusePeriodSeconds@ - The length of time, in seconds, for which Amazon SQS can reuse a <http://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#data-keys data key> to encrypt or decrypt messages before calling AWS KMS again. An integer representing seconds, between 60 seconds (1 minute) and 86,400 seconds (24 hours). The default is 300 (5 minutes). A shorter time period provides better security but results in more calls to KMS which might incur charges after Free Tier. For more information, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-server-side-encryption.html#sqs-how-does-the-data-key-reuse-period-work How Does the Data Key Reuse Period Work?> .  The following attributes apply only to <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html FIFO (first-in-first-out) queues> :     * @FifoQueue@ - Designates a queue as FIFO. Valid values: @true@ , @false@ . You can provide this attribute only during queue creation. You can't change it for an existing queue. When you set this attribute, you must also provide the @MessageGroupId@ for your messages explicitly. For more information, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html#FIFO-queues-understanding-logic FIFO Queue Logic> in the /Amazon Simple Queue Service Developer Guide/ .     * @ContentBasedDeduplication@ - Enables content-based deduplication. Valid values: @true@ , @false@ . For more information, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html#FIFO-queues-exactly-once-processing Exactly-Once Processing> in the /Amazon Simple Queue Service Developer Guide/ .      * Every message must have a unique @MessageDeduplicationId@ ,     * You may provide a @MessageDeduplicationId@ explicitly.     * If you aren't able to provide a @MessageDeduplicationId@ and you enable @ContentBasedDeduplication@ for your queue, Amazon SQS uses a SHA-256 hash to generate the @MessageDeduplicationId@ using the body of the message (but not the attributes of the message).      * If you don't provide a @MessageDeduplicationId@ and the queue doesn't have @ContentBasedDeduplication@ set, the action fails with an error.     * If the queue has @ContentBasedDeduplication@ set, your @MessageDeduplicationId@ overrides the generated one.     * When @ContentBasedDeduplication@ is in effect, messages with identical content sent within the deduplication interval are treated as duplicates and only one copy of the message is delivered.     * If you send one message with @ContentBasedDeduplication@ enabled and then another message with a @MessageDeduplicationId@ that is the same as the one generated for the first @MessageDeduplicationId@ , the two messages are treated as duplicates and only one copy of the message is delivered.  Any other valid special request parameters (such as the following) are ignored:     * @ApproximateNumberOfMessages@      * @ApproximateNumberOfMessagesDelayed@      * @ApproximateNumberOfMessagesNotVisible@      * @CreatedTimestamp@      * @LastModifiedTimestamp@      * @QueueArn@
cqAttributes :: Lens' CreateQueue (HashMap QueueAttributeName Text)
cqAttributes = lens _cqAttributes (\ s a -> s{_cqAttributes = a}) . _Default . _Map

-- | The name of the new queue. The following limits apply to this name:     * A queue name can have up to 80 characters.     * Valid values: alphanumeric characters, hyphens (@-@ ), and underscores (@_@ ).     * A FIFO queue name must end with the @.fifo@ suffix. Queue names are case-sensitive.
cqQueueName :: Lens' CreateQueue Text
cqQueueName = lens _cqQueueName (\ s a -> s{_cqQueueName = a})

instance AWSRequest CreateQueue where
        type Rs CreateQueue = CreateQueueResponse
        request = postQuery sqs
        response
          = receiveXMLWrapper "CreateQueueResult"
              (\ s h x ->
                 CreateQueueResponse' <$>
                   (x .@? "QueueUrl") <*> (pure (fromEnum s)))

instance Hashable CreateQueue where

instance NFData CreateQueue where

instance ToHeaders CreateQueue where
        toHeaders = const mempty

instance ToPath CreateQueue where
        toPath = const "/"

instance ToQuery CreateQueue where
        toQuery CreateQueue'{..}
          = mconcat
              ["Action" =: ("CreateQueue" :: ByteString),
               "Version" =: ("2012-11-05" :: ByteString),
               toQuery
                 (toQueryMap "Attribute" "Name" "Value" <$>
                    _cqAttributes),
               "QueueName" =: _cqQueueName]

-- | Returns the @QueueUrl@ attribute of the created queue.
--
--
--
-- /See:/ 'createQueueResponse' smart constructor.
data CreateQueueResponse = CreateQueueResponse'
  { _cqrsQueueURL       :: !(Maybe Text)
  , _cqrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateQueueResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cqrsQueueURL' - The URL of the created Amazon SQS queue.
--
-- * 'cqrsResponseStatus' - -- | The response status code.
createQueueResponse
    :: Int -- ^ 'cqrsResponseStatus'
    -> CreateQueueResponse
createQueueResponse pResponseStatus_ =
  CreateQueueResponse'
    {_cqrsQueueURL = Nothing, _cqrsResponseStatus = pResponseStatus_}


-- | The URL of the created Amazon SQS queue.
cqrsQueueURL :: Lens' CreateQueueResponse (Maybe Text)
cqrsQueueURL = lens _cqrsQueueURL (\ s a -> s{_cqrsQueueURL = a})

-- | -- | The response status code.
cqrsResponseStatus :: Lens' CreateQueueResponse Int
cqrsResponseStatus = lens _cqrsResponseStatus (\ s a -> s{_cqrsResponseStatus = a})

instance NFData CreateQueueResponse where
