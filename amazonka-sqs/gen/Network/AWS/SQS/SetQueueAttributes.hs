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
-- Module      : Network.AWS.SQS.SetQueueAttributes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the value of one or more queue attributes. When you change a queue's attributes, the change can take up to 60 seconds for most of the attributes to propagate throughout the Amazon SQS system. Changes made to the @MessageRetentionPeriod@ attribute can take up to 15 minutes.
--
--
module Network.AWS.SQS.SetQueueAttributes
    (
    -- * Creating a Request
      setQueueAttributes
    , SetQueueAttributes
    -- * Request Lenses
    , sqaQueueURL
    , sqaAttributes

    -- * Destructuring the Response
    , setQueueAttributesResponse
    , SetQueueAttributesResponse
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
-- /See:/ 'setQueueAttributes' smart constructor.
data SetQueueAttributes = SetQueueAttributes'
  { _sqaQueueURL   :: !Text
  , _sqaAttributes :: !(Map QueueAttributeName Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetQueueAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sqaQueueURL' - The URL of the Amazon SQS queue whose attributes are set. Queue URLs are case-sensitive.
--
-- * 'sqaAttributes' - A map of attributes to set. The following lists the names, descriptions, and values of the special request parameters that the @SetQueueAttributes@ action uses:     * @DelaySeconds@ - The length of time, in seconds, for which the delivery of all messages in the queue is delayed. Valid values: An integer from 0 to 900 (15 minutes). The default is 0 (zero).      * @MaximumMessageSize@ - The limit of how many bytes a message can contain before Amazon SQS rejects it. Valid values: An integer from 1,024 bytes (1 KiB) up to 262,144 bytes (256 KiB). The default is 262,144 (256 KiB).      * @MessageRetentionPeriod@ - The length of time, in seconds, for which Amazon SQS retains a message. Valid values: An integer representing seconds, from 60 (1 minute) to 1,209,600 (14 days). The default is 345,600 (4 days).      * @Policy@ - The queue's policy. A valid AWS policy. For more information about policy structure, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/PoliciesOverview.html Overview of AWS IAM Policies> in the /Amazon IAM User Guide/ .      * @ReceiveMessageWaitTimeSeconds@ - The length of time, in seconds, for which a @'ReceiveMessage' @ action waits for a message to arrive. Valid values: an integer from 0 to 20 (seconds). The default is 0.      * @RedrivePolicy@ - The string that includes the parameters for the dead-letter queue functionality of the source queue. For more information about the redrive policy and dead-letter queues, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-dead-letter-queues.html Using Amazon SQS Dead-Letter Queues> in the /Amazon Simple Queue Service Developer Guide/ .      * @deadLetterTargetArn@ - The Amazon Resource Name (ARN) of the dead-letter queue to which Amazon SQS moves messages after the value of @maxReceiveCount@ is exceeded.     * @maxReceiveCount@ - The number of times a message is delivered to the source queue before being moved to the dead-letter queue.     * @VisibilityTimeout@ - The visibility timeout for the queue. Valid values: an integer from 0 to 43,200 (12 hours). The default is 30. For more information about the visibility timeout, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-visibility-timeout.html Visibility Timeout> in the /Amazon Simple Queue Service Developer Guide/ . The following attributes apply only to <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-server-side-encryption.html server-side-encryption> :     * @KmsMasterKeyId@ - The ID of an AWS-managed customer master key (CMK) for Amazon SQS or a custom CMK. For more information, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-server-side-encryption.html#sqs-sse-key-terms Key Terms> . While the alias of the AWS-managed CMK for Amazon SQS is always @alias/aws/sqs@ , the alias of a custom CMK can, for example, be @alias//MyAlias/ @ . For more examples, see <http://docs.aws.amazon.com/kms/latest/APIReference/API_DescribeKey.html#API_DescribeKey_RequestParameters KeyId> in the /AWS Key Management Service API Reference/ .      * @KmsDataKeyReusePeriodSeconds@ - The length of time, in seconds, for which Amazon SQS can reuse a <http://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#data-keys data key> to encrypt or decrypt messages before calling AWS KMS again. An integer representing seconds, between 60 seconds (1 minute) and 86,400 seconds (24 hours). The default is 300 (5 minutes). A shorter time period provides better security but results in more calls to KMS which might incur charges after Free Tier. For more information, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-server-side-encryption.html#sqs-how-does-the-data-key-reuse-period-work How Does the Data Key Reuse Period Work?> .  The following attribute applies only to <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html FIFO (first-in-first-out) queues> :     * @ContentBasedDeduplication@ - Enables content-based deduplication. For more information, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html#FIFO-queues-exactly-once-processing Exactly-Once Processing> in the /Amazon Simple Queue Service Developer Guide/ .      * Every message must have a unique @MessageDeduplicationId@ ,     * You may provide a @MessageDeduplicationId@ explicitly.     * If you aren't able to provide a @MessageDeduplicationId@ and you enable @ContentBasedDeduplication@ for your queue, Amazon SQS uses a SHA-256 hash to generate the @MessageDeduplicationId@ using the body of the message (but not the attributes of the message).      * If you don't provide a @MessageDeduplicationId@ and the queue doesn't have @ContentBasedDeduplication@ set, the action fails with an error.     * If the queue has @ContentBasedDeduplication@ set, your @MessageDeduplicationId@ overrides the generated one.     * When @ContentBasedDeduplication@ is in effect, messages with identical content sent within the deduplication interval are treated as duplicates and only one copy of the message is delivered.     * If you send one message with @ContentBasedDeduplication@ enabled and then another message with a @MessageDeduplicationId@ that is the same as the one generated for the first @MessageDeduplicationId@ , the two messages are treated as duplicates and only one copy of the message is delivered.  Any other valid special request parameters (such as the following) are ignored:     * @ApproximateNumberOfMessages@      * @ApproximateNumberOfMessagesDelayed@      * @ApproximateNumberOfMessagesNotVisible@      * @CreatedTimestamp@      * @LastModifiedTimestamp@      * @QueueArn@
setQueueAttributes
    :: Text -- ^ 'sqaQueueURL'
    -> SetQueueAttributes
setQueueAttributes pQueueURL_ =
  SetQueueAttributes' {_sqaQueueURL = pQueueURL_, _sqaAttributes = mempty}


-- | The URL of the Amazon SQS queue whose attributes are set. Queue URLs are case-sensitive.
sqaQueueURL :: Lens' SetQueueAttributes Text
sqaQueueURL = lens _sqaQueueURL (\ s a -> s{_sqaQueueURL = a})

-- | A map of attributes to set. The following lists the names, descriptions, and values of the special request parameters that the @SetQueueAttributes@ action uses:     * @DelaySeconds@ - The length of time, in seconds, for which the delivery of all messages in the queue is delayed. Valid values: An integer from 0 to 900 (15 minutes). The default is 0 (zero).      * @MaximumMessageSize@ - The limit of how many bytes a message can contain before Amazon SQS rejects it. Valid values: An integer from 1,024 bytes (1 KiB) up to 262,144 bytes (256 KiB). The default is 262,144 (256 KiB).      * @MessageRetentionPeriod@ - The length of time, in seconds, for which Amazon SQS retains a message. Valid values: An integer representing seconds, from 60 (1 minute) to 1,209,600 (14 days). The default is 345,600 (4 days).      * @Policy@ - The queue's policy. A valid AWS policy. For more information about policy structure, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/PoliciesOverview.html Overview of AWS IAM Policies> in the /Amazon IAM User Guide/ .      * @ReceiveMessageWaitTimeSeconds@ - The length of time, in seconds, for which a @'ReceiveMessage' @ action waits for a message to arrive. Valid values: an integer from 0 to 20 (seconds). The default is 0.      * @RedrivePolicy@ - The string that includes the parameters for the dead-letter queue functionality of the source queue. For more information about the redrive policy and dead-letter queues, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-dead-letter-queues.html Using Amazon SQS Dead-Letter Queues> in the /Amazon Simple Queue Service Developer Guide/ .      * @deadLetterTargetArn@ - The Amazon Resource Name (ARN) of the dead-letter queue to which Amazon SQS moves messages after the value of @maxReceiveCount@ is exceeded.     * @maxReceiveCount@ - The number of times a message is delivered to the source queue before being moved to the dead-letter queue.     * @VisibilityTimeout@ - The visibility timeout for the queue. Valid values: an integer from 0 to 43,200 (12 hours). The default is 30. For more information about the visibility timeout, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-visibility-timeout.html Visibility Timeout> in the /Amazon Simple Queue Service Developer Guide/ . The following attributes apply only to <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-server-side-encryption.html server-side-encryption> :     * @KmsMasterKeyId@ - The ID of an AWS-managed customer master key (CMK) for Amazon SQS or a custom CMK. For more information, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-server-side-encryption.html#sqs-sse-key-terms Key Terms> . While the alias of the AWS-managed CMK for Amazon SQS is always @alias/aws/sqs@ , the alias of a custom CMK can, for example, be @alias//MyAlias/ @ . For more examples, see <http://docs.aws.amazon.com/kms/latest/APIReference/API_DescribeKey.html#API_DescribeKey_RequestParameters KeyId> in the /AWS Key Management Service API Reference/ .      * @KmsDataKeyReusePeriodSeconds@ - The length of time, in seconds, for which Amazon SQS can reuse a <http://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#data-keys data key> to encrypt or decrypt messages before calling AWS KMS again. An integer representing seconds, between 60 seconds (1 minute) and 86,400 seconds (24 hours). The default is 300 (5 minutes). A shorter time period provides better security but results in more calls to KMS which might incur charges after Free Tier. For more information, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-server-side-encryption.html#sqs-how-does-the-data-key-reuse-period-work How Does the Data Key Reuse Period Work?> .  The following attribute applies only to <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html FIFO (first-in-first-out) queues> :     * @ContentBasedDeduplication@ - Enables content-based deduplication. For more information, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html#FIFO-queues-exactly-once-processing Exactly-Once Processing> in the /Amazon Simple Queue Service Developer Guide/ .      * Every message must have a unique @MessageDeduplicationId@ ,     * You may provide a @MessageDeduplicationId@ explicitly.     * If you aren't able to provide a @MessageDeduplicationId@ and you enable @ContentBasedDeduplication@ for your queue, Amazon SQS uses a SHA-256 hash to generate the @MessageDeduplicationId@ using the body of the message (but not the attributes of the message).      * If you don't provide a @MessageDeduplicationId@ and the queue doesn't have @ContentBasedDeduplication@ set, the action fails with an error.     * If the queue has @ContentBasedDeduplication@ set, your @MessageDeduplicationId@ overrides the generated one.     * When @ContentBasedDeduplication@ is in effect, messages with identical content sent within the deduplication interval are treated as duplicates and only one copy of the message is delivered.     * If you send one message with @ContentBasedDeduplication@ enabled and then another message with a @MessageDeduplicationId@ that is the same as the one generated for the first @MessageDeduplicationId@ , the two messages are treated as duplicates and only one copy of the message is delivered.  Any other valid special request parameters (such as the following) are ignored:     * @ApproximateNumberOfMessages@      * @ApproximateNumberOfMessagesDelayed@      * @ApproximateNumberOfMessagesNotVisible@      * @CreatedTimestamp@      * @LastModifiedTimestamp@      * @QueueArn@
sqaAttributes :: Lens' SetQueueAttributes (HashMap QueueAttributeName Text)
sqaAttributes = lens _sqaAttributes (\ s a -> s{_sqaAttributes = a}) . _Map

instance AWSRequest SetQueueAttributes where
        type Rs SetQueueAttributes =
             SetQueueAttributesResponse
        request = postQuery sqs
        response = receiveNull SetQueueAttributesResponse'

instance Hashable SetQueueAttributes where

instance NFData SetQueueAttributes where

instance ToHeaders SetQueueAttributes where
        toHeaders = const mempty

instance ToPath SetQueueAttributes where
        toPath = const "/"

instance ToQuery SetQueueAttributes where
        toQuery SetQueueAttributes'{..}
          = mconcat
              ["Action" =: ("SetQueueAttributes" :: ByteString),
               "Version" =: ("2012-11-05" :: ByteString),
               "QueueUrl" =: _sqaQueueURL,
               toQueryMap "Attribute" "Name" "Value" _sqaAttributes]

-- | /See:/ 'setQueueAttributesResponse' smart constructor.
data SetQueueAttributesResponse =
  SetQueueAttributesResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetQueueAttributesResponse' with the minimum fields required to make a request.
--
setQueueAttributesResponse
    :: SetQueueAttributesResponse
setQueueAttributesResponse = SetQueueAttributesResponse'


instance NFData SetQueueAttributesResponse where
