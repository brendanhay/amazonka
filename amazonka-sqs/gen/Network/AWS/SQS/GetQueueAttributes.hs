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
-- Module      : Network.AWS.SQS.GetQueueAttributes
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets attributes for the specified queue.
--
--
-- @&amp;Attribute.1=this@
--
-- @&amp;Attribute.2=that@
--
module Network.AWS.SQS.GetQueueAttributes
    (
    -- * Creating a Request
      getQueueAttributes
    , GetQueueAttributes
    -- * Request Lenses
    , gqaAttributeNames
    , gqaQueueURL

    -- * Destructuring the Response
    , getQueueAttributesResponse
    , GetQueueAttributesResponse
    -- * Response Lenses
    , gqarsAttributes
    , gqarsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SQS.Types
import           Network.AWS.SQS.Types.Product

-- |
--
--
--
-- /See:/ 'getQueueAttributes' smart constructor.
data GetQueueAttributes = GetQueueAttributes'
    { _gqaAttributeNames :: !(Maybe [QueueAttributeName])
    , _gqaQueueURL       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetQueueAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gqaAttributeNames' - A list of attributes for which to retrieve information. The following attributes are supported:     * @All@ - Returns all values.      * @ApproximateNumberOfMessages@ - Returns the approximate number of visible messages in a queue. For more information, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/ApproximateNumber.html Resources Required to Process Messages> in the /Amazon SQS Developer Guide/ .      * @ApproximateNumberOfMessagesDelayed@ - Returns the approximate number of messages that are waiting to be added to the queue.      * @ApproximateNumberOfMessagesNotVisible@ - Returns the approximate number of messages that have not timed-out and are not deleted. For more information, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/ApproximateNumber.html Resources Required to Process Messages> in the /Amazon SQS Developer Guide/ .      * @CreatedTimestamp@ - Returns the time when the queue was created in seconds (epoch time).     * @DelaySeconds@ - Returns the default delay on the queue in seconds.     * @LastModifiedTimestamp@ - Returns the time when the queue was last changed in seconds (epoch time).     * @MaximumMessageSize@ - Returns the limit of how many bytes a message can contain before Amazon SQS rejects it.     * @MessageRetentionPeriod@ - Returns the number of seconds for which Amazon SQS retains a message.     * @Policy@ - Returns the policy of the queue.     * @QueueArn@ - Returns the Amazon resource name (ARN) of the queue.     * @ReceiveMessageWaitTimeSeconds@ - Returns the number of seconds for which ReceiveMessage call will wait for a message to arrive.      * @RedrivePolicy@ - Returns the parameters for dead letter queue functionality of the source queue. For more information about the redrive policy and dead letter queues, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/SQSDeadLetterQueue.html Using Amazon SQS Dead Letter Queues> in the /Amazon SQS Developer Guide/ .      * @VisibilityTimeout@ - Returns the visibility timeout for the queue. For more information about the visibility timeout, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/AboutVT.html Visibility Timeout> in the /Amazon SQS Developer Guide/ .  The following attributes apply only to <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html FIFO (first-in-first-out) queues> :     * @FifoQueue@ - Returns whether the queue is FIFO. For more information, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html#FIFO-queues-understanding-logic FIFO Queue Logic> in the /Amazon SQS Developer Guide/ .     * @ContentBasedDeduplication@ - Returns whether content-based deduplication is enabled for the queue. For more information, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html#FIFO-queues-exactly-once-processing Exactly-Once Processing> in the /Amazon SQS Developer Guide/ .
--
-- * 'gqaQueueURL' - The URL of the Amazon SQS queue to take action on. Queue URLs are case-sensitive.
getQueueAttributes
    :: Text -- ^ 'gqaQueueURL'
    -> GetQueueAttributes
getQueueAttributes pQueueURL_ =
    GetQueueAttributes'
    { _gqaAttributeNames = Nothing
    , _gqaQueueURL = pQueueURL_
    }

-- | A list of attributes for which to retrieve information. The following attributes are supported:     * @All@ - Returns all values.      * @ApproximateNumberOfMessages@ - Returns the approximate number of visible messages in a queue. For more information, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/ApproximateNumber.html Resources Required to Process Messages> in the /Amazon SQS Developer Guide/ .      * @ApproximateNumberOfMessagesDelayed@ - Returns the approximate number of messages that are waiting to be added to the queue.      * @ApproximateNumberOfMessagesNotVisible@ - Returns the approximate number of messages that have not timed-out and are not deleted. For more information, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/ApproximateNumber.html Resources Required to Process Messages> in the /Amazon SQS Developer Guide/ .      * @CreatedTimestamp@ - Returns the time when the queue was created in seconds (epoch time).     * @DelaySeconds@ - Returns the default delay on the queue in seconds.     * @LastModifiedTimestamp@ - Returns the time when the queue was last changed in seconds (epoch time).     * @MaximumMessageSize@ - Returns the limit of how many bytes a message can contain before Amazon SQS rejects it.     * @MessageRetentionPeriod@ - Returns the number of seconds for which Amazon SQS retains a message.     * @Policy@ - Returns the policy of the queue.     * @QueueArn@ - Returns the Amazon resource name (ARN) of the queue.     * @ReceiveMessageWaitTimeSeconds@ - Returns the number of seconds for which ReceiveMessage call will wait for a message to arrive.      * @RedrivePolicy@ - Returns the parameters for dead letter queue functionality of the source queue. For more information about the redrive policy and dead letter queues, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/SQSDeadLetterQueue.html Using Amazon SQS Dead Letter Queues> in the /Amazon SQS Developer Guide/ .      * @VisibilityTimeout@ - Returns the visibility timeout for the queue. For more information about the visibility timeout, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/AboutVT.html Visibility Timeout> in the /Amazon SQS Developer Guide/ .  The following attributes apply only to <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html FIFO (first-in-first-out) queues> :     * @FifoQueue@ - Returns whether the queue is FIFO. For more information, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html#FIFO-queues-understanding-logic FIFO Queue Logic> in the /Amazon SQS Developer Guide/ .     * @ContentBasedDeduplication@ - Returns whether content-based deduplication is enabled for the queue. For more information, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html#FIFO-queues-exactly-once-processing Exactly-Once Processing> in the /Amazon SQS Developer Guide/ .
gqaAttributeNames :: Lens' GetQueueAttributes [QueueAttributeName]
gqaAttributeNames = lens _gqaAttributeNames (\ s a -> s{_gqaAttributeNames = a}) . _Default . _Coerce;

-- | The URL of the Amazon SQS queue to take action on. Queue URLs are case-sensitive.
gqaQueueURL :: Lens' GetQueueAttributes Text
gqaQueueURL = lens _gqaQueueURL (\ s a -> s{_gqaQueueURL = a});

instance AWSRequest GetQueueAttributes where
        type Rs GetQueueAttributes =
             GetQueueAttributesResponse
        request = postQuery sqs
        response
          = receiveXMLWrapper "GetQueueAttributesResult"
              (\ s h x ->
                 GetQueueAttributesResponse' <$>
                   (may (parseXMLMap "Attribute" "Name" "Value") x) <*>
                     (pure (fromEnum s)))

instance Hashable GetQueueAttributes

instance NFData GetQueueAttributes

instance ToHeaders GetQueueAttributes where
        toHeaders = const mempty

instance ToPath GetQueueAttributes where
        toPath = const "/"

instance ToQuery GetQueueAttributes where
        toQuery GetQueueAttributes'{..}
          = mconcat
              ["Action" =: ("GetQueueAttributes" :: ByteString),
               "Version" =: ("2012-11-05" :: ByteString),
               toQuery
                 (toQueryList "AttributeName" <$> _gqaAttributeNames),
               "QueueUrl" =: _gqaQueueURL]

-- | A list of returned queue attributes.
--
--
--
-- /See:/ 'getQueueAttributesResponse' smart constructor.
data GetQueueAttributesResponse = GetQueueAttributesResponse'
    { _gqarsAttributes     :: !(Maybe (Map QueueAttributeName Text))
    , _gqarsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetQueueAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gqarsAttributes' - A map of attributes to the respective values.
--
-- * 'gqarsResponseStatus' - -- | The response status code.
getQueueAttributesResponse
    :: Int -- ^ 'gqarsResponseStatus'
    -> GetQueueAttributesResponse
getQueueAttributesResponse pResponseStatus_ =
    GetQueueAttributesResponse'
    { _gqarsAttributes = Nothing
    , _gqarsResponseStatus = pResponseStatus_
    }

-- | A map of attributes to the respective values.
gqarsAttributes :: Lens' GetQueueAttributesResponse (HashMap QueueAttributeName Text)
gqarsAttributes = lens _gqarsAttributes (\ s a -> s{_gqarsAttributes = a}) . _Default . _Map;

-- | -- | The response status code.
gqarsResponseStatus :: Lens' GetQueueAttributesResponse Int
gqarsResponseStatus = lens _gqarsResponseStatus (\ s a -> s{_gqarsResponseStatus = a});

instance NFData GetQueueAttributesResponse
