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
-- Module      : Network.AWS.SQS.ReceiveMessage
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves one or more messages, with a maximum limit of 10 messages, from the specified queue. Long poll support is enabled by using the @WaitTimeSeconds@ parameter. For more information, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-long-polling.html Amazon SQS Long Poll> in the /Amazon SQS Developer Guide/ .
--
--
-- Short poll is the default behavior where a weighted random set of machines is sampled on a @ReceiveMessage@ call. This means only the messages on the sampled machines are returned. If the number of messages in the queue is small (less than 1000), it is likely you will get fewer messages than you requested per @ReceiveMessage@ call. If the number of messages in the queue is extremely small, you might not receive any messages in a particular @ReceiveMessage@ response; in which case you should repeat the request.
--
-- For each message returned, the response includes the following:
--
--     * Message body
--
--     * MD5 digest of the message body. For information about MD5, see <https://www.ietf.org/rfc/rfc1321.txt RFC1321> .
--
--     * Message ID you received when you sent the message to the queue.
--
--     * Receipt handle.
--
--     * Message attributes.
--
--     * MD5 digest of the message attributes.
--
--
--
-- The receipt handle is the identifier you must provide when deleting the message. For more information, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/ImportantIdentifiers.html Queue and Message Identifiers> in the /Amazon SQS Developer Guide/ .
--
-- You can provide the @VisibilityTimeout@ parameter in your request, which will be applied to the messages that Amazon SQS returns in the response. If you don't include the parameter, the overall visibility timeout for the queue is used for the returned messages. For more information, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/AboutVT.html Visibility Timeout> in the /Amazon SQS Developer Guide/ .
--
-- A message that is not deleted or a message whose visibility is not extended before the visibility timeout expires counts as a failed receive. Depending on the configuration of the queue, the message might be sent to the dead letter queue.
--
module Network.AWS.SQS.ReceiveMessage
    (
    -- * Creating a Request
      receiveMessage
    , ReceiveMessage
    -- * Request Lenses
    , rmReceiveRequestAttemptId
    , rmVisibilityTimeout
    , rmMessageAttributeNames
    , rmWaitTimeSeconds
    , rmAttributeNames
    , rmMaxNumberOfMessages
    , rmQueueURL

    -- * Destructuring the Response
    , receiveMessageResponse
    , ReceiveMessageResponse
    -- * Response Lenses
    , rmrsMessages
    , rmrsResponseStatus
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
-- /See:/ 'receiveMessage' smart constructor.
data ReceiveMessage = ReceiveMessage'
    { _rmReceiveRequestAttemptId :: !(Maybe Text)
    , _rmVisibilityTimeout       :: !(Maybe Int)
    , _rmMessageAttributeNames   :: !(Maybe [Text])
    , _rmWaitTimeSeconds         :: !(Maybe Int)
    , _rmAttributeNames          :: !(Maybe [MessageAttribute])
    , _rmMaxNumberOfMessages     :: !(Maybe Int)
    , _rmQueueURL                :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ReceiveMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rmReceiveRequestAttemptId' - This parameter applies only to FIFO (first-in-first-out) queues. The token used for deduplication of @ReceiveMessage@ calls. If a networking issue occurs after a @ReceiveMessage@ action, and instead of a response you receive a generic error, you can retry the same action with an identical @ReceiveRequestAttemptId@ to retrieve the same set of messages, even if their visibility timeout has not yet expired.     * You can use @ReceiveRequestAttemptId@ only for 5 minutes after a @ReceiveMessage@ action.     * When you set @FifoQueue@ , a caller of the @ReceiveMessage@ action can provide a @ReceiveRequestAttemptId@ explicitly.     * If a caller of the @ReceiveMessage@ action doesn't provide a @ReceiveRequestAttemptId@ , Amazon SQS generates a @ReceiveRequestAttemptId@ .     * You can retry the @ReceiveMessage@ action with the same @ReceiveRequestAttemptId@ if none of the messages have been modified (deleted or had their visibility changes).     * During a visibility timeout, subsequent calls with the same @ReceiveRequestAttemptId@ return the same messages and receipt handles. If a retry occurs within the deduplication interval, it resets the visibility timeout. For more information, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/AboutVT.html Visibility Timeout> in the /Amazon Simple Queue Service Developer Guide/ . /Important:/ If a caller of the @ReceiveMessage@ action is still processing messages when the visibility timeout expires and messages become visible, another worker reading from the same queue can receive the same messages and therefore process duplicates. Also, if a reader whose message processing time is longer than the visibility timeout tries to delete the processed messages, the action fails with an error. To mitigate this effect, ensure that your application observes a safe threshold before the visibility timeout expires and extend the visibility timeout as necessary.     * While messages with a particular @MessageGroupId@ are invisible, no more messages belonging to the same @MessageGroupId@ are returned until the visibility timeout expires. You can still receive messages with another @MessageGroupId@ as long as it is also visible.     * If a caller of @ReceiveMessage@ can't track the @ReceiveRequestAttemptId@ , no retries will work until the original visibility timeout expires. As a result, delays might occur but the messages in the queue will remain in a strict order. The length of @ReceiveRequestAttemptId@ is 128 characters. @ReceiveRequestAttemptId@ can contain alphanumeric characters (@a-z@ , @A-Z@ , @0-9@ ) and punctuation (@!"#$%&'()*+,-./:;<=>?@[\]^_`{|}~@ ). For best practices of using @ReceiveRequestAttemptId@ , see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queue-recommendations.html#using-receiverequestattemptid-request-parameter Using the ReceiveRequestAttemptId Request Parameter> in the /Amazon Simple Queue Service Developer Guide/ .
--
-- * 'rmVisibilityTimeout' - The duration (in seconds) that the received messages are hidden from subsequent retrieve requests after being retrieved by a @ReceiveMessage@ request.
--
-- * 'rmMessageAttributeNames' - The name of the message attribute, where /N/ is the index. The message attribute name can contain the following characters: A-Z, a-z, 0-9, underscore (_), hyphen (-), and period (.). The name must not start or end with a period, and it should not have successive periods. The name is case sensitive and must be unique among all attribute names for the message. The name can be up to 256 characters long. The name can't start with "AWS." or "Amazon." (or any variations in casing), because these prefixes are reserved for use by Amazon Web Services. When using @ReceiveMessage@ , you can send a list of attribute names to receive, or you can return all of the attributes by specifying "All" or ".*" in your request. You can also use "bar.*" to return all message attributes starting with the "bar" prefix.
--
-- * 'rmWaitTimeSeconds' - The duration (in seconds) for which the call will wait for a message to arrive in the queue before returning. If a message is available, the call will return sooner than WaitTimeSeconds.
--
-- * 'rmAttributeNames' - A list of attributes that need to be returned along with each message. These attributes include:     * @All@ - Returns all values.     * @ApproximateFirstReceiveTimestamp@ - Returns the time the message was first received from the queue (epoch time in milliseconds).     * @ApproximateReceiveCount@ - Returns the number of times a message has been received from the queue but not deleted.     * @SenderId@      * For an IAM user, returns the IAM user ID, for example @ABCDEFGHI1JKLMNOPQ23R@ .     * For an IAM role, returns the IAM role ID, for example @ABCDE1F2GH3I4JK5LMNOP:i-a123b456@ .     * @SentTimestamp@ - Returns the time the message was sent to the queue (epoch time in milliseconds).     * @MessageDeduplicationId@ - Returns the value provided by the sender that calls the @'SendMessage' @ action.     * @MessageGroupId@ - Returns the value provided by the sender that calls the @'SendMessage' @ action. Messages with the same @MessageGroupId@ are returned in sequence.     * @SequenceNumber@ - Returns the value provided by Amazon SQS. Any other valid special request parameters (such as the following) that are specified are ignored:     * @ApproximateNumberOfMessages@      * @ApproximateNumberOfMessagesDelayed@      * @ApproximateNumberOfMessagesNotVisible@      * @CreatedTimestamp@      * @ContentBasedDeduplication@      * @DelaySeconds@      * @LastModifiedTimestamp@      * @MaximumMessageSize@      * @MessageRetentionPeriod@      * @Policy@      * @QueueArn@ ,      * @ReceiveMessageWaitTimeSeconds@      * @RedrivePolicy@      * @FifoQueue@      * @VisibilityTimeout@
--
-- * 'rmMaxNumberOfMessages' - The maximum number of messages to return. Amazon SQS never returns more messages than this value but might return fewer. Values can be from 1 to 10. Default is 1. All of the messages are not necessarily returned.
--
-- * 'rmQueueURL' - The URL of the Amazon SQS queue to take action on. Queue URLs are case-sensitive.
receiveMessage
    :: Text -- ^ 'rmQueueURL'
    -> ReceiveMessage
receiveMessage pQueueURL_ =
    ReceiveMessage'
    { _rmReceiveRequestAttemptId = Nothing
    , _rmVisibilityTimeout = Nothing
    , _rmMessageAttributeNames = Nothing
    , _rmWaitTimeSeconds = Nothing
    , _rmAttributeNames = Nothing
    , _rmMaxNumberOfMessages = Nothing
    , _rmQueueURL = pQueueURL_
    }

-- | This parameter applies only to FIFO (first-in-first-out) queues. The token used for deduplication of @ReceiveMessage@ calls. If a networking issue occurs after a @ReceiveMessage@ action, and instead of a response you receive a generic error, you can retry the same action with an identical @ReceiveRequestAttemptId@ to retrieve the same set of messages, even if their visibility timeout has not yet expired.     * You can use @ReceiveRequestAttemptId@ only for 5 minutes after a @ReceiveMessage@ action.     * When you set @FifoQueue@ , a caller of the @ReceiveMessage@ action can provide a @ReceiveRequestAttemptId@ explicitly.     * If a caller of the @ReceiveMessage@ action doesn't provide a @ReceiveRequestAttemptId@ , Amazon SQS generates a @ReceiveRequestAttemptId@ .     * You can retry the @ReceiveMessage@ action with the same @ReceiveRequestAttemptId@ if none of the messages have been modified (deleted or had their visibility changes).     * During a visibility timeout, subsequent calls with the same @ReceiveRequestAttemptId@ return the same messages and receipt handles. If a retry occurs within the deduplication interval, it resets the visibility timeout. For more information, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/AboutVT.html Visibility Timeout> in the /Amazon Simple Queue Service Developer Guide/ . /Important:/ If a caller of the @ReceiveMessage@ action is still processing messages when the visibility timeout expires and messages become visible, another worker reading from the same queue can receive the same messages and therefore process duplicates. Also, if a reader whose message processing time is longer than the visibility timeout tries to delete the processed messages, the action fails with an error. To mitigate this effect, ensure that your application observes a safe threshold before the visibility timeout expires and extend the visibility timeout as necessary.     * While messages with a particular @MessageGroupId@ are invisible, no more messages belonging to the same @MessageGroupId@ are returned until the visibility timeout expires. You can still receive messages with another @MessageGroupId@ as long as it is also visible.     * If a caller of @ReceiveMessage@ can't track the @ReceiveRequestAttemptId@ , no retries will work until the original visibility timeout expires. As a result, delays might occur but the messages in the queue will remain in a strict order. The length of @ReceiveRequestAttemptId@ is 128 characters. @ReceiveRequestAttemptId@ can contain alphanumeric characters (@a-z@ , @A-Z@ , @0-9@ ) and punctuation (@!"#$%&'()*+,-./:;<=>?@[\]^_`{|}~@ ). For best practices of using @ReceiveRequestAttemptId@ , see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queue-recommendations.html#using-receiverequestattemptid-request-parameter Using the ReceiveRequestAttemptId Request Parameter> in the /Amazon Simple Queue Service Developer Guide/ .
rmReceiveRequestAttemptId :: Lens' ReceiveMessage (Maybe Text)
rmReceiveRequestAttemptId = lens _rmReceiveRequestAttemptId (\ s a -> s{_rmReceiveRequestAttemptId = a});

-- | The duration (in seconds) that the received messages are hidden from subsequent retrieve requests after being retrieved by a @ReceiveMessage@ request.
rmVisibilityTimeout :: Lens' ReceiveMessage (Maybe Int)
rmVisibilityTimeout = lens _rmVisibilityTimeout (\ s a -> s{_rmVisibilityTimeout = a});

-- | The name of the message attribute, where /N/ is the index. The message attribute name can contain the following characters: A-Z, a-z, 0-9, underscore (_), hyphen (-), and period (.). The name must not start or end with a period, and it should not have successive periods. The name is case sensitive and must be unique among all attribute names for the message. The name can be up to 256 characters long. The name can't start with "AWS." or "Amazon." (or any variations in casing), because these prefixes are reserved for use by Amazon Web Services. When using @ReceiveMessage@ , you can send a list of attribute names to receive, or you can return all of the attributes by specifying "All" or ".*" in your request. You can also use "bar.*" to return all message attributes starting with the "bar" prefix.
rmMessageAttributeNames :: Lens' ReceiveMessage [Text]
rmMessageAttributeNames = lens _rmMessageAttributeNames (\ s a -> s{_rmMessageAttributeNames = a}) . _Default . _Coerce;

-- | The duration (in seconds) for which the call will wait for a message to arrive in the queue before returning. If a message is available, the call will return sooner than WaitTimeSeconds.
rmWaitTimeSeconds :: Lens' ReceiveMessage (Maybe Int)
rmWaitTimeSeconds = lens _rmWaitTimeSeconds (\ s a -> s{_rmWaitTimeSeconds = a});

-- | A list of attributes that need to be returned along with each message. These attributes include:     * @All@ - Returns all values.     * @ApproximateFirstReceiveTimestamp@ - Returns the time the message was first received from the queue (epoch time in milliseconds).     * @ApproximateReceiveCount@ - Returns the number of times a message has been received from the queue but not deleted.     * @SenderId@      * For an IAM user, returns the IAM user ID, for example @ABCDEFGHI1JKLMNOPQ23R@ .     * For an IAM role, returns the IAM role ID, for example @ABCDE1F2GH3I4JK5LMNOP:i-a123b456@ .     * @SentTimestamp@ - Returns the time the message was sent to the queue (epoch time in milliseconds).     * @MessageDeduplicationId@ - Returns the value provided by the sender that calls the @'SendMessage' @ action.     * @MessageGroupId@ - Returns the value provided by the sender that calls the @'SendMessage' @ action. Messages with the same @MessageGroupId@ are returned in sequence.     * @SequenceNumber@ - Returns the value provided by Amazon SQS. Any other valid special request parameters (such as the following) that are specified are ignored:     * @ApproximateNumberOfMessages@      * @ApproximateNumberOfMessagesDelayed@      * @ApproximateNumberOfMessagesNotVisible@      * @CreatedTimestamp@      * @ContentBasedDeduplication@      * @DelaySeconds@      * @LastModifiedTimestamp@      * @MaximumMessageSize@      * @MessageRetentionPeriod@      * @Policy@      * @QueueArn@ ,      * @ReceiveMessageWaitTimeSeconds@      * @RedrivePolicy@      * @FifoQueue@      * @VisibilityTimeout@
rmAttributeNames :: Lens' ReceiveMessage [MessageAttribute]
rmAttributeNames = lens _rmAttributeNames (\ s a -> s{_rmAttributeNames = a}) . _Default . _Coerce;

-- | The maximum number of messages to return. Amazon SQS never returns more messages than this value but might return fewer. Values can be from 1 to 10. Default is 1. All of the messages are not necessarily returned.
rmMaxNumberOfMessages :: Lens' ReceiveMessage (Maybe Int)
rmMaxNumberOfMessages = lens _rmMaxNumberOfMessages (\ s a -> s{_rmMaxNumberOfMessages = a});

-- | The URL of the Amazon SQS queue to take action on. Queue URLs are case-sensitive.
rmQueueURL :: Lens' ReceiveMessage Text
rmQueueURL = lens _rmQueueURL (\ s a -> s{_rmQueueURL = a});

instance AWSRequest ReceiveMessage where
        type Rs ReceiveMessage = ReceiveMessageResponse
        request = postQuery sqs
        response
          = receiveXMLWrapper "ReceiveMessageResult"
              (\ s h x ->
                 ReceiveMessageResponse' <$>
                   (may (parseXMLList "Message") x) <*>
                     (pure (fromEnum s)))

instance Hashable ReceiveMessage

instance NFData ReceiveMessage

instance ToHeaders ReceiveMessage where
        toHeaders = const mempty

instance ToPath ReceiveMessage where
        toPath = const "/"

instance ToQuery ReceiveMessage where
        toQuery ReceiveMessage'{..}
          = mconcat
              ["Action" =: ("ReceiveMessage" :: ByteString),
               "Version" =: ("2012-11-05" :: ByteString),
               "ReceiveRequestAttemptId" =:
                 _rmReceiveRequestAttemptId,
               "VisibilityTimeout" =: _rmVisibilityTimeout,
               toQuery
                 (toQueryList "MessageAttributeName" <$>
                    _rmMessageAttributeNames),
               "WaitTimeSeconds" =: _rmWaitTimeSeconds,
               toQuery
                 (toQueryList "AttributeName" <$> _rmAttributeNames),
               "MaxNumberOfMessages" =: _rmMaxNumberOfMessages,
               "QueueUrl" =: _rmQueueURL]

-- | A list of received messages.
--
--
--
-- /See:/ 'receiveMessageResponse' smart constructor.
data ReceiveMessageResponse = ReceiveMessageResponse'
    { _rmrsMessages       :: !(Maybe [Message])
    , _rmrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ReceiveMessageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rmrsMessages' - A list of messages.
--
-- * 'rmrsResponseStatus' - -- | The response status code.
receiveMessageResponse
    :: Int -- ^ 'rmrsResponseStatus'
    -> ReceiveMessageResponse
receiveMessageResponse pResponseStatus_ =
    ReceiveMessageResponse'
    { _rmrsMessages = Nothing
    , _rmrsResponseStatus = pResponseStatus_
    }

-- | A list of messages.
rmrsMessages :: Lens' ReceiveMessageResponse [Message]
rmrsMessages = lens _rmrsMessages (\ s a -> s{_rmrsMessages = a}) . _Default . _Coerce;

-- | -- | The response status code.
rmrsResponseStatus :: Lens' ReceiveMessageResponse Int
rmrsResponseStatus = lens _rmrsResponseStatus (\ s a -> s{_rmrsResponseStatus = a});

instance NFData ReceiveMessageResponse
