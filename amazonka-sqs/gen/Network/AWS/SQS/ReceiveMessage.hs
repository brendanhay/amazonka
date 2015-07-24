{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.ReceiveMessage
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Retrieves one or more messages, with a maximum limit of 10 messages,
-- from the specified queue. Long poll support is enabled by using the
-- @WaitTimeSeconds@ parameter. For more information, see
-- <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-long-polling.html Amazon SQS Long Poll>
-- in the /Amazon SQS Developer Guide/.
--
-- Short poll is the default behavior where a weighted random set of
-- machines is sampled on a @ReceiveMessage@ call. This means only the
-- messages on the sampled machines are returned. If the number of messages
-- in the queue is small (less than 1000), it is likely you will get fewer
-- messages than you requested per @ReceiveMessage@ call. If the number of
-- messages in the queue is extremely small, you might not receive any
-- messages in a particular @ReceiveMessage@ response; in which case you
-- should repeat the request.
--
-- For each message returned, the response includes the following:
--
-- -   Message body
--
-- -   MD5 digest of the message body. For information about MD5, go to
--     <http://www.faqs.org/rfcs/rfc1321.html>.
--
-- -   Message ID you received when you sent the message to the queue.
--
-- -   Receipt handle.
--
-- -   Message attributes.
--
-- -   MD5 digest of the message attributes.
--
-- The receipt handle is the identifier you must provide when deleting the
-- message. For more information, see
-- <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/ImportantIdentifiers.html Queue and Message Identifiers>
-- in the /Amazon SQS Developer Guide/.
--
-- You can provide the @VisibilityTimeout@ parameter in your request, which
-- will be applied to the messages that Amazon SQS returns in the response.
-- If you do not include the parameter, the overall visibility timeout for
-- the queue is used for the returned messages. For more information, see
-- <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/AboutVT.html Visibility Timeout>
-- in the /Amazon SQS Developer Guide/.
--
-- Going forward, new attributes might be added. If you are writing code
-- that calls this action, we recommend that you structure your code so
-- that it can handle new attributes gracefully.
--
-- <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_ReceiveMessage.html>
module Network.AWS.SQS.ReceiveMessage
    (
    -- * Request
      ReceiveMessage
    -- ** Request constructor
    , receiveMessage
    -- ** Request lenses
    , rmVisibilityTimeout
    , rmMessageAttributeNames
    , rmWaitTimeSeconds
    , rmAttributeNames
    , rmMaxNumberOfMessages
    , rmQueueURL

    -- * Response
    , ReceiveMessageResponse
    -- ** Response constructor
    , receiveMessageResponse
    -- ** Response lenses
    , rmrsMessages
    , rmrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SQS.Types

-- | /See:/ 'receiveMessage' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rmVisibilityTimeout'
--
-- * 'rmMessageAttributeNames'
--
-- * 'rmWaitTimeSeconds'
--
-- * 'rmAttributeNames'
--
-- * 'rmMaxNumberOfMessages'
--
-- * 'rmQueueURL'
data ReceiveMessage = ReceiveMessage'
    { _rmVisibilityTimeout     :: !(Maybe Int)
    , _rmMessageAttributeNames :: !(Maybe [Text])
    , _rmWaitTimeSeconds       :: !(Maybe Int)
    , _rmAttributeNames        :: !(Maybe [QueueAttributeName])
    , _rmMaxNumberOfMessages   :: !(Maybe Int)
    , _rmQueueURL              :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ReceiveMessage' smart constructor.
receiveMessage :: Text -> ReceiveMessage
receiveMessage pQueueURL_ =
    ReceiveMessage'
    { _rmVisibilityTimeout = Nothing
    , _rmMessageAttributeNames = Nothing
    , _rmWaitTimeSeconds = Nothing
    , _rmAttributeNames = Nothing
    , _rmMaxNumberOfMessages = Nothing
    , _rmQueueURL = pQueueURL_
    }

-- | The duration (in seconds) that the received messages are hidden from
-- subsequent retrieve requests after being retrieved by a @ReceiveMessage@
-- request.
rmVisibilityTimeout :: Lens' ReceiveMessage (Maybe Int)
rmVisibilityTimeout = lens _rmVisibilityTimeout (\ s a -> s{_rmVisibilityTimeout = a});

-- | The name of the message attribute, where /N/ is the index. The message
-- attribute name can contain the following characters: A-Z, a-z, 0-9,
-- underscore (_), hyphen (-), and period (.). The name must not start or
-- end with a period, and it should not have successive periods. The name
-- is case sensitive and must be unique among all attribute names for the
-- message. The name can be up to 256 characters long. The name cannot
-- start with \"AWS.\" or \"Amazon.\" (or any variations in casing),
-- because these prefixes are reserved for use by Amazon Web Services.
--
-- When using @ReceiveMessage@, you can send a list of attribute names to
-- receive, or you can return all of the attributes by specifying \"All\"
-- or \".*\" in your request. You can also use \"foo.*\" to return all
-- message attributes starting with the \"foo\" prefix.
rmMessageAttributeNames :: Lens' ReceiveMessage [Text]
rmMessageAttributeNames = lens _rmMessageAttributeNames (\ s a -> s{_rmMessageAttributeNames = a}) . _Default;

-- | The duration (in seconds) for which the call will wait for a message to
-- arrive in the queue before returning. If a message is available, the
-- call will return sooner than WaitTimeSeconds.
rmWaitTimeSeconds :: Lens' ReceiveMessage (Maybe Int)
rmWaitTimeSeconds = lens _rmWaitTimeSeconds (\ s a -> s{_rmWaitTimeSeconds = a});

-- | A list of attributes that need to be returned along with each message.
--
-- The following lists the names and descriptions of the attributes that
-- can be returned:
--
-- -   @All@ - returns all values.
-- -   @ApproximateFirstReceiveTimestamp@ - returns the time when the
--     message was first received from the queue (epoch time in
--     milliseconds).
-- -   @ApproximateReceiveCount@ - returns the number of times a message
--     has been received from the queue but not deleted.
-- -   @SenderId@ - returns the AWS account number (or the IP address, if
--     anonymous access is allowed) of the sender.
-- -   @SentTimestamp@ - returns the time when the message was sent to the
--     queue (epoch time in milliseconds).
rmAttributeNames :: Lens' ReceiveMessage [QueueAttributeName]
rmAttributeNames = lens _rmAttributeNames (\ s a -> s{_rmAttributeNames = a}) . _Default;

-- | The maximum number of messages to return. Amazon SQS never returns more
-- messages than this value but may return fewer. Values can be from 1 to
-- 10. Default is 1.
--
-- All of the messages are not necessarily returned.
rmMaxNumberOfMessages :: Lens' ReceiveMessage (Maybe Int)
rmMaxNumberOfMessages = lens _rmMaxNumberOfMessages (\ s a -> s{_rmMaxNumberOfMessages = a});

-- | The URL of the Amazon SQS queue to take action on.
rmQueueURL :: Lens' ReceiveMessage Text
rmQueueURL = lens _rmQueueURL (\ s a -> s{_rmQueueURL = a});

instance AWSRequest ReceiveMessage where
        type Sv ReceiveMessage = SQS
        type Rs ReceiveMessage = ReceiveMessageResponse
        request = post "ReceiveMessage"
        response
          = receiveXMLWrapper "ReceiveMessageResult"
              (\ s h x ->
                 ReceiveMessageResponse' <$>
                   (may (parseXMLList "Message") x) <*>
                     (pure (fromEnum s)))

instance ToHeaders ReceiveMessage where
        toHeaders = const mempty

instance ToPath ReceiveMessage where
        toPath = const "/"

instance ToQuery ReceiveMessage where
        toQuery ReceiveMessage'{..}
          = mconcat
              ["Action" =: ("ReceiveMessage" :: ByteString),
               "Version" =: ("2012-11-05" :: ByteString),
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
-- /See:/ 'receiveMessageResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rmrsMessages'
--
-- * 'rmrsStatus'
data ReceiveMessageResponse = ReceiveMessageResponse'
    { _rmrsMessages :: !(Maybe [Message])
    , _rmrsStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ReceiveMessageResponse' smart constructor.
receiveMessageResponse :: Int -> ReceiveMessageResponse
receiveMessageResponse pStatus_ =
    ReceiveMessageResponse'
    { _rmrsMessages = Nothing
    , _rmrsStatus = pStatus_
    }

-- | A list of messages.
rmrsMessages :: Lens' ReceiveMessageResponse [Message]
rmrsMessages = lens _rmrsMessages (\ s a -> s{_rmrsMessages = a}) . _Default;

-- | FIXME: Undocumented member.
rmrsStatus :: Lens' ReceiveMessageResponse Int
rmrsStatus = lens _rmrsStatus (\ s a -> s{_rmrsStatus = a});
