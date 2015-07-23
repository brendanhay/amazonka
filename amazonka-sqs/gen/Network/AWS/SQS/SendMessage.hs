{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.SendMessage
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Delivers a message to the specified queue. With Amazon SQS, you now have
-- the ability to send large payload messages that are up to 256KB (262,144
-- bytes) in size. To send large payloads, you must use an AWS SDK that
-- supports SigV4 signing. To verify whether SigV4 is supported for an AWS
-- SDK, check the SDK release notes.
--
-- The following list shows the characters (in Unicode) allowed in your
-- message, according to the W3C XML specification. For more information,
-- go to <http://www.w3.org/TR/REC-xml/#charsets> If you send any
-- characters not included in the list, your request will be rejected.
--
-- #x9 | #xA | #xD | [#x20 to #xD7FF] | [#xE000 to #xFFFD] | [#x10000 to
-- #x10FFFF]
--
-- <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_SendMessage.html>
module Network.AWS.SQS.SendMessage
    (
    -- * Request
      SendMessage
    -- ** Request constructor
    , sendMessage
    -- ** Request lenses
    , smrqMessageAttributes
    , smrqDelaySeconds
    , smrqQueueURL
    , smrqMessageBody

    -- * Response
    , SendMessageResponse
    -- ** Response constructor
    , sendMessageResponse
    -- ** Response lenses
    , smrsMessageId
    , smrsMD5OfMessageBody
    , smrsMD5OfMessageAttributes
    , smrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SQS.Types

-- | /See:/ 'sendMessage' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'smrqMessageAttributes'
--
-- * 'smrqDelaySeconds'
--
-- * 'smrqQueueURL'
--
-- * 'smrqMessageBody'
data SendMessage = SendMessage'
    { _smrqMessageAttributes :: !(Maybe (Map Text MessageAttributeValue))
    , _smrqDelaySeconds      :: !(Maybe Int)
    , _smrqQueueURL          :: !Text
    , _smrqMessageBody       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SendMessage' smart constructor.
sendMessage :: Text -> Text -> SendMessage
sendMessage pQueueURL_ pMessageBody_ =
    SendMessage'
    { _smrqMessageAttributes = Nothing
    , _smrqDelaySeconds = Nothing
    , _smrqQueueURL = pQueueURL_
    , _smrqMessageBody = pMessageBody_
    }

-- | Each message attribute consists of a Name, Type, and Value. For more
-- information, see
-- <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/SQSMessageAttributes.html#SQSMessageAttributesNTV Message Attribute Items>.
smrqMessageAttributes :: Lens' SendMessage (HashMap Text MessageAttributeValue)
smrqMessageAttributes = lens _smrqMessageAttributes (\ s a -> s{_smrqMessageAttributes = a}) . _Default . _Map;

-- | The number of seconds (0 to 900 - 15 minutes) to delay a specific
-- message. Messages with a positive @DelaySeconds@ value become available
-- for processing after the delay time is finished. If you don\'t specify a
-- value, the default value for the queue applies.
smrqDelaySeconds :: Lens' SendMessage (Maybe Int)
smrqDelaySeconds = lens _smrqDelaySeconds (\ s a -> s{_smrqDelaySeconds = a});

-- | The URL of the Amazon SQS queue to take action on.
smrqQueueURL :: Lens' SendMessage Text
smrqQueueURL = lens _smrqQueueURL (\ s a -> s{_smrqQueueURL = a});

-- | The message to send. String maximum 256 KB in size. For a list of
-- allowed characters, see the preceding important note.
smrqMessageBody :: Lens' SendMessage Text
smrqMessageBody = lens _smrqMessageBody (\ s a -> s{_smrqMessageBody = a});

instance AWSRequest SendMessage where
        type Sv SendMessage = SQS
        type Rs SendMessage = SendMessageResponse
        request = post
        response
          = receiveXMLWrapper "SendMessageResult"
              (\ s h x ->
                 SendMessageResponse' <$>
                   (x .@? "MessageId") <*> (x .@? "MD5OfMessageBody")
                     <*> (x .@? "MD5OfMessageAttributes")
                     <*> (pure (fromEnum s)))

instance ToHeaders SendMessage where
        toHeaders = const mempty

instance ToPath SendMessage where
        toPath = const "/"

instance ToQuery SendMessage where
        toQuery SendMessage'{..}
          = mconcat
              ["Action" =: ("SendMessage" :: ByteString),
               "Version" =: ("2012-11-05" :: ByteString),
               toQuery
                 (toQueryMap "MessageAttribute" "Name" "Value" <$>
                    _smrqMessageAttributes),
               "DelaySeconds" =: _smrqDelaySeconds,
               "QueueUrl" =: _smrqQueueURL,
               "MessageBody" =: _smrqMessageBody]

-- | The MD5OfMessageBody and MessageId elements.
--
-- /See:/ 'sendMessageResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'smrsMessageId'
--
-- * 'smrsMD5OfMessageBody'
--
-- * 'smrsMD5OfMessageAttributes'
--
-- * 'smrsStatus'
data SendMessageResponse = SendMessageResponse'
    { _smrsMessageId              :: !(Maybe Text)
    , _smrsMD5OfMessageBody       :: !(Maybe Text)
    , _smrsMD5OfMessageAttributes :: !(Maybe Text)
    , _smrsStatus                 :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SendMessageResponse' smart constructor.
sendMessageResponse :: Int -> SendMessageResponse
sendMessageResponse pStatus_ =
    SendMessageResponse'
    { _smrsMessageId = Nothing
    , _smrsMD5OfMessageBody = Nothing
    , _smrsMD5OfMessageAttributes = Nothing
    , _smrsStatus = pStatus_
    }

-- | An element containing the message ID of the message sent to the queue.
-- For more information, see
-- <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/ImportantIdentifiers.html Queue and Message Identifiers>
-- in the /Amazon SQS Developer Guide/.
smrsMessageId :: Lens' SendMessageResponse (Maybe Text)
smrsMessageId = lens _smrsMessageId (\ s a -> s{_smrsMessageId = a});

-- | An MD5 digest of the non-URL-encoded message body string. This can be
-- used to verify that Amazon SQS received the message correctly. Amazon
-- SQS first URL decodes the message before creating the MD5 digest. For
-- information about MD5, go to <http://www.faqs.org/rfcs/rfc1321.html>.
smrsMD5OfMessageBody :: Lens' SendMessageResponse (Maybe Text)
smrsMD5OfMessageBody = lens _smrsMD5OfMessageBody (\ s a -> s{_smrsMD5OfMessageBody = a});

-- | An MD5 digest of the non-URL-encoded message attribute string. This can
-- be used to verify that Amazon SQS received the message correctly. Amazon
-- SQS first URL decodes the message before creating the MD5 digest. For
-- information about MD5, go to <http://www.faqs.org/rfcs/rfc1321.html>.
smrsMD5OfMessageAttributes :: Lens' SendMessageResponse (Maybe Text)
smrsMD5OfMessageAttributes = lens _smrsMD5OfMessageAttributes (\ s a -> s{_smrsMD5OfMessageAttributes = a});

-- | FIXME: Undocumented member.
smrsStatus :: Lens' SendMessageResponse Int
smrsStatus = lens _smrsStatus (\ s a -> s{_smrsStatus = a});
