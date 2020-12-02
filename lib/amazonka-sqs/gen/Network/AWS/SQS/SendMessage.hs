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
-- Module      : Network.AWS.SQS.SendMessage
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delivers a message to the specified queue.
--
--
-- /Important:/ A message can include only XML, JSON, and unformatted text. The following Unicode characters are allowed:
--
-- @#x9@ | @#xA@ | @#xD@ | @#x20@ to @#xD7FF@ | @#xE000@ to @#xFFFD@ | @#x10000@ to @#x10FFFF@
--
-- Any characters not included in this list will be rejected. For more information, see the <http://www.w3.org/TR/REC-xml/#charsets W3C specification for characters> .
--
module Network.AWS.SQS.SendMessage
    (
    -- * Creating a Request
      sendMessage
    , SendMessage
    -- * Request Lenses
    , smMessageAttributes
    , smDelaySeconds
    , smMessageDeduplicationId
    , smMessageGroupId
    , smQueueURL
    , smMessageBody

    -- * Destructuring the Response
    , sendMessageResponse
    , SendMessageResponse
    -- * Response Lenses
    , smrsSequenceNumber
    , smrsMessageId
    , smrsMD5OfMessageBody
    , smrsMD5OfMessageAttributes
    , smrsResponseStatus
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
-- /See:/ 'sendMessage' smart constructor.
data SendMessage = SendMessage'
  { _smMessageAttributes      :: !(Maybe (Map Text MessageAttributeValue))
  , _smDelaySeconds           :: !(Maybe Int)
  , _smMessageDeduplicationId :: !(Maybe Text)
  , _smMessageGroupId         :: !(Maybe Text)
  , _smQueueURL               :: !Text
  , _smMessageBody            :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SendMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smMessageAttributes' - Each message attribute consists of a @Name@ , @Type@ , and @Value@ . For more information, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-attributes.html#message-attributes-items-validation Message Attribute Items and Validation> in the /Amazon Simple Queue Service Developer Guide/ .
--
-- * 'smDelaySeconds' - The length of time, in seconds, for which to delay a specific message. Valid values: 0 to 900. Maximum: 15 minutes. Messages with a positive @DelaySeconds@ value become available for processing after the delay period is finished. If you don't specify a value, the default value for the queue applies.
--
-- * 'smMessageDeduplicationId' - This parameter applies only to FIFO (first-in-first-out) queues. The token used for deduplication of sent messages. If a message with a particular @MessageDeduplicationId@ is sent successfully, any messages sent with the same @MessageDeduplicationId@ are accepted successfully but aren't delivered during the 5-minute deduplication interval. For more information, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html#FIFO-queues-exactly-once-processing Exactly-Once Processing> in the /Amazon Simple Queue Service Developer Guide/ .     * Every message must have a unique @MessageDeduplicationId@ ,     * You may provide a @MessageDeduplicationId@ explicitly.     * If you aren't able to provide a @MessageDeduplicationId@ and you enable @ContentBasedDeduplication@ for your queue, Amazon SQS uses a SHA-256 hash to generate the @MessageDeduplicationId@ using the body of the message (but not the attributes of the message).      * If you don't provide a @MessageDeduplicationId@ and the queue doesn't have @ContentBasedDeduplication@ set, the action fails with an error.     * If the queue has @ContentBasedDeduplication@ set, your @MessageDeduplicationId@ overrides the generated one.     * When @ContentBasedDeduplication@ is in effect, messages with identical content sent within the deduplication interval are treated as duplicates and only one copy of the message is delivered.     * If you send one message with @ContentBasedDeduplication@ enabled and then another message with a @MessageDeduplicationId@ that is the same as the one generated for the first @MessageDeduplicationId@ , the two messages are treated as duplicates and only one copy of the message is delivered.  The length of @MessageDeduplicationId@ is 128 characters. @MessageDeduplicationId@ can contain alphanumeric characters (@a-z@ , @A-Z@ , @0-9@ ) and punctuation (@!"#$%&'()*+,-./:;<=>?@[\]^_`{|}~@ ). For best practices of using @MessageDeduplicationId@ , see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queue-recommendations.html#using-messagededuplicationid-property Using the MessageDeduplicationId Property> in the /Amazon Simple Queue Service Developer Guide/ .
--
-- * 'smMessageGroupId' - This parameter applies only to FIFO (first-in-first-out) queues. The tag that specifies that a message belongs to a specific message group. Messages that belong to the same message group are processed in a FIFO manner (however, messages in different message groups might be processed out of order). To interleave multiple ordered streams within a single queue, use @MessageGroupId@ values (for example, session data for multiple users). In this scenario, multiple readers can process the queue, but the session data of each user is processed in a FIFO fashion.     * You must associate a non-empty @MessageGroupId@ with a message. If you don't provide a @MessageGroupId@ , the action fails.     * @ReceiveMessage@ might return messages with multiple @MessageGroupId@ values. For each @MessageGroupId@ , the messages are sorted by time sent. The caller can't specify a @MessageGroupId@ . The length of @MessageGroupId@ is 128 characters. Valid values are alphanumeric characters and punctuation @(!"#$%&'()*+,-./:;<=>?@[\]^_`{|}~)@ . For best practices of using @MessageGroupId@ , see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queue-recommendations.html#using-messagegroupid-property Using the MessageGroupId Property> in the /Amazon Simple Queue Service Developer Guide/ . /Important:/ @MessageGroupId@ is required for FIFO queues. You can't use it for Standard queues.
--
-- * 'smQueueURL' - The URL of the Amazon SQS queue to which a message is sent. Queue URLs are case-sensitive.
--
-- * 'smMessageBody' - The message to send. The maximum string size is 256 KB. /Important:/ A message can include only XML, JSON, and unformatted text. The following Unicode characters are allowed: @#x9@ | @#xA@ | @#xD@ | @#x20@ to @#xD7FF@ | @#xE000@ to @#xFFFD@ | @#x10000@ to @#x10FFFF@  Any characters not included in this list will be rejected. For more information, see the <http://www.w3.org/TR/REC-xml/#charsets W3C specification for characters> .
sendMessage
    :: Text -- ^ 'smQueueURL'
    -> Text -- ^ 'smMessageBody'
    -> SendMessage
sendMessage pQueueURL_ pMessageBody_ =
  SendMessage'
    { _smMessageAttributes = Nothing
    , _smDelaySeconds = Nothing
    , _smMessageDeduplicationId = Nothing
    , _smMessageGroupId = Nothing
    , _smQueueURL = pQueueURL_
    , _smMessageBody = pMessageBody_
    }


-- | Each message attribute consists of a @Name@ , @Type@ , and @Value@ . For more information, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-attributes.html#message-attributes-items-validation Message Attribute Items and Validation> in the /Amazon Simple Queue Service Developer Guide/ .
smMessageAttributes :: Lens' SendMessage (HashMap Text MessageAttributeValue)
smMessageAttributes = lens _smMessageAttributes (\ s a -> s{_smMessageAttributes = a}) . _Default . _Map

-- | The length of time, in seconds, for which to delay a specific message. Valid values: 0 to 900. Maximum: 15 minutes. Messages with a positive @DelaySeconds@ value become available for processing after the delay period is finished. If you don't specify a value, the default value for the queue applies.
smDelaySeconds :: Lens' SendMessage (Maybe Int)
smDelaySeconds = lens _smDelaySeconds (\ s a -> s{_smDelaySeconds = a})

-- | This parameter applies only to FIFO (first-in-first-out) queues. The token used for deduplication of sent messages. If a message with a particular @MessageDeduplicationId@ is sent successfully, any messages sent with the same @MessageDeduplicationId@ are accepted successfully but aren't delivered during the 5-minute deduplication interval. For more information, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html#FIFO-queues-exactly-once-processing Exactly-Once Processing> in the /Amazon Simple Queue Service Developer Guide/ .     * Every message must have a unique @MessageDeduplicationId@ ,     * You may provide a @MessageDeduplicationId@ explicitly.     * If you aren't able to provide a @MessageDeduplicationId@ and you enable @ContentBasedDeduplication@ for your queue, Amazon SQS uses a SHA-256 hash to generate the @MessageDeduplicationId@ using the body of the message (but not the attributes of the message).      * If you don't provide a @MessageDeduplicationId@ and the queue doesn't have @ContentBasedDeduplication@ set, the action fails with an error.     * If the queue has @ContentBasedDeduplication@ set, your @MessageDeduplicationId@ overrides the generated one.     * When @ContentBasedDeduplication@ is in effect, messages with identical content sent within the deduplication interval are treated as duplicates and only one copy of the message is delivered.     * If you send one message with @ContentBasedDeduplication@ enabled and then another message with a @MessageDeduplicationId@ that is the same as the one generated for the first @MessageDeduplicationId@ , the two messages are treated as duplicates and only one copy of the message is delivered.  The length of @MessageDeduplicationId@ is 128 characters. @MessageDeduplicationId@ can contain alphanumeric characters (@a-z@ , @A-Z@ , @0-9@ ) and punctuation (@!"#$%&'()*+,-./:;<=>?@[\]^_`{|}~@ ). For best practices of using @MessageDeduplicationId@ , see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queue-recommendations.html#using-messagededuplicationid-property Using the MessageDeduplicationId Property> in the /Amazon Simple Queue Service Developer Guide/ .
smMessageDeduplicationId :: Lens' SendMessage (Maybe Text)
smMessageDeduplicationId = lens _smMessageDeduplicationId (\ s a -> s{_smMessageDeduplicationId = a})

-- | This parameter applies only to FIFO (first-in-first-out) queues. The tag that specifies that a message belongs to a specific message group. Messages that belong to the same message group are processed in a FIFO manner (however, messages in different message groups might be processed out of order). To interleave multiple ordered streams within a single queue, use @MessageGroupId@ values (for example, session data for multiple users). In this scenario, multiple readers can process the queue, but the session data of each user is processed in a FIFO fashion.     * You must associate a non-empty @MessageGroupId@ with a message. If you don't provide a @MessageGroupId@ , the action fails.     * @ReceiveMessage@ might return messages with multiple @MessageGroupId@ values. For each @MessageGroupId@ , the messages are sorted by time sent. The caller can't specify a @MessageGroupId@ . The length of @MessageGroupId@ is 128 characters. Valid values are alphanumeric characters and punctuation @(!"#$%&'()*+,-./:;<=>?@[\]^_`{|}~)@ . For best practices of using @MessageGroupId@ , see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queue-recommendations.html#using-messagegroupid-property Using the MessageGroupId Property> in the /Amazon Simple Queue Service Developer Guide/ . /Important:/ @MessageGroupId@ is required for FIFO queues. You can't use it for Standard queues.
smMessageGroupId :: Lens' SendMessage (Maybe Text)
smMessageGroupId = lens _smMessageGroupId (\ s a -> s{_smMessageGroupId = a})

-- | The URL of the Amazon SQS queue to which a message is sent. Queue URLs are case-sensitive.
smQueueURL :: Lens' SendMessage Text
smQueueURL = lens _smQueueURL (\ s a -> s{_smQueueURL = a})

-- | The message to send. The maximum string size is 256 KB. /Important:/ A message can include only XML, JSON, and unformatted text. The following Unicode characters are allowed: @#x9@ | @#xA@ | @#xD@ | @#x20@ to @#xD7FF@ | @#xE000@ to @#xFFFD@ | @#x10000@ to @#x10FFFF@  Any characters not included in this list will be rejected. For more information, see the <http://www.w3.org/TR/REC-xml/#charsets W3C specification for characters> .
smMessageBody :: Lens' SendMessage Text
smMessageBody = lens _smMessageBody (\ s a -> s{_smMessageBody = a})

instance AWSRequest SendMessage where
        type Rs SendMessage = SendMessageResponse
        request = postQuery sqs
        response
          = receiveXMLWrapper "SendMessageResult"
              (\ s h x ->
                 SendMessageResponse' <$>
                   (x .@? "SequenceNumber") <*> (x .@? "MessageId") <*>
                     (x .@? "MD5OfMessageBody")
                     <*> (x .@? "MD5OfMessageAttributes")
                     <*> (pure (fromEnum s)))

instance Hashable SendMessage where

instance NFData SendMessage where

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
                    _smMessageAttributes),
               "DelaySeconds" =: _smDelaySeconds,
               "MessageDeduplicationId" =:
                 _smMessageDeduplicationId,
               "MessageGroupId" =: _smMessageGroupId,
               "QueueUrl" =: _smQueueURL,
               "MessageBody" =: _smMessageBody]

-- | The @MD5OfMessageBody@ and @MessageId@ elements.
--
--
--
-- /See:/ 'sendMessageResponse' smart constructor.
data SendMessageResponse = SendMessageResponse'
  { _smrsSequenceNumber         :: !(Maybe Text)
  , _smrsMessageId              :: !(Maybe Text)
  , _smrsMD5OfMessageBody       :: !(Maybe Text)
  , _smrsMD5OfMessageAttributes :: !(Maybe Text)
  , _smrsResponseStatus         :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SendMessageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smrsSequenceNumber' - This parameter applies only to FIFO (first-in-first-out) queues. The large, non-consecutive number that Amazon SQS assigns to each message. The length of @SequenceNumber@ is 128 bits. @SequenceNumber@ continues to increase for a particular @MessageGroupId@ .
--
-- * 'smrsMessageId' - An attribute containing the @MessageId@ of the message sent to the queue. For more information, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-queue-message-identifiers.html Queue and Message Identifiers> in the /Amazon Simple Queue Service Developer Guide/ .
--
-- * 'smrsMD5OfMessageBody' - An MD5 digest of the non-URL-encoded message attribute string. You can use this attribute to verify that Amazon SQS received the message correctly. Amazon SQS URL-decodes the message before creating the MD5 digest. For information about MD5, see <https://www.ietf.org/rfc/rfc1321.txt RFC1321> .
--
-- * 'smrsMD5OfMessageAttributes' - An MD5 digest of the non-URL-encoded message attribute string. You can use this attribute to verify that Amazon SQS received the message correctly. Amazon SQS URL-decodes the message before creating the MD5 digest. For information about MD5, see <https://www.ietf.org/rfc/rfc1321.txt RFC1321> .
--
-- * 'smrsResponseStatus' - -- | The response status code.
sendMessageResponse
    :: Int -- ^ 'smrsResponseStatus'
    -> SendMessageResponse
sendMessageResponse pResponseStatus_ =
  SendMessageResponse'
    { _smrsSequenceNumber = Nothing
    , _smrsMessageId = Nothing
    , _smrsMD5OfMessageBody = Nothing
    , _smrsMD5OfMessageAttributes = Nothing
    , _smrsResponseStatus = pResponseStatus_
    }


-- | This parameter applies only to FIFO (first-in-first-out) queues. The large, non-consecutive number that Amazon SQS assigns to each message. The length of @SequenceNumber@ is 128 bits. @SequenceNumber@ continues to increase for a particular @MessageGroupId@ .
smrsSequenceNumber :: Lens' SendMessageResponse (Maybe Text)
smrsSequenceNumber = lens _smrsSequenceNumber (\ s a -> s{_smrsSequenceNumber = a})

-- | An attribute containing the @MessageId@ of the message sent to the queue. For more information, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-queue-message-identifiers.html Queue and Message Identifiers> in the /Amazon Simple Queue Service Developer Guide/ .
smrsMessageId :: Lens' SendMessageResponse (Maybe Text)
smrsMessageId = lens _smrsMessageId (\ s a -> s{_smrsMessageId = a})

-- | An MD5 digest of the non-URL-encoded message attribute string. You can use this attribute to verify that Amazon SQS received the message correctly. Amazon SQS URL-decodes the message before creating the MD5 digest. For information about MD5, see <https://www.ietf.org/rfc/rfc1321.txt RFC1321> .
smrsMD5OfMessageBody :: Lens' SendMessageResponse (Maybe Text)
smrsMD5OfMessageBody = lens _smrsMD5OfMessageBody (\ s a -> s{_smrsMD5OfMessageBody = a})

-- | An MD5 digest of the non-URL-encoded message attribute string. You can use this attribute to verify that Amazon SQS received the message correctly. Amazon SQS URL-decodes the message before creating the MD5 digest. For information about MD5, see <https://www.ietf.org/rfc/rfc1321.txt RFC1321> .
smrsMD5OfMessageAttributes :: Lens' SendMessageResponse (Maybe Text)
smrsMD5OfMessageAttributes = lens _smrsMD5OfMessageAttributes (\ s a -> s{_smrsMD5OfMessageAttributes = a})

-- | -- | The response status code.
smrsResponseStatus :: Lens' SendMessageResponse Int
smrsResponseStatus = lens _smrsResponseStatus (\ s a -> s{_smrsResponseStatus = a})

instance NFData SendMessageResponse where
