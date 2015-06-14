{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Network.AWS.SQS.Types
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Network.AWS.SQS.Types
    (
    -- * Service
      SQS
    -- ** Errors
    , RESTError

    -- * BatchResultErrorEntry
    , BatchResultErrorEntry
    , batchResultErrorEntry
    , breeMessage
    , breeId
    , breeSenderFault
    , breeCode

    -- * ChangeMessageVisibilityBatchRequestEntry
    , ChangeMessageVisibilityBatchRequestEntry
    , changeMessageVisibilityBatchRequestEntry
    , chaVisibilityTimeout
    , chaId
    , chaReceiptHandle

    -- * ChangeMessageVisibilityBatchResultEntry
    , ChangeMessageVisibilityBatchResultEntry
    , changeMessageVisibilityBatchResultEntry
    , cmvbreId

    -- * DeleteMessageBatchRequestEntry
    , DeleteMessageBatchRequestEntry
    , deleteMessageBatchRequestEntry
    , dmbreId
    , dmbreReceiptHandle

    -- * DeleteMessageBatchResultEntry
    , DeleteMessageBatchResultEntry
    , deleteMessageBatchResultEntry
    , delId

    -- * Message
    , Message
    , message
    , mesMessageAttributes
    , mesMD5OfBody
    , mesBody
    , mesAttributes
    , mesMessageId
    , mesReceiptHandle
    , mesMD5OfMessageAttributes

    -- * MessageAttributeValue
    , MessageAttributeValue
    , messageAttributeValue
    , mavBinaryValue
    , mavStringListValues
    , mavStringValue
    , mavBinaryListValues
    , mavDataType

    -- * QueueAttributeName
    , QueueAttributeName (..)

    -- * SendMessageBatchRequestEntry
    , SendMessageBatchRequestEntry
    , sendMessageBatchRequestEntry
    , senMessageAttributes
    , senDelaySeconds
    , senId
    , senMessageBody

    -- * SendMessageBatchResultEntry
    , SendMessageBatchResultEntry
    , sendMessageBatchResultEntry
    , smbreMD5OfMessageAttributes
    , smbreId
    , smbreMessageId
    , smbreMD5OfMessageBody
    ) where

import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | Version @2012-11-05@ of the Amazon Simple Queue Service SDK.
data SQS

instance AWSService SQS where
    type Sg SQS = V4
    type Er SQS = RESTError

    service = service'
      where
        service' :: Service SQS
        service' = Service
            { _svcAbbrev  = "SQS"
            , _svcPrefix  = "sqs"
            , _svcVersion = "2012-11-05"
            , _svcHandle  = handle
            , _svcRetry   = retry
            }

        handle :: Status
               -> Maybe (LazyByteString -> ServiceError RESTError)
        handle = restError statusSuccess service'

        retry :: Retry SQS
        retry = undefined

        check :: Status
              -> RESTError
              -> Bool
        check (statusCode -> s) (awsErrorCode -> e) = undefined

-- | /See:/ 'batchResultErrorEntry' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'breeMessage'
--
-- * 'breeId'
--
-- * 'breeSenderFault'
--
-- * 'breeCode'
data BatchResultErrorEntry = BatchResultErrorEntry'{_breeMessage :: Maybe Text, _breeId :: Text, _breeSenderFault :: Bool, _breeCode :: Text} deriving (Eq, Read, Show)

-- | 'BatchResultErrorEntry' smart constructor.
batchResultErrorEntry :: Text -> Bool -> Text -> BatchResultErrorEntry
batchResultErrorEntry pId pSenderFault pCode = BatchResultErrorEntry'{_breeMessage = Nothing, _breeId = pId, _breeSenderFault = pSenderFault, _breeCode = pCode};

-- | A message explaining why the action failed on this entry.
breeMessage :: Lens' BatchResultErrorEntry (Maybe Text)
breeMessage = lens _breeMessage (\ s a -> s{_breeMessage = a});

-- | The id of an entry in a batch request.
breeId :: Lens' BatchResultErrorEntry Text
breeId = lens _breeId (\ s a -> s{_breeId = a});

-- | Whether the error happened due to the sender\'s fault.
breeSenderFault :: Lens' BatchResultErrorEntry Bool
breeSenderFault = lens _breeSenderFault (\ s a -> s{_breeSenderFault = a});

-- | An error code representing why the action failed on this entry.
breeCode :: Lens' BatchResultErrorEntry Text
breeCode = lens _breeCode (\ s a -> s{_breeCode = a});

instance FromXML BatchResultErrorEntry where
        parseXML x
          = BatchResultErrorEntry' <$>
              x .@? "Message" <*> x .@ "Id" <*> x .@ "SenderFault"
                <*> x .@ "Code"

-- | /See:/ 'changeMessageVisibilityBatchRequestEntry' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'chaVisibilityTimeout'
--
-- * 'chaId'
--
-- * 'chaReceiptHandle'
data ChangeMessageVisibilityBatchRequestEntry = ChangeMessageVisibilityBatchRequestEntry'{_chaVisibilityTimeout :: Maybe Int, _chaId :: Text, _chaReceiptHandle :: Text} deriving (Eq, Read, Show)

-- | 'ChangeMessageVisibilityBatchRequestEntry' smart constructor.
changeMessageVisibilityBatchRequestEntry :: Text -> Text -> ChangeMessageVisibilityBatchRequestEntry
changeMessageVisibilityBatchRequestEntry pId pReceiptHandle = ChangeMessageVisibilityBatchRequestEntry'{_chaVisibilityTimeout = Nothing, _chaId = pId, _chaReceiptHandle = pReceiptHandle};

-- | The new value (in seconds) for the message\'s visibility timeout.
chaVisibilityTimeout :: Lens' ChangeMessageVisibilityBatchRequestEntry (Maybe Int)
chaVisibilityTimeout = lens _chaVisibilityTimeout (\ s a -> s{_chaVisibilityTimeout = a});

-- | An identifier for this particular receipt handle. This is used to
-- communicate the result. Note that the @Id@s of a batch request need to
-- be unique within the request.
chaId :: Lens' ChangeMessageVisibilityBatchRequestEntry Text
chaId = lens _chaId (\ s a -> s{_chaId = a});

-- | A receipt handle.
chaReceiptHandle :: Lens' ChangeMessageVisibilityBatchRequestEntry Text
chaReceiptHandle = lens _chaReceiptHandle (\ s a -> s{_chaReceiptHandle = a});

instance ToQuery
         ChangeMessageVisibilityBatchRequestEntry where
        toQuery ChangeMessageVisibilityBatchRequestEntry'{..}
          = mconcat
              ["VisibilityTimeout" =: _chaVisibilityTimeout,
               "Id" =: _chaId, "ReceiptHandle" =: _chaReceiptHandle]

-- | /See:/ 'changeMessageVisibilityBatchResultEntry' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cmvbreId'
newtype ChangeMessageVisibilityBatchResultEntry = ChangeMessageVisibilityBatchResultEntry'{_cmvbreId :: Text} deriving (Eq, Read, Show)

-- | 'ChangeMessageVisibilityBatchResultEntry' smart constructor.
changeMessageVisibilityBatchResultEntry :: Text -> ChangeMessageVisibilityBatchResultEntry
changeMessageVisibilityBatchResultEntry pId = ChangeMessageVisibilityBatchResultEntry'{_cmvbreId = pId};

-- | Represents a message whose visibility timeout has been changed
-- successfully.
cmvbreId :: Lens' ChangeMessageVisibilityBatchResultEntry Text
cmvbreId = lens _cmvbreId (\ s a -> s{_cmvbreId = a});

instance FromXML
         ChangeMessageVisibilityBatchResultEntry where
        parseXML x
          = ChangeMessageVisibilityBatchResultEntry' <$>
              x .@ "Id"

-- | /See:/ 'deleteMessageBatchRequestEntry' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmbreId'
--
-- * 'dmbreReceiptHandle'
data DeleteMessageBatchRequestEntry = DeleteMessageBatchRequestEntry'{_dmbreId :: Text, _dmbreReceiptHandle :: Text} deriving (Eq, Read, Show)

-- | 'DeleteMessageBatchRequestEntry' smart constructor.
deleteMessageBatchRequestEntry :: Text -> Text -> DeleteMessageBatchRequestEntry
deleteMessageBatchRequestEntry pId pReceiptHandle = DeleteMessageBatchRequestEntry'{_dmbreId = pId, _dmbreReceiptHandle = pReceiptHandle};

-- | An identifier for this particular receipt handle. This is used to
-- communicate the result. Note that the @Id@s of a batch request need to
-- be unique within the request.
dmbreId :: Lens' DeleteMessageBatchRequestEntry Text
dmbreId = lens _dmbreId (\ s a -> s{_dmbreId = a});

-- | A receipt handle.
dmbreReceiptHandle :: Lens' DeleteMessageBatchRequestEntry Text
dmbreReceiptHandle = lens _dmbreReceiptHandle (\ s a -> s{_dmbreReceiptHandle = a});

instance ToQuery DeleteMessageBatchRequestEntry where
        toQuery DeleteMessageBatchRequestEntry'{..}
          = mconcat
              ["Id" =: _dmbreId,
               "ReceiptHandle" =: _dmbreReceiptHandle]

-- | /See:/ 'deleteMessageBatchResultEntry' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delId'
newtype DeleteMessageBatchResultEntry = DeleteMessageBatchResultEntry'{_delId :: Text} deriving (Eq, Read, Show)

-- | 'DeleteMessageBatchResultEntry' smart constructor.
deleteMessageBatchResultEntry :: Text -> DeleteMessageBatchResultEntry
deleteMessageBatchResultEntry pId = DeleteMessageBatchResultEntry'{_delId = pId};

-- | Represents a successfully deleted message.
delId :: Lens' DeleteMessageBatchResultEntry Text
delId = lens _delId (\ s a -> s{_delId = a});

instance FromXML DeleteMessageBatchResultEntry where
        parseXML x
          = DeleteMessageBatchResultEntry' <$> x .@ "Id"

-- | /See:/ 'message' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mesMessageAttributes'
--
-- * 'mesMD5OfBody'
--
-- * 'mesBody'
--
-- * 'mesAttributes'
--
-- * 'mesMessageId'
--
-- * 'mesReceiptHandle'
--
-- * 'mesMD5OfMessageAttributes'
data Message = Message'{_mesMessageAttributes :: HashMap Text MessageAttributeValue, _mesMD5OfBody :: Maybe Text, _mesBody :: Maybe Text, _mesAttributes :: HashMap QueueAttributeName Text, _mesMessageId :: Maybe Text, _mesReceiptHandle :: Maybe Text, _mesMD5OfMessageAttributes :: Maybe Text} deriving (Eq, Read, Show)

-- | 'Message' smart constructor.
message :: Message
message = Message'{_mesMessageAttributes = mempty, _mesMD5OfBody = Nothing, _mesBody = Nothing, _mesAttributes = mempty, _mesMessageId = Nothing, _mesReceiptHandle = Nothing, _mesMD5OfMessageAttributes = Nothing};

-- | Each message attribute consists of a Name, Type, and Value. For more
-- information, see
-- <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/SQSMessageAttributes.html#SQSMessageAttributesNTV Message Attribute Items>.
mesMessageAttributes :: Lens' Message (HashMap Text MessageAttributeValue)
mesMessageAttributes = lens _mesMessageAttributes (\ s a -> s{_mesMessageAttributes = a}) . _Coerce;

-- | An MD5 digest of the non-URL-encoded message body string.
mesMD5OfBody :: Lens' Message (Maybe Text)
mesMD5OfBody = lens _mesMD5OfBody (\ s a -> s{_mesMD5OfBody = a});

-- | The message\'s contents (not URL-encoded).
mesBody :: Lens' Message (Maybe Text)
mesBody = lens _mesBody (\ s a -> s{_mesBody = a});

-- | @SenderId@, @SentTimestamp@, @ApproximateReceiveCount@, and\/or
-- @ApproximateFirstReceiveTimestamp@. @SentTimestamp@ and
-- @ApproximateFirstReceiveTimestamp@ are each returned as an integer
-- representing the <http://en.wikipedia.org/wiki/Unix_time epoch time> in
-- milliseconds.
mesAttributes :: Lens' Message (HashMap QueueAttributeName Text)
mesAttributes = lens _mesAttributes (\ s a -> s{_mesAttributes = a}) . _Coerce;

-- | A unique identifier for the message. Message IDs are considered unique
-- across all AWS accounts for an extended period of time.
mesMessageId :: Lens' Message (Maybe Text)
mesMessageId = lens _mesMessageId (\ s a -> s{_mesMessageId = a});

-- | An identifier associated with the act of receiving the message. A new
-- receipt handle is returned every time you receive a message. When
-- deleting a message, you provide the last received receipt handle to
-- delete the message.
mesReceiptHandle :: Lens' Message (Maybe Text)
mesReceiptHandle = lens _mesReceiptHandle (\ s a -> s{_mesReceiptHandle = a});

-- | An MD5 digest of the non-URL-encoded message attribute string. This can
-- be used to verify that Amazon SQS received the message correctly. Amazon
-- SQS first URL decodes the message before creating the MD5 digest. For
-- information about MD5, go to <http://www.faqs.org/rfcs/rfc1321.html>.
mesMD5OfMessageAttributes :: Lens' Message (Maybe Text)
mesMD5OfMessageAttributes = lens _mesMD5OfMessageAttributes (\ s a -> s{_mesMD5OfMessageAttributes = a});

instance FromXML Message where
        parseXML x
          = Message' <$>
              parseXMLMap "MessageAttribute" "Name" "Value" x <*>
                x .@? "MD5OfBody"
                <*> x .@? "Body"
                <*> parseXMLMap "Attribute" "Name" "Value" x
                <*> x .@? "MessageId"
                <*> x .@? "ReceiptHandle"
                <*> x .@? "MD5OfMessageAttributes"

-- | /See:/ 'messageAttributeValue' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mavBinaryValue'
--
-- * 'mavStringListValues'
--
-- * 'mavStringValue'
--
-- * 'mavBinaryListValues'
--
-- * 'mavDataType'
data MessageAttributeValue = MessageAttributeValue'{_mavBinaryValue :: Maybe Base64, _mavStringListValues :: [Text], _mavStringValue :: Maybe Text, _mavBinaryListValues :: [Base64], _mavDataType :: Text} deriving (Eq, Read, Show)

-- | 'MessageAttributeValue' smart constructor.
messageAttributeValue :: Text -> MessageAttributeValue
messageAttributeValue pDataType = MessageAttributeValue'{_mavBinaryValue = Nothing, _mavStringListValues = mempty, _mavStringValue = Nothing, _mavBinaryListValues = mempty, _mavDataType = pDataType};

-- | Binary type attributes can store any binary data, for example,
-- compressed data, encrypted data, or images.
mavBinaryValue :: Lens' MessageAttributeValue (Maybe Base64)
mavBinaryValue = lens _mavBinaryValue (\ s a -> s{_mavBinaryValue = a});

-- | Not implemented. Reserved for future use.
mavStringListValues :: Lens' MessageAttributeValue [Text]
mavStringListValues = lens _mavStringListValues (\ s a -> s{_mavStringListValues = a});

-- | Strings are Unicode with UTF8 binary encoding. For a list of code
-- values, see
-- <http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters>.
mavStringValue :: Lens' MessageAttributeValue (Maybe Text)
mavStringValue = lens _mavStringValue (\ s a -> s{_mavStringValue = a});

-- | Not implemented. Reserved for future use.
mavBinaryListValues :: Lens' MessageAttributeValue [Base64]
mavBinaryListValues = lens _mavBinaryListValues (\ s a -> s{_mavBinaryListValues = a});

-- | Amazon SQS supports the following logical data types: String, Number,
-- and Binary. In addition, you can append your own custom labels. For more
-- information, see
-- <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/SQSMessageAttributes.html#SQSMessageAttributes.DataTypes Message Attribute Data Types>.
mavDataType :: Lens' MessageAttributeValue Text
mavDataType = lens _mavDataType (\ s a -> s{_mavDataType = a});

instance FromXML MessageAttributeValue where
        parseXML x
          = MessageAttributeValue' <$>
              x .@? "BinaryValue" <*>
                (x .@? "StringListValue" .!@ mempty >>=
                   parseXMLList "StringListValue")
                <*> x .@? "StringValue"
                <*>
                (x .@? "BinaryListValue" .!@ mempty >>=
                   parseXMLList "BinaryListValue")
                <*> x .@ "DataType"

instance ToQuery MessageAttributeValue where
        toQuery MessageAttributeValue'{..}
          = mconcat
              ["BinaryValue" =: _mavBinaryValue,
               "StringListValue" =:
                 "StringListValue" =: _mavStringListValues,
               "StringValue" =: _mavStringValue,
               "BinaryListValue" =:
                 "BinaryListValue" =: _mavBinaryListValues,
               "DataType" =: _mavDataType]

data QueueAttributeName = MessageRetentionPeriod | LastModifiedTimestamp | VisibilityTimeout | RedrivePolicy | ApproximateNumberOfMessagesDelayed | MaximumMessageSize | DelaySeconds | QueueARN | ApproximateNumberOfMessages | ReceiveMessageWaitTimeSeconds | Policy | CreatedTimestamp | ApproximateNumberOfMessagesNotVisible deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText QueueAttributeName where
    parser = takeLowerText >>= \case
        "ApproximateNumberOfMessages" -> pure ApproximateNumberOfMessages
        "ApproximateNumberOfMessagesDelayed" -> pure ApproximateNumberOfMessagesDelayed
        "ApproximateNumberOfMessagesNotVisible" -> pure ApproximateNumberOfMessagesNotVisible
        "CreatedTimestamp" -> pure CreatedTimestamp
        "DelaySeconds" -> pure DelaySeconds
        "LastModifiedTimestamp" -> pure LastModifiedTimestamp
        "MaximumMessageSize" -> pure MaximumMessageSize
        "MessageRetentionPeriod" -> pure MessageRetentionPeriod
        "Policy" -> pure Policy
        "QueueArn" -> pure QueueARN
        "ReceiveMessageWaitTimeSeconds" -> pure ReceiveMessageWaitTimeSeconds
        "RedrivePolicy" -> pure RedrivePolicy
        "VisibilityTimeout" -> pure VisibilityTimeout
        e -> fail ("Failure parsing QueueAttributeName from " ++ show e)

instance ToText QueueAttributeName where
    toText = \case
        ApproximateNumberOfMessages -> "ApproximateNumberOfMessages"
        ApproximateNumberOfMessagesDelayed -> "ApproximateNumberOfMessagesDelayed"
        ApproximateNumberOfMessagesNotVisible -> "ApproximateNumberOfMessagesNotVisible"
        CreatedTimestamp -> "CreatedTimestamp"
        DelaySeconds -> "DelaySeconds"
        LastModifiedTimestamp -> "LastModifiedTimestamp"
        MaximumMessageSize -> "MaximumMessageSize"
        MessageRetentionPeriod -> "MessageRetentionPeriod"
        Policy -> "Policy"
        QueueARN -> "QueueArn"
        ReceiveMessageWaitTimeSeconds -> "ReceiveMessageWaitTimeSeconds"
        RedrivePolicy -> "RedrivePolicy"
        VisibilityTimeout -> "VisibilityTimeout"

instance Hashable QueueAttributeName
instance ToQuery QueueAttributeName
instance ToHeader QueueAttributeName

instance FromXML QueueAttributeName where
    parseXML = parseXMLText "QueueAttributeName"

-- | /See:/ 'sendMessageBatchRequestEntry' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'senMessageAttributes'
--
-- * 'senDelaySeconds'
--
-- * 'senId'
--
-- * 'senMessageBody'
data SendMessageBatchRequestEntry = SendMessageBatchRequestEntry'{_senMessageAttributes :: HashMap Text MessageAttributeValue, _senDelaySeconds :: Maybe Int, _senId :: Text, _senMessageBody :: Text} deriving (Eq, Read, Show)

-- | 'SendMessageBatchRequestEntry' smart constructor.
sendMessageBatchRequestEntry :: Text -> Text -> SendMessageBatchRequestEntry
sendMessageBatchRequestEntry pId pMessageBody = SendMessageBatchRequestEntry'{_senMessageAttributes = mempty, _senDelaySeconds = Nothing, _senId = pId, _senMessageBody = pMessageBody};

-- | Each message attribute consists of a Name, Type, and Value. For more
-- information, see
-- <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/SQSMessageAttributes.html#SQSMessageAttributesNTV Message Attribute Items>.
senMessageAttributes :: Lens' SendMessageBatchRequestEntry (HashMap Text MessageAttributeValue)
senMessageAttributes = lens _senMessageAttributes (\ s a -> s{_senMessageAttributes = a}) . _Coerce;

-- | The number of seconds for which the message has to be delayed.
senDelaySeconds :: Lens' SendMessageBatchRequestEntry (Maybe Int)
senDelaySeconds = lens _senDelaySeconds (\ s a -> s{_senDelaySeconds = a});

-- | An identifier for the message in this batch. This is used to communicate
-- the result. Note that the @Id@s of a batch request need to be unique
-- within the request.
senId :: Lens' SendMessageBatchRequestEntry Text
senId = lens _senId (\ s a -> s{_senId = a});

-- | Body of the message.
senMessageBody :: Lens' SendMessageBatchRequestEntry Text
senMessageBody = lens _senMessageBody (\ s a -> s{_senMessageBody = a});

instance ToQuery SendMessageBatchRequestEntry where
        toQuery SendMessageBatchRequestEntry'{..}
          = mconcat
              [toQueryMap "MessageAttribute" "Name" "Value"
                 _senMessageAttributes,
               "DelaySeconds" =: _senDelaySeconds, "Id" =: _senId,
               "MessageBody" =: _senMessageBody]

-- | /See:/ 'sendMessageBatchResultEntry' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'smbreMD5OfMessageAttributes'
--
-- * 'smbreId'
--
-- * 'smbreMessageId'
--
-- * 'smbreMD5OfMessageBody'
data SendMessageBatchResultEntry = SendMessageBatchResultEntry'{_smbreMD5OfMessageAttributes :: Maybe Text, _smbreId :: Text, _smbreMessageId :: Text, _smbreMD5OfMessageBody :: Text} deriving (Eq, Read, Show)

-- | 'SendMessageBatchResultEntry' smart constructor.
sendMessageBatchResultEntry :: Text -> Text -> Text -> SendMessageBatchResultEntry
sendMessageBatchResultEntry pId pMessageId pMD5OfMessageBody = SendMessageBatchResultEntry'{_smbreMD5OfMessageAttributes = Nothing, _smbreId = pId, _smbreMessageId = pMessageId, _smbreMD5OfMessageBody = pMD5OfMessageBody};

-- | An MD5 digest of the non-URL-encoded message attribute string. This can
-- be used to verify that Amazon SQS received the message batch correctly.
-- Amazon SQS first URL decodes the message before creating the MD5 digest.
-- For information about MD5, go to
-- <http://www.faqs.org/rfcs/rfc1321.html>.
smbreMD5OfMessageAttributes :: Lens' SendMessageBatchResultEntry (Maybe Text)
smbreMD5OfMessageAttributes = lens _smbreMD5OfMessageAttributes (\ s a -> s{_smbreMD5OfMessageAttributes = a});

-- | An identifier for the message in this batch.
smbreId :: Lens' SendMessageBatchResultEntry Text
smbreId = lens _smbreId (\ s a -> s{_smbreId = a});

-- | An identifier for the message.
smbreMessageId :: Lens' SendMessageBatchResultEntry Text
smbreMessageId = lens _smbreMessageId (\ s a -> s{_smbreMessageId = a});

-- | An MD5 digest of the non-URL-encoded message body string. This can be
-- used to verify that Amazon SQS received the message correctly. Amazon
-- SQS first URL decodes the message before creating the MD5 digest. For
-- information about MD5, go to <http://www.faqs.org/rfcs/rfc1321.html>.
smbreMD5OfMessageBody :: Lens' SendMessageBatchResultEntry Text
smbreMD5OfMessageBody = lens _smbreMD5OfMessageBody (\ s a -> s{_smbreMD5OfMessageBody = a});

instance FromXML SendMessageBatchResultEntry where
        parseXML x
          = SendMessageBatchResultEntry' <$>
              x .@? "MD5OfMessageAttributes" <*> x .@ "Id" <*>
                x .@ "MessageId"
                <*> x .@ "MD5OfMessageBody"
