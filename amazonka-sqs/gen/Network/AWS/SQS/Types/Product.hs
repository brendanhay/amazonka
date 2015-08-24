{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.Types.Product
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SQS.Types.Product where

import           Network.AWS.Prelude
import           Network.AWS.SQS.Types.Sum

-- | This is used in the responses of batch API to give a detailed
-- description of the result of an action on each entry in the request.
--
-- /See:/ 'batchResultErrorEntry' smart constructor.
data BatchResultErrorEntry = BatchResultErrorEntry'
    { _breeMessage     :: !(Maybe Text)
    , _breeId          :: !Text
    , _breeSenderFault :: !Bool
    , _breeCode        :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'BatchResultErrorEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'breeMessage'
--
-- * 'breeId'
--
-- * 'breeSenderFault'
--
-- * 'breeCode'
batchResultErrorEntry
    :: Text -- ^ 'breeId'
    -> Bool -- ^ 'breeSenderFault'
    -> Text -- ^ 'breeCode'
    -> BatchResultErrorEntry
batchResultErrorEntry pId_ pSenderFault_ pCode_ =
    BatchResultErrorEntry'
    { _breeMessage = Nothing
    , _breeId = pId_
    , _breeSenderFault = pSenderFault_
    , _breeCode = pCode_
    }

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
              (x .@? "Message") <*> (x .@ "Id") <*>
                (x .@ "SenderFault")
                <*> (x .@ "Code")

-- | Encloses a receipt handle and an entry id for each message in
-- ChangeMessageVisibilityBatch.
--
-- All of the following parameters are list parameters that must be
-- prefixed with 'ChangeMessageVisibilityBatchRequestEntry.n', where 'n' is
-- an integer value starting with 1. For example, a parameter list for this
-- action might look like this:
--
-- '&ChangeMessageVisibilityBatchRequestEntry.1.Id=change_visibility_msg_2'
--
-- '&ChangeMessageVisibilityBatchRequestEntry.1.ReceiptHandle=Your_Receipt_Handle'
--
-- '&ChangeMessageVisibilityBatchRequestEntry.1.VisibilityTimeout=45'
--
-- /See:/ 'changeMessageVisibilityBatchRequestEntry' smart constructor.
data ChangeMessageVisibilityBatchRequestEntry = ChangeMessageVisibilityBatchRequestEntry'
    { _cVisibilityTimeout :: !(Maybe Int)
    , _cId                :: !Text
    , _cReceiptHandle     :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ChangeMessageVisibilityBatchRequestEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cVisibilityTimeout'
--
-- * 'cId'
--
-- * 'cReceiptHandle'
changeMessageVisibilityBatchRequestEntry
    :: Text -- ^ 'cId'
    -> Text -- ^ 'cReceiptHandle'
    -> ChangeMessageVisibilityBatchRequestEntry
changeMessageVisibilityBatchRequestEntry pId_ pReceiptHandle_ =
    ChangeMessageVisibilityBatchRequestEntry'
    { _cVisibilityTimeout = Nothing
    , _cId = pId_
    , _cReceiptHandle = pReceiptHandle_
    }

-- | The new value (in seconds) for the message\'s visibility timeout.
cVisibilityTimeout :: Lens' ChangeMessageVisibilityBatchRequestEntry (Maybe Int)
cVisibilityTimeout = lens _cVisibilityTimeout (\ s a -> s{_cVisibilityTimeout = a});

-- | An identifier for this particular receipt handle. This is used to
-- communicate the result. Note that the 'Id's of a batch request need to
-- be unique within the request.
cId :: Lens' ChangeMessageVisibilityBatchRequestEntry Text
cId = lens _cId (\ s a -> s{_cId = a});

-- | A receipt handle.
cReceiptHandle :: Lens' ChangeMessageVisibilityBatchRequestEntry Text
cReceiptHandle = lens _cReceiptHandle (\ s a -> s{_cReceiptHandle = a});

instance ToQuery
         ChangeMessageVisibilityBatchRequestEntry where
        toQuery ChangeMessageVisibilityBatchRequestEntry'{..}
          = mconcat
              ["VisibilityTimeout" =: _cVisibilityTimeout,
               "Id" =: _cId, "ReceiptHandle" =: _cReceiptHandle]

-- | Encloses the id of an entry in ChangeMessageVisibilityBatch.
--
-- /See:/ 'changeMessageVisibilityBatchResultEntry' smart constructor.
newtype ChangeMessageVisibilityBatchResultEntry = ChangeMessageVisibilityBatchResultEntry'
    { _cmvbreId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ChangeMessageVisibilityBatchResultEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmvbreId'
changeMessageVisibilityBatchResultEntry
    :: Text -- ^ 'cmvbreId'
    -> ChangeMessageVisibilityBatchResultEntry
changeMessageVisibilityBatchResultEntry pId_ =
    ChangeMessageVisibilityBatchResultEntry'
    { _cmvbreId = pId_
    }

-- | Represents a message whose visibility timeout has been changed
-- successfully.
cmvbreId :: Lens' ChangeMessageVisibilityBatchResultEntry Text
cmvbreId = lens _cmvbreId (\ s a -> s{_cmvbreId = a});

instance FromXML
         ChangeMessageVisibilityBatchResultEntry where
        parseXML x
          = ChangeMessageVisibilityBatchResultEntry' <$>
              (x .@ "Id")

-- | Encloses a receipt handle and an identifier for it.
--
-- /See:/ 'deleteMessageBatchRequestEntry' smart constructor.
data DeleteMessageBatchRequestEntry = DeleteMessageBatchRequestEntry'
    { _dmbreId            :: !Text
    , _dmbreReceiptHandle :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteMessageBatchRequestEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmbreId'
--
-- * 'dmbreReceiptHandle'
deleteMessageBatchRequestEntry
    :: Text -- ^ 'dmbreId'
    -> Text -- ^ 'dmbreReceiptHandle'
    -> DeleteMessageBatchRequestEntry
deleteMessageBatchRequestEntry pId_ pReceiptHandle_ =
    DeleteMessageBatchRequestEntry'
    { _dmbreId = pId_
    , _dmbreReceiptHandle = pReceiptHandle_
    }

-- | An identifier for this particular receipt handle. This is used to
-- communicate the result. Note that the 'Id's of a batch request need to
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

-- | Encloses the id an entry in DeleteMessageBatch.
--
-- /See:/ 'deleteMessageBatchResultEntry' smart constructor.
newtype DeleteMessageBatchResultEntry = DeleteMessageBatchResultEntry'
    { _dId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteMessageBatchResultEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dId'
deleteMessageBatchResultEntry
    :: Text -- ^ 'dId'
    -> DeleteMessageBatchResultEntry
deleteMessageBatchResultEntry pId_ =
    DeleteMessageBatchResultEntry'
    { _dId = pId_
    }

-- | Represents a successfully deleted message.
dId :: Lens' DeleteMessageBatchResultEntry Text
dId = lens _dId (\ s a -> s{_dId = a});

instance FromXML DeleteMessageBatchResultEntry where
        parseXML x
          = DeleteMessageBatchResultEntry' <$> (x .@ "Id")

-- | An Amazon SQS message.
--
-- /See:/ 'message' smart constructor.
data Message = Message'
    { _mMessageAttributes      :: !(Maybe (Map Text MessageAttributeValue))
    , _mMD5OfBody              :: !(Maybe Text)
    , _mBody                   :: !(Maybe Text)
    , _mAttributes             :: !(Maybe (Map QueueAttributeName Text))
    , _mMessageId              :: !(Maybe Text)
    , _mReceiptHandle          :: !(Maybe Text)
    , _mMD5OfMessageAttributes :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Message' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mMessageAttributes'
--
-- * 'mMD5OfBody'
--
-- * 'mBody'
--
-- * 'mAttributes'
--
-- * 'mMessageId'
--
-- * 'mReceiptHandle'
--
-- * 'mMD5OfMessageAttributes'
message
    :: Message
message =
    Message'
    { _mMessageAttributes = Nothing
    , _mMD5OfBody = Nothing
    , _mBody = Nothing
    , _mAttributes = Nothing
    , _mMessageId = Nothing
    , _mReceiptHandle = Nothing
    , _mMD5OfMessageAttributes = Nothing
    }

-- | Each message attribute consists of a Name, Type, and Value. For more
-- information, see
-- <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/SQSMessageAttributes.html#SQSMessageAttributesNTV Message Attribute Items>.
mMessageAttributes :: Lens' Message (HashMap Text MessageAttributeValue)
mMessageAttributes = lens _mMessageAttributes (\ s a -> s{_mMessageAttributes = a}) . _Default . _Map;

-- | An MD5 digest of the non-URL-encoded message body string.
mMD5OfBody :: Lens' Message (Maybe Text)
mMD5OfBody = lens _mMD5OfBody (\ s a -> s{_mMD5OfBody = a});

-- | The message\'s contents (not URL-encoded).
mBody :: Lens' Message (Maybe Text)
mBody = lens _mBody (\ s a -> s{_mBody = a});

-- | 'SenderId', 'SentTimestamp', 'ApproximateReceiveCount', and\/or
-- 'ApproximateFirstReceiveTimestamp'. 'SentTimestamp' and
-- 'ApproximateFirstReceiveTimestamp' are each returned as an integer
-- representing the <http://en.wikipedia.org/wiki/Unix_time epoch time> in
-- milliseconds.
mAttributes :: Lens' Message (HashMap QueueAttributeName Text)
mAttributes = lens _mAttributes (\ s a -> s{_mAttributes = a}) . _Default . _Map;

-- | A unique identifier for the message. Message IDs are considered unique
-- across all AWS accounts for an extended period of time.
mMessageId :: Lens' Message (Maybe Text)
mMessageId = lens _mMessageId (\ s a -> s{_mMessageId = a});

-- | An identifier associated with the act of receiving the message. A new
-- receipt handle is returned every time you receive a message. When
-- deleting a message, you provide the last received receipt handle to
-- delete the message.
mReceiptHandle :: Lens' Message (Maybe Text)
mReceiptHandle = lens _mReceiptHandle (\ s a -> s{_mReceiptHandle = a});

-- | An MD5 digest of the non-URL-encoded message attribute string. This can
-- be used to verify that Amazon SQS received the message correctly. Amazon
-- SQS first URL decodes the message before creating the MD5 digest. For
-- information about MD5, go to <http://www.faqs.org/rfcs/rfc1321.html>.
mMD5OfMessageAttributes :: Lens' Message (Maybe Text)
mMD5OfMessageAttributes = lens _mMD5OfMessageAttributes (\ s a -> s{_mMD5OfMessageAttributes = a});

instance FromXML Message where
        parseXML x
          = Message' <$>
              (may (parseXMLMap "MessageAttribute" "Name" "Value")
                 x)
                <*> (x .@? "MD5OfBody")
                <*> (x .@? "Body")
                <*> (may (parseXMLMap "Attribute" "Name" "Value") x)
                <*> (x .@? "MessageId")
                <*> (x .@? "ReceiptHandle")
                <*> (x .@? "MD5OfMessageAttributes")

-- | The user-specified message attribute value. For string data types, the
-- value attribute has the same restrictions on the content as the message
-- body. For more information, see
-- <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_SendMessage.html SendMessage>.
--
-- Name, type, and value must not be empty or null. In addition, the
-- message body should not be empty or null. All parts of the message
-- attribute, including name, type, and value, are included in the message
-- size restriction, which is currently 256 KB (262,144 bytes).
--
-- /See:/ 'messageAttributeValue' smart constructor.
data MessageAttributeValue = MessageAttributeValue'
    { _mavBinaryValue      :: !(Maybe Base64)
    , _mavStringListValues :: !(Maybe [Text])
    , _mavStringValue      :: !(Maybe Text)
    , _mavBinaryListValues :: !(Maybe [Base64])
    , _mavDataType         :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'MessageAttributeValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
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
messageAttributeValue
    :: Text -- ^ 'mavDataType'
    -> MessageAttributeValue
messageAttributeValue pDataType_ =
    MessageAttributeValue'
    { _mavBinaryValue = Nothing
    , _mavStringListValues = Nothing
    , _mavStringValue = Nothing
    , _mavBinaryListValues = Nothing
    , _mavDataType = pDataType_
    }

-- | Binary type attributes can store any binary data, for example,
-- compressed data, encrypted data, or images.
--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data,
-- despite what the AWS documentation might say.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
mavBinaryValue :: Lens' MessageAttributeValue (Maybe ByteString)
mavBinaryValue = lens _mavBinaryValue (\ s a -> s{_mavBinaryValue = a}) . mapping _Base64;

-- | Not implemented. Reserved for future use.
mavStringListValues :: Lens' MessageAttributeValue [Text]
mavStringListValues = lens _mavStringListValues (\ s a -> s{_mavStringListValues = a}) . _Default . _Coerce;

-- | Strings are Unicode with UTF8 binary encoding. For a list of code
-- values, see
-- <http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters>.
mavStringValue :: Lens' MessageAttributeValue (Maybe Text)
mavStringValue = lens _mavStringValue (\ s a -> s{_mavStringValue = a});

-- | Not implemented. Reserved for future use.
mavBinaryListValues :: Lens' MessageAttributeValue [ByteString]
mavBinaryListValues = lens _mavBinaryListValues (\ s a -> s{_mavBinaryListValues = a}) . _Default . _Coerce;

-- | Amazon SQS supports the following logical data types: String, Number,
-- and Binary. In addition, you can append your own custom labels. For more
-- information, see
-- <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/SQSMessageAttributes.html#SQSMessageAttributes.DataTypes Message Attribute Data Types>.
mavDataType :: Lens' MessageAttributeValue Text
mavDataType = lens _mavDataType (\ s a -> s{_mavDataType = a});

instance FromXML MessageAttributeValue where
        parseXML x
          = MessageAttributeValue' <$>
              (x .@? "BinaryValue") <*>
                (x .@? "StringListValue" .!@ mempty >>=
                   may (parseXMLList "StringListValue"))
                <*> (x .@? "StringValue")
                <*>
                (x .@? "BinaryListValue" .!@ mempty >>=
                   may (parseXMLList "BinaryListValue"))
                <*> (x .@ "DataType")

instance ToQuery MessageAttributeValue where
        toQuery MessageAttributeValue'{..}
          = mconcat
              ["BinaryValue" =: _mavBinaryValue,
               "StringListValue" =:
                 toQuery
                   (toQueryList "StringListValue" <$>
                      _mavStringListValues),
               "StringValue" =: _mavStringValue,
               "BinaryListValue" =:
                 toQuery
                   (toQueryList "BinaryListValue" <$>
                      _mavBinaryListValues),
               "DataType" =: _mavDataType]

-- | Contains the details of a single Amazon SQS message along with a 'Id'.
--
-- /See:/ 'sendMessageBatchRequestEntry' smart constructor.
data SendMessageBatchRequestEntry = SendMessageBatchRequestEntry'
    { _sMessageAttributes :: !(Maybe (Map Text MessageAttributeValue))
    , _sDelaySeconds      :: !(Maybe Int)
    , _sId                :: !Text
    , _sMessageBody       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SendMessageBatchRequestEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sMessageAttributes'
--
-- * 'sDelaySeconds'
--
-- * 'sId'
--
-- * 'sMessageBody'
sendMessageBatchRequestEntry
    :: Text -- ^ 'sId'
    -> Text -- ^ 'sMessageBody'
    -> SendMessageBatchRequestEntry
sendMessageBatchRequestEntry pId_ pMessageBody_ =
    SendMessageBatchRequestEntry'
    { _sMessageAttributes = Nothing
    , _sDelaySeconds = Nothing
    , _sId = pId_
    , _sMessageBody = pMessageBody_
    }

-- | Each message attribute consists of a Name, Type, and Value. For more
-- information, see
-- <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/SQSMessageAttributes.html#SQSMessageAttributesNTV Message Attribute Items>.
sMessageAttributes :: Lens' SendMessageBatchRequestEntry (HashMap Text MessageAttributeValue)
sMessageAttributes = lens _sMessageAttributes (\ s a -> s{_sMessageAttributes = a}) . _Default . _Map;

-- | The number of seconds for which the message has to be delayed.
sDelaySeconds :: Lens' SendMessageBatchRequestEntry (Maybe Int)
sDelaySeconds = lens _sDelaySeconds (\ s a -> s{_sDelaySeconds = a});

-- | An identifier for the message in this batch. This is used to communicate
-- the result. Note that the 'Id's of a batch request need to be unique
-- within the request.
sId :: Lens' SendMessageBatchRequestEntry Text
sId = lens _sId (\ s a -> s{_sId = a});

-- | Body of the message.
sMessageBody :: Lens' SendMessageBatchRequestEntry Text
sMessageBody = lens _sMessageBody (\ s a -> s{_sMessageBody = a});

instance ToQuery SendMessageBatchRequestEntry where
        toQuery SendMessageBatchRequestEntry'{..}
          = mconcat
              [toQuery
                 (toQueryMap "MessageAttribute" "Name" "Value" <$>
                    _sMessageAttributes),
               "DelaySeconds" =: _sDelaySeconds, "Id" =: _sId,
               "MessageBody" =: _sMessageBody]

-- | Encloses a message ID for successfully enqueued message of a
-- SendMessageBatch.
--
-- /See:/ 'sendMessageBatchResultEntry' smart constructor.
data SendMessageBatchResultEntry = SendMessageBatchResultEntry'
    { _smbreMD5OfMessageAttributes :: !(Maybe Text)
    , _smbreId                     :: !Text
    , _smbreMessageId              :: !Text
    , _smbreMD5OfMessageBody       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SendMessageBatchResultEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smbreMD5OfMessageAttributes'
--
-- * 'smbreId'
--
-- * 'smbreMessageId'
--
-- * 'smbreMD5OfMessageBody'
sendMessageBatchResultEntry
    :: Text -- ^ 'smbreId'
    -> Text -- ^ 'smbreMessageId'
    -> Text -- ^ 'smbreMD5OfMessageBody'
    -> SendMessageBatchResultEntry
sendMessageBatchResultEntry pId_ pMessageId_ pMD5OfMessageBody_ =
    SendMessageBatchResultEntry'
    { _smbreMD5OfMessageAttributes = Nothing
    , _smbreId = pId_
    , _smbreMessageId = pMessageId_
    , _smbreMD5OfMessageBody = pMD5OfMessageBody_
    }

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
              (x .@? "MD5OfMessageAttributes") <*> (x .@ "Id") <*>
                (x .@ "MessageId")
                <*> (x .@ "MD5OfMessageBody")
