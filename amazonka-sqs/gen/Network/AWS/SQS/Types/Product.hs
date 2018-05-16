{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SQS.Types.Product where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SQS.Types.Sum

-- | This is used in the responses of batch API to give a detailed description of the result of an action on each entry in the request.
--
--
--
-- /See:/ 'batchResultErrorEntry' smart constructor.
data BatchResultErrorEntry = BatchResultErrorEntry'
  { _breeMessage     :: !(Maybe Text)
  , _breeId          :: !Text
  , _breeSenderFault :: !Bool
  , _breeCode        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchResultErrorEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'breeMessage' - A message explaining why the action failed on this entry.
--
-- * 'breeId' - The @Id@ of an entry in a batch request.
--
-- * 'breeSenderFault' - Specifies whether the error happened due to the sender's fault.
--
-- * 'breeCode' - An error code representing why the action failed on this entry.
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
breeMessage = lens _breeMessage (\ s a -> s{_breeMessage = a})

-- | The @Id@ of an entry in a batch request.
breeId :: Lens' BatchResultErrorEntry Text
breeId = lens _breeId (\ s a -> s{_breeId = a})

-- | Specifies whether the error happened due to the sender's fault.
breeSenderFault :: Lens' BatchResultErrorEntry Bool
breeSenderFault = lens _breeSenderFault (\ s a -> s{_breeSenderFault = a})

-- | An error code representing why the action failed on this entry.
breeCode :: Lens' BatchResultErrorEntry Text
breeCode = lens _breeCode (\ s a -> s{_breeCode = a})

instance FromXML BatchResultErrorEntry where
        parseXML x
          = BatchResultErrorEntry' <$>
              (x .@? "Message") <*> (x .@ "Id") <*>
                (x .@ "SenderFault")
                <*> (x .@ "Code")

instance Hashable BatchResultErrorEntry where

instance NFData BatchResultErrorEntry where

-- | Encloses a receipt handle and an entry id for each message in @'ChangeMessageVisibilityBatch' .@
--
--
-- /Important:/ All of the following list parameters must be prefixed with @ChangeMessageVisibilityBatchRequestEntry.n@ , where @n@ is an integer value starting with @1@ . For example, a parameter list for this action might look like this:
--
-- @&amp;ChangeMessageVisibilityBatchRequestEntry.1.Id=change_visibility_msg_2@
--
-- @&amp;ChangeMessageVisibilityBatchRequestEntry.1.ReceiptHandle=<replaceable>Your_Receipt_Handle</replaceable>@
--
-- @&amp;ChangeMessageVisibilityBatchRequestEntry.1.VisibilityTimeout=45@
--
--
-- /See:/ 'changeMessageVisibilityBatchRequestEntry' smart constructor.
data ChangeMessageVisibilityBatchRequestEntry = ChangeMessageVisibilityBatchRequestEntry'
  { _cVisibilityTimeout :: !(Maybe Int)
  , _cId                :: !Text
  , _cReceiptHandle     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ChangeMessageVisibilityBatchRequestEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cVisibilityTimeout' - The new value (in seconds) for the message's visibility timeout.
--
-- * 'cId' - An identifier for this particular receipt handle used to communicate the result.
--
-- * 'cReceiptHandle' - A receipt handle.
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


-- | The new value (in seconds) for the message's visibility timeout.
cVisibilityTimeout :: Lens' ChangeMessageVisibilityBatchRequestEntry (Maybe Int)
cVisibilityTimeout = lens _cVisibilityTimeout (\ s a -> s{_cVisibilityTimeout = a})

-- | An identifier for this particular receipt handle used to communicate the result.
cId :: Lens' ChangeMessageVisibilityBatchRequestEntry Text
cId = lens _cId (\ s a -> s{_cId = a})

-- | A receipt handle.
cReceiptHandle :: Lens' ChangeMessageVisibilityBatchRequestEntry Text
cReceiptHandle = lens _cReceiptHandle (\ s a -> s{_cReceiptHandle = a})

instance Hashable
           ChangeMessageVisibilityBatchRequestEntry
         where

instance NFData
           ChangeMessageVisibilityBatchRequestEntry
         where

instance ToQuery
           ChangeMessageVisibilityBatchRequestEntry
         where
        toQuery ChangeMessageVisibilityBatchRequestEntry'{..}
          = mconcat
              ["VisibilityTimeout" =: _cVisibilityTimeout,
               "Id" =: _cId, "ReceiptHandle" =: _cReceiptHandle]

-- | Encloses the @Id@ of an entry in @'ChangeMessageVisibilityBatch' .@
--
--
--
-- /See:/ 'changeMessageVisibilityBatchResultEntry' smart constructor.
newtype ChangeMessageVisibilityBatchResultEntry = ChangeMessageVisibilityBatchResultEntry'
  { _cmvbreId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ChangeMessageVisibilityBatchResultEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmvbreId' - Represents a message whose visibility timeout has been changed successfully.
changeMessageVisibilityBatchResultEntry
    :: Text -- ^ 'cmvbreId'
    -> ChangeMessageVisibilityBatchResultEntry
changeMessageVisibilityBatchResultEntry pId_ =
  ChangeMessageVisibilityBatchResultEntry' {_cmvbreId = pId_}


-- | Represents a message whose visibility timeout has been changed successfully.
cmvbreId :: Lens' ChangeMessageVisibilityBatchResultEntry Text
cmvbreId = lens _cmvbreId (\ s a -> s{_cmvbreId = a})

instance FromXML
           ChangeMessageVisibilityBatchResultEntry
         where
        parseXML x
          = ChangeMessageVisibilityBatchResultEntry' <$>
              (x .@ "Id")

instance Hashable
           ChangeMessageVisibilityBatchResultEntry
         where

instance NFData
           ChangeMessageVisibilityBatchResultEntry
         where

-- | Encloses a receipt handle and an identifier for it.
--
--
--
-- /See:/ 'deleteMessageBatchRequestEntry' smart constructor.
data DeleteMessageBatchRequestEntry = DeleteMessageBatchRequestEntry'
  { _dmbreId            :: !Text
  , _dmbreReceiptHandle :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteMessageBatchRequestEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmbreId' - An identifier for this particular receipt handle. This is used to communicate the result.
--
-- * 'dmbreReceiptHandle' - A receipt handle.
deleteMessageBatchRequestEntry
    :: Text -- ^ 'dmbreId'
    -> Text -- ^ 'dmbreReceiptHandle'
    -> DeleteMessageBatchRequestEntry
deleteMessageBatchRequestEntry pId_ pReceiptHandle_ =
  DeleteMessageBatchRequestEntry'
    {_dmbreId = pId_, _dmbreReceiptHandle = pReceiptHandle_}


-- | An identifier for this particular receipt handle. This is used to communicate the result.
dmbreId :: Lens' DeleteMessageBatchRequestEntry Text
dmbreId = lens _dmbreId (\ s a -> s{_dmbreId = a})

-- | A receipt handle.
dmbreReceiptHandle :: Lens' DeleteMessageBatchRequestEntry Text
dmbreReceiptHandle = lens _dmbreReceiptHandle (\ s a -> s{_dmbreReceiptHandle = a})

instance Hashable DeleteMessageBatchRequestEntry
         where

instance NFData DeleteMessageBatchRequestEntry where

instance ToQuery DeleteMessageBatchRequestEntry where
        toQuery DeleteMessageBatchRequestEntry'{..}
          = mconcat
              ["Id" =: _dmbreId,
               "ReceiptHandle" =: _dmbreReceiptHandle]

-- | Encloses the @Id@ of an entry in @'DeleteMessageBatch' .@
--
--
--
-- /See:/ 'deleteMessageBatchResultEntry' smart constructor.
newtype DeleteMessageBatchResultEntry = DeleteMessageBatchResultEntry'
  { _dId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteMessageBatchResultEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dId' - Represents a successfully deleted message.
deleteMessageBatchResultEntry
    :: Text -- ^ 'dId'
    -> DeleteMessageBatchResultEntry
deleteMessageBatchResultEntry pId_ =
  DeleteMessageBatchResultEntry' {_dId = pId_}


-- | Represents a successfully deleted message.
dId :: Lens' DeleteMessageBatchResultEntry Text
dId = lens _dId (\ s a -> s{_dId = a})

instance FromXML DeleteMessageBatchResultEntry where
        parseXML x
          = DeleteMessageBatchResultEntry' <$> (x .@ "Id")

instance Hashable DeleteMessageBatchResultEntry where

instance NFData DeleteMessageBatchResultEntry where

-- | An Amazon SQS message.
--
--
--
-- /See:/ 'message' smart constructor.
data Message = Message'
  { _mMessageAttributes      :: !(Maybe (Map Text MessageAttributeValue))
  , _mMD5OfBody              :: !(Maybe Text)
  , _mBody                   :: !(Maybe Text)
  , _mAttributes             :: !(Maybe (Map MessageAttribute Text))
  , _mReceiptHandle          :: !(Maybe Text)
  , _mMessageId              :: !(Maybe Text)
  , _mMD5OfMessageAttributes :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Message' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mMessageAttributes' - Each message attribute consists of a @Name@ , @Type@ , and @Value@ . For more information, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-attributes.html#message-attributes-items-validation Message Attribute Items and Validation> in the /Amazon Simple Queue Service Developer Guide/ .
--
-- * 'mMD5OfBody' - An MD5 digest of the non-URL-encoded message body string.
--
-- * 'mBody' - The message's contents (not URL-encoded).
--
-- * 'mAttributes' - @SenderId@ , @SentTimestamp@ , @ApproximateReceiveCount@ , and/or @ApproximateFirstReceiveTimestamp@ . @SentTimestamp@ and @ApproximateFirstReceiveTimestamp@ are each returned as an integer representing the <http://en.wikipedia.org/wiki/Unix_time epoch time> in milliseconds.
--
-- * 'mReceiptHandle' - An identifier associated with the act of receiving the message. A new receipt handle is returned every time you receive a message. When deleting a message, you provide the last received receipt handle to delete the message.
--
-- * 'mMessageId' - A unique identifier for the message. A @MessageId@ is considered unique across all AWS accounts for an extended period of time.
--
-- * 'mMD5OfMessageAttributes' - An MD5 digest of the non-URL-encoded message attribute string. You can use this attribute to verify that Amazon SQS received the message correctly. Amazon SQS URL-decodes the message before creating the MD5 digest. For information about MD5, see <https://www.ietf.org/rfc/rfc1321.txt RFC1321> .
message
    :: Message
message =
  Message'
    { _mMessageAttributes = Nothing
    , _mMD5OfBody = Nothing
    , _mBody = Nothing
    , _mAttributes = Nothing
    , _mReceiptHandle = Nothing
    , _mMessageId = Nothing
    , _mMD5OfMessageAttributes = Nothing
    }


-- | Each message attribute consists of a @Name@ , @Type@ , and @Value@ . For more information, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-attributes.html#message-attributes-items-validation Message Attribute Items and Validation> in the /Amazon Simple Queue Service Developer Guide/ .
mMessageAttributes :: Lens' Message (HashMap Text MessageAttributeValue)
mMessageAttributes = lens _mMessageAttributes (\ s a -> s{_mMessageAttributes = a}) . _Default . _Map

-- | An MD5 digest of the non-URL-encoded message body string.
mMD5OfBody :: Lens' Message (Maybe Text)
mMD5OfBody = lens _mMD5OfBody (\ s a -> s{_mMD5OfBody = a})

-- | The message's contents (not URL-encoded).
mBody :: Lens' Message (Maybe Text)
mBody = lens _mBody (\ s a -> s{_mBody = a})

-- | @SenderId@ , @SentTimestamp@ , @ApproximateReceiveCount@ , and/or @ApproximateFirstReceiveTimestamp@ . @SentTimestamp@ and @ApproximateFirstReceiveTimestamp@ are each returned as an integer representing the <http://en.wikipedia.org/wiki/Unix_time epoch time> in milliseconds.
mAttributes :: Lens' Message (HashMap MessageAttribute Text)
mAttributes = lens _mAttributes (\ s a -> s{_mAttributes = a}) . _Default . _Map

-- | An identifier associated with the act of receiving the message. A new receipt handle is returned every time you receive a message. When deleting a message, you provide the last received receipt handle to delete the message.
mReceiptHandle :: Lens' Message (Maybe Text)
mReceiptHandle = lens _mReceiptHandle (\ s a -> s{_mReceiptHandle = a})

-- | A unique identifier for the message. A @MessageId@ is considered unique across all AWS accounts for an extended period of time.
mMessageId :: Lens' Message (Maybe Text)
mMessageId = lens _mMessageId (\ s a -> s{_mMessageId = a})

-- | An MD5 digest of the non-URL-encoded message attribute string. You can use this attribute to verify that Amazon SQS received the message correctly. Amazon SQS URL-decodes the message before creating the MD5 digest. For information about MD5, see <https://www.ietf.org/rfc/rfc1321.txt RFC1321> .
mMD5OfMessageAttributes :: Lens' Message (Maybe Text)
mMD5OfMessageAttributes = lens _mMD5OfMessageAttributes (\ s a -> s{_mMD5OfMessageAttributes = a})

instance FromXML Message where
        parseXML x
          = Message' <$>
              (may (parseXMLMap "MessageAttribute" "Name" "Value")
                 x)
                <*> (x .@? "MD5OfBody")
                <*> (x .@? "Body")
                <*> (may (parseXMLMap "Attribute" "Name" "Value") x)
                <*> (x .@? "ReceiptHandle")
                <*> (x .@? "MessageId")
                <*> (x .@? "MD5OfMessageAttributes")

instance Hashable Message where

instance NFData Message where

-- | The user-specified message attribute value. For string data types, the @Value@ attribute has the same restrictions on the content as the message body. For more information, see @'SendMessage' .@
--
--
-- @Name@ , @type@ , @value@ and the message body must not be empty or null. All parts of the message attribute, including @Name@ , @Type@ , and @Value@ , are part of the message size restriction (256 KB or 262,144 bytes).
--
--
-- /See:/ 'messageAttributeValue' smart constructor.
data MessageAttributeValue = MessageAttributeValue'
  { _mavBinaryValue      :: !(Maybe Base64)
  , _mavStringListValues :: !(Maybe [Text])
  , _mavStringValue      :: !(Maybe Text)
  , _mavBinaryListValues :: !(Maybe [Base64])
  , _mavDataType         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MessageAttributeValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mavBinaryValue' - Binary type attributes can store any binary data, such as compressed data, encrypted data, or images.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'mavStringListValues' - Not implemented. Reserved for future use.
--
-- * 'mavStringValue' - Strings are Unicode with UTF-8 binary encoding. For a list of code values, see <http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters ASCII Printable Characters> .
--
-- * 'mavBinaryListValues' - Not implemented. Reserved for future use.
--
-- * 'mavDataType' - Amazon SQS supports the following logical data types: @String@ , @Number@ , and @Binary@ . For the @Number@ data type, you must use @StringValue@ . You can also append custom labels. For more information, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-attributes.html#message-attributes-data-types-validation Message Attribute Data Types and Validation> in the /Amazon Simple Queue Service Developer Guide/ .
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


-- | Binary type attributes can store any binary data, such as compressed data, encrypted data, or images.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
mavBinaryValue :: Lens' MessageAttributeValue (Maybe ByteString)
mavBinaryValue = lens _mavBinaryValue (\ s a -> s{_mavBinaryValue = a}) . mapping _Base64

-- | Not implemented. Reserved for future use.
mavStringListValues :: Lens' MessageAttributeValue [Text]
mavStringListValues = lens _mavStringListValues (\ s a -> s{_mavStringListValues = a}) . _Default . _Coerce

-- | Strings are Unicode with UTF-8 binary encoding. For a list of code values, see <http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters ASCII Printable Characters> .
mavStringValue :: Lens' MessageAttributeValue (Maybe Text)
mavStringValue = lens _mavStringValue (\ s a -> s{_mavStringValue = a})

-- | Not implemented. Reserved for future use.
mavBinaryListValues :: Lens' MessageAttributeValue [ByteString]
mavBinaryListValues = lens _mavBinaryListValues (\ s a -> s{_mavBinaryListValues = a}) . _Default . _Coerce

-- | Amazon SQS supports the following logical data types: @String@ , @Number@ , and @Binary@ . For the @Number@ data type, you must use @StringValue@ . You can also append custom labels. For more information, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-attributes.html#message-attributes-data-types-validation Message Attribute Data Types and Validation> in the /Amazon Simple Queue Service Developer Guide/ .
mavDataType :: Lens' MessageAttributeValue Text
mavDataType = lens _mavDataType (\ s a -> s{_mavDataType = a})

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

instance Hashable MessageAttributeValue where

instance NFData MessageAttributeValue where

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

-- | Contains the details of a single Amazon SQS message along with an @Id@ .
--
--
--
-- /See:/ 'sendMessageBatchRequestEntry' smart constructor.
data SendMessageBatchRequestEntry = SendMessageBatchRequestEntry'
  { _sMessageAttributes      :: !(Maybe (Map Text MessageAttributeValue))
  , _sDelaySeconds           :: !(Maybe Int)
  , _sMessageDeduplicationId :: !(Maybe Text)
  , _sMessageGroupId         :: !(Maybe Text)
  , _sId                     :: !Text
  , _sMessageBody            :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SendMessageBatchRequestEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sMessageAttributes' - Each message attribute consists of a @Name@ , @Type@ , and @Value@ . For more information, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-attributes.html#message-attributes-items-validation Message Attribute Items and Validation> in the /Amazon Simple Queue Service Developer Guide/ .
--
-- * 'sDelaySeconds' - The length of time, in seconds, for which a specific message is delayed. Valid values: 0 to 900. Maximum: 15 minutes. Messages with a positive @DelaySeconds@ value become available for processing after the delay period is finished. If you don't specify a value, the default value for the queue is applied.
--
-- * 'sMessageDeduplicationId' - This parameter applies only to FIFO (first-in-first-out) queues. The token used for deduplication of messages within a 5-minute minimum deduplication interval. If a message with a particular @MessageDeduplicationId@ is sent successfully, subsequent messages with the same @MessageDeduplicationId@ are accepted successfully but aren't delivered. For more information, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html#FIFO-queues-exactly-once-processing Exactly-Once Processing> in the /Amazon Simple Queue Service Developer Guide/ .     * Every message must have a unique @MessageDeduplicationId@ ,     * You may provide a @MessageDeduplicationId@ explicitly.     * If you aren't able to provide a @MessageDeduplicationId@ and you enable @ContentBasedDeduplication@ for your queue, Amazon SQS uses a SHA-256 hash to generate the @MessageDeduplicationId@ using the body of the message (but not the attributes of the message).      * If you don't provide a @MessageDeduplicationId@ and the queue doesn't have @ContentBasedDeduplication@ set, the action fails with an error.     * If the queue has @ContentBasedDeduplication@ set, your @MessageDeduplicationId@ overrides the generated one.     * When @ContentBasedDeduplication@ is in effect, messages with identical content sent within the deduplication interval are treated as duplicates and only one copy of the message is delivered.     * If you send one message with @ContentBasedDeduplication@ enabled and then another message with a @MessageDeduplicationId@ that is the same as the one generated for the first @MessageDeduplicationId@ , the two messages are treated as duplicates and only one copy of the message is delivered.  The length of @MessageDeduplicationId@ is 128 characters. @MessageDeduplicationId@ can contain alphanumeric characters (@a-z@ , @A-Z@ , @0-9@ ) and punctuation (@!"#$%&'()*+,-./:;<=>?@[\]^_`{|}~@ ). For best practices of using @MessageDeduplicationId@ , see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queue-recommendations.html#using-messagededuplicationid-property Using the MessageDeduplicationId Property> in the /Amazon Simple Queue Service Developer Guide/ .
--
-- * 'sMessageGroupId' - This parameter applies only to FIFO (first-in-first-out) queues. The tag that specifies that a message belongs to a specific message group. Messages that belong to the same message group are processed in a FIFO manner (however, messages in different message groups might be processed out of order). To interleave multiple ordered streams within a single queue, use @MessageGroupId@ values (for example, session data for multiple users). In this scenario, multiple readers can process the queue, but the session data of each user is processed in a FIFO fashion.     * You must associate a non-empty @MessageGroupId@ with a message. If you don't provide a @MessageGroupId@ , the action fails.     * @ReceiveMessage@ might return messages with multiple @MessageGroupId@ values. For each @MessageGroupId@ , the messages are sorted by time sent. The caller can't specify a @MessageGroupId@ . The length of @MessageGroupId@ is 128 characters. Valid values are alphanumeric characters and punctuation @(!"#$%&'()*+,-./:;<=>?@[\]^_`{|}~)@ . For best practices of using @MessageGroupId@ , see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queue-recommendations.html#using-messagegroupid-property Using the MessageGroupId Property> in the /Amazon Simple Queue Service Developer Guide/ . /Important:/ @MessageGroupId@ is required for FIFO queues. You can't use it for Standard queues.
--
-- * 'sId' - An identifier for a message in this batch used to communicate the result.
--
-- * 'sMessageBody' - The body of the message.
sendMessageBatchRequestEntry
    :: Text -- ^ 'sId'
    -> Text -- ^ 'sMessageBody'
    -> SendMessageBatchRequestEntry
sendMessageBatchRequestEntry pId_ pMessageBody_ =
  SendMessageBatchRequestEntry'
    { _sMessageAttributes = Nothing
    , _sDelaySeconds = Nothing
    , _sMessageDeduplicationId = Nothing
    , _sMessageGroupId = Nothing
    , _sId = pId_
    , _sMessageBody = pMessageBody_
    }


-- | Each message attribute consists of a @Name@ , @Type@ , and @Value@ . For more information, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-attributes.html#message-attributes-items-validation Message Attribute Items and Validation> in the /Amazon Simple Queue Service Developer Guide/ .
sMessageAttributes :: Lens' SendMessageBatchRequestEntry (HashMap Text MessageAttributeValue)
sMessageAttributes = lens _sMessageAttributes (\ s a -> s{_sMessageAttributes = a}) . _Default . _Map

-- | The length of time, in seconds, for which a specific message is delayed. Valid values: 0 to 900. Maximum: 15 minutes. Messages with a positive @DelaySeconds@ value become available for processing after the delay period is finished. If you don't specify a value, the default value for the queue is applied.
sDelaySeconds :: Lens' SendMessageBatchRequestEntry (Maybe Int)
sDelaySeconds = lens _sDelaySeconds (\ s a -> s{_sDelaySeconds = a})

-- | This parameter applies only to FIFO (first-in-first-out) queues. The token used for deduplication of messages within a 5-minute minimum deduplication interval. If a message with a particular @MessageDeduplicationId@ is sent successfully, subsequent messages with the same @MessageDeduplicationId@ are accepted successfully but aren't delivered. For more information, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html#FIFO-queues-exactly-once-processing Exactly-Once Processing> in the /Amazon Simple Queue Service Developer Guide/ .     * Every message must have a unique @MessageDeduplicationId@ ,     * You may provide a @MessageDeduplicationId@ explicitly.     * If you aren't able to provide a @MessageDeduplicationId@ and you enable @ContentBasedDeduplication@ for your queue, Amazon SQS uses a SHA-256 hash to generate the @MessageDeduplicationId@ using the body of the message (but not the attributes of the message).      * If you don't provide a @MessageDeduplicationId@ and the queue doesn't have @ContentBasedDeduplication@ set, the action fails with an error.     * If the queue has @ContentBasedDeduplication@ set, your @MessageDeduplicationId@ overrides the generated one.     * When @ContentBasedDeduplication@ is in effect, messages with identical content sent within the deduplication interval are treated as duplicates and only one copy of the message is delivered.     * If you send one message with @ContentBasedDeduplication@ enabled and then another message with a @MessageDeduplicationId@ that is the same as the one generated for the first @MessageDeduplicationId@ , the two messages are treated as duplicates and only one copy of the message is delivered.  The length of @MessageDeduplicationId@ is 128 characters. @MessageDeduplicationId@ can contain alphanumeric characters (@a-z@ , @A-Z@ , @0-9@ ) and punctuation (@!"#$%&'()*+,-./:;<=>?@[\]^_`{|}~@ ). For best practices of using @MessageDeduplicationId@ , see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queue-recommendations.html#using-messagededuplicationid-property Using the MessageDeduplicationId Property> in the /Amazon Simple Queue Service Developer Guide/ .
sMessageDeduplicationId :: Lens' SendMessageBatchRequestEntry (Maybe Text)
sMessageDeduplicationId = lens _sMessageDeduplicationId (\ s a -> s{_sMessageDeduplicationId = a})

-- | This parameter applies only to FIFO (first-in-first-out) queues. The tag that specifies that a message belongs to a specific message group. Messages that belong to the same message group are processed in a FIFO manner (however, messages in different message groups might be processed out of order). To interleave multiple ordered streams within a single queue, use @MessageGroupId@ values (for example, session data for multiple users). In this scenario, multiple readers can process the queue, but the session data of each user is processed in a FIFO fashion.     * You must associate a non-empty @MessageGroupId@ with a message. If you don't provide a @MessageGroupId@ , the action fails.     * @ReceiveMessage@ might return messages with multiple @MessageGroupId@ values. For each @MessageGroupId@ , the messages are sorted by time sent. The caller can't specify a @MessageGroupId@ . The length of @MessageGroupId@ is 128 characters. Valid values are alphanumeric characters and punctuation @(!"#$%&'()*+,-./:;<=>?@[\]^_`{|}~)@ . For best practices of using @MessageGroupId@ , see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queue-recommendations.html#using-messagegroupid-property Using the MessageGroupId Property> in the /Amazon Simple Queue Service Developer Guide/ . /Important:/ @MessageGroupId@ is required for FIFO queues. You can't use it for Standard queues.
sMessageGroupId :: Lens' SendMessageBatchRequestEntry (Maybe Text)
sMessageGroupId = lens _sMessageGroupId (\ s a -> s{_sMessageGroupId = a})

-- | An identifier for a message in this batch used to communicate the result.
sId :: Lens' SendMessageBatchRequestEntry Text
sId = lens _sId (\ s a -> s{_sId = a})

-- | The body of the message.
sMessageBody :: Lens' SendMessageBatchRequestEntry Text
sMessageBody = lens _sMessageBody (\ s a -> s{_sMessageBody = a})

instance Hashable SendMessageBatchRequestEntry where

instance NFData SendMessageBatchRequestEntry where

instance ToQuery SendMessageBatchRequestEntry where
        toQuery SendMessageBatchRequestEntry'{..}
          = mconcat
              [toQuery
                 (toQueryMap "MessageAttribute" "Name" "Value" <$>
                    _sMessageAttributes),
               "DelaySeconds" =: _sDelaySeconds,
               "MessageDeduplicationId" =: _sMessageDeduplicationId,
               "MessageGroupId" =: _sMessageGroupId, "Id" =: _sId,
               "MessageBody" =: _sMessageBody]

-- | Encloses a @MessageId@ for a successfully-enqueued message in a @'SendMessageBatch' .@
--
--
--
-- /See:/ 'sendMessageBatchResultEntry' smart constructor.
data SendMessageBatchResultEntry = SendMessageBatchResultEntry'
  { _smbreSequenceNumber         :: !(Maybe Text)
  , _smbreMD5OfMessageAttributes :: !(Maybe Text)
  , _smbreId                     :: !Text
  , _smbreMessageId              :: !Text
  , _smbreMD5OfMessageBody       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SendMessageBatchResultEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smbreSequenceNumber' - This parameter applies only to FIFO (first-in-first-out) queues. The large, non-consecutive number that Amazon SQS assigns to each message. The length of @SequenceNumber@ is 128 bits. As @SequenceNumber@ continues to increase for a particular @MessageGroupId@ .
--
-- * 'smbreMD5OfMessageAttributes' - An MD5 digest of the non-URL-encoded message attribute string. You can use this attribute to verify that Amazon SQS received the message correctly. Amazon SQS URL-decodes the message before creating the MD5 digest. For information about MD5, see <https://www.ietf.org/rfc/rfc1321.txt RFC1321> .
--
-- * 'smbreId' - An identifier for the message in this batch.
--
-- * 'smbreMessageId' - An identifier for the message.
--
-- * 'smbreMD5OfMessageBody' - An MD5 digest of the non-URL-encoded message attribute string. You can use this attribute to verify that Amazon SQS received the message correctly. Amazon SQS URL-decodes the message before creating the MD5 digest. For information about MD5, see <https://www.ietf.org/rfc/rfc1321.txt RFC1321> .
sendMessageBatchResultEntry
    :: Text -- ^ 'smbreId'
    -> Text -- ^ 'smbreMessageId'
    -> Text -- ^ 'smbreMD5OfMessageBody'
    -> SendMessageBatchResultEntry
sendMessageBatchResultEntry pId_ pMessageId_ pMD5OfMessageBody_ =
  SendMessageBatchResultEntry'
    { _smbreSequenceNumber = Nothing
    , _smbreMD5OfMessageAttributes = Nothing
    , _smbreId = pId_
    , _smbreMessageId = pMessageId_
    , _smbreMD5OfMessageBody = pMD5OfMessageBody_
    }


-- | This parameter applies only to FIFO (first-in-first-out) queues. The large, non-consecutive number that Amazon SQS assigns to each message. The length of @SequenceNumber@ is 128 bits. As @SequenceNumber@ continues to increase for a particular @MessageGroupId@ .
smbreSequenceNumber :: Lens' SendMessageBatchResultEntry (Maybe Text)
smbreSequenceNumber = lens _smbreSequenceNumber (\ s a -> s{_smbreSequenceNumber = a})

-- | An MD5 digest of the non-URL-encoded message attribute string. You can use this attribute to verify that Amazon SQS received the message correctly. Amazon SQS URL-decodes the message before creating the MD5 digest. For information about MD5, see <https://www.ietf.org/rfc/rfc1321.txt RFC1321> .
smbreMD5OfMessageAttributes :: Lens' SendMessageBatchResultEntry (Maybe Text)
smbreMD5OfMessageAttributes = lens _smbreMD5OfMessageAttributes (\ s a -> s{_smbreMD5OfMessageAttributes = a})

-- | An identifier for the message in this batch.
smbreId :: Lens' SendMessageBatchResultEntry Text
smbreId = lens _smbreId (\ s a -> s{_smbreId = a})

-- | An identifier for the message.
smbreMessageId :: Lens' SendMessageBatchResultEntry Text
smbreMessageId = lens _smbreMessageId (\ s a -> s{_smbreMessageId = a})

-- | An MD5 digest of the non-URL-encoded message attribute string. You can use this attribute to verify that Amazon SQS received the message correctly. Amazon SQS URL-decodes the message before creating the MD5 digest. For information about MD5, see <https://www.ietf.org/rfc/rfc1321.txt RFC1321> .
smbreMD5OfMessageBody :: Lens' SendMessageBatchResultEntry Text
smbreMD5OfMessageBody = lens _smbreMD5OfMessageBody (\ s a -> s{_smbreMD5OfMessageBody = a})

instance FromXML SendMessageBatchResultEntry where
        parseXML x
          = SendMessageBatchResultEntry' <$>
              (x .@? "SequenceNumber") <*>
                (x .@? "MD5OfMessageAttributes")
                <*> (x .@ "Id")
                <*> (x .@ "MessageId")
                <*> (x .@ "MD5OfMessageBody")

instance Hashable SendMessageBatchResultEntry where

instance NFData SendMessageBatchResultEntry where
