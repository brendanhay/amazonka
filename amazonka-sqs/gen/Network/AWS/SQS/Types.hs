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

    -- * Errors
    , _InvalidBatchEntryId
    , _TooManyEntriesInBatchRequest
    , _InvalidAttributeName
    , _QueueDeletedRecently
    , _QueueDoesNotExist
    , _UnsupportedOperation
    , _InvalidMessageContents
    , _BatchRequestTooLong
    , _OverLimit
    , _PurgeQueueInProgress
    , _QueueNameExists
    , _InvalidIdFormat
    , _ReceiptHandleIsInvalid
    , _EmptyBatchRequest
    , _MessageNotInflight
    , _BatchEntryIdsNotDistinct

    -- * QueueAttributeName
    , QueueAttributeName (..)

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

    service = const svc
      where
        svc :: Service SQS
        svc = Service
            { _svcAbbrev   = "SQS"
            , _svcPrefix   = "sqs"
            , _svcVersion  = "2012-11-05"
            , _svcEndpoint = defaultEndpoint svc
            , _svcTimeout  = 80000000
            , _svcStatus   = statusSuccess
            , _svcError    = parseXMLError
            , _svcRetry    = retry
            }

        retry :: Retry
        retry = Exponential
            { _retryBase     = 0
            , _retryGrowth   = 0
            , _retryAttempts = 0
            , _retryCheck    = check
            }

        check :: ServiceError -> Bool
        check ServiceError'{..} = error "FIXME: Retry check not implemented."

-- | The @Id@ of a batch entry in a batch request does not abide by the
-- specification.
_InvalidBatchEntryId :: AWSError a => Geting (First ServiceError) a ServiceError
_InvalidBatchEntryId = _ServiceError . hasCode "AWS.SimpleQueueService.InvalidBatchEntryId" . hasStatus 400;

-- | Batch request contains more number of entries than permissible.
_TooManyEntriesInBatchRequest :: AWSError a => Geting (First ServiceError) a ServiceError
_TooManyEntriesInBatchRequest = _ServiceError . hasCode "AWS.SimpleQueueService.TooManyEntriesInBatchRequest" . hasStatus 400;

-- | The attribute referred to does not exist.
_InvalidAttributeName :: AWSError a => Geting (First ServiceError) a ServiceError
_InvalidAttributeName = _ServiceError . hasCode "InvalidAttributeName";

-- | You must wait 60 seconds after deleting a queue before you can create
-- another with the same name.
_QueueDeletedRecently :: AWSError a => Geting (First ServiceError) a ServiceError
_QueueDeletedRecently = _ServiceError . hasCode "AWS.SimpleQueueService.QueueDeletedRecently" . hasStatus 400;

-- | The queue referred to does not exist.
_QueueDoesNotExist :: AWSError a => Geting (First ServiceError) a ServiceError
_QueueDoesNotExist = _ServiceError . hasCode "AWS.SimpleQueueService.NonExistentQueue" . hasStatus 400;

-- | Error code 400. Unsupported operation.
_UnsupportedOperation :: AWSError a => Geting (First ServiceError) a ServiceError
_UnsupportedOperation = _ServiceError . hasCode "AWS.SimpleQueueService.UnsupportedOperation" . hasStatus 400;

-- | The message contains characters outside the allowed set.
_InvalidMessageContents :: AWSError a => Geting (First ServiceError) a ServiceError
_InvalidMessageContents = _ServiceError . hasCode "InvalidMessageContents";

-- | The length of all the messages put together is more than the limit.
_BatchRequestTooLong :: AWSError a => Geting (First ServiceError) a ServiceError
_BatchRequestTooLong = _ServiceError . hasCode "AWS.SimpleQueueService.BatchRequestTooLong" . hasStatus 400;

-- | The action that you requested would violate a limit. For example,
-- ReceiveMessage returns this error if the maximum number of messages
-- inflight has already been reached. AddPermission returns this error if
-- the maximum number of permissions for the queue has already been
-- reached.
_OverLimit :: AWSError a => Geting (First ServiceError) a ServiceError
_OverLimit = _ServiceError . hasCode "OverLimit" . hasStatus 403;

-- | Indicates that the specified queue previously received a @PurgeQueue@
-- request within the last 60 seconds, the time it can take to delete the
-- messages in the queue.
_PurgeQueueInProgress :: AWSError a => Geting (First ServiceError) a ServiceError
_PurgeQueueInProgress = _ServiceError . hasCode "AWS.SimpleQueueService.PurgeQueueInProgress" . hasStatus 403;

-- | A queue already exists with this name. Amazon SQS returns this error
-- only if the request includes attributes whose values differ from those
-- of the existing queue.
_QueueNameExists :: AWSError a => Geting (First ServiceError) a ServiceError
_QueueNameExists = _ServiceError . hasCode "QueueAlreadyExists" . hasStatus 400;

-- | The receipt handle is not valid for the current version.
_InvalidIdFormat :: AWSError a => Geting (First ServiceError) a ServiceError
_InvalidIdFormat = _ServiceError . hasCode "InvalidIdFormat";

-- | The receipt handle provided is not valid.
_ReceiptHandleIsInvalid :: AWSError a => Geting (First ServiceError) a ServiceError
_ReceiptHandleIsInvalid = _ServiceError . hasCode "ReceiptHandleIsInvalid";

-- | Batch request does not contain an entry.
_EmptyBatchRequest :: AWSError a => Geting (First ServiceError) a ServiceError
_EmptyBatchRequest = _ServiceError . hasCode "AWS.SimpleQueueService.EmptyBatchRequest" . hasStatus 400;

-- | The message referred to is not in flight.
_MessageNotInflight :: AWSError a => Geting (First ServiceError) a ServiceError
_MessageNotInflight = _ServiceError . hasCode "AWS.SimpleQueueService.MessageNotInflight" . hasStatus 400;

-- | Two or more batch entries have the same @Id@ in the request.
_BatchEntryIdsNotDistinct :: AWSError a => Geting (First ServiceError) a ServiceError
_BatchEntryIdsNotDistinct = _ServiceError . hasCode "AWS.SimpleQueueService.BatchEntryIdsNotDistinct" . hasStatus 400;

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

-- | This is used in the responses of batch API to give a detailed
-- description of the result of an action on each entry in the request.
--
-- /See:/ 'batchResultErrorEntry' smart constructor.
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
              (x .@? "Message") <*> (x .@ "Id") <*>
                (x .@ "SenderFault")
                <*> (x .@ "Code")

-- | Encloses a receipt handle and an entry id for each message in
-- ChangeMessageVisibilityBatch.
--
-- All of the following parameters are list parameters that must be
-- prefixed with @ChangeMessageVisibilityBatchRequestEntry.n@, where @n@ is
-- an integer value starting with 1. For example, a parameter list for this
-- action might look like this:
--
-- @&ChangeMessageVisibilityBatchRequestEntry.1.Id=change_visibility_msg_2@
--
-- @&ChangeMessageVisibilityBatchRequestEntry.1.ReceiptHandle=Your_Receipt_Handle@
--
-- @&ChangeMessageVisibilityBatchRequestEntry.1.VisibilityTimeout=45@
--
-- /See:/ 'changeMessageVisibilityBatchRequestEntry' smart constructor.
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

-- | Encloses the id of an entry in ChangeMessageVisibilityBatch.
--
-- /See:/ 'changeMessageVisibilityBatchResultEntry' smart constructor.
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
              (x .@ "Id")

-- | Encloses a receipt handle and an identifier for it.
--
-- /See:/ 'deleteMessageBatchRequestEntry' smart constructor.
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

-- | Encloses the id an entry in DeleteMessageBatch.
--
-- /See:/ 'deleteMessageBatchResultEntry' smart constructor.
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
          = DeleteMessageBatchResultEntry' <$> (x .@ "Id")

-- | An Amazon SQS message.
--
-- /See:/ 'message' smart constructor.
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
data Message = Message'{_mesMessageAttributes :: Maybe (Map Text MessageAttributeValue), _mesMD5OfBody :: Maybe Text, _mesBody :: Maybe Text, _mesAttributes :: Maybe (Map QueueAttributeName Text), _mesMessageId :: Maybe Text, _mesReceiptHandle :: Maybe Text, _mesMD5OfMessageAttributes :: Maybe Text} deriving (Eq, Read, Show)

-- | 'Message' smart constructor.
message :: Message
message = Message'{_mesMessageAttributes = Nothing, _mesMD5OfBody = Nothing, _mesBody = Nothing, _mesAttributes = Nothing, _mesMessageId = Nothing, _mesReceiptHandle = Nothing, _mesMD5OfMessageAttributes = Nothing};

-- | Each message attribute consists of a Name, Type, and Value. For more
-- information, see
-- <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/SQSMessageAttributes.html#SQSMessageAttributesNTV Message Attribute Items>.
mesMessageAttributes :: Lens' Message (HashMap Text MessageAttributeValue)
mesMessageAttributes = lens _mesMessageAttributes (\ s a -> s{_mesMessageAttributes = a}) . _Default . _Map;

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
mesAttributes = lens _mesAttributes (\ s a -> s{_mesAttributes = a}) . _Default . _Map;

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
data MessageAttributeValue = MessageAttributeValue'{_mavBinaryValue :: Maybe Base64, _mavStringListValues :: Maybe [Text], _mavStringValue :: Maybe Text, _mavBinaryListValues :: Maybe [Base64], _mavDataType :: Text} deriving (Eq, Read, Show)

-- | 'MessageAttributeValue' smart constructor.
messageAttributeValue :: Text -> MessageAttributeValue
messageAttributeValue pDataType = MessageAttributeValue'{_mavBinaryValue = Nothing, _mavStringListValues = Nothing, _mavStringValue = Nothing, _mavBinaryListValues = Nothing, _mavDataType = pDataType};

-- | Binary type attributes can store any binary data, for example,
-- compressed data, encrypted data, or images.
mavBinaryValue :: Lens' MessageAttributeValue (Maybe Base64)
mavBinaryValue = lens _mavBinaryValue (\ s a -> s{_mavBinaryValue = a});

-- | Not implemented. Reserved for future use.
mavStringListValues :: Lens' MessageAttributeValue [Text]
mavStringListValues = lens _mavStringListValues (\ s a -> s{_mavStringListValues = a}) . _Default;

-- | Strings are Unicode with UTF8 binary encoding. For a list of code
-- values, see
-- <http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters>.
mavStringValue :: Lens' MessageAttributeValue (Maybe Text)
mavStringValue = lens _mavStringValue (\ s a -> s{_mavStringValue = a});

-- | Not implemented. Reserved for future use.
mavBinaryListValues :: Lens' MessageAttributeValue [Base64]
mavBinaryListValues = lens _mavBinaryListValues (\ s a -> s{_mavBinaryListValues = a}) . _Default;

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

-- | Contains the details of a single Amazon SQS message along with a @Id@.
--
-- /See:/ 'sendMessageBatchRequestEntry' smart constructor.
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
data SendMessageBatchRequestEntry = SendMessageBatchRequestEntry'{_senMessageAttributes :: Maybe (Map Text MessageAttributeValue), _senDelaySeconds :: Maybe Int, _senId :: Text, _senMessageBody :: Text} deriving (Eq, Read, Show)

-- | 'SendMessageBatchRequestEntry' smart constructor.
sendMessageBatchRequestEntry :: Text -> Text -> SendMessageBatchRequestEntry
sendMessageBatchRequestEntry pId pMessageBody = SendMessageBatchRequestEntry'{_senMessageAttributes = Nothing, _senDelaySeconds = Nothing, _senId = pId, _senMessageBody = pMessageBody};

-- | Each message attribute consists of a Name, Type, and Value. For more
-- information, see
-- <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/SQSMessageAttributes.html#SQSMessageAttributesNTV Message Attribute Items>.
senMessageAttributes :: Lens' SendMessageBatchRequestEntry (HashMap Text MessageAttributeValue)
senMessageAttributes = lens _senMessageAttributes (\ s a -> s{_senMessageAttributes = a}) . _Default . _Map;

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
              [toQuery
                 (toQueryMap "MessageAttribute" "Name" "Value" <$>
                    _senMessageAttributes),
               "DelaySeconds" =: _senDelaySeconds, "Id" =: _senId,
               "MessageBody" =: _senMessageBody]

-- | Encloses a message ID for successfully enqueued message of a
-- SendMessageBatch.
--
-- /See:/ 'sendMessageBatchResultEntry' smart constructor.
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
              (x .@? "MD5OfMessageAttributes") <*> (x .@ "Id") <*>
                (x .@ "MessageId")
                <*> (x .@ "MD5OfMessageBody")
