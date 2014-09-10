{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SQS.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon Simple Queue Service (SQS) is a fast, reliable, scalable, fully
-- managed message queuing service. SQS makes it simple and cost-effective to
-- decouple the components of a cloud application. You can use SQS to transmit
-- any volume of data, at any level of throughput, without losing messages or
-- requiring other services to be always available. With SQS, you can offload
-- the administrative burden of operating and scaling a highly available
-- messaging cluster, while paying a low price for only what you use.
module Network.AWS.SQS.Types
    (
    -- * Service
      SQS
    -- ** XML
    , xmlOptions

    -- * QueueAttributeName
    , QueueAttributeName (..)

    -- * ChangeMessageVisibilityBatchResultEntry
    , ChangeMessageVisibilityBatchResultEntry
    , mkChangeMessageVisibilityBatchResultEntry
    , cmvbrerId

    -- * DeleteMessageBatchResultEntry
    , DeleteMessageBatchResultEntry
    , mkDeleteMessageBatchResultEntry
    , dmbrerId

    -- * BatchResultErrorEntry
    , BatchResultErrorEntry
    , mkBatchResultErrorEntry
    , breeId
    , breeSenderFault
    , breeCode
    , breeMessage

    -- * ChangeMessageVisibilityBatchRequestEntry
    , ChangeMessageVisibilityBatchRequestEntry
    , mkChangeMessageVisibilityBatchRequestEntry
    , cmvbreId
    , cmvbreReceiptHandle
    , cmvbreVisibilityTimeout

    -- * DeleteMessageBatchRequestEntry
    , DeleteMessageBatchRequestEntry
    , mkDeleteMessageBatchRequestEntry
    , dmbreId
    , dmbreReceiptHandle

    -- * Message
    , Message
    , mkMessage
    , mMessageId
    , mReceiptHandle
    , mMD5OfBody
    , mBody
    , mAttributes
    , mMD5OfMessageAttributes
    , mMessageAttributes

    -- * MessageAttributeValue
    , MessageAttributeValue
    , mkMessageAttributeValue
    , mavStringValue
    , mavBinaryValue
    , mavStringListValues
    , mavBinaryListValues
    , mavDataType

    -- * SendMessageBatchRequestEntry
    , SendMessageBatchRequestEntry
    , mkSendMessageBatchRequestEntry
    , smbreId
    , smbreMessageBody
    , smbreDelaySeconds
    , smbreMessageAttributes

    -- * SendMessageBatchResultEntry
    , SendMessageBatchResultEntry
    , mkSendMessageBatchResultEntry
    , smbrerId
    , smbrerMessageId
    , smbrerMD5OfMessageBody
    , smbrerMD5OfMessageAttributes
    ) where

import Network.AWS.Prelude
import Network.AWS.Signing.V4

-- | Supported version (@2012-11-05@) of the
-- @Amazon Simple Queue Service@ service.
data SQS deriving (Typeable)

instance AWSService SQS where
    type Sg SQS = V4
    data Er SQS
        = BatchEntryIdsNotDistinct
        | BatchRequestTooLong
        | EmptyBatchRequest
        | InvalidAttributeName
        | InvalidBatchEntryId
        | InvalidIdFormat
        | InvalidMessageContents
        | MessageNotInflight
        | OverLimit
        | QueueDeletedRecently
        | QueueDoesNotExist
        | QueueNameExists
        | ReceiptHandleIsInvalid
        | SQSClient HttpException
        | SQSSerializer String
        | SQSService String
        | TooManyEntriesInBatchRequest
        | UnsupportedOperation

    service = Service'
        { _svcEndpoint = Regional
        , _svcPrefix   = "sqs"
        , _svcVersion  = "2012-11-05"
        , _svcTarget   = Nothing
        }

deriving instance Show    (Er SQS)
deriving instance Generic (Er SQS)

instance AWSError (Er SQS) where
    awsError = const "SQSError"

instance AWSServiceError (Er SQS) where
    serviceError    = SQSService
    clientError     = SQSClient
    serializerError = SQSSerializer

instance Exception (Er SQS)

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def

data QueueAttributeName
    = QueueAttributeNameApproximateNumberOfMessages -- ^ ApproximateNumberOfMessages
    | QueueAttributeNameApproximateNumberOfMessagesDelayed -- ^ ApproximateNumberOfMessagesDelayed
    | QueueAttributeNameApproximateNumberOfMessagesNotVisible -- ^ ApproximateNumberOfMessagesNotVisible
    | QueueAttributeNameCreatedTimestamp -- ^ CreatedTimestamp
    | QueueAttributeNameDelaySeconds -- ^ DelaySeconds
    | QueueAttributeNameLastModifiedTimestamp -- ^ LastModifiedTimestamp
    | QueueAttributeNameMaximumMessageSize -- ^ MaximumMessageSize
    | QueueAttributeNameMessageRetentionPeriod -- ^ MessageRetentionPeriod
    | QueueAttributeNamePolicy -- ^ Policy
    | QueueAttributeNameQueueArn -- ^ QueueArn
    | QueueAttributeNameReceiveMessageWaitTimeSeconds -- ^ ReceiveMessageWaitTimeSeconds
    | QueueAttributeNameRedrivePolicy -- ^ RedrivePolicy
    | QueueAttributeNameVisibilityTimeout -- ^ VisibilityTimeout
      deriving (Eq, Show, Generic)

instance Hashable QueueAttributeName

instance FromText QueueAttributeName where
    parser = match "ApproximateNumberOfMessages" QueueAttributeNameApproximateNumberOfMessages
         <|> match "ApproximateNumberOfMessagesDelayed" QueueAttributeNameApproximateNumberOfMessagesDelayed
         <|> match "ApproximateNumberOfMessagesNotVisible" QueueAttributeNameApproximateNumberOfMessagesNotVisible
         <|> match "CreatedTimestamp" QueueAttributeNameCreatedTimestamp
         <|> match "DelaySeconds" QueueAttributeNameDelaySeconds
         <|> match "LastModifiedTimestamp" QueueAttributeNameLastModifiedTimestamp
         <|> match "MaximumMessageSize" QueueAttributeNameMaximumMessageSize
         <|> match "MessageRetentionPeriod" QueueAttributeNameMessageRetentionPeriod
         <|> match "Policy" QueueAttributeNamePolicy
         <|> match "QueueArn" QueueAttributeNameQueueArn
         <|> match "ReceiveMessageWaitTimeSeconds" QueueAttributeNameReceiveMessageWaitTimeSeconds
         <|> match "RedrivePolicy" QueueAttributeNameRedrivePolicy
         <|> match "VisibilityTimeout" QueueAttributeNameVisibilityTimeout

instance ToText QueueAttributeName where
    toText QueueAttributeNameApproximateNumberOfMessages = "ApproximateNumberOfMessages"
    toText QueueAttributeNameApproximateNumberOfMessagesDelayed = "ApproximateNumberOfMessagesDelayed"
    toText QueueAttributeNameApproximateNumberOfMessagesNotVisible = "ApproximateNumberOfMessagesNotVisible"
    toText QueueAttributeNameCreatedTimestamp = "CreatedTimestamp"
    toText QueueAttributeNameDelaySeconds = "DelaySeconds"
    toText QueueAttributeNameLastModifiedTimestamp = "LastModifiedTimestamp"
    toText QueueAttributeNameMaximumMessageSize = "MaximumMessageSize"
    toText QueueAttributeNameMessageRetentionPeriod = "MessageRetentionPeriod"
    toText QueueAttributeNamePolicy = "Policy"
    toText QueueAttributeNameQueueArn = "QueueArn"
    toText QueueAttributeNameReceiveMessageWaitTimeSeconds = "ReceiveMessageWaitTimeSeconds"
    toText QueueAttributeNameRedrivePolicy = "RedrivePolicy"
    toText QueueAttributeNameVisibilityTimeout = "VisibilityTimeout"

instance ToByteString QueueAttributeName

instance FromXML QueueAttributeName where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Name"

instance ToQuery QueueAttributeName where
    toQuery = genericQuery def

-- | Encloses the id of an entry in ChangeMessageVisibilityBatch.
newtype ChangeMessageVisibilityBatchResultEntry = ChangeMessageVisibilityBatchResultEntry
    { _cmvbrerId :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ChangeMessageVisibilityBatchResultEntry' data type.
--
-- 'ChangeMessageVisibilityBatchResultEntry' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Id ::@ @Text@
--
mkChangeMessageVisibilityBatchResultEntry :: Text -- ^ 'cmvbrerId'
                                          -> ChangeMessageVisibilityBatchResultEntry
mkChangeMessageVisibilityBatchResultEntry p1 = ChangeMessageVisibilityBatchResultEntry
    { _cmvbrerId = p1
    }

-- | Represents a message whose visibility timeout has been changed
-- successfully.
cmvbrerId :: Lens' ChangeMessageVisibilityBatchResultEntry Text
cmvbrerId = lens _cmvbrerId (\s a -> s { _cmvbrerId = a })

instance FromXML ChangeMessageVisibilityBatchResultEntry where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ChangeMessageVisibilityBatchResultEntry"

-- | Encloses the id an entry in DeleteMessageBatch.
newtype DeleteMessageBatchResultEntry = DeleteMessageBatchResultEntry
    { _dmbrerId :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DeleteMessageBatchResultEntry' data type.
--
-- 'DeleteMessageBatchResultEntry' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Id ::@ @Text@
--
mkDeleteMessageBatchResultEntry :: Text -- ^ 'dmbrerId'
                                -> DeleteMessageBatchResultEntry
mkDeleteMessageBatchResultEntry p1 = DeleteMessageBatchResultEntry
    { _dmbrerId = p1
    }

-- | Represents a successfully deleted message.
dmbrerId :: Lens' DeleteMessageBatchResultEntry Text
dmbrerId = lens _dmbrerId (\s a -> s { _dmbrerId = a })

instance FromXML DeleteMessageBatchResultEntry where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteMessageBatchResultEntry"

-- | This is used in the responses of batch API to give a detailed description
-- of the result of an action on each entry in the request.
data BatchResultErrorEntry = BatchResultErrorEntry
    { _breeId :: !Text
    , _breeSenderFault :: !Bool
    , _breeCode :: !Text
    , _breeMessage :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'BatchResultErrorEntry' data type.
--
-- 'BatchResultErrorEntry' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Id ::@ @Text@
--
-- * @SenderFault ::@ @Bool@
--
-- * @Code ::@ @Text@
--
-- * @Message ::@ @Maybe Text@
--
mkBatchResultErrorEntry :: Text -- ^ 'breeId'
                        -> Bool -- ^ 'breeSenderFault'
                        -> Text -- ^ 'breeCode'
                        -> BatchResultErrorEntry
mkBatchResultErrorEntry p1 p2 p3 = BatchResultErrorEntry
    { _breeId = p1
    , _breeSenderFault = p2
    , _breeCode = p3
    , _breeMessage = Nothing
    }

-- | The id of an entry in a batch request.
breeId :: Lens' BatchResultErrorEntry Text
breeId = lens _breeId (\s a -> s { _breeId = a })

-- | Whether the error happened due to the sender's fault.
breeSenderFault :: Lens' BatchResultErrorEntry Bool
breeSenderFault = lens _breeSenderFault (\s a -> s { _breeSenderFault = a })

-- | An error code representing why the action failed on this entry.
breeCode :: Lens' BatchResultErrorEntry Text
breeCode = lens _breeCode (\s a -> s { _breeCode = a })

-- | A message explaining why the action failed on this entry.
breeMessage :: Lens' BatchResultErrorEntry (Maybe Text)
breeMessage = lens _breeMessage (\s a -> s { _breeMessage = a })

instance FromXML BatchResultErrorEntry where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "BatchResultErrorEntry"

-- | Encloses a receipt handle and an entry id for each message in
-- ChangeMessageVisibilityBatch. All of the following parameters are list
-- parameters that must be prefixed with
-- ChangeMessageVisibilityBatchRequestEntry.n, where n is an integer value
-- starting with 1. For example, a parameter list for this action might look
-- like this:
-- &amp;ChangeMessageVisibilityBatchRequestEntry.1.Id=change_visibility_msg_2
-- &amp;ChangeMessageVisibilityBatchRequestEntry.1.ReceiptHandle=Your_Receipt_Handle
-- &amp;ChangeMessageVisibilityBatchRequestEntry.1.VisibilityTimeout=45.
data ChangeMessageVisibilityBatchRequestEntry = ChangeMessageVisibilityBatchRequestEntry
    { _cmvbreId :: !Text
    , _cmvbreReceiptHandle :: !Text
    , _cmvbreVisibilityTimeout :: !(Maybe Integer)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ChangeMessageVisibilityBatchRequestEntry' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Id ::@ @Text@
--
-- * @ReceiptHandle ::@ @Text@
--
-- * @VisibilityTimeout ::@ @Maybe Integer@
--
mkChangeMessageVisibilityBatchRequestEntry :: Text -- ^ 'cmvbreId'
                                           -> Text -- ^ 'cmvbreReceiptHandle'
                                           -> ChangeMessageVisibilityBatchRequestEntry
mkChangeMessageVisibilityBatchRequestEntry p1 p2 = ChangeMessageVisibilityBatchRequestEntry
    { _cmvbreId = p1
    , _cmvbreReceiptHandle = p2
    , _cmvbreVisibilityTimeout = Nothing
    }

-- | An identifier for this particular receipt handle. This is used to
-- communicate the result. Note that the Ids of a batch request need to be
-- unique within the request.
cmvbreId :: Lens' ChangeMessageVisibilityBatchRequestEntry Text
cmvbreId = lens _cmvbreId (\s a -> s { _cmvbreId = a })

-- | A receipt handle.
cmvbreReceiptHandle :: Lens' ChangeMessageVisibilityBatchRequestEntry Text
cmvbreReceiptHandle =
    lens _cmvbreReceiptHandle (\s a -> s { _cmvbreReceiptHandle = a })

-- | The new value (in seconds) for the message's visibility timeout.
cmvbreVisibilityTimeout :: Lens' ChangeMessageVisibilityBatchRequestEntry (Maybe Integer)
cmvbreVisibilityTimeout =
    lens _cmvbreVisibilityTimeout
         (\s a -> s { _cmvbreVisibilityTimeout = a })

instance ToQuery ChangeMessageVisibilityBatchRequestEntry where
    toQuery = genericQuery def

-- | Encloses a receipt handle and an identifier for it.
data DeleteMessageBatchRequestEntry = DeleteMessageBatchRequestEntry
    { _dmbreId :: !Text
    , _dmbreReceiptHandle :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DeleteMessageBatchRequestEntry' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Id ::@ @Text@
--
-- * @ReceiptHandle ::@ @Text@
--
mkDeleteMessageBatchRequestEntry :: Text -- ^ 'dmbreId'
                                 -> Text -- ^ 'dmbreReceiptHandle'
                                 -> DeleteMessageBatchRequestEntry
mkDeleteMessageBatchRequestEntry p1 p2 = DeleteMessageBatchRequestEntry
    { _dmbreId = p1
    , _dmbreReceiptHandle = p2
    }

-- | An identifier for this particular receipt handle. This is used to
-- communicate the result. Note that the Ids of a batch request need to be
-- unique within the request.
dmbreId :: Lens' DeleteMessageBatchRequestEntry Text
dmbreId = lens _dmbreId (\s a -> s { _dmbreId = a })

-- | A receipt handle.
dmbreReceiptHandle :: Lens' DeleteMessageBatchRequestEntry Text
dmbreReceiptHandle =
    lens _dmbreReceiptHandle (\s a -> s { _dmbreReceiptHandle = a })

instance ToQuery DeleteMessageBatchRequestEntry where
    toQuery = genericQuery def

-- | An Amazon SQS message.
data Message = Message
    { _mMessageId :: !(Maybe Text)
    , _mReceiptHandle :: !(Maybe Text)
    , _mMD5OfBody :: !(Maybe Text)
    , _mBody :: !(Maybe Text)
    , _mAttributes :: Map QueueAttributeName Text
    , _mMD5OfMessageAttributes :: !(Maybe Text)
    , _mMessageAttributes :: Map Text MessageAttributeValue
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Message' data type.
--
-- 'Message' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @MessageId ::@ @Maybe Text@
--
-- * @ReceiptHandle ::@ @Maybe Text@
--
-- * @MD5OfBody ::@ @Maybe Text@
--
-- * @Body ::@ @Maybe Text@
--
-- * @Attributes ::@ @Map QueueAttributeName Text@
--
-- * @MD5OfMessageAttributes ::@ @Maybe Text@
--
-- * @MessageAttributes ::@ @Map Text MessageAttributeValue@
--
mkMessage :: Message
mkMessage = Message
    { _mMessageId = Nothing
    , _mReceiptHandle = Nothing
    , _mMD5OfBody = Nothing
    , _mBody = Nothing
    , _mAttributes = mempty
    , _mMD5OfMessageAttributes = Nothing
    , _mMessageAttributes = mempty
    }

-- | A unique identifier for the message. Message IDs are considered unique
-- across all AWS accounts for an extended period of time.
mMessageId :: Lens' Message (Maybe Text)
mMessageId = lens _mMessageId (\s a -> s { _mMessageId = a })

-- | An identifier associated with the act of receiving the message. A new
-- receipt handle is returned every time you receive a message. When deleting
-- a message, you provide the last received receipt handle to delete the
-- message.
mReceiptHandle :: Lens' Message (Maybe Text)
mReceiptHandle = lens _mReceiptHandle (\s a -> s { _mReceiptHandle = a })

-- | An MD5 digest of the non-URL-encoded message body string.
mMD5OfBody :: Lens' Message (Maybe Text)
mMD5OfBody = lens _mMD5OfBody (\s a -> s { _mMD5OfBody = a })

-- | The message's contents (not URL-encoded).
mBody :: Lens' Message (Maybe Text)
mBody = lens _mBody (\s a -> s { _mBody = a })

-- | SenderId, SentTimestamp, ApproximateReceiveCount, and/or
-- ApproximateFirstReceiveTimestamp. SentTimestamp and
-- ApproximateFirstReceiveTimestamp are each returned as an integer
-- representing the epoch time in milliseconds.
mAttributes :: Lens' Message (Map QueueAttributeName Text)
mAttributes = lens _mAttributes (\s a -> s { _mAttributes = a })

-- | An MD5 digest of the non-URL-encoded message attribute string. This can be
-- used to verify that Amazon SQS received the message correctly. Amazon SQS
-- first URL decodes the message before creating the MD5 digest. For
-- information about MD5, go to http://www.faqs.org/rfcs/rfc1321.html.
mMD5OfMessageAttributes :: Lens' Message (Maybe Text)
mMD5OfMessageAttributes =
    lens _mMD5OfMessageAttributes
         (\s a -> s { _mMD5OfMessageAttributes = a })

-- | Each message attribute consists of a Name, Type, and Value. For more
-- information, see Message Attribute Items.
mMessageAttributes :: Lens' Message (Map Text MessageAttributeValue)
mMessageAttributes =
    lens _mMessageAttributes (\s a -> s { _mMessageAttributes = a })

instance FromXML Message where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Message"

-- | The user-specified message attribute value. For string data types, the
-- value attribute has the same restrictions on the content as the message
-- body. For more information, see SendMessage. Name, type, and value must not
-- be empty or null. In addition, the message body should not be empty or
-- null. All parts of the message attribute, including name, type, and value,
-- are included in the message size restriction, which is currently 256 KB
-- (262,144 bytes).
data MessageAttributeValue = MessageAttributeValue
    { _mavStringValue :: !(Maybe Text)
    , _mavBinaryValue :: !(Maybe ByteString)
    , _mavStringListValues :: [Text]
    , _mavBinaryListValues :: [ByteString]
    , _mavDataType :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'MessageAttributeValue' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @StringValue ::@ @Maybe Text@
--
-- * @BinaryValue ::@ @Maybe ByteString@
--
-- * @StringListValues ::@ @[Text]@
--
-- * @BinaryListValues ::@ @[ByteString]@
--
-- * @DataType ::@ @Text@
--
mkMessageAttributeValue :: Text -- ^ 'mavDataType'
                        -> MessageAttributeValue
mkMessageAttributeValue p5 = MessageAttributeValue
    { _mavStringValue = Nothing
    , _mavBinaryValue = Nothing
    , _mavStringListValues = mempty
    , _mavBinaryListValues = mempty
    , _mavDataType = p5
    }

-- | Strings are Unicode with UTF8 binary encoding. For a list of code values,
-- see http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters.
mavStringValue :: Lens' MessageAttributeValue (Maybe Text)
mavStringValue = lens _mavStringValue (\s a -> s { _mavStringValue = a })

-- | Binary type attributes can store any binary data, for example, compressed
-- data, encrypted data, or images.
mavBinaryValue :: Lens' MessageAttributeValue (Maybe ByteString)
mavBinaryValue = lens _mavBinaryValue (\s a -> s { _mavBinaryValue = a })

-- | Not implemented. Reserved for future use.
mavStringListValues :: Lens' MessageAttributeValue [Text]
mavStringListValues =
    lens _mavStringListValues (\s a -> s { _mavStringListValues = a })

-- | Not implemented. Reserved for future use.
mavBinaryListValues :: Lens' MessageAttributeValue [ByteString]
mavBinaryListValues =
    lens _mavBinaryListValues (\s a -> s { _mavBinaryListValues = a })

-- | Amazon SQS supports the following logical data types: String, Number, and
-- Binary. In addition, you can append your own custom labels. For more
-- information, see Message Attribute Data Types.
mavDataType :: Lens' MessageAttributeValue Text
mavDataType = lens _mavDataType (\s a -> s { _mavDataType = a })

instance FromXML MessageAttributeValue where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Value"

instance ToQuery MessageAttributeValue where
    toQuery = genericQuery def

-- | Contains the details of a single Amazon SQS message along with a Id.
data SendMessageBatchRequestEntry = SendMessageBatchRequestEntry
    { _smbreId :: !Text
    , _smbreMessageBody :: !Text
    , _smbreDelaySeconds :: !(Maybe Integer)
    , _smbreMessageAttributes :: Map Text MessageAttributeValue
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'SendMessageBatchRequestEntry' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Id ::@ @Text@
--
-- * @MessageBody ::@ @Text@
--
-- * @DelaySeconds ::@ @Maybe Integer@
--
-- * @MessageAttributes ::@ @Map Text MessageAttributeValue@
--
mkSendMessageBatchRequestEntry :: Text -- ^ 'smbreId'
                               -> Text -- ^ 'smbreMessageBody'
                               -> SendMessageBatchRequestEntry
mkSendMessageBatchRequestEntry p1 p2 = SendMessageBatchRequestEntry
    { _smbreId = p1
    , _smbreMessageBody = p2
    , _smbreDelaySeconds = Nothing
    , _smbreMessageAttributes = mempty
    }

-- | An identifier for the message in this batch. This is used to communicate
-- the result. Note that the Ids of a batch request need to be unique within
-- the request.
smbreId :: Lens' SendMessageBatchRequestEntry Text
smbreId = lens _smbreId (\s a -> s { _smbreId = a })

-- | Body of the message.
smbreMessageBody :: Lens' SendMessageBatchRequestEntry Text
smbreMessageBody =
    lens _smbreMessageBody (\s a -> s { _smbreMessageBody = a })

-- | The number of seconds for which the message has to be delayed.
smbreDelaySeconds :: Lens' SendMessageBatchRequestEntry (Maybe Integer)
smbreDelaySeconds =
    lens _smbreDelaySeconds (\s a -> s { _smbreDelaySeconds = a })

-- | Each message attribute consists of a Name, Type, and Value. For more
-- information, see Message Attribute Items.
smbreMessageAttributes :: Lens' SendMessageBatchRequestEntry (Map Text MessageAttributeValue)
smbreMessageAttributes =
    lens _smbreMessageAttributes (\s a -> s { _smbreMessageAttributes = a })

instance ToQuery SendMessageBatchRequestEntry where
    toQuery = genericQuery def

-- | Encloses a message ID for successfully enqueued message of a
-- SendMessageBatch.
data SendMessageBatchResultEntry = SendMessageBatchResultEntry
    { _smbrerId :: !Text
    , _smbrerMessageId :: !Text
    , _smbrerMD5OfMessageBody :: !Text
    , _smbrerMD5OfMessageAttributes :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'SendMessageBatchResultEntry' data type.
--
-- 'SendMessageBatchResultEntry' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Id ::@ @Text@
--
-- * @MessageId ::@ @Text@
--
-- * @MD5OfMessageBody ::@ @Text@
--
-- * @MD5OfMessageAttributes ::@ @Maybe Text@
--
mkSendMessageBatchResultEntry :: Text -- ^ 'smbrerId'
                              -> Text -- ^ 'smbrerMessageId'
                              -> Text -- ^ 'smbrerMD5OfMessageBody'
                              -> SendMessageBatchResultEntry
mkSendMessageBatchResultEntry p1 p2 p3 = SendMessageBatchResultEntry
    { _smbrerId = p1
    , _smbrerMessageId = p2
    , _smbrerMD5OfMessageBody = p3
    , _smbrerMD5OfMessageAttributes = Nothing
    }

-- | An identifier for the message in this batch.
smbrerId :: Lens' SendMessageBatchResultEntry Text
smbrerId = lens _smbrerId (\s a -> s { _smbrerId = a })

-- | An identifier for the message.
smbrerMessageId :: Lens' SendMessageBatchResultEntry Text
smbrerMessageId = lens _smbrerMessageId (\s a -> s { _smbrerMessageId = a })

-- | An MD5 digest of the non-URL-encoded message body string. This can be used
-- to verify that Amazon SQS received the message correctly. Amazon SQS first
-- URL decodes the message before creating the MD5 digest. For information
-- about MD5, go to http://www.faqs.org/rfcs/rfc1321.html.
smbrerMD5OfMessageBody :: Lens' SendMessageBatchResultEntry Text
smbrerMD5OfMessageBody =
    lens _smbrerMD5OfMessageBody (\s a -> s { _smbrerMD5OfMessageBody = a })

-- | An MD5 digest of the non-URL-encoded message attribute string. This can be
-- used to verify that Amazon SQS received the message batch correctly. Amazon
-- SQS first URL decodes the message before creating the MD5 digest. For
-- information about MD5, go to http://www.faqs.org/rfcs/rfc1321.html.
smbrerMD5OfMessageAttributes :: Lens' SendMessageBatchResultEntry (Maybe Text)
smbrerMD5OfMessageAttributes =
    lens _smbrerMD5OfMessageAttributes
         (\s a -> s { _smbrerMD5OfMessageAttributes = a })

instance FromXML SendMessageBatchResultEntry where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "SendMessageBatchResultEntry"
