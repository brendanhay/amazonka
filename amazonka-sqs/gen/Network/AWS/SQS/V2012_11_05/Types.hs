{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SQS.V2012_11_05.Types
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
module Network.AWS.SQS.V2012_11_05.Types
    (
    -- * Service
      SQS
    -- ** Errors
    , Er (..)
    -- ** XML
    , xmlOptions

    -- * QueueAttributeName
    , QueueAttributeName (..)

    -- * ChangeMessageVisibilityBatchResultEntry
    , ChangeMessageVisibilityBatchResultEntry (..)
    , cmvbrfId

    -- * DeleteMessageBatchResultEntry
    , DeleteMessageBatchResultEntry (..)
    , dmbrfId

    -- * BatchResultErrorEntry
    , BatchResultErrorEntry (..)
    , breeId
    , breeSenderFault
    , breeCode
    , breeMessage

    -- * ChangeMessageVisibilityBatchRequestEntry
    , ChangeMessageVisibilityBatchRequestEntry (..)
    , cmvbreId
    , cmvbreReceiptHandle
    , cmvbreVisibilityTimeout

    -- * DeleteMessageBatchRequestEntry
    , DeleteMessageBatchRequestEntry (..)
    , dmbreId
    , dmbreReceiptHandle

    -- * Message
    , Message (..)
    , nMessageId
    , nReceiptHandle
    , nMD5OfBody
    , nBody
    , nAttributes
    , nMD5OfMessageAttributes
    , nMessageAttributes

    -- * MessageAttributeValue
    , MessageAttributeValue (..)
    , mavStringValue
    , mavBinaryValue
    , mavStringListValues
    , mavBinaryListValues
    , mavDataType

    -- * SendMessageBatchRequestEntry
    , SendMessageBatchRequestEntry (..)
    , smbreId
    , smbreMessageBody
    , smbreDelaySeconds
    , smbreMessageAttributes

    -- * SendMessageBatchResultEntry
    , SendMessageBatchResultEntry (..)
    , smbrfId
    , smbrfMessageId
    , smbrfMD5OfMessageBody
    , smbrfMD5OfMessageAttributes

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
    { xmlNamespace = Just "http://queue.amazonaws.com/doc/2012-11-05/"
    }

-- | The name of a queue attribute.
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
    { _cmvbrfId :: Text
      -- ^ Represents a message whose visibility timeout has been changed
      -- successfully.
    } deriving (Show, Generic)

-- | Represents a message whose visibility timeout has been changed
-- successfully.
cmvbrfId :: Lens' ChangeMessageVisibilityBatchResultEntry (Text)
cmvbrfId f x =
    f (_cmvbrfId x)
        <&> \y -> x { _cmvbrfId = y }
{-# INLINE cmvbrfId #-}

instance FromXML ChangeMessageVisibilityBatchResultEntry where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ChangeMessageVisibilityBatchResultEntry"

-- | Encloses the id an entry in DeleteMessageBatch.
newtype DeleteMessageBatchResultEntry = DeleteMessageBatchResultEntry
    { _dmbrfId :: Text
      -- ^ Represents a successfully deleted message.
    } deriving (Show, Generic)

-- | Represents a successfully deleted message.
dmbrfId :: Lens' DeleteMessageBatchResultEntry (Text)
dmbrfId f x =
    f (_dmbrfId x)
        <&> \y -> x { _dmbrfId = y }
{-# INLINE dmbrfId #-}

instance FromXML DeleteMessageBatchResultEntry where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteMessageBatchResultEntry"

-- | This is used in the responses of batch API to give a detailed description
-- of the result of an action on each entry in the request.
data BatchResultErrorEntry = BatchResultErrorEntry
    { _breeId :: Text
      -- ^ The id of an entry in a batch request.
    , _breeSenderFault :: Bool
      -- ^ Whether the error happened due to the sender's fault.
    , _breeCode :: Text
      -- ^ An error code representing why the action failed on this entry.
    , _breeMessage :: Maybe Text
      -- ^ A message explaining why the action failed on this entry.
    } deriving (Show, Generic)

-- | The id of an entry in a batch request.
breeId :: Lens' BatchResultErrorEntry (Text)
breeId f x =
    f (_breeId x)
        <&> \y -> x { _breeId = y }
{-# INLINE breeId #-}

-- | Whether the error happened due to the sender's fault.
breeSenderFault :: Lens' BatchResultErrorEntry (Bool)
breeSenderFault f x =
    f (_breeSenderFault x)
        <&> \y -> x { _breeSenderFault = y }
{-# INLINE breeSenderFault #-}

-- | An error code representing why the action failed on this entry.
breeCode :: Lens' BatchResultErrorEntry (Text)
breeCode f x =
    f (_breeCode x)
        <&> \y -> x { _breeCode = y }
{-# INLINE breeCode #-}

-- | A message explaining why the action failed on this entry.
breeMessage :: Lens' BatchResultErrorEntry (Maybe Text)
breeMessage f x =
    f (_breeMessage x)
        <&> \y -> x { _breeMessage = y }
{-# INLINE breeMessage #-}

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
    { _cmvbreId :: Text
      -- ^ An identifier for this particular receipt handle. This is used to
      -- communicate the result. Note that the Ids of a batch request need
      -- to be unique within the request.
    , _cmvbreReceiptHandle :: Text
      -- ^ A receipt handle.
    , _cmvbreVisibilityTimeout :: Maybe Integer
      -- ^ The new value (in seconds) for the message's visibility timeout.
    } deriving (Show, Generic)

-- | An identifier for this particular receipt handle. This is used to
-- communicate the result. Note that the Ids of a batch request need to be
-- unique within the request.
cmvbreId :: Lens' ChangeMessageVisibilityBatchRequestEntry (Text)
cmvbreId f x =
    f (_cmvbreId x)
        <&> \y -> x { _cmvbreId = y }
{-# INLINE cmvbreId #-}

-- | A receipt handle.
cmvbreReceiptHandle :: Lens' ChangeMessageVisibilityBatchRequestEntry (Text)
cmvbreReceiptHandle f x =
    f (_cmvbreReceiptHandle x)
        <&> \y -> x { _cmvbreReceiptHandle = y }
{-# INLINE cmvbreReceiptHandle #-}

-- | The new value (in seconds) for the message's visibility timeout.
cmvbreVisibilityTimeout :: Lens' ChangeMessageVisibilityBatchRequestEntry (Maybe Integer)
cmvbreVisibilityTimeout f x =
    f (_cmvbreVisibilityTimeout x)
        <&> \y -> x { _cmvbreVisibilityTimeout = y }
{-# INLINE cmvbreVisibilityTimeout #-}

instance ToQuery ChangeMessageVisibilityBatchRequestEntry where
    toQuery = genericQuery def

-- | Encloses a receipt handle and an identifier for it.
data DeleteMessageBatchRequestEntry = DeleteMessageBatchRequestEntry
    { _dmbreId :: Text
      -- ^ An identifier for this particular receipt handle. This is used to
      -- communicate the result. Note that the Ids of a batch request need
      -- to be unique within the request.
    , _dmbreReceiptHandle :: Text
      -- ^ A receipt handle.
    } deriving (Show, Generic)

-- | An identifier for this particular receipt handle. This is used to
-- communicate the result. Note that the Ids of a batch request need to be
-- unique within the request.
dmbreId :: Lens' DeleteMessageBatchRequestEntry (Text)
dmbreId f x =
    f (_dmbreId x)
        <&> \y -> x { _dmbreId = y }
{-# INLINE dmbreId #-}

-- | A receipt handle.
dmbreReceiptHandle :: Lens' DeleteMessageBatchRequestEntry (Text)
dmbreReceiptHandle f x =
    f (_dmbreReceiptHandle x)
        <&> \y -> x { _dmbreReceiptHandle = y }
{-# INLINE dmbreReceiptHandle #-}

instance ToQuery DeleteMessageBatchRequestEntry where
    toQuery = genericQuery def

-- | An Amazon SQS message.
data Message = Message
    { _nMessageId :: Maybe Text
      -- ^ A unique identifier for the message. Message IDs are considered
      -- unique across all AWS accounts for an extended period of time.
    , _nReceiptHandle :: Maybe Text
      -- ^ An identifier associated with the act of receiving the message. A
      -- new receipt handle is returned every time you receive a message.
      -- When deleting a message, you provide the last received receipt
      -- handle to delete the message.
    , _nMD5OfBody :: Maybe Text
      -- ^ An MD5 digest of the non-URL-encoded message body string.
    , _nBody :: Maybe Text
      -- ^ The message's contents (not URL-encoded).
    , _nAttributes :: Map QueueAttributeName Text
      -- ^ SenderId, SentTimestamp, ApproximateReceiveCount, and/or
      -- ApproximateFirstReceiveTimestamp. SentTimestamp and
      -- ApproximateFirstReceiveTimestamp are each returned as an integer
      -- representing the epoch time in milliseconds.
    , _nMD5OfMessageAttributes :: Maybe Text
      -- ^ An MD5 digest of the non-URL-encoded message attribute string.
      -- This can be used to verify that Amazon SQS received the message
      -- correctly. Amazon SQS first URL decodes the message before
      -- creating the MD5 digest. For information about MD5, go to
      -- http://www.faqs.org/rfcs/rfc1321.html.
    , _nMessageAttributes :: Map Text MessageAttributeValue
      -- ^ Each message attribute consists of a Name, Type, and Value. For
      -- more information, see Message Attribute Items.
    } deriving (Show, Generic)

-- | A unique identifier for the message. Message IDs are considered unique
-- across all AWS accounts for an extended period of time.
nMessageId :: Lens' Message (Maybe Text)
nMessageId f x =
    f (_nMessageId x)
        <&> \y -> x { _nMessageId = y }
{-# INLINE nMessageId #-}

-- | An identifier associated with the act of receiving the message. A new
-- receipt handle is returned every time you receive a message. When deleting
-- a message, you provide the last received receipt handle to delete the
-- message.
nReceiptHandle :: Lens' Message (Maybe Text)
nReceiptHandle f x =
    f (_nReceiptHandle x)
        <&> \y -> x { _nReceiptHandle = y }
{-# INLINE nReceiptHandle #-}

-- | An MD5 digest of the non-URL-encoded message body string.
nMD5OfBody :: Lens' Message (Maybe Text)
nMD5OfBody f x =
    f (_nMD5OfBody x)
        <&> \y -> x { _nMD5OfBody = y }
{-# INLINE nMD5OfBody #-}

-- | The message's contents (not URL-encoded).
nBody :: Lens' Message (Maybe Text)
nBody f x =
    f (_nBody x)
        <&> \y -> x { _nBody = y }
{-# INLINE nBody #-}

-- | SenderId, SentTimestamp, ApproximateReceiveCount, and/or
-- ApproximateFirstReceiveTimestamp. SentTimestamp and
-- ApproximateFirstReceiveTimestamp are each returned as an integer
-- representing the epoch time in milliseconds.
nAttributes :: Lens' Message (Map QueueAttributeName Text)
nAttributes f x =
    f (_nAttributes x)
        <&> \y -> x { _nAttributes = y }
{-# INLINE nAttributes #-}

-- | An MD5 digest of the non-URL-encoded message attribute string. This can be
-- used to verify that Amazon SQS received the message correctly. Amazon SQS
-- first URL decodes the message before creating the MD5 digest. For
-- information about MD5, go to http://www.faqs.org/rfcs/rfc1321.html.
nMD5OfMessageAttributes :: Lens' Message (Maybe Text)
nMD5OfMessageAttributes f x =
    f (_nMD5OfMessageAttributes x)
        <&> \y -> x { _nMD5OfMessageAttributes = y }
{-# INLINE nMD5OfMessageAttributes #-}

-- | Each message attribute consists of a Name, Type, and Value. For more
-- information, see Message Attribute Items.
nMessageAttributes :: Lens' Message (Map Text MessageAttributeValue)
nMessageAttributes f x =
    f (_nMessageAttributes x)
        <&> \y -> x { _nMessageAttributes = y }
{-# INLINE nMessageAttributes #-}

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
    { _mavStringValue :: Maybe Text
      -- ^ Strings are Unicode with UTF8 binary encoding. For a list of code
      -- values, see
      -- http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters.
    , _mavBinaryValue :: Maybe ByteString
      -- ^ Binary type attributes can store any binary data, for example,
      -- compressed data, encrypted data, or images.
    , _mavStringListValues :: [Text]
      -- ^ Not implemented. Reserved for future use.
    , _mavBinaryListValues :: [ByteString]
      -- ^ Not implemented. Reserved for future use.
    , _mavDataType :: Text
      -- ^ Amazon SQS supports the following logical data types: String,
      -- Number, and Binary. In addition, you can append your own custom
      -- labels. For more information, see Message Attribute Data Types.
    } deriving (Show, Generic)

-- | Strings are Unicode with UTF8 binary encoding. For a list of code values,
-- see http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters.
mavStringValue :: Lens' MessageAttributeValue (Maybe Text)
mavStringValue f x =
    f (_mavStringValue x)
        <&> \y -> x { _mavStringValue = y }
{-# INLINE mavStringValue #-}

-- | Binary type attributes can store any binary data, for example, compressed
-- data, encrypted data, or images.
mavBinaryValue :: Lens' MessageAttributeValue (Maybe ByteString)
mavBinaryValue f x =
    f (_mavBinaryValue x)
        <&> \y -> x { _mavBinaryValue = y }
{-# INLINE mavBinaryValue #-}

-- | Not implemented. Reserved for future use.
mavStringListValues :: Lens' MessageAttributeValue ([Text])
mavStringListValues f x =
    f (_mavStringListValues x)
        <&> \y -> x { _mavStringListValues = y }
{-# INLINE mavStringListValues #-}

-- | Not implemented. Reserved for future use.
mavBinaryListValues :: Lens' MessageAttributeValue ([ByteString])
mavBinaryListValues f x =
    f (_mavBinaryListValues x)
        <&> \y -> x { _mavBinaryListValues = y }
{-# INLINE mavBinaryListValues #-}

-- | Amazon SQS supports the following logical data types: String, Number, and
-- Binary. In addition, you can append your own custom labels. For more
-- information, see Message Attribute Data Types.
mavDataType :: Lens' MessageAttributeValue (Text)
mavDataType f x =
    f (_mavDataType x)
        <&> \y -> x { _mavDataType = y }
{-# INLINE mavDataType #-}

instance FromXML MessageAttributeValue where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Value"

instance ToQuery MessageAttributeValue where
    toQuery = genericQuery def

-- | Contains the details of a single Amazon SQS message along with a Id.
data SendMessageBatchRequestEntry = SendMessageBatchRequestEntry
    { _smbreId :: Text
      -- ^ An identifier for the message in this batch. This is used to
      -- communicate the result. Note that the Ids of a batch request need
      -- to be unique within the request.
    , _smbreMessageBody :: Text
      -- ^ Body of the message.
    , _smbreDelaySeconds :: Maybe Integer
      -- ^ The number of seconds for which the message has to be delayed.
    , _smbreMessageAttributes :: Map Text MessageAttributeValue
      -- ^ Each message attribute consists of a Name, Type, and Value. For
      -- more information, see Message Attribute Items.
    } deriving (Show, Generic)

-- | An identifier for the message in this batch. This is used to communicate
-- the result. Note that the Ids of a batch request need to be unique within
-- the request.
smbreId :: Lens' SendMessageBatchRequestEntry (Text)
smbreId f x =
    f (_smbreId x)
        <&> \y -> x { _smbreId = y }
{-# INLINE smbreId #-}

-- | Body of the message.
smbreMessageBody :: Lens' SendMessageBatchRequestEntry (Text)
smbreMessageBody f x =
    f (_smbreMessageBody x)
        <&> \y -> x { _smbreMessageBody = y }
{-# INLINE smbreMessageBody #-}

-- | The number of seconds for which the message has to be delayed.
smbreDelaySeconds :: Lens' SendMessageBatchRequestEntry (Maybe Integer)
smbreDelaySeconds f x =
    f (_smbreDelaySeconds x)
        <&> \y -> x { _smbreDelaySeconds = y }
{-# INLINE smbreDelaySeconds #-}

-- | Each message attribute consists of a Name, Type, and Value. For more
-- information, see Message Attribute Items.
smbreMessageAttributes :: Lens' SendMessageBatchRequestEntry (Map Text MessageAttributeValue)
smbreMessageAttributes f x =
    f (_smbreMessageAttributes x)
        <&> \y -> x { _smbreMessageAttributes = y }
{-# INLINE smbreMessageAttributes #-}

instance ToQuery SendMessageBatchRequestEntry where
    toQuery = genericQuery def

-- | Encloses a message ID for successfully enqueued message of a
-- SendMessageBatch.
data SendMessageBatchResultEntry = SendMessageBatchResultEntry
    { _smbrfId :: Text
      -- ^ An identifier for the message in this batch.
    , _smbrfMessageId :: Text
      -- ^ An identifier for the message.
    , _smbrfMD5OfMessageBody :: Text
      -- ^ An MD5 digest of the non-URL-encoded message body string. This
      -- can be used to verify that Amazon SQS received the message
      -- correctly. Amazon SQS first URL decodes the message before
      -- creating the MD5 digest. For information about MD5, go to
      -- http://www.faqs.org/rfcs/rfc1321.html.
    , _smbrfMD5OfMessageAttributes :: Maybe Text
      -- ^ An MD5 digest of the non-URL-encoded message attribute string.
      -- This can be used to verify that Amazon SQS received the message
      -- batch correctly. Amazon SQS first URL decodes the message before
      -- creating the MD5 digest. For information about MD5, go to
      -- http://www.faqs.org/rfcs/rfc1321.html.
    } deriving (Show, Generic)

-- | An identifier for the message in this batch.
smbrfId :: Lens' SendMessageBatchResultEntry (Text)
smbrfId f x =
    f (_smbrfId x)
        <&> \y -> x { _smbrfId = y }
{-# INLINE smbrfId #-}

-- | An identifier for the message.
smbrfMessageId :: Lens' SendMessageBatchResultEntry (Text)
smbrfMessageId f x =
    f (_smbrfMessageId x)
        <&> \y -> x { _smbrfMessageId = y }
{-# INLINE smbrfMessageId #-}

-- | An MD5 digest of the non-URL-encoded message body string. This can be used
-- to verify that Amazon SQS received the message correctly. Amazon SQS first
-- URL decodes the message before creating the MD5 digest. For information
-- about MD5, go to http://www.faqs.org/rfcs/rfc1321.html.
smbrfMD5OfMessageBody :: Lens' SendMessageBatchResultEntry (Text)
smbrfMD5OfMessageBody f x =
    f (_smbrfMD5OfMessageBody x)
        <&> \y -> x { _smbrfMD5OfMessageBody = y }
{-# INLINE smbrfMD5OfMessageBody #-}

-- | An MD5 digest of the non-URL-encoded message attribute string. This can be
-- used to verify that Amazon SQS received the message batch correctly. Amazon
-- SQS first URL decodes the message before creating the MD5 digest. For
-- information about MD5, go to http://www.faqs.org/rfcs/rfc1321.html.
smbrfMD5OfMessageAttributes :: Lens' SendMessageBatchResultEntry (Maybe Text)
smbrfMD5OfMessageAttributes f x =
    f (_smbrfMD5OfMessageAttributes x)
        <&> \y -> x { _smbrfMD5OfMessageAttributes = y }
{-# INLINE smbrfMD5OfMessageAttributes #-}

instance FromXML SendMessageBatchResultEntry where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "SendMessageBatchResultEntry"
