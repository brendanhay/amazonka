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
cmvbrfId
    :: Functor f
    => (Text
    -> f (Text))
    -> ChangeMessageVisibilityBatchResultEntry
    -> f ChangeMessageVisibilityBatchResultEntry
cmvbrfId f x =
    (\y -> x { _cmvbrfId = y })
       <$> f (_cmvbrfId x)
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
dmbrfId
    :: Functor f
    => (Text
    -> f (Text))
    -> DeleteMessageBatchResultEntry
    -> f DeleteMessageBatchResultEntry
dmbrfId f x =
    (\y -> x { _dmbrfId = y })
       <$> f (_dmbrfId x)
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
breeId
    :: Functor f
    => (Text
    -> f (Text))
    -> BatchResultErrorEntry
    -> f BatchResultErrorEntry
breeId f x =
    (\y -> x { _breeId = y })
       <$> f (_breeId x)
{-# INLINE breeId #-}

-- | Whether the error happened due to the sender's fault.
breeSenderFault
    :: Functor f
    => (Bool
    -> f (Bool))
    -> BatchResultErrorEntry
    -> f BatchResultErrorEntry
breeSenderFault f x =
    (\y -> x { _breeSenderFault = y })
       <$> f (_breeSenderFault x)
{-# INLINE breeSenderFault #-}

-- | An error code representing why the action failed on this entry.
breeCode
    :: Functor f
    => (Text
    -> f (Text))
    -> BatchResultErrorEntry
    -> f BatchResultErrorEntry
breeCode f x =
    (\y -> x { _breeCode = y })
       <$> f (_breeCode x)
{-# INLINE breeCode #-}

-- | A message explaining why the action failed on this entry.
breeMessage
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> BatchResultErrorEntry
    -> f BatchResultErrorEntry
breeMessage f x =
    (\y -> x { _breeMessage = y })
       <$> f (_breeMessage x)
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
cmvbreId
    :: Functor f
    => (Text
    -> f (Text))
    -> ChangeMessageVisibilityBatchRequestEntry
    -> f ChangeMessageVisibilityBatchRequestEntry
cmvbreId f x =
    (\y -> x { _cmvbreId = y })
       <$> f (_cmvbreId x)
{-# INLINE cmvbreId #-}

-- | A receipt handle.
cmvbreReceiptHandle
    :: Functor f
    => (Text
    -> f (Text))
    -> ChangeMessageVisibilityBatchRequestEntry
    -> f ChangeMessageVisibilityBatchRequestEntry
cmvbreReceiptHandle f x =
    (\y -> x { _cmvbreReceiptHandle = y })
       <$> f (_cmvbreReceiptHandle x)
{-# INLINE cmvbreReceiptHandle #-}

-- | The new value (in seconds) for the message's visibility timeout.
cmvbreVisibilityTimeout
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> ChangeMessageVisibilityBatchRequestEntry
    -> f ChangeMessageVisibilityBatchRequestEntry
cmvbreVisibilityTimeout f x =
    (\y -> x { _cmvbreVisibilityTimeout = y })
       <$> f (_cmvbreVisibilityTimeout x)
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
dmbreId
    :: Functor f
    => (Text
    -> f (Text))
    -> DeleteMessageBatchRequestEntry
    -> f DeleteMessageBatchRequestEntry
dmbreId f x =
    (\y -> x { _dmbreId = y })
       <$> f (_dmbreId x)
{-# INLINE dmbreId #-}

-- | A receipt handle.
dmbreReceiptHandle
    :: Functor f
    => (Text
    -> f (Text))
    -> DeleteMessageBatchRequestEntry
    -> f DeleteMessageBatchRequestEntry
dmbreReceiptHandle f x =
    (\y -> x { _dmbreReceiptHandle = y })
       <$> f (_dmbreReceiptHandle x)
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
nMessageId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Message
    -> f Message
nMessageId f x =
    (\y -> x { _nMessageId = y })
       <$> f (_nMessageId x)
{-# INLINE nMessageId #-}

-- | An identifier associated with the act of receiving the message. A new
-- receipt handle is returned every time you receive a message. When deleting
-- a message, you provide the last received receipt handle to delete the
-- message.
nReceiptHandle
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Message
    -> f Message
nReceiptHandle f x =
    (\y -> x { _nReceiptHandle = y })
       <$> f (_nReceiptHandle x)
{-# INLINE nReceiptHandle #-}

-- | An MD5 digest of the non-URL-encoded message body string.
nMD5OfBody
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Message
    -> f Message
nMD5OfBody f x =
    (\y -> x { _nMD5OfBody = y })
       <$> f (_nMD5OfBody x)
{-# INLINE nMD5OfBody #-}

-- | The message's contents (not URL-encoded).
nBody
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Message
    -> f Message
nBody f x =
    (\y -> x { _nBody = y })
       <$> f (_nBody x)
{-# INLINE nBody #-}

-- | SenderId, SentTimestamp, ApproximateReceiveCount, and/or
-- ApproximateFirstReceiveTimestamp. SentTimestamp and
-- ApproximateFirstReceiveTimestamp are each returned as an integer
-- representing the epoch time in milliseconds.
nAttributes
    :: Functor f
    => (Map QueueAttributeName Text
    -> f (Map QueueAttributeName Text))
    -> Message
    -> f Message
nAttributes f x =
    (\y -> x { _nAttributes = y })
       <$> f (_nAttributes x)
{-# INLINE nAttributes #-}

-- | An MD5 digest of the non-URL-encoded message attribute string. This can be
-- used to verify that Amazon SQS received the message correctly. Amazon SQS
-- first URL decodes the message before creating the MD5 digest. For
-- information about MD5, go to http://www.faqs.org/rfcs/rfc1321.html.
nMD5OfMessageAttributes
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Message
    -> f Message
nMD5OfMessageAttributes f x =
    (\y -> x { _nMD5OfMessageAttributes = y })
       <$> f (_nMD5OfMessageAttributes x)
{-# INLINE nMD5OfMessageAttributes #-}

-- | Each message attribute consists of a Name, Type, and Value. For more
-- information, see Message Attribute Items.
nMessageAttributes
    :: Functor f
    => (Map Text MessageAttributeValue
    -> f (Map Text MessageAttributeValue))
    -> Message
    -> f Message
nMessageAttributes f x =
    (\y -> x { _nMessageAttributes = y })
       <$> f (_nMessageAttributes x)
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
mavStringValue
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> MessageAttributeValue
    -> f MessageAttributeValue
mavStringValue f x =
    (\y -> x { _mavStringValue = y })
       <$> f (_mavStringValue x)
{-# INLINE mavStringValue #-}

-- | Binary type attributes can store any binary data, for example, compressed
-- data, encrypted data, or images.
mavBinaryValue
    :: Functor f
    => (Maybe ByteString
    -> f (Maybe ByteString))
    -> MessageAttributeValue
    -> f MessageAttributeValue
mavBinaryValue f x =
    (\y -> x { _mavBinaryValue = y })
       <$> f (_mavBinaryValue x)
{-# INLINE mavBinaryValue #-}

-- | Not implemented. Reserved for future use.
mavStringListValues
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> MessageAttributeValue
    -> f MessageAttributeValue
mavStringListValues f x =
    (\y -> x { _mavStringListValues = y })
       <$> f (_mavStringListValues x)
{-# INLINE mavStringListValues #-}

-- | Not implemented. Reserved for future use.
mavBinaryListValues
    :: Functor f
    => ([ByteString]
    -> f ([ByteString]))
    -> MessageAttributeValue
    -> f MessageAttributeValue
mavBinaryListValues f x =
    (\y -> x { _mavBinaryListValues = y })
       <$> f (_mavBinaryListValues x)
{-# INLINE mavBinaryListValues #-}

-- | Amazon SQS supports the following logical data types: String, Number, and
-- Binary. In addition, you can append your own custom labels. For more
-- information, see Message Attribute Data Types.
mavDataType
    :: Functor f
    => (Text
    -> f (Text))
    -> MessageAttributeValue
    -> f MessageAttributeValue
mavDataType f x =
    (\y -> x { _mavDataType = y })
       <$> f (_mavDataType x)
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
smbreId
    :: Functor f
    => (Text
    -> f (Text))
    -> SendMessageBatchRequestEntry
    -> f SendMessageBatchRequestEntry
smbreId f x =
    (\y -> x { _smbreId = y })
       <$> f (_smbreId x)
{-# INLINE smbreId #-}

-- | Body of the message.
smbreMessageBody
    :: Functor f
    => (Text
    -> f (Text))
    -> SendMessageBatchRequestEntry
    -> f SendMessageBatchRequestEntry
smbreMessageBody f x =
    (\y -> x { _smbreMessageBody = y })
       <$> f (_smbreMessageBody x)
{-# INLINE smbreMessageBody #-}

-- | The number of seconds for which the message has to be delayed.
smbreDelaySeconds
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> SendMessageBatchRequestEntry
    -> f SendMessageBatchRequestEntry
smbreDelaySeconds f x =
    (\y -> x { _smbreDelaySeconds = y })
       <$> f (_smbreDelaySeconds x)
{-# INLINE smbreDelaySeconds #-}

-- | Each message attribute consists of a Name, Type, and Value. For more
-- information, see Message Attribute Items.
smbreMessageAttributes
    :: Functor f
    => (Map Text MessageAttributeValue
    -> f (Map Text MessageAttributeValue))
    -> SendMessageBatchRequestEntry
    -> f SendMessageBatchRequestEntry
smbreMessageAttributes f x =
    (\y -> x { _smbreMessageAttributes = y })
       <$> f (_smbreMessageAttributes x)
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
smbrfId
    :: Functor f
    => (Text
    -> f (Text))
    -> SendMessageBatchResultEntry
    -> f SendMessageBatchResultEntry
smbrfId f x =
    (\y -> x { _smbrfId = y })
       <$> f (_smbrfId x)
{-# INLINE smbrfId #-}

-- | An identifier for the message.
smbrfMessageId
    :: Functor f
    => (Text
    -> f (Text))
    -> SendMessageBatchResultEntry
    -> f SendMessageBatchResultEntry
smbrfMessageId f x =
    (\y -> x { _smbrfMessageId = y })
       <$> f (_smbrfMessageId x)
{-# INLINE smbrfMessageId #-}

-- | An MD5 digest of the non-URL-encoded message body string. This can be used
-- to verify that Amazon SQS received the message correctly. Amazon SQS first
-- URL decodes the message before creating the MD5 digest. For information
-- about MD5, go to http://www.faqs.org/rfcs/rfc1321.html.
smbrfMD5OfMessageBody
    :: Functor f
    => (Text
    -> f (Text))
    -> SendMessageBatchResultEntry
    -> f SendMessageBatchResultEntry
smbrfMD5OfMessageBody f x =
    (\y -> x { _smbrfMD5OfMessageBody = y })
       <$> f (_smbrfMD5OfMessageBody x)
{-# INLINE smbrfMD5OfMessageBody #-}

-- | An MD5 digest of the non-URL-encoded message attribute string. This can be
-- used to verify that Amazon SQS received the message batch correctly. Amazon
-- SQS first URL decodes the message before creating the MD5 digest. For
-- information about MD5, go to http://www.faqs.org/rfcs/rfc1321.html.
smbrfMD5OfMessageAttributes
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> SendMessageBatchResultEntry
    -> f SendMessageBatchResultEntry
smbrfMD5OfMessageAttributes f x =
    (\y -> x { _smbrfMD5OfMessageAttributes = y })
       <$> f (_smbrfMD5OfMessageAttributes x)
{-# INLINE smbrfMD5OfMessageAttributes #-}

instance FromXML SendMessageBatchResultEntry where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "SendMessageBatchResultEntry"
