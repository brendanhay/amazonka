{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TemplateHaskell             #-}
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
    ( module Network.AWS.SQS.V2012_11_05.Types
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
    fromXMLRoot    = fromRoot "AttributeName"

instance ToQuery QueueAttributeName where
    toQuery = genericQuery def

-- | Encloses the id of an entry in ChangeMessageVisibilityBatch.
newtype ChangeMessageVisibilityBatchResultEntry = ChangeMessageVisibilityBatchResultEntry
    { _cmvbrfId :: Text
      -- ^ Represents a message whose visibility timeout has been changed
      -- successfully.
    } deriving (Show, Generic)

instance FromXML ChangeMessageVisibilityBatchResultEntry where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ChangeMessageVisibilityBatchResultEntry"

-- | Encloses the id an entry in DeleteMessageBatch.
newtype DeleteMessageBatchResultEntry = DeleteMessageBatchResultEntry
    { _dmbrfId :: Text
      -- ^ Represents a successfully deleted message.
    } deriving (Show, Generic)

instance FromXML DeleteMessageBatchResultEntry where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteMessageBatchResultEntry"

-- | This is used in the responses of batch API to give a detailed description
-- of the result of an action on each entry in the request.
data BatchResultErrorEntry = BatchResultErrorEntry
    { _breeCode :: Text
      -- ^ An error code representing why the action failed on this entry.
    , _breeId :: Text
      -- ^ The id of an entry in a batch request.
    , _breeMessage :: Maybe Text
      -- ^ A message explaining why the action failed on this entry.
    , _breeSenderFault :: Bool
      -- ^ Whether the error happened due to the sender's fault.
    } deriving (Show, Generic)

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

instance ToQuery DeleteMessageBatchRequestEntry where
    toQuery = genericQuery def

-- | An Amazon SQS message.
data Message = Message
    { _nAttributes :: Map QueueAttributeName Text
      -- ^ SenderId, SentTimestamp, ApproximateReceiveCount, and/or
      -- ApproximateFirstReceiveTimestamp. SentTimestamp and
      -- ApproximateFirstReceiveTimestamp are each returned as an integer
      -- representing the epoch time in milliseconds.
    , _nBody :: Maybe Text
      -- ^ The message's contents (not URL-encoded).
    , _nMD5OfBody :: Maybe Text
      -- ^ An MD5 digest of the non-URL-encoded message body string.
    , _nMD5OfMessageAttributes :: Maybe Text
      -- ^ An MD5 digest of the non-URL-encoded message attribute string.
      -- This can be used to verify that Amazon SQS received the message
      -- correctly. Amazon SQS first URL decodes the message before
      -- creating the MD5 digest. For information about MD5, go to
      -- http://www.faqs.org/rfcs/rfc1321.html.
    , _nMessageAttributes :: Map Text MessageAttributeValue
      -- ^ Each message attribute consists of a Name, Type, and Value. For
      -- more information, see Message Attribute Items.
    , _nMessageId :: Maybe Text
      -- ^ A unique identifier for the message. Message IDs are considered
      -- unique across all AWS accounts for an extended period of time.
    , _nReceiptHandle :: Maybe Text
      -- ^ An identifier associated with the act of receiving the message. A
      -- new receipt handle is returned every time you receive a message.
      -- When deleting a message, you provide the last received receipt
      -- handle to delete the message.
    } deriving (Show, Generic)

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
    { _mavBinaryListValues :: [ByteString]
      -- ^ Not implemented. Reserved for future use.
    , _mavBinaryValue :: Maybe ByteString
      -- ^ Binary type attributes can store any binary data, for example,
      -- compressed data, encrypted data, or images.
    , _mavDataType :: Text
      -- ^ Amazon SQS supports the following logical data types: String,
      -- Number, and Binary. In addition, you can append your own custom
      -- labels. For more information, see Message Attribute Data Types.
    , _mavStringListValues :: [Text]
      -- ^ Not implemented. Reserved for future use.
    , _mavStringValue :: Maybe Text
      -- ^ Strings are Unicode with UTF8 binary encoding. For a list of code
      -- values, see
      -- http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters.
    } deriving (Show, Generic)

instance FromXML MessageAttributeValue where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Value"

instance ToQuery MessageAttributeValue where
    toQuery = genericQuery def

-- | Contains the details of a single Amazon SQS message along with a Id.
data SendMessageBatchRequestEntry = SendMessageBatchRequestEntry
    { _smbreDelaySeconds :: Maybe Integer
      -- ^ The number of seconds for which the message has to be delayed.
    , _smbreId :: Text
      -- ^ An identifier for the message in this batch. This is used to
      -- communicate the result. Note that the Ids of a batch request need
      -- to be unique within the request.
    , _smbreMessageAttributes :: Map Text MessageAttributeValue
      -- ^ Each message attribute consists of a Name, Type, and Value. For
      -- more information, see Message Attribute Items.
    , _smbreMessageBody :: Text
      -- ^ Body of the message.
    } deriving (Show, Generic)

instance ToQuery SendMessageBatchRequestEntry where
    toQuery = genericQuery def

-- | Encloses a message ID for successfully enqueued message of a
-- SendMessageBatch.
data SendMessageBatchResultEntry = SendMessageBatchResultEntry
    { _smbrfId :: Text
      -- ^ An identifier for the message in this batch.
    , _smbrfMD5OfMessageAttributes :: Maybe Text
      -- ^ An MD5 digest of the non-URL-encoded message attribute string.
      -- This can be used to verify that Amazon SQS received the message
      -- batch correctly. Amazon SQS first URL decodes the message before
      -- creating the MD5 digest. For information about MD5, go to
      -- http://www.faqs.org/rfcs/rfc1321.html.
    , _smbrfMD5OfMessageBody :: Text
      -- ^ An MD5 digest of the non-URL-encoded message body string. This
      -- can be used to verify that Amazon SQS received the message
      -- correctly. Amazon SQS first URL decodes the message before
      -- creating the MD5 digest. For information about MD5, go to
      -- http://www.faqs.org/rfcs/rfc1321.html.
    , _smbrfMessageId :: Text
      -- ^ An identifier for the message.
    } deriving (Show, Generic)

instance FromXML SendMessageBatchResultEntry where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "SendMessageBatchResultEntry"

makeLenses ''ChangeMessageVisibilityBatchResultEntry
makeLenses ''DeleteMessageBatchResultEntry
makeLenses ''BatchResultErrorEntry
makeLenses ''ChangeMessageVisibilityBatchRequestEntry
makeLenses ''DeleteMessageBatchRequestEntry
makeLenses ''Message
makeLenses ''MessageAttributeValue
makeLenses ''SendMessageBatchRequestEntry
makeLenses ''SendMessageBatchResultEntry
