{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE ViewPatterns                #-}

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
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Network.AWS.SQS.Types
    (
    -- * Service
      SQS
    -- ** Error
    , RESTError
    -- ** XML
    , ns

    -- * DeleteMessageBatchRequestEntry
    , DeleteMessageBatchRequestEntry
    , deleteMessageBatchRequestEntry
    , dmbreId
    , dmbreReceiptHandle

    -- * MessageAttributeValue
    , MessageAttributeValue
    , messageAttributeValue
    , mavBinaryListValues
    , mavBinaryValue
    , mavDataType
    , mavStringListValues
    , mavStringValue

    -- * ChangeMessageVisibilityBatchResultEntry
    , ChangeMessageVisibilityBatchResultEntry
    , changeMessageVisibilityBatchResultEntry
    , cmvbreId

    -- * ChangeMessageVisibilityBatchRequestEntry
    , ChangeMessageVisibilityBatchRequestEntry
    , changeMessageVisibilityBatchRequestEntry
    , cmvbre1Id
    , cmvbre1ReceiptHandle
    , cmvbre1VisibilityTimeout

    -- * DeleteMessageBatchResultEntry
    , DeleteMessageBatchResultEntry
    , deleteMessageBatchResultEntry
    , dmbre1Id

    -- * Message
    , Message
    , message
    , mAttributes
    , mBody
    , mMD5OfBody
    , mMD5OfMessageAttributes
    , mMessageAttributes
    , mMessageId
    , mReceiptHandle

    -- * SendMessageBatchRequestEntry
    , SendMessageBatchRequestEntry
    , sendMessageBatchRequestEntry
    , smbreDelaySeconds
    , smbreId
    , smbreMessageAttributes
    , smbreMessageBody

    -- * SendMessageBatchResultEntry
    , SendMessageBatchResultEntry
    , sendMessageBatchResultEntry
    , smbre1Id
    , smbre1MD5OfMessageAttributes
    , smbre1MD5OfMessageBody
    , smbre1MessageId

    -- * BatchResultErrorEntry
    , BatchResultErrorEntry
    , batchResultErrorEntry
    , breeCode
    , breeId
    , breeMessage
    , breeSenderFault
    ) where

import Network.AWS.Prelude
import Network.AWS.Signing
import qualified GHC.Exts

-- | Version @2012-11-05@ of the Amazon Simple Queue Service service.
data SQS

instance AWSService SQS where
    type Sg SQS = V4
    type Er SQS = RESTError

    service = service'
      where
        service' :: Service SQS
        service' = Service
              { _svcAbbrev       = "SQS"
              , _svcPrefix       = "sqs"
              , _svcVersion      = "2012-11-05"
              , _svcTargetPrefix = Nothing
              , _svcJSONVersion  = Nothing
              , _svcHandle       = handle
              , _svcRetry        = retry
              }

        handle :: Status
               -> Maybe (LazyByteString -> ServiceError RESTError)
        handle = restError statusSuccess service'

        retry :: Retry RESTError
        retry = Retry
            { _rPolicy = exponentialBackon 0.05 2 5
            , _rCheck  = check
            }

        check :: Status
              -> RESTError
              -> Bool
        check (statusCode -> s) (awsErrorCode -> e)
            | s == 403 && "RequestThrottled" == e = True -- Request Limit Exceeded
            | s == 500  = True -- General Server Error
            | s == 509  = True -- Limit Exceeded
            | s == 503  = True -- Service Unavailable
            | otherwise = False

ns :: Text
ns = "http://queue.amazonaws.com/doc/2012-11-05/"
{-# INLINE ns #-}

data DeleteMessageBatchRequestEntry = DeleteMessageBatchRequestEntry
    { _dmbreId            :: Text
    , _dmbreReceiptHandle :: Text
    } deriving (Eq, Ord, Show)

-- | 'DeleteMessageBatchRequestEntry' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmbreId' @::@ 'Text'
--
-- * 'dmbreReceiptHandle' @::@ 'Text'
--
deleteMessageBatchRequestEntry :: Text -- ^ 'dmbreId'
                               -> Text -- ^ 'dmbreReceiptHandle'
                               -> DeleteMessageBatchRequestEntry
deleteMessageBatchRequestEntry p1 p2 = DeleteMessageBatchRequestEntry
    { _dmbreId            = p1
    , _dmbreReceiptHandle = p2
    }

-- | An identifier for this particular receipt handle. This is used to communicate
-- the result. Note that the 'Id's of a batch request need to be unique within the
-- request.
dmbreId :: Lens' DeleteMessageBatchRequestEntry Text
dmbreId = lens _dmbreId (\s a -> s { _dmbreId = a })

-- | A receipt handle.
dmbreReceiptHandle :: Lens' DeleteMessageBatchRequestEntry Text
dmbreReceiptHandle =
    lens _dmbreReceiptHandle (\s a -> s { _dmbreReceiptHandle = a })

instance FromXML DeleteMessageBatchRequestEntry where
    parseXML x = DeleteMessageBatchRequestEntry
        <$> x .@  "Id"
        <*> x .@  "ReceiptHandle"

instance ToQuery DeleteMessageBatchRequestEntry where
    toQuery DeleteMessageBatchRequestEntry{..} = mconcat
        [ "Id"            =? _dmbreId
        , "ReceiptHandle" =? _dmbreReceiptHandle
        ]

data MessageAttributeValue = MessageAttributeValue
    { _mavBinaryListValues :: List "member" Base64
    , _mavBinaryValue      :: Maybe Base64
    , _mavDataType         :: Text
    , _mavStringListValues :: List "member" Text
    , _mavStringValue      :: Maybe Text
    } deriving (Eq, Show)

-- | 'MessageAttributeValue' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mavBinaryListValues' @::@ ['Base64']
--
-- * 'mavBinaryValue' @::@ 'Maybe' 'Base64'
--
-- * 'mavDataType' @::@ 'Text'
--
-- * 'mavStringListValues' @::@ ['Text']
--
-- * 'mavStringValue' @::@ 'Maybe' 'Text'
--
messageAttributeValue :: Text -- ^ 'mavDataType'
                      -> MessageAttributeValue
messageAttributeValue p1 = MessageAttributeValue
    { _mavDataType         = p1
    , _mavStringValue      = Nothing
    , _mavBinaryValue      = Nothing
    , _mavStringListValues = mempty
    , _mavBinaryListValues = mempty
    }

-- | Not implemented. Reserved for future use.
mavBinaryListValues :: Lens' MessageAttributeValue [Base64]
mavBinaryListValues =
    lens _mavBinaryListValues (\s a -> s { _mavBinaryListValues = a })
        . _List

-- | Binary type attributes can store any binary data, for example, compressed
-- data, encrypted data, or images.
mavBinaryValue :: Lens' MessageAttributeValue (Maybe Base64)
mavBinaryValue = lens _mavBinaryValue (\s a -> s { _mavBinaryValue = a })

-- | Amazon SQS supports the following logical data types: String, Number, and
-- Binary. In addition, you can append your own custom labels. For more
-- information, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/SQSMessageAttributes.html#SQSMessageAttributes.DataTypes Message Attribute Data Types>.
mavDataType :: Lens' MessageAttributeValue Text
mavDataType = lens _mavDataType (\s a -> s { _mavDataType = a })

-- | Not implemented. Reserved for future use.
mavStringListValues :: Lens' MessageAttributeValue [Text]
mavStringListValues =
    lens _mavStringListValues (\s a -> s { _mavStringListValues = a })
        . _List

-- | Strings are Unicode with UTF8 binary encoding. For a list of code values, see <http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters>.
mavStringValue :: Lens' MessageAttributeValue (Maybe Text)
mavStringValue = lens _mavStringValue (\s a -> s { _mavStringValue = a })

instance FromXML MessageAttributeValue where
    parseXML x = MessageAttributeValue
        <$> x .@? "BinaryListValue" .!@ mempty
        <*> x .@? "BinaryValue"
        <*> x .@  "DataType"
        <*> x .@? "StringListValue" .!@ mempty
        <*> x .@? "StringValue"

instance ToQuery MessageAttributeValue where
    toQuery MessageAttributeValue{..} = mconcat
        [ "BinaryListValue" =? _mavBinaryListValues
        , "BinaryValue"     =? _mavBinaryValue
        , "DataType"        =? _mavDataType
        , "StringListValue" =? _mavStringListValues
        , "StringValue"     =? _mavStringValue
        ]

newtype ChangeMessageVisibilityBatchResultEntry = ChangeMessageVisibilityBatchResultEntry
    { _cmvbreId :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'ChangeMessageVisibilityBatchResultEntry' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cmvbreId' @::@ 'Text'
--
changeMessageVisibilityBatchResultEntry :: Text -- ^ 'cmvbreId'
                                        -> ChangeMessageVisibilityBatchResultEntry
changeMessageVisibilityBatchResultEntry p1 = ChangeMessageVisibilityBatchResultEntry
    { _cmvbreId = p1
    }

-- | Represents a message whose visibility timeout has been changed successfully.
cmvbreId :: Lens' ChangeMessageVisibilityBatchResultEntry Text
cmvbreId = lens _cmvbreId (\s a -> s { _cmvbreId = a })

instance FromXML ChangeMessageVisibilityBatchResultEntry where
    parseXML x = ChangeMessageVisibilityBatchResultEntry
        <$> x .@  "Id"

instance ToQuery ChangeMessageVisibilityBatchResultEntry where
    toQuery ChangeMessageVisibilityBatchResultEntry{..} = mconcat
        [ "Id" =? _cmvbreId
        ]

data ChangeMessageVisibilityBatchRequestEntry = ChangeMessageVisibilityBatchRequestEntry
    { _cmvbre1Id                :: Text
    , _cmvbre1ReceiptHandle     :: Text
    , _cmvbre1VisibilityTimeout :: Maybe Int
    } deriving (Eq, Ord, Show)

-- | 'ChangeMessageVisibilityBatchRequestEntry' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cmvbre1Id' @::@ 'Text'
--
-- * 'cmvbre1ReceiptHandle' @::@ 'Text'
--
-- * 'cmvbre1VisibilityTimeout' @::@ 'Maybe' 'Int'
--
changeMessageVisibilityBatchRequestEntry :: Text -- ^ 'cmvbre1Id'
                                         -> Text -- ^ 'cmvbre1ReceiptHandle'
                                         -> ChangeMessageVisibilityBatchRequestEntry
changeMessageVisibilityBatchRequestEntry p1 p2 = ChangeMessageVisibilityBatchRequestEntry
    { _cmvbre1Id                = p1
    , _cmvbre1ReceiptHandle     = p2
    , _cmvbre1VisibilityTimeout = Nothing
    }

-- | An identifier for this particular receipt handle. This is used to communicate
-- the result. Note that the 'Id's of a batch request need to be unique within the
-- request.
cmvbre1Id :: Lens' ChangeMessageVisibilityBatchRequestEntry Text
cmvbre1Id = lens _cmvbre1Id (\s a -> s { _cmvbre1Id = a })

-- | A receipt handle.
cmvbre1ReceiptHandle :: Lens' ChangeMessageVisibilityBatchRequestEntry Text
cmvbre1ReceiptHandle =
    lens _cmvbre1ReceiptHandle (\s a -> s { _cmvbre1ReceiptHandle = a })

-- | The new value (in seconds) for the message's visibility timeout.
cmvbre1VisibilityTimeout :: Lens' ChangeMessageVisibilityBatchRequestEntry (Maybe Int)
cmvbre1VisibilityTimeout =
    lens _cmvbre1VisibilityTimeout
        (\s a -> s { _cmvbre1VisibilityTimeout = a })

instance FromXML ChangeMessageVisibilityBatchRequestEntry where
    parseXML x = ChangeMessageVisibilityBatchRequestEntry
        <$> x .@  "Id"
        <*> x .@  "ReceiptHandle"
        <*> x .@? "VisibilityTimeout"

instance ToQuery ChangeMessageVisibilityBatchRequestEntry where
    toQuery ChangeMessageVisibilityBatchRequestEntry{..} = mconcat
        [ "Id"                =? _cmvbre1Id
        , "ReceiptHandle"     =? _cmvbre1ReceiptHandle
        , "VisibilityTimeout" =? _cmvbre1VisibilityTimeout
        ]

newtype DeleteMessageBatchResultEntry = DeleteMessageBatchResultEntry
    { _dmbre1Id :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'DeleteMessageBatchResultEntry' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmbre1Id' @::@ 'Text'
--
deleteMessageBatchResultEntry :: Text -- ^ 'dmbre1Id'
                              -> DeleteMessageBatchResultEntry
deleteMessageBatchResultEntry p1 = DeleteMessageBatchResultEntry
    { _dmbre1Id = p1
    }

-- | Represents a successfully deleted message.
dmbre1Id :: Lens' DeleteMessageBatchResultEntry Text
dmbre1Id = lens _dmbre1Id (\s a -> s { _dmbre1Id = a })

instance FromXML DeleteMessageBatchResultEntry where
    parseXML x = DeleteMessageBatchResultEntry
        <$> x .@  "Id"

instance ToQuery DeleteMessageBatchResultEntry where
    toQuery DeleteMessageBatchResultEntry{..} = mconcat
        [ "Id" =? _dmbre1Id
        ]

data Message = Message
    { _mAttributes             :: EMap "entry" "Name" "Value" Text Text
    , _mBody                   :: Maybe Text
    , _mMD5OfBody              :: Maybe Text
    , _mMD5OfMessageAttributes :: Maybe Text
    , _mMessageAttributes      :: EMap "entry" "Name" "Value" Text MessageAttributeValue
    , _mMessageId              :: Maybe Text
    , _mReceiptHandle          :: Maybe Text
    } deriving (Eq, Show)

-- | 'Message' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mAttributes' @::@ 'HashMap' 'Text' 'Text'
--
-- * 'mBody' @::@ 'Maybe' 'Text'
--
-- * 'mMD5OfBody' @::@ 'Maybe' 'Text'
--
-- * 'mMD5OfMessageAttributes' @::@ 'Maybe' 'Text'
--
-- * 'mMessageAttributes' @::@ 'HashMap' 'Text' 'MessageAttributeValue'
--
-- * 'mMessageId' @::@ 'Maybe' 'Text'
--
-- * 'mReceiptHandle' @::@ 'Maybe' 'Text'
--
message :: Message
message = Message
    { _mMessageId              = Nothing
    , _mReceiptHandle          = Nothing
    , _mMD5OfBody              = Nothing
    , _mBody                   = Nothing
    , _mAttributes             = mempty
    , _mMD5OfMessageAttributes = Nothing
    , _mMessageAttributes      = mempty
    }

-- | 'SenderId', 'SentTimestamp', 'ApproximateReceiveCount', and/or 'ApproximateFirstReceiveTimestamp'. 'SentTimestamp' and 'ApproximateFirstReceiveTimestamp' are each returned as an
-- integer representing the <http://en.wikipedia.org/wiki/Unix_time epoch time> in milliseconds.
mAttributes :: Lens' Message (HashMap Text Text)
mAttributes = lens _mAttributes (\s a -> s { _mAttributes = a }) . _EMap

-- | The message's contents (not URL-encoded).
mBody :: Lens' Message (Maybe Text)
mBody = lens _mBody (\s a -> s { _mBody = a })

-- | An MD5 digest of the non-URL-encoded message body string.
mMD5OfBody :: Lens' Message (Maybe Text)
mMD5OfBody = lens _mMD5OfBody (\s a -> s { _mMD5OfBody = a })

-- | An MD5 digest of the non-URL-encoded message attribute string. This can be
-- used to verify that Amazon SQS received the message correctly. Amazon SQS
-- first URL decodes the message before creating the MD5 digest. For information
-- about MD5, go to <http://www.faqs.org/rfcs/rfc1321.html http://www.faqs.org/rfcs/rfc1321.html>.
mMD5OfMessageAttributes :: Lens' Message (Maybe Text)
mMD5OfMessageAttributes =
    lens _mMD5OfMessageAttributes (\s a -> s { _mMD5OfMessageAttributes = a })

-- | Each message attribute consists of a Name, Type, and Value. For more
-- information, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/SQSMessageAttributes.html#SQSMessageAttributesNTV Message Attribute Items>.
mMessageAttributes :: Lens' Message (HashMap Text MessageAttributeValue)
mMessageAttributes =
    lens _mMessageAttributes (\s a -> s { _mMessageAttributes = a })
        . _EMap

-- | A unique identifier for the message. Message IDs are considered unique across
-- all AWS accounts for an extended period of time.
mMessageId :: Lens' Message (Maybe Text)
mMessageId = lens _mMessageId (\s a -> s { _mMessageId = a })

-- | An identifier associated with the act of receiving the message. A new receipt
-- handle is returned every time you receive a message. When deleting a message,
-- you provide the last received receipt handle to delete the message.
mReceiptHandle :: Lens' Message (Maybe Text)
mReceiptHandle = lens _mReceiptHandle (\s a -> s { _mReceiptHandle = a })

instance FromXML Message where
    parseXML x = Message
        <$> parseXML x
        <*> x .@? "Body"
        <*> x .@? "MD5OfBody"
        <*> x .@? "MD5OfMessageAttributes"
        <*> parseXML x
        <*> x .@? "MessageId"
        <*> x .@? "ReceiptHandle"

instance ToQuery Message where
    toQuery Message{..} = mconcat
        [ toQuery                 _mAttributes
        , "Body"                   =? _mBody
        , "MD5OfBody"              =? _mMD5OfBody
        , "MD5OfMessageAttributes" =? _mMD5OfMessageAttributes
        , toQuery                 _mMessageAttributes
        , "MessageId"              =? _mMessageId
        , "ReceiptHandle"          =? _mReceiptHandle
        ]

data SendMessageBatchRequestEntry = SendMessageBatchRequestEntry
    { _smbreDelaySeconds      :: Maybe Int
    , _smbreId                :: Text
    , _smbreMessageAttributes :: EMap "entry" "Name" "Value" Text MessageAttributeValue
    , _smbreMessageBody       :: Text
    } deriving (Eq, Show)

-- | 'SendMessageBatchRequestEntry' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'smbreDelaySeconds' @::@ 'Maybe' 'Int'
--
-- * 'smbreId' @::@ 'Text'
--
-- * 'smbreMessageAttributes' @::@ 'HashMap' 'Text' 'MessageAttributeValue'
--
-- * 'smbreMessageBody' @::@ 'Text'
--
sendMessageBatchRequestEntry :: Text -- ^ 'smbreId'
                             -> Text -- ^ 'smbreMessageBody'
                             -> SendMessageBatchRequestEntry
sendMessageBatchRequestEntry p1 p2 = SendMessageBatchRequestEntry
    { _smbreId                = p1
    , _smbreMessageBody       = p2
    , _smbreDelaySeconds      = Nothing
    , _smbreMessageAttributes = mempty
    }

-- | The number of seconds for which the message has to be delayed.
smbreDelaySeconds :: Lens' SendMessageBatchRequestEntry (Maybe Int)
smbreDelaySeconds =
    lens _smbreDelaySeconds (\s a -> s { _smbreDelaySeconds = a })

-- | An identifier for the message in this batch. This is used to communicate the
-- result. Note that the 'Id's of a batch request need to be unique within the
-- request.
smbreId :: Lens' SendMessageBatchRequestEntry Text
smbreId = lens _smbreId (\s a -> s { _smbreId = a })

-- | Each message attribute consists of a Name, Type, and Value. For more
-- information, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/SQSMessageAttributes.html#SQSMessageAttributesNTV Message Attribute Items>.
smbreMessageAttributes :: Lens' SendMessageBatchRequestEntry (HashMap Text MessageAttributeValue)
smbreMessageAttributes =
    lens _smbreMessageAttributes (\s a -> s { _smbreMessageAttributes = a })
        . _EMap

-- | Body of the message.
smbreMessageBody :: Lens' SendMessageBatchRequestEntry Text
smbreMessageBody = lens _smbreMessageBody (\s a -> s { _smbreMessageBody = a })

instance FromXML SendMessageBatchRequestEntry where
    parseXML x = SendMessageBatchRequestEntry
        <$> x .@? "DelaySeconds"
        <*> x .@  "Id"
        <*> parseXML x
        <*> x .@  "MessageBody"

instance ToQuery SendMessageBatchRequestEntry where
    toQuery SendMessageBatchRequestEntry{..} = mconcat
        [ "DelaySeconds"     =? _smbreDelaySeconds
        , "Id"               =? _smbreId
        , toQuery           _smbreMessageAttributes
        , "MessageBody"      =? _smbreMessageBody
        ]

data SendMessageBatchResultEntry = SendMessageBatchResultEntry
    { _smbre1Id                     :: Text
    , _smbre1MD5OfMessageAttributes :: Maybe Text
    , _smbre1MD5OfMessageBody       :: Text
    , _smbre1MessageId              :: Text
    } deriving (Eq, Ord, Show)

-- | 'SendMessageBatchResultEntry' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'smbre1Id' @::@ 'Text'
--
-- * 'smbre1MD5OfMessageAttributes' @::@ 'Maybe' 'Text'
--
-- * 'smbre1MD5OfMessageBody' @::@ 'Text'
--
-- * 'smbre1MessageId' @::@ 'Text'
--
sendMessageBatchResultEntry :: Text -- ^ 'smbre1Id'
                            -> Text -- ^ 'smbre1MessageId'
                            -> Text -- ^ 'smbre1MD5OfMessageBody'
                            -> SendMessageBatchResultEntry
sendMessageBatchResultEntry p1 p2 p3 = SendMessageBatchResultEntry
    { _smbre1Id                     = p1
    , _smbre1MessageId              = p2
    , _smbre1MD5OfMessageBody       = p3
    , _smbre1MD5OfMessageAttributes = Nothing
    }

-- | An identifier for the message in this batch.
smbre1Id :: Lens' SendMessageBatchResultEntry Text
smbre1Id = lens _smbre1Id (\s a -> s { _smbre1Id = a })

-- | An MD5 digest of the non-URL-encoded message attribute string. This can be
-- used to verify that Amazon SQS received the message batch correctly. Amazon
-- SQS first URL decodes the message before creating the MD5 digest. For
-- information about MD5, go to <http://www.faqs.org/rfcs/rfc1321.html http://www.faqs.org/rfcs/rfc1321.html>.
smbre1MD5OfMessageAttributes :: Lens' SendMessageBatchResultEntry (Maybe Text)
smbre1MD5OfMessageAttributes =
    lens _smbre1MD5OfMessageAttributes
        (\s a -> s { _smbre1MD5OfMessageAttributes = a })

-- | An MD5 digest of the non-URL-encoded message body string. This can be used to
-- verify that Amazon SQS received the message correctly. Amazon SQS first URL
-- decodes the message before creating the MD5 digest. For information about
-- MD5, go to <http://www.faqs.org/rfcs/rfc1321.html http://www.faqs.org/rfcs/rfc1321.html>.
smbre1MD5OfMessageBody :: Lens' SendMessageBatchResultEntry Text
smbre1MD5OfMessageBody =
    lens _smbre1MD5OfMessageBody (\s a -> s { _smbre1MD5OfMessageBody = a })

-- | An identifier for the message.
smbre1MessageId :: Lens' SendMessageBatchResultEntry Text
smbre1MessageId = lens _smbre1MessageId (\s a -> s { _smbre1MessageId = a })

instance FromXML SendMessageBatchResultEntry where
    parseXML x = SendMessageBatchResultEntry
        <$> x .@  "Id"
        <*> x .@? "MD5OfMessageAttributes"
        <*> x .@  "MD5OfMessageBody"
        <*> x .@  "MessageId"

instance ToQuery SendMessageBatchResultEntry where
    toQuery SendMessageBatchResultEntry{..} = mconcat
        [ "Id"                     =? _smbre1Id
        , "MD5OfMessageAttributes" =? _smbre1MD5OfMessageAttributes
        , "MD5OfMessageBody"       =? _smbre1MD5OfMessageBody
        , "MessageId"              =? _smbre1MessageId
        ]

data BatchResultErrorEntry = BatchResultErrorEntry
    { _breeCode        :: Text
    , _breeId          :: Text
    , _breeMessage     :: Maybe Text
    , _breeSenderFault :: Bool
    } deriving (Eq, Ord, Show)

-- | 'BatchResultErrorEntry' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'breeCode' @::@ 'Text'
--
-- * 'breeId' @::@ 'Text'
--
-- * 'breeMessage' @::@ 'Maybe' 'Text'
--
-- * 'breeSenderFault' @::@ 'Bool'
--
batchResultErrorEntry :: Text -- ^ 'breeId'
                      -> Bool -- ^ 'breeSenderFault'
                      -> Text -- ^ 'breeCode'
                      -> BatchResultErrorEntry
batchResultErrorEntry p1 p2 p3 = BatchResultErrorEntry
    { _breeId          = p1
    , _breeSenderFault = p2
    , _breeCode        = p3
    , _breeMessage     = Nothing
    }

-- | An error code representing why the action failed on this entry.
breeCode :: Lens' BatchResultErrorEntry Text
breeCode = lens _breeCode (\s a -> s { _breeCode = a })

-- | The id of an entry in a batch request.
breeId :: Lens' BatchResultErrorEntry Text
breeId = lens _breeId (\s a -> s { _breeId = a })

-- | A message explaining why the action failed on this entry.
breeMessage :: Lens' BatchResultErrorEntry (Maybe Text)
breeMessage = lens _breeMessage (\s a -> s { _breeMessage = a })

-- | Whether the error happened due to the sender's fault.
breeSenderFault :: Lens' BatchResultErrorEntry Bool
breeSenderFault = lens _breeSenderFault (\s a -> s { _breeSenderFault = a })

instance FromXML BatchResultErrorEntry where
    parseXML x = BatchResultErrorEntry
        <$> x .@  "Code"
        <*> x .@  "Id"
        <*> x .@? "Message"
        <*> x .@  "SenderFault"

instance ToQuery BatchResultErrorEntry where
    toQuery BatchResultErrorEntry{..} = mconcat
        [ "Code"        =? _breeCode
        , "Id"          =? _breeId
        , "Message"     =? _breeMessage
        , "SenderFault" =? _breeSenderFault
        ]
