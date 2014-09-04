{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.V2010_03_31.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon Simple Notification Service (Amazon SNS) is a fast, flexible, fully
-- managed push messaging service. Amazon SNS makes it simple and
-- cost-effective to push notifications to Apple, Google, Fire OS, and Windows
-- devices, as well as Android devices in China with Baidu Cloud Push. You can
-- also use SNS to push notifications to internet connected smart devices, as
-- well as other distributed services. Besides pushing cloud notifications
-- directly to mobile devices, Amazon SNS can also deliver notifications by
-- SMS text message or email, to Amazon Simple Queue Service (SQS) queues, or
-- to any HTTP endpoint. To prevent messages from being lost, all messages
-- published to Amazon SNS are stored redundantly across multiple availability
-- zones.
module Network.AWS.SNS.V2010_03_31.Types
    (
    -- * Service
      SNS
    -- ** Errors
    , Er (..)
    -- ** XML
    , xmlOptions

    -- * Topic
    , Topic (..)
    , tcTopicArn

    -- * Endpoint
    , Endpoint (..)
    , fEndpointArn
    , fAttributes

    -- * MessageAttributeValue
    , MessageAttributeValue (..)
    , mavDataType
    , mavStringValue
    , mavBinaryValue

    -- * PlatformApplication
    , PlatformApplication (..)
    , paPlatformApplicationArn
    , paAttributes

    -- * Subscription
    , Subscription (..)
    , ssnSubscriptionArn
    , ssnOwner
    , ssnProtocol
    , ssnEndpoint
    , ssnTopicArn

    ) where

import Network.AWS.Prelude
import Network.AWS.Signing.V4

-- | Supported version (@2010-03-31@) of the
-- @Amazon Simple Notification Service@ service.
data SNS deriving (Typeable)

instance AWSService SNS where
    type Sg SNS = V4
    data Er SNS
        = AuthorizationErrorException
            { _aeeMessage :: Maybe Text
            }
        | EndpointDisabledException
            { _edeMessage :: Maybe Text
            }
        | InternalErrorException
            { _ieeMessage :: Maybe Text
            }
        | InvalidParameterException
            { _ipeMessage :: Maybe Text
            }
        | InvalidParameterValueException
            { _ipveMessage :: Maybe Text
            }
        | NotFoundException
            { _nfeMessage :: Maybe Text
            }
        | PlatformApplicationDisabledException
            { _padeMessage :: Maybe Text
            }
        | SNSClient HttpException
        | SNSSerializer String
        | SNSService String
        | SubscriptionLimitExceededException
            { _sleeMessage :: Maybe Text
            }
        | TopicLimitExceededException
            { _tleeMessage :: Maybe Text
            }

    service = Service'
        { _svcEndpoint = Regional
        , _svcPrefix   = "sns"
        , _svcVersion  = "2010-03-31"
        , _svcTarget   = Nothing
        }

deriving instance Show    (Er SNS)
deriving instance Generic (Er SNS)

instance AWSError (Er SNS) where
    awsError = const "SNSError"

instance AWSServiceError (Er SNS) where
    serviceError    = SNSService
    clientError     = SNSClient
    serializerError = SNSSerializer

instance Exception (Er SNS)

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def
    { xmlNamespace = Just "http://sns.amazonaws.com/doc/2010-03-31/"
    }

-- | A wrapper type for the topic's Amazon Resource Name (ARN). To retrieve a
-- topic's attributes, use GetTopicAttributes.
newtype Topic = Topic
    { _tcTopicArn :: Maybe Text
      -- ^ The topic's ARN.
    } deriving (Show, Generic)

-- | The topic's ARN.
tcTopicArn :: Lens' Topic (Maybe Text)
tcTopicArn f x =
    f (_tcTopicArn x)
        <&> \y -> x { _tcTopicArn = y }
{-# INLINE tcTopicArn #-}

instance FromXML Topic where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Topic"

-- | Endpoint for mobile app and device.
data Endpoint = Endpoint
    { _fEndpointArn :: Maybe Text
      -- ^ EndpointArn for mobile app and device.
    , _fAttributes :: Map Text Text
      -- ^ Attributes for endpoint.
    } deriving (Show, Generic)

-- | EndpointArn for mobile app and device.
fEndpointArn :: Lens' Endpoint (Maybe Text)
fEndpointArn f x =
    f (_fEndpointArn x)
        <&> \y -> x { _fEndpointArn = y }
{-# INLINE fEndpointArn #-}

-- | Attributes for endpoint.
fAttributes :: Lens' Endpoint (Map Text Text)
fAttributes f x =
    f (_fAttributes x)
        <&> \y -> x { _fAttributes = y }
{-# INLINE fAttributes #-}

instance FromXML Endpoint where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Endpoint"

-- | The user-specified message attribute value. For string data types, the
-- value attribute has the same restrictions on the content as the message
-- body. For more information, see Publish. Name, type, and value must not be
-- empty or null. In addition, the message body should not be empty or null.
-- All parts of the message attribute, including name, type, and value, are
-- included in the message size restriction, which is currently 256 KB
-- (262,144 bytes). For more information, see Using Amazon SNS Message
-- Attributes.
data MessageAttributeValue = MessageAttributeValue
    { _mavDataType :: Text
      -- ^ Amazon SNS supports the following logical data types: String,
      -- Number, and Binary. For more information, see Message Attribute
      -- Data Types.
    , _mavStringValue :: Maybe Text
      -- ^ Strings are Unicode with UTF8 binary encoding. For a list of code
      -- values, see
      -- http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters.
    , _mavBinaryValue :: Maybe ByteString
      -- ^ Binary type attributes can store any binary data, for example,
      -- compressed data, encrypted data, or images.
    } deriving (Show, Generic)

-- | Amazon SNS supports the following logical data types: String, Number, and
-- Binary. For more information, see Message Attribute Data Types.
mavDataType :: Lens' MessageAttributeValue (Text)
mavDataType f x =
    f (_mavDataType x)
        <&> \y -> x { _mavDataType = y }
{-# INLINE mavDataType #-}

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

instance ToQuery MessageAttributeValue where
    toQuery = genericQuery def

-- | Platform application object.
data PlatformApplication = PlatformApplication
    { _paPlatformApplicationArn :: Maybe Text
      -- ^ PlatformApplicationArn for platform application object.
    , _paAttributes :: Map Text Text
      -- ^ Attributes for platform application object.
    } deriving (Show, Generic)

-- | PlatformApplicationArn for platform application object.
paPlatformApplicationArn :: Lens' PlatformApplication (Maybe Text)
paPlatformApplicationArn f x =
    f (_paPlatformApplicationArn x)
        <&> \y -> x { _paPlatformApplicationArn = y }
{-# INLINE paPlatformApplicationArn #-}

-- | Attributes for platform application object.
paAttributes :: Lens' PlatformApplication (Map Text Text)
paAttributes f x =
    f (_paAttributes x)
        <&> \y -> x { _paAttributes = y }
{-# INLINE paAttributes #-}

instance FromXML PlatformApplication where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PlatformApplication"

-- | A wrapper type for the attributes of an Amazon SNS subscription.
data Subscription = Subscription
    { _ssnSubscriptionArn :: Maybe Text
      -- ^ The subscription's ARN.
    , _ssnOwner :: Maybe Text
      -- ^ The subscription's owner.
    , _ssnProtocol :: Maybe Text
      -- ^ The subscription's protocol.
    , _ssnEndpoint :: Maybe Text
      -- ^ The subscription's endpoint (format depends on the protocol).
    , _ssnTopicArn :: Maybe Text
      -- ^ The ARN of the subscription's topic.
    } deriving (Show, Generic)

-- | The subscription's ARN.
ssnSubscriptionArn :: Lens' Subscription (Maybe Text)
ssnSubscriptionArn f x =
    f (_ssnSubscriptionArn x)
        <&> \y -> x { _ssnSubscriptionArn = y }
{-# INLINE ssnSubscriptionArn #-}

-- | The subscription's owner.
ssnOwner :: Lens' Subscription (Maybe Text)
ssnOwner f x =
    f (_ssnOwner x)
        <&> \y -> x { _ssnOwner = y }
{-# INLINE ssnOwner #-}

-- | The subscription's protocol.
ssnProtocol :: Lens' Subscription (Maybe Text)
ssnProtocol f x =
    f (_ssnProtocol x)
        <&> \y -> x { _ssnProtocol = y }
{-# INLINE ssnProtocol #-}

-- | The subscription's endpoint (format depends on the protocol).
ssnEndpoint :: Lens' Subscription (Maybe Text)
ssnEndpoint f x =
    f (_ssnEndpoint x)
        <&> \y -> x { _ssnEndpoint = y }
{-# INLINE ssnEndpoint #-}

-- | The ARN of the subscription's topic.
ssnTopicArn :: Lens' Subscription (Maybe Text)
ssnTopicArn f x =
    f (_ssnTopicArn x)
        <&> \y -> x { _ssnTopicArn = y }
{-# INLINE ssnTopicArn #-}

instance FromXML Subscription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Subscription"
