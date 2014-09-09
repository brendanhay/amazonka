{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
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
    -- ** XML
    , xmlOptions

    -- * Topic
    , Topic
    , mkTopic
    , tTopicArn

    -- * Endpoint
    , Endpoint
    , mkEndpoint
    , eEndpointArn
    , eAttributes

    -- * MessageAttributeValue
    , MessageAttributeValue
    , mkMessageAttributeValue
    , mavDataType
    , mavStringValue
    , mavBinaryValue

    -- * PlatformApplication
    , PlatformApplication
    , mkPlatformApplication
    , paPlatformApplicationArn
    , paAttributes

    -- * Subscription
    , Subscription
    , mkSubscription
    , sSubscriptionArn
    , sOwner
    , sProtocol
    , sEndpoint
    , sTopicArn
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
    { _tTopicArn :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Topic' data type.
--
-- 'Topic' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @TopicArn ::@ @Maybe Text@
--
mkTopic :: Topic
mkTopic = Topic
    { _tTopicArn = Nothing
    }

-- | The topic's ARN.
tTopicArn :: Lens' Topic (Maybe Text)
tTopicArn = lens _tTopicArn (\s a -> s { _tTopicArn = a })

instance FromXML Topic where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Topic"

-- | Endpoint for mobile app and device.
data Endpoint = Endpoint
    { _eEndpointArn :: Maybe Text
    , _eAttributes :: Map Text Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Endpoint' data type.
--
-- 'Endpoint' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @EndpointArn ::@ @Maybe Text@
--
-- * @Attributes ::@ @Map Text Text@
--
mkEndpoint :: Endpoint
mkEndpoint = Endpoint
    { _eEndpointArn = Nothing
    , _eAttributes = mempty
    }

-- | EndpointArn for mobile app and device.
eEndpointArn :: Lens' Endpoint (Maybe Text)
eEndpointArn = lens _eEndpointArn (\s a -> s { _eEndpointArn = a })

-- | Attributes for endpoint.
eAttributes :: Lens' Endpoint (Map Text Text)
eAttributes = lens _eAttributes (\s a -> s { _eAttributes = a })

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
    , _mavStringValue :: Maybe Text
    , _mavBinaryValue :: Maybe ByteString
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'MessageAttributeValue' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DataType ::@ @Text@
--
-- * @StringValue ::@ @Maybe Text@
--
-- * @BinaryValue ::@ @Maybe ByteString@
--
mkMessageAttributeValue :: Text -- ^ 'mavDataType'
                        -> MessageAttributeValue
mkMessageAttributeValue p1 = MessageAttributeValue
    { _mavDataType = p1
    , _mavStringValue = Nothing
    , _mavBinaryValue = Nothing
    }

-- | Amazon SNS supports the following logical data types: String, Number, and
-- Binary. For more information, see Message Attribute Data Types.
mavDataType :: Lens' MessageAttributeValue Text
mavDataType = lens _mavDataType (\s a -> s { _mavDataType = a })

-- | Strings are Unicode with UTF8 binary encoding. For a list of code values,
-- see http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters.
mavStringValue :: Lens' MessageAttributeValue (Maybe Text)
mavStringValue = lens _mavStringValue (\s a -> s { _mavStringValue = a })

-- | Binary type attributes can store any binary data, for example, compressed
-- data, encrypted data, or images.
mavBinaryValue :: Lens' MessageAttributeValue (Maybe ByteString)
mavBinaryValue = lens _mavBinaryValue (\s a -> s { _mavBinaryValue = a })

instance ToQuery MessageAttributeValue where
    toQuery = genericQuery def

-- | Platform application object.
data PlatformApplication = PlatformApplication
    { _paPlatformApplicationArn :: Maybe Text
    , _paAttributes :: Map Text Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'PlatformApplication' data type.
--
-- 'PlatformApplication' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @PlatformApplicationArn ::@ @Maybe Text@
--
-- * @Attributes ::@ @Map Text Text@
--
mkPlatformApplication :: PlatformApplication
mkPlatformApplication = PlatformApplication
    { _paPlatformApplicationArn = Nothing
    , _paAttributes = mempty
    }

-- | PlatformApplicationArn for platform application object.
paPlatformApplicationArn :: Lens' PlatformApplication (Maybe Text)
paPlatformApplicationArn =
    lens _paPlatformApplicationArn
         (\s a -> s { _paPlatformApplicationArn = a })

-- | Attributes for platform application object.
paAttributes :: Lens' PlatformApplication (Map Text Text)
paAttributes = lens _paAttributes (\s a -> s { _paAttributes = a })

instance FromXML PlatformApplication where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PlatformApplication"

-- | A wrapper type for the attributes of an Amazon SNS subscription.
data Subscription = Subscription
    { _sSubscriptionArn :: Maybe Text
    , _sOwner :: Maybe Text
    , _sProtocol :: Maybe Text
    , _sEndpoint :: Maybe Text
    , _sTopicArn :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Subscription' data type.
--
-- 'Subscription' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SubscriptionArn ::@ @Maybe Text@
--
-- * @Owner ::@ @Maybe Text@
--
-- * @Protocol ::@ @Maybe Text@
--
-- * @Endpoint ::@ @Maybe Text@
--
-- * @TopicArn ::@ @Maybe Text@
--
mkSubscription :: Subscription
mkSubscription = Subscription
    { _sSubscriptionArn = Nothing
    , _sOwner = Nothing
    , _sProtocol = Nothing
    , _sEndpoint = Nothing
    , _sTopicArn = Nothing
    }

-- | The subscription's ARN.
sSubscriptionArn :: Lens' Subscription (Maybe Text)
sSubscriptionArn =
    lens _sSubscriptionArn (\s a -> s { _sSubscriptionArn = a })

-- | The subscription's owner.
sOwner :: Lens' Subscription (Maybe Text)
sOwner = lens _sOwner (\s a -> s { _sOwner = a })

-- | The subscription's protocol.
sProtocol :: Lens' Subscription (Maybe Text)
sProtocol = lens _sProtocol (\s a -> s { _sProtocol = a })

-- | The subscription's endpoint (format depends on the protocol).
sEndpoint :: Lens' Subscription (Maybe Text)
sEndpoint = lens _sEndpoint (\s a -> s { _sEndpoint = a })

-- | The ARN of the subscription's topic.
sTopicArn :: Lens' Subscription (Maybe Text)
sTopicArn = lens _sTopicArn (\s a -> s { _sTopicArn = a })

instance FromXML Subscription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Subscription"
