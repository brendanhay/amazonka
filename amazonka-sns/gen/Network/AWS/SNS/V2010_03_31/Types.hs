{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TemplateHaskell             #-}
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
    ( module Network.AWS.SNS.V2010_03_31.Types
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
      -- ^ The topic's ARN.
    } deriving (Show, Generic)

instance FromXML Topic where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Topic"

-- | Endpoint for mobile app and device.
data Endpoint = Endpoint
    { _fAttributes :: Map Text Text
      -- ^ Attributes for endpoint.
    , _fEndpointArn :: Maybe Text
      -- ^ EndpointArn for mobile app and device.
    } deriving (Show, Generic)

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
    { _mavBinaryValue :: Maybe ByteString
      -- ^ Binary type attributes can store any binary data, for example,
      -- compressed data, encrypted data, or images.
    , _mavStringValue :: Maybe Text
      -- ^ Strings are Unicode with UTF8 binary encoding. For a list of code
      -- values, see
      -- http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters.
    , _mavDataType :: Text
      -- ^ Amazon SNS supports the following logical data types: String,
      -- Number, and Binary. For more information, see Message Attribute
      -- Data Types.
    } deriving (Show, Generic)

instance ToQuery MessageAttributeValue where
    toQuery = genericQuery def

-- | Platform application object.
data PlatformApplication = PlatformApplication
    { _paPlatformApplicationArn :: Maybe Text
      -- ^ PlatformApplicationArn for platform application object.
    , _paAttributes :: Map Text Text
      -- ^ Attributes for platform application object.
    } deriving (Show, Generic)

instance FromXML PlatformApplication where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PlatformApplication"

-- | A wrapper type for the attributes of an Amazon SNS subscription.
data Subscription = Subscription
    { _snProtocol :: Maybe Text
      -- ^ The subscription's protocol.
    , _snOwner :: Maybe Text
      -- ^ The subscription's owner.
    , _snTopicArn :: Maybe Text
      -- ^ The ARN of the subscription's topic.
    , _snEndpoint :: Maybe Text
      -- ^ The subscription's endpoint (format depends on the protocol).
    , _snSubscriptionArn :: Maybe Text
      -- ^ The subscription's ARN.
    } deriving (Show, Generic)

instance FromXML Subscription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Subscription"

makeLenses ''Topic
makeLenses ''Endpoint
makeLenses ''MessageAttributeValue
makeLenses ''PlatformApplication
makeLenses ''Subscription
