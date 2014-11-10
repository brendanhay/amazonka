{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.SNS.Types
    (
    -- * Service
      SNS
    -- ** Errors
    , SNSError (..)
    , _SNSHttp
    , _SNSSerializer
    , _SNSService
    -- ** XML
    , xmlOptions

    -- * Topic
    , Topic
    , topic
    , tTopicArn

    -- * MessageAttributeValue
    , MessageAttributeValue
    , messageAttributeValue
    , mavBinaryValue
    , mavDataType
    , mavStringValue

    -- * PlatformApplication
    , PlatformApplication
    , platformApplication
    , paAttributes
    , paPlatformApplicationArn

    -- * Subscription
    , Subscription
    , subscription
    , sEndpoint
    , sOwner
    , sProtocol
    , sSubscriptionArn
    , sTopicArn

    -- * Endpoint
    , Endpoint
    , endpoint
    , eAttributes
    , eEndpointArn
    ) where

import Network.AWS.Prelude
import Network.AWS.Signing.V4

-- | Supported version (@2010-03-31@) of the Amazon Simple Notification Service.
data SNS deriving (Typeable)

instance AWSService SNS where
    type Sg SNS = V4
    type Er SNS = SNSError

    service = Service
        { _svcEndpoint = Regional
        , _svcAbbrev   = "SNS"
        , _svcPrefix   = "sns"
        , _svcVersion  = "2010-03-31"
        , _svcTarget   = Nothing
        }

    handle = xmlError alwaysFail

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def

newtype Topic = Topic
    { _tTopicArn :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'Topic' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tTopicArn' @::@ 'Maybe' 'Text'
--
topic :: Topic
topic = Topic
    { _tTopicArn = Nothing
    }

-- | The topic's ARN.
tTopicArn :: Lens' Topic (Maybe Text)
tTopicArn = lens _tTopicArn (\s a -> s { _tTopicArn = a })

instance FromXML Topic where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Topic"

instance ToQuery Topic

data MessageAttributeValue = MessageAttributeValue
    { _mavBinaryValue :: Maybe Base64
    , _mavDataType    :: Text
    , _mavStringValue :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'MessageAttributeValue' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mavBinaryValue' @::@ 'Maybe' 'Base64'
--
-- * 'mavDataType' @::@ 'Text'
--
-- * 'mavStringValue' @::@ 'Maybe' 'Text'
--
messageAttributeValue :: Text -- ^ 'mavDataType'
                      -> MessageAttributeValue
messageAttributeValue p1 = MessageAttributeValue
    { _mavDataType    = p1
    , _mavStringValue = Nothing
    , _mavBinaryValue = Nothing
    }

-- | Binary type attributes can store any binary data, for example, compressed
-- data, encrypted data, or images.
mavBinaryValue :: Lens' MessageAttributeValue (Maybe Base64)
mavBinaryValue = lens _mavBinaryValue (\s a -> s { _mavBinaryValue = a })

-- | Amazon SNS supports the following logical data types: String, Number, and
-- Binary. For more information, see Message Attribute Data Types.
mavDataType :: Lens' MessageAttributeValue Text
mavDataType = lens _mavDataType (\s a -> s { _mavDataType = a })

-- | Strings are Unicode with UTF8 binary encoding. For a list of code values,
-- see http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters.
mavStringValue :: Lens' MessageAttributeValue (Maybe Text)
mavStringValue = lens _mavStringValue (\s a -> s { _mavStringValue = a })

instance FromXML MessageAttributeValue where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "MessageAttributeValue"

instance ToQuery MessageAttributeValue

data PlatformApplication = PlatformApplication
    { _paAttributes             :: Map Text Text
    , _paPlatformApplicationArn :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'PlatformApplication' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'paAttributes' @::@ 'HashMap' 'Text' 'Text'
--
-- * 'paPlatformApplicationArn' @::@ 'Maybe' 'Text'
--
platformApplication :: PlatformApplication
platformApplication = PlatformApplication
    { _paPlatformApplicationArn = Nothing
    , _paAttributes             = mempty
    }

-- | Attributes for platform application object.
paAttributes :: Lens' PlatformApplication (HashMap Text Text)
paAttributes = lens _paAttributes (\s a -> s { _paAttributes = a })
    . _Map

-- | PlatformApplicationArn for platform application object.
paPlatformApplicationArn :: Lens' PlatformApplication (Maybe Text)
paPlatformApplicationArn =
    lens _paPlatformApplicationArn
        (\s a -> s { _paPlatformApplicationArn = a })

instance FromXML PlatformApplication where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PlatformApplication"

instance ToQuery PlatformApplication

data Subscription = Subscription
    { _sEndpoint        :: Maybe Text
    , _sOwner           :: Maybe Text
    , _sProtocol        :: Maybe Text
    , _sSubscriptionArn :: Maybe Text
    , _sTopicArn        :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'Subscription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sEndpoint' @::@ 'Maybe' 'Text'
--
-- * 'sOwner' @::@ 'Maybe' 'Text'
--
-- * 'sProtocol' @::@ 'Maybe' 'Text'
--
-- * 'sSubscriptionArn' @::@ 'Maybe' 'Text'
--
-- * 'sTopicArn' @::@ 'Maybe' 'Text'
--
subscription :: Subscription
subscription = Subscription
    { _sSubscriptionArn = Nothing
    , _sOwner           = Nothing
    , _sProtocol        = Nothing
    , _sEndpoint        = Nothing
    , _sTopicArn        = Nothing
    }

-- | The subscription's endpoint (format depends on the protocol).
sEndpoint :: Lens' Subscription (Maybe Text)
sEndpoint = lens _sEndpoint (\s a -> s { _sEndpoint = a })

-- | The subscription's owner.
sOwner :: Lens' Subscription (Maybe Text)
sOwner = lens _sOwner (\s a -> s { _sOwner = a })

-- | The subscription's protocol.
sProtocol :: Lens' Subscription (Maybe Text)
sProtocol = lens _sProtocol (\s a -> s { _sProtocol = a })

-- | The subscription's ARN.
sSubscriptionArn :: Lens' Subscription (Maybe Text)
sSubscriptionArn = lens _sSubscriptionArn (\s a -> s { _sSubscriptionArn = a })

-- | The ARN of the subscription's topic.
sTopicArn :: Lens' Subscription (Maybe Text)
sTopicArn = lens _sTopicArn (\s a -> s { _sTopicArn = a })

instance FromXML Subscription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Subscription"

instance ToQuery Subscription

data Endpoint = Endpoint
    { _eAttributes  :: Map Text Text
    , _eEndpointArn :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'Endpoint' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eAttributes' @::@ 'HashMap' 'Text' 'Text'
--
-- * 'eEndpointArn' @::@ 'Maybe' 'Text'
--
endpoint :: Endpoint
endpoint = Endpoint
    { _eEndpointArn = Nothing
    , _eAttributes  = mempty
    }

-- | Attributes for endpoint.
eAttributes :: Lens' Endpoint (HashMap Text Text)
eAttributes = lens _eAttributes (\s a -> s { _eAttributes = a })
    . _Map

-- | EndpointArn for mobile app and device.
eEndpointArn :: Lens' Endpoint (Maybe Text)
eEndpointArn = lens _eEndpointArn (\s a -> s { _eEndpointArn = a })

instance FromXML Endpoint where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Endpoint"

instance ToQuery Endpoint
