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

-- Module      : Network.AWS.SNS.Types
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

module Network.AWS.SNS.Types
    (
    -- * Service
      SNS
    -- ** Error
    , RESTError
    -- ** XML
    , ns

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
    , s1Endpoint
    , s1Owner
    , s1Protocol
    , s1SubscriptionArn
    , s1TopicArn

    -- * Endpoint
    , Endpoint
    , endpoint
    , eAttributes
    , eEndpointArn
    ) where

import Network.AWS.Error
import Network.AWS.Prelude
import Network.AWS.Signing
import qualified GHC.Exts

-- | Version @2010-03-31@ of the Amazon Simple Notification Service service.
data SNS

instance AWSService SNS where
    type Sg SNS = V4
    type Er SNS = RESTError

    service = service'
      where
        service' :: Service SNS
        service' = Service
              { _svcAbbrev       = "SNS"
              , _svcPrefix       = "sns"
              , _svcVersion      = "2010-03-31"
              , _svcTargetPrefix = Nothing
              , _svcJSONVersion  = Nothing
              , _svcDelay        = Exp 0.05 2 5
              , _svcHandle       = handle
              , _svcRetry        = retry
              }

        handle :: Status
               -> Maybe (LazyByteString -> ServiceError RESTError)
        handle = restError statusSuccess service'

        retry :: Status
              -> RESTError
              -> Bool
        retry (statusCode -> s) (awsErrorCode -> e)
            | s == 500  = True -- General Server Error
            | s == 509  = True -- Limit Exceeded
            | s == 503  = True -- Service Unavailable
            | s == 400  = "Throttling" == e -- Throttling
            | otherwise = False

ns :: Text
ns = "http://sns.amazonaws.com/doc/2010-03-31/"
{-# INLINE ns #-}

newtype Topic = Topic
    { _tTopicArn :: Maybe Text
    } deriving (Eq, Ord, Show, Monoid)

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
    parseXML x = Topic
        <$> x .@? "TopicArn"

instance ToQuery Topic where
    toQuery Topic{..} = mconcat
        [ "TopicArn" =? _tTopicArn
        ]

data MessageAttributeValue = MessageAttributeValue
    { _mavBinaryValue :: Maybe Base64
    , _mavDataType    :: Text
    , _mavStringValue :: Maybe Text
    } deriving (Eq, Show)

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
-- Binary. For more information, see <http://docs.aws.amazon.com/sns/latest/dg/SNSMessageAttributes.html#SNSMessageAttributes.DataTypes Message Attribute Data Types>.
mavDataType :: Lens' MessageAttributeValue Text
mavDataType = lens _mavDataType (\s a -> s { _mavDataType = a })

-- | Strings are Unicode with UTF8 binary encoding. For a list of code values, see <http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters>.
mavStringValue :: Lens' MessageAttributeValue (Maybe Text)
mavStringValue = lens _mavStringValue (\s a -> s { _mavStringValue = a })

instance FromXML MessageAttributeValue where
    parseXML x = MessageAttributeValue
        <$> x .@? "BinaryValue"
        <*> x .@  "DataType"
        <*> x .@? "StringValue"

instance ToQuery MessageAttributeValue where
    toQuery MessageAttributeValue{..} = mconcat
        [ "BinaryValue" =? _mavBinaryValue
        , "DataType"    =? _mavDataType
        , "StringValue" =? _mavStringValue
        ]

data PlatformApplication = PlatformApplication
    { _paAttributes             :: EMap "entry" "key" "value" Text Text
    , _paPlatformApplicationArn :: Maybe Text
    } deriving (Eq, Show)

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
paAttributes = lens _paAttributes (\s a -> s { _paAttributes = a }) . _EMap

-- | PlatformApplicationArn for platform application object.
paPlatformApplicationArn :: Lens' PlatformApplication (Maybe Text)
paPlatformApplicationArn =
    lens _paPlatformApplicationArn
        (\s a -> s { _paPlatformApplicationArn = a })

instance FromXML PlatformApplication where
    parseXML x = PlatformApplication
        <$> x .@? "Attributes" .!@ mempty
        <*> x .@? "PlatformApplicationArn"

instance ToQuery PlatformApplication where
    toQuery PlatformApplication{..} = mconcat
        [ "Attributes"             =? _paAttributes
        , "PlatformApplicationArn" =? _paPlatformApplicationArn
        ]

data Subscription = Subscription
    { _s1Endpoint        :: Maybe Text
    , _s1Owner           :: Maybe Text
    , _s1Protocol        :: Maybe Text
    , _s1SubscriptionArn :: Maybe Text
    , _s1TopicArn        :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'Subscription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 's1Endpoint' @::@ 'Maybe' 'Text'
--
-- * 's1Owner' @::@ 'Maybe' 'Text'
--
-- * 's1Protocol' @::@ 'Maybe' 'Text'
--
-- * 's1SubscriptionArn' @::@ 'Maybe' 'Text'
--
-- * 's1TopicArn' @::@ 'Maybe' 'Text'
--
subscription :: Subscription
subscription = Subscription
    { _s1SubscriptionArn = Nothing
    , _s1Owner           = Nothing
    , _s1Protocol        = Nothing
    , _s1Endpoint        = Nothing
    , _s1TopicArn        = Nothing
    }

-- | The subscription's endpoint (format depends on the protocol).
s1Endpoint :: Lens' Subscription (Maybe Text)
s1Endpoint = lens _s1Endpoint (\s a -> s { _s1Endpoint = a })

-- | The subscription's owner.
s1Owner :: Lens' Subscription (Maybe Text)
s1Owner = lens _s1Owner (\s a -> s { _s1Owner = a })

-- | The subscription's protocol.
s1Protocol :: Lens' Subscription (Maybe Text)
s1Protocol = lens _s1Protocol (\s a -> s { _s1Protocol = a })

-- | The subscription's ARN.
s1SubscriptionArn :: Lens' Subscription (Maybe Text)
s1SubscriptionArn =
    lens _s1SubscriptionArn (\s a -> s { _s1SubscriptionArn = a })

-- | The ARN of the subscription's topic.
s1TopicArn :: Lens' Subscription (Maybe Text)
s1TopicArn = lens _s1TopicArn (\s a -> s { _s1TopicArn = a })

instance FromXML Subscription where
    parseXML x = Subscription
        <$> x .@? "Endpoint"
        <*> x .@? "Owner"
        <*> x .@? "Protocol"
        <*> x .@? "SubscriptionArn"
        <*> x .@? "TopicArn"

instance ToQuery Subscription where
    toQuery Subscription{..} = mconcat
        [ "Endpoint"        =? _s1Endpoint
        , "Owner"           =? _s1Owner
        , "Protocol"        =? _s1Protocol
        , "SubscriptionArn" =? _s1SubscriptionArn
        , "TopicArn"        =? _s1TopicArn
        ]

data Endpoint = Endpoint
    { _eAttributes  :: EMap "entry" "key" "value" Text Text
    , _eEndpointArn :: Maybe Text
    } deriving (Eq, Show)

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
eAttributes = lens _eAttributes (\s a -> s { _eAttributes = a }) . _EMap

-- | EndpointArn for mobile app and device.
eEndpointArn :: Lens' Endpoint (Maybe Text)
eEndpointArn = lens _eEndpointArn (\s a -> s { _eEndpointArn = a })

instance FromXML Endpoint where
    parseXML x = Endpoint
        <$> x .@? "Attributes" .!@ mempty
        <*> x .@? "EndpointArn"

instance ToQuery Endpoint where
    toQuery Endpoint{..} = mconcat
        [ "Attributes"  =? _eAttributes
        , "EndpointArn" =? _eEndpointArn
        ]
