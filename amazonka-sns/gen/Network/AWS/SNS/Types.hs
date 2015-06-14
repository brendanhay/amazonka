{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Network.AWS.SNS.Types
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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
    -- ** Errors
    , RESTError

    -- * Endpoint
    , Endpoint
    , endpoint
    , endAttributes
    , endEndpointARN

    -- * MessageAttributeValue
    , MessageAttributeValue
    , messageAttributeValue
    , mavBinaryValue
    , mavStringValue
    , mavDataType

    -- * PlatformApplication
    , PlatformApplication
    , platformApplication
    , paPlatformApplicationARN
    , paAttributes

    -- * Subscription
    , Subscription
    , subscription
    , subProtocol
    , subOwner
    , subTopicARN
    , subEndpoint
    , subSubscriptionARN

    -- * Topic
    , Topic
    , topic
    , topTopicARN
    ) where

import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | Version @2010-03-31@ of the Amazon Simple Notification Service SDK.
data SNS

instance AWSService SNS where
    type Sg SNS = V4
    type Er SNS = RESTError

    service = service'
      where
        service' :: Service SNS
        service' = Service
            { _svcAbbrev  = "SNS"
            , _svcPrefix  = "sns"
            , _svcVersion = "2010-03-31"
            , _svcHandle  = handle
            , _svcRetry   = retry
            }

        handle :: Status
               -> Maybe (LazyByteString -> ServiceError RESTError)
        handle = restError statusSuccess service'

        retry :: Retry SNS
        retry = undefined

        check :: Status
              -> RESTError
              -> Bool
        check (statusCode -> s) (awsErrorCode -> e) = undefined

-- | /See:/ 'endpoint' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'endAttributes'
--
-- * 'endEndpointARN'
data Endpoint = Endpoint'{_endAttributes :: HashMap Text Text, _endEndpointARN :: Maybe Text} deriving (Eq, Read, Show)

-- | 'Endpoint' smart constructor.
endpoint :: Endpoint
endpoint = Endpoint'{_endAttributes = mempty, _endEndpointARN = Nothing};

-- | Attributes for endpoint.
endAttributes :: Lens' Endpoint (HashMap Text Text)
endAttributes = lens _endAttributes (\ s a -> s{_endAttributes = a}) . _Coerce;

-- | EndpointArn for mobile app and device.
endEndpointARN :: Lens' Endpoint (Maybe Text)
endEndpointARN = lens _endEndpointARN (\ s a -> s{_endEndpointARN = a});

instance FromXML Endpoint where
        parseXML x
          = Endpoint' <$>
              (x .@? "Attributes" .!@ mempty >>=
                 parseXMLMap "entry" "key" "value")
                <*> x .@? "EndpointArn"

instance ToQuery Endpoint where
        toQuery Endpoint'{..}
          = mconcat
              ["Attributes" =:
                 toQueryMap "entry" "key" "value" _endAttributes,
               "EndpointArn" =: _endEndpointARN]

-- | /See:/ 'messageAttributeValue' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mavBinaryValue'
--
-- * 'mavStringValue'
--
-- * 'mavDataType'
data MessageAttributeValue = MessageAttributeValue'{_mavBinaryValue :: Maybe Base64, _mavStringValue :: Maybe Text, _mavDataType :: Text} deriving (Eq, Read, Show)

-- | 'MessageAttributeValue' smart constructor.
messageAttributeValue :: Text -> MessageAttributeValue
messageAttributeValue pDataType = MessageAttributeValue'{_mavBinaryValue = Nothing, _mavStringValue = Nothing, _mavDataType = pDataType};

-- | Binary type attributes can store any binary data, for example,
-- compressed data, encrypted data, or images.
mavBinaryValue :: Lens' MessageAttributeValue (Maybe Base64)
mavBinaryValue = lens _mavBinaryValue (\ s a -> s{_mavBinaryValue = a});

-- | Strings are Unicode with UTF8 binary encoding. For a list of code
-- values, see
-- <http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters>.
mavStringValue :: Lens' MessageAttributeValue (Maybe Text)
mavStringValue = lens _mavStringValue (\ s a -> s{_mavStringValue = a});

-- | Amazon SNS supports the following logical data types: String, Number,
-- and Binary. For more information, see
-- <http://docs.aws.amazon.com/sns/latest/dg/SNSMessageAttributes.html#SNSMessageAttributes.DataTypes Message Attribute Data Types>.
mavDataType :: Lens' MessageAttributeValue Text
mavDataType = lens _mavDataType (\ s a -> s{_mavDataType = a});

instance ToQuery MessageAttributeValue where
        toQuery MessageAttributeValue'{..}
          = mconcat
              ["BinaryValue" =: _mavBinaryValue,
               "StringValue" =: _mavStringValue,
               "DataType" =: _mavDataType]

-- | /See:/ 'platformApplication' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'paPlatformApplicationARN'
--
-- * 'paAttributes'
data PlatformApplication = PlatformApplication'{_paPlatformApplicationARN :: Maybe Text, _paAttributes :: HashMap Text Text} deriving (Eq, Read, Show)

-- | 'PlatformApplication' smart constructor.
platformApplication :: PlatformApplication
platformApplication = PlatformApplication'{_paPlatformApplicationARN = Nothing, _paAttributes = mempty};

-- | PlatformApplicationArn for platform application object.
paPlatformApplicationARN :: Lens' PlatformApplication (Maybe Text)
paPlatformApplicationARN = lens _paPlatformApplicationARN (\ s a -> s{_paPlatformApplicationARN = a});

-- | Attributes for platform application object.
paAttributes :: Lens' PlatformApplication (HashMap Text Text)
paAttributes = lens _paAttributes (\ s a -> s{_paAttributes = a}) . _Coerce;

instance FromXML PlatformApplication where
        parseXML x
          = PlatformApplication' <$>
              x .@? "PlatformApplicationArn" <*>
                (x .@? "Attributes" .!@ mempty >>=
                   parseXMLMap "entry" "key" "value")

-- | /See:/ 'subscription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'subProtocol'
--
-- * 'subOwner'
--
-- * 'subTopicARN'
--
-- * 'subEndpoint'
--
-- * 'subSubscriptionARN'
data Subscription = Subscription'{_subProtocol :: Maybe Text, _subOwner :: Maybe Text, _subTopicARN :: Maybe Text, _subEndpoint :: Maybe Endpoint, _subSubscriptionARN :: Maybe Text} deriving (Eq, Read, Show)

-- | 'Subscription' smart constructor.
subscription :: Subscription
subscription = Subscription'{_subProtocol = Nothing, _subOwner = Nothing, _subTopicARN = Nothing, _subEndpoint = Nothing, _subSubscriptionARN = Nothing};

-- | The subscription\'s protocol.
subProtocol :: Lens' Subscription (Maybe Text)
subProtocol = lens _subProtocol (\ s a -> s{_subProtocol = a});

-- | The subscription\'s owner.
subOwner :: Lens' Subscription (Maybe Text)
subOwner = lens _subOwner (\ s a -> s{_subOwner = a});

-- | The ARN of the subscription\'s topic.
subTopicARN :: Lens' Subscription (Maybe Text)
subTopicARN = lens _subTopicARN (\ s a -> s{_subTopicARN = a});

-- | The subscription\'s endpoint (format depends on the protocol).
subEndpoint :: Lens' Subscription (Maybe Endpoint)
subEndpoint = lens _subEndpoint (\ s a -> s{_subEndpoint = a});

-- | The subscription\'s ARN.
subSubscriptionARN :: Lens' Subscription (Maybe Text)
subSubscriptionARN = lens _subSubscriptionARN (\ s a -> s{_subSubscriptionARN = a});

instance FromXML Subscription where
        parseXML x
          = Subscription' <$>
              x .@? "Protocol" <*> x .@? "Owner" <*>
                x .@? "TopicArn"
                <*> x .@? "Endpoint"
                <*> x .@? "SubscriptionArn"

-- | /See:/ 'topic' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'topTopicARN'
newtype Topic = Topic'{_topTopicARN :: Maybe Text} deriving (Eq, Read, Show)

-- | 'Topic' smart constructor.
topic :: Topic
topic = Topic'{_topTopicARN = Nothing};

-- | The topic\'s ARN.
topTopicARN :: Lens' Topic (Maybe Text)
topTopicARN = lens _topTopicARN (\ s a -> s{_topTopicARN = a});

instance FromXML Topic where
        parseXML x = Topic' <$> x .@? "TopicArn"
