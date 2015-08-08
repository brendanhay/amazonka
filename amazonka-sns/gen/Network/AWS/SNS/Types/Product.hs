{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.Types.Product
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SNS.Types.Product where

import           Network.AWS.Prelude
import           Network.AWS.SNS.Types.Sum

-- | Endpoint for mobile app and device.
--
-- /See:/ 'endpoint' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eAttributes'
--
-- * 'eEndpointARN'
data Endpoint = Endpoint'
    { _eAttributes  :: !(Maybe (Map Text Text))
    , _eEndpointARN :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Endpoint' smart constructor.
endpoint :: Endpoint
endpoint =
    Endpoint'
    { _eAttributes = Nothing
    , _eEndpointARN = Nothing
    }

-- | Attributes for endpoint.
eAttributes :: Lens' Endpoint (HashMap Text Text)
eAttributes = lens _eAttributes (\ s a -> s{_eAttributes = a}) . _Default . _Map;

-- | EndpointArn for mobile app and device.
eEndpointARN :: Lens' Endpoint (Maybe Text)
eEndpointARN = lens _eEndpointARN (\ s a -> s{_eEndpointARN = a});

instance FromXML Endpoint where
        parseXML x
          = Endpoint' <$>
              (x .@? "Attributes" .!@ mempty >>=
                 may (parseXMLMap "entry" "key" "value"))
                <*> (x .@? "EndpointArn")

instance ToQuery Endpoint where
        toQuery Endpoint'{..}
          = mconcat
              ["Attributes" =:
                 toQuery
                   (toQueryMap "entry" "key" "value" <$> _eAttributes),
               "EndpointArn" =: _eEndpointARN]

-- | The user-specified message attribute value. For string data types, the
-- value attribute has the same restrictions on the content as the message
-- body. For more information, see
-- <http://docs.aws.amazon.com/sns/latest/api/API_Publish.html Publish>.
--
-- Name, type, and value must not be empty or null. In addition, the
-- message body should not be empty or null. All parts of the message
-- attribute, including name, type, and value, are included in the message
-- size restriction, which is currently 256 KB (262,144 bytes). For more
-- information, see
-- <http://docs.aws.amazon.com/sns/latest/dg/SNSMessageAttributes.html Using Amazon SNS Message Attributes>.
--
-- /See:/ 'messageAttributeValue' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mavBinaryValue'
--
-- * 'mavStringValue'
--
-- * 'mavDataType'
data MessageAttributeValue = MessageAttributeValue'
    { _mavBinaryValue :: !(Maybe Base64)
    , _mavStringValue :: !(Maybe Text)
    , _mavDataType    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'MessageAttributeValue' smart constructor.
messageAttributeValue :: Text -> MessageAttributeValue
messageAttributeValue pDataType_ =
    MessageAttributeValue'
    { _mavBinaryValue = Nothing
    , _mavStringValue = Nothing
    , _mavDataType = pDataType_
    }

-- | Binary type attributes can store any binary data, for example,
-- compressed data, encrypted data, or images.
mavBinaryValue :: Lens' MessageAttributeValue (Maybe ByteString)
mavBinaryValue = lens _mavBinaryValue (\ s a -> s{_mavBinaryValue = a}) . mapping _Base64;

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

-- | Platform application object.
--
-- /See:/ 'platformApplication' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'paPlatformApplicationARN'
--
-- * 'paAttributes'
data PlatformApplication = PlatformApplication'
    { _paPlatformApplicationARN :: !(Maybe Text)
    , _paAttributes             :: !(Maybe (Map Text Text))
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PlatformApplication' smart constructor.
platformApplication :: PlatformApplication
platformApplication =
    PlatformApplication'
    { _paPlatformApplicationARN = Nothing
    , _paAttributes = Nothing
    }

-- | PlatformApplicationArn for platform application object.
paPlatformApplicationARN :: Lens' PlatformApplication (Maybe Text)
paPlatformApplicationARN = lens _paPlatformApplicationARN (\ s a -> s{_paPlatformApplicationARN = a});

-- | Attributes for platform application object.
paAttributes :: Lens' PlatformApplication (HashMap Text Text)
paAttributes = lens _paAttributes (\ s a -> s{_paAttributes = a}) . _Default . _Map;

instance FromXML PlatformApplication where
        parseXML x
          = PlatformApplication' <$>
              (x .@? "PlatformApplicationArn") <*>
                (x .@? "Attributes" .!@ mempty >>=
                   may (parseXMLMap "entry" "key" "value"))

-- | A wrapper type for the attributes of an Amazon SNS subscription.
--
-- /See:/ 'subscription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sProtocol'
--
-- * 'sOwner'
--
-- * 'sTopicARN'
--
-- * 'sEndpoint'
--
-- * 'sSubscriptionARN'
data Subscription = Subscription'
    { _sProtocol        :: !(Maybe Text)
    , _sOwner           :: !(Maybe Text)
    , _sTopicARN        :: !(Maybe Text)
    , _sEndpoint        :: !(Maybe Endpoint)
    , _sSubscriptionARN :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Subscription' smart constructor.
subscription :: Subscription
subscription =
    Subscription'
    { _sProtocol = Nothing
    , _sOwner = Nothing
    , _sTopicARN = Nothing
    , _sEndpoint = Nothing
    , _sSubscriptionARN = Nothing
    }

-- | The subscription\'s protocol.
sProtocol :: Lens' Subscription (Maybe Text)
sProtocol = lens _sProtocol (\ s a -> s{_sProtocol = a});

-- | The subscription\'s owner.
sOwner :: Lens' Subscription (Maybe Text)
sOwner = lens _sOwner (\ s a -> s{_sOwner = a});

-- | The ARN of the subscription\'s topic.
sTopicARN :: Lens' Subscription (Maybe Text)
sTopicARN = lens _sTopicARN (\ s a -> s{_sTopicARN = a});

-- | The subscription\'s endpoint (format depends on the protocol).
sEndpoint :: Lens' Subscription (Maybe Endpoint)
sEndpoint = lens _sEndpoint (\ s a -> s{_sEndpoint = a});

-- | The subscription\'s ARN.
sSubscriptionARN :: Lens' Subscription (Maybe Text)
sSubscriptionARN = lens _sSubscriptionARN (\ s a -> s{_sSubscriptionARN = a});

instance FromXML Subscription where
        parseXML x
          = Subscription' <$>
              (x .@? "Protocol") <*> (x .@? "Owner") <*>
                (x .@? "TopicArn")
                <*> (x .@? "Endpoint")
                <*> (x .@? "SubscriptionArn")

-- | A wrapper type for the topic\'s Amazon Resource Name (ARN). To retrieve
-- a topic\'s attributes, use @GetTopicAttributes@.
--
-- /See:/ 'topic' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tTopicARN'
newtype Topic = Topic'
    { _tTopicARN :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Topic' smart constructor.
topic :: Topic
topic =
    Topic'
    { _tTopicARN = Nothing
    }

-- | The topic\'s ARN.
tTopicARN :: Lens' Topic (Maybe Text)
tTopicARN = lens _tTopicARN (\ s a -> s{_tTopicARN = a});

instance FromXML Topic where
        parseXML x = Topic' <$> (x .@? "TopicArn")
