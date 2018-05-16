{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SNS.Types.Product where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SNS.Types.Sum

-- | Endpoint for mobile app and device.
--
--
--
-- /See:/ 'endpoint' smart constructor.
data Endpoint = Endpoint'
  { _eAttributes  :: !(Maybe (Map Text Text))
  , _eEndpointARN :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Endpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eAttributes' - Attributes for endpoint.
--
-- * 'eEndpointARN' - EndpointArn for mobile app and device.
endpoint
    :: Endpoint
endpoint = Endpoint' {_eAttributes = Nothing, _eEndpointARN = Nothing}


-- | Attributes for endpoint.
eAttributes :: Lens' Endpoint (HashMap Text Text)
eAttributes = lens _eAttributes (\ s a -> s{_eAttributes = a}) . _Default . _Map

-- | EndpointArn for mobile app and device.
eEndpointARN :: Lens' Endpoint (Maybe Text)
eEndpointARN = lens _eEndpointARN (\ s a -> s{_eEndpointARN = a})

instance FromXML Endpoint where
        parseXML x
          = Endpoint' <$>
              (x .@? "Attributes" .!@ mempty >>=
                 may (parseXMLMap "entry" "key" "value"))
                <*> (x .@? "EndpointArn")

instance Hashable Endpoint where

instance NFData Endpoint where

-- | The user-specified message attribute value. For string data types, the value attribute has the same restrictions on the content as the message body. For more information, see <http://docs.aws.amazon.com/sns/latest/api/API_Publish.html Publish> .
--
--
-- Name, type, and value must not be empty or null. In addition, the message body should not be empty or null. All parts of the message attribute, including name, type, and value, are included in the message size restriction, which is currently 256 KB (262,144 bytes). For more information, see <http://docs.aws.amazon.com/sns/latest/dg/SNSMessageAttributes.html Using Amazon SNS Message Attributes> .
--
--
-- /See:/ 'messageAttributeValue' smart constructor.
data MessageAttributeValue = MessageAttributeValue'
  { _mavBinaryValue :: !(Maybe Base64)
  , _mavStringValue :: !(Maybe Text)
  , _mavDataType    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MessageAttributeValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mavBinaryValue' - Binary type attributes can store any binary data, for example, compressed data, encrypted data, or images.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'mavStringValue' - Strings are Unicode with UTF8 binary encoding. For a list of code values, see <http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters> .
--
-- * 'mavDataType' - Amazon SNS supports the following logical data types: String, Number, and Binary. For more information, see <http://docs.aws.amazon.com/sns/latest/dg/SNSMessageAttributes.html#SNSMessageAttributes.DataTypes Message Attribute Data Types> .
messageAttributeValue
    :: Text -- ^ 'mavDataType'
    -> MessageAttributeValue
messageAttributeValue pDataType_ =
  MessageAttributeValue'
    { _mavBinaryValue = Nothing
    , _mavStringValue = Nothing
    , _mavDataType = pDataType_
    }


-- | Binary type attributes can store any binary data, for example, compressed data, encrypted data, or images.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
mavBinaryValue :: Lens' MessageAttributeValue (Maybe ByteString)
mavBinaryValue = lens _mavBinaryValue (\ s a -> s{_mavBinaryValue = a}) . mapping _Base64

-- | Strings are Unicode with UTF8 binary encoding. For a list of code values, see <http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters> .
mavStringValue :: Lens' MessageAttributeValue (Maybe Text)
mavStringValue = lens _mavStringValue (\ s a -> s{_mavStringValue = a})

-- | Amazon SNS supports the following logical data types: String, Number, and Binary. For more information, see <http://docs.aws.amazon.com/sns/latest/dg/SNSMessageAttributes.html#SNSMessageAttributes.DataTypes Message Attribute Data Types> .
mavDataType :: Lens' MessageAttributeValue Text
mavDataType = lens _mavDataType (\ s a -> s{_mavDataType = a})

instance Hashable MessageAttributeValue where

instance NFData MessageAttributeValue where

instance ToQuery MessageAttributeValue where
        toQuery MessageAttributeValue'{..}
          = mconcat
              ["BinaryValue" =: _mavBinaryValue,
               "StringValue" =: _mavStringValue,
               "DataType" =: _mavDataType]

-- | Platform application object.
--
--
--
-- /See:/ 'platformApplication' smart constructor.
data PlatformApplication = PlatformApplication'
  { _paPlatformApplicationARN :: !(Maybe Text)
  , _paAttributes             :: !(Maybe (Map Text Text))
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PlatformApplication' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'paPlatformApplicationARN' - PlatformApplicationArn for platform application object.
--
-- * 'paAttributes' - Attributes for platform application object.
platformApplication
    :: PlatformApplication
platformApplication =
  PlatformApplication'
    {_paPlatformApplicationARN = Nothing, _paAttributes = Nothing}


-- | PlatformApplicationArn for platform application object.
paPlatformApplicationARN :: Lens' PlatformApplication (Maybe Text)
paPlatformApplicationARN = lens _paPlatformApplicationARN (\ s a -> s{_paPlatformApplicationARN = a})

-- | Attributes for platform application object.
paAttributes :: Lens' PlatformApplication (HashMap Text Text)
paAttributes = lens _paAttributes (\ s a -> s{_paAttributes = a}) . _Default . _Map

instance FromXML PlatformApplication where
        parseXML x
          = PlatformApplication' <$>
              (x .@? "PlatformApplicationArn") <*>
                (x .@? "Attributes" .!@ mempty >>=
                   may (parseXMLMap "entry" "key" "value"))

instance Hashable PlatformApplication where

instance NFData PlatformApplication where

-- | A wrapper type for the attributes of an Amazon SNS subscription.
--
--
--
-- /See:/ 'subscription' smart constructor.
data Subscription = Subscription'
  { _sProtocol        :: !(Maybe Text)
  , _sOwner           :: !(Maybe Text)
  , _sTopicARN        :: !(Maybe Text)
  , _sEndpoint        :: !(Maybe Text)
  , _sSubscriptionARN :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Subscription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sProtocol' - The subscription's protocol.
--
-- * 'sOwner' - The subscription's owner.
--
-- * 'sTopicARN' - The ARN of the subscription's topic.
--
-- * 'sEndpoint' - The subscription's endpoint (format depends on the protocol).
--
-- * 'sSubscriptionARN' - The subscription's ARN.
subscription
    :: Subscription
subscription =
  Subscription'
    { _sProtocol = Nothing
    , _sOwner = Nothing
    , _sTopicARN = Nothing
    , _sEndpoint = Nothing
    , _sSubscriptionARN = Nothing
    }


-- | The subscription's protocol.
sProtocol :: Lens' Subscription (Maybe Text)
sProtocol = lens _sProtocol (\ s a -> s{_sProtocol = a})

-- | The subscription's owner.
sOwner :: Lens' Subscription (Maybe Text)
sOwner = lens _sOwner (\ s a -> s{_sOwner = a})

-- | The ARN of the subscription's topic.
sTopicARN :: Lens' Subscription (Maybe Text)
sTopicARN = lens _sTopicARN (\ s a -> s{_sTopicARN = a})

-- | The subscription's endpoint (format depends on the protocol).
sEndpoint :: Lens' Subscription (Maybe Text)
sEndpoint = lens _sEndpoint (\ s a -> s{_sEndpoint = a})

-- | The subscription's ARN.
sSubscriptionARN :: Lens' Subscription (Maybe Text)
sSubscriptionARN = lens _sSubscriptionARN (\ s a -> s{_sSubscriptionARN = a})

instance FromXML Subscription where
        parseXML x
          = Subscription' <$>
              (x .@? "Protocol") <*> (x .@? "Owner") <*>
                (x .@? "TopicArn")
                <*> (x .@? "Endpoint")
                <*> (x .@? "SubscriptionArn")

instance Hashable Subscription where

instance NFData Subscription where

-- | A wrapper type for the topic's Amazon Resource Name (ARN). To retrieve a topic's attributes, use @GetTopicAttributes@ .
--
--
--
-- /See:/ 'topic' smart constructor.
newtype Topic = Topic'
  { _tTopicARN :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Topic' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tTopicARN' - The topic's ARN.
topic
    :: Topic
topic = Topic' {_tTopicARN = Nothing}


-- | The topic's ARN.
tTopicARN :: Lens' Topic (Maybe Text)
tTopicARN = lens _tTopicARN (\ s a -> s{_tTopicARN = a})

instance FromXML Topic where
        parseXML x = Topic' <$> (x .@? "TopicArn")

instance Hashable Topic where

instance NFData Topic where
