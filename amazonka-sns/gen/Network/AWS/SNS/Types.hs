{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SNS.Types
    (
    -- * Service
      SNS

    -- * Errors
    , _EndpointDisabledException
    , _AuthorizationErrorException
    , _InvalidParameterException
    , _SubscriptionLimitExceededException
    , _PlatformApplicationDisabledException
    , _InternalErrorException
    , _NotFoundException
    , _InvalidParameterValueException
    , _TopicLimitExceededException

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

import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | Version @2010-03-31@ of the Amazon Simple Notification Service SDK.
data SNS

instance AWSService SNS where
    type Sg SNS = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "SNS"
            , _svcPrefix = "sns"
            , _svcVersion = "2010-03-31"
            , _svcEndpoint = defaultEndpoint svc
            , _svcTimeout = Just 70000000
            , _svcStatus = statusSuccess
            , _svcError = parseXMLError
            , _svcRetry = retry
            }
        retry =
            Exponential
            { _retryBase = 5.0e-2
            , _retryGrowth = 2
            , _retryAttempts = 5
            , _retryCheck = check
            }
        check e
          | has (hasCode "ThrottlingException" . hasStatus 400) e =
              Just "throttling_exception"
          | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
          | has (hasStatus 503) e = Just "service_unavailable"
          | has (hasStatus 500) e = Just "general_server_error"
          | has (hasStatus 509) e = Just "limit_exceeded"
          | otherwise = Nothing

-- | Exception error indicating endpoint disabled.
_EndpointDisabledException :: AWSError a => Getting (First ServiceError) a ServiceError
_EndpointDisabledException =
    _ServiceError . hasStatus 400 . hasCode "EndpointDisabled"

-- | Indicates that the user has been denied access to the requested
-- resource.
_AuthorizationErrorException :: AWSError a => Getting (First ServiceError) a ServiceError
_AuthorizationErrorException =
    _ServiceError . hasStatus 403 . hasCode "AuthorizationError"

-- | Indicates that a request parameter does not comply with the associated
-- constraints.
_InvalidParameterException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidParameterException =
    _ServiceError . hasStatus 400 . hasCode "InvalidParameter"

-- | Indicates that the customer already owns the maximum allowed number of
-- subscriptions.
_SubscriptionLimitExceededException :: AWSError a => Getting (First ServiceError) a ServiceError
_SubscriptionLimitExceededException =
    _ServiceError . hasStatus 403 . hasCode "SubscriptionLimitExceeded"

-- | Exception error indicating platform application disabled.
_PlatformApplicationDisabledException :: AWSError a => Getting (First ServiceError) a ServiceError
_PlatformApplicationDisabledException =
    _ServiceError . hasStatus 400 . hasCode "PlatformApplicationDisabled"

-- | Indicates an internal service error.
_InternalErrorException :: AWSError a => Getting (First ServiceError) a ServiceError
_InternalErrorException =
    _ServiceError . hasStatus 500 . hasCode "InternalError"

-- | Indicates that the requested resource does not exist.
_NotFoundException :: AWSError a => Getting (First ServiceError) a ServiceError
_NotFoundException = _ServiceError . hasStatus 404 . hasCode "NotFound"

-- | Indicates that a request parameter does not comply with the associated
-- constraints.
_InvalidParameterValueException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidParameterValueException =
    _ServiceError . hasStatus 400 . hasCode "ParameterValueInvalid"

-- | Indicates that the customer already owns the maximum allowed number of
-- topics.
_TopicLimitExceededException :: AWSError a => Getting (First ServiceError) a ServiceError
_TopicLimitExceededException =
    _ServiceError . hasStatus 403 . hasCode "TopicLimitExceeded"

-- | Endpoint for mobile app and device.
--
-- /See:/ 'endpoint' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'endAttributes'
--
-- * 'endEndpointARN'
data Endpoint = Endpoint'
    { _endAttributes  :: !(Maybe (Map Text Text))
    , _endEndpointARN :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Endpoint' smart constructor.
endpoint :: Endpoint
endpoint =
    Endpoint'
    { _endAttributes = Nothing
    , _endEndpointARN = Nothing
    }

-- | Attributes for endpoint.
endAttributes :: Lens' Endpoint (HashMap Text Text)
endAttributes = lens _endAttributes (\ s a -> s{_endAttributes = a}) . _Default . _Map;

-- | EndpointArn for mobile app and device.
endEndpointARN :: Lens' Endpoint (Maybe Text)
endEndpointARN = lens _endEndpointARN (\ s a -> s{_endEndpointARN = a});

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
                   (toQueryMap "entry" "key" "value" <$>
                      _endAttributes),
               "EndpointArn" =: _endEndpointARN]

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
messageAttributeValue pDataType =
    MessageAttributeValue'
    { _mavBinaryValue = Nothing
    , _mavStringValue = Nothing
    , _mavDataType = pDataType
    }

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
-- * 'subProtocol'
--
-- * 'subOwner'
--
-- * 'subTopicARN'
--
-- * 'subEndpoint'
--
-- * 'subSubscriptionARN'
data Subscription = Subscription'
    { _subProtocol        :: !(Maybe Text)
    , _subOwner           :: !(Maybe Text)
    , _subTopicARN        :: !(Maybe Text)
    , _subEndpoint        :: !(Maybe Endpoint)
    , _subSubscriptionARN :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Subscription' smart constructor.
subscription :: Subscription
subscription =
    Subscription'
    { _subProtocol = Nothing
    , _subOwner = Nothing
    , _subTopicARN = Nothing
    , _subEndpoint = Nothing
    , _subSubscriptionARN = Nothing
    }

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
-- * 'topTopicARN'
newtype Topic = Topic'
    { _topTopicARN :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Topic' smart constructor.
topic :: Topic
topic =
    Topic'
    { _topTopicARN = Nothing
    }

-- | The topic\'s ARN.
topTopicARN :: Lens' Topic (Maybe Text)
topTopicARN = lens _topTopicARN (\ s a -> s{_topTopicARN = a});

instance FromXML Topic where
        parseXML x = Topic' <$> (x .@? "TopicArn")
