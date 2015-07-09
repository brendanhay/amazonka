{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.Publish
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Sends a message to all of a topic\'s subscribed endpoints. When a
-- @messageId@ is returned, the message has been saved and Amazon SNS will
-- attempt to deliver it to the topic\'s subscribers shortly. The format of
-- the outgoing message to each subscribed endpoint depends on the
-- notification protocol selected.
--
-- To use the @Publish@ action for sending a message to a mobile endpoint,
-- such as an app on a Kindle device or mobile phone, you must specify the
-- EndpointArn. The EndpointArn is returned when making a call with the
-- @CreatePlatformEndpoint@ action. The second example below shows a
-- request and response for publishing to a mobile endpoint.
--
-- <http://docs.aws.amazon.com/sns/latest/api/API_Publish.html>
module Network.AWS.SNS.Publish
    (
    -- * Request
      Publish
    -- ** Request constructor
    , publish
    -- ** Request lenses
    , pubMessageAttributes
    , pubTargetARN
    , pubSubject
    , pubTopicARN
    , pubMessageStructure
    , pubMessage

    -- * Response
    , PublishResponse
    -- ** Response constructor
    , publishResponse
    -- ** Response lenses
    , prMessageId
    , prStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SNS.Types

-- | Input for Publish action.
--
-- /See:/ 'publish' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pubMessageAttributes'
--
-- * 'pubTargetARN'
--
-- * 'pubSubject'
--
-- * 'pubTopicARN'
--
-- * 'pubMessageStructure'
--
-- * 'pubMessage'
data Publish = Publish'
    { _pubMessageAttributes :: !(Maybe (Map Text MessageAttributeValue))
    , _pubTargetARN         :: !(Maybe Text)
    , _pubSubject           :: !(Maybe Text)
    , _pubTopicARN          :: !(Maybe Text)
    , _pubMessageStructure  :: !(Maybe Text)
    , _pubMessage           :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Publish' smart constructor.
publish :: Text -> Publish
publish pMessage =
    Publish'
    { _pubMessageAttributes = Nothing
    , _pubTargetARN = Nothing
    , _pubSubject = Nothing
    , _pubTopicARN = Nothing
    , _pubMessageStructure = Nothing
    , _pubMessage = pMessage
    }

-- | Message attributes for Publish action.
pubMessageAttributes :: Lens' Publish (HashMap Text MessageAttributeValue)
pubMessageAttributes = lens _pubMessageAttributes (\ s a -> s{_pubMessageAttributes = a}) . _Default . _Map;

-- | Either TopicArn or EndpointArn, but not both.
pubTargetARN :: Lens' Publish (Maybe Text)
pubTargetARN = lens _pubTargetARN (\ s a -> s{_pubTargetARN = a});

-- | Optional parameter to be used as the \"Subject\" line when the message
-- is delivered to email endpoints. This field will also be included, if
-- present, in the standard JSON messages delivered to other endpoints.
--
-- Constraints: Subjects must be ASCII text that begins with a letter,
-- number, or punctuation mark; must not include line breaks or control
-- characters; and must be less than 100 characters long.
pubSubject :: Lens' Publish (Maybe Text)
pubSubject = lens _pubSubject (\ s a -> s{_pubSubject = a});

-- | The topic you want to publish to.
pubTopicARN :: Lens' Publish (Maybe Text)
pubTopicARN = lens _pubTopicARN (\ s a -> s{_pubTopicARN = a});

-- | Set @MessageStructure@ to @json@ if you want to send a different message
-- for each protocol. For example, using one publish action, you can send a
-- short message to your SMS subscribers and a longer message to your email
-- subscribers. If you set @MessageStructure@ to @json@, the value of the
-- @Message@ parameter must:
--
-- -   be a syntactically valid JSON object; and
-- -   contain at least a top-level JSON key of \"default\" with a value
--     that is a string.
--
-- You can define other top-level keys that define the message you want to
-- send to a specific transport protocol (e.g., \"http\").
--
-- For information about sending different messages for each protocol using
-- the AWS Management Console, go to
-- <http://docs.aws.amazon.com/sns/latest/gsg/Publish.html#sns-message-formatting-by-protocol Create Different Messages for Each Protocol>
-- in the /Amazon Simple Notification Service Getting Started Guide/.
--
-- Valid value: @json@
pubMessageStructure :: Lens' Publish (Maybe Text)
pubMessageStructure = lens _pubMessageStructure (\ s a -> s{_pubMessageStructure = a});

-- | The message you want to send to the topic.
--
-- If you want to send the same message to all transport protocols, include
-- the text of the message as a String value.
--
-- If you want to send different messages for each transport protocol, set
-- the value of the @MessageStructure@ parameter to @json@ and use a JSON
-- object for the @Message@ parameter. See the Examples section for the
-- format of the JSON object.
--
-- Constraints: Messages must be UTF-8 encoded strings at most 256 KB in
-- size (262144 bytes, not 262144 characters).
--
-- JSON-specific constraints:
--
-- -   Keys in the JSON object that correspond to supported transport
--     protocols must have simple JSON string values.
-- -   The values will be parsed (unescaped) before they are used in
--     outgoing messages.
-- -   Outbound notifications are JSON encoded (meaning that the characters
--     will be reescaped for sending).
-- -   Values have a minimum length of 0 (the empty string, \"\", is
--     allowed).
-- -   Values have a maximum length bounded by the overall message size
--     (so, including multiple protocols may limit message sizes).
-- -   Non-string values will cause the key to be ignored.
-- -   Keys that do not correspond to supported transport protocols are
--     ignored.
-- -   Duplicate keys are not allowed.
-- -   Failure to parse or validate any key or value in the message will
--     cause the @Publish@ call to return an error (no partial delivery).
pubMessage :: Lens' Publish Text
pubMessage = lens _pubMessage (\ s a -> s{_pubMessage = a});

instance AWSRequest Publish where
        type Sv Publish = SNS
        type Rs Publish = PublishResponse
        request = post
        response
          = receiveXMLWrapper "PublishResult"
              (\ s h x ->
                 PublishResponse' <$>
                   (x .@? "MessageId") <*> (pure (fromEnum s)))

instance ToHeaders Publish where
        toHeaders = const mempty

instance ToPath Publish where
        toPath = const "/"

instance ToQuery Publish where
        toQuery Publish'{..}
          = mconcat
              ["Action" =: ("Publish" :: ByteString),
               "Version" =: ("2010-03-31" :: ByteString),
               "MessageAttributes" =:
                 toQuery
                   (toQueryMap "entry" "Name" "Value" <$>
                      _pubMessageAttributes),
               "TargetArn" =: _pubTargetARN,
               "Subject" =: _pubSubject, "TopicArn" =: _pubTopicARN,
               "MessageStructure" =: _pubMessageStructure,
               "Message" =: _pubMessage]

-- | Response for Publish action.
--
-- /See:/ 'publishResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'prMessageId'
--
-- * 'prStatus'
data PublishResponse = PublishResponse'
    { _prMessageId :: !(Maybe Text)
    , _prStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PublishResponse' smart constructor.
publishResponse :: Int -> PublishResponse
publishResponse pStatus =
    PublishResponse'
    { _prMessageId = Nothing
    , _prStatus = pStatus
    }

-- | Unique identifier assigned to the published message.
--
-- Length Constraint: Maximum 100 characters
prMessageId :: Lens' PublishResponse (Maybe Text)
prMessageId = lens _prMessageId (\ s a -> s{_prMessageId = a});

-- | FIXME: Undocumented member.
prStatus :: Lens' PublishResponse Int
prStatus = lens _prStatus (\ s a -> s{_prStatus = a});
