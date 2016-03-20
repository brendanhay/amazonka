{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.Publish
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends a message to all of a topic\'s subscribed endpoints. When a
-- 'messageId' is returned, the message has been saved and Amazon SNS will
-- attempt to deliver it to the topic\'s subscribers shortly. The format of
-- the outgoing message to each subscribed endpoint depends on the
-- notification protocol selected.
--
-- To use the 'Publish' action for sending a message to a mobile endpoint,
-- such as an app on a Kindle device or mobile phone, you must specify the
-- EndpointArn. The EndpointArn is returned when making a call with the
-- 'CreatePlatformEndpoint' action. The second example below shows a
-- request and response for publishing to a mobile endpoint.
module Network.AWS.SNS.Publish
    (
    -- * Creating a Request
      publish
    , Publish
    -- * Request Lenses
    , pSubject
    , pTargetARN
    , pMessageAttributes
    , pTopicARN
    , pMessageStructure
    , pMessage

    -- * Destructuring the Response
    , publishResponse
    , PublishResponse
    -- * Response Lenses
    , prsMessageId
    , prsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SNS.Types
import           Network.AWS.SNS.Types.Product

-- | Input for Publish action.
--
-- /See:/ 'publish' smart constructor.
data Publish = Publish'
    { _pSubject           :: !(Maybe Text)
    , _pTargetARN         :: !(Maybe Text)
    , _pMessageAttributes :: !(Maybe (Map Text MessageAttributeValue))
    , _pTopicARN          :: !(Maybe Text)
    , _pMessageStructure  :: !(Maybe Text)
    , _pMessage           :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Publish' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pSubject'
--
-- * 'pTargetARN'
--
-- * 'pMessageAttributes'
--
-- * 'pTopicARN'
--
-- * 'pMessageStructure'
--
-- * 'pMessage'
publish
    :: Text -- ^ 'pMessage'
    -> Publish
publish pMessage_ =
    Publish'
    { _pSubject = Nothing
    , _pTargetARN = Nothing
    , _pMessageAttributes = Nothing
    , _pTopicARN = Nothing
    , _pMessageStructure = Nothing
    , _pMessage = pMessage_
    }

-- | Optional parameter to be used as the \"Subject\" line when the message
-- is delivered to email endpoints. This field will also be included, if
-- present, in the standard JSON messages delivered to other endpoints.
--
-- Constraints: Subjects must be ASCII text that begins with a letter,
-- number, or punctuation mark; must not include line breaks or control
-- characters; and must be less than 100 characters long.
pSubject :: Lens' Publish (Maybe Text)
pSubject = lens _pSubject (\ s a -> s{_pSubject = a});

-- | Either TopicArn or EndpointArn, but not both.
pTargetARN :: Lens' Publish (Maybe Text)
pTargetARN = lens _pTargetARN (\ s a -> s{_pTargetARN = a});

-- | Message attributes for Publish action.
pMessageAttributes :: Lens' Publish (HashMap Text MessageAttributeValue)
pMessageAttributes = lens _pMessageAttributes (\ s a -> s{_pMessageAttributes = a}) . _Default . _Map;

-- | The topic you want to publish to.
pTopicARN :: Lens' Publish (Maybe Text)
pTopicARN = lens _pTopicARN (\ s a -> s{_pTopicARN = a});

-- | Set 'MessageStructure' to 'json' if you want to send a different message
-- for each protocol. For example, using one publish action, you can send a
-- short message to your SMS subscribers and a longer message to your email
-- subscribers. If you set 'MessageStructure' to 'json', the value of the
-- 'Message' parameter must:
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
-- Valid value: 'json'
pMessageStructure :: Lens' Publish (Maybe Text)
pMessageStructure = lens _pMessageStructure (\ s a -> s{_pMessageStructure = a});

-- | The message you want to send to the topic.
--
-- If you want to send the same message to all transport protocols, include
-- the text of the message as a String value.
--
-- If you want to send different messages for each transport protocol, set
-- the value of the 'MessageStructure' parameter to 'json' and use a JSON
-- object for the 'Message' parameter. See the Examples section for the
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
--     cause the 'Publish' call to return an error (no partial delivery).
pMessage :: Lens' Publish Text
pMessage = lens _pMessage (\ s a -> s{_pMessage = a});

instance AWSRequest Publish where
        type Rs Publish = PublishResponse
        request = postQuery sNS
        response
          = receiveXMLWrapper "PublishResult"
              (\ s h x ->
                 PublishResponse' <$>
                   (x .@? "MessageId") <*> (pure (fromEnum s)))

instance Hashable Publish

instance ToHeaders Publish where
        toHeaders = const mempty

instance ToPath Publish where
        toPath = const "/"

instance ToQuery Publish where
        toQuery Publish'{..}
          = mconcat
              ["Action" =: ("Publish" :: ByteString),
               "Version" =: ("2010-03-31" :: ByteString),
               "Subject" =: _pSubject, "TargetArn" =: _pTargetARN,
               "MessageAttributes" =:
                 toQuery
                   (toQueryMap "entry" "Name" "Value" <$>
                      _pMessageAttributes),
               "TopicArn" =: _pTopicARN,
               "MessageStructure" =: _pMessageStructure,
               "Message" =: _pMessage]

-- | Response for Publish action.
--
-- /See:/ 'publishResponse' smart constructor.
data PublishResponse = PublishResponse'
    { _prsMessageId      :: !(Maybe Text)
    , _prsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PublishResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prsMessageId'
--
-- * 'prsResponseStatus'
publishResponse
    :: Int -- ^ 'prsResponseStatus'
    -> PublishResponse
publishResponse pResponseStatus_ =
    PublishResponse'
    { _prsMessageId = Nothing
    , _prsResponseStatus = pResponseStatus_
    }

-- | Unique identifier assigned to the published message.
--
-- Length Constraint: Maximum 100 characters
prsMessageId :: Lens' PublishResponse (Maybe Text)
prsMessageId = lens _prsMessageId (\ s a -> s{_prsMessageId = a});

-- | The response status code.
prsResponseStatus :: Lens' PublishResponse Int
prsResponseStatus = lens _prsResponseStatus (\ s a -> s{_prsResponseStatus = a});
