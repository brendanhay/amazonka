{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.Publish
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Sends a message to all of a topic's subscribed endpoints. When a messageId
-- is returned, the message has been saved and Amazon SNS will attempt to
-- deliver it to the topic's subscribers shortly. The format of the outgoing
-- message to each subscribed endpoint depends on the notification protocol
-- selected. To use the Publish action for sending a message to a mobile
-- endpoint, such as an app on a Kindle device or mobile phone, you must
-- specify the EndpointArn. The EndpointArn is returned when making a call
-- with the CreatePlatformEndpoint action. The second example below shows a
-- request and response for publishing to a mobile endpoint.
module Network.AWS.SNS.Publish
    (
    -- * Request
      Publish
    -- ** Request constructor
    , publish
    -- ** Request lenses
    , pMessage
    , pMessageAttributes
    , pMessageStructure
    , pSubject
    , pTargetArn
    , pTopicArn

    -- * Response
    , PublishResponse
    -- ** Response constructor
    , publishResponse
    -- ** Response lenses
    , prMessageId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SNS.Types
import qualified GHC.Exts

data Publish = Publish
    { _pMessage           :: Text
    , _pMessageAttributes :: Map Text MessageAttributeValue
    , _pMessageStructure  :: Maybe Text
    , _pSubject           :: Maybe Text
    , _pTargetArn         :: Maybe Text
    , _pTopicArn          :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'Publish' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pMessage' @::@ 'Text'
--
-- * 'pMessageAttributes' @::@ 'HashMap' 'Text' 'MessageAttributeValue'
--
-- * 'pMessageStructure' @::@ 'Maybe' 'Text'
--
-- * 'pSubject' @::@ 'Maybe' 'Text'
--
-- * 'pTargetArn' @::@ 'Maybe' 'Text'
--
-- * 'pTopicArn' @::@ 'Maybe' 'Text'
--
publish :: Text -- ^ 'pMessage'
        -> Publish
publish p1 = Publish
    { _pMessage           = p1
    , _pTopicArn          = Nothing
    , _pTargetArn         = Nothing
    , _pSubject           = Nothing
    , _pMessageStructure  = Nothing
    , _pMessageAttributes = mempty
    }

-- | The message you want to send to the topic. If you want to send the same
-- message to all transport protocols, include the text of the message as a
-- String value. If you want to send different messages for each transport
-- protocol, set the value of the MessageStructure parameter to json and use
-- a JSON object for the Message parameter. See the Examples section for the
-- format of the JSON object. Constraints: Messages must be UTF-8 encoded
-- strings at most 256 KB in size (262144 bytes, not 262144 characters).
-- JSON-specific constraints: Keys in the JSON object that correspond to
-- supported transport protocols must have simple JSON string values. The
-- values will be parsed (unescaped) before they are used in outgoing
-- messages. Outbound notifications are JSON encoded (meaning that the
-- characters will be reescaped for sending). Values have a minimum length
-- of 0 (the empty string, "", is allowed). Values have a maximum length
-- bounded by the overall message size (so, including multiple protocols may
-- limit message sizes). Non-string values will cause the key to be ignored.
-- Keys that do not correspond to supported transport protocols are ignored.
-- Duplicate keys are not allowed. Failure to parse or validate any key or
-- value in the message will cause the Publish call to return an error (no
-- partial delivery).
pMessage :: Lens' Publish Text
pMessage = lens _pMessage (\s a -> s { _pMessage = a })

-- | Message attributes for Publish action.
pMessageAttributes :: Lens' Publish (HashMap Text MessageAttributeValue)
pMessageAttributes =
    lens _pMessageAttributes (\s a -> s { _pMessageAttributes = a })
        . _Map

-- | Set MessageStructure to json if you want to send a different message for
-- each protocol. For example, using one publish action, you can send a
-- short message to your SMS subscribers and a longer message to your email
-- subscribers. If you set MessageStructure to json, the value of the
-- Message parameter must: be a syntactically valid JSON object; and contain
-- at least a top-level JSON key of "default" with a value that is a string.
-- You can define other top-level keys that define the message you want to
-- send to a specific transport protocol (e.g., "http"). For information
-- about sending different messages for each protocol using the AWS
-- Management Console, go to Create Different Messages for Each Protocol in
-- the Amazon Simple Notification Service Getting Started Guide. Valid
-- value: json.
pMessageStructure :: Lens' Publish (Maybe Text)
pMessageStructure =
    lens _pMessageStructure (\s a -> s { _pMessageStructure = a })

-- | Optional parameter to be used as the "Subject" line when the message is
-- delivered to email endpoints. This field will also be included, if
-- present, in the standard JSON messages delivered to other endpoints.
-- Constraints: Subjects must be ASCII text that begins with a letter,
-- number, or punctuation mark; must not include line breaks or control
-- characters; and must be less than 100 characters long.
pSubject :: Lens' Publish (Maybe Text)
pSubject = lens _pSubject (\s a -> s { _pSubject = a })

-- | Either TopicArn or EndpointArn, but not both.
pTargetArn :: Lens' Publish (Maybe Text)
pTargetArn = lens _pTargetArn (\s a -> s { _pTargetArn = a })

-- | The topic you want to publish to.
pTopicArn :: Lens' Publish (Maybe Text)
pTopicArn = lens _pTopicArn (\s a -> s { _pTopicArn = a })

newtype PublishResponse = PublishResponse
    { _prMessageId :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'PublishResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'prMessageId' @::@ 'Maybe' 'Text'
--
publishResponse :: PublishResponse
publishResponse = PublishResponse
    { _prMessageId = Nothing
    }

-- | Unique identifier assigned to the published message. Length Constraint:
-- Maximum 100 characters.
prMessageId :: Lens' PublishResponse (Maybe Text)
prMessageId = lens _prMessageId (\s a -> s { _prMessageId = a })

instance AWSRequest Publish where
    type Sv Publish = SNS
    type Rs Publish = PublishResponse

    request  = post "Publish"
    response = xmlResponse

instance FromXML PublishResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PublishResponse"

instance ToPath Publish where
    toPath = const "/"

instance ToHeaders Publish

instance ToQuery Publish
