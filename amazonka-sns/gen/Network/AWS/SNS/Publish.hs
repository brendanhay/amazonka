{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
      PublishInput
    -- ** Request constructor
    , publishInput
    -- ** Request lenses
    , piMessage
    , piMessageAttributes
    , piMessageStructure
    , piSubject
    , piTargetArn
    , piTopicArn

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

data PublishInput = PublishInput
    { _piMessage           :: Text
    , _piMessageAttributes :: Map Text MessageAttributeValue
    , _piMessageStructure  :: Maybe Text
    , _piSubject           :: Maybe Text
    , _piTargetArn         :: Maybe Text
    , _piTopicArn          :: Maybe Text
    } (Eq, Show, Generic)

-- | 'PublishInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'piMessage' @::@ 'Text'
--
-- * 'piMessageAttributes' @::@ 'HashMap' 'Text' 'MessageAttributeValue'
--
-- * 'piMessageStructure' @::@ 'Maybe' 'Text'
--
-- * 'piSubject' @::@ 'Maybe' 'Text'
--
-- * 'piTargetArn' @::@ 'Maybe' 'Text'
--
-- * 'piTopicArn' @::@ 'Maybe' 'Text'
--
publishInput :: Text -- ^ 'piMessage'
             -> PublishInput
publishInput p1 = PublishInput
    { _piMessage           = p1
    , _piTopicArn          = Nothing
    , _piTargetArn         = Nothing
    , _piSubject           = Nothing
    , _piMessageStructure  = Nothing
    , _piMessageAttributes = mempty
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
piMessage :: Lens' PublishInput Text
piMessage = lens _piMessage (\s a -> s { _piMessage = a })

-- | Message attributes for Publish action.
piMessageAttributes :: Lens' PublishInput (HashMap Text MessageAttributeValue)
piMessageAttributes =
    lens _piMessageAttributes (\s a -> s { _piMessageAttributes = a })
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
piMessageStructure :: Lens' PublishInput (Maybe Text)
piMessageStructure =
    lens _piMessageStructure (\s a -> s { _piMessageStructure = a })

-- | Optional parameter to be used as the "Subject" line when the message is
-- delivered to email endpoints. This field will also be included, if
-- present, in the standard JSON messages delivered to other endpoints.
-- Constraints: Subjects must be ASCII text that begins with a letter,
-- number, or punctuation mark; must not include line breaks or control
-- characters; and must be less than 100 characters long.
piSubject :: Lens' PublishInput (Maybe Text)
piSubject = lens _piSubject (\s a -> s { _piSubject = a })

-- | Either TopicArn or EndpointArn, but not both.
piTargetArn :: Lens' PublishInput (Maybe Text)
piTargetArn = lens _piTargetArn (\s a -> s { _piTargetArn = a })

-- | The topic you want to publish to.
piTopicArn :: Lens' PublishInput (Maybe Text)
piTopicArn = lens _piTopicArn (\s a -> s { _piTopicArn = a })
instance ToQuery PublishInput

instance ToPath PublishInput where
    toPath = const "/"

newtype PublishResponse = PublishResponse
    { _prMessageId :: Maybe Text
    } (Eq, Ord, Show, Generic, Monoid)

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

instance FromXML PublishResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PublishResponse"

instance AWSRequest PublishInput where
    type Sv PublishInput = SNS
    type Rs PublishInput = PublishResponse

    request  = post "Publish"
    response = xmlResponse $ \h x -> PublishResponse
        <$> x %| "MessageId"
