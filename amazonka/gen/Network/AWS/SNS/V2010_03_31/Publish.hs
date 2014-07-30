{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.V2010_03_31.Publish
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
-- request and response for publishing to a mobile endpoint. The following
-- example publishes the same message to all protocols:
-- http://sns.us-east-1.amazonaws.com/ ?Subject=My%20first%20message
-- &amp;TopicArn=arn%3Aaws%3Asns%3Aus-east-1%3A698519295917%3AMy-Topic
-- &amp;Message=Hello%20world%21 &amp;Action=Publish &amp;SignatureVersion=2
-- &amp;SignatureMethod=HmacSHA256 &amp;Timestamp=2010-03-31T12%3A00%3A00.000Z
-- &amp;AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &amp;Signature=9GZysQ4Jpnz%2BHklqM7VFTvEcjR2LIUtn6jW47054xxE%3D Use the
-- following JSON object format for the Message parameter to send different
-- messages to each protocol (linebreaks added for readability): { "default" :
-- "some message", "email" : "some email message", "email-json" : "some
-- email-json message", "http" : "some http message", "https" : "some https
-- message", "sqs" : "some sqs message" } &lt;PublishResponse
-- xmlns="http://sns.amazonaws.com/doc/2010-03-31/"&gt; &lt;PublishResult&gt;
-- &lt;MessageId&gt;94f20ce6-13c5-43a0-9a9e-ca52d816e90b&lt;/MessageId&gt;
-- &lt;/PublishResult&gt; &lt;ResponseMetadata&gt;
-- &lt;RequestId&gt;f187a3c1-376f-11df-8963-01868b7c937a&lt;/RequestId&gt;
-- &lt;/ResponseMetadata&gt; &lt;/PublishResponse&gt; POST
-- http://sns.us-west-2.amazonaws.com/ HTTP/1.1 ... Action=Publish
-- &amp;Message=%7B%22default%22%3A%22This+is+the+default+Message%22%2C%22APNS_SANDBOX%22%3A%22%7B+%5C%22aps%5C%22+%3A+%7B+%5C%22alert%5C%22+%3A+%5C%22You+have+got+email.%5C%22%2C+%5C%22badge%5C%22+%3A+9%2C%5C%22sound%5C%22+%3A%5C%22default%5C%22%7D%7D%22%7D
-- 
-- &amp;TargetArn=arn%3Aaws%3Asns%3Aus-west-2%3A803981987763%3Aendpoint%2FAPNS_SANDBOX%2Fpushapp%2F98e9ced9-f136-3893-9d60-776547eafebb
-- &amp;SignatureMethod=HmacSHA256 &amp;AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &amp;SignatureVersion=2 &amp;Version=2010-03-31
-- &amp;Signature=vmqc4XRupKAxsDAdN4j4Ayw5LQljXMps3kss4bkDfCk%3D
-- &amp;Timestamp=2013-07-18T22%3A44%3A09.452Z &amp;MessageStructure=json
-- HTTP/1.1 200 OK ... &lt;PublishResponse
-- xmlns="http://sns.amazonaws.com/doc/2010-03-31/"&gt; &lt;PublishResult&gt;
-- &lt;MessageId&gt;567910cd-659e-55d4-8ccb-5aaf14679dc0&lt;/MessageId&gt;
-- &lt;/PublishResult&gt; &lt;ResponseMetadata&gt;
-- &lt;RequestId&gt;d74b8436-ae13-5ab4-a9ff-ce54dfea72a0&lt;/RequestId&gt;
-- &lt;/ResponseMetadata&gt; &lt;/PublishResponse&gt;.
module Network.AWS.SNS.V2010_03_31.Publish where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error, Endpoint, Region)
import           Network.AWS.Request.Query
import           Network.AWS.SNS.V2010_03_31.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

-- | Minimum specification for a 'Publish' request.
publish :: Text -- ^ '_piMessage'
        -> Publish
publish p1 = Publish
    { _piMessage = p1
    , _piMessageAttributes = mempty
    , _piTargetArn = Nothing
    , _piMessageStructure = Nothing
    , _piSubject = Nothing
    , _piTopicArn = Nothing
    }

data Publish = Publish
    { _piMessage :: Text
      -- ^ The message you want to send to the topic. If you want to send
      -- the same message to all transport protocols, include the text of
      -- the message as a String value. If you want to send different
      -- messages for each transport protocol, set the value of the
      -- MessageStructure parameter to json and use a JSON object for the
      -- Message parameter. See the Examples section for the format of the
      -- JSON object. Constraints: Messages must be UTF-8 encoded strings
      -- at most 256 KB in size (262144 bytes, not 262144 characters).
      -- JSON-specific constraints: Keys in the JSON object that
      -- correspond to supported transport protocols must have simple JSON
      -- string values. The values will be parsed (unescaped) before they
      -- are used in outgoing messages. Outbound notifications are JSON
      -- encoded (meaning that the characters will be reescaped for
      -- sending). Values have a minimum length of 0 (the empty string,
      -- "", is allowed). Values have a maximum length bounded by the
      -- overall message size (so, including multiple protocols may limit
      -- message sizes). Non-string values will cause the key to be
      -- ignored. Keys that do not correspond to supported transport
      -- protocols are ignored. Duplicate keys are not allowed. Failure to
      -- parse or validate any key or value in the message will cause the
      -- Publish call to return an error (no partial delivery).
    , _piMessageAttributes :: HashMap Text MessageAttributeValue
      -- ^ Message attributes for Publish action.
    , _piTargetArn :: Maybe Text
      -- ^ Either TopicArn or EndpointArn, but not both.
    , _piMessageStructure :: Maybe Text
      -- ^ Set MessageStructure to json if you want to send a different
      -- message for each protocol. For example, using one publish action,
      -- you can send a short message to your SMS subscribers and a longer
      -- message to your email subscribers. If you set MessageStructure to
      -- json, the value of the Message parameter must: be a syntactically
      -- valid JSON object; and contain at least a top-level JSON key of
      -- "default" with a value that is a string. You can define other
      -- top-level keys that define the message you want to send to a
      -- specific transport protocol (e.g., "http"). For information about
      -- sending different messages for each protocol using the AWS
      -- Management Console, go to Create Different Messages for Each
      -- Protocol in the Amazon Simple Notification Service Getting
      -- Started Guide. Valid value: json.
    , _piSubject :: Maybe Text
      -- ^ Optional parameter to be used as the "Subject" line when the
      -- message is delivered to email endpoints. This field will also be
      -- included, if present, in the standard JSON messages delivered to
      -- other endpoints. Constraints: Subjects must be ASCII text that
      -- begins with a letter, number, or punctuation mark; must not
      -- include line breaks or control characters; and must be less than
      -- 100 characters long.
    , _piTopicArn :: Maybe Text
      -- ^ The topic you want to publish to.
    } deriving (Generic)

instance ToQuery Publish where
    toQuery = genericToQuery def

instance AWSRequest Publish where
    type Sv Publish = SNS
    type Rs Publish = PublishResponse

    request = post "Publish"
    response _ = xmlResponse

data PublishResponse = PublishResponse
    { _prMessageId :: Maybe Text
      -- ^ Unique identifier assigned to the published message. Length
      -- Constraint: Maximum 100 characters.
    } deriving (Generic)

instance FromXML PublishResponse where
    fromXMLOptions = xmlOptions
