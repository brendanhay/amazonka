{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SQS.SendMessage
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Delivers a message to the specified queue. With Amazon SQS, you now have
-- the ability to send large payload messages that are up to 256KB (262,144
-- bytes) in size. To send large payloads, you must use an AWS SDK that
-- supports SigV4 signing. To verify whether SigV4 is supported for an AWS
-- SDK, check the SDK release notes. The following list shows the characters
-- (in Unicode) allowed in your message, according to the W3C XML
-- specification. For more information, go to
-- http://www.w3.org/TR/REC-xml/#charsets If you send any characters not
-- included in the list, your request will be rejected. #x9 | #xA | #xD |
-- [#x20 to #xD7FF] | [#xE000 to #xFFFD] | [#x10000 to #x10FFFF].
--
-- <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_SendMessage.html>
module Network.AWS.SQS.SendMessage
    (
    -- * Request
      SendMessage
    -- ** Request constructor
    , sendMessage
    -- ** Request lenses
    , smDelaySeconds
    , smMessageAttributes
    , smMessageBody
    , smQueueUrl

    -- * Response
    , SendMessageResponse
    -- ** Response constructor
    , sendMessageResponse
    -- ** Response lenses
    , smrMD5OfMessageAttributes
    , smrMD5OfMessageBody
    , smrMessageId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SQS.Types
import qualified GHC.Exts

data SendMessage = SendMessage
    { _smDelaySeconds      :: Maybe Int
    , _smMessageAttributes :: Flatten (Map Text MessageAttributeValue)
    , _smMessageBody       :: Text
    , _smQueueUrl          :: Text
    } deriving (Eq, Show, Generic)

-- | 'SendMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'smDelaySeconds' @::@ 'Maybe' 'Int'
--
-- * 'smMessageAttributes' @::@ ('HashMap' 'Text' 'MessageAttributeValue')
--
-- * 'smMessageBody' @::@ 'Text'
--
-- * 'smQueueUrl' @::@ 'Text'
--
sendMessage :: Text -- ^ 'smQueueUrl'
            -> Text -- ^ 'smMessageBody'
            -> (HashMap Text MessageAttributeValue) -- ^ 'smMessageAttributes'
            -> SendMessage
sendMessage p1 p2 p3 = SendMessage
    { _smQueueUrl          = p1
    , _smMessageBody       = p2
    , _smMessageAttributes = withIso _Flatten (const id) p3
    , _smDelaySeconds      = Nothing
    }

-- | The number of seconds (0 to 900 - 15 minutes) to delay a specific
-- message. Messages with a positive DelaySeconds value become available for
-- processing after the delay time is finished. If you don't specify a
-- value, the default value for the queue applies.
smDelaySeconds :: Lens' SendMessage (Maybe Int)
smDelaySeconds = lens _smDelaySeconds (\s a -> s { _smDelaySeconds = a })

-- | Each message attribute consists of a Name, Type, and Value. For more
-- information, see Message Attribute Items.
smMessageAttributes :: Lens' SendMessage ((HashMap Text MessageAttributeValue))
smMessageAttributes =
    lens _smMessageAttributes (\s a -> s { _smMessageAttributes = a })
        . _Flatten . _Map

-- | The message to send. String maximum 256 KB in size. For a list of allowed
-- characters, see the preceding important note.
smMessageBody :: Lens' SendMessage Text
smMessageBody = lens _smMessageBody (\s a -> s { _smMessageBody = a })

-- | The URL of the Amazon SQS queue to take action on.
smQueueUrl :: Lens' SendMessage Text
smQueueUrl = lens _smQueueUrl (\s a -> s { _smQueueUrl = a })

data SendMessageResponse = SendMessageResponse
    { _smrMD5OfMessageAttributes :: Maybe Text
    , _smrMD5OfMessageBody       :: Maybe Text
    , _smrMessageId              :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'SendMessageResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'smrMD5OfMessageAttributes' @::@ 'Maybe' 'Text'
--
-- * 'smrMD5OfMessageBody' @::@ 'Maybe' 'Text'
--
-- * 'smrMessageId' @::@ 'Maybe' 'Text'
--
sendMessageResponse :: SendMessageResponse
sendMessageResponse = SendMessageResponse
    { _smrMD5OfMessageBody       = Nothing
    , _smrMD5OfMessageAttributes = Nothing
    , _smrMessageId              = Nothing
    }

-- | An MD5 digest of the non-URL-encoded message attribute string. This can
-- be used to verify that Amazon SQS received the message correctly. Amazon
-- SQS first URL decodes the message before creating the MD5 digest. For
-- information about MD5, go to http://www.faqs.org/rfcs/rfc1321.html.
smrMD5OfMessageAttributes :: Lens' SendMessageResponse (Maybe Text)
smrMD5OfMessageAttributes =
    lens _smrMD5OfMessageAttributes
        (\s a -> s { _smrMD5OfMessageAttributes = a })

-- | An MD5 digest of the non-URL-encoded message body string. This can be
-- used to verify that Amazon SQS received the message correctly. Amazon SQS
-- first URL decodes the message before creating the MD5 digest. For
-- information about MD5, go to http://www.faqs.org/rfcs/rfc1321.html.
smrMD5OfMessageBody :: Lens' SendMessageResponse (Maybe Text)
smrMD5OfMessageBody =
    lens _smrMD5OfMessageBody (\s a -> s { _smrMD5OfMessageBody = a })

-- | An element containing the message ID of the message sent to the queue.
-- For more information, see Queue and Message Identifiers in the Amazon SQS
-- Developer Guide.
smrMessageId :: Lens' SendMessageResponse (Maybe Text)
smrMessageId = lens _smrMessageId (\s a -> s { _smrMessageId = a })

instance ToPath SendMessage where
    toPath = const "/"

instance ToQuery SendMessage

instance ToHeaders SendMessage

instance AWSRequest SendMessage where
    type Sv SendMessage = SQS
    type Rs SendMessage = SendMessageResponse

    request  = post "SendMessage"
    response = xmlResponse

instance FromXML SendMessageResponse where
    parseXML = withElement "SendMessageResult" $ \x ->
            <$> x .@? "MD5OfMessageAttributes"
            <*> x .@? "MD5OfMessageBody"
            <*> x .@? "MessageId"
