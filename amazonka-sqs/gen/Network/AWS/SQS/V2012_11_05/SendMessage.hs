{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SQS.V2012_11_05.SendMessage
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
-- [#x20 to #xD7FF] | [#xE000 to #xFFFD] | [#x10000 to #x10FFFF] The following
-- example SendMessage request sends a message containing "This is a test
-- message" to the queue. You must URL encode the entire URL; however, we've
-- URL encoded only the message body to make the example easier for you to
-- read. The following example response includes the MD5 digest for "This is a
-- test message". http://sqs.us-east-1.amazonaws.com/123456789012/testQueue/
-- ?Action=SendMessage &MessageBody=This+is+a+test+message
-- &MessageAttribute.1.Name=test_attribute_name_1
-- &MessageAttribute.1.Value.StringValue=test_attribute_value_1
-- &MessageAttribute.1.Value.DataType=String
-- &MessageAttribute.2.Name=test_attribute_name_2
-- &MessageAttribute.2.Value.StringValue=test_attribute_value_2
-- &MessageAttribute.2.Value.DataType=String &Version=2012-11-05
-- &SignatureMethod=HmacSHA256 &Expires=2014-05-05T22%3A52%3A43PST
-- &AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &SignatureVersion=2
-- &Signature=Dqlp3Sd6ljTUA9Uf6SGtEExwUQEXAMPLE
-- fafb00f5732ab283681e124bf8747ed1 3ae8f24a165a8cedc005670c81a27295
-- 5fea7756-0ea4-451a-a703-a558b933e274 27daac76-34dd-47df-bd01-1f6e873584a0.
module Network.AWS.SQS.V2012_11_05.SendMessage
    (
    -- * Request
      SendMessage
    -- ** Request constructor
    , sendMessage
    -- ** Request lenses
    , smrQueueUrl
    , smrMessageBody
    , smrDelaySeconds
    , smrMessageAttributes

    -- * Response
    , SendMessageResponse
    -- ** Response lenses
    , smsMD5OfMessageBody
    , smsMD5OfMessageAttributes
    , smsMessageId
    ) where

import Network.AWS.Request.Query
import Network.AWS.SQS.V2012_11_05.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'SendMessage' request.
sendMessage :: Text -- ^ 'smrQueueUrl'
            -> Text -- ^ 'smrMessageBody'
            -> SendMessage
sendMessage p1 p2 = SendMessage
    { _smrQueueUrl = p1
    , _smrMessageBody = p2
    , _smrDelaySeconds = Nothing
    , _smrMessageAttributes = mempty
    }

data SendMessage = SendMessage
    { _smrQueueUrl :: Text
      -- ^ The URL of the Amazon SQS queue to take action on.
    , _smrMessageBody :: Text
      -- ^ The message to send. String maximum 256 KB in size. For a list of
      -- allowed characters, see the preceding important note.
    , _smrDelaySeconds :: Maybe Integer
      -- ^ The number of seconds (0 to 900 - 15 minutes) to delay a specific
      -- message. Messages with a positive DelaySeconds value become
      -- available for processing after the delay time is finished. If you
      -- don't specify a value, the default value for the queue applies.
    , _smrMessageAttributes :: Map Text MessageAttributeValue
      -- ^ Each message attribute consists of a Name, Type, and Value. For
      -- more information, see Message Attribute Items.
    } deriving (Show, Generic)

-- | The URL of the Amazon SQS queue to take action on.
smrQueueUrl
    :: Functor f
    => (Text
    -> f (Text))
    -> SendMessage
    -> f SendMessage
smrQueueUrl f x =
    (\y -> x { _smrQueueUrl = y })
       <$> f (_smrQueueUrl x)
{-# INLINE smrQueueUrl #-}

-- | The message to send. String maximum 256 KB in size. For a list of allowed
-- characters, see the preceding important note.
smrMessageBody
    :: Functor f
    => (Text
    -> f (Text))
    -> SendMessage
    -> f SendMessage
smrMessageBody f x =
    (\y -> x { _smrMessageBody = y })
       <$> f (_smrMessageBody x)
{-# INLINE smrMessageBody #-}

-- | The number of seconds (0 to 900 - 15 minutes) to delay a specific message.
-- Messages with a positive DelaySeconds value become available for processing
-- after the delay time is finished. If you don't specify a value, the default
-- value for the queue applies.
smrDelaySeconds
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> SendMessage
    -> f SendMessage
smrDelaySeconds f x =
    (\y -> x { _smrDelaySeconds = y })
       <$> f (_smrDelaySeconds x)
{-# INLINE smrDelaySeconds #-}

-- | Each message attribute consists of a Name, Type, and Value. For more
-- information, see Message Attribute Items.
smrMessageAttributes
    :: Functor f
    => (Map Text MessageAttributeValue
    -> f (Map Text MessageAttributeValue))
    -> SendMessage
    -> f SendMessage
smrMessageAttributes f x =
    (\y -> x { _smrMessageAttributes = y })
       <$> f (_smrMessageAttributes x)
{-# INLINE smrMessageAttributes #-}

instance ToQuery SendMessage where
    toQuery = genericQuery def

data SendMessageResponse = SendMessageResponse
    { _smsMD5OfMessageBody :: Maybe Text
      -- ^ An MD5 digest of the non-URL-encoded message body string. This
      -- can be used to verify that Amazon SQS received the message
      -- correctly. Amazon SQS first URL decodes the message before
      -- creating the MD5 digest. For information about MD5, go to
      -- http://www.faqs.org/rfcs/rfc1321.html.
    , _smsMD5OfMessageAttributes :: Maybe Text
      -- ^ An MD5 digest of the non-URL-encoded message attribute string.
      -- This can be used to verify that Amazon SQS received the message
      -- correctly. Amazon SQS first URL decodes the message before
      -- creating the MD5 digest. For information about MD5, go to
      -- http://www.faqs.org/rfcs/rfc1321.html.
    , _smsMessageId :: Maybe Text
      -- ^ An element containing the message ID of the message sent to the
      -- queue. For more information, see Queue and Message Identifiers in
      -- the Amazon SQS Developer Guide.
    } deriving (Show, Generic)

-- | An MD5 digest of the non-URL-encoded message body string. This can be used
-- to verify that Amazon SQS received the message correctly. Amazon SQS first
-- URL decodes the message before creating the MD5 digest. For information
-- about MD5, go to http://www.faqs.org/rfcs/rfc1321.html.
smsMD5OfMessageBody
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> SendMessageResponse
    -> f SendMessageResponse
smsMD5OfMessageBody f x =
    (\y -> x { _smsMD5OfMessageBody = y })
       <$> f (_smsMD5OfMessageBody x)
{-# INLINE smsMD5OfMessageBody #-}

-- | An MD5 digest of the non-URL-encoded message attribute string. This can be
-- used to verify that Amazon SQS received the message correctly. Amazon SQS
-- first URL decodes the message before creating the MD5 digest. For
-- information about MD5, go to http://www.faqs.org/rfcs/rfc1321.html.
smsMD5OfMessageAttributes
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> SendMessageResponse
    -> f SendMessageResponse
smsMD5OfMessageAttributes f x =
    (\y -> x { _smsMD5OfMessageAttributes = y })
       <$> f (_smsMD5OfMessageAttributes x)
{-# INLINE smsMD5OfMessageAttributes #-}

-- | An element containing the message ID of the message sent to the queue. For
-- more information, see Queue and Message Identifiers in the Amazon SQS
-- Developer Guide.
smsMessageId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> SendMessageResponse
    -> f SendMessageResponse
smsMessageId f x =
    (\y -> x { _smsMessageId = y })
       <$> f (_smsMessageId x)
{-# INLINE smsMessageId #-}

instance FromXML SendMessageResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest SendMessage where
    type Sv SendMessage = SQS
    type Rs SendMessage = SendMessageResponse

    request = post "SendMessage"
    response _ = xmlResponse
