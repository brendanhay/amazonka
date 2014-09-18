{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SQS.ReceiveMessage
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves one or more messages, with a maximum limit of 10 messages, from
-- the specified queue. Long poll support is enabled by using the
-- WaitTimeSeconds parameter. For more information, see Amazon SQS Long Poll
-- in the Amazon SQS Developer Guide. Short poll is the default behavior where
-- a weighted random set of machines is sampled on a ReceiveMessage call. This
-- means only the messages on the sampled machines are returned. If the number
-- of messages in the queue is small (less than 1000), it is likely you will
-- get fewer messages than you requested per ReceiveMessage call. If the
-- number of messages in the queue is extremely small, you might not receive
-- any messages in a particular ReceiveMessage response; in which case you
-- should repeat the request. For each message returned, the response includes
-- the following: Message body MD5 digest of the message body. For information
-- about MD5, go to http://www.faqs.org/rfcs/rfc1321.html. Message ID you
-- received when you sent the message to the queue. Receipt handle. Message
-- attributes. MD5 digest of the message attributes. The receipt handle is the
-- identifier you must provide when deleting the message. For more
-- information, see Queue and Message Identifiers in the Amazon SQS Developer
-- Guide. You can provide the VisibilityTimeout parameter in your request,
-- which will be applied to the messages that Amazon SQS returns in the
-- response. If you do not include the parameter, the overall visibility
-- timeout for the queue is used for the returned messages. For more
-- information, see Visibility Timeout in the Amazon SQS Developer Guide.
-- Going forward, new attributes might be added. If you are writing code that
-- calls this action, we recommend that you structure your code so that it can
-- handle new attributes gracefully. The following example Query request
-- receives messages from the specified queue.
-- http://sqs.us-east-1.amazonaws.com/123456789012/testQueue/
-- ?Action=ReceiveMessage &MaxNumberOfMessages=5 &VisibilityTimeout=15
-- &AttributeName=All; &Version=2009-02-01 &SignatureMethod=HmacSHA256
-- &Expires=2009-04-18T22%3A52%3A43PST &AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &SignatureVersion=2 &Signature=Dqlp3Sd6ljTUA9Uf6SGtEExwUQEXAMPLE
-- 5fea7756-0ea4-451a-a703-a558b933e274
-- MbZj6wDWli+JvwwJaBV+3dcjk2YW2vA3+STFFljTM8tJJg6HRG6PYSasuWXPJB+Cw
-- Lj1FjgXUv1uSj1gUPAWV66FU/WeR4mq2OKpEGYWbnLmpRCJVAyeMjeU5ZBdtcQ+QE
-- auMZc8ZRv37sIW2iJKq3M9MFx1YvV11A2x/KSbkJ0= fafb00f5732ab283681e124bf8747ed1
-- This is a test message SenderId 195004372649 SentTimestamp 1238099229000
-- ApproximateReceiveCount 5 ApproximateFirstReceiveTimestamp 1250700979248
-- b6633655-283d-45b4-aee4-4e84e0ae6afa.
module Network.AWS.SQS.ReceiveMessage
    (
    -- * Request
      ReceiveMessage
    -- ** Request constructor
    , receiveMessage
    -- ** Request lenses
    , rmQueueUrl
    , rmAttributeNames
    , rmMessageAttributeNames
    , rmMaxNumberOfMessages
    , rmVisibilityTimeout
    , rmWaitTimeSeconds

    -- * Response
    , ReceiveMessageResponse
    -- ** Response constructor
    , receiveMessageResponse
    -- ** Response lenses
    , rmrMessages
    ) where

import Network.AWS.Request.Query
import Network.AWS.SQS.Types
import Network.AWS.Prelude

data ReceiveMessage = ReceiveMessage
    { _rmQueueUrl :: Text
    , _rmAttributeNames :: [QueueAttributeName]
    , _rmMessageAttributeNames :: [Text]
    , _rmMaxNumberOfMessages :: Maybe Integer
    , _rmVisibilityTimeout :: Maybe Integer
    , _rmWaitTimeSeconds :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ReceiveMessage' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @QueueUrl ::@ @Text@
--
-- * @AttributeNames ::@ @[QueueAttributeName]@
--
-- * @MessageAttributeNames ::@ @[Text]@
--
-- * @MaxNumberOfMessages ::@ @Maybe Integer@
--
-- * @VisibilityTimeout ::@ @Maybe Integer@
--
-- * @WaitTimeSeconds ::@ @Maybe Integer@
--
receiveMessage :: Text -- ^ 'rmQueueUrl'
                 -> ReceiveMessage
receiveMessage p1 = ReceiveMessage
    { _rmQueueUrl = p1
    , _rmAttributeNames = mempty
    , _rmMessageAttributeNames = mempty
    , _rmMaxNumberOfMessages = Nothing
    , _rmVisibilityTimeout = Nothing
    , _rmWaitTimeSeconds = Nothing
    }

-- | The URL of the Amazon SQS queue to take action on.
rmQueueUrl :: Lens' ReceiveMessage Text
rmQueueUrl = lens _rmQueueUrl (\s a -> s { _rmQueueUrl = a })

-- | A list of attributes that need to be returned along with each message. The
-- following lists the names and descriptions of the attributes that can be
-- returned: All - returns all values. ApproximateFirstReceiveTimestamp -
-- returns the time when the message was first received (epoch time in
-- milliseconds). ApproximateReceiveCount - returns the number of times a
-- message has been received but not deleted. SenderId - returns the AWS
-- account number (or the IP address, if anonymous access is allowed) of the
-- sender. SentTimestamp - returns the time when the message was sent (epoch
-- time in milliseconds).
rmAttributeNames :: Lens' ReceiveMessage [QueueAttributeName]
rmAttributeNames =
    lens _rmAttributeNames (\s a -> s { _rmAttributeNames = a })

-- | The message attribute Name can contain the following characters: A-Z, a-z,
-- 0-9, underscore(_), hyphen(-), and period (.). The message attribute name
-- must not start or end with a period, and it should not have successive
-- periods. The message attribute name is case sensitive and must be unique
-- among all attribute names for the message. The message attribute name can
-- be up to 256 characters long. Attribute names cannot start with "AWS." or
-- "Amazon." because these prefixes are reserved for use by Amazon Web
-- Services.
rmMessageAttributeNames :: Lens' ReceiveMessage [Text]
rmMessageAttributeNames =
    lens _rmMessageAttributeNames
         (\s a -> s { _rmMessageAttributeNames = a })

-- | The maximum number of messages to return. Amazon SQS never returns more
-- messages than this value but may return fewer. Values can be from 1 to 10.
-- Default is 1. All of the messages are not necessarily returned.
rmMaxNumberOfMessages :: Lens' ReceiveMessage (Maybe Integer)
rmMaxNumberOfMessages =
    lens _rmMaxNumberOfMessages (\s a -> s { _rmMaxNumberOfMessages = a })

-- | The duration (in seconds) that the received messages are hidden from
-- subsequent retrieve requests after being retrieved by a ReceiveMessage
-- request.
rmVisibilityTimeout :: Lens' ReceiveMessage (Maybe Integer)
rmVisibilityTimeout =
    lens _rmVisibilityTimeout (\s a -> s { _rmVisibilityTimeout = a })

-- | The duration (in seconds) for which the call will wait for a message to
-- arrive in the queue before returning. If a message is available, the call
-- will return sooner than WaitTimeSeconds.
rmWaitTimeSeconds :: Lens' ReceiveMessage (Maybe Integer)
rmWaitTimeSeconds =
    lens _rmWaitTimeSeconds (\s a -> s { _rmWaitTimeSeconds = a })

instance ToQuery ReceiveMessage where
    toQuery = genericQuery def

-- | A list of received messages.
newtype ReceiveMessageResponse = ReceiveMessageResponse
    { _rmrMessages :: [Message]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ReceiveMessageResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Messages ::@ @[Message]@
--
receiveMessageResponse :: ReceiveMessageResponse
receiveMessageResponse = ReceiveMessageResponse
    { _rmrMessages = mempty
    }

-- | A list of messages.
rmrMessages :: Lens' ReceiveMessageResponse [Message]
rmrMessages = lens _rmrMessages (\s a -> s { _rmrMessages = a })

instance FromXML ReceiveMessageResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ReceiveMessage where
    type Sv ReceiveMessage = SQS
    type Rs ReceiveMessage = ReceiveMessageResponse

    request = post "ReceiveMessage"
    response _ = xmlResponse
