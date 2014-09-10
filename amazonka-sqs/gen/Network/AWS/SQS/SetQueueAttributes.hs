{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SQS
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Sets the value of one or more queue attributes. When you change a queue's
-- attributes, the change can take up to 60 seconds for most of the attributes
-- to propagate throughout the SQS system. Changes made to the
-- MessageRetentionPeriod attribute can take up to 15 minutes. Going forward,
-- new attributes might be added. If you are writing code that calls this
-- action, we recommend that you structure your code so that it can handle new
-- attributes gracefully. The following example Query request sets a policy
-- that gives all users ReceiveMessage permission for the queue named
-- testQueue. For more examples of policies, see Amazon SQS Policy Examples in
-- the Amazon SQS Developer Guide.
-- http://sqs.us-east-1.amazonaws.com/123456789012/testQueue/
-- ?Action=SetQueueAttributes &Version=2009-02-01 &Attribute.Name=Policy
-- &Attribute.Value=%7B%22Version%22%3A%222008-10-17%22%2C%22Id%22
-- %3A%22%2F123456789012%2FtestQueue%2FSQSDefaultPolicy%22%2C%22Stat
-- ement%22%3A%5B%7B%22Sid%22%3A%22Queue1ReceiveMessage%22%2C%22Effe
-- ct%22%3A%22Allow%22%2C%22Principal%22%3A%7B%22AWS%22%3A%22*%22%7D
-- %2C%22Action%22%3A%22SQS%3AReceiveMessage%22%2C%22Resource%22%3A%
-- 22arn%3Aaws%3Aaws%3Asqs%3Aus%2Deast%2D1%3A123456789012%3AtestQueue%22%7D%5D%7D
-- &Timestamp=2009-05-06T16%3A57%3A31.000Z
-- &AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &Signature=%2Bd7ZlPIdnb%2BhpEna2TgfwQjfGF8%3D The following example Query
-- request sets the visibility timeout to 35 seconds for the queue named
-- testQueue. Note: There is a 120,000 limit for the number of inflight
-- messages per queue. Messages are inflight after they have been received by
-- the queue, but have not yet been deleted from the queue. If you reach the
-- 120,000 limit, you will receive an OverLimit error message from Amazon SQS.
-- To help avoid reaching the limit, you should delete the messages from the
-- queue after they have been processed. You can also increase the number of
-- queues you use to process the messages.
-- http://sqs.us-east-1.amazonaws.com/123456789012/testQueue/
-- ?Action=SetQueueAttributes &Attribute.Name=VisibilityTimeout
-- &Attribute.Value=35 &Version=2009-02-01 &SignatureMethod=HmacSHA256
-- &Expires=2009-04-18T22%3A52%3A43PST &AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &SignatureVersion=2 &Signature=Dqlp3Sd6ljTUA9Uf6SGtEExwUQEXAMPLE
-- e5cca473-4fc0-4198-a451-8abb94d02c75.
module Network.AWS.SQS
    (
    -- * Request
      SetQueueAttributes
    -- ** Request constructor
    , mkSetQueueAttributes
    -- ** Request lenses
    , sqaQueueUrl
    , sqaAttributes

    -- * Response
    , SetQueueAttributesResponse
    -- ** Response constructor
    , mkSetQueueAttributesResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.SQS.Types
import Network.AWS.Prelude

data SetQueueAttributes = SetQueueAttributes
    { _sqaQueueUrl :: !Text
    , _sqaAttributes :: Map QueueAttributeName Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'SetQueueAttributes' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @QueueUrl ::@ @Text@
--
-- * @Attributes ::@ @Map QueueAttributeName Text@
--
mkSetQueueAttributes :: Text -- ^ 'sqaQueueUrl'
                     -> Map QueueAttributeName Text -- ^ 'sqaAttributes'
                     -> SetQueueAttributes
mkSetQueueAttributes p1 p2 = SetQueueAttributes
    { _sqaQueueUrl = p1
    , _sqaAttributes = p2
    }

-- | The URL of the Amazon SQS queue to take action on.
sqaQueueUrl :: Lens' SetQueueAttributes Text
sqaQueueUrl = lens _sqaQueueUrl (\s a -> s { _sqaQueueUrl = a })

-- | A map of attributes to set. The following lists the names, descriptions,
-- and values of the special request parameters the SetQueueAttributes action
-- uses: DelaySeconds - The time in seconds that the delivery of all messages
-- in the queue will be delayed. An integer from 0 to 900 (15 minutes). The
-- default for this attribute is 0 (zero). MaximumMessageSize - The limit of
-- how many bytes a message can contain before Amazon SQS rejects it. An
-- integer from 1024 bytes (1 KiB) up to 262144 bytes (256 KiB). The default
-- for this attribute is 262144 (256 KiB). MessageRetentionPeriod - The number
-- of seconds Amazon SQS retains a message. Integer representing seconds, from
-- 60 (1 minute) to 1209600 (14 days). The default for this attribute is
-- 345600 (4 days). Policy - The queue's policy. A valid form-url-encoded
-- policy. For more information about policy structure, see Basic Policy
-- Structure in the Amazon SQS Developer Guide. For more information about
-- form-url-encoding, see
-- http://www.w3.org/MarkUp/html-spec/html-spec_8.html#SEC8.2.1.
-- ReceiveMessageWaitTimeSeconds - The time for which a ReceiveMessage call
-- will wait for a message to arrive. An integer from 0 to 20 (seconds). The
-- default for this attribute is 0. VisibilityTimeout - The visibility timeout
-- for the queue. An integer from 0 to 43200 (12 hours). The default for this
-- attribute is 30. For more information about visibility timeout, see
-- Visibility Timeout in the Amazon SQS Developer Guide. RedrivePolicy - The
-- parameters for dead letter queue functionality of the source queue. For
-- more information about RedrivePolicy and dead letter queues, see Using
-- Amazon SQS Dead Letter Queues in the Amazon SQS Developer Guide.
sqaAttributes :: Lens' SetQueueAttributes (Map QueueAttributeName Text)
sqaAttributes = lens _sqaAttributes (\s a -> s { _sqaAttributes = a })

instance ToQuery SetQueueAttributes where
    toQuery = genericQuery def

data SetQueueAttributesResponse = SetQueueAttributesResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'SetQueueAttributesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkSetQueueAttributesResponse :: SetQueueAttributesResponse
mkSetQueueAttributesResponse = SetQueueAttributesResponse

instance AWSRequest SetQueueAttributes where
    type Sv SetQueueAttributes = SQS
    type Rs SetQueueAttributes = SetQueueAttributesResponse

    request = post "SetQueueAttributes"
    response _ = nullaryResponse SetQueueAttributesResponse
