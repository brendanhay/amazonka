{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SQS.GetQueueAttributes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gets attributes for the specified queue. The following attributes are
-- supported: All - returns all values. ApproximateNumberOfMessages - returns
-- the approximate number of visible messages in a queue. For more
-- information, see Resources Required to Process Messages in the Amazon SQS
-- Developer Guide. ApproximateNumberOfMessagesNotVisible - returns the
-- approximate number of messages that are not timed-out and not deleted. For
-- more information, see Resources Required to Process Messages in the Amazon
-- SQS Developer Guide. VisibilityTimeout - returns the visibility timeout for
-- the queue. For more information about visibility timeout, see Visibility
-- Timeout in the Amazon SQS Developer Guide. CreatedTimestamp - returns the
-- time when the queue was created (epoch time in seconds).
-- LastModifiedTimestamp - returns the time when the queue was last changed
-- (epoch time in seconds). Policy - returns the queue's policy.
-- MaximumMessageSize - returns the limit of how many bytes a message can
-- contain before Amazon SQS rejects it. MessageRetentionPeriod - returns the
-- number of seconds Amazon SQS retains a message. QueueArn - returns the
-- queue's Amazon resource name (ARN). ApproximateNumberOfMessagesDelayed -
-- returns the approximate number of messages that are pending to be added to
-- the queue. DelaySeconds - returns the default delay on the queue in
-- seconds. ReceiveMessageWaitTimeSeconds - returns the time for which a
-- ReceiveMessage call will wait for a message to arrive. RedrivePolicy -
-- returns the parameters for dead letter queue functionality of the source
-- queue. For more information about RedrivePolicy and dead letter queues, see
-- Using Amazon SQS Dead Letter Queues in the Amazon SQS Developer Guide.
-- Going forward, new attributes might be added. If you are writing code that
-- calls this action, we recommend that you structure your code so that it can
-- handle new attributes gracefully. Some API actions take lists of
-- parameters. These lists are specified using the param.n notation. Values of
-- n are integers starting from 1. For example, a parameter list with two
-- elements looks like this: &amp;Attribute.1=this &amp;Attribute.2=that The
-- following example Query requests gets all the attribute values for the
-- specified queue. http://sqs.us-east-1.amazonaws.com/123456789012/testQueue/
-- ?Action=GetQueueAttributes &AttributeName.1=All &Version=2012-11-05
-- &SignatureMethod=HmacSHA256 &Expires=2013-10-18T22%3A52%3A43PST
-- &AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &SignatureVersion=2
-- &Signature=Dqlp3Sd6ljTUA9Uf6SGtEExwUQEXAMPLE ReceiveMessageWaitTimeSeconds
-- 2 VisibilityTimeout 30 ApproximateNumberOfMessages 0
-- ApproximateNumberOfMessagesNotVisible 0 CreatedTimestamp 1286771522
-- LastModifiedTimestamp 1286771522 QueueArn
-- arn:aws:sqs:us-east-1:123456789012:qfoo MaximumMessageSize 8192
-- MessageRetentionPeriod 345600 1ea71be5-b5a2-4f9d-b85a-945d8d08cd0b The
-- following example Query request gets three attribute values for the
-- specified queue. http://sqs.us-east-1.amazonaws.com/123456789012/testQueue/
-- ?Action=GetQueueAttributes &Action=GetQueueAttributes &Version=2012-11-05
-- &AttributeName.1=VisibilityTimeout &AttributeName.2=DelaySeconds
-- &AttributeName.3=ReceiveMessageWaitTimeSeconds &SignatureMethod=HmacSHA256
-- &Expires=2013-10-18T22%3A52%3A43PST &AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &SignatureVersion=2 &Signature=Dqlp3Sd6ljTUA9Uf6SGtEExwUQEXAMPLE
-- VisibilityTimeout 30 DelaySeconds 0 ReceiveMessageWaitTimeSeconds 2.
module Network.AWS.SQS.GetQueueAttributes
    (
    -- * Request
      GetQueueAttributes
    -- ** Request constructor
    , mkGetQueueAttributes
    -- ** Request lenses
    , gqaQueueUrl
    , gqaAttributeNames

    -- * Response
    , GetQueueAttributesResponse
    -- ** Response constructor
    , mkGetQueueAttributesResponse
    -- ** Response lenses
    , gqarAttributes
    ) where

import Network.AWS.Request.Query
import Network.AWS.SQS.Types
import Network.AWS.Prelude

data GetQueueAttributes = GetQueueAttributes
    { _gqaQueueUrl :: !Text
    , _gqaAttributeNames :: [QueueAttributeName]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetQueueAttributes' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @QueueUrl ::@ @Text@
--
-- * @AttributeNames ::@ @[QueueAttributeName]@
--
mkGetQueueAttributes :: Text -- ^ 'gqaQueueUrl'
                     -> GetQueueAttributes
mkGetQueueAttributes p1 = GetQueueAttributes
    { _gqaQueueUrl = p1
    , _gqaAttributeNames = mempty
    }

-- | The URL of the Amazon SQS queue to take action on.
gqaQueueUrl :: Lens' GetQueueAttributes Text
gqaQueueUrl = lens _gqaQueueUrl (\s a -> s { _gqaQueueUrl = a })

-- | A list of attributes to retrieve information for.
gqaAttributeNames :: Lens' GetQueueAttributes [QueueAttributeName]
gqaAttributeNames =
    lens _gqaAttributeNames (\s a -> s { _gqaAttributeNames = a })

instance ToQuery GetQueueAttributes where
    toQuery = genericQuery def

-- | A list of returned queue attributes.
newtype GetQueueAttributesResponse = GetQueueAttributesResponse
    { _gqarAttributes :: Map QueueAttributeName Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetQueueAttributesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Attributes ::@ @Map QueueAttributeName Text@
--
mkGetQueueAttributesResponse :: GetQueueAttributesResponse
mkGetQueueAttributesResponse = GetQueueAttributesResponse
    { _gqarAttributes = mempty
    }

-- | A map of attributes to the respective values.
gqarAttributes :: Lens' GetQueueAttributesResponse (Map QueueAttributeName Text)
gqarAttributes = lens _gqarAttributes (\s a -> s { _gqarAttributes = a })

instance FromXML GetQueueAttributesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetQueueAttributes where
    type Sv GetQueueAttributes = SQS
    type Rs GetQueueAttributes = GetQueueAttributesResponse

    request = post "GetQueueAttributes"
    response _ = xmlResponse
