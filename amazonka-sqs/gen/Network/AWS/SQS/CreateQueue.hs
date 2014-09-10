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

-- | Creates a new queue, or returns the URL of an existing one. When you
-- request CreateQueue, you provide a name for the queue. To successfully
-- create a new queue, you must provide a name that is unique within the scope
-- of your own queues. If you delete a queue, you must wait at least 60
-- seconds before creating a queue with the same name. You may pass one or
-- more attributes in the request. If you do not provide a value for any
-- attribute, the queue will have the default value for that attribute.
-- Permitted attributes are the same that can be set using SetQueueAttributes.
-- Use GetQueueUrl to get a queue's URL. GetQueueUrl requires only the
-- QueueName parameter. If you provide the name of an existing queue, along
-- with the exact names and values of all the queue's attributes, CreateQueue
-- returns the queue URL for the existing queue. If the queue name, attribute
-- names, or attribute values do not match an existing queue, CreateQueue
-- returns an error. Some API actions take lists of parameters. These lists
-- are specified using the param.n notation. Values of n are integers starting
-- from 1. For example, a parameter list with two elements looks like this:
-- &amp;Attribute.1=this &amp;Attribute.2=that The following example Query
-- request creates a new queue named testQueue.
-- http://sqs.us-east-1.amazonaws.com/ ?Action=CreateQueue
-- &QueueName=testQueue &Attribute.1.Name=VisibilityTimeout
-- &Attribute.1.Value=40 &Version=2011-10-01 &SignatureMethod=HmacSHA256
-- &Expires=2011-10-18T22%3A52%3A43PST &AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &SignatureVersion=2 &Signature=Dqlp3Sd6ljTUA9Uf6SGtEExwUQEXAMPLE
-- http://&useast1-query;/123456789012/testQueue
-- 7a62c49f-347e-4fc4-9331-6e8e7a96aa73.
module Network.AWS.SQS
    (
    -- * Request
      CreateQueue
    -- ** Request constructor
    , mkCreateQueue
    -- ** Request lenses
    , cqQueueName
    , cqAttributes

    -- * Response
    , CreateQueueResponse
    -- ** Response constructor
    , mkCreateQueueResponse
    -- ** Response lenses
    , cqrQueueUrl
    ) where

import Network.AWS.Request.Query
import Network.AWS.SQS.Types
import Network.AWS.Prelude

data CreateQueue = CreateQueue
    { _cqQueueName :: !Text
    , _cqAttributes :: Map QueueAttributeName Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateQueue' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @QueueName ::@ @Text@
--
-- * @Attributes ::@ @Map QueueAttributeName Text@
--
mkCreateQueue :: Text -- ^ 'cqQueueName'
              -> CreateQueue
mkCreateQueue p1 = CreateQueue
    { _cqQueueName = p1
    , _cqAttributes = mempty
    }

-- | The name for the queue to be created.
cqQueueName :: Lens' CreateQueue Text
cqQueueName = lens _cqQueueName (\s a -> s { _cqQueueName = a })

-- | A map of attributes with their corresponding values. The following lists
-- the names, descriptions, and values of the special request parameters the
-- CreateQueue action uses: DelaySeconds - The time in seconds that the
-- delivery of all messages in the queue will be delayed. An integer from 0 to
-- 900 (15 minutes). The default for this attribute is 0 (zero).
-- MaximumMessageSize - The limit of how many bytes a message can contain
-- before Amazon SQS rejects it. An integer from 1024 bytes (1 KiB) up to
-- 262144 bytes (256 KiB). The default for this attribute is 262144 (256 KiB).
-- MessageRetentionPeriod - The number of seconds Amazon SQS retains a
-- message. Integer representing seconds, from 60 (1 minute) to 1209600 (14
-- days). The default for this attribute is 345600 (4 days). Policy - The
-- queue's policy. A valid form-url-encoded policy. For more information about
-- policy structure, see Basic Policy Structure in the Amazon SQS Developer
-- Guide. For more information about form-url-encoding, see
-- http://www.w3.org/MarkUp/html-spec/html-spec_8.html#SEC8.2.1.
-- ReceiveMessageWaitTimeSeconds - The time for which a ReceiveMessage call
-- will wait for a message to arrive. An integer from 0 to 20 (seconds). The
-- default for this attribute is 0. VisibilityTimeout - The visibility timeout
-- for the queue. An integer from 0 to 43200 (12 hours). The default for this
-- attribute is 30. For more information about visibility timeout, see
-- Visibility Timeout in the Amazon SQS Developer Guide.
cqAttributes :: Lens' CreateQueue (Map QueueAttributeName Text)
cqAttributes = lens _cqAttributes (\s a -> s { _cqAttributes = a })

instance ToQuery CreateQueue where
    toQuery = genericQuery def

-- | Returns the QueueUrl element of the created queue.
newtype CreateQueueResponse = CreateQueueResponse
    { _cqrQueueUrl :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateQueueResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @QueueUrl ::@ @Maybe Text@
--
mkCreateQueueResponse :: CreateQueueResponse
mkCreateQueueResponse = CreateQueueResponse
    { _cqrQueueUrl = Nothing
    }

-- | The URL for the created Amazon SQS queue.
cqrQueueUrl :: Lens' CreateQueueResponse (Maybe Text)
cqrQueueUrl = lens _cqrQueueUrl (\s a -> s { _cqrQueueUrl = a })

instance FromXML CreateQueueResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateQueue where
    type Sv CreateQueue = SQS
    type Rs CreateQueue = CreateQueueResponse

    request = post "CreateQueue"
    response _ = xmlResponse
