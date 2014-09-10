{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SQS.ListDeadLetterSourceQueues
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of your queues that have the RedrivePolicy queue attribute
-- configured with a dead letter queue. The following example Query request
-- returns a list of dead letter source queues. In this example only one
-- source queue, MySourceQueue, was configured with a dead letter queue.
-- Action=ListDeadLetterSourceQueues &Version=2012-11-05
-- http://sqs.us-east-1.amazonaws.com/123456789012/MySourceQueue
-- 8ffb921f-b85e-53d9-abcf-d8d0057f38fc For more information about using dead
-- letter queues, see Using Amazon SQS Dead Letter Queues.
module Network.AWS.SQS
    (
    -- * Request
      ListDeadLetterSourceQueues
    -- ** Request constructor
    , mkListDeadLetterSourceQueues
    -- ** Request lenses
    , ldlsqQueueUrl

    -- * Response
    , ListDeadLetterSourceQueuesResponse
    -- ** Response constructor
    , mkListDeadLetterSourceQueuesResponse
    -- ** Response lenses
    , ldlsqrQueueUrls
    ) where

import Network.AWS.Request.Query
import Network.AWS.SQS.Types
import Network.AWS.Prelude

newtype ListDeadLetterSourceQueues = ListDeadLetterSourceQueues
    { _ldlsqQueueUrl :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListDeadLetterSourceQueues' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @QueueUrl ::@ @Text@
--
mkListDeadLetterSourceQueues :: Text -- ^ 'ldlsqQueueUrl'
                             -> ListDeadLetterSourceQueues
mkListDeadLetterSourceQueues p1 = ListDeadLetterSourceQueues
    { _ldlsqQueueUrl = p1
    }

-- | The queue URL of a dead letter queue.
ldlsqQueueUrl :: Lens' ListDeadLetterSourceQueues Text
ldlsqQueueUrl = lens _ldlsqQueueUrl (\s a -> s { _ldlsqQueueUrl = a })

instance ToQuery ListDeadLetterSourceQueues where
    toQuery = genericQuery def

-- | A list of your dead letter source queues.
newtype ListDeadLetterSourceQueuesResponse = ListDeadLetterSourceQueuesResponse
    { _ldlsqrQueueUrls :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListDeadLetterSourceQueuesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @QueueUrls ::@ @[Text]@
--
mkListDeadLetterSourceQueuesResponse :: [Text] -- ^ 'ldlsqrQueueUrls'
                                     -> ListDeadLetterSourceQueuesResponse
mkListDeadLetterSourceQueuesResponse p1 = ListDeadLetterSourceQueuesResponse
    { _ldlsqrQueueUrls = p1
    }

-- | A list of source queue URLs that have the RedrivePolicy queue attribute
-- configured with a dead letter queue.
ldlsqrQueueUrls :: Lens' ListDeadLetterSourceQueuesResponse [Text]
ldlsqrQueueUrls = lens _ldlsqrQueueUrls (\s a -> s { _ldlsqrQueueUrls = a })

instance FromXML ListDeadLetterSourceQueuesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListDeadLetterSourceQueues where
    type Sv ListDeadLetterSourceQueues = SQS
    type Rs ListDeadLetterSourceQueues = ListDeadLetterSourceQueuesResponse

    request = post "ListDeadLetterSourceQueues"
    response _ = xmlResponse
