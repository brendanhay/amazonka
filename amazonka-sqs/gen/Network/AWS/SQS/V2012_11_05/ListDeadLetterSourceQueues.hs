{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SQS.V2012_11_05.ListDeadLetterSourceQueues
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
module Network.AWS.SQS.V2012_11_05.ListDeadLetterSourceQueues
    (
    -- * Request
      ListDeadLetterSourceQueues
    -- ** Request constructor
    , mkListDeadLetterSourceQueuesRequest
    -- ** Request lenses
    , ldlsqrQueueUrl

    -- * Response
    , ListDeadLetterSourceQueuesResponse
    -- ** Response lenses
    , ldlsqsQueueUrls
    ) where

import Network.AWS.Request.Query
import Network.AWS.SQS.V2012_11_05.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListDeadLetterSourceQueues' request.
mkListDeadLetterSourceQueuesRequest :: Text -- ^ 'ldlsqrQueueUrl'
                                    -> ListDeadLetterSourceQueues
mkListDeadLetterSourceQueuesRequest p1 = ListDeadLetterSourceQueues
    { _ldlsqrQueueUrl = p1
    }
{-# INLINE mkListDeadLetterSourceQueuesRequest #-}

newtype ListDeadLetterSourceQueues = ListDeadLetterSourceQueues
    { _ldlsqrQueueUrl :: Text
      -- ^ The queue URL of a dead letter queue.
    } deriving (Show, Generic)

-- | The queue URL of a dead letter queue.
ldlsqrQueueUrl :: Lens' ListDeadLetterSourceQueues (Text)
ldlsqrQueueUrl = lens _ldlsqrQueueUrl (\s a -> s { _ldlsqrQueueUrl = a })
{-# INLINE ldlsqrQueueUrl #-}

instance ToQuery ListDeadLetterSourceQueues where
    toQuery = genericQuery def

newtype ListDeadLetterSourceQueuesResponse = ListDeadLetterSourceQueuesResponse
    { _ldlsqsQueueUrls :: [Text]
      -- ^ A list of source queue URLs that have the RedrivePolicy queue
      -- attribute configured with a dead letter queue.
    } deriving (Show, Generic)

-- | A list of source queue URLs that have the RedrivePolicy queue attribute
-- configured with a dead letter queue.
ldlsqsQueueUrls :: Lens' ListDeadLetterSourceQueuesResponse ([Text])
ldlsqsQueueUrls = lens _ldlsqsQueueUrls (\s a -> s { _ldlsqsQueueUrls = a })
{-# INLINE ldlsqsQueueUrls #-}

instance FromXML ListDeadLetterSourceQueuesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListDeadLetterSourceQueues where
    type Sv ListDeadLetterSourceQueues = SQS
    type Rs ListDeadLetterSourceQueues = ListDeadLetterSourceQueuesResponse

    request = post "ListDeadLetterSourceQueues"
    response _ = xmlResponse
