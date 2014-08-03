{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
module Network.AWS.SQS.V2012_11_05.ListDeadLetterSourceQueues where

import Control.Lens
import Network.AWS.Request.Query
import Network.AWS.SQS.V2012_11_05.Types
import Network.AWS.Prelude

data ListDeadLetterSourceQueues = ListDeadLetterSourceQueues
    { _ldlsqrQueueUrl :: Text
      -- ^ The queue URL of a dead letter queue.
    } deriving (Generic)

makeLenses ''ListDeadLetterSourceQueues

instance ToQuery ListDeadLetterSourceQueues where
    toQuery = genericToQuery def

data ListDeadLetterSourceQueuesResponse = ListDeadLetterSourceQueuesResponse
    { _ldlsqsQueueUrls :: [Text]
      -- ^ A list of source queue URLs that have the RedrivePolicy queue
      -- attribute configured with a dead letter queue.
    } deriving (Generic)

makeLenses ''ListDeadLetterSourceQueuesResponse

instance FromXML ListDeadLetterSourceQueuesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListDeadLetterSourceQueues where
    type Sv ListDeadLetterSourceQueues = SQS
    type Rs ListDeadLetterSourceQueues = ListDeadLetterSourceQueuesResponse

    request = post "ListDeadLetterSourceQueues"
    response _ = xmlResponse
