{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
-- configured with a dead letter queue. For more information about using dead
-- letter queues, see Using Amazon SQS Dead Letter Queues.
module Network.AWS.SQS.ListDeadLetterSourceQueues
    (
    -- * Request
      ListDeadLetterSourceQueues
    -- ** Request constructor
    , listDeadLetterSourceQueues
    -- ** Request lenses
    , ldlsqQueueUrl

    -- * Response
    , ListDeadLetterSourceQueuesResult
    -- ** Response constructor
    , listDeadLetterSourceQueuesResult
    -- ** Response lenses
    , ldlsqrQueueUrls
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SQS.Types

newtype ListDeadLetterSourceQueues = ListDeadLetterSourceQueues
    { _ldlsqQueueUrl :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'ListDeadLetterSourceQueues' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldlsqQueueUrl' @::@ 'Text'
--
listDeadLetterSourceQueues :: Text -- ^ 'ldlsqQueueUrl'
                           -> ListDeadLetterSourceQueues
listDeadLetterSourceQueues p1 = ListDeadLetterSourceQueues
    { _ldlsqQueueUrl = p1
    }

-- | The queue URL of a dead letter queue.
ldlsqQueueUrl :: Lens' ListDeadLetterSourceQueues Text
ldlsqQueueUrl = lens _ldlsqQueueUrl (\s a -> s { _ldlsqQueueUrl = a })
instance ToQuery ListDeadLetterSourceQueues

instance ToPath ListDeadLetterSourceQueues where
    toPath = const "/"

newtype ListDeadLetterSourceQueuesResult = ListDeadLetterSourceQueuesResult
    { _ldlsqrQueueUrls :: [Text]
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'ListDeadLetterSourceQueuesResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldlsqrQueueUrls' @::@ ['Text']
--
listDeadLetterSourceQueuesResult :: ListDeadLetterSourceQueuesResult
listDeadLetterSourceQueuesResult = ListDeadLetterSourceQueuesResult
    { _ldlsqrQueueUrls = mempty
    }

-- | A list of source queue URLs that have the RedrivePolicy queue attribute
-- configured with a dead letter queue.
ldlsqrQueueUrls :: Lens' ListDeadLetterSourceQueuesResult [Text]
ldlsqrQueueUrls = lens _ldlsqrQueueUrls (\s a -> s { _ldlsqrQueueUrls = a })
instance FromXML ListDeadLetterSourceQueuesResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ListDeadLetterSourceQueuesResult"

instance AWSRequest ListDeadLetterSourceQueues where
    type Sv ListDeadLetterSourceQueues = SQS
    type Rs ListDeadLetterSourceQueues = ListDeadLetterSourceQueuesResult

    request  = post "ListDeadLetterSourceQueues"
    response = xmlResponse $ \h x -> ListDeadLetterSourceQueuesResult
        <$> x %| "queueUrls"
