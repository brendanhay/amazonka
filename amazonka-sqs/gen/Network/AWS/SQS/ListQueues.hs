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

-- Module      : Network.AWS.SQS.ListQueues
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of your queues. The maximum number of queues that can be
-- returned is 1000. If you specify a value for the optional QueueNamePrefix
-- parameter, only queues with a name beginning with the specified value are
-- returned.
module Network.AWS.SQS.ListQueues
    (
    -- * Request
      ListQueues
    -- ** Request constructor
    , listQueues
    -- ** Request lenses
    , lqQueueNamePrefix

    -- * Response
    , ListQueuesResult
    -- ** Response constructor
    , listQueuesResult
    -- ** Response lenses
    , lqrQueueUrls
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SQS.Types

newtype ListQueues = ListQueues
    { _lqQueueNamePrefix :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListQueues' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lqQueueNamePrefix' @::@ 'Maybe' 'Text'
--
listQueues :: ListQueues
listQueues = ListQueues
    { _lqQueueNamePrefix = Nothing
    }

-- | A string to use for filtering the list results. Only those queues whose
-- name begins with the specified string are returned.
lqQueueNamePrefix :: Lens' ListQueues (Maybe Text)
lqQueueNamePrefix =
    lens _lqQueueNamePrefix (\s a -> s { _lqQueueNamePrefix = a })
instance ToQuery ListQueues

instance ToPath ListQueues where
    toPath = const "/"

newtype ListQueuesResult = ListQueuesResult
    { _lqrQueueUrls :: [Text]
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'ListQueuesResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lqrQueueUrls' @::@ ['Text']
--
listQueuesResult :: ListQueuesResult
listQueuesResult = ListQueuesResult
    { _lqrQueueUrls = mempty
    }

-- | A list of queue URLs, up to 1000 entries.
lqrQueueUrls :: Lens' ListQueuesResult [Text]
lqrQueueUrls = lens _lqrQueueUrls (\s a -> s { _lqrQueueUrls = a })
instance FromXML ListQueuesResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ListQueuesResult"

instance AWSRequest ListQueues where
    type Sv ListQueues = SQS
    type Rs ListQueues = ListQueuesResult

    request  = post "ListQueues"
    response = xmlResponse $ \h x -> ListQueuesResult
        <$> x %| "QueueUrls"
