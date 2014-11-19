{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
--
-- <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_ListQueues.html>
module Network.AWS.SQS.ListQueues
    (
    -- * Request
      ListQueues
    -- ** Request constructor
    , listQueues
    -- ** Request lenses
    , lqQueueNamePrefix

    -- * Response
    , ListQueuesResponse
    -- ** Response constructor
    , listQueuesResponse
    -- ** Response lenses
    , lqrQueueUrls
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SQS.Types
import qualified GHC.Exts

newtype ListQueues = ListQueues
    { _lqQueueNamePrefix :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

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

newtype ListQueuesResponse = ListQueuesResponse
    { _lqrQueueUrls :: Flatten [Text]
    } deriving (Eq, Ord, Show, Generic, Monoid, Semigroup)

-- | 'ListQueuesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lqrQueueUrls' @::@ ['Text']
--
listQueuesResponse :: [Text] -- ^ 'lqrQueueUrls'
                   -> ListQueuesResponse
listQueuesResponse p1 = ListQueuesResponse
    { _lqrQueueUrls = withIso _Flatten (const id) p1
    }

-- | A list of queue URLs, up to 1000 entries.
lqrQueueUrls :: Lens' ListQueuesResponse [Text]
lqrQueueUrls = lens _lqrQueueUrls (\s a -> s { _lqrQueueUrls = a })
    . _Flatten

instance ToPath ListQueues where
    toPath = const "/"

instance ToQuery ListQueues

instance ToHeaders ListQueues

instance AWSRequest ListQueues where
    type Sv ListQueues = SQS
    type Rs ListQueues = ListQueuesResponse

    request  = post "ListQueues"
    response = xmlResponse

instance FromXML ListQueuesResponse where
    parseXML = withElement "ListQueuesResult" $ \x ->
            <$> parseXML x
