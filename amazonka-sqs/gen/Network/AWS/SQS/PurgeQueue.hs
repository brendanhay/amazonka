{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SQS.PurgeQueue
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deletes the messages in a queue specified by the queue URL.
--
-- When you use the 'PurgeQueue' API, the deleted messages in the queue cannot be
-- retrieved. When you purge a queue, the message deletion process takes up to
-- 60 seconds. All messages sent to the queue before calling 'PurgeQueue' will be
-- deleted; messages sent to the queue while it is being purged may be deleted.
-- While the queue is being purged, messages sent to the queue before 'PurgeQueue'
-- was called may be received, but will be deleted within the next minute.
--
-- <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_PurgeQueue.html>
module Network.AWS.SQS.PurgeQueue
    (
    -- * Request
      PurgeQueue
    -- ** Request constructor
    , purgeQueue
    -- ** Request lenses
    , pqQueueUrl

    -- * Response
    , PurgeQueueResponse
    -- ** Response constructor
    , purgeQueueResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SQS.Types
import qualified GHC.Exts

newtype PurgeQueue = PurgeQueue
    { _pqQueueUrl :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'PurgeQueue' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pqQueueUrl' @::@ 'Text'
--
purgeQueue :: Text -- ^ 'pqQueueUrl'
           -> PurgeQueue
purgeQueue p1 = PurgeQueue
    { _pqQueueUrl = p1
    }

-- | The queue URL of the queue to delete the messages from when using the 'PurgeQueue' API.
pqQueueUrl :: Lens' PurgeQueue Text
pqQueueUrl = lens _pqQueueUrl (\s a -> s { _pqQueueUrl = a })

data PurgeQueueResponse = PurgeQueueResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'PurgeQueueResponse' constructor.
purgeQueueResponse :: PurgeQueueResponse
purgeQueueResponse = PurgeQueueResponse

instance ToPath PurgeQueue where
    toPath = const "/"

instance ToQuery PurgeQueue where
    toQuery PurgeQueue{..} = mconcat
        [ "QueueUrl" =? _pqQueueUrl
        ]

instance ToHeaders PurgeQueue

instance AWSRequest PurgeQueue where
    type Sv PurgeQueue = SQS
    type Rs PurgeQueue = PurgeQueueResponse

    request  = post "PurgeQueue"
    response = nullResponse PurgeQueueResponse
