{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SQS.DeleteQueue
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the queue specified by the queue URL, regardless of whether the
-- queue is empty. If the specified queue does not exist, Amazon SQS returns a
-- successful response. Use DeleteQueue with care; once you delete your queue,
-- any messages in the queue are no longer available. When you delete a queue,
-- the deletion process takes up to 60 seconds. Requests you send involving
-- that queue during the 60 seconds might succeed. For example, a SendMessage
-- request might succeed, but after the 60 seconds, the queue and that message
-- you sent no longer exist. Also, when you delete a queue, you must wait at
-- least 60 seconds before creating a queue with the same name. We reserve the
-- right to delete queues that have had no activity for more than 30 days. For
-- more information, see How Amazon SQS Queues Work in the Amazon SQS
-- Developer Guide.
--
-- <DeleteQueue.html>
module Network.AWS.SQS.DeleteQueue
    (
    -- * Request
      DeleteQueue
    -- ** Request constructor
    , deleteQueue
    -- ** Request lenses
    , dqQueueUrl

    -- * Response
    , DeleteQueueResponse
    -- ** Response constructor
    , deleteQueueResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SQS.Types
import qualified GHC.Exts

newtype DeleteQueue = DeleteQueue
    { _dqQueueUrl :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DeleteQueue' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dqQueueUrl' @::@ 'Text'
--
deleteQueue :: Text -- ^ 'dqQueueUrl'
            -> DeleteQueue
deleteQueue p1 = DeleteQueue
    { _dqQueueUrl = p1
    }

-- | The URL of the Amazon SQS queue to take action on.
dqQueueUrl :: Lens' DeleteQueue Text
dqQueueUrl = lens _dqQueueUrl (\s a -> s { _dqQueueUrl = a })

data DeleteQueueResponse = DeleteQueueResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteQueueResponse' constructor.
deleteQueueResponse :: DeleteQueueResponse
deleteQueueResponse = DeleteQueueResponse

instance AWSRequest DeleteQueue where
    type Sv DeleteQueue = SQS
    type Rs DeleteQueue = DeleteQueueResponse

    request  = post "DeleteQueue"
    response = nullResponse DeleteQueueResponse

instance ToPath DeleteQueue where
    toPath = const "/"

instance ToHeaders DeleteQueue

instance ToQuery DeleteQueue
