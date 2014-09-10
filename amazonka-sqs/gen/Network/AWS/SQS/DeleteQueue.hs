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
-- Developer Guide. The following example Query request deletes the specified
-- queue. http://sqs.us-east-1.amazonaws.com/123456789012/testQueue/
-- ?Action=DeleteQueue &Version=2009-02-01 &SignatureMethod=HmacSHA256
-- &Expires=2009-04-18T22%3A52%3A43PST &AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &SignatureVersion=2 &Signature=Dqlp3Sd6ljTUA9Uf6SGtEExwUQEXAMPLE
-- 6fde8d1e-52cd-4581-8cd9-c512f4c64223.
module Network.AWS.SQS
    (
    -- * Request
      DeleteQueue
    -- ** Request constructor
    , mkDeleteQueue
    -- ** Request lenses
    , dqQueueUrl

    -- * Response
    , DeleteQueueResponse
    -- ** Response constructor
    , mkDeleteQueueResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.SQS.Types
import Network.AWS.Prelude

newtype DeleteQueue = DeleteQueue
    { _dqQueueUrl :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteQueue' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @QueueUrl ::@ @Text@
--
mkDeleteQueue :: Text -- ^ 'dqQueueUrl'
              -> DeleteQueue
mkDeleteQueue p1 = DeleteQueue
    { _dqQueueUrl = p1
    }

-- | The URL of the Amazon SQS queue to take action on.
dqQueueUrl :: Lens' DeleteQueue Text
dqQueueUrl = lens _dqQueueUrl (\s a -> s { _dqQueueUrl = a })

instance ToQuery DeleteQueue where
    toQuery = genericQuery def

data DeleteQueueResponse = DeleteQueueResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteQueueResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDeleteQueueResponse :: DeleteQueueResponse
mkDeleteQueueResponse = DeleteQueueResponse

instance AWSRequest DeleteQueue where
    type Sv DeleteQueue = SQS
    type Rs DeleteQueue = DeleteQueueResponse

    request = post "DeleteQueue"
    response _ = nullaryResponse DeleteQueueResponse
