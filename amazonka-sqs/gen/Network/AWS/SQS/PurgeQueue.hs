{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.PurgeQueue
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the messages in a queue specified by the __queue URL__.
--
-- When you use the 'PurgeQueue' API, the deleted messages in the queue
-- cannot be retrieved.
--
-- When you purge a queue, the message deletion process takes up to 60
-- seconds. All messages sent to the queue before calling 'PurgeQueue' will
-- be deleted; messages sent to the queue while it is being purged may be
-- deleted. While the queue is being purged, messages sent to the queue
-- before 'PurgeQueue' was called may be received, but will be deleted
-- within the next minute.
module Network.AWS.SQS.PurgeQueue
    (
    -- * Creating a Request
      purgeQueue
    , PurgeQueue
    -- * Request Lenses
    , pqQueueURL

    -- * Destructuring the Response
    , purgeQueueResponse
    , PurgeQueueResponse
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SQS.Types
import           Network.AWS.SQS.Types.Product

-- | /See:/ 'purgeQueue' smart constructor.
newtype PurgeQueue = PurgeQueue'
    { _pqQueueURL :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PurgeQueue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pqQueueURL'
purgeQueue
    :: Text -- ^ 'pqQueueURL'
    -> PurgeQueue
purgeQueue pQueueURL_ =
    PurgeQueue'
    { _pqQueueURL = pQueueURL_
    }

-- | The queue URL of the queue to delete the messages from when using the
-- 'PurgeQueue' API.
pqQueueURL :: Lens' PurgeQueue Text
pqQueueURL = lens _pqQueueURL (\ s a -> s{_pqQueueURL = a});

instance AWSRequest PurgeQueue where
        type Rs PurgeQueue = PurgeQueueResponse
        request = postQuery sqs
        response = receiveNull PurgeQueueResponse'

instance Hashable PurgeQueue

instance NFData PurgeQueue

instance ToHeaders PurgeQueue where
        toHeaders = const mempty

instance ToPath PurgeQueue where
        toPath = const "/"

instance ToQuery PurgeQueue where
        toQuery PurgeQueue'{..}
          = mconcat
              ["Action" =: ("PurgeQueue" :: ByteString),
               "Version" =: ("2012-11-05" :: ByteString),
               "QueueUrl" =: _pqQueueURL]

-- | /See:/ 'purgeQueueResponse' smart constructor.
data PurgeQueueResponse =
    PurgeQueueResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PurgeQueueResponse' with the minimum fields required to make a request.
--
purgeQueueResponse
    :: PurgeQueueResponse
purgeQueueResponse = PurgeQueueResponse'

instance NFData PurgeQueueResponse
