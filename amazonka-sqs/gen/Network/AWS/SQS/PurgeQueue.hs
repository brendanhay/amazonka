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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the messages in a queue specified by the @QueueURL@ parameter.
--
--
-- /Important:/ When you use the @PurgeQueue@ action, you can't retrieve a message deleted from a queue.
--
-- When you purge a queue, the message deletion process takes up to 60 seconds. All messages sent to the queue before calling the @PurgeQueue@ action are deleted. Messages sent to the queue while it is being purged might be deleted. While the queue is being purged, messages sent to the queue before @PurgeQueue@ is called might be received, but are deleted within the next minute.
--
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

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SQS.Types
import Network.AWS.SQS.Types.Product

-- |
--
--
--
-- /See:/ 'purgeQueue' smart constructor.
newtype PurgeQueue = PurgeQueue'
  { _pqQueueURL :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PurgeQueue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pqQueueURL' - The URL of the queue from which the @PurgeQueue@ action deletes messages. Queue URLs are case-sensitive.
purgeQueue
    :: Text -- ^ 'pqQueueURL'
    -> PurgeQueue
purgeQueue pQueueURL_ = PurgeQueue' {_pqQueueURL = pQueueURL_}


-- | The URL of the queue from which the @PurgeQueue@ action deletes messages. Queue URLs are case-sensitive.
pqQueueURL :: Lens' PurgeQueue Text
pqQueueURL = lens _pqQueueURL (\ s a -> s{_pqQueueURL = a})

instance AWSRequest PurgeQueue where
        type Rs PurgeQueue = PurgeQueueResponse
        request = postQuery sqs
        response = receiveNull PurgeQueueResponse'

instance Hashable PurgeQueue where

instance NFData PurgeQueue where

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
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PurgeQueueResponse' with the minimum fields required to make a request.
--
purgeQueueResponse
    :: PurgeQueueResponse
purgeQueueResponse = PurgeQueueResponse'


instance NFData PurgeQueueResponse where
