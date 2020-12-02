{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.PurgeQueue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the messages in a queue specified by the @QueueURL@ parameter.
--
--
-- /Important:/ When you use the @PurgeQueue@ action, you can't retrieve any messages deleted from a queue.
--
-- The message deletion process takes up to 60 seconds. We recommend waiting for 60 seconds regardless of your queue's size.
--
-- Messages sent to the queue /before/ you call @PurgeQueue@ might be received but are deleted within the next minute.
--
-- Messages sent to the queue /after/ you call @PurgeQueue@ might be deleted while the queue is being purged.
module Network.AWS.SQS.PurgeQueue
  ( -- * Creating a Request
    purgeQueue,
    PurgeQueue,

    -- * Request Lenses
    pqQueueURL,

    -- * Destructuring the Response
    purgeQueueResponse,
    PurgeQueueResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SQS.Types

-- |
--
--
--
-- /See:/ 'purgeQueue' smart constructor.
newtype PurgeQueue = PurgeQueue' {_pqQueueURL :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PurgeQueue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pqQueueURL' - The URL of the queue from which the @PurgeQueue@ action deletes messages. Queue URLs and names are case-sensitive.
purgeQueue ::
  -- | 'pqQueueURL'
  Text ->
  PurgeQueue
purgeQueue pQueueURL_ = PurgeQueue' {_pqQueueURL = pQueueURL_}

-- | The URL of the queue from which the @PurgeQueue@ action deletes messages. Queue URLs and names are case-sensitive.
pqQueueURL :: Lens' PurgeQueue Text
pqQueueURL = lens _pqQueueURL (\s a -> s {_pqQueueURL = a})

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
  toQuery PurgeQueue' {..} =
    mconcat
      [ "Action" =: ("PurgeQueue" :: ByteString),
        "Version" =: ("2012-11-05" :: ByteString),
        "QueueUrl" =: _pqQueueURL
      ]

-- | /See:/ 'purgeQueueResponse' smart constructor.
data PurgeQueueResponse = PurgeQueueResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PurgeQueueResponse' with the minimum fields required to make a request.
purgeQueueResponse ::
  PurgeQueueResponse
purgeQueueResponse = PurgeQueueResponse'

instance NFData PurgeQueueResponse
