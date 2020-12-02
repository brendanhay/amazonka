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
-- Module      : Network.AWS.SQS.UntagQueue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove cost allocation tags from the specified Amazon SQS queue. For an overview, see <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-queue-tags.html Tagging Your Amazon SQS Queues> in the /Amazon Simple Queue Service Developer Guide/ .
module Network.AWS.SQS.UntagQueue
  ( -- * Creating a Request
    untagQueue,
    UntagQueue,

    -- * Request Lenses
    uqQueueURL,
    uqTagKeys,

    -- * Destructuring the Response
    untagQueueResponse,
    UntagQueueResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SQS.Types

-- | /See:/ 'untagQueue' smart constructor.
data UntagQueue = UntagQueue'
  { _uqQueueURL :: !Text,
    _uqTagKeys :: ![Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UntagQueue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uqQueueURL' - The URL of the queue.
--
-- * 'uqTagKeys' - The list of tags to be removed from the specified queue.
untagQueue ::
  -- | 'uqQueueURL'
  Text ->
  UntagQueue
untagQueue pQueueURL_ =
  UntagQueue' {_uqQueueURL = pQueueURL_, _uqTagKeys = mempty}

-- | The URL of the queue.
uqQueueURL :: Lens' UntagQueue Text
uqQueueURL = lens _uqQueueURL (\s a -> s {_uqQueueURL = a})

-- | The list of tags to be removed from the specified queue.
uqTagKeys :: Lens' UntagQueue [Text]
uqTagKeys = lens _uqTagKeys (\s a -> s {_uqTagKeys = a}) . _Coerce

instance AWSRequest UntagQueue where
  type Rs UntagQueue = UntagQueueResponse
  request = postQuery sqs
  response = receiveNull UntagQueueResponse'

instance Hashable UntagQueue

instance NFData UntagQueue

instance ToHeaders UntagQueue where
  toHeaders = const mempty

instance ToPath UntagQueue where
  toPath = const "/"

instance ToQuery UntagQueue where
  toQuery UntagQueue' {..} =
    mconcat
      [ "Action" =: ("UntagQueue" :: ByteString),
        "Version" =: ("2012-11-05" :: ByteString),
        "QueueUrl" =: _uqQueueURL,
        toQueryList "TagKey" _uqTagKeys
      ]

-- | /See:/ 'untagQueueResponse' smart constructor.
data UntagQueueResponse = UntagQueueResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UntagQueueResponse' with the minimum fields required to make a request.
untagQueueResponse ::
  UntagQueueResponse
untagQueueResponse = UntagQueueResponse'

instance NFData UntagQueueResponse
