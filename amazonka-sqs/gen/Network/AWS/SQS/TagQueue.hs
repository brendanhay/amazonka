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
-- Module      : Network.AWS.SQS.TagQueue
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Add cost allocation tags to the specified Amazon SQS queue. For an overview, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-tagging-queues.html Tagging Amazon SQS Queues> in the /Amazon Simple Queue Service Developer Guide/ .
--
--
-- When you use queue tags, keep the following guidelines in mind:
--
--     * Adding more than 50 tags to a queue isn't recommended.
--
--     * Tags don't have any semantic meaning. Amazon SQS interprets tags as character strings.
--
--     * Tags are case-sensitive.
--
--     * A new tag with a key identical to that of an existing tag overwrites the existing tag.
--
--     * Tagging API actions are limited to 5 TPS per AWS account. If your application requires a higher throughput, file a <https://console.aws.amazon.com/support/home#/case/create?issueType=technical technical support request> .
--
--
--
-- For a full list of tag restrictions, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/limits-queues.html Limits Related to Queues> in the /Amazon Simple Queue Service Developer Guide/ .
--
module Network.AWS.SQS.TagQueue
    (
    -- * Creating a Request
      tagQueue
    , TagQueue
    -- * Request Lenses
    , tqQueueURL
    , tqTags

    -- * Destructuring the Response
    , tagQueueResponse
    , TagQueueResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SQS.Types
import Network.AWS.SQS.Types.Product

-- | /See:/ 'tagQueue' smart constructor.
data TagQueue = TagQueue'
  { _tqQueueURL :: !Text
  , _tqTags     :: !(Map Text Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TagQueue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tqQueueURL' - The URL of the queue.
--
-- * 'tqTags' - The list of tags to be added to the specified queue.
tagQueue
    :: Text -- ^ 'tqQueueURL'
    -> TagQueue
tagQueue pQueueURL_ = TagQueue' {_tqQueueURL = pQueueURL_, _tqTags = mempty}


-- | The URL of the queue.
tqQueueURL :: Lens' TagQueue Text
tqQueueURL = lens _tqQueueURL (\ s a -> s{_tqQueueURL = a})

-- | The list of tags to be added to the specified queue.
tqTags :: Lens' TagQueue (HashMap Text Text)
tqTags = lens _tqTags (\ s a -> s{_tqTags = a}) . _Map

instance AWSRequest TagQueue where
        type Rs TagQueue = TagQueueResponse
        request = postQuery sqs
        response = receiveNull TagQueueResponse'

instance Hashable TagQueue where

instance NFData TagQueue where

instance ToHeaders TagQueue where
        toHeaders = const mempty

instance ToPath TagQueue where
        toPath = const "/"

instance ToQuery TagQueue where
        toQuery TagQueue'{..}
          = mconcat
              ["Action" =: ("TagQueue" :: ByteString),
               "Version" =: ("2012-11-05" :: ByteString),
               "QueueUrl" =: _tqQueueURL,
               toQueryMap "Tags" "Key" "Value" _tqTags]

-- | /See:/ 'tagQueueResponse' smart constructor.
data TagQueueResponse =
  TagQueueResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TagQueueResponse' with the minimum fields required to make a request.
--
tagQueueResponse
    :: TagQueueResponse
tagQueueResponse = TagQueueResponse'


instance NFData TagQueueResponse where
