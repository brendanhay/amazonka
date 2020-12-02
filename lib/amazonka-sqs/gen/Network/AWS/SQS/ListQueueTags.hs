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
-- Module      : Network.AWS.SQS.ListQueueTags
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all cost allocation tags added to the specified Amazon SQS queue. For an overview, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-tagging-queues.html Tagging Amazon SQS Queues> in the /Amazon Simple Queue Service Developer Guide/ .
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
module Network.AWS.SQS.ListQueueTags
    (
    -- * Creating a Request
      listQueueTags
    , ListQueueTags
    -- * Request Lenses
    , lqtQueueURL

    -- * Destructuring the Response
    , listQueueTagsResponse
    , ListQueueTagsResponse
    -- * Response Lenses
    , lqtrsTags
    , lqtrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SQS.Types
import Network.AWS.SQS.Types.Product

-- | /See:/ 'listQueueTags' smart constructor.
newtype ListQueueTags = ListQueueTags'
  { _lqtQueueURL :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListQueueTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lqtQueueURL' - The URL of the queue.
listQueueTags
    :: Text -- ^ 'lqtQueueURL'
    -> ListQueueTags
listQueueTags pQueueURL_ = ListQueueTags' {_lqtQueueURL = pQueueURL_}


-- | The URL of the queue.
lqtQueueURL :: Lens' ListQueueTags Text
lqtQueueURL = lens _lqtQueueURL (\ s a -> s{_lqtQueueURL = a})

instance AWSRequest ListQueueTags where
        type Rs ListQueueTags = ListQueueTagsResponse
        request = postQuery sqs
        response
          = receiveXMLWrapper "ListQueueTagsResult"
              (\ s h x ->
                 ListQueueTagsResponse' <$>
                   (may (parseXMLMap "Tag" "Key" "Value") x) <*>
                     (pure (fromEnum s)))

instance Hashable ListQueueTags where

instance NFData ListQueueTags where

instance ToHeaders ListQueueTags where
        toHeaders = const mempty

instance ToPath ListQueueTags where
        toPath = const "/"

instance ToQuery ListQueueTags where
        toQuery ListQueueTags'{..}
          = mconcat
              ["Action" =: ("ListQueueTags" :: ByteString),
               "Version" =: ("2012-11-05" :: ByteString),
               "QueueUrl" =: _lqtQueueURL]

-- | /See:/ 'listQueueTagsResponse' smart constructor.
data ListQueueTagsResponse = ListQueueTagsResponse'
  { _lqtrsTags           :: !(Maybe (Map Text Text))
  , _lqtrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListQueueTagsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lqtrsTags' - The list of all tags added to the specified queue.
--
-- * 'lqtrsResponseStatus' - -- | The response status code.
listQueueTagsResponse
    :: Int -- ^ 'lqtrsResponseStatus'
    -> ListQueueTagsResponse
listQueueTagsResponse pResponseStatus_ =
  ListQueueTagsResponse'
    {_lqtrsTags = Nothing, _lqtrsResponseStatus = pResponseStatus_}


-- | The list of all tags added to the specified queue.
lqtrsTags :: Lens' ListQueueTagsResponse (HashMap Text Text)
lqtrsTags = lens _lqtrsTags (\ s a -> s{_lqtrsTags = a}) . _Default . _Map

-- | -- | The response status code.
lqtrsResponseStatus :: Lens' ListQueueTagsResponse Int
lqtrsResponseStatus = lens _lqtrsResponseStatus (\ s a -> s{_lqtrsResponseStatus = a})

instance NFData ListQueueTagsResponse where
