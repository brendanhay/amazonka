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
-- Module      : Network.AWS.KinesisVideo.ListTagsForStream
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of tags associated with the specified stream.
--
--
-- In the request, you must specify either the @StreamName@ or the @StreamARN@ .
--
module Network.AWS.KinesisVideo.ListTagsForStream
    (
    -- * Creating a Request
      listTagsForStream
    , ListTagsForStream
    -- * Request Lenses
    , ltfsStreamARN
    , ltfsNextToken
    , ltfsStreamName

    -- * Destructuring the Response
    , listTagsForStreamResponse
    , ListTagsForStreamResponse
    -- * Response Lenses
    , ltfsrsNextToken
    , ltfsrsTags
    , ltfsrsResponseStatus
    ) where

import Network.AWS.KinesisVideo.Types
import Network.AWS.KinesisVideo.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listTagsForStream' smart constructor.
data ListTagsForStream = ListTagsForStream'
  { _ltfsStreamARN  :: !(Maybe Text)
  , _ltfsNextToken  :: !(Maybe Text)
  , _ltfsStreamName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTagsForStream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltfsStreamARN' - The Amazon Resource Name (ARN) of the stream that you want to list tags for.
--
-- * 'ltfsNextToken' - If you specify this parameter and the result of a @ListTagsForStream@ call is truncated, the response includes a token that you can use in the next request to fetch the next batch of tags.
--
-- * 'ltfsStreamName' - The name of the stream that you want to list tags for.
listTagsForStream
    :: ListTagsForStream
listTagsForStream =
  ListTagsForStream'
    { _ltfsStreamARN = Nothing
    , _ltfsNextToken = Nothing
    , _ltfsStreamName = Nothing
    }


-- | The Amazon Resource Name (ARN) of the stream that you want to list tags for.
ltfsStreamARN :: Lens' ListTagsForStream (Maybe Text)
ltfsStreamARN = lens _ltfsStreamARN (\ s a -> s{_ltfsStreamARN = a})

-- | If you specify this parameter and the result of a @ListTagsForStream@ call is truncated, the response includes a token that you can use in the next request to fetch the next batch of tags.
ltfsNextToken :: Lens' ListTagsForStream (Maybe Text)
ltfsNextToken = lens _ltfsNextToken (\ s a -> s{_ltfsNextToken = a})

-- | The name of the stream that you want to list tags for.
ltfsStreamName :: Lens' ListTagsForStream (Maybe Text)
ltfsStreamName = lens _ltfsStreamName (\ s a -> s{_ltfsStreamName = a})

instance AWSRequest ListTagsForStream where
        type Rs ListTagsForStream = ListTagsForStreamResponse
        request = postJSON kinesisVideo
        response
          = receiveJSON
              (\ s h x ->
                 ListTagsForStreamResponse' <$>
                   (x .?> "NextToken") <*> (x .?> "Tags" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable ListTagsForStream where

instance NFData ListTagsForStream where

instance ToHeaders ListTagsForStream where
        toHeaders = const mempty

instance ToJSON ListTagsForStream where
        toJSON ListTagsForStream'{..}
          = object
              (catMaybes
                 [("StreamARN" .=) <$> _ltfsStreamARN,
                  ("NextToken" .=) <$> _ltfsNextToken,
                  ("StreamName" .=) <$> _ltfsStreamName])

instance ToPath ListTagsForStream where
        toPath = const "/listTagsForStream"

instance ToQuery ListTagsForStream where
        toQuery = const mempty

-- | /See:/ 'listTagsForStreamResponse' smart constructor.
data ListTagsForStreamResponse = ListTagsForStreamResponse'
  { _ltfsrsNextToken      :: !(Maybe Text)
  , _ltfsrsTags           :: !(Maybe (Map Text Text))
  , _ltfsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTagsForStreamResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltfsrsNextToken' - If you specify this parameter and the result of a @ListTags@ call is truncated, the response includes a token that you can use in the next request to fetch the next set of tags.
--
-- * 'ltfsrsTags' - A map of tag keys and values associated with the specified stream.
--
-- * 'ltfsrsResponseStatus' - -- | The response status code.
listTagsForStreamResponse
    :: Int -- ^ 'ltfsrsResponseStatus'
    -> ListTagsForStreamResponse
listTagsForStreamResponse pResponseStatus_ =
  ListTagsForStreamResponse'
    { _ltfsrsNextToken = Nothing
    , _ltfsrsTags = Nothing
    , _ltfsrsResponseStatus = pResponseStatus_
    }


-- | If you specify this parameter and the result of a @ListTags@ call is truncated, the response includes a token that you can use in the next request to fetch the next set of tags.
ltfsrsNextToken :: Lens' ListTagsForStreamResponse (Maybe Text)
ltfsrsNextToken = lens _ltfsrsNextToken (\ s a -> s{_ltfsrsNextToken = a})

-- | A map of tag keys and values associated with the specified stream.
ltfsrsTags :: Lens' ListTagsForStreamResponse (HashMap Text Text)
ltfsrsTags = lens _ltfsrsTags (\ s a -> s{_ltfsrsTags = a}) . _Default . _Map

-- | -- | The response status code.
ltfsrsResponseStatus :: Lens' ListTagsForStreamResponse Int
ltfsrsResponseStatus = lens _ltfsrsResponseStatus (\ s a -> s{_ltfsrsResponseStatus = a})

instance NFData ListTagsForStreamResponse where
