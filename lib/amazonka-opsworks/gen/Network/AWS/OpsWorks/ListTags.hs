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
-- Module      : Network.AWS.OpsWorks.ListTags
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of tags that are applied to the specified stack or layer.
--
--
module Network.AWS.OpsWorks.ListTags
    (
    -- * Creating a Request
      listTags
    , ListTags
    -- * Request Lenses
    , ltNextToken
    , ltMaxResults
    , ltResourceARN

    -- * Destructuring the Response
    , listTagsResponse
    , ListTagsResponse
    -- * Response Lenses
    , ltrsNextToken
    , ltrsTags
    , ltrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listTags' smart constructor.
data ListTags = ListTags'
  { _ltNextToken   :: !(Maybe Text)
  , _ltMaxResults  :: !(Maybe Int)
  , _ltResourceARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltNextToken' - Do not use. A validation exception occurs if you add a @NextToken@ parameter to a @ListTagsRequest@ call.
--
-- * 'ltMaxResults' - Do not use. A validation exception occurs if you add a @MaxResults@ parameter to a @ListTagsRequest@ call.
--
-- * 'ltResourceARN' - The stack or layer's Amazon Resource Number (ARN).
listTags
    :: Text -- ^ 'ltResourceARN'
    -> ListTags
listTags pResourceARN_ =
  ListTags'
    { _ltNextToken = Nothing
    , _ltMaxResults = Nothing
    , _ltResourceARN = pResourceARN_
    }


-- | Do not use. A validation exception occurs if you add a @NextToken@ parameter to a @ListTagsRequest@ call.
ltNextToken :: Lens' ListTags (Maybe Text)
ltNextToken = lens _ltNextToken (\ s a -> s{_ltNextToken = a})

-- | Do not use. A validation exception occurs if you add a @MaxResults@ parameter to a @ListTagsRequest@ call.
ltMaxResults :: Lens' ListTags (Maybe Int)
ltMaxResults = lens _ltMaxResults (\ s a -> s{_ltMaxResults = a})

-- | The stack or layer's Amazon Resource Number (ARN).
ltResourceARN :: Lens' ListTags Text
ltResourceARN = lens _ltResourceARN (\ s a -> s{_ltResourceARN = a})

instance AWSRequest ListTags where
        type Rs ListTags = ListTagsResponse
        request = postJSON opsWorks
        response
          = receiveJSON
              (\ s h x ->
                 ListTagsResponse' <$>
                   (x .?> "NextToken") <*> (x .?> "Tags" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable ListTags where

instance NFData ListTags where

instance ToHeaders ListTags where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.ListTags" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListTags where
        toJSON ListTags'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _ltNextToken,
                  ("MaxResults" .=) <$> _ltMaxResults,
                  Just ("ResourceArn" .= _ltResourceARN)])

instance ToPath ListTags where
        toPath = const "/"

instance ToQuery ListTags where
        toQuery = const mempty

-- | Contains the response to a @ListTags@ request.
--
--
--
-- /See:/ 'listTagsResponse' smart constructor.
data ListTagsResponse = ListTagsResponse'
  { _ltrsNextToken      :: !(Maybe Text)
  , _ltrsTags           :: !(Maybe (Map Text Text))
  , _ltrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTagsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltrsNextToken' - If a paginated request does not return all of the remaining results, this parameter is set to a token that you can assign to the request object's @NextToken@ parameter to get the next set of results. If the previous paginated request returned all of the remaining results, this parameter is set to @null@ .
--
-- * 'ltrsTags' - A set of key-value pairs that contain tag keys and tag values that are attached to a stack or layer.
--
-- * 'ltrsResponseStatus' - -- | The response status code.
listTagsResponse
    :: Int -- ^ 'ltrsResponseStatus'
    -> ListTagsResponse
listTagsResponse pResponseStatus_ =
  ListTagsResponse'
    { _ltrsNextToken = Nothing
    , _ltrsTags = Nothing
    , _ltrsResponseStatus = pResponseStatus_
    }


-- | If a paginated request does not return all of the remaining results, this parameter is set to a token that you can assign to the request object's @NextToken@ parameter to get the next set of results. If the previous paginated request returned all of the remaining results, this parameter is set to @null@ .
ltrsNextToken :: Lens' ListTagsResponse (Maybe Text)
ltrsNextToken = lens _ltrsNextToken (\ s a -> s{_ltrsNextToken = a})

-- | A set of key-value pairs that contain tag keys and tag values that are attached to a stack or layer.
ltrsTags :: Lens' ListTagsResponse (HashMap Text Text)
ltrsTags = lens _ltrsTags (\ s a -> s{_ltrsTags = a}) . _Default . _Map

-- | -- | The response status code.
ltrsResponseStatus :: Lens' ListTagsResponse Int
ltrsResponseStatus = lens _ltrsResponseStatus (\ s a -> s{_ltrsResponseStatus = a})

instance NFData ListTagsResponse where
