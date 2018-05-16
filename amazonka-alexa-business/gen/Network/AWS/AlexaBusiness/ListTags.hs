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
-- Module      : Network.AWS.AlexaBusiness.ListTags
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all tags for a specific resource.
--
--
--
-- This operation returns paginated results.
module Network.AWS.AlexaBusiness.ListTags
    (
    -- * Creating a Request
      listTags
    , ListTags
    -- * Request Lenses
    , ltNextToken
    , ltMaxResults
    , ltARN

    -- * Destructuring the Response
    , listTagsResponse
    , ListTagsResponse
    -- * Response Lenses
    , ltrsNextToken
    , ltrsTags
    , ltrsResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listTags' smart constructor.
data ListTags = ListTags'
  { _ltNextToken  :: !(Maybe Text)
  , _ltMaxResults :: !(Maybe Nat)
  , _ltARN        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltNextToken' - An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
--
-- * 'ltMaxResults' - The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
--
-- * 'ltARN' - The ARN of the specific resource for which to list tags. Required.
listTags
    :: Text -- ^ 'ltARN'
    -> ListTags
listTags pARN_ =
  ListTags' {_ltNextToken = Nothing, _ltMaxResults = Nothing, _ltARN = pARN_}


-- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
ltNextToken :: Lens' ListTags (Maybe Text)
ltNextToken = lens _ltNextToken (\ s a -> s{_ltNextToken = a})

-- | The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
ltMaxResults :: Lens' ListTags (Maybe Natural)
ltMaxResults = lens _ltMaxResults (\ s a -> s{_ltMaxResults = a}) . mapping _Nat

-- | The ARN of the specific resource for which to list tags. Required.
ltARN :: Lens' ListTags Text
ltARN = lens _ltARN (\ s a -> s{_ltARN = a})

instance AWSPager ListTags where
        page rq rs
          | stop (rs ^. ltrsNextToken) = Nothing
          | stop (rs ^. ltrsTags) = Nothing
          | otherwise =
            Just $ rq & ltNextToken .~ rs ^. ltrsNextToken

instance AWSRequest ListTags where
        type Rs ListTags = ListTagsResponse
        request = postJSON alexaBusiness
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
                    ("AlexaForBusiness.ListTags" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListTags where
        toJSON ListTags'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _ltNextToken,
                  ("MaxResults" .=) <$> _ltMaxResults,
                  Just ("Arn" .= _ltARN)])

instance ToPath ListTags where
        toPath = const "/"

instance ToQuery ListTags where
        toQuery = const mempty

-- | /See:/ 'listTagsResponse' smart constructor.
data ListTagsResponse = ListTagsResponse'
  { _ltrsNextToken      :: !(Maybe Text)
  , _ltrsTags           :: !(Maybe [Tag])
  , _ltrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTagsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltrsNextToken' - The token returned to indicate that there is more data available.
--
-- * 'ltrsTags' - The list of tags requested for the specific resource.
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


-- | The token returned to indicate that there is more data available.
ltrsNextToken :: Lens' ListTagsResponse (Maybe Text)
ltrsNextToken = lens _ltrsNextToken (\ s a -> s{_ltrsNextToken = a})

-- | The list of tags requested for the specific resource.
ltrsTags :: Lens' ListTagsResponse [Tag]
ltrsTags = lens _ltrsTags (\ s a -> s{_ltrsTags = a}) . _Default . _Coerce

-- | -- | The response status code.
ltrsResponseStatus :: Lens' ListTagsResponse Int
ltrsResponseStatus = lens _ltrsResponseStatus (\ s a -> s{_ltrsResponseStatus = a})

instance NFData ListTagsResponse where
