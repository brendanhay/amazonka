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
-- Module      : Network.AWS.DAX.ListTags
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all of the tags for a DAX cluster. You can call @ListTags@ up to 10 times per second, per account.
--
--
module Network.AWS.DAX.ListTags
    (
    -- * Creating a Request
      listTags
    , ListTags
    -- * Request Lenses
    , ltNextToken
    , ltResourceName

    -- * Destructuring the Response
    , listTagsResponse
    , ListTagsResponse
    -- * Response Lenses
    , ltrsNextToken
    , ltrsTags
    , ltrsResponseStatus
    ) where

import Network.AWS.DAX.Types
import Network.AWS.DAX.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listTags' smart constructor.
data ListTags = ListTags'
  { _ltNextToken    :: !(Maybe Text)
  , _ltResourceName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltNextToken' - An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token.
--
-- * 'ltResourceName' - The name of the DAX resource to which the tags belong.
listTags
    :: Text -- ^ 'ltResourceName'
    -> ListTags
listTags pResourceName_ =
  ListTags' {_ltNextToken = Nothing, _ltResourceName = pResourceName_}


-- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token.
ltNextToken :: Lens' ListTags (Maybe Text)
ltNextToken = lens _ltNextToken (\ s a -> s{_ltNextToken = a})

-- | The name of the DAX resource to which the tags belong.
ltResourceName :: Lens' ListTags Text
ltResourceName = lens _ltResourceName (\ s a -> s{_ltResourceName = a})

instance AWSRequest ListTags where
        type Rs ListTags = ListTagsResponse
        request = postJSON dax
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
                    ("AmazonDAXV3.ListTags" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListTags where
        toJSON ListTags'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _ltNextToken,
                  Just ("ResourceName" .= _ltResourceName)])

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
-- * 'ltrsNextToken' - If this value is present, there are additional results to be displayed. To retrieve them, call @ListTags@ again, with @NextToken@ set to this value.
--
-- * 'ltrsTags' - A list of tags currently associated with the DAX cluster.
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


-- | If this value is present, there are additional results to be displayed. To retrieve them, call @ListTags@ again, with @NextToken@ set to this value.
ltrsNextToken :: Lens' ListTagsResponse (Maybe Text)
ltrsNextToken = lens _ltrsNextToken (\ s a -> s{_ltrsNextToken = a})

-- | A list of tags currently associated with the DAX cluster.
ltrsTags :: Lens' ListTagsResponse [Tag]
ltrsTags = lens _ltrsTags (\ s a -> s{_ltrsTags = a}) . _Default . _Coerce

-- | -- | The response status code.
ltrsResponseStatus :: Lens' ListTagsResponse Int
ltrsResponseStatus = lens _ltrsResponseStatus (\ s a -> s{_ltrsResponseStatus = a})

instance NFData ListTagsResponse where
