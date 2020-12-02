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
-- Module      : Network.AWS.Lambda.ListTags
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of tags assigned to a function when supplied the function ARN (Amazon Resource Name). For more information on Tagging, see <http://docs.aws.amazon.com/lambda/latest/dg/tagging.html Tagging Lambda Functions> in the __AWS Lambda Developer Guide__ .
--
--
module Network.AWS.Lambda.ListTags
    (
    -- * Creating a Request
      listTags
    , ListTags
    -- * Request Lenses
    , ltResource

    -- * Destructuring the Response
    , listTagsResponse
    , ListTagsResponse
    -- * Response Lenses
    , ltrsTags
    , ltrsResponseStatus
    ) where

import Network.AWS.Lambda.Types
import Network.AWS.Lambda.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listTags' smart constructor.
newtype ListTags = ListTags'
  { _ltResource :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltResource' - The ARN (Amazon Resource Name) of the function. For more information, see <http://docs.aws.amazon.com/lambda/latest/dg/tagging.html Tagging Lambda Functions> in the __AWS Lambda Developer Guide__ .
listTags
    :: Text -- ^ 'ltResource'
    -> ListTags
listTags pResource_ = ListTags' {_ltResource = pResource_}


-- | The ARN (Amazon Resource Name) of the function. For more information, see <http://docs.aws.amazon.com/lambda/latest/dg/tagging.html Tagging Lambda Functions> in the __AWS Lambda Developer Guide__ .
ltResource :: Lens' ListTags Text
ltResource = lens _ltResource (\ s a -> s{_ltResource = a})

instance AWSRequest ListTags where
        type Rs ListTags = ListTagsResponse
        request = get lambda
        response
          = receiveJSON
              (\ s h x ->
                 ListTagsResponse' <$>
                   (x .?> "Tags" .!@ mempty) <*> (pure (fromEnum s)))

instance Hashable ListTags where

instance NFData ListTags where

instance ToHeaders ListTags where
        toHeaders = const mempty

instance ToPath ListTags where
        toPath ListTags'{..}
          = mconcat ["/2017-03-31/tags/", toBS _ltResource]

instance ToQuery ListTags where
        toQuery = const mempty

-- | /See:/ 'listTagsResponse' smart constructor.
data ListTagsResponse = ListTagsResponse'
  { _ltrsTags           :: !(Maybe (Map Text Text))
  , _ltrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTagsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltrsTags' - The list of tags assigned to the function. For more information, see <http://docs.aws.amazon.com/lambda/latest/dg/tagging.html Tagging Lambda Functions> in the __AWS Lambda Developer Guide__ .
--
-- * 'ltrsResponseStatus' - -- | The response status code.
listTagsResponse
    :: Int -- ^ 'ltrsResponseStatus'
    -> ListTagsResponse
listTagsResponse pResponseStatus_ =
  ListTagsResponse'
    {_ltrsTags = Nothing, _ltrsResponseStatus = pResponseStatus_}


-- | The list of tags assigned to the function. For more information, see <http://docs.aws.amazon.com/lambda/latest/dg/tagging.html Tagging Lambda Functions> in the __AWS Lambda Developer Guide__ .
ltrsTags :: Lens' ListTagsResponse (HashMap Text Text)
ltrsTags = lens _ltrsTags (\ s a -> s{_ltrsTags = a}) . _Default . _Map

-- | -- | The response status code.
ltrsResponseStatus :: Lens' ListTagsResponse Int
ltrsResponseStatus = lens _ltrsResponseStatus (\ s a -> s{_ltrsResponseStatus = a})

instance NFData ListTagsResponse where
