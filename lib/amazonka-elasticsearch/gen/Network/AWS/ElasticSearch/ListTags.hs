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
-- Module      : Network.AWS.ElasticSearch.ListTags
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all tags for the given Elasticsearch domain.
--
--
module Network.AWS.ElasticSearch.ListTags
    (
    -- * Creating a Request
      listTags
    , ListTags
    -- * Request Lenses
    , ltARN

    -- * Destructuring the Response
    , listTagsResponse
    , ListTagsResponse
    -- * Response Lenses
    , ltrsTagList
    , ltrsResponseStatus
    ) where

import Network.AWS.ElasticSearch.Types
import Network.AWS.ElasticSearch.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the @'ListTags' @ operation. Specify the @ARN@ for the Elasticsearch domain to which the tags are attached that you want to view are attached.
--
--
--
-- /See:/ 'listTags' smart constructor.
newtype ListTags = ListTags'
  { _ltARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltARN' - Specify the @ARN@ for the Elasticsearch domain to which the tags are attached that you want to view.
listTags
    :: Text -- ^ 'ltARN'
    -> ListTags
listTags pARN_ = ListTags' {_ltARN = pARN_}


-- | Specify the @ARN@ for the Elasticsearch domain to which the tags are attached that you want to view.
ltARN :: Lens' ListTags Text
ltARN = lens _ltARN (\ s a -> s{_ltARN = a})

instance AWSRequest ListTags where
        type Rs ListTags = ListTagsResponse
        request = get elasticSearch
        response
          = receiveJSON
              (\ s h x ->
                 ListTagsResponse' <$>
                   (x .?> "TagList" .!@ mempty) <*> (pure (fromEnum s)))

instance Hashable ListTags where

instance NFData ListTags where

instance ToHeaders ListTags where
        toHeaders = const mempty

instance ToPath ListTags where
        toPath = const "/2015-01-01/tags/"

instance ToQuery ListTags where
        toQuery ListTags'{..} = mconcat ["arn" =: _ltARN]

-- | The result of a @ListTags@ operation. Contains tags for all requested Elasticsearch domains.
--
--
--
-- /See:/ 'listTagsResponse' smart constructor.
data ListTagsResponse = ListTagsResponse'
  { _ltrsTagList        :: !(Maybe [Tag])
  , _ltrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTagsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltrsTagList' - List of @Tag@ for the requested Elasticsearch domain.
--
-- * 'ltrsResponseStatus' - -- | The response status code.
listTagsResponse
    :: Int -- ^ 'ltrsResponseStatus'
    -> ListTagsResponse
listTagsResponse pResponseStatus_ =
  ListTagsResponse'
    {_ltrsTagList = Nothing, _ltrsResponseStatus = pResponseStatus_}


-- | List of @Tag@ for the requested Elasticsearch domain.
ltrsTagList :: Lens' ListTagsResponse [Tag]
ltrsTagList = lens _ltrsTagList (\ s a -> s{_ltrsTagList = a}) . _Default . _Coerce

-- | -- | The response status code.
ltrsResponseStatus :: Lens' ListTagsResponse Int
ltrsResponseStatus = lens _ltrsResponseStatus (\ s a -> s{_ltrsResponseStatus = a})

instance NFData ListTagsResponse where
