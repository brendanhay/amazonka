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
-- Module      : Network.AWS.ElasticSearch.ListElasticsearchVersions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all supported Elasticsearch versions
--
--
--
-- This operation returns paginated results.
module Network.AWS.ElasticSearch.ListElasticsearchVersions
    (
    -- * Creating a Request
      listElasticsearchVersions
    , ListElasticsearchVersions
    -- * Request Lenses
    , levNextToken
    , levMaxResults

    -- * Destructuring the Response
    , listElasticsearchVersionsResponse
    , ListElasticsearchVersionsResponse
    -- * Response Lenses
    , levrsNextToken
    , levrsElasticsearchVersions
    , levrsResponseStatus
    ) where

import Network.AWS.ElasticSearch.Types
import Network.AWS.ElasticSearch.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the @'ListElasticsearchVersions' @ operation. Use @'MaxResults' @ to control the maximum number of results to retrieve in a single call.
--
--
-- Use @'NextToken' @ in response to retrieve more results. If the received response does not contain a NextToken, then there are no more results to retrieve.
--
--
--
--
-- /See:/ 'listElasticsearchVersions' smart constructor.
data ListElasticsearchVersions = ListElasticsearchVersions'
  { _levNextToken  :: !(Maybe Text)
  , _levMaxResults :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListElasticsearchVersions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'levNextToken' - Undocumented member.
--
-- * 'levMaxResults' - Set this value to limit the number of results returned. Value provided must be greater than 10 else it wont be honored.
listElasticsearchVersions
    :: ListElasticsearchVersions
listElasticsearchVersions =
  ListElasticsearchVersions' {_levNextToken = Nothing, _levMaxResults = Nothing}


-- | Undocumented member.
levNextToken :: Lens' ListElasticsearchVersions (Maybe Text)
levNextToken = lens _levNextToken (\ s a -> s{_levNextToken = a})

-- | Set this value to limit the number of results returned. Value provided must be greater than 10 else it wont be honored.
levMaxResults :: Lens' ListElasticsearchVersions (Maybe Int)
levMaxResults = lens _levMaxResults (\ s a -> s{_levMaxResults = a})

instance AWSPager ListElasticsearchVersions where
        page rq rs
          | stop (rs ^. levrsNextToken) = Nothing
          | stop (rs ^. levrsElasticsearchVersions) = Nothing
          | otherwise =
            Just $ rq & levNextToken .~ rs ^. levrsNextToken

instance AWSRequest ListElasticsearchVersions where
        type Rs ListElasticsearchVersions =
             ListElasticsearchVersionsResponse
        request = get elasticSearch
        response
          = receiveJSON
              (\ s h x ->
                 ListElasticsearchVersionsResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "ElasticsearchVersions" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListElasticsearchVersions where

instance NFData ListElasticsearchVersions where

instance ToHeaders ListElasticsearchVersions where
        toHeaders = const mempty

instance ToPath ListElasticsearchVersions where
        toPath = const "/2015-01-01/es/versions"

instance ToQuery ListElasticsearchVersions where
        toQuery ListElasticsearchVersions'{..}
          = mconcat
              ["nextToken" =: _levNextToken,
               "maxResults" =: _levMaxResults]

-- | Container for the parameters for response received from @'ListElasticsearchVersions' @ operation.
--
--
--
-- /See:/ 'listElasticsearchVersionsResponse' smart constructor.
data ListElasticsearchVersionsResponse = ListElasticsearchVersionsResponse'
  { _levrsNextToken             :: !(Maybe Text)
  , _levrsElasticsearchVersions :: !(Maybe [Text])
  , _levrsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListElasticsearchVersionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'levrsNextToken' - Undocumented member.
--
-- * 'levrsElasticsearchVersions' - Undocumented member.
--
-- * 'levrsResponseStatus' - -- | The response status code.
listElasticsearchVersionsResponse
    :: Int -- ^ 'levrsResponseStatus'
    -> ListElasticsearchVersionsResponse
listElasticsearchVersionsResponse pResponseStatus_ =
  ListElasticsearchVersionsResponse'
    { _levrsNextToken = Nothing
    , _levrsElasticsearchVersions = Nothing
    , _levrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
levrsNextToken :: Lens' ListElasticsearchVersionsResponse (Maybe Text)
levrsNextToken = lens _levrsNextToken (\ s a -> s{_levrsNextToken = a})

-- | Undocumented member.
levrsElasticsearchVersions :: Lens' ListElasticsearchVersionsResponse [Text]
levrsElasticsearchVersions = lens _levrsElasticsearchVersions (\ s a -> s{_levrsElasticsearchVersions = a}) . _Default . _Coerce

-- | -- | The response status code.
levrsResponseStatus :: Lens' ListElasticsearchVersionsResponse Int
levrsResponseStatus = lens _levrsResponseStatus (\ s a -> s{_levrsResponseStatus = a})

instance NFData ListElasticsearchVersionsResponse
         where
