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
-- Module      : Network.AWS.SageMaker.Search
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Finds Amazon SageMaker resources that match a search query. Matching resource objects are returned as a list of @SearchResult@ objects in the response. You can sort the search results by any resource property in a ascending or descending order.
--
--
-- You can query against the following value types: numerical, text, Booleans, and timestamps.
--
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.Search
    (
    -- * Creating a Request
      search
    , Search
    -- * Request Lenses
    , sNextToken
    , sSearchExpression
    , sSortOrder
    , sMaxResults
    , sSortBy
    , sResource

    -- * Destructuring the Response
    , searchResponse
    , SearchResponse
    -- * Response Lenses
    , srsResults
    , srsNextToken
    , srsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'search' smart constructor.
data Search = Search'
  { _sNextToken        :: !(Maybe Text)
  , _sSearchExpression :: !(Maybe SearchExpression)
  , _sSortOrder        :: !(Maybe SearchSortOrder)
  , _sMaxResults       :: !(Maybe Nat)
  , _sSortBy           :: !(Maybe Text)
  , _sResource         :: !ResourceType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Search' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sNextToken' - If more than @MaxResults@ resource objects match the specified @SearchExpression@ , the @SearchResponse@ includes a @NextToken@ . The @NextToken@ can be passed to the next @SearchRequest@ to continue retrieving results for the specified @SearchExpression@ and @Sort@ parameters.
--
-- * 'sSearchExpression' - A Boolean conditional statement. Resource objects must satisfy this condition to be included in search results. You must provide at least one subexpression, filter, or nested filter. The maximum number of recursive @SubExpressions@ , @NestedFilters@ , and @Filters@ that can be included in a @SearchExpression@ object is 50.
--
-- * 'sSortOrder' - How @SearchResults@ are ordered. Valid values are @Ascending@ or @Descending@ . The default is @Descending@ .
--
-- * 'sMaxResults' - The maximum number of results to return in a @SearchResponse@ .
--
-- * 'sSortBy' - The name of the resource property used to sort the @SearchResults@ . The default is @LastModifiedTime@ .
--
-- * 'sResource' - The name of the Amazon SageMaker resource to search for. Currently, the only valid @Resource@ value is @TrainingJob@ .
search
    :: ResourceType -- ^ 'sResource'
    -> Search
search pResource_ =
  Search'
    { _sNextToken = Nothing
    , _sSearchExpression = Nothing
    , _sSortOrder = Nothing
    , _sMaxResults = Nothing
    , _sSortBy = Nothing
    , _sResource = pResource_
    }


-- | If more than @MaxResults@ resource objects match the specified @SearchExpression@ , the @SearchResponse@ includes a @NextToken@ . The @NextToken@ can be passed to the next @SearchRequest@ to continue retrieving results for the specified @SearchExpression@ and @Sort@ parameters.
sNextToken :: Lens' Search (Maybe Text)
sNextToken = lens _sNextToken (\ s a -> s{_sNextToken = a})

-- | A Boolean conditional statement. Resource objects must satisfy this condition to be included in search results. You must provide at least one subexpression, filter, or nested filter. The maximum number of recursive @SubExpressions@ , @NestedFilters@ , and @Filters@ that can be included in a @SearchExpression@ object is 50.
sSearchExpression :: Lens' Search (Maybe SearchExpression)
sSearchExpression = lens _sSearchExpression (\ s a -> s{_sSearchExpression = a})

-- | How @SearchResults@ are ordered. Valid values are @Ascending@ or @Descending@ . The default is @Descending@ .
sSortOrder :: Lens' Search (Maybe SearchSortOrder)
sSortOrder = lens _sSortOrder (\ s a -> s{_sSortOrder = a})

-- | The maximum number of results to return in a @SearchResponse@ .
sMaxResults :: Lens' Search (Maybe Natural)
sMaxResults = lens _sMaxResults (\ s a -> s{_sMaxResults = a}) . mapping _Nat

-- | The name of the resource property used to sort the @SearchResults@ . The default is @LastModifiedTime@ .
sSortBy :: Lens' Search (Maybe Text)
sSortBy = lens _sSortBy (\ s a -> s{_sSortBy = a})

-- | The name of the Amazon SageMaker resource to search for. Currently, the only valid @Resource@ value is @TrainingJob@ .
sResource :: Lens' Search ResourceType
sResource = lens _sResource (\ s a -> s{_sResource = a})

instance AWSPager Search where
        page rq rs
          | stop (rs ^. srsNextToken) = Nothing
          | stop (rs ^. srsResults) = Nothing
          | otherwise =
            Just $ rq & sNextToken .~ rs ^. srsNextToken

instance AWSRequest Search where
        type Rs Search = SearchResponse
        request = postJSON sageMaker
        response
          = receiveJSON
              (\ s h x ->
                 SearchResponse' <$>
                   (x .?> "Results" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable Search where

instance NFData Search where

instance ToHeaders Search where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.Search" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON Search where
        toJSON Search'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _sNextToken,
                  ("SearchExpression" .=) <$> _sSearchExpression,
                  ("SortOrder" .=) <$> _sSortOrder,
                  ("MaxResults" .=) <$> _sMaxResults,
                  ("SortBy" .=) <$> _sSortBy,
                  Just ("Resource" .= _sResource)])

instance ToPath Search where
        toPath = const "/"

instance ToQuery Search where
        toQuery = const mempty

-- | /See:/ 'searchResponse' smart constructor.
data SearchResponse = SearchResponse'
  { _srsResults        :: !(Maybe [SearchRecord])
  , _srsNextToken      :: !(Maybe Text)
  , _srsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SearchResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srsResults' - A list of @SearchResult@ objects.
--
-- * 'srsNextToken' - If the result of the previous @Search@ request was truncated, the response includes a NextToken. To retrieve the next set of results, use the token in the next request.
--
-- * 'srsResponseStatus' - -- | The response status code.
searchResponse
    :: Int -- ^ 'srsResponseStatus'
    -> SearchResponse
searchResponse pResponseStatus_ =
  SearchResponse'
    { _srsResults = Nothing
    , _srsNextToken = Nothing
    , _srsResponseStatus = pResponseStatus_
    }


-- | A list of @SearchResult@ objects.
srsResults :: Lens' SearchResponse [SearchRecord]
srsResults = lens _srsResults (\ s a -> s{_srsResults = a}) . _Default . _Coerce

-- | If the result of the previous @Search@ request was truncated, the response includes a NextToken. To retrieve the next set of results, use the token in the next request.
srsNextToken :: Lens' SearchResponse (Maybe Text)
srsNextToken = lens _srsNextToken (\ s a -> s{_srsNextToken = a})

-- | -- | The response status code.
srsResponseStatus :: Lens' SearchResponse Int
srsResponseStatus = lens _srsResponseStatus (\ s a -> s{_srsResponseStatus = a})

instance NFData SearchResponse where
