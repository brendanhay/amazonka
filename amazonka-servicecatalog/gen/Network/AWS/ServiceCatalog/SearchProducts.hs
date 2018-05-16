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
-- Module      : Network.AWS.ServiceCatalog.SearchProducts
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the products to which the caller has access.
--
--
module Network.AWS.ServiceCatalog.SearchProducts
    (
    -- * Creating a Request
      searchProducts
    , SearchProducts
    -- * Request Lenses
    , spFilters
    , spSortOrder
    , spAcceptLanguage
    , spPageToken
    , spPageSize
    , spSortBy

    -- * Destructuring the Response
    , searchProductsResponse
    , SearchProductsResponse
    -- * Response Lenses
    , sprsNextPageToken
    , sprsProductViewAggregations
    , sprsProductViewSummaries
    , sprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'searchProducts' smart constructor.
data SearchProducts = SearchProducts'
  { _spFilters        :: !(Maybe (Map ProductViewFilterBy [Text]))
  , _spSortOrder      :: !(Maybe SortOrder)
  , _spAcceptLanguage :: !(Maybe Text)
  , _spPageToken      :: !(Maybe Text)
  , _spPageSize       :: !(Maybe Nat)
  , _spSortBy         :: !(Maybe ProductViewSortBy)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SearchProducts' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spFilters' - The search filters. If no search filters are specified, the output includes all products to which the caller has access.
--
-- * 'spSortOrder' - The sort order. If no value is specified, the results are not sorted.
--
-- * 'spAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'spPageToken' - The page token for the next set of results. To retrieve the first set of results, use null.
--
-- * 'spPageSize' - The maximum number of items to return with this call.
--
-- * 'spSortBy' - The sort field. If no value is specified, the results are not sorted.
searchProducts
    :: SearchProducts
searchProducts =
  SearchProducts'
    { _spFilters = Nothing
    , _spSortOrder = Nothing
    , _spAcceptLanguage = Nothing
    , _spPageToken = Nothing
    , _spPageSize = Nothing
    , _spSortBy = Nothing
    }


-- | The search filters. If no search filters are specified, the output includes all products to which the caller has access.
spFilters :: Lens' SearchProducts (HashMap ProductViewFilterBy [Text])
spFilters = lens _spFilters (\ s a -> s{_spFilters = a}) . _Default . _Map

-- | The sort order. If no value is specified, the results are not sorted.
spSortOrder :: Lens' SearchProducts (Maybe SortOrder)
spSortOrder = lens _spSortOrder (\ s a -> s{_spSortOrder = a})

-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
spAcceptLanguage :: Lens' SearchProducts (Maybe Text)
spAcceptLanguage = lens _spAcceptLanguage (\ s a -> s{_spAcceptLanguage = a})

-- | The page token for the next set of results. To retrieve the first set of results, use null.
spPageToken :: Lens' SearchProducts (Maybe Text)
spPageToken = lens _spPageToken (\ s a -> s{_spPageToken = a})

-- | The maximum number of items to return with this call.
spPageSize :: Lens' SearchProducts (Maybe Natural)
spPageSize = lens _spPageSize (\ s a -> s{_spPageSize = a}) . mapping _Nat

-- | The sort field. If no value is specified, the results are not sorted.
spSortBy :: Lens' SearchProducts (Maybe ProductViewSortBy)
spSortBy = lens _spSortBy (\ s a -> s{_spSortBy = a})

instance AWSRequest SearchProducts where
        type Rs SearchProducts = SearchProductsResponse
        request = postJSON serviceCatalog
        response
          = receiveJSON
              (\ s h x ->
                 SearchProductsResponse' <$>
                   (x .?> "NextPageToken") <*>
                     (x .?> "ProductViewAggregations" .!@ mempty)
                     <*> (x .?> "ProductViewSummaries" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable SearchProducts where

instance NFData SearchProducts where

instance ToHeaders SearchProducts where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.SearchProducts" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON SearchProducts where
        toJSON SearchProducts'{..}
          = object
              (catMaybes
                 [("Filters" .=) <$> _spFilters,
                  ("SortOrder" .=) <$> _spSortOrder,
                  ("AcceptLanguage" .=) <$> _spAcceptLanguage,
                  ("PageToken" .=) <$> _spPageToken,
                  ("PageSize" .=) <$> _spPageSize,
                  ("SortBy" .=) <$> _spSortBy])

instance ToPath SearchProducts where
        toPath = const "/"

instance ToQuery SearchProducts where
        toQuery = const mempty

-- | /See:/ 'searchProductsResponse' smart constructor.
data SearchProductsResponse = SearchProductsResponse'
  { _sprsNextPageToken :: !(Maybe Text)
  , _sprsProductViewAggregations :: !(Maybe (Map Text [ProductViewAggregationValue]))
  , _sprsProductViewSummaries :: !(Maybe [ProductViewSummary])
  , _sprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SearchProductsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sprsNextPageToken' - The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- * 'sprsProductViewAggregations' - The product view aggregations.
--
-- * 'sprsProductViewSummaries' - Information about the product views.
--
-- * 'sprsResponseStatus' - -- | The response status code.
searchProductsResponse
    :: Int -- ^ 'sprsResponseStatus'
    -> SearchProductsResponse
searchProductsResponse pResponseStatus_ =
  SearchProductsResponse'
    { _sprsNextPageToken = Nothing
    , _sprsProductViewAggregations = Nothing
    , _sprsProductViewSummaries = Nothing
    , _sprsResponseStatus = pResponseStatus_
    }


-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
sprsNextPageToken :: Lens' SearchProductsResponse (Maybe Text)
sprsNextPageToken = lens _sprsNextPageToken (\ s a -> s{_sprsNextPageToken = a})

-- | The product view aggregations.
sprsProductViewAggregations :: Lens' SearchProductsResponse (HashMap Text [ProductViewAggregationValue])
sprsProductViewAggregations = lens _sprsProductViewAggregations (\ s a -> s{_sprsProductViewAggregations = a}) . _Default . _Map

-- | Information about the product views.
sprsProductViewSummaries :: Lens' SearchProductsResponse [ProductViewSummary]
sprsProductViewSummaries = lens _sprsProductViewSummaries (\ s a -> s{_sprsProductViewSummaries = a}) . _Default . _Coerce

-- | -- | The response status code.
sprsResponseStatus :: Lens' SearchProductsResponse Int
sprsResponseStatus = lens _sprsResponseStatus (\ s a -> s{_sprsResponseStatus = a})

instance NFData SearchProductsResponse where
