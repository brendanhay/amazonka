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
-- Module      : Network.AWS.ServiceCatalog.SearchProductsAsAdmin
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves summary and status information about all products created within the caller's account. If a portfolio ID is provided, this operation retrieves information for only those products that are associated with the specified portfolio.
--
--
module Network.AWS.ServiceCatalog.SearchProductsAsAdmin
    (
    -- * Creating a Request
      searchProductsAsAdmin
    , SearchProductsAsAdmin
    -- * Request Lenses
    , spaaPortfolioId
    , spaaFilters
    , spaaSortOrder
    , spaaAcceptLanguage
    , spaaPageToken
    , spaaPageSize
    , spaaProductSource
    , spaaSortBy

    -- * Destructuring the Response
    , searchProductsAsAdminResponse
    , SearchProductsAsAdminResponse
    -- * Response Lenses
    , spaarsNextPageToken
    , spaarsProductViewDetails
    , spaarsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'searchProductsAsAdmin' smart constructor.
data SearchProductsAsAdmin = SearchProductsAsAdmin'
  { _spaaPortfolioId    :: !(Maybe Text)
  , _spaaFilters        :: !(Maybe (Map ProductViewFilterBy [Text]))
  , _spaaSortOrder      :: !(Maybe SortOrder)
  , _spaaAcceptLanguage :: !(Maybe Text)
  , _spaaPageToken      :: !(Maybe Text)
  , _spaaPageSize       :: !(Maybe Nat)
  , _spaaProductSource  :: !(Maybe ProductSource)
  , _spaaSortBy         :: !(Maybe ProductViewSortBy)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SearchProductsAsAdmin' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spaaPortfolioId' - The portfolio identifier.
--
-- * 'spaaFilters' - The list of filters with which to limit search results. If no search filters are specified, the output is all the products to which the administrator has access.
--
-- * 'spaaSortOrder' - The sort order specifier. If no value is specified, results are not sorted.
--
-- * 'spaaAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'spaaPageToken' - The page token of the first page retrieved. If null, this retrieves the first page of size @PageSize@ .
--
-- * 'spaaPageSize' - The maximum number of items to return in the results. If more results exist than fit in the specified @PageSize@ , the value of @NextPageToken@ in the response is non-null.
--
-- * 'spaaProductSource' - Access level of the source of the product.
--
-- * 'spaaSortBy' - The sort field specifier. If no value is specified, results are not sorted.
searchProductsAsAdmin
    :: SearchProductsAsAdmin
searchProductsAsAdmin =
  SearchProductsAsAdmin'
  { _spaaPortfolioId = Nothing
  , _spaaFilters = Nothing
  , _spaaSortOrder = Nothing
  , _spaaAcceptLanguage = Nothing
  , _spaaPageToken = Nothing
  , _spaaPageSize = Nothing
  , _spaaProductSource = Nothing
  , _spaaSortBy = Nothing
  }


-- | The portfolio identifier.
spaaPortfolioId :: Lens' SearchProductsAsAdmin (Maybe Text)
spaaPortfolioId = lens _spaaPortfolioId (\ s a -> s{_spaaPortfolioId = a});

-- | The list of filters with which to limit search results. If no search filters are specified, the output is all the products to which the administrator has access.
spaaFilters :: Lens' SearchProductsAsAdmin (HashMap ProductViewFilterBy [Text])
spaaFilters = lens _spaaFilters (\ s a -> s{_spaaFilters = a}) . _Default . _Map;

-- | The sort order specifier. If no value is specified, results are not sorted.
spaaSortOrder :: Lens' SearchProductsAsAdmin (Maybe SortOrder)
spaaSortOrder = lens _spaaSortOrder (\ s a -> s{_spaaSortOrder = a});

-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
spaaAcceptLanguage :: Lens' SearchProductsAsAdmin (Maybe Text)
spaaAcceptLanguage = lens _spaaAcceptLanguage (\ s a -> s{_spaaAcceptLanguage = a});

-- | The page token of the first page retrieved. If null, this retrieves the first page of size @PageSize@ .
spaaPageToken :: Lens' SearchProductsAsAdmin (Maybe Text)
spaaPageToken = lens _spaaPageToken (\ s a -> s{_spaaPageToken = a});

-- | The maximum number of items to return in the results. If more results exist than fit in the specified @PageSize@ , the value of @NextPageToken@ in the response is non-null.
spaaPageSize :: Lens' SearchProductsAsAdmin (Maybe Natural)
spaaPageSize = lens _spaaPageSize (\ s a -> s{_spaaPageSize = a}) . mapping _Nat;

-- | Access level of the source of the product.
spaaProductSource :: Lens' SearchProductsAsAdmin (Maybe ProductSource)
spaaProductSource = lens _spaaProductSource (\ s a -> s{_spaaProductSource = a});

-- | The sort field specifier. If no value is specified, results are not sorted.
spaaSortBy :: Lens' SearchProductsAsAdmin (Maybe ProductViewSortBy)
spaaSortBy = lens _spaaSortBy (\ s a -> s{_spaaSortBy = a});

instance AWSRequest SearchProductsAsAdmin where
        type Rs SearchProductsAsAdmin =
             SearchProductsAsAdminResponse
        request = postJSON serviceCatalog
        response
          = receiveJSON
              (\ s h x ->
                 SearchProductsAsAdminResponse' <$>
                   (x .?> "NextPageToken") <*>
                     (x .?> "ProductViewDetails" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable SearchProductsAsAdmin where

instance NFData SearchProductsAsAdmin where

instance ToHeaders SearchProductsAsAdmin where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.SearchProductsAsAdmin"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON SearchProductsAsAdmin where
        toJSON SearchProductsAsAdmin'{..}
          = object
              (catMaybes
                 [("PortfolioId" .=) <$> _spaaPortfolioId,
                  ("Filters" .=) <$> _spaaFilters,
                  ("SortOrder" .=) <$> _spaaSortOrder,
                  ("AcceptLanguage" .=) <$> _spaaAcceptLanguage,
                  ("PageToken" .=) <$> _spaaPageToken,
                  ("PageSize" .=) <$> _spaaPageSize,
                  ("ProductSource" .=) <$> _spaaProductSource,
                  ("SortBy" .=) <$> _spaaSortBy])

instance ToPath SearchProductsAsAdmin where
        toPath = const "/"

instance ToQuery SearchProductsAsAdmin where
        toQuery = const mempty

-- | /See:/ 'searchProductsAsAdminResponse' smart constructor.
data SearchProductsAsAdminResponse = SearchProductsAsAdminResponse'
  { _spaarsNextPageToken      :: !(Maybe Text)
  , _spaarsProductViewDetails :: !(Maybe [ProductViewDetail])
  , _spaarsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SearchProductsAsAdminResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spaarsNextPageToken' - The page token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
--
-- * 'spaarsProductViewDetails' - List of detailed product view information objects.
--
-- * 'spaarsResponseStatus' - -- | The response status code.
searchProductsAsAdminResponse
    :: Int -- ^ 'spaarsResponseStatus'
    -> SearchProductsAsAdminResponse
searchProductsAsAdminResponse pResponseStatus_ =
  SearchProductsAsAdminResponse'
  { _spaarsNextPageToken = Nothing
  , _spaarsProductViewDetails = Nothing
  , _spaarsResponseStatus = pResponseStatus_
  }


-- | The page token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
spaarsNextPageToken :: Lens' SearchProductsAsAdminResponse (Maybe Text)
spaarsNextPageToken = lens _spaarsNextPageToken (\ s a -> s{_spaarsNextPageToken = a});

-- | List of detailed product view information objects.
spaarsProductViewDetails :: Lens' SearchProductsAsAdminResponse [ProductViewDetail]
spaarsProductViewDetails = lens _spaarsProductViewDetails (\ s a -> s{_spaarsProductViewDetails = a}) . _Default . _Coerce;

-- | -- | The response status code.
spaarsResponseStatus :: Lens' SearchProductsAsAdminResponse Int
spaarsResponseStatus = lens _spaarsResponseStatus (\ s a -> s{_spaarsResponseStatus = a});

instance NFData SearchProductsAsAdminResponse where
