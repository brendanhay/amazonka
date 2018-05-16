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
-- Module      : Network.AWS.ServiceCatalog.SearchProvisionedProducts
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the provisioned products that meet the specified criteria.
--
--
module Network.AWS.ServiceCatalog.SearchProvisionedProducts
    (
    -- * Creating a Request
      searchProvisionedProducts
    , SearchProvisionedProducts
    -- * Request Lenses
    , sppFilters
    , sppSortOrder
    , sppAcceptLanguage
    , sppAccessLevelFilter
    , sppPageToken
    , sppPageSize
    , sppSortBy

    -- * Destructuring the Response
    , searchProvisionedProductsResponse
    , SearchProvisionedProductsResponse
    -- * Response Lenses
    , srsNextPageToken
    , srsProvisionedProducts
    , srsTotalResultsCount
    , srsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'searchProvisionedProducts' smart constructor.
data SearchProvisionedProducts = SearchProvisionedProducts'
  { _sppFilters :: !(Maybe (Map ProvisionedProductViewFilterBy [Text]))
  , _sppSortOrder :: !(Maybe SortOrder)
  , _sppAcceptLanguage :: !(Maybe Text)
  , _sppAccessLevelFilter :: !(Maybe AccessLevelFilter)
  , _sppPageToken :: !(Maybe Text)
  , _sppPageSize :: !(Maybe Nat)
  , _sppSortBy :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SearchProvisionedProducts' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sppFilters' - The search filters. When the key is @SearchQuery@ , the searchable fields are @arn@ , @createdTime@ , @id@ , @lastRecordId@ , @idempotencyToken@ , @name@ , @physicalId@ , @productId@ , @provisioningArtifact@ , @type@ , @status@ , @tags@ , @userArn@ , and @userArnSession@ . Example: @"SearchQuery":["status:AVAILABLE"]@
--
-- * 'sppSortOrder' - The sort order. If no value is specified, the results are not sorted.
--
-- * 'sppAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'sppAccessLevelFilter' - The access level to use to obtain results. The default is @User@ .
--
-- * 'sppPageToken' - The page token for the next set of results. To retrieve the first set of results, use null.
--
-- * 'sppPageSize' - The maximum number of items to return with this call.
--
-- * 'sppSortBy' - The sort field. If no value is specified, the results are not sorted. The valid values are @arn@ , @id@ , @name@ , and @lastRecordId@ .
searchProvisionedProducts
    :: SearchProvisionedProducts
searchProvisionedProducts =
  SearchProvisionedProducts'
    { _sppFilters = Nothing
    , _sppSortOrder = Nothing
    , _sppAcceptLanguage = Nothing
    , _sppAccessLevelFilter = Nothing
    , _sppPageToken = Nothing
    , _sppPageSize = Nothing
    , _sppSortBy = Nothing
    }


-- | The search filters. When the key is @SearchQuery@ , the searchable fields are @arn@ , @createdTime@ , @id@ , @lastRecordId@ , @idempotencyToken@ , @name@ , @physicalId@ , @productId@ , @provisioningArtifact@ , @type@ , @status@ , @tags@ , @userArn@ , and @userArnSession@ . Example: @"SearchQuery":["status:AVAILABLE"]@
sppFilters :: Lens' SearchProvisionedProducts (HashMap ProvisionedProductViewFilterBy [Text])
sppFilters = lens _sppFilters (\ s a -> s{_sppFilters = a}) . _Default . _Map

-- | The sort order. If no value is specified, the results are not sorted.
sppSortOrder :: Lens' SearchProvisionedProducts (Maybe SortOrder)
sppSortOrder = lens _sppSortOrder (\ s a -> s{_sppSortOrder = a})

-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
sppAcceptLanguage :: Lens' SearchProvisionedProducts (Maybe Text)
sppAcceptLanguage = lens _sppAcceptLanguage (\ s a -> s{_sppAcceptLanguage = a})

-- | The access level to use to obtain results. The default is @User@ .
sppAccessLevelFilter :: Lens' SearchProvisionedProducts (Maybe AccessLevelFilter)
sppAccessLevelFilter = lens _sppAccessLevelFilter (\ s a -> s{_sppAccessLevelFilter = a})

-- | The page token for the next set of results. To retrieve the first set of results, use null.
sppPageToken :: Lens' SearchProvisionedProducts (Maybe Text)
sppPageToken = lens _sppPageToken (\ s a -> s{_sppPageToken = a})

-- | The maximum number of items to return with this call.
sppPageSize :: Lens' SearchProvisionedProducts (Maybe Natural)
sppPageSize = lens _sppPageSize (\ s a -> s{_sppPageSize = a}) . mapping _Nat

-- | The sort field. If no value is specified, the results are not sorted. The valid values are @arn@ , @id@ , @name@ , and @lastRecordId@ .
sppSortBy :: Lens' SearchProvisionedProducts (Maybe Text)
sppSortBy = lens _sppSortBy (\ s a -> s{_sppSortBy = a})

instance AWSRequest SearchProvisionedProducts where
        type Rs SearchProvisionedProducts =
             SearchProvisionedProductsResponse
        request = postJSON serviceCatalog
        response
          = receiveJSON
              (\ s h x ->
                 SearchProvisionedProductsResponse' <$>
                   (x .?> "NextPageToken") <*>
                     (x .?> "ProvisionedProducts" .!@ mempty)
                     <*> (x .?> "TotalResultsCount")
                     <*> (pure (fromEnum s)))

instance Hashable SearchProvisionedProducts where

instance NFData SearchProvisionedProducts where

instance ToHeaders SearchProvisionedProducts where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.SearchProvisionedProducts"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON SearchProvisionedProducts where
        toJSON SearchProvisionedProducts'{..}
          = object
              (catMaybes
                 [("Filters" .=) <$> _sppFilters,
                  ("SortOrder" .=) <$> _sppSortOrder,
                  ("AcceptLanguage" .=) <$> _sppAcceptLanguage,
                  ("AccessLevelFilter" .=) <$> _sppAccessLevelFilter,
                  ("PageToken" .=) <$> _sppPageToken,
                  ("PageSize" .=) <$> _sppPageSize,
                  ("SortBy" .=) <$> _sppSortBy])

instance ToPath SearchProvisionedProducts where
        toPath = const "/"

instance ToQuery SearchProvisionedProducts where
        toQuery = const mempty

-- | /See:/ 'searchProvisionedProductsResponse' smart constructor.
data SearchProvisionedProductsResponse = SearchProvisionedProductsResponse'
  { _srsNextPageToken       :: !(Maybe Text)
  , _srsProvisionedProducts :: !(Maybe [ProvisionedProductAttribute])
  , _srsTotalResultsCount   :: !(Maybe Int)
  , _srsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SearchProvisionedProductsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srsNextPageToken' - The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- * 'srsProvisionedProducts' - Information about the provisioned products.
--
-- * 'srsTotalResultsCount' - The number of provisioned products found.
--
-- * 'srsResponseStatus' - -- | The response status code.
searchProvisionedProductsResponse
    :: Int -- ^ 'srsResponseStatus'
    -> SearchProvisionedProductsResponse
searchProvisionedProductsResponse pResponseStatus_ =
  SearchProvisionedProductsResponse'
    { _srsNextPageToken = Nothing
    , _srsProvisionedProducts = Nothing
    , _srsTotalResultsCount = Nothing
    , _srsResponseStatus = pResponseStatus_
    }


-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
srsNextPageToken :: Lens' SearchProvisionedProductsResponse (Maybe Text)
srsNextPageToken = lens _srsNextPageToken (\ s a -> s{_srsNextPageToken = a})

-- | Information about the provisioned products.
srsProvisionedProducts :: Lens' SearchProvisionedProductsResponse [ProvisionedProductAttribute]
srsProvisionedProducts = lens _srsProvisionedProducts (\ s a -> s{_srsProvisionedProducts = a}) . _Default . _Coerce

-- | The number of provisioned products found.
srsTotalResultsCount :: Lens' SearchProvisionedProductsResponse (Maybe Int)
srsTotalResultsCount = lens _srsTotalResultsCount (\ s a -> s{_srsTotalResultsCount = a})

-- | -- | The response status code.
srsResponseStatus :: Lens' SearchProvisionedProductsResponse Int
srsResponseStatus = lens _srsResponseStatus (\ s a -> s{_srsResponseStatus = a})

instance NFData SearchProvisionedProductsResponse
         where
