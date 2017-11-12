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
-- Module      : Network.AWS.ServiceCatalog.ScanProvisionedProducts
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of all the ProvisionedProduct objects that are currently available (not terminated).
--
--
module Network.AWS.ServiceCatalog.ScanProvisionedProducts
    (
    -- * Creating a Request
      scanProvisionedProducts
    , ScanProvisionedProducts
    -- * Request Lenses
    , sppAcceptLanguage
    , sppAccessLevelFilter
    , sppPageToken
    , sppPageSize

    -- * Destructuring the Response
    , scanProvisionedProductsResponse
    , ScanProvisionedProductsResponse
    -- * Response Lenses
    , spprsNextPageToken
    , spprsProvisionedProducts
    , spprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'scanProvisionedProducts' smart constructor.
data ScanProvisionedProducts = ScanProvisionedProducts'
  { _sppAcceptLanguage    :: !(Maybe Text)
  , _sppAccessLevelFilter :: !(Maybe AccessLevelFilter)
  , _sppPageToken         :: !(Maybe Text)
  , _sppPageSize          :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ScanProvisionedProducts' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sppAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'sppAccessLevelFilter' - The access level for obtaining results. If left unspecified, @User@ level access is used.
--
-- * 'sppPageToken' - The page token of the first page retrieved. If null, this retrieves the first page of size @PageSize@ .
--
-- * 'sppPageSize' - The maximum number of items to return in the results. If more results exist than fit in the specified @PageSize@ , the value of @NextPageToken@ in the response is non-null.
scanProvisionedProducts
    :: ScanProvisionedProducts
scanProvisionedProducts =
  ScanProvisionedProducts'
  { _sppAcceptLanguage = Nothing
  , _sppAccessLevelFilter = Nothing
  , _sppPageToken = Nothing
  , _sppPageSize = Nothing
  }


-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
sppAcceptLanguage :: Lens' ScanProvisionedProducts (Maybe Text)
sppAcceptLanguage = lens _sppAcceptLanguage (\ s a -> s{_sppAcceptLanguage = a});

-- | The access level for obtaining results. If left unspecified, @User@ level access is used.
sppAccessLevelFilter :: Lens' ScanProvisionedProducts (Maybe AccessLevelFilter)
sppAccessLevelFilter = lens _sppAccessLevelFilter (\ s a -> s{_sppAccessLevelFilter = a});

-- | The page token of the first page retrieved. If null, this retrieves the first page of size @PageSize@ .
sppPageToken :: Lens' ScanProvisionedProducts (Maybe Text)
sppPageToken = lens _sppPageToken (\ s a -> s{_sppPageToken = a});

-- | The maximum number of items to return in the results. If more results exist than fit in the specified @PageSize@ , the value of @NextPageToken@ in the response is non-null.
sppPageSize :: Lens' ScanProvisionedProducts (Maybe Natural)
sppPageSize = lens _sppPageSize (\ s a -> s{_sppPageSize = a}) . mapping _Nat;

instance AWSRequest ScanProvisionedProducts where
        type Rs ScanProvisionedProducts =
             ScanProvisionedProductsResponse
        request = postJSON serviceCatalog
        response
          = receiveJSON
              (\ s h x ->
                 ScanProvisionedProductsResponse' <$>
                   (x .?> "NextPageToken") <*>
                     (x .?> "ProvisionedProducts" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ScanProvisionedProducts where

instance NFData ScanProvisionedProducts where

instance ToHeaders ScanProvisionedProducts where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.ScanProvisionedProducts"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ScanProvisionedProducts where
        toJSON ScanProvisionedProducts'{..}
          = object
              (catMaybes
                 [("AcceptLanguage" .=) <$> _sppAcceptLanguage,
                  ("AccessLevelFilter" .=) <$> _sppAccessLevelFilter,
                  ("PageToken" .=) <$> _sppPageToken,
                  ("PageSize" .=) <$> _sppPageSize])

instance ToPath ScanProvisionedProducts where
        toPath = const "/"

instance ToQuery ScanProvisionedProducts where
        toQuery = const mempty

-- | /See:/ 'scanProvisionedProductsResponse' smart constructor.
data ScanProvisionedProductsResponse = ScanProvisionedProductsResponse'
  { _spprsNextPageToken       :: !(Maybe Text)
  , _spprsProvisionedProducts :: !(Maybe [ProvisionedProductDetail])
  , _spprsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ScanProvisionedProductsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spprsNextPageToken' - The page token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
--
-- * 'spprsProvisionedProducts' - A list of ProvisionedProduct detail objects.
--
-- * 'spprsResponseStatus' - -- | The response status code.
scanProvisionedProductsResponse
    :: Int -- ^ 'spprsResponseStatus'
    -> ScanProvisionedProductsResponse
scanProvisionedProductsResponse pResponseStatus_ =
  ScanProvisionedProductsResponse'
  { _spprsNextPageToken = Nothing
  , _spprsProvisionedProducts = Nothing
  , _spprsResponseStatus = pResponseStatus_
  }


-- | The page token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
spprsNextPageToken :: Lens' ScanProvisionedProductsResponse (Maybe Text)
spprsNextPageToken = lens _spprsNextPageToken (\ s a -> s{_spprsNextPageToken = a});

-- | A list of ProvisionedProduct detail objects.
spprsProvisionedProducts :: Lens' ScanProvisionedProductsResponse [ProvisionedProductDetail]
spprsProvisionedProducts = lens _spprsProvisionedProducts (\ s a -> s{_spprsProvisionedProducts = a}) . _Default . _Coerce;

-- | -- | The response status code.
spprsResponseStatus :: Lens' ScanProvisionedProductsResponse Int
spprsResponseStatus = lens _spprsResponseStatus (\ s a -> s{_spprsResponseStatus = a});

instance NFData ScanProvisionedProductsResponse where
