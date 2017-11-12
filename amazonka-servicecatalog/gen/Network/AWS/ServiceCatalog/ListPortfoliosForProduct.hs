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
-- Module      : Network.AWS.ServiceCatalog.ListPortfoliosForProduct
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all portfolios that the specified product is associated with.
--
--
module Network.AWS.ServiceCatalog.ListPortfoliosForProduct
    (
    -- * Creating a Request
      listPortfoliosForProduct
    , ListPortfoliosForProduct
    -- * Request Lenses
    , lisAcceptLanguage
    , lisPageToken
    , lisPageSize
    , lisProductId

    -- * Destructuring the Response
    , listPortfoliosForProductResponse
    , ListPortfoliosForProductResponse
    -- * Response Lenses
    , lpfprsNextPageToken
    , lpfprsPortfolioDetails
    , lpfprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'listPortfoliosForProduct' smart constructor.
data ListPortfoliosForProduct = ListPortfoliosForProduct'
  { _lisAcceptLanguage :: !(Maybe Text)
  , _lisPageToken      :: !(Maybe Text)
  , _lisPageSize       :: !(Maybe Nat)
  , _lisProductId      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPortfoliosForProduct' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lisAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'lisPageToken' - The page token of the first page retrieved. If null, this retrieves the first page of size @PageSize@ .
--
-- * 'lisPageSize' - The maximum number of items to return in the results. If more results exist than fit in the specified @PageSize@ , the value of @NextPageToken@ in the response is non-null.
--
-- * 'lisProductId' - The product identifier.
listPortfoliosForProduct
    :: Text -- ^ 'lisProductId'
    -> ListPortfoliosForProduct
listPortfoliosForProduct pProductId_ =
  ListPortfoliosForProduct'
  { _lisAcceptLanguage = Nothing
  , _lisPageToken = Nothing
  , _lisPageSize = Nothing
  , _lisProductId = pProductId_
  }


-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
lisAcceptLanguage :: Lens' ListPortfoliosForProduct (Maybe Text)
lisAcceptLanguage = lens _lisAcceptLanguage (\ s a -> s{_lisAcceptLanguage = a});

-- | The page token of the first page retrieved. If null, this retrieves the first page of size @PageSize@ .
lisPageToken :: Lens' ListPortfoliosForProduct (Maybe Text)
lisPageToken = lens _lisPageToken (\ s a -> s{_lisPageToken = a});

-- | The maximum number of items to return in the results. If more results exist than fit in the specified @PageSize@ , the value of @NextPageToken@ in the response is non-null.
lisPageSize :: Lens' ListPortfoliosForProduct (Maybe Natural)
lisPageSize = lens _lisPageSize (\ s a -> s{_lisPageSize = a}) . mapping _Nat;

-- | The product identifier.
lisProductId :: Lens' ListPortfoliosForProduct Text
lisProductId = lens _lisProductId (\ s a -> s{_lisProductId = a});

instance AWSRequest ListPortfoliosForProduct where
        type Rs ListPortfoliosForProduct =
             ListPortfoliosForProductResponse
        request = postJSON serviceCatalog
        response
          = receiveJSON
              (\ s h x ->
                 ListPortfoliosForProductResponse' <$>
                   (x .?> "NextPageToken") <*>
                     (x .?> "PortfolioDetails" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListPortfoliosForProduct where

instance NFData ListPortfoliosForProduct where

instance ToHeaders ListPortfoliosForProduct where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.ListPortfoliosForProduct"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListPortfoliosForProduct where
        toJSON ListPortfoliosForProduct'{..}
          = object
              (catMaybes
                 [("AcceptLanguage" .=) <$> _lisAcceptLanguage,
                  ("PageToken" .=) <$> _lisPageToken,
                  ("PageSize" .=) <$> _lisPageSize,
                  Just ("ProductId" .= _lisProductId)])

instance ToPath ListPortfoliosForProduct where
        toPath = const "/"

instance ToQuery ListPortfoliosForProduct where
        toQuery = const mempty

-- | /See:/ 'listPortfoliosForProductResponse' smart constructor.
data ListPortfoliosForProductResponse = ListPortfoliosForProductResponse'
  { _lpfprsNextPageToken    :: !(Maybe Text)
  , _lpfprsPortfolioDetails :: !(Maybe [PortfolioDetail])
  , _lpfprsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPortfoliosForProductResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpfprsNextPageToken' - The page token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
--
-- * 'lpfprsPortfolioDetails' - List of detailed portfolio information objects.
--
-- * 'lpfprsResponseStatus' - -- | The response status code.
listPortfoliosForProductResponse
    :: Int -- ^ 'lpfprsResponseStatus'
    -> ListPortfoliosForProductResponse
listPortfoliosForProductResponse pResponseStatus_ =
  ListPortfoliosForProductResponse'
  { _lpfprsNextPageToken = Nothing
  , _lpfprsPortfolioDetails = Nothing
  , _lpfprsResponseStatus = pResponseStatus_
  }


-- | The page token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
lpfprsNextPageToken :: Lens' ListPortfoliosForProductResponse (Maybe Text)
lpfprsNextPageToken = lens _lpfprsNextPageToken (\ s a -> s{_lpfprsNextPageToken = a});

-- | List of detailed portfolio information objects.
lpfprsPortfolioDetails :: Lens' ListPortfoliosForProductResponse [PortfolioDetail]
lpfprsPortfolioDetails = lens _lpfprsPortfolioDetails (\ s a -> s{_lpfprsPortfolioDetails = a}) . _Default . _Coerce;

-- | -- | The response status code.
lpfprsResponseStatus :: Lens' ListPortfoliosForProductResponse Int
lpfprsResponseStatus = lens _lpfprsResponseStatus (\ s a -> s{_lpfprsResponseStatus = a});

instance NFData ListPortfoliosForProductResponse
         where
