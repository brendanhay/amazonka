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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all portfolios that the specified product is associated with.
--
--
--
-- This operation returns paginated results.
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
import Network.AWS.Pager
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
-- * 'lisPageToken' - The page token for the next set of results. To retrieve the first set of results, use null.
--
-- * 'lisPageSize' - The maximum number of items to return with this call.
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
lisAcceptLanguage = lens _lisAcceptLanguage (\ s a -> s{_lisAcceptLanguage = a})

-- | The page token for the next set of results. To retrieve the first set of results, use null.
lisPageToken :: Lens' ListPortfoliosForProduct (Maybe Text)
lisPageToken = lens _lisPageToken (\ s a -> s{_lisPageToken = a})

-- | The maximum number of items to return with this call.
lisPageSize :: Lens' ListPortfoliosForProduct (Maybe Natural)
lisPageSize = lens _lisPageSize (\ s a -> s{_lisPageSize = a}) . mapping _Nat

-- | The product identifier.
lisProductId :: Lens' ListPortfoliosForProduct Text
lisProductId = lens _lisProductId (\ s a -> s{_lisProductId = a})

instance AWSPager ListPortfoliosForProduct where
        page rq rs
          | stop (rs ^. lpfprsNextPageToken) = Nothing
          | stop (rs ^. lpfprsPortfolioDetails) = Nothing
          | otherwise =
            Just $ rq & lisPageToken .~ rs ^. lpfprsNextPageToken

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
-- * 'lpfprsNextPageToken' - The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- * 'lpfprsPortfolioDetails' - Information about the portfolios.
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


-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
lpfprsNextPageToken :: Lens' ListPortfoliosForProductResponse (Maybe Text)
lpfprsNextPageToken = lens _lpfprsNextPageToken (\ s a -> s{_lpfprsNextPageToken = a})

-- | Information about the portfolios.
lpfprsPortfolioDetails :: Lens' ListPortfoliosForProductResponse [PortfolioDetail]
lpfprsPortfolioDetails = lens _lpfprsPortfolioDetails (\ s a -> s{_lpfprsPortfolioDetails = a}) . _Default . _Coerce

-- | -- | The response status code.
lpfprsResponseStatus :: Lens' ListPortfoliosForProductResponse Int
lpfprsResponseStatus = lens _lpfprsResponseStatus (\ s a -> s{_lpfprsResponseStatus = a})

instance NFData ListPortfoliosForProductResponse
         where
