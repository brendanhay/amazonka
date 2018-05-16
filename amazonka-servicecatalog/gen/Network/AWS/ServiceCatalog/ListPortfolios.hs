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
-- Module      : Network.AWS.ServiceCatalog.ListPortfolios
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all portfolios in the catalog.
--
--
--
-- This operation returns paginated results.
module Network.AWS.ServiceCatalog.ListPortfolios
    (
    -- * Creating a Request
      listPortfolios
    , ListPortfolios
    -- * Request Lenses
    , lpAcceptLanguage
    , lpPageToken
    , lpPageSize

    -- * Destructuring the Response
    , listPortfoliosResponse
    , ListPortfoliosResponse
    -- * Response Lenses
    , lprsNextPageToken
    , lprsPortfolioDetails
    , lprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'listPortfolios' smart constructor.
data ListPortfolios = ListPortfolios'
  { _lpAcceptLanguage :: !(Maybe Text)
  , _lpPageToken      :: !(Maybe Text)
  , _lpPageSize       :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPortfolios' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'lpPageToken' - The page token for the next set of results. To retrieve the first set of results, use null.
--
-- * 'lpPageSize' - The maximum number of items to return with this call.
listPortfolios
    :: ListPortfolios
listPortfolios =
  ListPortfolios'
    {_lpAcceptLanguage = Nothing, _lpPageToken = Nothing, _lpPageSize = Nothing}


-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
lpAcceptLanguage :: Lens' ListPortfolios (Maybe Text)
lpAcceptLanguage = lens _lpAcceptLanguage (\ s a -> s{_lpAcceptLanguage = a})

-- | The page token for the next set of results. To retrieve the first set of results, use null.
lpPageToken :: Lens' ListPortfolios (Maybe Text)
lpPageToken = lens _lpPageToken (\ s a -> s{_lpPageToken = a})

-- | The maximum number of items to return with this call.
lpPageSize :: Lens' ListPortfolios (Maybe Natural)
lpPageSize = lens _lpPageSize (\ s a -> s{_lpPageSize = a}) . mapping _Nat

instance AWSPager ListPortfolios where
        page rq rs
          | stop (rs ^. lprsNextPageToken) = Nothing
          | stop (rs ^. lprsPortfolioDetails) = Nothing
          | otherwise =
            Just $ rq & lpPageToken .~ rs ^. lprsNextPageToken

instance AWSRequest ListPortfolios where
        type Rs ListPortfolios = ListPortfoliosResponse
        request = postJSON serviceCatalog
        response
          = receiveJSON
              (\ s h x ->
                 ListPortfoliosResponse' <$>
                   (x .?> "NextPageToken") <*>
                     (x .?> "PortfolioDetails" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListPortfolios where

instance NFData ListPortfolios where

instance ToHeaders ListPortfolios where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.ListPortfolios" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListPortfolios where
        toJSON ListPortfolios'{..}
          = object
              (catMaybes
                 [("AcceptLanguage" .=) <$> _lpAcceptLanguage,
                  ("PageToken" .=) <$> _lpPageToken,
                  ("PageSize" .=) <$> _lpPageSize])

instance ToPath ListPortfolios where
        toPath = const "/"

instance ToQuery ListPortfolios where
        toQuery = const mempty

-- | /See:/ 'listPortfoliosResponse' smart constructor.
data ListPortfoliosResponse = ListPortfoliosResponse'
  { _lprsNextPageToken    :: !(Maybe Text)
  , _lprsPortfolioDetails :: !(Maybe [PortfolioDetail])
  , _lprsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPortfoliosResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lprsNextPageToken' - The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- * 'lprsPortfolioDetails' - Information about the portfolios.
--
-- * 'lprsResponseStatus' - -- | The response status code.
listPortfoliosResponse
    :: Int -- ^ 'lprsResponseStatus'
    -> ListPortfoliosResponse
listPortfoliosResponse pResponseStatus_ =
  ListPortfoliosResponse'
    { _lprsNextPageToken = Nothing
    , _lprsPortfolioDetails = Nothing
    , _lprsResponseStatus = pResponseStatus_
    }


-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
lprsNextPageToken :: Lens' ListPortfoliosResponse (Maybe Text)
lprsNextPageToken = lens _lprsNextPageToken (\ s a -> s{_lprsNextPageToken = a})

-- | Information about the portfolios.
lprsPortfolioDetails :: Lens' ListPortfoliosResponse [PortfolioDetail]
lprsPortfolioDetails = lens _lprsPortfolioDetails (\ s a -> s{_lprsPortfolioDetails = a}) . _Default . _Coerce

-- | -- | The response status code.
lprsResponseStatus :: Lens' ListPortfoliosResponse Int
lprsResponseStatus = lens _lprsResponseStatus (\ s a -> s{_lprsResponseStatus = a})

instance NFData ListPortfoliosResponse where
