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
-- Module      : Network.AWS.ServiceCatalog.ListPrincipalsForPortfolio
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all principal ARNs associated with the specified portfolio.
--
--
--
-- This operation returns paginated results.
module Network.AWS.ServiceCatalog.ListPrincipalsForPortfolio
    (
    -- * Creating a Request
      listPrincipalsForPortfolio
    , ListPrincipalsForPortfolio
    -- * Request Lenses
    , lpfpAcceptLanguage
    , lpfpPageToken
    , lpfpPageSize
    , lpfpPortfolioId

    -- * Destructuring the Response
    , listPrincipalsForPortfolioResponse
    , ListPrincipalsForPortfolioResponse
    -- * Response Lenses
    , lisrsNextPageToken
    , lisrsPrincipals
    , lisrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'listPrincipalsForPortfolio' smart constructor.
data ListPrincipalsForPortfolio = ListPrincipalsForPortfolio'
  { _lpfpAcceptLanguage :: !(Maybe Text)
  , _lpfpPageToken      :: !(Maybe Text)
  , _lpfpPageSize       :: !(Maybe Nat)
  , _lpfpPortfolioId    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPrincipalsForPortfolio' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpfpAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'lpfpPageToken' - The page token for the next set of results. To retrieve the first set of results, use null.
--
-- * 'lpfpPageSize' - The maximum number of items to return with this call.
--
-- * 'lpfpPortfolioId' - The portfolio identifier.
listPrincipalsForPortfolio
    :: Text -- ^ 'lpfpPortfolioId'
    -> ListPrincipalsForPortfolio
listPrincipalsForPortfolio pPortfolioId_ =
  ListPrincipalsForPortfolio'
    { _lpfpAcceptLanguage = Nothing
    , _lpfpPageToken = Nothing
    , _lpfpPageSize = Nothing
    , _lpfpPortfolioId = pPortfolioId_
    }


-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
lpfpAcceptLanguage :: Lens' ListPrincipalsForPortfolio (Maybe Text)
lpfpAcceptLanguage = lens _lpfpAcceptLanguage (\ s a -> s{_lpfpAcceptLanguage = a})

-- | The page token for the next set of results. To retrieve the first set of results, use null.
lpfpPageToken :: Lens' ListPrincipalsForPortfolio (Maybe Text)
lpfpPageToken = lens _lpfpPageToken (\ s a -> s{_lpfpPageToken = a})

-- | The maximum number of items to return with this call.
lpfpPageSize :: Lens' ListPrincipalsForPortfolio (Maybe Natural)
lpfpPageSize = lens _lpfpPageSize (\ s a -> s{_lpfpPageSize = a}) . mapping _Nat

-- | The portfolio identifier.
lpfpPortfolioId :: Lens' ListPrincipalsForPortfolio Text
lpfpPortfolioId = lens _lpfpPortfolioId (\ s a -> s{_lpfpPortfolioId = a})

instance AWSPager ListPrincipalsForPortfolio where
        page rq rs
          | stop (rs ^. lisrsNextPageToken) = Nothing
          | stop (rs ^. lisrsPrincipals) = Nothing
          | otherwise =
            Just $ rq & lpfpPageToken .~ rs ^. lisrsNextPageToken

instance AWSRequest ListPrincipalsForPortfolio where
        type Rs ListPrincipalsForPortfolio =
             ListPrincipalsForPortfolioResponse
        request = postJSON serviceCatalog
        response
          = receiveJSON
              (\ s h x ->
                 ListPrincipalsForPortfolioResponse' <$>
                   (x .?> "NextPageToken") <*>
                     (x .?> "Principals" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListPrincipalsForPortfolio where

instance NFData ListPrincipalsForPortfolio where

instance ToHeaders ListPrincipalsForPortfolio where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.ListPrincipalsForPortfolio"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListPrincipalsForPortfolio where
        toJSON ListPrincipalsForPortfolio'{..}
          = object
              (catMaybes
                 [("AcceptLanguage" .=) <$> _lpfpAcceptLanguage,
                  ("PageToken" .=) <$> _lpfpPageToken,
                  ("PageSize" .=) <$> _lpfpPageSize,
                  Just ("PortfolioId" .= _lpfpPortfolioId)])

instance ToPath ListPrincipalsForPortfolio where
        toPath = const "/"

instance ToQuery ListPrincipalsForPortfolio where
        toQuery = const mempty

-- | /See:/ 'listPrincipalsForPortfolioResponse' smart constructor.
data ListPrincipalsForPortfolioResponse = ListPrincipalsForPortfolioResponse'
  { _lisrsNextPageToken  :: !(Maybe Text)
  , _lisrsPrincipals     :: !(Maybe [Principal])
  , _lisrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPrincipalsForPortfolioResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lisrsNextPageToken' - The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- * 'lisrsPrincipals' - The IAM principals (users or roles) associated with the portfolio.
--
-- * 'lisrsResponseStatus' - -- | The response status code.
listPrincipalsForPortfolioResponse
    :: Int -- ^ 'lisrsResponseStatus'
    -> ListPrincipalsForPortfolioResponse
listPrincipalsForPortfolioResponse pResponseStatus_ =
  ListPrincipalsForPortfolioResponse'
    { _lisrsNextPageToken = Nothing
    , _lisrsPrincipals = Nothing
    , _lisrsResponseStatus = pResponseStatus_
    }


-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
lisrsNextPageToken :: Lens' ListPrincipalsForPortfolioResponse (Maybe Text)
lisrsNextPageToken = lens _lisrsNextPageToken (\ s a -> s{_lisrsNextPageToken = a})

-- | The IAM principals (users or roles) associated with the portfolio.
lisrsPrincipals :: Lens' ListPrincipalsForPortfolioResponse [Principal]
lisrsPrincipals = lens _lisrsPrincipals (\ s a -> s{_lisrsPrincipals = a}) . _Default . _Coerce

-- | -- | The response status code.
lisrsResponseStatus :: Lens' ListPrincipalsForPortfolioResponse Int
lisrsResponseStatus = lens _lisrsResponseStatus (\ s a -> s{_lisrsResponseStatus = a})

instance NFData ListPrincipalsForPortfolioResponse
         where
