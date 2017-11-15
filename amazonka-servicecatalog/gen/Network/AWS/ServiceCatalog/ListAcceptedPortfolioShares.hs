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
-- Module      : Network.AWS.ServiceCatalog.ListAcceptedPortfolioShares
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists details of all portfolios for which sharing was accepted by this account.
--
--
module Network.AWS.ServiceCatalog.ListAcceptedPortfolioShares
    (
    -- * Creating a Request
      listAcceptedPortfolioShares
    , ListAcceptedPortfolioShares
    -- * Request Lenses
    , lapsAcceptLanguage
    , lapsPageToken
    , lapsPageSize

    -- * Destructuring the Response
    , listAcceptedPortfolioSharesResponse
    , ListAcceptedPortfolioSharesResponse
    -- * Response Lenses
    , lapsrsNextPageToken
    , lapsrsPortfolioDetails
    , lapsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'listAcceptedPortfolioShares' smart constructor.
data ListAcceptedPortfolioShares = ListAcceptedPortfolioShares'
  { _lapsAcceptLanguage :: !(Maybe Text)
  , _lapsPageToken      :: !(Maybe Text)
  , _lapsPageSize       :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAcceptedPortfolioShares' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lapsAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'lapsPageToken' - The page token of the first page retrieved. If null, this retrieves the first page of size @PageSize@ .
--
-- * 'lapsPageSize' - The maximum number of items to return in the results. If more results exist than fit in the specified @PageSize@ , the value of @NextPageToken@ in the response is non-null.
listAcceptedPortfolioShares
    :: ListAcceptedPortfolioShares
listAcceptedPortfolioShares =
  ListAcceptedPortfolioShares'
  { _lapsAcceptLanguage = Nothing
  , _lapsPageToken = Nothing
  , _lapsPageSize = Nothing
  }


-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
lapsAcceptLanguage :: Lens' ListAcceptedPortfolioShares (Maybe Text)
lapsAcceptLanguage = lens _lapsAcceptLanguage (\ s a -> s{_lapsAcceptLanguage = a});

-- | The page token of the first page retrieved. If null, this retrieves the first page of size @PageSize@ .
lapsPageToken :: Lens' ListAcceptedPortfolioShares (Maybe Text)
lapsPageToken = lens _lapsPageToken (\ s a -> s{_lapsPageToken = a});

-- | The maximum number of items to return in the results. If more results exist than fit in the specified @PageSize@ , the value of @NextPageToken@ in the response is non-null.
lapsPageSize :: Lens' ListAcceptedPortfolioShares (Maybe Natural)
lapsPageSize = lens _lapsPageSize (\ s a -> s{_lapsPageSize = a}) . mapping _Nat;

instance AWSRequest ListAcceptedPortfolioShares where
        type Rs ListAcceptedPortfolioShares =
             ListAcceptedPortfolioSharesResponse
        request = postJSON serviceCatalog
        response
          = receiveJSON
              (\ s h x ->
                 ListAcceptedPortfolioSharesResponse' <$>
                   (x .?> "NextPageToken") <*>
                     (x .?> "PortfolioDetails" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListAcceptedPortfolioShares where

instance NFData ListAcceptedPortfolioShares where

instance ToHeaders ListAcceptedPortfolioShares where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.ListAcceptedPortfolioShares"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListAcceptedPortfolioShares where
        toJSON ListAcceptedPortfolioShares'{..}
          = object
              (catMaybes
                 [("AcceptLanguage" .=) <$> _lapsAcceptLanguage,
                  ("PageToken" .=) <$> _lapsPageToken,
                  ("PageSize" .=) <$> _lapsPageSize])

instance ToPath ListAcceptedPortfolioShares where
        toPath = const "/"

instance ToQuery ListAcceptedPortfolioShares where
        toQuery = const mempty

-- | /See:/ 'listAcceptedPortfolioSharesResponse' smart constructor.
data ListAcceptedPortfolioSharesResponse = ListAcceptedPortfolioSharesResponse'
  { _lapsrsNextPageToken    :: !(Maybe Text)
  , _lapsrsPortfolioDetails :: !(Maybe [PortfolioDetail])
  , _lapsrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAcceptedPortfolioSharesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lapsrsNextPageToken' - The page token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
--
-- * 'lapsrsPortfolioDetails' - List of detailed portfolio information objects.
--
-- * 'lapsrsResponseStatus' - -- | The response status code.
listAcceptedPortfolioSharesResponse
    :: Int -- ^ 'lapsrsResponseStatus'
    -> ListAcceptedPortfolioSharesResponse
listAcceptedPortfolioSharesResponse pResponseStatus_ =
  ListAcceptedPortfolioSharesResponse'
  { _lapsrsNextPageToken = Nothing
  , _lapsrsPortfolioDetails = Nothing
  , _lapsrsResponseStatus = pResponseStatus_
  }


-- | The page token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
lapsrsNextPageToken :: Lens' ListAcceptedPortfolioSharesResponse (Maybe Text)
lapsrsNextPageToken = lens _lapsrsNextPageToken (\ s a -> s{_lapsrsNextPageToken = a});

-- | List of detailed portfolio information objects.
lapsrsPortfolioDetails :: Lens' ListAcceptedPortfolioSharesResponse [PortfolioDetail]
lapsrsPortfolioDetails = lens _lapsrsPortfolioDetails (\ s a -> s{_lapsrsPortfolioDetails = a}) . _Default . _Coerce;

-- | -- | The response status code.
lapsrsResponseStatus :: Lens' ListAcceptedPortfolioSharesResponse Int
lapsrsResponseStatus = lens _lapsrsResponseStatus (\ s a -> s{_lapsrsResponseStatus = a});

instance NFData ListAcceptedPortfolioSharesResponse
         where
