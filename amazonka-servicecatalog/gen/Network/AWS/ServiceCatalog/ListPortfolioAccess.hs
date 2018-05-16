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
-- Module      : Network.AWS.ServiceCatalog.ListPortfolioAccess
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the account IDs that have access to the specified portfolio.
--
--
module Network.AWS.ServiceCatalog.ListPortfolioAccess
    (
    -- * Creating a Request
      listPortfolioAccess
    , ListPortfolioAccess
    -- * Request Lenses
    , lAcceptLanguage
    , lPortfolioId

    -- * Destructuring the Response
    , listPortfolioAccessResponse
    , ListPortfolioAccessResponse
    -- * Response Lenses
    , lparsNextPageToken
    , lparsAccountIds
    , lparsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'listPortfolioAccess' smart constructor.
data ListPortfolioAccess = ListPortfolioAccess'
  { _lAcceptLanguage :: !(Maybe Text)
  , _lPortfolioId    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPortfolioAccess' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'lPortfolioId' - The portfolio identifier.
listPortfolioAccess
    :: Text -- ^ 'lPortfolioId'
    -> ListPortfolioAccess
listPortfolioAccess pPortfolioId_ =
  ListPortfolioAccess'
    {_lAcceptLanguage = Nothing, _lPortfolioId = pPortfolioId_}


-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
lAcceptLanguage :: Lens' ListPortfolioAccess (Maybe Text)
lAcceptLanguage = lens _lAcceptLanguage (\ s a -> s{_lAcceptLanguage = a})

-- | The portfolio identifier.
lPortfolioId :: Lens' ListPortfolioAccess Text
lPortfolioId = lens _lPortfolioId (\ s a -> s{_lPortfolioId = a})

instance AWSRequest ListPortfolioAccess where
        type Rs ListPortfolioAccess =
             ListPortfolioAccessResponse
        request = postJSON serviceCatalog
        response
          = receiveJSON
              (\ s h x ->
                 ListPortfolioAccessResponse' <$>
                   (x .?> "NextPageToken") <*>
                     (x .?> "AccountIds" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListPortfolioAccess where

instance NFData ListPortfolioAccess where

instance ToHeaders ListPortfolioAccess where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.ListPortfolioAccess" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListPortfolioAccess where
        toJSON ListPortfolioAccess'{..}
          = object
              (catMaybes
                 [("AcceptLanguage" .=) <$> _lAcceptLanguage,
                  Just ("PortfolioId" .= _lPortfolioId)])

instance ToPath ListPortfolioAccess where
        toPath = const "/"

instance ToQuery ListPortfolioAccess where
        toQuery = const mempty

-- | /See:/ 'listPortfolioAccessResponse' smart constructor.
data ListPortfolioAccessResponse = ListPortfolioAccessResponse'
  { _lparsNextPageToken  :: !(Maybe Text)
  , _lparsAccountIds     :: !(Maybe [Text])
  , _lparsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPortfolioAccessResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lparsNextPageToken' - The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- * 'lparsAccountIds' - Information about the AWS accounts with access to the portfolio.
--
-- * 'lparsResponseStatus' - -- | The response status code.
listPortfolioAccessResponse
    :: Int -- ^ 'lparsResponseStatus'
    -> ListPortfolioAccessResponse
listPortfolioAccessResponse pResponseStatus_ =
  ListPortfolioAccessResponse'
    { _lparsNextPageToken = Nothing
    , _lparsAccountIds = Nothing
    , _lparsResponseStatus = pResponseStatus_
    }


-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
lparsNextPageToken :: Lens' ListPortfolioAccessResponse (Maybe Text)
lparsNextPageToken = lens _lparsNextPageToken (\ s a -> s{_lparsNextPageToken = a})

-- | Information about the AWS accounts with access to the portfolio.
lparsAccountIds :: Lens' ListPortfolioAccessResponse [Text]
lparsAccountIds = lens _lparsAccountIds (\ s a -> s{_lparsAccountIds = a}) . _Default . _Coerce

-- | -- | The response status code.
lparsResponseStatus :: Lens' ListPortfolioAccessResponse Int
lparsResponseStatus = lens _lparsResponseStatus (\ s a -> s{_lparsResponseStatus = a})

instance NFData ListPortfolioAccessResponse where
