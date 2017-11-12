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
-- Module      : Network.AWS.ServiceCatalog.ListConstraintsForPortfolio
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves detailed constraint information for the specified portfolio and product.
--
--
module Network.AWS.ServiceCatalog.ListConstraintsForPortfolio
    (
    -- * Creating a Request
      listConstraintsForPortfolio
    , ListConstraintsForPortfolio
    -- * Request Lenses
    , lcfpAcceptLanguage
    , lcfpPageToken
    , lcfpPageSize
    , lcfpProductId
    , lcfpPortfolioId

    -- * Destructuring the Response
    , listConstraintsForPortfolioResponse
    , ListConstraintsForPortfolioResponse
    -- * Response Lenses
    , lcfprsNextPageToken
    , lcfprsConstraintDetails
    , lcfprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'listConstraintsForPortfolio' smart constructor.
data ListConstraintsForPortfolio = ListConstraintsForPortfolio'
  { _lcfpAcceptLanguage :: !(Maybe Text)
  , _lcfpPageToken      :: !(Maybe Text)
  , _lcfpPageSize       :: !(Maybe Nat)
  , _lcfpProductId      :: !(Maybe Text)
  , _lcfpPortfolioId    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListConstraintsForPortfolio' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcfpAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'lcfpPageToken' - The page token of the first page retrieved. If null, this retrieves the first page of size @PageSize@ .
--
-- * 'lcfpPageSize' - The maximum number of items to return in the results. If more results exist than fit in the specified @PageSize@ , the value of @NextPageToken@ in the response is non-null.
--
-- * 'lcfpProductId' - The product identifier.
--
-- * 'lcfpPortfolioId' - The portfolio identifier.
listConstraintsForPortfolio
    :: Text -- ^ 'lcfpPortfolioId'
    -> ListConstraintsForPortfolio
listConstraintsForPortfolio pPortfolioId_ =
  ListConstraintsForPortfolio'
  { _lcfpAcceptLanguage = Nothing
  , _lcfpPageToken = Nothing
  , _lcfpPageSize = Nothing
  , _lcfpProductId = Nothing
  , _lcfpPortfolioId = pPortfolioId_
  }


-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
lcfpAcceptLanguage :: Lens' ListConstraintsForPortfolio (Maybe Text)
lcfpAcceptLanguage = lens _lcfpAcceptLanguage (\ s a -> s{_lcfpAcceptLanguage = a});

-- | The page token of the first page retrieved. If null, this retrieves the first page of size @PageSize@ .
lcfpPageToken :: Lens' ListConstraintsForPortfolio (Maybe Text)
lcfpPageToken = lens _lcfpPageToken (\ s a -> s{_lcfpPageToken = a});

-- | The maximum number of items to return in the results. If more results exist than fit in the specified @PageSize@ , the value of @NextPageToken@ in the response is non-null.
lcfpPageSize :: Lens' ListConstraintsForPortfolio (Maybe Natural)
lcfpPageSize = lens _lcfpPageSize (\ s a -> s{_lcfpPageSize = a}) . mapping _Nat;

-- | The product identifier.
lcfpProductId :: Lens' ListConstraintsForPortfolio (Maybe Text)
lcfpProductId = lens _lcfpProductId (\ s a -> s{_lcfpProductId = a});

-- | The portfolio identifier.
lcfpPortfolioId :: Lens' ListConstraintsForPortfolio Text
lcfpPortfolioId = lens _lcfpPortfolioId (\ s a -> s{_lcfpPortfolioId = a});

instance AWSRequest ListConstraintsForPortfolio where
        type Rs ListConstraintsForPortfolio =
             ListConstraintsForPortfolioResponse
        request = postJSON serviceCatalog
        response
          = receiveJSON
              (\ s h x ->
                 ListConstraintsForPortfolioResponse' <$>
                   (x .?> "NextPageToken") <*>
                     (x .?> "ConstraintDetails" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListConstraintsForPortfolio where

instance NFData ListConstraintsForPortfolio where

instance ToHeaders ListConstraintsForPortfolio where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.ListConstraintsForPortfolio"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListConstraintsForPortfolio where
        toJSON ListConstraintsForPortfolio'{..}
          = object
              (catMaybes
                 [("AcceptLanguage" .=) <$> _lcfpAcceptLanguage,
                  ("PageToken" .=) <$> _lcfpPageToken,
                  ("PageSize" .=) <$> _lcfpPageSize,
                  ("ProductId" .=) <$> _lcfpProductId,
                  Just ("PortfolioId" .= _lcfpPortfolioId)])

instance ToPath ListConstraintsForPortfolio where
        toPath = const "/"

instance ToQuery ListConstraintsForPortfolio where
        toQuery = const mempty

-- | /See:/ 'listConstraintsForPortfolioResponse' smart constructor.
data ListConstraintsForPortfolioResponse = ListConstraintsForPortfolioResponse'
  { _lcfprsNextPageToken     :: !(Maybe Text)
  , _lcfprsConstraintDetails :: !(Maybe [ConstraintDetail])
  , _lcfprsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListConstraintsForPortfolioResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcfprsNextPageToken' - The page token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
--
-- * 'lcfprsConstraintDetails' - List of detailed constraint information objects.
--
-- * 'lcfprsResponseStatus' - -- | The response status code.
listConstraintsForPortfolioResponse
    :: Int -- ^ 'lcfprsResponseStatus'
    -> ListConstraintsForPortfolioResponse
listConstraintsForPortfolioResponse pResponseStatus_ =
  ListConstraintsForPortfolioResponse'
  { _lcfprsNextPageToken = Nothing
  , _lcfprsConstraintDetails = Nothing
  , _lcfprsResponseStatus = pResponseStatus_
  }


-- | The page token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
lcfprsNextPageToken :: Lens' ListConstraintsForPortfolioResponse (Maybe Text)
lcfprsNextPageToken = lens _lcfprsNextPageToken (\ s a -> s{_lcfprsNextPageToken = a});

-- | List of detailed constraint information objects.
lcfprsConstraintDetails :: Lens' ListConstraintsForPortfolioResponse [ConstraintDetail]
lcfprsConstraintDetails = lens _lcfprsConstraintDetails (\ s a -> s{_lcfprsConstraintDetails = a}) . _Default . _Coerce;

-- | -- | The response status code.
lcfprsResponseStatus :: Lens' ListConstraintsForPortfolioResponse Int
lcfprsResponseStatus = lens _lcfprsResponseStatus (\ s a -> s{_lcfprsResponseStatus = a});

instance NFData ListConstraintsForPortfolioResponse
         where
