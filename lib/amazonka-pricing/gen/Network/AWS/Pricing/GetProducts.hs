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
-- Module      : Network.AWS.Pricing.GetProducts
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all products that match the filter criteria.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Pricing.GetProducts
    (
    -- * Creating a Request
      getProducts
    , GetProducts
    -- * Request Lenses
    , gpFilters
    , gpFormatVersion
    , gpNextToken
    , gpServiceCode
    , gpMaxResults

    -- * Destructuring the Response
    , getProductsResponse
    , GetProductsResponse
    -- * Response Lenses
    , gprsFormatVersion
    , gprsNextToken
    , gprsPriceList
    , gprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Pricing.Types
import Network.AWS.Pricing.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getProducts' smart constructor.
data GetProducts = GetProducts'
  { _gpFilters       :: !(Maybe [Filter])
  , _gpFormatVersion :: !(Maybe Text)
  , _gpNextToken     :: !(Maybe Text)
  , _gpServiceCode   :: !(Maybe Text)
  , _gpMaxResults    :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetProducts' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpFilters' - The list of filters that limit the returned products. only products that match all filters are returned.
--
-- * 'gpFormatVersion' - The format version that you want the response to be in. Valid values are: @aws_v1@
--
-- * 'gpNextToken' - The pagination token that indicates the next set of results that you want to retrieve.
--
-- * 'gpServiceCode' - The code for the service whose products you want to retrieve.
--
-- * 'gpMaxResults' - The maximum number of results to return in the response.
getProducts
    :: GetProducts
getProducts =
  GetProducts'
    { _gpFilters = Nothing
    , _gpFormatVersion = Nothing
    , _gpNextToken = Nothing
    , _gpServiceCode = Nothing
    , _gpMaxResults = Nothing
    }


-- | The list of filters that limit the returned products. only products that match all filters are returned.
gpFilters :: Lens' GetProducts [Filter]
gpFilters = lens _gpFilters (\ s a -> s{_gpFilters = a}) . _Default . _Coerce

-- | The format version that you want the response to be in. Valid values are: @aws_v1@
gpFormatVersion :: Lens' GetProducts (Maybe Text)
gpFormatVersion = lens _gpFormatVersion (\ s a -> s{_gpFormatVersion = a})

-- | The pagination token that indicates the next set of results that you want to retrieve.
gpNextToken :: Lens' GetProducts (Maybe Text)
gpNextToken = lens _gpNextToken (\ s a -> s{_gpNextToken = a})

-- | The code for the service whose products you want to retrieve.
gpServiceCode :: Lens' GetProducts (Maybe Text)
gpServiceCode = lens _gpServiceCode (\ s a -> s{_gpServiceCode = a})

-- | The maximum number of results to return in the response.
gpMaxResults :: Lens' GetProducts (Maybe Natural)
gpMaxResults = lens _gpMaxResults (\ s a -> s{_gpMaxResults = a}) . mapping _Nat

instance AWSPager GetProducts where
        page rq rs
          | stop (rs ^. gprsNextToken) = Nothing
          | stop (rs ^. gprsPriceList) = Nothing
          | otherwise =
            Just $ rq & gpNextToken .~ rs ^. gprsNextToken

instance AWSRequest GetProducts where
        type Rs GetProducts = GetProductsResponse
        request = postJSON pricing
        response
          = receiveJSON
              (\ s h x ->
                 GetProductsResponse' <$>
                   (x .?> "FormatVersion") <*> (x .?> "NextToken") <*>
                     (x .?> "PriceList" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetProducts where

instance NFData GetProducts where

instance ToHeaders GetProducts where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSPriceListService.GetProducts" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetProducts where
        toJSON GetProducts'{..}
          = object
              (catMaybes
                 [("Filters" .=) <$> _gpFilters,
                  ("FormatVersion" .=) <$> _gpFormatVersion,
                  ("NextToken" .=) <$> _gpNextToken,
                  ("ServiceCode" .=) <$> _gpServiceCode,
                  ("MaxResults" .=) <$> _gpMaxResults])

instance ToPath GetProducts where
        toPath = const "/"

instance ToQuery GetProducts where
        toQuery = const mempty

-- | /See:/ 'getProductsResponse' smart constructor.
data GetProductsResponse = GetProductsResponse'
  { _gprsFormatVersion  :: !(Maybe Text)
  , _gprsNextToken      :: !(Maybe Text)
  , _gprsPriceList      :: !(Maybe [Text])
  , _gprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetProductsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gprsFormatVersion' - The format version of the response. For example, aws_v1.
--
-- * 'gprsNextToken' - The pagination token that indicates the next set of results to retrieve.
--
-- * 'gprsPriceList' - The list of products that match your filters. The list contains both the product metadata and the price information.
--
-- * 'gprsResponseStatus' - -- | The response status code.
getProductsResponse
    :: Int -- ^ 'gprsResponseStatus'
    -> GetProductsResponse
getProductsResponse pResponseStatus_ =
  GetProductsResponse'
    { _gprsFormatVersion = Nothing
    , _gprsNextToken = Nothing
    , _gprsPriceList = Nothing
    , _gprsResponseStatus = pResponseStatus_
    }


-- | The format version of the response. For example, aws_v1.
gprsFormatVersion :: Lens' GetProductsResponse (Maybe Text)
gprsFormatVersion = lens _gprsFormatVersion (\ s a -> s{_gprsFormatVersion = a})

-- | The pagination token that indicates the next set of results to retrieve.
gprsNextToken :: Lens' GetProductsResponse (Maybe Text)
gprsNextToken = lens _gprsNextToken (\ s a -> s{_gprsNextToken = a})

-- | The list of products that match your filters. The list contains both the product metadata and the price information.
gprsPriceList :: Lens' GetProductsResponse [Text]
gprsPriceList = lens _gprsPriceList (\ s a -> s{_gprsPriceList = a}) . _Default . _Coerce

-- | -- | The response status code.
gprsResponseStatus :: Lens' GetProductsResponse Int
gprsResponseStatus = lens _gprsResponseStatus (\ s a -> s{_gprsResponseStatus = a})

instance NFData GetProductsResponse where
