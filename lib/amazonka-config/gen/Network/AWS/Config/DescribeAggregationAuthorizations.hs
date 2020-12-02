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
-- Module      : Network.AWS.Config.DescribeAggregationAuthorizations
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of authorizations granted to various aggregator accounts and regions.
--
--
module Network.AWS.Config.DescribeAggregationAuthorizations
    (
    -- * Creating a Request
      describeAggregationAuthorizations
    , DescribeAggregationAuthorizations
    -- * Request Lenses
    , daaNextToken
    , daaLimit

    -- * Destructuring the Response
    , describeAggregationAuthorizationsResponse
    , DescribeAggregationAuthorizationsResponse
    -- * Response Lenses
    , daarsAggregationAuthorizations
    , daarsNextToken
    , daarsResponseStatus
    ) where

import Network.AWS.Config.Types
import Network.AWS.Config.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeAggregationAuthorizations' smart constructor.
data DescribeAggregationAuthorizations = DescribeAggregationAuthorizations'
  { _daaNextToken :: !(Maybe Text)
  , _daaLimit     :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAggregationAuthorizations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daaNextToken' - The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- * 'daaLimit' - The maximum number of AggregationAuthorizations returned on each page. The default is maximum. If you specify 0, AWS Config uses the default.
describeAggregationAuthorizations
    :: DescribeAggregationAuthorizations
describeAggregationAuthorizations =
  DescribeAggregationAuthorizations'
    {_daaNextToken = Nothing, _daaLimit = Nothing}


-- | The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
daaNextToken :: Lens' DescribeAggregationAuthorizations (Maybe Text)
daaNextToken = lens _daaNextToken (\ s a -> s{_daaNextToken = a})

-- | The maximum number of AggregationAuthorizations returned on each page. The default is maximum. If you specify 0, AWS Config uses the default.
daaLimit :: Lens' DescribeAggregationAuthorizations (Maybe Natural)
daaLimit = lens _daaLimit (\ s a -> s{_daaLimit = a}) . mapping _Nat

instance AWSRequest DescribeAggregationAuthorizations
         where
        type Rs DescribeAggregationAuthorizations =
             DescribeAggregationAuthorizationsResponse
        request = postJSON config
        response
          = receiveJSON
              (\ s h x ->
                 DescribeAggregationAuthorizationsResponse' <$>
                   (x .?> "AggregationAuthorizations" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeAggregationAuthorizations
         where

instance NFData DescribeAggregationAuthorizations
         where

instance ToHeaders DescribeAggregationAuthorizations
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StarlingDoveService.DescribeAggregationAuthorizations"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeAggregationAuthorizations
         where
        toJSON DescribeAggregationAuthorizations'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _daaNextToken,
                  ("Limit" .=) <$> _daaLimit])

instance ToPath DescribeAggregationAuthorizations
         where
        toPath = const "/"

instance ToQuery DescribeAggregationAuthorizations
         where
        toQuery = const mempty

-- | /See:/ 'describeAggregationAuthorizationsResponse' smart constructor.
data DescribeAggregationAuthorizationsResponse = DescribeAggregationAuthorizationsResponse'
  { _daarsAggregationAuthorizations :: !(Maybe [AggregationAuthorization])
  , _daarsNextToken                 :: !(Maybe Text)
  , _daarsResponseStatus            :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAggregationAuthorizationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daarsAggregationAuthorizations' - Returns a list of authorizations granted to various aggregator accounts and regions.
--
-- * 'daarsNextToken' - The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- * 'daarsResponseStatus' - -- | The response status code.
describeAggregationAuthorizationsResponse
    :: Int -- ^ 'daarsResponseStatus'
    -> DescribeAggregationAuthorizationsResponse
describeAggregationAuthorizationsResponse pResponseStatus_ =
  DescribeAggregationAuthorizationsResponse'
    { _daarsAggregationAuthorizations = Nothing
    , _daarsNextToken = Nothing
    , _daarsResponseStatus = pResponseStatus_
    }


-- | Returns a list of authorizations granted to various aggregator accounts and regions.
daarsAggregationAuthorizations :: Lens' DescribeAggregationAuthorizationsResponse [AggregationAuthorization]
daarsAggregationAuthorizations = lens _daarsAggregationAuthorizations (\ s a -> s{_daarsAggregationAuthorizations = a}) . _Default . _Coerce

-- | The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
daarsNextToken :: Lens' DescribeAggregationAuthorizationsResponse (Maybe Text)
daarsNextToken = lens _daarsNextToken (\ s a -> s{_daarsNextToken = a})

-- | -- | The response status code.
daarsResponseStatus :: Lens' DescribeAggregationAuthorizationsResponse Int
daarsResponseStatus = lens _daarsResponseStatus (\ s a -> s{_daarsResponseStatus = a})

instance NFData
           DescribeAggregationAuthorizationsResponse
         where
