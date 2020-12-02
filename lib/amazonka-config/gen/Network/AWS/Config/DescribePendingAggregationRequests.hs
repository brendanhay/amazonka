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
-- Module      : Network.AWS.Config.DescribePendingAggregationRequests
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all pending aggregation requests.
--
--
module Network.AWS.Config.DescribePendingAggregationRequests
    (
    -- * Creating a Request
      describePendingAggregationRequests
    , DescribePendingAggregationRequests
    -- * Request Lenses
    , dparNextToken
    , dparLimit

    -- * Destructuring the Response
    , describePendingAggregationRequestsResponse
    , DescribePendingAggregationRequestsResponse
    -- * Response Lenses
    , dparrsNextToken
    , dparrsPendingAggregationRequests
    , dparrsResponseStatus
    ) where

import Network.AWS.Config.Types
import Network.AWS.Config.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describePendingAggregationRequests' smart constructor.
data DescribePendingAggregationRequests = DescribePendingAggregationRequests'
  { _dparNextToken :: !(Maybe Text)
  , _dparLimit     :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribePendingAggregationRequests' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dparNextToken' - The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- * 'dparLimit' - The maximum number of evaluation results returned on each page. The default is maximum. If you specify 0, AWS Config uses the default.
describePendingAggregationRequests
    :: DescribePendingAggregationRequests
describePendingAggregationRequests =
  DescribePendingAggregationRequests'
    {_dparNextToken = Nothing, _dparLimit = Nothing}


-- | The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
dparNextToken :: Lens' DescribePendingAggregationRequests (Maybe Text)
dparNextToken = lens _dparNextToken (\ s a -> s{_dparNextToken = a})

-- | The maximum number of evaluation results returned on each page. The default is maximum. If you specify 0, AWS Config uses the default.
dparLimit :: Lens' DescribePendingAggregationRequests (Maybe Natural)
dparLimit = lens _dparLimit (\ s a -> s{_dparLimit = a}) . mapping _Nat

instance AWSRequest
           DescribePendingAggregationRequests
         where
        type Rs DescribePendingAggregationRequests =
             DescribePendingAggregationRequestsResponse
        request = postJSON config
        response
          = receiveJSON
              (\ s h x ->
                 DescribePendingAggregationRequestsResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "PendingAggregationRequests" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribePendingAggregationRequests
         where

instance NFData DescribePendingAggregationRequests
         where

instance ToHeaders DescribePendingAggregationRequests
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StarlingDoveService.DescribePendingAggregationRequests"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribePendingAggregationRequests
         where
        toJSON DescribePendingAggregationRequests'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _dparNextToken,
                  ("Limit" .=) <$> _dparLimit])

instance ToPath DescribePendingAggregationRequests
         where
        toPath = const "/"

instance ToQuery DescribePendingAggregationRequests
         where
        toQuery = const mempty

-- | /See:/ 'describePendingAggregationRequestsResponse' smart constructor.
data DescribePendingAggregationRequestsResponse = DescribePendingAggregationRequestsResponse'
  { _dparrsNextToken                  :: !(Maybe Text)
  , _dparrsPendingAggregationRequests :: !(Maybe [PendingAggregationRequest])
  , _dparrsResponseStatus             :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribePendingAggregationRequestsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dparrsNextToken' - The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- * 'dparrsPendingAggregationRequests' - Returns a PendingAggregationRequests object.
--
-- * 'dparrsResponseStatus' - -- | The response status code.
describePendingAggregationRequestsResponse
    :: Int -- ^ 'dparrsResponseStatus'
    -> DescribePendingAggregationRequestsResponse
describePendingAggregationRequestsResponse pResponseStatus_ =
  DescribePendingAggregationRequestsResponse'
    { _dparrsNextToken = Nothing
    , _dparrsPendingAggregationRequests = Nothing
    , _dparrsResponseStatus = pResponseStatus_
    }


-- | The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
dparrsNextToken :: Lens' DescribePendingAggregationRequestsResponse (Maybe Text)
dparrsNextToken = lens _dparrsNextToken (\ s a -> s{_dparrsNextToken = a})

-- | Returns a PendingAggregationRequests object.
dparrsPendingAggregationRequests :: Lens' DescribePendingAggregationRequestsResponse [PendingAggregationRequest]
dparrsPendingAggregationRequests = lens _dparrsPendingAggregationRequests (\ s a -> s{_dparrsPendingAggregationRequests = a}) . _Default . _Coerce

-- | -- | The response status code.
dparrsResponseStatus :: Lens' DescribePendingAggregationRequestsResponse Int
dparrsResponseStatus = lens _dparrsResponseStatus (\ s a -> s{_dparrsResponseStatus = a})

instance NFData
           DescribePendingAggregationRequestsResponse
         where
