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
-- Module      : Network.AWS.Route53AutoNaming.ListOperations
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists operations that match the criteria that you specify.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Route53AutoNaming.ListOperations
    (
    -- * Creating a Request
      listOperations
    , ListOperations
    -- * Request Lenses
    , loFilters
    , loNextToken
    , loMaxResults

    -- * Destructuring the Response
    , listOperationsResponse
    , ListOperationsResponse
    -- * Response Lenses
    , lorsNextToken
    , lorsOperations
    , lorsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53AutoNaming.Types
import Network.AWS.Route53AutoNaming.Types.Product

-- | /See:/ 'listOperations' smart constructor.
data ListOperations = ListOperations'
  { _loFilters    :: !(Maybe [OperationFilter])
  , _loNextToken  :: !(Maybe Text)
  , _loMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListOperations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'loFilters' - A complex type that contains specifications for the operations that you want to list, for example, operations that you started between a specified start date and end date. If you specify more than one filter, an operation must match all filters to be returned by @ListOperations@ .
--
-- * 'loNextToken' - For the first @ListOperations@ request, omit this value. If the response contains @NextToken@ , submit another @ListOperations@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
--
-- * 'loMaxResults' - The maximum number of items that you want Amazon Route 53 to return in the response to a @ListOperations@ request. If you don't specify a value for @MaxResults@ , Route 53 returns up to 100 operations.
listOperations
    :: ListOperations
listOperations =
  ListOperations'
    {_loFilters = Nothing, _loNextToken = Nothing, _loMaxResults = Nothing}


-- | A complex type that contains specifications for the operations that you want to list, for example, operations that you started between a specified start date and end date. If you specify more than one filter, an operation must match all filters to be returned by @ListOperations@ .
loFilters :: Lens' ListOperations [OperationFilter]
loFilters = lens _loFilters (\ s a -> s{_loFilters = a}) . _Default . _Coerce

-- | For the first @ListOperations@ request, omit this value. If the response contains @NextToken@ , submit another @ListOperations@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
loNextToken :: Lens' ListOperations (Maybe Text)
loNextToken = lens _loNextToken (\ s a -> s{_loNextToken = a})

-- | The maximum number of items that you want Amazon Route 53 to return in the response to a @ListOperations@ request. If you don't specify a value for @MaxResults@ , Route 53 returns up to 100 operations.
loMaxResults :: Lens' ListOperations (Maybe Natural)
loMaxResults = lens _loMaxResults (\ s a -> s{_loMaxResults = a}) . mapping _Nat

instance AWSPager ListOperations where
        page rq rs
          | stop (rs ^. lorsNextToken) = Nothing
          | stop (rs ^. lorsOperations) = Nothing
          | otherwise =
            Just $ rq & loNextToken .~ rs ^. lorsNextToken

instance AWSRequest ListOperations where
        type Rs ListOperations = ListOperationsResponse
        request = postJSON route53AutoNaming
        response
          = receiveJSON
              (\ s h x ->
                 ListOperationsResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "Operations" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListOperations where

instance NFData ListOperations where

instance ToHeaders ListOperations where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Route53AutoNaming_v20170314.ListOperations" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListOperations where
        toJSON ListOperations'{..}
          = object
              (catMaybes
                 [("Filters" .=) <$> _loFilters,
                  ("NextToken" .=) <$> _loNextToken,
                  ("MaxResults" .=) <$> _loMaxResults])

instance ToPath ListOperations where
        toPath = const "/"

instance ToQuery ListOperations where
        toQuery = const mempty

-- | /See:/ 'listOperationsResponse' smart constructor.
data ListOperationsResponse = ListOperationsResponse'
  { _lorsNextToken      :: !(Maybe Text)
  , _lorsOperations     :: !(Maybe [OperationSummary])
  , _lorsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListOperationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lorsNextToken' - If the response contains @NextToken@ , submit another @ListOperations@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
--
-- * 'lorsOperations' - Summary information about the operations that match the specified criteria.
--
-- * 'lorsResponseStatus' - -- | The response status code.
listOperationsResponse
    :: Int -- ^ 'lorsResponseStatus'
    -> ListOperationsResponse
listOperationsResponse pResponseStatus_ =
  ListOperationsResponse'
    { _lorsNextToken = Nothing
    , _lorsOperations = Nothing
    , _lorsResponseStatus = pResponseStatus_
    }


-- | If the response contains @NextToken@ , submit another @ListOperations@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
lorsNextToken :: Lens' ListOperationsResponse (Maybe Text)
lorsNextToken = lens _lorsNextToken (\ s a -> s{_lorsNextToken = a})

-- | Summary information about the operations that match the specified criteria.
lorsOperations :: Lens' ListOperationsResponse [OperationSummary]
lorsOperations = lens _lorsOperations (\ s a -> s{_lorsOperations = a}) . _Default . _Coerce

-- | -- | The response status code.
lorsResponseStatus :: Lens' ListOperationsResponse Int
lorsResponseStatus = lens _lorsResponseStatus (\ s a -> s{_lorsResponseStatus = a})

instance NFData ListOperationsResponse where
