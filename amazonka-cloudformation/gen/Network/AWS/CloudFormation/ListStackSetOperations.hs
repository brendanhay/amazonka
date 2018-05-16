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
-- Module      : Network.AWS.CloudFormation.ListStackSetOperations
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns summary information about operations performed on a stack set.
--
--
module Network.AWS.CloudFormation.ListStackSetOperations
    (
    -- * Creating a Request
      listStackSetOperations
    , ListStackSetOperations
    -- * Request Lenses
    , lssoNextToken
    , lssoMaxResults
    , lssoStackSetName

    -- * Destructuring the Response
    , listStackSetOperationsResponse
    , ListStackSetOperationsResponse
    -- * Response Lenses
    , lssorsNextToken
    , lssorsSummaries
    , lssorsResponseStatus
    ) where

import Network.AWS.CloudFormation.Types
import Network.AWS.CloudFormation.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listStackSetOperations' smart constructor.
data ListStackSetOperations = ListStackSetOperations'
  { _lssoNextToken    :: !(Maybe Text)
  , _lssoMaxResults   :: !(Maybe Nat)
  , _lssoStackSetName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListStackSetOperations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lssoNextToken' - If the previous paginated request didn't return all of the remaining results, the response object's @NextToken@ parameter value is set to a token. To retrieve the next set of results, call @ListStackSetOperations@ again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, the previous response object's @NextToken@ parameter is set to @null@ .
--
-- * 'lssoMaxResults' - The maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
--
-- * 'lssoStackSetName' - The name or unique ID of the stack set that you want to get operation summaries for.
listStackSetOperations
    :: Text -- ^ 'lssoStackSetName'
    -> ListStackSetOperations
listStackSetOperations pStackSetName_ =
  ListStackSetOperations'
    { _lssoNextToken = Nothing
    , _lssoMaxResults = Nothing
    , _lssoStackSetName = pStackSetName_
    }


-- | If the previous paginated request didn't return all of the remaining results, the response object's @NextToken@ parameter value is set to a token. To retrieve the next set of results, call @ListStackSetOperations@ again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, the previous response object's @NextToken@ parameter is set to @null@ .
lssoNextToken :: Lens' ListStackSetOperations (Maybe Text)
lssoNextToken = lens _lssoNextToken (\ s a -> s{_lssoNextToken = a})

-- | The maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
lssoMaxResults :: Lens' ListStackSetOperations (Maybe Natural)
lssoMaxResults = lens _lssoMaxResults (\ s a -> s{_lssoMaxResults = a}) . mapping _Nat

-- | The name or unique ID of the stack set that you want to get operation summaries for.
lssoStackSetName :: Lens' ListStackSetOperations Text
lssoStackSetName = lens _lssoStackSetName (\ s a -> s{_lssoStackSetName = a})

instance AWSRequest ListStackSetOperations where
        type Rs ListStackSetOperations =
             ListStackSetOperationsResponse
        request = postQuery cloudFormation
        response
          = receiveXMLWrapper "ListStackSetOperationsResult"
              (\ s h x ->
                 ListStackSetOperationsResponse' <$>
                   (x .@? "NextToken") <*>
                     (x .@? "Summaries" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable ListStackSetOperations where

instance NFData ListStackSetOperations where

instance ToHeaders ListStackSetOperations where
        toHeaders = const mempty

instance ToPath ListStackSetOperations where
        toPath = const "/"

instance ToQuery ListStackSetOperations where
        toQuery ListStackSetOperations'{..}
          = mconcat
              ["Action" =:
                 ("ListStackSetOperations" :: ByteString),
               "Version" =: ("2010-05-15" :: ByteString),
               "NextToken" =: _lssoNextToken,
               "MaxResults" =: _lssoMaxResults,
               "StackSetName" =: _lssoStackSetName]

-- | /See:/ 'listStackSetOperationsResponse' smart constructor.
data ListStackSetOperationsResponse = ListStackSetOperationsResponse'
  { _lssorsNextToken      :: !(Maybe Text)
  , _lssorsSummaries      :: !(Maybe [StackSetOperationSummary])
  , _lssorsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListStackSetOperationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lssorsNextToken' - If the request doesn't return all results, @NextToken@ is set to a token. To retrieve the next set of results, call @ListOperationResults@ again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, @NextToken@ is set to @null@ .
--
-- * 'lssorsSummaries' - A list of @StackSetOperationSummary@ structures that contain summary information about operations for the specified stack set.
--
-- * 'lssorsResponseStatus' - -- | The response status code.
listStackSetOperationsResponse
    :: Int -- ^ 'lssorsResponseStatus'
    -> ListStackSetOperationsResponse
listStackSetOperationsResponse pResponseStatus_ =
  ListStackSetOperationsResponse'
    { _lssorsNextToken = Nothing
    , _lssorsSummaries = Nothing
    , _lssorsResponseStatus = pResponseStatus_
    }


-- | If the request doesn't return all results, @NextToken@ is set to a token. To retrieve the next set of results, call @ListOperationResults@ again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, @NextToken@ is set to @null@ .
lssorsNextToken :: Lens' ListStackSetOperationsResponse (Maybe Text)
lssorsNextToken = lens _lssorsNextToken (\ s a -> s{_lssorsNextToken = a})

-- | A list of @StackSetOperationSummary@ structures that contain summary information about operations for the specified stack set.
lssorsSummaries :: Lens' ListStackSetOperationsResponse [StackSetOperationSummary]
lssorsSummaries = lens _lssorsSummaries (\ s a -> s{_lssorsSummaries = a}) . _Default . _Coerce

-- | -- | The response status code.
lssorsResponseStatus :: Lens' ListStackSetOperationsResponse Int
lssorsResponseStatus = lens _lssorsResponseStatus (\ s a -> s{_lssorsResponseStatus = a})

instance NFData ListStackSetOperationsResponse where
