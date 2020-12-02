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
-- Module      : Network.AWS.CloudFormation.ListStackSetOperationResults
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns summary information about the results of a stack set operation.
--
--
module Network.AWS.CloudFormation.ListStackSetOperationResults
    (
    -- * Creating a Request
      listStackSetOperationResults
    , ListStackSetOperationResults
    -- * Request Lenses
    , lssorNextToken
    , lssorMaxResults
    , lssorStackSetName
    , lssorOperationId

    -- * Destructuring the Response
    , listStackSetOperationResultsResponse
    , ListStackSetOperationResultsResponse
    -- * Response Lenses
    , lssorrsNextToken
    , lssorrsSummaries
    , lssorrsResponseStatus
    ) where

import Network.AWS.CloudFormation.Types
import Network.AWS.CloudFormation.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listStackSetOperationResults' smart constructor.
data ListStackSetOperationResults = ListStackSetOperationResults'
  { _lssorNextToken    :: !(Maybe Text)
  , _lssorMaxResults   :: !(Maybe Nat)
  , _lssorStackSetName :: !Text
  , _lssorOperationId  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListStackSetOperationResults' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lssorNextToken' - If the previous request didn't return all of the remaining results, the response object's @NextToken@ parameter value is set to a token. To retrieve the next set of results, call @ListStackSetOperationResults@ again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, the previous response object's @NextToken@ parameter is set to @null@ .
--
-- * 'lssorMaxResults' - The maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
--
-- * 'lssorStackSetName' - The name or unique ID of the stack set that you want to get operation results for.
--
-- * 'lssorOperationId' - The ID of the stack set operation.
listStackSetOperationResults
    :: Text -- ^ 'lssorStackSetName'
    -> Text -- ^ 'lssorOperationId'
    -> ListStackSetOperationResults
listStackSetOperationResults pStackSetName_ pOperationId_ =
  ListStackSetOperationResults'
    { _lssorNextToken = Nothing
    , _lssorMaxResults = Nothing
    , _lssorStackSetName = pStackSetName_
    , _lssorOperationId = pOperationId_
    }


-- | If the previous request didn't return all of the remaining results, the response object's @NextToken@ parameter value is set to a token. To retrieve the next set of results, call @ListStackSetOperationResults@ again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, the previous response object's @NextToken@ parameter is set to @null@ .
lssorNextToken :: Lens' ListStackSetOperationResults (Maybe Text)
lssorNextToken = lens _lssorNextToken (\ s a -> s{_lssorNextToken = a})

-- | The maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
lssorMaxResults :: Lens' ListStackSetOperationResults (Maybe Natural)
lssorMaxResults = lens _lssorMaxResults (\ s a -> s{_lssorMaxResults = a}) . mapping _Nat

-- | The name or unique ID of the stack set that you want to get operation results for.
lssorStackSetName :: Lens' ListStackSetOperationResults Text
lssorStackSetName = lens _lssorStackSetName (\ s a -> s{_lssorStackSetName = a})

-- | The ID of the stack set operation.
lssorOperationId :: Lens' ListStackSetOperationResults Text
lssorOperationId = lens _lssorOperationId (\ s a -> s{_lssorOperationId = a})

instance AWSRequest ListStackSetOperationResults
         where
        type Rs ListStackSetOperationResults =
             ListStackSetOperationResultsResponse
        request = postQuery cloudFormation
        response
          = receiveXMLWrapper
              "ListStackSetOperationResultsResult"
              (\ s h x ->
                 ListStackSetOperationResultsResponse' <$>
                   (x .@? "NextToken") <*>
                     (x .@? "Summaries" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable ListStackSetOperationResults where

instance NFData ListStackSetOperationResults where

instance ToHeaders ListStackSetOperationResults where
        toHeaders = const mempty

instance ToPath ListStackSetOperationResults where
        toPath = const "/"

instance ToQuery ListStackSetOperationResults where
        toQuery ListStackSetOperationResults'{..}
          = mconcat
              ["Action" =:
                 ("ListStackSetOperationResults" :: ByteString),
               "Version" =: ("2010-05-15" :: ByteString),
               "NextToken" =: _lssorNextToken,
               "MaxResults" =: _lssorMaxResults,
               "StackSetName" =: _lssorStackSetName,
               "OperationId" =: _lssorOperationId]

-- | /See:/ 'listStackSetOperationResultsResponse' smart constructor.
data ListStackSetOperationResultsResponse = ListStackSetOperationResultsResponse'
  { _lssorrsNextToken      :: !(Maybe Text)
  , _lssorrsSummaries      :: !(Maybe [StackSetOperationResultSummary])
  , _lssorrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListStackSetOperationResultsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lssorrsNextToken' - If the request doesn't return all results, @NextToken@ is set to a token. To retrieve the next set of results, call @ListOperationResults@ again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, @NextToken@ is set to @null@ .
--
-- * 'lssorrsSummaries' - A list of @StackSetOperationResultSummary@ structures that contain information about the specified operation results, for accounts and regions that are included in the operation.
--
-- * 'lssorrsResponseStatus' - -- | The response status code.
listStackSetOperationResultsResponse
    :: Int -- ^ 'lssorrsResponseStatus'
    -> ListStackSetOperationResultsResponse
listStackSetOperationResultsResponse pResponseStatus_ =
  ListStackSetOperationResultsResponse'
    { _lssorrsNextToken = Nothing
    , _lssorrsSummaries = Nothing
    , _lssorrsResponseStatus = pResponseStatus_
    }


-- | If the request doesn't return all results, @NextToken@ is set to a token. To retrieve the next set of results, call @ListOperationResults@ again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, @NextToken@ is set to @null@ .
lssorrsNextToken :: Lens' ListStackSetOperationResultsResponse (Maybe Text)
lssorrsNextToken = lens _lssorrsNextToken (\ s a -> s{_lssorrsNextToken = a})

-- | A list of @StackSetOperationResultSummary@ structures that contain information about the specified operation results, for accounts and regions that are included in the operation.
lssorrsSummaries :: Lens' ListStackSetOperationResultsResponse [StackSetOperationResultSummary]
lssorrsSummaries = lens _lssorrsSummaries (\ s a -> s{_lssorrsSummaries = a}) . _Default . _Coerce

-- | -- | The response status code.
lssorrsResponseStatus :: Lens' ListStackSetOperationResultsResponse Int
lssorrsResponseStatus = lens _lssorrsResponseStatus (\ s a -> s{_lssorrsResponseStatus = a})

instance NFData ListStackSetOperationResultsResponse
         where
