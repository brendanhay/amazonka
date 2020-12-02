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
-- Module      : Network.AWS.CloudFormation.ListStackSets
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns summary information about stack sets that are associated with the user.
--
--
module Network.AWS.CloudFormation.ListStackSets
    (
    -- * Creating a Request
      listStackSets
    , ListStackSets
    -- * Request Lenses
    , lssStatus
    , lssNextToken
    , lssMaxResults

    -- * Destructuring the Response
    , listStackSetsResponse
    , ListStackSetsResponse
    -- * Response Lenses
    , lssrsNextToken
    , lssrsSummaries
    , lssrsResponseStatus
    ) where

import Network.AWS.CloudFormation.Types
import Network.AWS.CloudFormation.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listStackSets' smart constructor.
data ListStackSets = ListStackSets'
  { _lssStatus     :: !(Maybe StackSetStatus)
  , _lssNextToken  :: !(Maybe Text)
  , _lssMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListStackSets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lssStatus' - The status of the stack sets that you want to get summary information about.
--
-- * 'lssNextToken' - If the previous paginated request didn't return all of the remaining results, the response object's @NextToken@ parameter value is set to a token. To retrieve the next set of results, call @ListStackSets@ again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, the previous response object's @NextToken@ parameter is set to @null@ .
--
-- * 'lssMaxResults' - The maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
listStackSets
    :: ListStackSets
listStackSets =
  ListStackSets'
    {_lssStatus = Nothing, _lssNextToken = Nothing, _lssMaxResults = Nothing}


-- | The status of the stack sets that you want to get summary information about.
lssStatus :: Lens' ListStackSets (Maybe StackSetStatus)
lssStatus = lens _lssStatus (\ s a -> s{_lssStatus = a})

-- | If the previous paginated request didn't return all of the remaining results, the response object's @NextToken@ parameter value is set to a token. To retrieve the next set of results, call @ListStackSets@ again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, the previous response object's @NextToken@ parameter is set to @null@ .
lssNextToken :: Lens' ListStackSets (Maybe Text)
lssNextToken = lens _lssNextToken (\ s a -> s{_lssNextToken = a})

-- | The maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
lssMaxResults :: Lens' ListStackSets (Maybe Natural)
lssMaxResults = lens _lssMaxResults (\ s a -> s{_lssMaxResults = a}) . mapping _Nat

instance AWSRequest ListStackSets where
        type Rs ListStackSets = ListStackSetsResponse
        request = postQuery cloudFormation
        response
          = receiveXMLWrapper "ListStackSetsResult"
              (\ s h x ->
                 ListStackSetsResponse' <$>
                   (x .@? "NextToken") <*>
                     (x .@? "Summaries" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable ListStackSets where

instance NFData ListStackSets where

instance ToHeaders ListStackSets where
        toHeaders = const mempty

instance ToPath ListStackSets where
        toPath = const "/"

instance ToQuery ListStackSets where
        toQuery ListStackSets'{..}
          = mconcat
              ["Action" =: ("ListStackSets" :: ByteString),
               "Version" =: ("2010-05-15" :: ByteString),
               "Status" =: _lssStatus, "NextToken" =: _lssNextToken,
               "MaxResults" =: _lssMaxResults]

-- | /See:/ 'listStackSetsResponse' smart constructor.
data ListStackSetsResponse = ListStackSetsResponse'
  { _lssrsNextToken      :: !(Maybe Text)
  , _lssrsSummaries      :: !(Maybe [StackSetSummary])
  , _lssrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListStackSetsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lssrsNextToken' - If the request doesn't return all of the remaining results, @NextToken@ is set to a token. To retrieve the next set of results, call @ListStackInstances@ again and assign that token to the request object's @NextToken@ parameter. If the request returns all results, @NextToken@ is set to @null@ .
--
-- * 'lssrsSummaries' - A list of @StackSetSummary@ structures that contain information about the user's stack sets.
--
-- * 'lssrsResponseStatus' - -- | The response status code.
listStackSetsResponse
    :: Int -- ^ 'lssrsResponseStatus'
    -> ListStackSetsResponse
listStackSetsResponse pResponseStatus_ =
  ListStackSetsResponse'
    { _lssrsNextToken = Nothing
    , _lssrsSummaries = Nothing
    , _lssrsResponseStatus = pResponseStatus_
    }


-- | If the request doesn't return all of the remaining results, @NextToken@ is set to a token. To retrieve the next set of results, call @ListStackInstances@ again and assign that token to the request object's @NextToken@ parameter. If the request returns all results, @NextToken@ is set to @null@ .
lssrsNextToken :: Lens' ListStackSetsResponse (Maybe Text)
lssrsNextToken = lens _lssrsNextToken (\ s a -> s{_lssrsNextToken = a})

-- | A list of @StackSetSummary@ structures that contain information about the user's stack sets.
lssrsSummaries :: Lens' ListStackSetsResponse [StackSetSummary]
lssrsSummaries = lens _lssrsSummaries (\ s a -> s{_lssrsSummaries = a}) . _Default . _Coerce

-- | -- | The response status code.
lssrsResponseStatus :: Lens' ListStackSetsResponse Int
lssrsResponseStatus = lens _lssrsResponseStatus (\ s a -> s{_lssrsResponseStatus = a})

instance NFData ListStackSetsResponse where
