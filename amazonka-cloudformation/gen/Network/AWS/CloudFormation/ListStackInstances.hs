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
-- Module      : Network.AWS.CloudFormation.ListStackInstances
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns summary information about stack instances that are associated with the specified stack set. You can filter for stack instances that are associated with a specific AWS account name or region.
--
--
module Network.AWS.CloudFormation.ListStackInstances
    (
    -- * Creating a Request
      listStackInstances
    , ListStackInstances
    -- * Request Lenses
    , lsiStackInstanceRegion
    , lsiNextToken
    , lsiStackInstanceAccount
    , lsiMaxResults
    , lsiStackSetName

    -- * Destructuring the Response
    , listStackInstancesResponse
    , ListStackInstancesResponse
    -- * Response Lenses
    , lsirsNextToken
    , lsirsSummaries
    , lsirsResponseStatus
    ) where

import Network.AWS.CloudFormation.Types
import Network.AWS.CloudFormation.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listStackInstances' smart constructor.
data ListStackInstances = ListStackInstances'
  { _lsiStackInstanceRegion  :: !(Maybe Text)
  , _lsiNextToken            :: !(Maybe Text)
  , _lsiStackInstanceAccount :: !(Maybe Text)
  , _lsiMaxResults           :: !(Maybe Nat)
  , _lsiStackSetName         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListStackInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsiStackInstanceRegion' - The name of the region where you want to list stack instances.
--
-- * 'lsiNextToken' - If the previous request didn't return all of the remaining results, the response's @NextToken@ parameter value is set to a token. To retrieve the next set of results, call @ListStackInstances@ again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, the previous response object's @NextToken@ parameter is set to @null@ .
--
-- * 'lsiStackInstanceAccount' - The name of the AWS account that you want to list stack instances for.
--
-- * 'lsiMaxResults' - The maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
--
-- * 'lsiStackSetName' - The name or unique ID of the stack set that you want to list stack instances for.
listStackInstances
    :: Text -- ^ 'lsiStackSetName'
    -> ListStackInstances
listStackInstances pStackSetName_ =
  ListStackInstances'
    { _lsiStackInstanceRegion = Nothing
    , _lsiNextToken = Nothing
    , _lsiStackInstanceAccount = Nothing
    , _lsiMaxResults = Nothing
    , _lsiStackSetName = pStackSetName_
    }


-- | The name of the region where you want to list stack instances.
lsiStackInstanceRegion :: Lens' ListStackInstances (Maybe Text)
lsiStackInstanceRegion = lens _lsiStackInstanceRegion (\ s a -> s{_lsiStackInstanceRegion = a})

-- | If the previous request didn't return all of the remaining results, the response's @NextToken@ parameter value is set to a token. To retrieve the next set of results, call @ListStackInstances@ again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, the previous response object's @NextToken@ parameter is set to @null@ .
lsiNextToken :: Lens' ListStackInstances (Maybe Text)
lsiNextToken = lens _lsiNextToken (\ s a -> s{_lsiNextToken = a})

-- | The name of the AWS account that you want to list stack instances for.
lsiStackInstanceAccount :: Lens' ListStackInstances (Maybe Text)
lsiStackInstanceAccount = lens _lsiStackInstanceAccount (\ s a -> s{_lsiStackInstanceAccount = a})

-- | The maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
lsiMaxResults :: Lens' ListStackInstances (Maybe Natural)
lsiMaxResults = lens _lsiMaxResults (\ s a -> s{_lsiMaxResults = a}) . mapping _Nat

-- | The name or unique ID of the stack set that you want to list stack instances for.
lsiStackSetName :: Lens' ListStackInstances Text
lsiStackSetName = lens _lsiStackSetName (\ s a -> s{_lsiStackSetName = a})

instance AWSRequest ListStackInstances where
        type Rs ListStackInstances =
             ListStackInstancesResponse
        request = postQuery cloudFormation
        response
          = receiveXMLWrapper "ListStackInstancesResult"
              (\ s h x ->
                 ListStackInstancesResponse' <$>
                   (x .@? "NextToken") <*>
                     (x .@? "Summaries" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable ListStackInstances where

instance NFData ListStackInstances where

instance ToHeaders ListStackInstances where
        toHeaders = const mempty

instance ToPath ListStackInstances where
        toPath = const "/"

instance ToQuery ListStackInstances where
        toQuery ListStackInstances'{..}
          = mconcat
              ["Action" =: ("ListStackInstances" :: ByteString),
               "Version" =: ("2010-05-15" :: ByteString),
               "StackInstanceRegion" =: _lsiStackInstanceRegion,
               "NextToken" =: _lsiNextToken,
               "StackInstanceAccount" =: _lsiStackInstanceAccount,
               "MaxResults" =: _lsiMaxResults,
               "StackSetName" =: _lsiStackSetName]

-- | /See:/ 'listStackInstancesResponse' smart constructor.
data ListStackInstancesResponse = ListStackInstancesResponse'
  { _lsirsNextToken      :: !(Maybe Text)
  , _lsirsSummaries      :: !(Maybe [StackInstanceSummary])
  , _lsirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListStackInstancesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsirsNextToken' - If the request doesn't return all of the remaining results, @NextToken@ is set to a token. To retrieve the next set of results, call @ListStackInstances@ again and assign that token to the request object's @NextToken@ parameter. If the request returns all results, @NextToken@ is set to @null@ .
--
-- * 'lsirsSummaries' - A list of @StackInstanceSummary@ structures that contain information about the specified stack instances.
--
-- * 'lsirsResponseStatus' - -- | The response status code.
listStackInstancesResponse
    :: Int -- ^ 'lsirsResponseStatus'
    -> ListStackInstancesResponse
listStackInstancesResponse pResponseStatus_ =
  ListStackInstancesResponse'
    { _lsirsNextToken = Nothing
    , _lsirsSummaries = Nothing
    , _lsirsResponseStatus = pResponseStatus_
    }


-- | If the request doesn't return all of the remaining results, @NextToken@ is set to a token. To retrieve the next set of results, call @ListStackInstances@ again and assign that token to the request object's @NextToken@ parameter. If the request returns all results, @NextToken@ is set to @null@ .
lsirsNextToken :: Lens' ListStackInstancesResponse (Maybe Text)
lsirsNextToken = lens _lsirsNextToken (\ s a -> s{_lsirsNextToken = a})

-- | A list of @StackInstanceSummary@ structures that contain information about the specified stack instances.
lsirsSummaries :: Lens' ListStackInstancesResponse [StackInstanceSummary]
lsirsSummaries = lens _lsirsSummaries (\ s a -> s{_lsirsSummaries = a}) . _Default . _Coerce

-- | -- | The response status code.
lsirsResponseStatus :: Lens' ListStackInstancesResponse Int
lsirsResponseStatus = lens _lsirsResponseStatus (\ s a -> s{_lsirsResponseStatus = a})

instance NFData ListStackInstancesResponse where
