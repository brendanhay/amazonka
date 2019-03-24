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
-- Module      : Network.AWS.SSM.DescribeAssociationExecutions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this API action to view all executions for a specific association ID.
--
--
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeAssociationExecutions
    (
    -- * Creating a Request
      describeAssociationExecutions
    , DescribeAssociationExecutions
    -- * Request Lenses
    , daeFilters
    , daeNextToken
    , daeMaxResults
    , daeAssociationId

    -- * Destructuring the Response
    , describeAssociationExecutionsResponse
    , DescribeAssociationExecutionsResponse
    -- * Response Lenses
    , daersNextToken
    , daersAssociationExecutions
    , daersResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'describeAssociationExecutions' smart constructor.
data DescribeAssociationExecutions = DescribeAssociationExecutions'
  { _daeFilters       :: !(Maybe (List1 AssociationExecutionFilter))
  , _daeNextToken     :: !(Maybe Text)
  , _daeMaxResults    :: !(Maybe Nat)
  , _daeAssociationId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAssociationExecutions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daeFilters' - Filters for the request. You can specify the following filters and values. ExecutionId (EQUAL) Status (EQUAL) CreatedTime (EQUAL, GREATER_THAN, LESS_THAN)
--
-- * 'daeNextToken' - A token to start the list. Use this token to get the next set of results.
--
-- * 'daeMaxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- * 'daeAssociationId' - The association ID for which you want to view execution history details.
describeAssociationExecutions
    :: Text -- ^ 'daeAssociationId'
    -> DescribeAssociationExecutions
describeAssociationExecutions pAssociationId_ =
  DescribeAssociationExecutions'
    { _daeFilters = Nothing
    , _daeNextToken = Nothing
    , _daeMaxResults = Nothing
    , _daeAssociationId = pAssociationId_
    }


-- | Filters for the request. You can specify the following filters and values. ExecutionId (EQUAL) Status (EQUAL) CreatedTime (EQUAL, GREATER_THAN, LESS_THAN)
daeFilters :: Lens' DescribeAssociationExecutions (Maybe (NonEmpty AssociationExecutionFilter))
daeFilters = lens _daeFilters (\ s a -> s{_daeFilters = a}) . mapping _List1

-- | A token to start the list. Use this token to get the next set of results.
daeNextToken :: Lens' DescribeAssociationExecutions (Maybe Text)
daeNextToken = lens _daeNextToken (\ s a -> s{_daeNextToken = a})

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
daeMaxResults :: Lens' DescribeAssociationExecutions (Maybe Natural)
daeMaxResults = lens _daeMaxResults (\ s a -> s{_daeMaxResults = a}) . mapping _Nat

-- | The association ID for which you want to view execution history details.
daeAssociationId :: Lens' DescribeAssociationExecutions Text
daeAssociationId = lens _daeAssociationId (\ s a -> s{_daeAssociationId = a})

instance AWSPager DescribeAssociationExecutions where
        page rq rs
          | stop (rs ^. daersNextToken) = Nothing
          | stop (rs ^. daersAssociationExecutions) = Nothing
          | otherwise =
            Just $ rq & daeNextToken .~ rs ^. daersNextToken

instance AWSRequest DescribeAssociationExecutions
         where
        type Rs DescribeAssociationExecutions =
             DescribeAssociationExecutionsResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 DescribeAssociationExecutionsResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "AssociationExecutions" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeAssociationExecutions where

instance NFData DescribeAssociationExecutions where

instance ToHeaders DescribeAssociationExecutions
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.DescribeAssociationExecutions" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeAssociationExecutions where
        toJSON DescribeAssociationExecutions'{..}
          = object
              (catMaybes
                 [("Filters" .=) <$> _daeFilters,
                  ("NextToken" .=) <$> _daeNextToken,
                  ("MaxResults" .=) <$> _daeMaxResults,
                  Just ("AssociationId" .= _daeAssociationId)])

instance ToPath DescribeAssociationExecutions where
        toPath = const "/"

instance ToQuery DescribeAssociationExecutions where
        toQuery = const mempty

-- | /See:/ 'describeAssociationExecutionsResponse' smart constructor.
data DescribeAssociationExecutionsResponse = DescribeAssociationExecutionsResponse'
  { _daersNextToken             :: !(Maybe Text)
  , _daersAssociationExecutions :: !(Maybe [AssociationExecution])
  , _daersResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAssociationExecutionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daersNextToken' - The token for the next set of items to return. Use this token to get the next set of results.
--
-- * 'daersAssociationExecutions' - A list of the executions for the specified association ID.
--
-- * 'daersResponseStatus' - -- | The response status code.
describeAssociationExecutionsResponse
    :: Int -- ^ 'daersResponseStatus'
    -> DescribeAssociationExecutionsResponse
describeAssociationExecutionsResponse pResponseStatus_ =
  DescribeAssociationExecutionsResponse'
    { _daersNextToken = Nothing
    , _daersAssociationExecutions = Nothing
    , _daersResponseStatus = pResponseStatus_
    }


-- | The token for the next set of items to return. Use this token to get the next set of results.
daersNextToken :: Lens' DescribeAssociationExecutionsResponse (Maybe Text)
daersNextToken = lens _daersNextToken (\ s a -> s{_daersNextToken = a})

-- | A list of the executions for the specified association ID.
daersAssociationExecutions :: Lens' DescribeAssociationExecutionsResponse [AssociationExecution]
daersAssociationExecutions = lens _daersAssociationExecutions (\ s a -> s{_daersAssociationExecutions = a}) . _Default . _Coerce

-- | -- | The response status code.
daersResponseStatus :: Lens' DescribeAssociationExecutionsResponse Int
daersResponseStatus = lens _daersResponseStatus (\ s a -> s{_daersResponseStatus = a})

instance NFData DescribeAssociationExecutionsResponse
         where
