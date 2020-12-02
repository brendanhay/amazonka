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
-- Module      : Network.AWS.SSM.DescribeAutomationExecutions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides details about all active and terminated Automation executions.
--
--
module Network.AWS.SSM.DescribeAutomationExecutions
    (
    -- * Creating a Request
      describeAutomationExecutions
    , DescribeAutomationExecutions
    -- * Request Lenses
    , daeFilters
    , daeNextToken
    , daeMaxResults

    -- * Destructuring the Response
    , describeAutomationExecutionsResponse
    , DescribeAutomationExecutionsResponse
    -- * Response Lenses
    , daersNextToken
    , daersAutomationExecutionMetadataList
    , daersResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'describeAutomationExecutions' smart constructor.
data DescribeAutomationExecutions = DescribeAutomationExecutions'
  { _daeFilters    :: !(Maybe (List1 AutomationExecutionFilter))
  , _daeNextToken  :: !(Maybe Text)
  , _daeMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAutomationExecutions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daeFilters' - Filters used to limit the scope of executions that are requested.
--
-- * 'daeNextToken' - The token for the next set of items to return. (You received this token from a previous call.)
--
-- * 'daeMaxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
describeAutomationExecutions
    :: DescribeAutomationExecutions
describeAutomationExecutions =
  DescribeAutomationExecutions'
    {_daeFilters = Nothing, _daeNextToken = Nothing, _daeMaxResults = Nothing}


-- | Filters used to limit the scope of executions that are requested.
daeFilters :: Lens' DescribeAutomationExecutions (Maybe (NonEmpty AutomationExecutionFilter))
daeFilters = lens _daeFilters (\ s a -> s{_daeFilters = a}) . mapping _List1

-- | The token for the next set of items to return. (You received this token from a previous call.)
daeNextToken :: Lens' DescribeAutomationExecutions (Maybe Text)
daeNextToken = lens _daeNextToken (\ s a -> s{_daeNextToken = a})

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
daeMaxResults :: Lens' DescribeAutomationExecutions (Maybe Natural)
daeMaxResults = lens _daeMaxResults (\ s a -> s{_daeMaxResults = a}) . mapping _Nat

instance AWSRequest DescribeAutomationExecutions
         where
        type Rs DescribeAutomationExecutions =
             DescribeAutomationExecutionsResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 DescribeAutomationExecutionsResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "AutomationExecutionMetadataList" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeAutomationExecutions where

instance NFData DescribeAutomationExecutions where

instance ToHeaders DescribeAutomationExecutions where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.DescribeAutomationExecutions" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeAutomationExecutions where
        toJSON DescribeAutomationExecutions'{..}
          = object
              (catMaybes
                 [("Filters" .=) <$> _daeFilters,
                  ("NextToken" .=) <$> _daeNextToken,
                  ("MaxResults" .=) <$> _daeMaxResults])

instance ToPath DescribeAutomationExecutions where
        toPath = const "/"

instance ToQuery DescribeAutomationExecutions where
        toQuery = const mempty

-- | /See:/ 'describeAutomationExecutionsResponse' smart constructor.
data DescribeAutomationExecutionsResponse = DescribeAutomationExecutionsResponse'
  { _daersNextToken :: !(Maybe Text)
  , _daersAutomationExecutionMetadataList :: !(Maybe [AutomationExecutionMetadata])
  , _daersResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAutomationExecutionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daersNextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- * 'daersAutomationExecutionMetadataList' - The list of details about each automation execution which has occurred which matches the filter specification, if any.
--
-- * 'daersResponseStatus' - -- | The response status code.
describeAutomationExecutionsResponse
    :: Int -- ^ 'daersResponseStatus'
    -> DescribeAutomationExecutionsResponse
describeAutomationExecutionsResponse pResponseStatus_ =
  DescribeAutomationExecutionsResponse'
    { _daersNextToken = Nothing
    , _daersAutomationExecutionMetadataList = Nothing
    , _daersResponseStatus = pResponseStatus_
    }


-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
daersNextToken :: Lens' DescribeAutomationExecutionsResponse (Maybe Text)
daersNextToken = lens _daersNextToken (\ s a -> s{_daersNextToken = a})

-- | The list of details about each automation execution which has occurred which matches the filter specification, if any.
daersAutomationExecutionMetadataList :: Lens' DescribeAutomationExecutionsResponse [AutomationExecutionMetadata]
daersAutomationExecutionMetadataList = lens _daersAutomationExecutionMetadataList (\ s a -> s{_daersAutomationExecutionMetadataList = a}) . _Default . _Coerce

-- | -- | The response status code.
daersResponseStatus :: Lens' DescribeAutomationExecutionsResponse Int
daersResponseStatus = lens _daersResponseStatus (\ s a -> s{_daersResponseStatus = a})

instance NFData DescribeAutomationExecutionsResponse
         where
