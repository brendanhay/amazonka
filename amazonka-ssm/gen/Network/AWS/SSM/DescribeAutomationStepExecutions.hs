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
-- Module      : Network.AWS.SSM.DescribeAutomationStepExecutions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Information about all active and terminated step executions in an Automation workflow.
--
--
module Network.AWS.SSM.DescribeAutomationStepExecutions
    (
    -- * Creating a Request
      describeAutomationStepExecutions
    , DescribeAutomationStepExecutions
    -- * Request Lenses
    , daseFilters
    , daseReverseOrder
    , daseNextToken
    , daseMaxResults
    , daseAutomationExecutionId

    -- * Destructuring the Response
    , describeAutomationStepExecutionsResponse
    , DescribeAutomationStepExecutionsResponse
    -- * Response Lenses
    , dasersNextToken
    , dasersStepExecutions
    , dasersResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'describeAutomationStepExecutions' smart constructor.
data DescribeAutomationStepExecutions = DescribeAutomationStepExecutions'
  { _daseFilters               :: !(Maybe (List1 StepExecutionFilter))
  , _daseReverseOrder          :: !(Maybe Bool)
  , _daseNextToken             :: !(Maybe Text)
  , _daseMaxResults            :: !(Maybe Nat)
  , _daseAutomationExecutionId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAutomationStepExecutions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daseFilters' - One or more filters to limit the number of step executions returned by the request.
--
-- * 'daseReverseOrder' - A boolean that indicates whether to list step executions in reverse order by start time. The default value is false.
--
-- * 'daseNextToken' - The token for the next set of items to return. (You received this token from a previous call.)
--
-- * 'daseMaxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- * 'daseAutomationExecutionId' - The Automation execution ID for which you want step execution descriptions.
describeAutomationStepExecutions
    :: Text -- ^ 'daseAutomationExecutionId'
    -> DescribeAutomationStepExecutions
describeAutomationStepExecutions pAutomationExecutionId_ =
  DescribeAutomationStepExecutions'
    { _daseFilters = Nothing
    , _daseReverseOrder = Nothing
    , _daseNextToken = Nothing
    , _daseMaxResults = Nothing
    , _daseAutomationExecutionId = pAutomationExecutionId_
    }


-- | One or more filters to limit the number of step executions returned by the request.
daseFilters :: Lens' DescribeAutomationStepExecutions (Maybe (NonEmpty StepExecutionFilter))
daseFilters = lens _daseFilters (\ s a -> s{_daseFilters = a}) . mapping _List1

-- | A boolean that indicates whether to list step executions in reverse order by start time. The default value is false.
daseReverseOrder :: Lens' DescribeAutomationStepExecutions (Maybe Bool)
daseReverseOrder = lens _daseReverseOrder (\ s a -> s{_daseReverseOrder = a})

-- | The token for the next set of items to return. (You received this token from a previous call.)
daseNextToken :: Lens' DescribeAutomationStepExecutions (Maybe Text)
daseNextToken = lens _daseNextToken (\ s a -> s{_daseNextToken = a})

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
daseMaxResults :: Lens' DescribeAutomationStepExecutions (Maybe Natural)
daseMaxResults = lens _daseMaxResults (\ s a -> s{_daseMaxResults = a}) . mapping _Nat

-- | The Automation execution ID for which you want step execution descriptions.
daseAutomationExecutionId :: Lens' DescribeAutomationStepExecutions Text
daseAutomationExecutionId = lens _daseAutomationExecutionId (\ s a -> s{_daseAutomationExecutionId = a})

instance AWSRequest DescribeAutomationStepExecutions
         where
        type Rs DescribeAutomationStepExecutions =
             DescribeAutomationStepExecutionsResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 DescribeAutomationStepExecutionsResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "StepExecutions" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeAutomationStepExecutions
         where

instance NFData DescribeAutomationStepExecutions
         where

instance ToHeaders DescribeAutomationStepExecutions
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.DescribeAutomationStepExecutions" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeAutomationStepExecutions
         where
        toJSON DescribeAutomationStepExecutions'{..}
          = object
              (catMaybes
                 [("Filters" .=) <$> _daseFilters,
                  ("ReverseOrder" .=) <$> _daseReverseOrder,
                  ("NextToken" .=) <$> _daseNextToken,
                  ("MaxResults" .=) <$> _daseMaxResults,
                  Just
                    ("AutomationExecutionId" .=
                       _daseAutomationExecutionId)])

instance ToPath DescribeAutomationStepExecutions
         where
        toPath = const "/"

instance ToQuery DescribeAutomationStepExecutions
         where
        toQuery = const mempty

-- | /See:/ 'describeAutomationStepExecutionsResponse' smart constructor.
data DescribeAutomationStepExecutionsResponse = DescribeAutomationStepExecutionsResponse'
  { _dasersNextToken      :: !(Maybe Text)
  , _dasersStepExecutions :: !(Maybe [StepExecution])
  , _dasersResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAutomationStepExecutionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dasersNextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- * 'dasersStepExecutions' - A list of details about the current state of all steps that make up an execution.
--
-- * 'dasersResponseStatus' - -- | The response status code.
describeAutomationStepExecutionsResponse
    :: Int -- ^ 'dasersResponseStatus'
    -> DescribeAutomationStepExecutionsResponse
describeAutomationStepExecutionsResponse pResponseStatus_ =
  DescribeAutomationStepExecutionsResponse'
    { _dasersNextToken = Nothing
    , _dasersStepExecutions = Nothing
    , _dasersResponseStatus = pResponseStatus_
    }


-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
dasersNextToken :: Lens' DescribeAutomationStepExecutionsResponse (Maybe Text)
dasersNextToken = lens _dasersNextToken (\ s a -> s{_dasersNextToken = a})

-- | A list of details about the current state of all steps that make up an execution.
dasersStepExecutions :: Lens' DescribeAutomationStepExecutionsResponse [StepExecution]
dasersStepExecutions = lens _dasersStepExecutions (\ s a -> s{_dasersStepExecutions = a}) . _Default . _Coerce

-- | -- | The response status code.
dasersResponseStatus :: Lens' DescribeAutomationStepExecutionsResponse Int
dasersResponseStatus = lens _dasersResponseStatus (\ s a -> s{_dasersResponseStatus = a})

instance NFData
           DescribeAutomationStepExecutionsResponse
         where
