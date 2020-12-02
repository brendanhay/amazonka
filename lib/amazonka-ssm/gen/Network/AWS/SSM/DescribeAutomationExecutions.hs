{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeAutomationExecutions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides details about all active and terminated Automation executions.
--
--
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeAutomationExecutions
  ( -- * Creating a Request
    describeAutomationExecutions,
    DescribeAutomationExecutions,

    -- * Request Lenses
    daesFilters,
    daesNextToken,
    daesMaxResults,

    -- * Destructuring the Response
    describeAutomationExecutionsResponse,
    DescribeAutomationExecutionsResponse,

    -- * Response Lenses
    daesrsNextToken,
    daesrsAutomationExecutionMetadataList,
    daesrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types

-- | /See:/ 'describeAutomationExecutions' smart constructor.
data DescribeAutomationExecutions = DescribeAutomationExecutions'
  { _daesFilters ::
      !( Maybe
           ( List1
               AutomationExecutionFilter
           )
       ),
    _daesNextToken :: !(Maybe Text),
    _daesMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeAutomationExecutions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daesFilters' - Filters used to limit the scope of executions that are requested.
--
-- * 'daesNextToken' - The token for the next set of items to return. (You received this token from a previous call.)
--
-- * 'daesMaxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
describeAutomationExecutions ::
  DescribeAutomationExecutions
describeAutomationExecutions =
  DescribeAutomationExecutions'
    { _daesFilters = Nothing,
      _daesNextToken = Nothing,
      _daesMaxResults = Nothing
    }

-- | Filters used to limit the scope of executions that are requested.
daesFilters :: Lens' DescribeAutomationExecutions (Maybe (NonEmpty AutomationExecutionFilter))
daesFilters = lens _daesFilters (\s a -> s {_daesFilters = a}) . mapping _List1

-- | The token for the next set of items to return. (You received this token from a previous call.)
daesNextToken :: Lens' DescribeAutomationExecutions (Maybe Text)
daesNextToken = lens _daesNextToken (\s a -> s {_daesNextToken = a})

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
daesMaxResults :: Lens' DescribeAutomationExecutions (Maybe Natural)
daesMaxResults = lens _daesMaxResults (\s a -> s {_daesMaxResults = a}) . mapping _Nat

instance AWSPager DescribeAutomationExecutions where
  page rq rs
    | stop (rs ^. daesrsNextToken) = Nothing
    | stop (rs ^. daesrsAutomationExecutionMetadataList) = Nothing
    | otherwise = Just $ rq & daesNextToken .~ rs ^. daesrsNextToken

instance AWSRequest DescribeAutomationExecutions where
  type
    Rs DescribeAutomationExecutions =
      DescribeAutomationExecutionsResponse
  request = postJSON ssm
  response =
    receiveJSON
      ( \s h x ->
          DescribeAutomationExecutionsResponse'
            <$> (x .?> "NextToken")
            <*> (x .?> "AutomationExecutionMetadataList" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeAutomationExecutions

instance NFData DescribeAutomationExecutions

instance ToHeaders DescribeAutomationExecutions where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AmazonSSM.DescribeAutomationExecutions" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeAutomationExecutions where
  toJSON DescribeAutomationExecutions' {..} =
    object
      ( catMaybes
          [ ("Filters" .=) <$> _daesFilters,
            ("NextToken" .=) <$> _daesNextToken,
            ("MaxResults" .=) <$> _daesMaxResults
          ]
      )

instance ToPath DescribeAutomationExecutions where
  toPath = const "/"

instance ToQuery DescribeAutomationExecutions where
  toQuery = const mempty

-- | /See:/ 'describeAutomationExecutionsResponse' smart constructor.
data DescribeAutomationExecutionsResponse = DescribeAutomationExecutionsResponse'
  { _daesrsNextToken ::
      !(Maybe Text),
    _daesrsAutomationExecutionMetadataList ::
      !( Maybe
           [AutomationExecutionMetadata]
       ),
    _daesrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeAutomationExecutionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daesrsNextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- * 'daesrsAutomationExecutionMetadataList' - The list of details about each automation execution which has occurred which matches the filter specification, if any.
--
-- * 'daesrsResponseStatus' - -- | The response status code.
describeAutomationExecutionsResponse ::
  -- | 'daesrsResponseStatus'
  Int ->
  DescribeAutomationExecutionsResponse
describeAutomationExecutionsResponse pResponseStatus_ =
  DescribeAutomationExecutionsResponse'
    { _daesrsNextToken = Nothing,
      _daesrsAutomationExecutionMetadataList = Nothing,
      _daesrsResponseStatus = pResponseStatus_
    }

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
daesrsNextToken :: Lens' DescribeAutomationExecutionsResponse (Maybe Text)
daesrsNextToken = lens _daesrsNextToken (\s a -> s {_daesrsNextToken = a})

-- | The list of details about each automation execution which has occurred which matches the filter specification, if any.
daesrsAutomationExecutionMetadataList :: Lens' DescribeAutomationExecutionsResponse [AutomationExecutionMetadata]
daesrsAutomationExecutionMetadataList = lens _daesrsAutomationExecutionMetadataList (\s a -> s {_daesrsAutomationExecutionMetadataList = a}) . _Default . _Coerce

-- | -- | The response status code.
daesrsResponseStatus :: Lens' DescribeAutomationExecutionsResponse Int
daesrsResponseStatus = lens _daesrsResponseStatus (\s a -> s {_daesrsResponseStatus = a})

instance NFData DescribeAutomationExecutionsResponse
