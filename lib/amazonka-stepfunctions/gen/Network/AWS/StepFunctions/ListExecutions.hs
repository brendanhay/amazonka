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
-- Module      : Network.AWS.StepFunctions.ListExecutions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the executions of a state machine that meet the filtering criteria. Results are sorted by time, with the most recent execution first.
--
--
-- If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
--
-- This API action is not supported by @EXPRESS@ state machines.
--
--
-- This operation returns paginated results.
module Network.AWS.StepFunctions.ListExecutions
  ( -- * Creating a Request
    listExecutions,
    ListExecutions,

    -- * Request Lenses
    leStatusFilter,
    leNextToken,
    leMaxResults,
    leStateMachineARN,

    -- * Destructuring the Response
    listExecutionsResponse,
    ListExecutionsResponse,

    -- * Response Lenses
    lersNextToken,
    lersResponseStatus,
    lersExecutions,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StepFunctions.Types

-- | /See:/ 'listExecutions' smart constructor.
data ListExecutions = ListExecutions'
  { _leStatusFilter ::
      !(Maybe ExecutionStatus),
    _leNextToken :: !(Maybe Text),
    _leMaxResults :: !(Maybe Nat),
    _leStateMachineARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListExecutions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'leStatusFilter' - If specified, only list the executions whose current execution status matches the given filter.
--
-- * 'leNextToken' - If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
--
-- * 'leMaxResults' - The maximum number of results that are returned per call. You can use @nextToken@ to obtain further pages of results. The default is 100 and the maximum allowed page size is 1000. A value of 0 uses the default. This is only an upper limit. The actual number of results returned per call might be fewer than the specified maximum.
--
-- * 'leStateMachineARN' - The Amazon Resource Name (ARN) of the state machine whose executions is listed.
listExecutions ::
  -- | 'leStateMachineARN'
  Text ->
  ListExecutions
listExecutions pStateMachineARN_ =
  ListExecutions'
    { _leStatusFilter = Nothing,
      _leNextToken = Nothing,
      _leMaxResults = Nothing,
      _leStateMachineARN = pStateMachineARN_
    }

-- | If specified, only list the executions whose current execution status matches the given filter.
leStatusFilter :: Lens' ListExecutions (Maybe ExecutionStatus)
leStatusFilter = lens _leStatusFilter (\s a -> s {_leStatusFilter = a})

-- | If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
leNextToken :: Lens' ListExecutions (Maybe Text)
leNextToken = lens _leNextToken (\s a -> s {_leNextToken = a})

-- | The maximum number of results that are returned per call. You can use @nextToken@ to obtain further pages of results. The default is 100 and the maximum allowed page size is 1000. A value of 0 uses the default. This is only an upper limit. The actual number of results returned per call might be fewer than the specified maximum.
leMaxResults :: Lens' ListExecutions (Maybe Natural)
leMaxResults = lens _leMaxResults (\s a -> s {_leMaxResults = a}) . mapping _Nat

-- | The Amazon Resource Name (ARN) of the state machine whose executions is listed.
leStateMachineARN :: Lens' ListExecutions Text
leStateMachineARN = lens _leStateMachineARN (\s a -> s {_leStateMachineARN = a})

instance AWSPager ListExecutions where
  page rq rs
    | stop (rs ^. lersNextToken) = Nothing
    | stop (rs ^. lersExecutions) = Nothing
    | otherwise = Just $ rq & leNextToken .~ rs ^. lersNextToken

instance AWSRequest ListExecutions where
  type Rs ListExecutions = ListExecutionsResponse
  request = postJSON stepFunctions
  response =
    receiveJSON
      ( \s h x ->
          ListExecutionsResponse'
            <$> (x .?> "nextToken")
            <*> (pure (fromEnum s))
            <*> (x .?> "executions" .!@ mempty)
      )

instance Hashable ListExecutions

instance NFData ListExecutions

instance ToHeaders ListExecutions where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSStepFunctions.ListExecutions" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.0" :: ByteString)
          ]
      )

instance ToJSON ListExecutions where
  toJSON ListExecutions' {..} =
    object
      ( catMaybes
          [ ("statusFilter" .=) <$> _leStatusFilter,
            ("nextToken" .=) <$> _leNextToken,
            ("maxResults" .=) <$> _leMaxResults,
            Just ("stateMachineArn" .= _leStateMachineARN)
          ]
      )

instance ToPath ListExecutions where
  toPath = const "/"

instance ToQuery ListExecutions where
  toQuery = const mempty

-- | /See:/ 'listExecutionsResponse' smart constructor.
data ListExecutionsResponse = ListExecutionsResponse'
  { _lersNextToken ::
      !(Maybe Text),
    _lersResponseStatus :: !Int,
    _lersExecutions :: ![ExecutionListItem]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListExecutionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lersNextToken' - If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
--
-- * 'lersResponseStatus' - -- | The response status code.
--
-- * 'lersExecutions' - The list of matching executions.
listExecutionsResponse ::
  -- | 'lersResponseStatus'
  Int ->
  ListExecutionsResponse
listExecutionsResponse pResponseStatus_ =
  ListExecutionsResponse'
    { _lersNextToken = Nothing,
      _lersResponseStatus = pResponseStatus_,
      _lersExecutions = mempty
    }

-- | If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
lersNextToken :: Lens' ListExecutionsResponse (Maybe Text)
lersNextToken = lens _lersNextToken (\s a -> s {_lersNextToken = a})

-- | -- | The response status code.
lersResponseStatus :: Lens' ListExecutionsResponse Int
lersResponseStatus = lens _lersResponseStatus (\s a -> s {_lersResponseStatus = a})

-- | The list of matching executions.
lersExecutions :: Lens' ListExecutionsResponse [ExecutionListItem]
lersExecutions = lens _lersExecutions (\s a -> s {_lersExecutions = a}) . _Coerce

instance NFData ListExecutionsResponse
