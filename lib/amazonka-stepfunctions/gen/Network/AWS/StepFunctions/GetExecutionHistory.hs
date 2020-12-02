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
-- Module      : Network.AWS.StepFunctions.GetExecutionHistory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the history of the specified execution as a list of events. By default, the results are returned in ascending order of the @timeStamp@ of the events. Use the @reverseOrder@ parameter to get the latest events first.
--
--
-- If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
--
-- This API action is not supported by @EXPRESS@ state machines.
--
--
-- This operation returns paginated results.
module Network.AWS.StepFunctions.GetExecutionHistory
  ( -- * Creating a Request
    getExecutionHistory,
    GetExecutionHistory,

    -- * Request Lenses
    gehReverseOrder,
    gehIncludeExecutionData,
    gehNextToken,
    gehMaxResults,
    gehExecutionARN,

    -- * Destructuring the Response
    getExecutionHistoryResponse,
    GetExecutionHistoryResponse,

    -- * Response Lenses
    gehrsNextToken,
    gehrsResponseStatus,
    gehrsEvents,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StepFunctions.Types

-- | /See:/ 'getExecutionHistory' smart constructor.
data GetExecutionHistory = GetExecutionHistory'
  { _gehReverseOrder ::
      !(Maybe Bool),
    _gehIncludeExecutionData :: !(Maybe Bool),
    _gehNextToken :: !(Maybe Text),
    _gehMaxResults :: !(Maybe Nat),
    _gehExecutionARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetExecutionHistory' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gehReverseOrder' - Lists events in descending order of their @timeStamp@ .
--
-- * 'gehIncludeExecutionData' - You can select whether execution data (input or output of a history event) is returned. The default is @true@ .
--
-- * 'gehNextToken' - If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
--
-- * 'gehMaxResults' - The maximum number of results that are returned per call. You can use @nextToken@ to obtain further pages of results. The default is 100 and the maximum allowed page size is 1000. A value of 0 uses the default. This is only an upper limit. The actual number of results returned per call might be fewer than the specified maximum.
--
-- * 'gehExecutionARN' - The Amazon Resource Name (ARN) of the execution.
getExecutionHistory ::
  -- | 'gehExecutionARN'
  Text ->
  GetExecutionHistory
getExecutionHistory pExecutionARN_ =
  GetExecutionHistory'
    { _gehReverseOrder = Nothing,
      _gehIncludeExecutionData = Nothing,
      _gehNextToken = Nothing,
      _gehMaxResults = Nothing,
      _gehExecutionARN = pExecutionARN_
    }

-- | Lists events in descending order of their @timeStamp@ .
gehReverseOrder :: Lens' GetExecutionHistory (Maybe Bool)
gehReverseOrder = lens _gehReverseOrder (\s a -> s {_gehReverseOrder = a})

-- | You can select whether execution data (input or output of a history event) is returned. The default is @true@ .
gehIncludeExecutionData :: Lens' GetExecutionHistory (Maybe Bool)
gehIncludeExecutionData = lens _gehIncludeExecutionData (\s a -> s {_gehIncludeExecutionData = a})

-- | If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
gehNextToken :: Lens' GetExecutionHistory (Maybe Text)
gehNextToken = lens _gehNextToken (\s a -> s {_gehNextToken = a})

-- | The maximum number of results that are returned per call. You can use @nextToken@ to obtain further pages of results. The default is 100 and the maximum allowed page size is 1000. A value of 0 uses the default. This is only an upper limit. The actual number of results returned per call might be fewer than the specified maximum.
gehMaxResults :: Lens' GetExecutionHistory (Maybe Natural)
gehMaxResults = lens _gehMaxResults (\s a -> s {_gehMaxResults = a}) . mapping _Nat

-- | The Amazon Resource Name (ARN) of the execution.
gehExecutionARN :: Lens' GetExecutionHistory Text
gehExecutionARN = lens _gehExecutionARN (\s a -> s {_gehExecutionARN = a})

instance AWSPager GetExecutionHistory where
  page rq rs
    | stop (rs ^. gehrsNextToken) = Nothing
    | stop (rs ^. gehrsEvents) = Nothing
    | otherwise = Just $ rq & gehNextToken .~ rs ^. gehrsNextToken

instance AWSRequest GetExecutionHistory where
  type Rs GetExecutionHistory = GetExecutionHistoryResponse
  request = postJSON stepFunctions
  response =
    receiveJSON
      ( \s h x ->
          GetExecutionHistoryResponse'
            <$> (x .?> "nextToken")
            <*> (pure (fromEnum s))
            <*> (x .?> "events" .!@ mempty)
      )

instance Hashable GetExecutionHistory

instance NFData GetExecutionHistory

instance ToHeaders GetExecutionHistory where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSStepFunctions.GetExecutionHistory" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.0" :: ByteString)
          ]
      )

instance ToJSON GetExecutionHistory where
  toJSON GetExecutionHistory' {..} =
    object
      ( catMaybes
          [ ("reverseOrder" .=) <$> _gehReverseOrder,
            ("includeExecutionData" .=) <$> _gehIncludeExecutionData,
            ("nextToken" .=) <$> _gehNextToken,
            ("maxResults" .=) <$> _gehMaxResults,
            Just ("executionArn" .= _gehExecutionARN)
          ]
      )

instance ToPath GetExecutionHistory where
  toPath = const "/"

instance ToQuery GetExecutionHistory where
  toQuery = const mempty

-- | /See:/ 'getExecutionHistoryResponse' smart constructor.
data GetExecutionHistoryResponse = GetExecutionHistoryResponse'
  { _gehrsNextToken ::
      !(Maybe Text),
    _gehrsResponseStatus :: !Int,
    _gehrsEvents :: ![HistoryEvent]
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetExecutionHistoryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gehrsNextToken' - If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
--
-- * 'gehrsResponseStatus' - -- | The response status code.
--
-- * 'gehrsEvents' - The list of events that occurred in the execution.
getExecutionHistoryResponse ::
  -- | 'gehrsResponseStatus'
  Int ->
  GetExecutionHistoryResponse
getExecutionHistoryResponse pResponseStatus_ =
  GetExecutionHistoryResponse'
    { _gehrsNextToken = Nothing,
      _gehrsResponseStatus = pResponseStatus_,
      _gehrsEvents = mempty
    }

-- | If @nextToken@ is returned, there are more results available. The value of @nextToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 24 hours. Using an expired pagination token will return an /HTTP 400 InvalidToken/ error.
gehrsNextToken :: Lens' GetExecutionHistoryResponse (Maybe Text)
gehrsNextToken = lens _gehrsNextToken (\s a -> s {_gehrsNextToken = a})

-- | -- | The response status code.
gehrsResponseStatus :: Lens' GetExecutionHistoryResponse Int
gehrsResponseStatus = lens _gehrsResponseStatus (\s a -> s {_gehrsResponseStatus = a})

-- | The list of events that occurred in the execution.
gehrsEvents :: Lens' GetExecutionHistoryResponse [HistoryEvent]
gehrsEvents = lens _gehrsEvents (\s a -> s {_gehrsEvents = a}) . _Coerce

instance NFData GetExecutionHistoryResponse
