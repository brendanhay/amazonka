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
-- Module      : Network.AWS.StepFunctions.GetExecutionHistory
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the history of the specified execution as a list of events. By default, the results are returned in ascending order of the @timeStamp@ of the events. Use the @reverseOrder@ parameter to get the latest events first.
--
--
-- If a @nextToken@ is returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextToken@ . Keep all other arguments unchanged.
--
--
-- This operation returns paginated results.
module Network.AWS.StepFunctions.GetExecutionHistory
    (
    -- * Creating a Request
      getExecutionHistory
    , GetExecutionHistory
    -- * Request Lenses
    , gehReverseOrder
    , gehNextToken
    , gehMaxResults
    , gehExecutionARN

    -- * Destructuring the Response
    , getExecutionHistoryResponse
    , GetExecutionHistoryResponse
    -- * Response Lenses
    , gehrsNextToken
    , gehrsResponseStatus
    , gehrsEvents
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StepFunctions.Types
import Network.AWS.StepFunctions.Types.Product

-- | /See:/ 'getExecutionHistory' smart constructor.
data GetExecutionHistory = GetExecutionHistory'
  { _gehReverseOrder :: !(Maybe Bool)
  , _gehNextToken    :: !(Maybe Text)
  , _gehMaxResults   :: !(Maybe Nat)
  , _gehExecutionARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetExecutionHistory' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gehReverseOrder' - Lists events in descending order of their @timeStamp@ .
--
-- * 'gehNextToken' - If a @nextToken@ is returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextToken@ . Keep all other arguments unchanged. The configured @maxResults@ determines how many results can be returned in a single call.
--
-- * 'gehMaxResults' - The maximum number of results that are returned per call. You can use @nextToken@ to obtain further pages of results. The default is 100 and the maximum allowed page size is 100. A value of 0 uses the default. This is only an upper limit. The actual number of results returned per call might be fewer than the specified maximum.
--
-- * 'gehExecutionARN' - The Amazon Resource Name (ARN) of the execution.
getExecutionHistory
    :: Text -- ^ 'gehExecutionARN'
    -> GetExecutionHistory
getExecutionHistory pExecutionARN_ =
  GetExecutionHistory'
    { _gehReverseOrder = Nothing
    , _gehNextToken = Nothing
    , _gehMaxResults = Nothing
    , _gehExecutionARN = pExecutionARN_
    }


-- | Lists events in descending order of their @timeStamp@ .
gehReverseOrder :: Lens' GetExecutionHistory (Maybe Bool)
gehReverseOrder = lens _gehReverseOrder (\ s a -> s{_gehReverseOrder = a})

-- | If a @nextToken@ is returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextToken@ . Keep all other arguments unchanged. The configured @maxResults@ determines how many results can be returned in a single call.
gehNextToken :: Lens' GetExecutionHistory (Maybe Text)
gehNextToken = lens _gehNextToken (\ s a -> s{_gehNextToken = a})

-- | The maximum number of results that are returned per call. You can use @nextToken@ to obtain further pages of results. The default is 100 and the maximum allowed page size is 100. A value of 0 uses the default. This is only an upper limit. The actual number of results returned per call might be fewer than the specified maximum.
gehMaxResults :: Lens' GetExecutionHistory (Maybe Natural)
gehMaxResults = lens _gehMaxResults (\ s a -> s{_gehMaxResults = a}) . mapping _Nat

-- | The Amazon Resource Name (ARN) of the execution.
gehExecutionARN :: Lens' GetExecutionHistory Text
gehExecutionARN = lens _gehExecutionARN (\ s a -> s{_gehExecutionARN = a})

instance AWSPager GetExecutionHistory where
        page rq rs
          | stop (rs ^. gehrsNextToken) = Nothing
          | stop (rs ^. gehrsEvents) = Nothing
          | otherwise =
            Just $ rq & gehNextToken .~ rs ^. gehrsNextToken

instance AWSRequest GetExecutionHistory where
        type Rs GetExecutionHistory =
             GetExecutionHistoryResponse
        request = postJSON stepFunctions
        response
          = receiveJSON
              (\ s h x ->
                 GetExecutionHistoryResponse' <$>
                   (x .?> "nextToken") <*> (pure (fromEnum s)) <*>
                     (x .?> "events" .!@ mempty))

instance Hashable GetExecutionHistory where

instance NFData GetExecutionHistory where

instance ToHeaders GetExecutionHistory where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSStepFunctions.GetExecutionHistory" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON GetExecutionHistory where
        toJSON GetExecutionHistory'{..}
          = object
              (catMaybes
                 [("reverseOrder" .=) <$> _gehReverseOrder,
                  ("nextToken" .=) <$> _gehNextToken,
                  ("maxResults" .=) <$> _gehMaxResults,
                  Just ("executionArn" .= _gehExecutionARN)])

instance ToPath GetExecutionHistory where
        toPath = const "/"

instance ToQuery GetExecutionHistory where
        toQuery = const mempty

-- | /See:/ 'getExecutionHistoryResponse' smart constructor.
data GetExecutionHistoryResponse = GetExecutionHistoryResponse'
  { _gehrsNextToken      :: !(Maybe Text)
  , _gehrsResponseStatus :: !Int
  , _gehrsEvents         :: ![HistoryEvent]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetExecutionHistoryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gehrsNextToken' - If a @nextToken@ is returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextToken@ . Keep all other arguments unchanged. The configured @maxResults@ determines how many results can be returned in a single call.
--
-- * 'gehrsResponseStatus' - -- | The response status code.
--
-- * 'gehrsEvents' - The list of events that occurred in the execution.
getExecutionHistoryResponse
    :: Int -- ^ 'gehrsResponseStatus'
    -> GetExecutionHistoryResponse
getExecutionHistoryResponse pResponseStatus_ =
  GetExecutionHistoryResponse'
    { _gehrsNextToken = Nothing
    , _gehrsResponseStatus = pResponseStatus_
    , _gehrsEvents = mempty
    }


-- | If a @nextToken@ is returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextToken@ . Keep all other arguments unchanged. The configured @maxResults@ determines how many results can be returned in a single call.
gehrsNextToken :: Lens' GetExecutionHistoryResponse (Maybe Text)
gehrsNextToken = lens _gehrsNextToken (\ s a -> s{_gehrsNextToken = a})

-- | -- | The response status code.
gehrsResponseStatus :: Lens' GetExecutionHistoryResponse Int
gehrsResponseStatus = lens _gehrsResponseStatus (\ s a -> s{_gehrsResponseStatus = a})

-- | The list of events that occurred in the execution.
gehrsEvents :: Lens' GetExecutionHistoryResponse [HistoryEvent]
gehrsEvents = lens _gehrsEvents (\ s a -> s{_gehrsEvents = a}) . _Coerce

instance NFData GetExecutionHistoryResponse where
