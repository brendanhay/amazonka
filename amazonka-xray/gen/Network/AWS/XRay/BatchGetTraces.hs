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
-- Module      : Network.AWS.XRay.BatchGetTraces
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of traces specified by ID. Each trace is a collection of segment documents that originates from a single request. Use @GetTraceSummaries@ to get a list of trace IDs.
--
--
--
-- This operation returns paginated results.
module Network.AWS.XRay.BatchGetTraces
    (
    -- * Creating a Request
      batchGetTraces
    , BatchGetTraces
    -- * Request Lenses
    , bgtNextToken
    , bgtTraceIds

    -- * Destructuring the Response
    , batchGetTracesResponse
    , BatchGetTracesResponse
    -- * Response Lenses
    , bgtrsNextToken
    , bgtrsTraces
    , bgtrsUnprocessedTraceIds
    , bgtrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.XRay.Types
import Network.AWS.XRay.Types.Product

-- | /See:/ 'batchGetTraces' smart constructor.
data BatchGetTraces = BatchGetTraces'
  { _bgtNextToken :: !(Maybe Text)
  , _bgtTraceIds  :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchGetTraces' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgtNextToken' - Pagination token. Not used.
--
-- * 'bgtTraceIds' - Specify the trace IDs of requests for which to retrieve segments.
batchGetTraces
    :: BatchGetTraces
batchGetTraces =
  BatchGetTraces' {_bgtNextToken = Nothing, _bgtTraceIds = mempty}


-- | Pagination token. Not used.
bgtNextToken :: Lens' BatchGetTraces (Maybe Text)
bgtNextToken = lens _bgtNextToken (\ s a -> s{_bgtNextToken = a})

-- | Specify the trace IDs of requests for which to retrieve segments.
bgtTraceIds :: Lens' BatchGetTraces [Text]
bgtTraceIds = lens _bgtTraceIds (\ s a -> s{_bgtTraceIds = a}) . _Coerce

instance AWSPager BatchGetTraces where
        page rq rs
          | stop (rs ^. bgtrsNextToken) = Nothing
          | stop (rs ^. bgtrsTraces) = Nothing
          | otherwise =
            Just $ rq & bgtNextToken .~ rs ^. bgtrsNextToken

instance AWSRequest BatchGetTraces where
        type Rs BatchGetTraces = BatchGetTracesResponse
        request = postJSON xRay
        response
          = receiveJSON
              (\ s h x ->
                 BatchGetTracesResponse' <$>
                   (x .?> "NextToken") <*> (x .?> "Traces" .!@ mempty)
                     <*> (x .?> "UnprocessedTraceIds" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable BatchGetTraces where

instance NFData BatchGetTraces where

instance ToHeaders BatchGetTraces where
        toHeaders = const mempty

instance ToJSON BatchGetTraces where
        toJSON BatchGetTraces'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _bgtNextToken,
                  Just ("TraceIds" .= _bgtTraceIds)])

instance ToPath BatchGetTraces where
        toPath = const "/Traces"

instance ToQuery BatchGetTraces where
        toQuery = const mempty

-- | /See:/ 'batchGetTracesResponse' smart constructor.
data BatchGetTracesResponse = BatchGetTracesResponse'
  { _bgtrsNextToken           :: !(Maybe Text)
  , _bgtrsTraces              :: !(Maybe [Trace])
  , _bgtrsUnprocessedTraceIds :: !(Maybe [Text])
  , _bgtrsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchGetTracesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgtrsNextToken' - Pagination token. Not used.
--
-- * 'bgtrsTraces' - Full traces for the specified requests.
--
-- * 'bgtrsUnprocessedTraceIds' - Trace IDs of requests that haven't been processed.
--
-- * 'bgtrsResponseStatus' - -- | The response status code.
batchGetTracesResponse
    :: Int -- ^ 'bgtrsResponseStatus'
    -> BatchGetTracesResponse
batchGetTracesResponse pResponseStatus_ =
  BatchGetTracesResponse'
    { _bgtrsNextToken = Nothing
    , _bgtrsTraces = Nothing
    , _bgtrsUnprocessedTraceIds = Nothing
    , _bgtrsResponseStatus = pResponseStatus_
    }


-- | Pagination token. Not used.
bgtrsNextToken :: Lens' BatchGetTracesResponse (Maybe Text)
bgtrsNextToken = lens _bgtrsNextToken (\ s a -> s{_bgtrsNextToken = a})

-- | Full traces for the specified requests.
bgtrsTraces :: Lens' BatchGetTracesResponse [Trace]
bgtrsTraces = lens _bgtrsTraces (\ s a -> s{_bgtrsTraces = a}) . _Default . _Coerce

-- | Trace IDs of requests that haven't been processed.
bgtrsUnprocessedTraceIds :: Lens' BatchGetTracesResponse [Text]
bgtrsUnprocessedTraceIds = lens _bgtrsUnprocessedTraceIds (\ s a -> s{_bgtrsUnprocessedTraceIds = a}) . _Default . _Coerce

-- | -- | The response status code.
bgtrsResponseStatus :: Lens' BatchGetTracesResponse Int
bgtrsResponseStatus = lens _bgtrsResponseStatus (\ s a -> s{_bgtrsResponseStatus = a})

instance NFData BatchGetTracesResponse where
