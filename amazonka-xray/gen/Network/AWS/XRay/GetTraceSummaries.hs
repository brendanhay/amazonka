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
-- Module      : Network.AWS.XRay.GetTraceSummaries
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves IDs and metadata for traces available for a specified time frame using an optional filter. To get the full traces, pass the trace IDs to @BatchGetTraces@ .
--
--
-- A filter expression can target traced requests that hit specific service nodes or edges, have errors, or come from a known user. For example, the following filter expression targets traces that pass through @api.example.com@ :
--
-- @service("api.example.com")@
--
-- This filter expression finds traces that have an annotation named @account@ with the value @12345@ :
--
-- @annotation.account = "12345"@
--
-- For a full list of indexed fields and keywords that you can use in filter expressions, see <http://docs.aws.amazon.com/xray/latest/devguide/xray-console-filters.html Using Filter Expressions> in the /AWS X-Ray Developer Guide/ .
--
--
-- This operation returns paginated results.
module Network.AWS.XRay.GetTraceSummaries
    (
    -- * Creating a Request
      getTraceSummaries
    , GetTraceSummaries
    -- * Request Lenses
    , gtsFilterExpression
    , gtsNextToken
    , gtsSampling
    , gtsStartTime
    , gtsEndTime

    -- * Destructuring the Response
    , getTraceSummariesResponse
    , GetTraceSummariesResponse
    -- * Response Lenses
    , gtsrsTracesProcessedCount
    , gtsrsNextToken
    , gtsrsApproximateTime
    , gtsrsTraceSummaries
    , gtsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.XRay.Types
import Network.AWS.XRay.Types.Product

-- | /See:/ 'getTraceSummaries' smart constructor.
data GetTraceSummaries = GetTraceSummaries'
  { _gtsFilterExpression :: !(Maybe Text)
  , _gtsNextToken        :: !(Maybe Text)
  , _gtsSampling         :: !(Maybe Bool)
  , _gtsStartTime        :: !POSIX
  , _gtsEndTime          :: !POSIX
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetTraceSummaries' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtsFilterExpression' - Specify a filter expression to retrieve trace summaries for services or requests that meet certain requirements.
--
-- * 'gtsNextToken' - Specify the pagination token returned by a previous request to retrieve the next page of results.
--
-- * 'gtsSampling' - Set to @true@ to get summaries for only a subset of available traces.
--
-- * 'gtsStartTime' - The start of the time frame for which to retrieve traces.
--
-- * 'gtsEndTime' - The end of the time frame for which to retrieve traces.
getTraceSummaries
    :: UTCTime -- ^ 'gtsStartTime'
    -> UTCTime -- ^ 'gtsEndTime'
    -> GetTraceSummaries
getTraceSummaries pStartTime_ pEndTime_ =
  GetTraceSummaries'
    { _gtsFilterExpression = Nothing
    , _gtsNextToken = Nothing
    , _gtsSampling = Nothing
    , _gtsStartTime = _Time # pStartTime_
    , _gtsEndTime = _Time # pEndTime_
    }


-- | Specify a filter expression to retrieve trace summaries for services or requests that meet certain requirements.
gtsFilterExpression :: Lens' GetTraceSummaries (Maybe Text)
gtsFilterExpression = lens _gtsFilterExpression (\ s a -> s{_gtsFilterExpression = a})

-- | Specify the pagination token returned by a previous request to retrieve the next page of results.
gtsNextToken :: Lens' GetTraceSummaries (Maybe Text)
gtsNextToken = lens _gtsNextToken (\ s a -> s{_gtsNextToken = a})

-- | Set to @true@ to get summaries for only a subset of available traces.
gtsSampling :: Lens' GetTraceSummaries (Maybe Bool)
gtsSampling = lens _gtsSampling (\ s a -> s{_gtsSampling = a})

-- | The start of the time frame for which to retrieve traces.
gtsStartTime :: Lens' GetTraceSummaries UTCTime
gtsStartTime = lens _gtsStartTime (\ s a -> s{_gtsStartTime = a}) . _Time

-- | The end of the time frame for which to retrieve traces.
gtsEndTime :: Lens' GetTraceSummaries UTCTime
gtsEndTime = lens _gtsEndTime (\ s a -> s{_gtsEndTime = a}) . _Time

instance AWSPager GetTraceSummaries where
        page rq rs
          | stop (rs ^. gtsrsNextToken) = Nothing
          | stop (rs ^. gtsrsTraceSummaries) = Nothing
          | otherwise =
            Just $ rq & gtsNextToken .~ rs ^. gtsrsNextToken

instance AWSRequest GetTraceSummaries where
        type Rs GetTraceSummaries = GetTraceSummariesResponse
        request = postJSON xRay
        response
          = receiveJSON
              (\ s h x ->
                 GetTraceSummariesResponse' <$>
                   (x .?> "TracesProcessedCount") <*>
                     (x .?> "NextToken")
                     <*> (x .?> "ApproximateTime")
                     <*> (x .?> "TraceSummaries" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetTraceSummaries where

instance NFData GetTraceSummaries where

instance ToHeaders GetTraceSummaries where
        toHeaders = const mempty

instance ToJSON GetTraceSummaries where
        toJSON GetTraceSummaries'{..}
          = object
              (catMaybes
                 [("FilterExpression" .=) <$> _gtsFilterExpression,
                  ("NextToken" .=) <$> _gtsNextToken,
                  ("Sampling" .=) <$> _gtsSampling,
                  Just ("StartTime" .= _gtsStartTime),
                  Just ("EndTime" .= _gtsEndTime)])

instance ToPath GetTraceSummaries where
        toPath = const "/TraceSummaries"

instance ToQuery GetTraceSummaries where
        toQuery = const mempty

-- | /See:/ 'getTraceSummariesResponse' smart constructor.
data GetTraceSummariesResponse = GetTraceSummariesResponse'
  { _gtsrsTracesProcessedCount :: !(Maybe Integer)
  , _gtsrsNextToken            :: !(Maybe Text)
  , _gtsrsApproximateTime      :: !(Maybe POSIX)
  , _gtsrsTraceSummaries       :: !(Maybe [TraceSummary])
  , _gtsrsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetTraceSummariesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtsrsTracesProcessedCount' - The total number of traces processed, including traces that did not match the specified filter expression.
--
-- * 'gtsrsNextToken' - If the requested time frame contained more than one page of results, you can use this token to retrieve the next page. The first page contains the most most recent results, closest to the end of the time frame.
--
-- * 'gtsrsApproximateTime' - The start time of this page of results.
--
-- * 'gtsrsTraceSummaries' - Trace IDs and metadata for traces that were found in the specified time frame.
--
-- * 'gtsrsResponseStatus' - -- | The response status code.
getTraceSummariesResponse
    :: Int -- ^ 'gtsrsResponseStatus'
    -> GetTraceSummariesResponse
getTraceSummariesResponse pResponseStatus_ =
  GetTraceSummariesResponse'
    { _gtsrsTracesProcessedCount = Nothing
    , _gtsrsNextToken = Nothing
    , _gtsrsApproximateTime = Nothing
    , _gtsrsTraceSummaries = Nothing
    , _gtsrsResponseStatus = pResponseStatus_
    }


-- | The total number of traces processed, including traces that did not match the specified filter expression.
gtsrsTracesProcessedCount :: Lens' GetTraceSummariesResponse (Maybe Integer)
gtsrsTracesProcessedCount = lens _gtsrsTracesProcessedCount (\ s a -> s{_gtsrsTracesProcessedCount = a})

-- | If the requested time frame contained more than one page of results, you can use this token to retrieve the next page. The first page contains the most most recent results, closest to the end of the time frame.
gtsrsNextToken :: Lens' GetTraceSummariesResponse (Maybe Text)
gtsrsNextToken = lens _gtsrsNextToken (\ s a -> s{_gtsrsNextToken = a})

-- | The start time of this page of results.
gtsrsApproximateTime :: Lens' GetTraceSummariesResponse (Maybe UTCTime)
gtsrsApproximateTime = lens _gtsrsApproximateTime (\ s a -> s{_gtsrsApproximateTime = a}) . mapping _Time

-- | Trace IDs and metadata for traces that were found in the specified time frame.
gtsrsTraceSummaries :: Lens' GetTraceSummariesResponse [TraceSummary]
gtsrsTraceSummaries = lens _gtsrsTraceSummaries (\ s a -> s{_gtsrsTraceSummaries = a}) . _Default . _Coerce

-- | -- | The response status code.
gtsrsResponseStatus :: Lens' GetTraceSummariesResponse Int
gtsrsResponseStatus = lens _gtsrsResponseStatus (\ s a -> s{_gtsrsResponseStatus = a})

instance NFData GetTraceSummariesResponse where
