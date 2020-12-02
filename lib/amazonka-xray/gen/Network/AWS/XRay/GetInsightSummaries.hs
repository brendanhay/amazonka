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
-- Module      : Network.AWS.XRay.GetInsightSummaries
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the summaries of all insights in the specified group matching the provided filter values.
module Network.AWS.XRay.GetInsightSummaries
  ( -- * Creating a Request
    getInsightSummaries,
    GetInsightSummaries,

    -- * Request Lenses
    gisStates,
    gisNextToken,
    gisGroupARN,
    gisGroupName,
    gisMaxResults,
    gisStartTime,
    gisEndTime,

    -- * Destructuring the Response
    getInsightSummariesResponse,
    GetInsightSummariesResponse,

    -- * Response Lenses
    gisrsInsightSummaries,
    gisrsNextToken,
    gisrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.XRay.Types

-- | /See:/ 'getInsightSummaries' smart constructor.
data GetInsightSummaries = GetInsightSummaries'
  { _gisStates ::
      !(Maybe [InsightState]),
    _gisNextToken :: !(Maybe Text),
    _gisGroupARN :: !(Maybe Text),
    _gisGroupName :: !(Maybe Text),
    _gisMaxResults :: !(Maybe Nat),
    _gisStartTime :: !POSIX,
    _gisEndTime :: !POSIX
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetInsightSummaries' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gisStates' - The list of insight states.
--
-- * 'gisNextToken' - Pagination token.
--
-- * 'gisGroupARN' - The Amazon Resource Name (ARN) of the group. Required if the GroupName isn't provided.
--
-- * 'gisGroupName' - The name of the group. Required if the GroupARN isn't provided.
--
-- * 'gisMaxResults' - The maximum number of results to display.
--
-- * 'gisStartTime' - The beginning of the time frame in which the insights started. The start time can't be more than 30 days old.
--
-- * 'gisEndTime' - The end of the time frame in which the insights ended. The end time can't be more than 30 days old.
getInsightSummaries ::
  -- | 'gisStartTime'
  UTCTime ->
  -- | 'gisEndTime'
  UTCTime ->
  GetInsightSummaries
getInsightSummaries pStartTime_ pEndTime_ =
  GetInsightSummaries'
    { _gisStates = Nothing,
      _gisNextToken = Nothing,
      _gisGroupARN = Nothing,
      _gisGroupName = Nothing,
      _gisMaxResults = Nothing,
      _gisStartTime = _Time # pStartTime_,
      _gisEndTime = _Time # pEndTime_
    }

-- | The list of insight states.
gisStates :: Lens' GetInsightSummaries [InsightState]
gisStates = lens _gisStates (\s a -> s {_gisStates = a}) . _Default . _Coerce

-- | Pagination token.
gisNextToken :: Lens' GetInsightSummaries (Maybe Text)
gisNextToken = lens _gisNextToken (\s a -> s {_gisNextToken = a})

-- | The Amazon Resource Name (ARN) of the group. Required if the GroupName isn't provided.
gisGroupARN :: Lens' GetInsightSummaries (Maybe Text)
gisGroupARN = lens _gisGroupARN (\s a -> s {_gisGroupARN = a})

-- | The name of the group. Required if the GroupARN isn't provided.
gisGroupName :: Lens' GetInsightSummaries (Maybe Text)
gisGroupName = lens _gisGroupName (\s a -> s {_gisGroupName = a})

-- | The maximum number of results to display.
gisMaxResults :: Lens' GetInsightSummaries (Maybe Natural)
gisMaxResults = lens _gisMaxResults (\s a -> s {_gisMaxResults = a}) . mapping _Nat

-- | The beginning of the time frame in which the insights started. The start time can't be more than 30 days old.
gisStartTime :: Lens' GetInsightSummaries UTCTime
gisStartTime = lens _gisStartTime (\s a -> s {_gisStartTime = a}) . _Time

-- | The end of the time frame in which the insights ended. The end time can't be more than 30 days old.
gisEndTime :: Lens' GetInsightSummaries UTCTime
gisEndTime = lens _gisEndTime (\s a -> s {_gisEndTime = a}) . _Time

instance AWSRequest GetInsightSummaries where
  type Rs GetInsightSummaries = GetInsightSummariesResponse
  request = postJSON xRay
  response =
    receiveJSON
      ( \s h x ->
          GetInsightSummariesResponse'
            <$> (x .?> "InsightSummaries" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable GetInsightSummaries

instance NFData GetInsightSummaries

instance ToHeaders GetInsightSummaries where
  toHeaders = const mempty

instance ToJSON GetInsightSummaries where
  toJSON GetInsightSummaries' {..} =
    object
      ( catMaybes
          [ ("States" .=) <$> _gisStates,
            ("NextToken" .=) <$> _gisNextToken,
            ("GroupARN" .=) <$> _gisGroupARN,
            ("GroupName" .=) <$> _gisGroupName,
            ("MaxResults" .=) <$> _gisMaxResults,
            Just ("StartTime" .= _gisStartTime),
            Just ("EndTime" .= _gisEndTime)
          ]
      )

instance ToPath GetInsightSummaries where
  toPath = const "/InsightSummaries"

instance ToQuery GetInsightSummaries where
  toQuery = const mempty

-- | /See:/ 'getInsightSummariesResponse' smart constructor.
data GetInsightSummariesResponse = GetInsightSummariesResponse'
  { _gisrsInsightSummaries ::
      !(Maybe [InsightSummary]),
    _gisrsNextToken :: !(Maybe Text),
    _gisrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetInsightSummariesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gisrsInsightSummaries' - The summary of each insight within the group matching the provided filters. The summary contains the InsightID, start and end time, the root cause service, the root cause and client impact statistics, the top anomalous services, and the status of the insight.
--
-- * 'gisrsNextToken' - Pagination token.
--
-- * 'gisrsResponseStatus' - -- | The response status code.
getInsightSummariesResponse ::
  -- | 'gisrsResponseStatus'
  Int ->
  GetInsightSummariesResponse
getInsightSummariesResponse pResponseStatus_ =
  GetInsightSummariesResponse'
    { _gisrsInsightSummaries = Nothing,
      _gisrsNextToken = Nothing,
      _gisrsResponseStatus = pResponseStatus_
    }

-- | The summary of each insight within the group matching the provided filters. The summary contains the InsightID, start and end time, the root cause service, the root cause and client impact statistics, the top anomalous services, and the status of the insight.
gisrsInsightSummaries :: Lens' GetInsightSummariesResponse [InsightSummary]
gisrsInsightSummaries = lens _gisrsInsightSummaries (\s a -> s {_gisrsInsightSummaries = a}) . _Default . _Coerce

-- | Pagination token.
gisrsNextToken :: Lens' GetInsightSummariesResponse (Maybe Text)
gisrsNextToken = lens _gisrsNextToken (\s a -> s {_gisrsNextToken = a})

-- | -- | The response status code.
gisrsResponseStatus :: Lens' GetInsightSummariesResponse Int
gisrsResponseStatus = lens _gisrsResponseStatus (\s a -> s {_gisrsResponseStatus = a})

instance NFData GetInsightSummariesResponse
