{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.JourneyDateRangeKpiResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.JourneyDateRangeKpiResponse where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.BaseKpiResult
import Network.AWS.Prelude

-- | Provides the results of a query that retrieved the data for a standard engagement metric that applies to a journey, and provides information about that query.
--
--
--
-- /See:/ 'journeyDateRangeKpiResponse' smart constructor.
data JourneyDateRangeKpiResponse = JourneyDateRangeKpiResponse'
  { _jdrkNextToken ::
      !(Maybe Text),
    _jdrkKpiResult :: !BaseKpiResult,
    _jdrkKpiName :: !Text,
    _jdrkJourneyId :: !Text,
    _jdrkEndTime :: !POSIX,
    _jdrkStartTime :: !POSIX,
    _jdrkApplicationId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'JourneyDateRangeKpiResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jdrkNextToken' - The string to use in a subsequent request to get the next page of results in a paginated response. This value is null for the Journey Engagement Metrics resource because the resource returns all results in a single page.
--
-- * 'jdrkKpiResult' - An array of objects that contains the results of the query. Each object contains the value for the metric and metadata about that value.
--
-- * 'jdrkKpiName' - The name of the metric, also referred to as a /key performance indicator (KPI)/ , that the data was retrieved for. This value describes the associated metric and consists of two or more terms, which are comprised of lowercase alphanumeric characters, separated by a hyphen. For a list of possible values, see the <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide> .
--
-- * 'jdrkJourneyId' - The unique identifier for the journey that the metric applies to.
--
-- * 'jdrkEndTime' - The last date and time of the date range that was used to filter the query results, in extended ISO 8601 format. The date range is inclusive.
--
-- * 'jdrkStartTime' - The first date and time of the date range that was used to filter the query results, in extended ISO 8601 format. The date range is inclusive.
--
-- * 'jdrkApplicationId' - The unique identifier for the application that the metric applies to.
journeyDateRangeKpiResponse ::
  -- | 'jdrkKpiResult'
  BaseKpiResult ->
  -- | 'jdrkKpiName'
  Text ->
  -- | 'jdrkJourneyId'
  Text ->
  -- | 'jdrkEndTime'
  UTCTime ->
  -- | 'jdrkStartTime'
  UTCTime ->
  -- | 'jdrkApplicationId'
  Text ->
  JourneyDateRangeKpiResponse
journeyDateRangeKpiResponse
  pKpiResult_
  pKpiName_
  pJourneyId_
  pEndTime_
  pStartTime_
  pApplicationId_ =
    JourneyDateRangeKpiResponse'
      { _jdrkNextToken = Nothing,
        _jdrkKpiResult = pKpiResult_,
        _jdrkKpiName = pKpiName_,
        _jdrkJourneyId = pJourneyId_,
        _jdrkEndTime = _Time # pEndTime_,
        _jdrkStartTime = _Time # pStartTime_,
        _jdrkApplicationId = pApplicationId_
      }

-- | The string to use in a subsequent request to get the next page of results in a paginated response. This value is null for the Journey Engagement Metrics resource because the resource returns all results in a single page.
jdrkNextToken :: Lens' JourneyDateRangeKpiResponse (Maybe Text)
jdrkNextToken = lens _jdrkNextToken (\s a -> s {_jdrkNextToken = a})

-- | An array of objects that contains the results of the query. Each object contains the value for the metric and metadata about that value.
jdrkKpiResult :: Lens' JourneyDateRangeKpiResponse BaseKpiResult
jdrkKpiResult = lens _jdrkKpiResult (\s a -> s {_jdrkKpiResult = a})

-- | The name of the metric, also referred to as a /key performance indicator (KPI)/ , that the data was retrieved for. This value describes the associated metric and consists of two or more terms, which are comprised of lowercase alphanumeric characters, separated by a hyphen. For a list of possible values, see the <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide> .
jdrkKpiName :: Lens' JourneyDateRangeKpiResponse Text
jdrkKpiName = lens _jdrkKpiName (\s a -> s {_jdrkKpiName = a})

-- | The unique identifier for the journey that the metric applies to.
jdrkJourneyId :: Lens' JourneyDateRangeKpiResponse Text
jdrkJourneyId = lens _jdrkJourneyId (\s a -> s {_jdrkJourneyId = a})

-- | The last date and time of the date range that was used to filter the query results, in extended ISO 8601 format. The date range is inclusive.
jdrkEndTime :: Lens' JourneyDateRangeKpiResponse UTCTime
jdrkEndTime = lens _jdrkEndTime (\s a -> s {_jdrkEndTime = a}) . _Time

-- | The first date and time of the date range that was used to filter the query results, in extended ISO 8601 format. The date range is inclusive.
jdrkStartTime :: Lens' JourneyDateRangeKpiResponse UTCTime
jdrkStartTime = lens _jdrkStartTime (\s a -> s {_jdrkStartTime = a}) . _Time

-- | The unique identifier for the application that the metric applies to.
jdrkApplicationId :: Lens' JourneyDateRangeKpiResponse Text
jdrkApplicationId = lens _jdrkApplicationId (\s a -> s {_jdrkApplicationId = a})

instance FromJSON JourneyDateRangeKpiResponse where
  parseJSON =
    withObject
      "JourneyDateRangeKpiResponse"
      ( \x ->
          JourneyDateRangeKpiResponse'
            <$> (x .:? "NextToken")
            <*> (x .: "KpiResult")
            <*> (x .: "KpiName")
            <*> (x .: "JourneyId")
            <*> (x .: "EndTime")
            <*> (x .: "StartTime")
            <*> (x .: "ApplicationId")
      )

instance Hashable JourneyDateRangeKpiResponse

instance NFData JourneyDateRangeKpiResponse
