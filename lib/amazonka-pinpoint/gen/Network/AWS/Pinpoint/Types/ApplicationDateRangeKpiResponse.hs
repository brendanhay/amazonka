{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ApplicationDateRangeKpiResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ApplicationDateRangeKpiResponse where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.BaseKpiResult
import Network.AWS.Prelude

-- | Provides the results of a query that retrieved the data for a standard metric that applies to an application, and provides information about that query.
--
--
--
-- /See:/ 'applicationDateRangeKpiResponse' smart constructor.
data ApplicationDateRangeKpiResponse = ApplicationDateRangeKpiResponse'
  { _adrkNextToken ::
      !(Maybe Text),
    _adrkKpiResult ::
      !BaseKpiResult,
    _adrkKpiName :: !Text,
    _adrkEndTime :: !POSIX,
    _adrkStartTime :: !POSIX,
    _adrkApplicationId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ApplicationDateRangeKpiResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adrkNextToken' - The string to use in a subsequent request to get the next page of results in a paginated response. This value is null for the Application Metrics resource because the resource returns all results in a single page.
--
-- * 'adrkKpiResult' - An array of objects that contains the results of the query. Each object contains the value for the metric and metadata about that value.
--
-- * 'adrkKpiName' - The name of the metric, also referred to as a /key performance indicator (KPI)/ , that the data was retrieved for. This value describes the associated metric and consists of two or more terms, which are comprised of lowercase alphanumeric characters, separated by a hyphen. For a list of possible values, see the <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide> .
--
-- * 'adrkEndTime' - The last date and time of the date range that was used to filter the query results, in extended ISO 8601 format. The date range is inclusive.
--
-- * 'adrkStartTime' - The first date and time of the date range that was used to filter the query results, in extended ISO 8601 format. The date range is inclusive.
--
-- * 'adrkApplicationId' - The unique identifier for the application that the metric applies to.
applicationDateRangeKpiResponse ::
  -- | 'adrkKpiResult'
  BaseKpiResult ->
  -- | 'adrkKpiName'
  Text ->
  -- | 'adrkEndTime'
  UTCTime ->
  -- | 'adrkStartTime'
  UTCTime ->
  -- | 'adrkApplicationId'
  Text ->
  ApplicationDateRangeKpiResponse
applicationDateRangeKpiResponse
  pKpiResult_
  pKpiName_
  pEndTime_
  pStartTime_
  pApplicationId_ =
    ApplicationDateRangeKpiResponse'
      { _adrkNextToken = Nothing,
        _adrkKpiResult = pKpiResult_,
        _adrkKpiName = pKpiName_,
        _adrkEndTime = _Time # pEndTime_,
        _adrkStartTime = _Time # pStartTime_,
        _adrkApplicationId = pApplicationId_
      }

-- | The string to use in a subsequent request to get the next page of results in a paginated response. This value is null for the Application Metrics resource because the resource returns all results in a single page.
adrkNextToken :: Lens' ApplicationDateRangeKpiResponse (Maybe Text)
adrkNextToken = lens _adrkNextToken (\s a -> s {_adrkNextToken = a})

-- | An array of objects that contains the results of the query. Each object contains the value for the metric and metadata about that value.
adrkKpiResult :: Lens' ApplicationDateRangeKpiResponse BaseKpiResult
adrkKpiResult = lens _adrkKpiResult (\s a -> s {_adrkKpiResult = a})

-- | The name of the metric, also referred to as a /key performance indicator (KPI)/ , that the data was retrieved for. This value describes the associated metric and consists of two or more terms, which are comprised of lowercase alphanumeric characters, separated by a hyphen. For a list of possible values, see the <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide> .
adrkKpiName :: Lens' ApplicationDateRangeKpiResponse Text
adrkKpiName = lens _adrkKpiName (\s a -> s {_adrkKpiName = a})

-- | The last date and time of the date range that was used to filter the query results, in extended ISO 8601 format. The date range is inclusive.
adrkEndTime :: Lens' ApplicationDateRangeKpiResponse UTCTime
adrkEndTime = lens _adrkEndTime (\s a -> s {_adrkEndTime = a}) . _Time

-- | The first date and time of the date range that was used to filter the query results, in extended ISO 8601 format. The date range is inclusive.
adrkStartTime :: Lens' ApplicationDateRangeKpiResponse UTCTime
adrkStartTime = lens _adrkStartTime (\s a -> s {_adrkStartTime = a}) . _Time

-- | The unique identifier for the application that the metric applies to.
adrkApplicationId :: Lens' ApplicationDateRangeKpiResponse Text
adrkApplicationId = lens _adrkApplicationId (\s a -> s {_adrkApplicationId = a})

instance FromJSON ApplicationDateRangeKpiResponse where
  parseJSON =
    withObject
      "ApplicationDateRangeKpiResponse"
      ( \x ->
          ApplicationDateRangeKpiResponse'
            <$> (x .:? "NextToken")
            <*> (x .: "KpiResult")
            <*> (x .: "KpiName")
            <*> (x .: "EndTime")
            <*> (x .: "StartTime")
            <*> (x .: "ApplicationId")
      )

instance Hashable ApplicationDateRangeKpiResponse

instance NFData ApplicationDateRangeKpiResponse
